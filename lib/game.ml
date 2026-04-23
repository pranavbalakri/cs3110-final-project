type t = {
  level : Level.level_data;
  fireboy : Player.t;
  watergirl : Player.t;
  diamonds : Diamond.t array;
  buttons : Entities.button array;
  levers : Entities.lever array;
  gates : Entities.gate array;
  elevators : Entities.elevator array;
  signals : Signals.table;
  mutable debug : bool;
  mutable status : Types.status;
  mutable elapsed : float;
  mutable death_timer : float;
}

let init level =
  let fireboy = Player.create Types.Fireboy level.Level.fireboy_spawn in
  let watergirl = Player.create Types.Watergirl level.Level.watergirl_spawn in
  let diamonds =
    Array.of_list (List.map Diamond.from_spec level.Level.diamonds)
  in
  let buttons =
    Array.of_list (List.map Entities.button_of_spec level.Level.buttons)
  in
  let levers =
    Array.of_list (List.map Entities.lever_of_spec level.Level.levers)
  in
  let gates =
    Array.of_list (List.map Entities.gate_of_spec level.Level.gates)
  in
  let elevators =
    Array.of_list (List.map Entities.elev_of_spec level.Level.elevators)
  in
  {
    level;
    fireboy;
    watergirl;
    diamonds;
    buttons;
    levers;
    gates;
    elevators;
    signals = Signals.create ();
    debug = false;
    status = Types.Playing;
    elapsed = 0.;
    death_timer = 0.;
  }

let reset game =
  Player.reset game.fireboy game.level.fireboy_spawn;
  Player.reset game.watergirl game.level.watergirl_spawn;
  Array.iter Diamond.reset game.diamonds;
  Array.iter Entities.reset_lever game.levers;
  Array.iter Entities.reset_elevator game.elevators;
  Signals.clear game.signals;
  game.status <- Types.Playing;
  game.elapsed <- 0.;
  game.death_timer <- 0.

let update game (inp : Input.t) dt =
  if inp.reset then begin
    reset game;
    game
  end
  else begin
    if inp.debug_toggle then game.debug <- not game.debug;
    match game.status with
    | Types.Won | Types.Paused -> game
    | Types.Lost ->
        game.death_timer <- game.death_timer -. dt;
        if game.death_timer <= 0. then reset game;
        game
    | Types.Playing ->
        game.elapsed <- game.elapsed +. dt;

        (* 1. Clear signal table for this frame *)
        Signals.clear game.signals;

        (* 2. Advance elevators; record frame_delta for riding *)
        Array.iter (fun e -> Entities.update_elevator e dt) game.elevators;

        (* 3. Emit button signals based on player positions *)
        let players = [ game.fireboy; game.watergirl ] in
        Array.iter
          (fun b -> Entities.update_button b players game.signals)
          game.buttons;

        (* 4. Handle lever interact (edge-triggered) *)
        Array.iter
          (fun lv ->
            Entities.update_lever lv players inp.interact_pressed game.signals)
          game.levers;

        (* 5. Open/close gates based on signal snapshot *)
        Array.iter (fun g -> Entities.update_gate g game.signals) game.gates;

        (* 6. Build extra-solid list: closed gates + elevator platforms *)
        let extra_solid =
          Array.to_list game.gates
          |> List.filter_map (fun (g : Entities.gate) ->
                 if not g.is_open then Some (Entities.bbox_of_gate g) else None)
          |> fun closed ->
          closed
          @ (Array.to_list game.elevators
            |> List.map Entities.bbox_of_elevator)
        in

        (* 7. Translate players riding elevators before physics *)
        List.iter (Entities.apply_elevator_riding game.elevators) players;

        (* 8. Apply player input and gravity *)
        Player.apply_input game.fireboy inp.fireboy;
        Player.apply_input game.watergirl inp.watergirl;
        Player.apply_gravity game.fireboy;
        Player.apply_gravity game.watergirl;

        (* 9. Sweep-based movement with tile + entity collision *)
        Physics.move_player game.level extra_solid game.fireboy;
        Physics.move_player game.level extra_solid game.watergirl;

        (* 10. Hazards, doors, diamonds *)
        Physics.check_hazards game.level game.fireboy;
        Physics.check_hazards game.level game.watergirl;
        Physics.check_door game.level game.fireboy;
        Physics.check_door game.level game.watergirl;
        Array.iter
          (fun d ->
            Diamond.try_collect d game.fireboy;
            Diamond.try_collect d game.watergirl)
          game.diamonds;

        (* 11. Win / lose *)
        if (not game.fireboy.alive) || not game.watergirl.alive then begin
          game.status <- Types.Lost;
          game.death_timer <- 1.0
        end
        else if game.fireboy.at_door && game.watergirl.at_door then
          game.status <- Types.Won;

        game
  end
