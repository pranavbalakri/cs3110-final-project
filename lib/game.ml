type t = {
  level : Level.level_data;
  fireboy : Player.t;
  watergirl : Player.t;
  diamonds : Diamond.t array;
  buttons : Entities.button array;
  levers : Entities.lever array;
  gates : Entities.gate array;
  elevators : Entities.elevator array;
  crates : Entities.crate array;
  teleporters : Entities.teleporter_pair array;
  fans : Entities.fan array;
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
  let crates =
    Array.of_list (List.map Entities.crate_of_spec level.Level.crates)
  in
  let teleporters =
    Array.of_list (List.map Entities.tele_of_spec level.Level.teleporters)
  in
  let fans =
    Array.of_list (List.map Entities.fan_of_spec level.Level.fans)
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
    crates;
    teleporters;
    fans;
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
  Array.iter Entities.reset_crate game.crates;
  Array.iter Entities.reset_fan game.fans;
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

        (* 3. Emit button signals based on player + crate positions *)
        let players = [ game.fireboy; game.watergirl ] in
        let crates_list = Array.to_list game.crates in
        Array.iter
          (fun b -> Entities.update_button b players crates_list game.signals)
          game.buttons;

        (* 4. Handle lever interact (edge-triggered) *)
        Array.iter
          (fun lv ->
            Entities.update_lever lv players inp.interact_pressed game.signals)
          game.levers;

        (* 5. Open/close gates based on signal snapshot *)
        Array.iter (fun g -> Entities.update_gate g game.signals) game.gates;

        (* 6. Update fans (signal-driven) *)
        Array.iter (fun f -> Entities.update_fan f game.signals) game.fans;

        (* 7. Build extra-solid list: closed gates + elevator platforms *)
        let extra_solid =
          let closed_gates =
            Array.to_list game.gates
            |> List.filter_map (fun (g : Entities.gate) ->
                   if not g.is_open then Some (Entities.bbox_of_gate g)
                   else None)
          in
          closed_gates
          @ (Array.to_list game.elevators
             |> List.map Entities.bbox_of_elevator)
        in

        (* 8. Translate players riding elevators before physics *)
        List.iter (Entities.apply_elevator_riding game.elevators) players;

        (* 9. Compute ground surface for each player (feeds apply_input) *)
        Physics.compute_ground_surface game.level game.fireboy;
        Physics.compute_ground_surface game.level game.watergirl;

        (* 10. Fan membership: reset, then flag for each active fan *)
        game.fireboy.in_fan <- false;
        game.watergirl.in_fan <- false;
        Array.iter
          (fun f ->
            Entities.apply_fan_to_player f game.fireboy;
            Entities.apply_fan_to_player f game.watergirl)
          game.fans;

        (* 11. Apply player input and gravity (gravity knows about in_fan) *)
        Player.apply_input game.fireboy inp.fireboy;
        Player.apply_input game.watergirl inp.watergirl;
        Player.apply_gravity game.fireboy;
        Player.apply_gravity game.watergirl;

        (* 12. Move crates first (vertical only; horizontal is push-driven) *)
        let crates_list = Array.to_list game.crates in
        Array.iter
          (fun c -> Physics.move_crate game.level extra_solid crates_list c)
          game.crates;

        (* 13. Sweep-based player movement with tile + entity + crate collision *)
        let crates_list = Array.to_list game.crates in
        Physics.move_player game.level extra_solid crates_list game.fireboy;
        Physics.move_player game.level extra_solid crates_list game.watergirl;

        (* 14. Teleporters (after movement, with per-player cooldown) *)
        Entities.tick_teleport_cooldowns players dt;
        Array.iter
          (fun tp -> Entities.update_teleporter tp players)
          game.teleporters;

        (* 15. Hazards, doors, diamonds *)
        Physics.check_hazards game.level game.fireboy;
        Physics.check_hazards game.level game.watergirl;
        Physics.check_door game.level game.fireboy;
        Physics.check_door game.level game.watergirl;
        Array.iter
          (fun d ->
            Diamond.try_collect d game.fireboy;
            Diamond.try_collect d game.watergirl)
          game.diamonds;

        (* 16. Win / lose *)
        if (not game.fireboy.alive) || not game.watergirl.alive then begin
          game.status <- Types.Lost;
          game.death_timer <- 1.0
        end
        else if game.fireboy.at_door && game.watergirl.at_door then
          game.status <- Types.Won;

        game
  end
