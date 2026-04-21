type t = {
  level : Level.level_data;
  fireboy : Player.t;
  watergirl : Player.t;
  diamonds : Diamond.t array;
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
  {
    level;
    fireboy;
    watergirl;
    diamonds;
    status = Types.Playing;
    elapsed = 0.;
    death_timer = 0.;
  }

let reset game =
  Player.reset game.fireboy game.level.fireboy_spawn;
  Player.reset game.watergirl game.level.watergirl_spawn;
  Array.iter Diamond.reset game.diamonds;
  game.status <- Types.Playing;
  game.elapsed <- 0.;
  game.death_timer <- 0.

let update game (inp : Input.t) dt =
  if inp.reset then begin
    reset game;
    game
  end
  else
    match game.status with
    | Types.Won -> game
    | Types.Paused -> game
    | Types.Lost ->
        game.death_timer <- game.death_timer -. dt;
        if game.death_timer <= 0. then reset game;
        game
    | Types.Playing ->
        game.elapsed <- game.elapsed +. dt;
        Player.apply_input game.fireboy inp.fireboy;
        Player.apply_input game.watergirl inp.watergirl;
        Player.apply_gravity game.fireboy;
        Player.apply_gravity game.watergirl;
        Physics.move_player game.level game.fireboy;
        Physics.move_player game.level game.watergirl;
        Physics.check_hazards game.level game.fireboy;
        Physics.check_hazards game.level game.watergirl;
        Physics.check_door game.level game.fireboy;
        Physics.check_door game.level game.watergirl;
        Array.iter
          (fun d ->
            Diamond.try_collect d game.fireboy;
            Diamond.try_collect d game.watergirl)
          game.diamonds;
        if (not game.fireboy.alive) || not game.watergirl.alive then begin
          game.status <- Types.Lost;
          game.death_timer <- 1.0
        end
        else if game.fireboy.at_door && game.watergirl.at_door then
          game.status <- Types.Won;
        game
