let screen_height = 600

let world_to_screen_y world_y height =
  screen_height - int_of_float world_y - height

let color_wall = Gfx.rgb 100 100 100
let color_fire = Gfx.rgb 255 100 0
let color_water = Gfx.rgb 0 100 255
let color_goo = Gfx.rgb 100 180 100
let color_fireboy_door = Gfx.rgb 180 80 60
let color_watergirl_door = Gfx.rgb 60 80 180
let color_spikes = Gfx.rgb 80 80 80
let color_ice = Gfx.rgb 200 230 255
let color_conveyor = Gfx.rgb 120 120 120
let color_fireboy = Gfx.rgb 255 120 60
let color_watergirl = Gfx.rgb 60 120 255
let color_dead = Gfx.rgb 100 100 100
let color_diamond_red = Gfx.rgb 255 60 60
let color_diamond_blue = Gfx.rgb 60 60 255

let tile_color tile =
  match tile with
  | Types.Empty -> None
  | Types.Wall -> Some color_wall
  | Types.Fire -> Some color_fire
  | Types.Water -> Some color_water
  | Types.Goo -> Some color_goo
  | Types.Fireboy_door -> Some color_fireboy_door
  | Types.Watergirl_door -> Some color_watergirl_door
  | Types.Spikes -> Some color_spikes
  | Types.Ice -> Some color_ice
  | Types.Conveyor_left | Types.Conveyor_right -> Some color_conveyor
  | Types.Slope_up | Types.Slope_down -> Some color_wall

let draw_tiles level =
  let ts = Tuning.tile_size in
  for row = 0 to level.Level.height - 1 do
    for col = 0 to level.Level.width - 1 do
      let tile = level.Level.grid.(row).(col) in
      match tile_color tile with
      | None -> ()
      | Some c ->
          let x = col * ts in
          let y = world_to_screen_y (float_of_int (row * ts)) ts in
          Gfx.draw_rect ~x ~y ~w:ts ~h:ts c
    done
  done

let draw_player (p : Player.t) =
  let (x, y, w, h) = Player.bbox p in
  let screen_y = world_to_screen_y y (int_of_float h) in
  let color =
    if not p.alive then color_dead
    else match p.kind with Types.Fireboy -> color_fireboy | Types.Watergirl -> color_watergirl
  in
  Gfx.draw_rect ~x:(int_of_float x) ~y:screen_y ~w:(int_of_float w)
    ~h:(int_of_float h) color

let draw_diamond (d : Diamond.t) =
  if d.collected then ()
  else begin
    let screen_y = world_to_screen_y d.pos.y (int_of_float (Diamond.size /. 2.)) in
    let color =
      match d.kind with
      | Types.Fireboy -> color_diamond_red
      | Types.Watergirl -> color_diamond_blue
    in
    Gfx.draw_poly ~cx:(int_of_float d.pos.x) ~cy:screen_y ~sides:4
      ~radius:(Diamond.size /. 2.) ~rotation:45. color
  end

let draw_win_banner () =
  let text = "Level Complete!" in
  let x = 250 in
  let y = 250 in
  Gfx.draw_rect ~x:(x - 10) ~y:(y - 10) ~w:320 ~h:60 (Gfx.rgba 0 0 0 200);
  Gfx.draw_text text ~x ~y ~size:40 (Gfx.rgb 0 255 0)

let draw game =
  Gfx.clear (Gfx.rgb 30 30 40);
  draw_tiles game.Game.level;
  Array.iter draw_diamond game.Game.diamonds;
  draw_player game.Game.fireboy;
  draw_player game.Game.watergirl;
  Hud.draw game;
  if game.Game.status = Types.Won then draw_win_banner ()
