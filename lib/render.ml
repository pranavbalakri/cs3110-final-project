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

(* Entity colours *)
let color_button_up = Gfx.rgb 160 120 60
let color_button_down = Gfx.rgb 80 200 80
let color_lever_base = Gfx.rgb 140 100 50
let color_lever_active = Gfx.rgb 255 220 0
let color_lever_inactive = Gfx.rgb 80 60 30
let color_gate_closed = Gfx.rgba 160 80 200 220
let color_gate_open = Gfx.rgba 160 80 200 60
let color_elevator = Gfx.rgb 180 130 60
let color_crate = Gfx.rgb 160 110 60
let color_crate_outline = Gfx.rgb 80 50 20
let color_tele_a = Gfx.rgb 180 120 255
let color_tele_b = Gfx.rgb 120 255 200
let color_fan_base = Gfx.rgb 80 80 100
let color_fan_on = Gfx.rgba 180 220 255 90
let color_fan_off = Gfx.rgba 100 100 120 50

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
    else
      match p.kind with
      | Types.Fireboy -> color_fireboy
      | Types.Watergirl -> color_watergirl
  in
  Gfx.draw_rect ~x:(int_of_float x) ~y:screen_y ~w:(int_of_float w)
    ~h:(int_of_float h) color

let draw_diamond (d : Diamond.t) =
  if d.collected then ()
  else begin
    let screen_y =
      world_to_screen_y d.pos.y (int_of_float (Diamond.size /. 2.))
    in
    let color =
      match d.kind with
      | Types.Fireboy -> color_diamond_red
      | Types.Watergirl -> color_diamond_blue
    in
    Gfx.draw_poly ~cx:(int_of_float d.pos.x) ~cy:screen_y ~sides:4
      ~radius:(Diamond.size /. 2.) ~rotation:45. color
  end

(* ── Entity renderers ───────────────────────────────────────────────────── *)

let draw_button (b : Entities.button) =
  let x = int_of_float b.pos.x in
  let h = int_of_float b.h in
  let screen_y = world_to_screen_y b.pos.y h in
  let color = if b.pressed then color_button_down else color_button_up in
  Gfx.draw_rect ~x ~y:screen_y ~w:(int_of_float b.w) ~h color

let draw_lever (lv : Entities.lever) =
  let ts = float_of_int Tuning.tile_size in
  let bw = int_of_float (ts /. 2.) in
  let bh = int_of_float (ts /. 6.) in
  let bx = int_of_float lv.pos.x + int_of_float (ts /. 4.) in
  let by = world_to_screen_y lv.pos.y bh in
  Gfx.draw_rect ~x:bx ~y:by ~w:bw ~h:bh color_lever_base;
  (* Stick: a small rectangle offset left or right based on state *)
  let stick_w = int_of_float (ts /. 8.) in
  let stick_h = int_of_float (ts /. 3.) in
  let stick_x =
    if lv.state then bx + bw - stick_w - 2
    else bx + 2
  in
  let stick_y = world_to_screen_y (lv.pos.y +. ts /. 6.) stick_h in
  let stick_color = if lv.state then color_lever_active else color_lever_inactive in
  Gfx.draw_rect ~x:stick_x ~y:stick_y ~w:stick_w ~h:stick_h stick_color

let draw_gate (g : Entities.gate) =
  let x = int_of_float g.pos.x in
  let h = int_of_float g.h in
  let screen_y = world_to_screen_y g.pos.y h in
  let color = if g.is_open then color_gate_open else color_gate_closed in
  Gfx.draw_rect ~x ~y:screen_y ~w:(int_of_float g.w) ~h color;
  if not g.is_open then
    Gfx.draw_rect_lines ~x ~y:screen_y ~w:(int_of_float g.w) ~h
      (Gfx.rgb 200 100 255)

let draw_elevator (e : Entities.elevator) =
  let x = int_of_float e.pos.x in
  let h = int_of_float e.h in
  let screen_y = world_to_screen_y e.pos.y h in
  Gfx.draw_rect ~x ~y:screen_y ~w:(int_of_float e.w) ~h color_elevator

let draw_crate (c : Entities.crate) =
  let x = int_of_float c.pos.x in
  let w = int_of_float c.w in
  let h = int_of_float c.h in
  let screen_y = world_to_screen_y c.pos.y h in
  Gfx.draw_rect ~x ~y:screen_y ~w ~h color_crate;
  Gfx.draw_rect_lines ~x ~y:screen_y ~w ~h color_crate_outline

let draw_teleporter (tp : Entities.teleporter_pair) =
  let ts = int_of_float tp.tile_w in
  let ax = int_of_float tp.a.x in
  let ay = world_to_screen_y tp.a.y ts in
  let bx = int_of_float tp.b.x in
  let by = world_to_screen_y tp.b.y ts in
  Gfx.draw_rect ~x:ax ~y:ay ~w:ts ~h:ts (Gfx.rgba 180 120 255 70);
  Gfx.draw_rect_lines ~x:ax ~y:ay ~w:ts ~h:ts color_tele_a;
  Gfx.draw_rect ~x:bx ~y:by ~w:ts ~h:ts (Gfx.rgba 120 255 200 70);
  Gfx.draw_rect_lines ~x:bx ~y:by ~w:ts ~h:ts color_tele_b

let draw_fan (f : Entities.fan) =
  let (bx, by, bw, bh) = Entities.fan_base_rect f in
  let bx = int_of_float bx in
  let bh = int_of_float bh in
  let bw = int_of_float bw in
  let base_sy = world_to_screen_y by bh in
  Gfx.draw_rect ~x:bx ~y:base_sy ~w:bw ~h:bh color_fan_base;
  let (cx, cy, cw, ch) = Entities.fan_column_rect f in
  let cx = int_of_float cx in
  let cw = int_of_float cw in
  let ch = int_of_float ch in
  let col_sy = world_to_screen_y cy ch in
  let color = if f.is_on then color_fan_on else color_fan_off in
  Gfx.draw_rect ~x:cx ~y:col_sy ~w:cw ~h:ch color

(* ── Debug overlay (F3) ─────────────────────────────────────────────────── *)

let draw_debug (game : Game.t) =
  let bg = Gfx.rgba 0 0 0 180 in
  Gfx.draw_rect ~x:0 ~y:0 ~w:220 ~h:200 bg;
  Gfx.draw_text "-- signals (F3) --" ~x:5 ~y:5 ~size:14 (Gfx.rgb 200 200 200);
  (* Buttons *)
  Array.iteri
    (fun i (b : Entities.button) ->
      let s = Printf.sprintf "btn %s: %b" b.id b.pressed in
      let c = if b.pressed then Gfx.rgb 80 255 80 else Gfx.rgb 180 180 180 in
      Gfx.draw_text s ~x:5 ~y:(25 + (i * 16)) ~size:13 c)
    game.Game.buttons;
  let off_b = Array.length game.Game.buttons in
  (* Levers *)
  Array.iteri
    (fun i (lv : Entities.lever) ->
      let s = Printf.sprintf "lev %s: %b" lv.id lv.state in
      let c =
        if lv.state then Gfx.rgb 255 220 0 else Gfx.rgb 180 180 180
      in
      Gfx.draw_text s ~x:5 ~y:(25 + ((off_b + i) * 16)) ~size:13 c)
    game.Game.levers;
  let off_l = off_b + Array.length game.Game.levers in
  (* Gates *)
  Array.iteri
    (fun i (g : Entities.gate) ->
      let s = Printf.sprintf "gate %d: %s" i (if g.is_open then "open" else "closed") in
      let c =
        if g.is_open then Gfx.rgb 160 80 255 else Gfx.rgb 180 180 180
      in
      Gfx.draw_text s ~x:5 ~y:(25 + ((off_l + i) * 16)) ~size:13 c)
    game.Game.gates

(* ── Banner ─────────────────────────────────────────────────────────────── *)

let draw_win_banner () =
  let text = "Level Complete!" in
  let x = 250 in
  let y = 250 in
  Gfx.draw_rect ~x:(x - 10) ~y:(y - 10) ~w:320 ~h:60 (Gfx.rgba 0 0 0 200);
  Gfx.draw_text text ~x ~y ~size:40 (Gfx.rgb 0 255 0)

(* ── Main draw call ─────────────────────────────────────────────────────── *)

let draw game =
  Gfx.clear (Gfx.rgb 30 30 40);
  draw_tiles game.Game.level;
  (* Entities below players *)
  Array.iter draw_fan game.Game.fans;
  Array.iter draw_teleporter game.Game.teleporters;
  Array.iter draw_elevator game.Game.elevators;
  Array.iter draw_gate game.Game.gates;
  Array.iter draw_button game.Game.buttons;
  Array.iter draw_lever game.Game.levers;
  Array.iter draw_crate game.Game.crates;
  (* Collectibles and characters *)
  Array.iter draw_diamond game.Game.diamonds;
  draw_player game.Game.fireboy;
  draw_player game.Game.watergirl;
  Hud.draw game;
  if game.Game.status = Types.Won then draw_win_banner ();
  if game.Game.debug then draw_debug game
