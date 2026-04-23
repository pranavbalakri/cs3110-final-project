let is_solid tile =
  match tile with
  | Types.Wall | Types.Ice | Types.Conveyor_left | Types.Conveyor_right -> true
  | _ -> false

let is_deadly_for kind tile =
  match (kind, tile) with
  | Types.Fireboy, Types.Water -> true
  | Types.Watergirl, Types.Fire -> true
  | _, Types.Goo -> true
  | _, Types.Spikes -> true
  | _ -> false

let overlapping_tiles (x, y, w, h) =
  let ts = float_of_int Tuning.tile_size in
  let min_col = int_of_float (x /. ts) in
  let max_col = int_of_float ((x +. w -. 0.001) /. ts) in
  let min_row = int_of_float (y /. ts) in
  let max_row = int_of_float ((y +. h -. 0.001) /. ts) in
  let tiles = ref [] in
  for row = min_row to max_row do
    for col = min_col to max_col do
      tiles := (col, row) :: !tiles
    done
  done;
  !tiles

let rects_overlap (x1, y1, w1, h1) (x2, y2, w2, h2) =
  x1 < x2 +. w2 && x1 +. w1 > x2 && y1 < y2 +. h2 && y1 +. h1 > y2

(** True if [test_bbox] collides with any solid tile OR any extra solid rect
    (closed gates, elevator platforms). *)
let any_solid level extra_solid tiles test_bbox =
  List.exists (fun (col, row) -> is_solid (Level.get_tile level col row)) tiles
  || List.exists (rects_overlap test_bbox) extra_solid

(** Move player one frame with per-axis swept collision.
    [extra_solid] is a list of (x,y,w,h) rects treated as solid
    (closed gates, elevator platforms). *)
let move_player level extra_solid (p : Player.t) =
  if not p.alive then ()
  else begin
    let half_w = Player.width /. 2. in
    let step = 1.0 in
    let vx = p.vel.x in
    let steps_x = int_of_float (Float.abs vx /. step) + 1 in
    let dx = vx /. float_of_int steps_x in
    for _ = 1 to steps_x do
      let new_x = p.pos.x +. dx in
      let test_bbox = (new_x -. half_w, p.pos.y, Player.width, Player.height) in
      let tiles = overlapping_tiles test_bbox in
      if any_solid level extra_solid tiles test_bbox then
        p.vel <- { p.vel with Vec2.x = 0. }
      else p.pos <- { p.pos with Vec2.x = new_x }
    done;
    let vy = p.vel.y in
    let steps_y = int_of_float (Float.abs vy /. step) + 1 in
    let dy = vy /. float_of_int steps_y in
    p.on_ground <- false;
    for _ = 1 to steps_y do
      let new_y = p.pos.y +. dy in
      let test_bbox = (p.pos.x -. half_w, new_y, Player.width, Player.height) in
      let tiles = overlapping_tiles test_bbox in
      if any_solid level extra_solid tiles test_bbox then begin
        if dy < 0. then p.on_ground <- true;
        p.vel <- { p.vel with Vec2.y = 0. }
      end
      else p.pos <- { p.pos with Vec2.y = new_y }
    done
  end

let check_hazards level (p : Player.t) =
  if not p.alive then ()
  else begin
    let bbox = Player.bbox p in
    let tiles = overlapping_tiles bbox in
    let touching_deadly =
      List.exists
        (fun (col, row) ->
          is_deadly_for p.kind (Level.get_tile level col row))
        tiles
    in
    if touching_deadly then p.alive <- false
  end

let check_door level (p : Player.t) =
  if not p.alive then begin
    p.at_door <- false
  end
  else begin
    let center = Player.center p in
    let ts = float_of_int Tuning.tile_size in
    let col = int_of_float (center.x /. ts) in
    let row = int_of_float (center.y /. ts) in
    let tile = Level.get_tile level col row in
    let at_correct_door =
      match (p.kind, tile) with
      | Types.Fireboy, Types.Fireboy_door -> true
      | Types.Watergirl, Types.Watergirl_door -> true
      | _ -> false
    in
    p.at_door <- at_correct_door
  end
