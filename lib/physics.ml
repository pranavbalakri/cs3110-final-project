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

let any_solid level extra_solid tiles test_bbox =
  List.exists (fun (col, row) -> is_solid (Level.get_tile level col row)) tiles
  || List.exists (rects_overlap test_bbox) extra_solid

(** True if [test_bbox] is blocked by a tile / gate-elevator / other crate. *)
let bbox_blocked level extra_solid crates ?(ignore_crate = None) test_bbox =
  let tiles = overlapping_tiles test_bbox in
  any_solid level extra_solid tiles test_bbox
  || List.exists
       (fun (c : Entities.crate) ->
         (match ignore_crate with
          | Some ic -> c != ic
          | None -> true)
         && rects_overlap test_bbox (Entities.bbox_of_crate c))
       crates

(** Try to shove crate [c] horizontally by [dx]. Mutates [c] on success. *)
let try_push_crate level extra_solid crates (c : Entities.crate) dx =
  let new_x = c.pos.x +. dx in
  let test_bbox = (new_x, c.pos.y, c.w, c.h) in
  if bbox_blocked level extra_solid crates ~ignore_crate:(Some c) test_bbox then
    false
  else begin
    c.pos <- { c.pos with Vec2.x = new_x };
    true
  end

(** Determine the ground surface under [p]'s feet (1 px below bbox bottom).
    Conveyor beats ice beats normal when feet straddle tiles. *)
let compute_ground_surface level (p : Player.t) =
  let (x, y, w, _) = Player.bbox p in
  let ts = float_of_int Tuning.tile_size in
  let probe_y = y -. 1. in
  if probe_y < 0. then p.ground_surface <- Player.Normal
  else begin
    let col_l = int_of_float (x /. ts) in
    let col_r = int_of_float ((x +. w -. 0.001) /. ts) in
    let row = int_of_float (probe_y /. ts) in
    let found_conveyor = ref None in
    let found_ice = ref false in
    for c = col_l to col_r do
      match Level.get_tile level c row with
      | Types.Ice -> found_ice := true
      | Types.Conveyor_left ->
          found_conveyor := Some (-. Tuning.conveyor_speed)
      | Types.Conveyor_right ->
          found_conveyor := Some Tuning.conveyor_speed
      | _ -> ()
    done;
    p.ground_surface <-
      (match !found_conveyor with
       | Some s -> Player.Conveyor_belt s
       | None -> if !found_ice then Player.Ice else Player.Normal)
  end

(** Move player one frame with per-axis swept collision.
    Tiles and [extra_solid] (gates + elevators) are hard walls.
    Crates are hard walls vertically, but horizontally we attempt to push
    a single crate out of the way before blocking. *)
let move_player level extra_solid crates (p : Player.t) =
  if not p.alive then ()
  else begin
    let half_w = Player.width /. 2. in
    let step = 1.0 in
    (* Horizontal sweep *)
    let vx = p.vel.x in
    let steps_x = int_of_float (Float.abs vx /. step) + 1 in
    let dx = vx /. float_of_int steps_x in
    for _ = 1 to steps_x do
      let new_x = p.pos.x +. dx in
      let test_bbox = (new_x -. half_w, p.pos.y, Player.width, Player.height) in
      let tiles = overlapping_tiles test_bbox in
      if any_solid level extra_solid tiles test_bbox then
        p.vel <- { p.vel with Vec2.x = 0. }
      else begin
        let blocking_crate =
          List.find_opt
            (fun (c : Entities.crate) ->
              rects_overlap test_bbox (Entities.bbox_of_crate c))
            crates
        in
        match blocking_crate with
        | None -> p.pos <- { p.pos with Vec2.x = new_x }
        | Some c ->
            if try_push_crate level extra_solid crates c dx then
              p.pos <- { p.pos with Vec2.x = new_x }
            else p.vel <- { p.vel with Vec2.x = 0. }
      end
    done;
    (* Vertical sweep *)
    let vy = p.vel.y in
    let steps_y = int_of_float (Float.abs vy /. step) + 1 in
    let dy = vy /. float_of_int steps_y in
    p.on_ground <- false;
    for _ = 1 to steps_y do
      let new_y = p.pos.y +. dy in
      let test_bbox = (p.pos.x -. half_w, new_y, Player.width, Player.height) in
      if bbox_blocked level extra_solid crates test_bbox then begin
        if dy < 0. then p.on_ground <- true;
        p.vel <- { p.vel with Vec2.y = 0. }
      end
      else p.pos <- { p.pos with Vec2.y = new_y }
    done
  end

(** Move a crate one frame: horizontal decay + gravity, vertical sweep only. *)
let move_crate level extra_solid crates (c : Entities.crate) =
  c.vel <- { c.vel with Vec2.x = c.vel.x *. Tuning.crate_friction };
  let new_vy = c.vel.y -. Tuning.gravity in
  c.vel <- { c.vel with Vec2.y = Float.max (-.Tuning.max_fall) new_vy };
  let step = 1.0 in
  let vy = c.vel.y in
  let steps_y = int_of_float (Float.abs vy /. step) + 1 in
  let dy = vy /. float_of_int steps_y in
  for _ = 1 to steps_y do
    let new_y = c.pos.y +. dy in
    let test_bbox = (c.pos.x, new_y, c.w, c.h) in
    if bbox_blocked level extra_solid crates ~ignore_crate:(Some c) test_bbox
    then c.vel <- { c.vel with Vec2.y = 0. }
    else c.pos <- { c.pos with Vec2.y = new_y }
  done

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
  if not p.alive then p.at_door <- false
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
