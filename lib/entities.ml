(** Runtime entity state for Phase 4 interactables + Phase 5 world elements. *)

(* ── Types ─────────────────────────────────────────────────────────────── *)

(** Pressure-plate button: emits [id] each frame while any player overlaps. *)
type button = {
  id : Types.signal_id;
  pos : Vec2.t;   (** bottom-left, world coords *)
  w : float;
  h : float;
  mutable pressed : bool;
}

(** Lever: edge-triggered toggle that holds [id] high while [state = true]. *)
type lever = {
  id : Types.signal_id;
  pos : Vec2.t;
  mutable state : bool;
}

(** Gate: solid rectangle; passable (open) when [listener] evaluates true. *)
type gate = {
  listener : Signals.expr;
  pos : Vec2.t;
  w : float;
  h : float;
  mutable is_open : bool;
}

(** Elevator: solid platform that shuttles between [point_a] and [point_b]. *)
type elevator = {
  mutable pos : Vec2.t;
  w : float;
  h : float;      (** platform thickness *)
  point_a : Vec2.t;
  point_b : Vec2.t;
  speed : float;  (** pixels / second *)
  mutable going_to_b : bool;
  mutable frame_delta : Vec2.t;
}

(** Pushable crate: gravity-subject, rigid, one-level pushing. *)
type crate = {
  mutable pos : Vec2.t;
  mutable vel : Vec2.t;
  w : float;
  h : float;
  spawn : Vec2.t;
}

(** Teleporter pair: entering tile [a] relocates entity to tile [b] (and v.v.). *)
type teleporter_pair = {
  a : Vec2.t;       (** tile bottom-left *)
  b : Vec2.t;
  tile_w : float;
  tile_h : float;
}

(** Fan: upward column rising above a base tile while [listener] is true. *)
type fan = {
  listener : Signals.expr;
  col : int;
  base_row : int;
  height_tiles : int;
  mutable is_on : bool;
}

(* ── Constructors from level specs ──────────────────────────────────────── *)

let button_of_spec (s : Level.button_spec) : button =
  let ts = float_of_int Tuning.tile_size in
  let x = float_of_int s.col *. ts in
  let y = float_of_int s.row *. ts in
  { id = s.id; pos = { Vec2.x; y }; w = ts; h = ts /. 5.; pressed = false }

let lever_of_spec (s : Level.lever_spec) : lever =
  let ts = float_of_int Tuning.tile_size in
  let x = float_of_int s.col *. ts in
  let y = float_of_int s.row *. ts in
  { id = s.id; pos = { Vec2.x; y }; state = false }

let gate_of_spec (s : Level.gate_spec) : gate =
  let ts = float_of_int Tuning.tile_size in
  let x = float_of_int s.col *. ts in
  let y = float_of_int s.row *. ts in
  let listener =
    match s.listener_ids with
    | [ id ] -> Signals.Lit id
    | ids -> Signals.Any (List.map (fun id -> Signals.Lit id) ids)
  in
  {
    listener;
    pos = { Vec2.x; y };
    w = float_of_int s.w_tiles *. ts;
    h = float_of_int s.h_tiles *. ts;
    is_open = false;
  }

let elev_of_spec (s : Level.elev_spec) : elevator =
  let ts = float_of_int Tuning.tile_size in
  let xa = float_of_int s.col_a *. ts in
  let ya = float_of_int s.row_a *. ts in
  let xb = float_of_int s.col_b *. ts in
  let yb = float_of_int s.row_b *. ts in
  {
    pos = { Vec2.x = xa; y = ya };
    w = float_of_int s.w_tiles *. ts;
    h = ts /. 2.;
    point_a = { Vec2.x = xa; y = ya };
    point_b = { Vec2.x = xb; y = yb };
    speed = s.speed;
    going_to_b = true;
    frame_delta = Vec2.zero;
  }

let crate_of_spec (s : Level.crate_spec) : crate =
  let ts = float_of_int Tuning.tile_size in
  let cw = float_of_int Tuning.crate_w in
  let ch = float_of_int Tuning.crate_h in
  let x = (float_of_int s.col *. ts) +. ((ts -. cw) /. 2.) in
  let y = float_of_int s.row *. ts in
  { pos = { Vec2.x; y }; vel = Vec2.zero; w = cw; h = ch; spawn = { Vec2.x; y } }

let tele_of_spec (s : Level.tele_spec) : teleporter_pair =
  let ts = float_of_int Tuning.tile_size in
  {
    a = { Vec2.x = float_of_int s.col_a *. ts; y = float_of_int s.row_a *. ts };
    b = { Vec2.x = float_of_int s.col_b *. ts; y = float_of_int s.row_b *. ts };
    tile_w = ts;
    tile_h = ts;
  }

let fan_of_spec (s : Level.fan_spec) : fan =
  let listener =
    match s.listener_ids with
    | [] -> Signals.All []           (* [] ⇒ always true ⇒ always on *)
    | [ id ] -> Signals.Lit id
    | ids -> Signals.Any (List.map (fun id -> Signals.Lit id) ids)
  in
  {
    listener;
    col = s.col;
    base_row = s.row;
    height_tiles = s.height_tiles;
    is_on = false;
  }

(* ── Bounding boxes ─────────────────────────────────────────────────────── *)

let bbox_of_button (b : button) = (b.pos.x, b.pos.y, b.w, b.h)
let bbox_of_gate (g : gate) = (g.pos.x, g.pos.y, g.w, g.h)
let bbox_of_elevator (e : elevator) = (e.pos.x, e.pos.y, e.w, e.h)
let bbox_of_crate (c : crate) = (c.pos.x, c.pos.y, c.w, c.h)

let bbox_of_tele_a (t : teleporter_pair) = (t.a.x, t.a.y, t.tile_w, t.tile_h)
let bbox_of_tele_b (t : teleporter_pair) = (t.b.x, t.b.y, t.tile_w, t.tile_h)

let fan_column_rect (f : fan) =
  let ts = float_of_int Tuning.tile_size in
  let x = float_of_int f.col *. ts in
  let y = float_of_int (f.base_row + 1) *. ts in
  let w = ts in
  let h = float_of_int f.height_tiles *. ts in
  (x, y, w, h)

let fan_base_rect (f : fan) =
  let ts = float_of_int Tuning.tile_size in
  let x = float_of_int f.col *. ts in
  let y = float_of_int f.base_row *. ts in
  (x, y, ts, ts)

let rects_overlap (x1, y1, w1, h1) (x2, y2, w2, h2) =
  x1 < x2 +. w2 && x1 +. w1 > x2 && y1 < y2 +. h2 && y1 +. h1 > y2

(* ── Per-frame update functions ─────────────────────────────────────────── *)

(** Emit button's signal if any alive player or crate overlaps it. *)
let update_button
    (b : button)
    (players : Player.t list)
    (crates : crate list)
    (signals : Signals.table) =
  let b_bbox = bbox_of_button b in
  let by_player =
    List.exists
      (fun (p : Player.t) -> p.alive && rects_overlap b_bbox (Player.bbox p))
      players
  in
  let by_crate =
    List.exists (fun c -> rects_overlap b_bbox (bbox_of_crate c)) crates
  in
  let pressed = by_player || by_crate in
  b.pressed <- pressed;
  if pressed then Signals.emit signals b.id

let update_lever
    (lv : lever)
    (players : Player.t list)
    interact_pressed
    (signals : Signals.table) =
  if interact_pressed then begin
    let ts = float_of_int Tuning.tile_size in
    let zone = (lv.pos.x -. ts /. 4., lv.pos.y, ts *. 1.5, ts *. 2.) in
    let near =
      List.exists
        (fun (p : Player.t) -> p.alive && rects_overlap zone (Player.bbox p))
        players
    in
    if near then lv.state <- not lv.state
  end;
  if lv.state then Signals.emit signals lv.id

let update_gate (g : gate) (signals : Signals.table) =
  g.is_open <- Signals.eval signals g.listener

let update_elevator (e : elevator) dt =
  let target = if e.going_to_b then e.point_b else e.point_a in
  let dx = target.x -. e.pos.x in
  let dy = target.y -. e.pos.y in
  let dist = sqrt ((dx *. dx) +. (dy *. dy)) in
  if dist < 1.0 then begin
    e.pos <- target;
    e.going_to_b <- not e.going_to_b;
    e.frame_delta <- Vec2.zero
  end else begin
    let step = Float.min (e.speed *. dt) dist in
    let nx = e.pos.x +. (dx /. dist *. step) in
    let ny = e.pos.y +. (dy /. dist *. step) in
    e.frame_delta <- { Vec2.x = nx -. e.pos.x; y = ny -. e.pos.y };
    e.pos <- { Vec2.x = nx; y = ny }
  end

let player_on_elevator (e : elevator) (p : Player.t) =
  if not p.on_ground then false
  else
    let (px, py, pw, _) = Player.bbox p in
    let elev_top = e.pos.y +. e.h in
    abs_float (py -. elev_top) < 3.0
    && px < e.pos.x +. e.w
    && px +. pw > e.pos.x

let apply_elevator_riding (elevators : elevator array) (p : Player.t) =
  if not p.alive then ()
  else
    Array.iter
      (fun e ->
        if player_on_elevator e p then
          p.pos <- Vec2.add p.pos e.frame_delta)
      elevators

(** Advance all per-player teleport cooldowns by [dt]. *)
let tick_teleport_cooldowns (players : Player.t list) dt =
  List.iter
    (fun (p : Player.t) ->
      if p.teleport_cooldown > 0. then
        p.teleport_cooldown <- Float.max 0. (p.teleport_cooldown -. dt))
    players

(** Relocate the player's bbox so its centre ends up in the middle of tile
    [(tx, ty)] (where [tx,ty] is the tile's bottom-left corner). *)
let teleport_player_to (p : Player.t) (dest : Vec2.t) (tile_w : float) =
  p.pos <- { Vec2.x = dest.x +. (tile_w /. 2.); y = dest.y +. 2. };
  p.teleport_cooldown <- Tuning.teleport_cooldown

let update_teleporter (tp : teleporter_pair) (players : Player.t list) =
  List.iter
    (fun (p : Player.t) ->
      if (not p.alive) || p.teleport_cooldown > 0. then ()
      else
        let pbox = Player.bbox p in
        if rects_overlap pbox (bbox_of_tele_a tp) then
          teleport_player_to p tp.b tp.tile_w
        else if rects_overlap pbox (bbox_of_tele_b tp) then
          teleport_player_to p tp.a tp.tile_w)
    players

let update_fan (f : fan) (signals : Signals.table) =
  f.is_on <- Signals.eval signals f.listener

(** If [f] is on and [p] overlaps its column, mark [p.in_fan]. *)
let apply_fan_to_player (f : fan) (p : Player.t) =
  if (not p.alive) || not f.is_on then ()
  else if rects_overlap (fan_column_rect f) (Player.bbox p) then
    p.in_fan <- true

(* ── Reset ──────────────────────────────────────────────────────────────── *)

let reset_lever lv = lv.state <- false

let reset_elevator (e : elevator) =
  e.pos <- e.point_a;
  e.going_to_b <- true;
  e.frame_delta <- Vec2.zero

let reset_crate (c : crate) =
  c.pos <- c.spawn;
  c.vel <- Vec2.zero

let reset_fan (f : fan) = f.is_on <- false
