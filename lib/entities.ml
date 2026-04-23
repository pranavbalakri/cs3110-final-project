(** Runtime entity state for Phase 4 interactables. *)

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

(* ── Constructors from level specs ──────────────────────────────────────── *)

let button_of_spec (s : Level.button_spec) : button =
  let ts = float_of_int Tuning.tile_size in
  let x = float_of_int s.col *. ts in
  let y = float_of_int s.row *. ts in
  (* Button is a thin plate (1/5 tile tall) at the bottom of the tile cell *)
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
    h = ts /. 2.;   (* half-tile thick platform *)
    point_a = { Vec2.x = xa; y = ya };
    point_b = { Vec2.x = xb; y = yb };
    speed = s.speed;
    going_to_b = true;
    frame_delta = Vec2.zero;
  }

(* ── Bounding boxes ─────────────────────────────────────────────────────── *)

let bbox_of_button (b : button) = (b.pos.x, b.pos.y, b.w, b.h)
let bbox_of_gate (g : gate) = (g.pos.x, g.pos.y, g.w, g.h)
let bbox_of_elevator (e : elevator) = (e.pos.x, e.pos.y, e.w, e.h)

let rects_overlap (x1, y1, w1, h1) (x2, y2, w2, h2) =
  x1 < x2 +. w2 && x1 +. w1 > x2 && y1 < y2 +. h2 && y1 +. h1 > y2

(* ── Per-frame update functions ─────────────────────────────────────────── *)

(** Emit button's signal this frame if any alive player overlaps it. *)
let update_button (b : button) (players : Player.t list) (signals : Signals.table) =
  let b_bbox = bbox_of_button b in
  let pressed =
    List.exists
      (fun (p : Player.t) -> p.alive && rects_overlap b_bbox (Player.bbox p))
      players
  in
  b.pressed <- pressed;
  if pressed then Signals.emit signals b.id

(** Toggle lever state on interact if any alive player is within reach; always
    emit signal while state is true. *)
let update_lever
    (lv : lever)
    (players : Player.t list)
    interact_pressed
    (signals : Signals.table) =
  if interact_pressed then begin
    let ts = float_of_int Tuning.tile_size in
    (* Interact zone: one tile wide, two tiles tall, centred on lever tile *)
    let zone = (lv.pos.x -. ts /. 4., lv.pos.y, ts *. 1.5, ts *. 2.) in
    let near =
      List.exists
        (fun (p : Player.t) -> p.alive && rects_overlap zone (Player.bbox p))
        players
    in
    if near then lv.state <- not lv.state
  end;
  if lv.state then Signals.emit signals lv.id

(** Open/close gate based on current signal snapshot. *)
let update_gate (g : gate) (signals : Signals.table) =
  g.is_open <- Signals.eval signals g.listener

(** Advance elevator one timestep; record [frame_delta] for platform riding. *)
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

(** True if [p] is standing on top of elevator [e] (within 3 px tolerance). *)
let player_on_elevator (e : elevator) (p : Player.t) =
  if not p.on_ground then false
  else
    let (px, py, pw, _) = Player.bbox p in
    let elev_top = e.pos.y +. e.h in
    abs_float (py -. elev_top) < 3.0
    && px < e.pos.x +. e.w
    && px +. pw > e.pos.x

(** Translate a player by their elevator's frame delta before physics runs. *)
let apply_elevator_riding (elevators : elevator array) (p : Player.t) =
  if not p.alive then ()
  else
    Array.iter
      (fun e ->
        if player_on_elevator e p then
          p.pos <- Vec2.add p.pos e.frame_delta)
      elevators

(* ── Reset ──────────────────────────────────────────────────────────────── *)

let reset_lever lv = lv.state <- false

let reset_elevator e =
  e.pos <- e.point_a;
  e.going_to_b <- true;
  e.frame_delta <- Vec2.zero
