(** Runtime entity state and per-frame update logic for interactive world objects.

    Each entity type is constructed from the corresponding [Level.*_spec] record
    via a [*_of_spec] constructor.  Per-frame update functions mutate entity
    state and interact with the {!Signals} table, {!Player} records, and each
    other as needed. *)

(** {1 Entity types} *)

(** Pressure-plate button: emits its [id] into the signal table every frame
    that any alive player or crate overlaps its bounding box. *)
type button = {
  id : Types.signal_id;    (** Signal emitted while pressed. *)
  pos : Vec2.t;            (** Bottom-left corner in world space. *)
  w : float;               (** Width in pixels. *)
  h : float;               (** Height in pixels. *)
  mutable pressed : bool;  (** True iff a player or crate overlapped last frame. *)
}

(** Toggle lever: flips [state] on each interact-key press by a nearby player;
    emits [id] into the signal table every frame that [state] is [true]. *)
type lever = {
  id : Types.signal_id;   (** Signal emitted while the lever is active. *)
  pos : Vec2.t;           (** Bottom-left corner of the lever tile in world space. *)
  mutable state : bool;   (** Current toggle state; [true] = signal is high. *)
}

(** Gate: solid rectangular obstacle that opens (becomes passable) when
    its [listener] expression evaluates to [true]. *)
type gate = {
  listener : Signals.expr;  (** Boolean condition controlling the gate. *)
  pos : Vec2.t;             (** Bottom-left corner in world space. *)
  w : float;                (** Width in pixels. *)
  h : float;                (** Height in pixels. *)
  mutable is_open : bool;   (** True iff the gate is currently passable. *)
}

(** Elevator: solid platform that travels between [point_a] and [point_b]
    at a fixed speed; players standing on top ride with it. *)
type elevator = {
  mutable pos : Vec2.t;    (** Current bottom-left corner in world space. *)
  w : float;               (** Width of the platform in pixels. *)
  h : float;               (** Thickness of the platform in pixels. *)
  point_a : Vec2.t;        (** First endpoint position. *)
  point_b : Vec2.t;        (** Second endpoint position. *)
  speed : float;           (** Movement speed in pixels per second. *)
  mutable going_to_b : bool;
      (** True when travelling toward [point_b]; toggles at each endpoint. *)
  mutable frame_delta : Vec2.t;
      (** Displacement this frame; added to riding players before physics. *)
}

(** Pushable crate: a gravity-subject rigid box that players can shove
    horizontally by walking into it. *)
type crate = {
  mutable pos : Vec2.t;  (** Bottom-left corner in world space. *)
  mutable vel : Vec2.t;  (** Current velocity in pixels per frame. *)
  w : float;             (** Width in pixels. *)
  h : float;             (** Height in pixels. *)
  spawn : Vec2.t;        (** Initial spawn position used for reset. *)
}

(** Teleporter pair: bidirectional instant travel between two tile-sized zones.
    A player entering tile [a] is relocated to tile [b] and vice-versa. *)
type teleporter_pair = {
  a : Vec2.t;         (** Bottom-left corner of portal A in world space. *)
  b : Vec2.t;         (** Bottom-left corner of portal B in world space. *)
  tile_w : float;     (** Width of each portal tile. *)
  tile_h : float;     (** Height of each portal tile. *)
}

(** Fan: an upward-lift column that activates based on a signal expression.
    While on, any player inside [fan_column_rect] has their vertical velocity
    modified by {!Tuning.fan_lift}. *)
type fan = {
  listener : Signals.expr;   (** Condition controlling whether the fan is on. *)
  col : int;                 (** Tile column of the fan base. *)
  base_row : int;            (** Tile row of the fan base. *)
  height_tiles : int;        (** Number of tiles the lift column extends upward. *)
  mutable is_on : bool;      (** True iff the fan is currently active. *)
}

(** {1 Constructors from level specs} *)

(** [button_of_spec s] constructs a runtime button from a level specification. *)
val button_of_spec : Level.button_spec -> button

(** [lever_of_spec s] constructs a runtime lever from a level specification. *)
val lever_of_spec : Level.lever_spec -> lever

(** [gate_of_spec s] constructs a runtime gate from a level specification.
    A single [listener_id] becomes a {!Signals.Lit}; multiple ids become
    {!Signals.Any} over the individual literals. *)
val gate_of_spec : Level.gate_spec -> gate

(** [elev_of_spec s] constructs a runtime elevator from a level specification. *)
val elev_of_spec : Level.elev_spec -> elevator

(** [crate_of_spec s] constructs a runtime crate from a level specification,
    centring it horizontally within its spawn tile. *)
val crate_of_spec : Level.crate_spec -> crate

(** [tele_of_spec s] constructs a teleporter pair from a level specification. *)
val tele_of_spec : Level.tele_spec -> teleporter_pair

(** [fan_of_spec s] constructs a runtime fan from a level specification.
    An empty [listener_ids] list compiles to {!Signals.All}[[]] (always true). *)
val fan_of_spec : Level.fan_spec -> fan

(** {1 Bounding boxes} *)

(** [(x, y, w, h)] bounding rectangle of a button in world space. *)
val bbox_of_button : button -> float * float * float * float

(** [(x, y, w, h)] bounding rectangle of a gate in world space. *)
val bbox_of_gate : gate -> float * float * float * float

(** [(x, y, w, h)] bounding rectangle of an elevator platform in world space. *)
val bbox_of_elevator : elevator -> float * float * float * float

(** [(x, y, w, h)] bounding rectangle of a crate in world space. *)
val bbox_of_crate : crate -> float * float * float * float

(** [(x, y, w, h)] bounding rectangle of portal A of a teleporter pair. *)
val bbox_of_tele_a : teleporter_pair -> float * float * float * float

(** [(x, y, w, h)] bounding rectangle of portal B of a teleporter pair. *)
val bbox_of_tele_b : teleporter_pair -> float * float * float * float

(** [fan_column_rect f] returns the [(x, y, w, h)] rectangle of the lift
    column above [f]'s base tile, in world space. *)
val fan_column_rect : fan -> float * float * float * float

(** [fan_base_rect f] returns the [(x, y, w, h)] rectangle of the fan's
    base tile, used for rendering the fan housing. *)
val fan_base_rect : fan -> float * float * float * float

(** {1 Per-frame update functions} *)

(** [update_button b players crates signals] emits [b.id] into [signals] if any
    alive player or any crate overlaps [b].  Also updates [b.pressed]. *)
val update_button :
  button ->
  Player.t list ->
  crate list ->
  Signals.table ->
  unit

(** [update_lever lv players_with_interact signals] toggles [lv.state] when an
    alive player presses the interact key while overlapping [lv]'s zone, then
    emits [lv.id] into [signals] if [lv.state] is [true].

    [players_with_interact] pairs each player with a boolean that is [true] iff
    that player pressed the interact key this frame (edge-triggered). *)
val update_lever :
  lever ->
  (Player.t * bool) list ->
  Signals.table ->
  unit

(** [update_gate g signals] evaluates [g.listener] against [signals] and sets
    [g.is_open] accordingly.  Must be called after all emitters have run. *)
val update_gate : gate -> Signals.table -> unit

(** [update_elevator e dt] advances [e] toward its current target endpoint by
    [e.speed * dt] pixels, records [e.frame_delta], and reverses direction
    upon arrival.  [dt] is in seconds. *)
val update_elevator : elevator -> float -> unit

(** [player_on_elevator e p] returns [true] iff [p] is standing on top of [e]
    this frame (feet within 3 pixels of the elevator's top surface). *)
val player_on_elevator : elevator -> Player.t -> bool

(** [apply_elevator_riding elevators p] translates [p.pos] by the [frame_delta]
    of every elevator that [p] is riding.  Must be called before physics. *)
val apply_elevator_riding : elevator array -> Player.t -> unit

(** [tick_teleport_cooldowns players dt] decrements each player's
    [teleport_cooldown] by [dt] seconds, clamping at zero. *)
val tick_teleport_cooldowns : Player.t list -> float -> unit

(** [update_teleporter tp players] checks each player against both portals of
    [tp] and teleports any player whose bounding box overlaps a portal tile,
    provided their [teleport_cooldown] is zero. *)
val update_teleporter : teleporter_pair -> Player.t list -> unit

(** [update_fan f signals] evaluates [f.listener] and sets [f.is_on]. *)
val update_fan : fan -> Signals.table -> unit

(** [apply_fan_to_player f p] sets [p.in_fan] to [true] if [f] is on and [p]'s
    bounding box overlaps the fan's column rectangle. *)
val apply_fan_to_player : fan -> Player.t -> unit

(** {1 Reset functions} *)

(** [reset_lever lv] sets [lv.state] back to [false]. *)
val reset_lever : lever -> unit

(** [reset_elevator e] returns [e] to [point_a] heading toward [point_b]. *)
val reset_elevator : elevator -> unit

(** [reset_crate c] returns [c] to its spawn position with zero velocity. *)
val reset_crate : crate -> unit

(** [reset_fan f] sets [f.is_on] to [false]. *)
val reset_fan : fan -> unit
