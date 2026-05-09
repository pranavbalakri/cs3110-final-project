(** Player character state and per-frame update functions.

    A player is a mutable record holding position, velocity, and various
    gameplay flags.  {!apply_input} and {!apply_gravity} advance the
    logical state each frame; the [Physics] module then sweeps the
    resulting velocity against the tile grid and entities. *)

(** {1 Ground surface classification} *)

(** The type of surface under the player's feet, determined each frame by
    {!Physics.compute_ground_surface} and stored in the player record.
    It influences horizontal velocity computation in {!apply_input}. *)
type ground_surface =
  | Normal
      (** Standard friction: horizontal velocity is set directly from input. *)
  | Ice
      (** Low friction: velocity decays by {!Tuning.ice_friction} per frame when no
          directional input is held. *)
  | Conveyor_belt of float
      (** The signed extra velocity (px/frame) added by the belt regardless of
          player input.  Negative = leftward, positive = rightward. *)

(** {1 Player record} *)

(** Mutable state for one player character. *)
type t = {
  kind : Types.player_kind;
      (** Identity; determines which tiles are hazardous and which door to use. *)
  mutable pos : Vec2.t;
      (** World-space position of the bottom-centre of the bounding box. *)
  mutable vel : Vec2.t;
      (** Velocity in pixels per frame (x = horizontal, y = vertical/upward). *)
  mutable on_ground : bool;
      (** True iff the player's feet touched solid ground during the last sweep. *)
  mutable coyote_left : int;
      (** Frames remaining in the coyote-time window after walking off a ledge. *)
  mutable jump_buffer_left : int;
      (** Frames remaining in the jump-buffer window after pressing jump. *)
  mutable jump_hold_left : int;
      (** Frames of jump-hold boost remaining on the current jump. *)
  mutable alive : bool;
      (** False once the player contacts a hazard; dead players are not updated. *)
  mutable at_door : bool;
      (** True when the player's centre overlaps their exit door tile. *)
  mutable diamonds : int;
      (** Count of diamonds collected this level run. *)
  mutable ground_surface : ground_surface;
      (** Surface classification written by [Physics.compute_ground_surface]. *)
  mutable in_fan : bool;
      (** True if the player is inside an active fan column this frame. *)
  mutable teleport_cooldown : float;
      (** Seconds remaining before the player can use a teleporter again. *)
  mutable facing : Types.direction;
      (** Horizontal direction the player is currently facing. *)
}

(** {1 Constants} *)

(** Width of the player bounding box in pixels (derived from {!Tuning.player_w}). *)
val width : float

(** Height of the player bounding box in pixels (derived from {!Tuning.player_h}). *)
val height : float

(** {1 Construction} *)

(** [create kind spawn_pos] allocates a fresh player of the given [kind] at
    [spawn_pos] with all counters and flags at their default starting values. *)
val create : Types.player_kind -> Vec2.t -> t

(** {1 Bounding box} *)

(** [bbox p] returns [(x, y, w, h)] — the axis-aligned bounding rectangle of
    [p] in world space, where [(x, y)] is the top-left corner. *)
val bbox : t -> float * float * float * float

(** [center p] returns the world-space centre point of [p]'s bounding box. *)
val center : t -> Vec2.t

(** {1 Per-frame updates} *)

(** [apply_input p inp] reads [inp] and updates [p.vel] and the coyote /
    jump-buffer / jump-hold counters accordingly.  Has no effect when [p.alive]
    is false. *)
val apply_input : t -> Input.player_input -> unit

(** [apply_gravity p] applies downward acceleration (or fan lift when [p.in_fan]
    is true) to [p.vel.y], clamping to {!Tuning.max_fall}.  Has no effect when
    [p.alive] is false. *)
val apply_gravity : t -> unit

(** {1 Reset} *)

(** [reset p spawn_pos] restores [p] to its initial state at [spawn_pos],
    clearing all counters, flags, and the diamond count. *)
val reset : t -> Vec2.t -> unit
