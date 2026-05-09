(** Central repository of all numeric gameplay constants.

    Changing a value here propagates throughout the game without
    touching any other module.  All distances are in pixels, all times
    are in frames unless noted otherwise. *)

(** {1 World geometry} *)

(** Side length of one tile in pixels. *)
val tile_size : int

(** {1 Player dimensions (pixels)} *)

(** Width of the player bounding box. *)
val player_w : int

(** Height of the player bounding box. *)
val player_h : int

(** {1 Player movement} *)

(** Downward acceleration applied each frame when not in a fan column (px/frame²). *)
val gravity : float

(** Upward velocity set on the frame a jump begins (px/frame). *)
val jump_impulse : float

(** Number of frames the jump-hold boost lasts after the initial jump frame. *)
val jump_hold_frames : int

(** Extra upward velocity added per frame while the jump key is held (px/frame). *)
val jump_hold_boost : float

(** Horizontal speed while a directional key is held (px/frame). *)
val walk_speed : float

(** Number of frames after leaving a ledge during which a jump is still allowed. *)
val coyote_frames : int

(** Number of frames before landing during which a pressed jump is buffered. *)
val jump_buffer_frames : int

(** Maximum downward speed (px/frame); downward velocity is clamped to this. *)
val max_fall : float

(** {1 Surface modifiers} *)

(** Per-frame horizontal velocity multiplier applied on ice when no input is held. *)
val ice_friction : float

(** Horizontal velocity (px/frame) added while standing on a conveyor tile. *)
val conveyor_speed : float

(** {1 Fan physics} *)

(** Upward impulse added per frame while inside an active fan column (px/frame). *)
val fan_lift : float

(** Maximum upward velocity a fan can produce (px/frame). *)
val fan_max_up_vy : float

(** {1 Crate physics} *)

(** Width of a pushable crate bounding box (pixels). *)
val crate_w : int

(** Height of a pushable crate bounding box (pixels). *)
val crate_h : int

(** Per-frame horizontal velocity decay multiplier for a free-sliding crate. *)
val crate_friction : float

(** {1 Teleporter cooldown} *)

(** Seconds a player must wait after teleporting before they can teleport again.
    Prevents oscillation when the player remains overlapping the destination tile. *)
val teleport_cooldown : float
