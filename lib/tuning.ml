let tile_size = 40
let player_w = 26
let player_h = 34

let gravity = 0.48
let jump_impulse = 11.5
let walk_speed = 3.6
let coyote_frames = 6
let jump_buffer_frames = 6
let max_fall = 10.0

(* Phase 5 — surface + entity tuning *)

(** Per-frame multiplier applied to horizontal velocity on ice when no input. *)
let ice_friction = 0.94

(** Extra horizontal velocity (px/frame) applied while standing on a conveyor. *)
let conveyor_speed = 2.5

(** Per-frame upward impulse while inside an active fan column. *)
let fan_lift = 1.2

(** Cap on upward velocity produced by a fan. *)
let fan_max_up_vy = 6.0

(** Pushable-crate dimensions (pixels). *)
let crate_w = 30
let crate_h = 30

(** Per-frame horizontal velocity decay for a free crate. *)
let crate_friction = 0.7

(** Teleport cooldown (seconds) to prevent oscillation after a port. *)
let teleport_cooldown = 0.4
