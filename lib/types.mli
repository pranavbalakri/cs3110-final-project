(** Shared enumeration types used throughout the game.

    This module is a pure type namespace — it defines no values.
    Modules that need these types open or qualify them as [Types.foo]. *)

(** {1 Tile types} *)

(** Classification of every cell in the tile grid. *)
type tile =
  | Empty             (** Passable air cell. *)
  | Wall              (** Solid, impassable block. *)
  | Fire              (** Lethal to Watergirl; harmless to Fireboy. *)
  | Water             (** Lethal to Fireboy; harmless to Watergirl. *)
  | Goo               (** Lethal to both players. *)
  | Fireboy_door      (** Fireboy's exit door tile. *)
  | Watergirl_door    (** Watergirl's exit door tile. *)
  | Ice               (** Low-friction surface: horizontal velocity decays slowly. *)
  | Conveyor_left     (** Pushes the standing entity leftward each frame. *)
  | Conveyor_right    (** Pushes the standing entity rightward each frame. *)
  | Spikes            (** Lethal to both players on contact. *)
  | Slope_up          (** 45-degree ramp rising from left to right (/ shape). *)
  | Slope_down        (** 45-degree ramp falling from left to right (\ shape). *)

(** {1 Player identity} *)

(** Distinguishes the two controllable characters. *)
type player_kind =
  | Fireboy    (** The fire character (WASD controls). *)
  | Watergirl  (** The water character (IJKL / IJL controls). *)

(** {1 Direction} *)

(** Horizontal facing direction, used for sprite orientation and conveyor logic. *)
type direction =
  | Left
  | Right

(** {1 Signal identifiers} *)

(** Unique string key that names an emitter/listener channel in the signal graph. *)
type signal_id = string

(** {1 Animation state} *)

(** Current animation clip playing for a player character. *)
type anim_state =
  | Idle
  | Walk
  | Jump
  | Fall
  | Die

(** {1 Game status} *)

(** Overall game state that drives the main loop's branching. *)
type status =
  | Playing  (** Normal gameplay. *)
  | Won      (** Both players reached their doors; level complete. *)
  | Lost     (** One or both players died; brief death timer before reset. *)
  | Paused   (** Game is paused (unused in current UI, reserved). *)
