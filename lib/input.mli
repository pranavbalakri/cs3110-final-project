(** Raw input snapshot for one game frame.

    Call {!poll} once per frame (outside the drawing phase) to capture
    the current keyboard state.  The snapshot is then passed down into
    the update functions; no module should call [Gfx] key functions
    directly. *)

(** {1 Per-player input} *)

(** Keyboard state for a single player, sampled at the start of a frame. *)
type player_input = {
  left : bool;
      (** Directional key is currently held. *)
  right : bool;
      (** Directional key is currently held. *)
  jump : bool;
      (** Jump key is currently held (used for jump-hold boost). *)
  jump_pressed : bool;
      (** Jump key was pressed this frame (edge-triggered; feeds jump buffer). *)
  interact_pressed : bool;
      (** Interact key was pressed this frame (edge-triggered; toggles levers). *)
}

(** {1 Full-frame input snapshot} *)

(** Combined input for both players plus global controls. *)
type t = {
  fireboy : player_input;
      (** Input for Fireboy (WASD + E). *)
  watergirl : player_input;
      (** Input for Watergirl (IJKL + O). *)
  reset : bool;
      (** [true] if R was pressed this frame; triggers an immediate level reset. *)
  debug_toggle : bool;
      (** [true] if F3 was pressed this frame; toggles the signal debug overlay. *)
}

(** {1 Polling} *)

(** [poll ()] samples the keyboard and returns an immutable snapshot of the
    current frame's input state.  Must be called once per frame, before
    any update or render functions. *)
val poll : unit -> t
