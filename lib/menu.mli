(** Level-select menu.

    The menu displays a scrollable list of all entries in {!Level.levels}
    and lets the player highlight and confirm a choice.  Navigation and
    selection are driven by the caller (typically [bin/main.ml]) which
    polls input and calls {!move_up}, {!move_down}, or reads
    {!selected_level} when the confirm key is pressed. *)

(** {1 Type} *)

(** Mutable menu state holding the currently highlighted cursor index. *)
type t = {
  mutable cursor : int;
      (** Index into {!Level.levels} of the highlighted entry.
          Clamped to [[0, num_levels - 1]]. *)
}

(** {1 Constants} *)

(** Total number of selectable levels, equal to [Array.length Level.levels]. *)
val num_levels : int

(** {1 Construction} *)

(** [create ()] returns a fresh menu with the cursor on the first entry. *)
val create : unit -> t

(** {1 Navigation} *)

(** [move_up m] moves the cursor to the previous entry; no-ops at the top. *)
val move_up : t -> unit

(** [move_down m] moves the cursor to the next entry; no-ops at the bottom. *)
val move_down : t -> unit

(** {1 Selection} *)

(** [selected_level m] returns the {!Level.level_data} for the currently
    highlighted cursor position. *)
val selected_level : t -> Level.level_data

(** {1 Rendering} *)

(** [draw m] renders the full menu screen to the current frame:
    the game title, a subtitle, and a highlighted button for each level.
    Also renders navigation hint text at the bottom of the screen. *)
val draw : t -> unit
