(** Collectible diamond pickups.

    Each diamond has a kind ({!Types.Fireboy} or {!Types.Watergirl}); only the
    matching player can collect it.  Once collected the diamond is hidden and
    awards one point to that player's {!Player.t.diamonds} counter. *)

(** {1 Type} *)

(** Runtime state of one diamond. *)
type t = {
  pos : Vec2.t;
      (** World-space centre of the diamond. *)
  kind : Types.player_kind;
      (** Which player may collect this diamond. *)
  mutable collected : bool;
      (** True once the diamond has been picked up; it is skipped in all checks. *)
}

(** {1 Constants} *)

(** Side length of the diamond's square bounding box in pixels. *)
val size : float

(** {1 Construction} *)

(** [from_spec spec] converts a {!Level.diamond_spec} into a runtime diamond,
    computing the world-space centre from the tile coordinates in [spec]. *)
val from_spec : Level.diamond_spec -> t

(** {1 Bounding box} *)

(** [bbox d] returns [(x, y, w, h)] — the axis-aligned bounding rectangle of
    [d] in world space. *)
val bbox : t -> float * float * float * float

(** {1 Per-frame update} *)

(** [try_collect d p] checks whether player [p] overlaps [d] and, if so, marks
    [d] as collected and increments [p.diamonds].

    The call is a no-op when any of the following hold:
    {ul
      {li [d.collected] is already true.}
      {li [d.kind <> p.kind] (wrong player).}
      {li [p.alive] is false.}
    } *)
val try_collect : t -> Player.t -> unit

(** {1 Reset} *)

(** [reset d] marks [d] as uncollected, making it available again.
    Called when the level is restarted. *)
val reset : t -> unit
