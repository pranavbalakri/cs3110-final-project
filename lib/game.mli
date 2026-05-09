(** Top-level game state and the frame-by-frame update function.

    [Game.t] owns every runtime object for one level run: the two players,
    all entity arrays, the signal table, and gameplay flags.  [init] creates
    a fresh state from a {!Level.level_data}; [update] advances it by one
    logical frame; [reset] restores everything to initial conditions without
    re-allocating. *)

(** {1 Game state} *)

(** Complete mutable state for one level run. *)
type t = {
  level : Level.level_data;
      (** The loaded level (immutable tile grid + specs). *)
  firecaml : Player.t;
      (** Firecaml's runtime player record. *)
  watercaml : Player.t;
      (** Watercaml's runtime player record. *)
  diamonds : Diamond.t array;
      (** All collectible diamonds in the level. *)
  buttons : Entities.button array;
      (** Pressure-plate buttons. *)
  levers : Entities.lever array;
      (** Toggle levers. *)
  gates : Entities.gate array;
      (** Signal-controlled gates. *)
  elevators : Entities.elevator array;
      (** Moving elevator platforms. *)
  crates : Entities.crate array;
      (** Pushable crates. *)
  teleporters : Entities.teleporter_pair array;
      (** Bidirectional teleporter pairs. *)
  fans : Entities.fan array;
      (** Signal-controlled fan lift columns. *)
  signals : Signals.table;
      (** Per-frame signal snapshot, cleared at the start of each update. *)
  mutable debug : bool;
      (** When [true] the signal debug overlay is drawn this frame. *)
  mutable status : Types.status;
      (** Tracks whether the level is being played, won, or lost. *)
  mutable elapsed : float;
      (** Cumulative play time in seconds since the last reset. *)
  mutable death_timer : float;
      (** Seconds remaining in the death-screen pause before an auto-reset. *)
}

(** {1 Lifecycle} *)

(** [init level] allocates and returns a fresh game state for [level],
    constructing all entity arrays from the level's spec lists. *)
val init : Level.level_data -> t

(** [reset game] restores [game] to the same state as a freshly [init]'d game:
    players are moved to their spawn positions, all entities are reset, the
    signal table is cleared, and [status] is set to {!Types.Playing}.
    Entity arrays are reused (no allocation). *)
val reset : t -> unit

(** {1 Update} *)

(** [update game inp dt] advances [game] by one logical frame given input
    snapshot [inp] and elapsed time [dt] in seconds.  Returns [game] for
    convenience (the record is mutated in place).

    The update pipeline (when [status = Playing]):
    {ol
      {li Clear signals.}
      {li Advance elevators; record [frame_delta].}
      {li Emit button signals (player + crate overlap).}
      {li Handle lever interact (edge-triggered).}
      {li Open/close gates from signals.}
      {li Update fan on/off state from signals.}
      {li Build extra-solid list (closed gates + elevator platforms).}
      {li Apply elevator riding translations to players.}
      {li Compute ground surface for each player.}
      {li Reset and recompute [in_fan] membership for each player.}
      {li Apply player input and gravity.}
      {li Move crates (vertical gravity sweep).}
      {li Move players (horizontal + vertical swept collision against tiles,
          extra_solid, and crates).}
      {li Advance teleport cooldowns; run teleporter relocations.}
      {li Check hazards and exit doors.}
      {li Collect diamonds.}
      {li Evaluate win/lose conditions.}
    }

    When [status = Lost] the death timer is decremented and an auto-reset
    occurs when it reaches zero.  When [status = Won] or [status = Paused]
    the update is skipped.  A global reset (key R) is handled before the
    status check. *)
val update : t -> Input.t -> float -> t
