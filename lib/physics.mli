(** Collision detection and swept movement for players and crates.

    The physics system works in world space with y increasing upward.
    Movement is resolved axis-by-axis using a sub-pixel sweep: each axis
    is divided into steps of at most 1 pixel so that fast-moving objects
    cannot tunnel through thin walls.

    Callers in [Game.update] must:
    {ol
      {li Call {!compute_ground_surface} for each player after elevator
          riding but before {!Player.apply_input}.}
      {li Call {!move_player} / {!move_crate} after applying velocity.}
      {li Call {!check_hazards} and {!check_door} last.}
    } *)

(** {1 Tile classification} *)

(** [is_solid tile] returns [true] for tiles that block movement
    ({!Types.Wall}, {!Types.Ice}, {!Types.Conveyor_left}, {!Types.Conveyor_right}). *)
val is_solid : Types.tile -> bool

(** [is_deadly_for kind tile] returns [true] when [tile] is lethal for a
    player of the given [kind]:
    {ul
      {li {!Types.Water} is deadly for {!Types.Fireboy}.}
      {li {!Types.Fire} is deadly for {!Types.Watergirl}.}
      {li {!Types.Goo} and {!Types.Spikes} are deadly for both.}
    } *)
val is_deadly_for : Types.player_kind -> Types.tile -> bool

(** {1 Ramp geometry} *)

(** Internal classification of a slope tile's orientation. *)
type ramp =
  | Up    (** Slope rising left-to-right (/ shape). *)
  | Down  (** Slope falling left-to-right (\ shape). *)

(** [ramp_surface_y ramp ~tile_x ~tile_y ~x] returns [Some y] — the world-space
    y coordinate of the ramp surface directly below world x-coordinate [x] —
    for the given ramp tile at [(tile_x, tile_y)].  Returns [None] when [x] is
    outside the tile's column. *)
val ramp_surface_y :
  ramp -> tile_x:int -> tile_y:int -> x:float -> float option

(** {1 Tile-grid helpers} *)

(** [overlapping_tiles (x, y, w, h)] returns the list of [(col, row)] pairs for
    all tile cells that intersect the given axis-aligned bounding rectangle. *)
val overlapping_tiles : float * float * float * float -> (int * int) list

(** [try_push_crate level extra_solid crates c dx] attempts to shove crate [c]
    horizontally by [dx] pixels.  Returns [true] and mutates [c.pos] on success;
    returns [false] and leaves [c] unchanged if the destination is blocked by a
    tile, an [extra_solid] rectangle, or another crate. *)
val try_push_crate :
  Level.level_data ->
  (float * float * float * float) list ->
  Entities.crate list ->
  Entities.crate ->
  float ->
  bool

(** {1 Ground surface detection} *)

(** [compute_ground_surface level p] probes the tile(s) 1 pixel below [p]'s
    feet and writes the appropriate {!Player.ground_surface} value into [p].
    Conveyor surfaces take priority over ice; ice takes priority over normal.
    Must be called each frame before {!Player.apply_input}. *)
val compute_ground_surface : Level.level_data -> Player.t -> unit

(** {1 Movement} *)

(** [move_player level extra_solid crates p] sweeps [p] through one frame of
    movement using [p.vel].

    Collision order:
    {ol
      {li Horizontal sweep against tiles and [extra_solid] (closed gates,
          elevator platforms). When a crate blocks the horizontal path the
          engine tries to push it via {!try_push_crate}; if that fails,
          horizontal velocity is zeroed.}
      {li Ramp snap: if [p] is descending and its foot-centre is above a
          slope tile, [p.pos.y] is adjusted to the ramp surface.}
      {li Vertical sweep against tiles, [extra_solid], and [crates].
          Downward contact sets [p.on_ground].}
      {li Second ramp snap after the vertical sweep for stable down-ramp walking.}
    }

    Has no effect when [p.alive] is [false]. *)
val move_player :
  Level.level_data ->
  (float * float * float * float) list ->
  Entities.crate list ->
  Player.t ->
  unit

(** [move_crate level extra_solid crates c] applies gravity and the existing
    horizontal friction to [c], then performs a vertical-only swept collision
    (the crate cannot self-initiate horizontal movement; players push it via
    the horizontal player sweep). *)
val move_crate :
  Level.level_data ->
  (float * float * float * float) list ->
  Entities.crate list ->
  Entities.crate ->
  unit

(** {1 Hazard and door checks} *)

(** [check_hazards level p] sets [p.alive] to [false] if any tile overlapping
    [p]'s bounding box is deadly for [p.kind].  No-ops when [p.alive] is already
    [false]. *)
val check_hazards : Level.level_data -> Player.t -> unit

(** [check_door level p] sets [p.at_door] to [true] iff [p]'s centre tile is
    the correct exit door for [p.kind].  Sets [p.at_door] to [false] when
    [p.alive] is [false]. *)
val check_door : Level.level_data -> Player.t -> unit
