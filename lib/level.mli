(** Level data definitions, tile-map parser, and built-in levels.

    A level is described by a multi-line ASCII string (one character per tile)
    plus optional overlay entity specs passed as labeled arguments to {!parse}.
    The parser extracts spawn positions, collectibles, and interactive entity
    specs from special characters in the string, then combines them with the
    overlay lists into a {!level_data} record. *)

(** {1 Entity specification types}

    These records describe entities as they appear in the level definition.
    They are converted into runtime entity state by the [Entities] module. *)

(** Collectible diamond specification; extracted from 'R' (Fireboy) or 'B' (Watergirl)
    characters in the tile string. *)
type diamond_spec = {
  kind : Types.player_kind;  (** Which player can collect this diamond. *)
  col : int;                 (** Tile column of the diamond. *)
  row : int;                 (** Tile row of the diamond. *)
}

(** Pressure-plate button specification; extracted from 'p' characters.
    Emits a signal while any player or crate overlaps. *)
type button_spec = {
  id : string;  (** Auto-generated signal id: ["P_<col>_<row>"]. *)
  col : int;
  row : int;
}

(** Toggle lever specification; extracted from 'V' characters.
    Holds its signal high while its internal state is [true]. *)
type lever_spec = {
  id : string;  (** Auto-generated signal id: ["V_<col>_<row>"]. *)
  col : int;
  row : int;
}

(** Gate specification.  A gate is a solid rectangle that becomes passable
    when any of its [listener_ids] is emitted (logical OR).
    Passed via the [~gates] optional argument to {!parse}. *)
type gate_spec = {
  listener_ids : Types.signal_id list;
      (** Signal ids that, when any is true, open this gate. *)
  col : int;       (** Top-left tile column of the gate rectangle. *)
  row : int;       (** Top-left tile row of the gate rectangle. *)
  w_tiles : int;   (** Width of the gate in tiles. *)
  h_tiles : int;   (** Height of the gate in tiles. *)
}

(** Elevator specification.  An elevator shuttles between two tile positions
    at a fixed pixel-per-second speed.
    Passed via the [~elevators] optional argument to {!parse}. *)
type elev_spec = {
  col_a : int;    (** Column of the first endpoint. *)
  row_a : int;    (** Row of the first endpoint. *)
  col_b : int;    (** Column of the second endpoint. *)
  row_b : int;    (** Row of the second endpoint. *)
  w_tiles : int;  (** Width of the platform in tiles. *)
  speed : float;  (** Movement speed in pixels per second. *)
}

(** Pushable crate specification; extracted from 'C' characters.
    Crates are subject to gravity and can be shoved by players. *)
type crate_spec = {
  col : int;  (** Spawn tile column. *)
  row : int;  (** Spawn tile row. *)
}

(** Teleporter pair specification.  Entering either tile teleports the player
    to the centre of the other tile.
    Passed via the [~teleporters] optional argument to {!parse}. *)
type tele_spec = {
  col_a : int;  (** Column of the first portal tile. *)
  row_a : int;  (** Row of the first portal tile. *)
  col_b : int;  (** Column of the second portal tile. *)
  row_b : int;  (** Row of the second portal tile. *)
}

(** Fan specification.  A fan produces an upward-lift column above its base
    tile while any of its [listener_ids] is true (empty list = always on).
    Passed via the [~fans] optional argument to {!parse}. *)
type fan_spec = {
  col : int;                         (** Column of the fan base tile. *)
  row : int;                         (** Row of the fan base tile. *)
  height_tiles : int;                (** Number of tiles the lift column extends upward. *)
  listener_ids : Types.signal_id list;
      (** Signal ids that activate the fan; empty means always active. *)
}

(** {1 Level data record} *)

(** Complete description of a loaded level, passed to {!Game.init}. *)
type level_data = {
  width : int;                         (** Grid width in tiles. *)
  height : int;                        (** Grid height in tiles. *)
  grid : Types.tile array array;
      (** Tile grid indexed [grid.(row).(col)]; row 0 is the bottom row. *)
  fireboy_spawn : Vec2.t;              (** World-space spawn position for Fireboy. *)
  watergirl_spawn : Vec2.t;            (** World-space spawn position for Watergirl. *)
  diamonds : diamond_spec list;
  buttons : button_spec list;
  levers : lever_spec list;
  gates : gate_spec list;
  elevators : elev_spec list;
  crates : crate_spec list;
  teleporters : tele_spec list;
  fans : fan_spec list;
}

(** {1 Parser} *)

(** [parse ?gates ?elevators ?teleporters ?fans lines] converts the ASCII
    tile-map [lines] into a {!level_data} record.

    Recognised characters:
    {ul
      {li [#] — {!Types.Wall}}
      {li [F] — {!Types.Fire}}
      {li [W] — {!Types.Water}}
      {li [G] — {!Types.Goo}}
      {li [r] — {!Types.Fireboy_door}}
      {li [b] — {!Types.Watergirl_door}}
      {li [^] — {!Types.Spikes}}
      {li [I] — {!Types.Ice}}
      {li [<] — {!Types.Conveyor_left}}
      {li [>] — {!Types.Conveyor_right}}
      {li [/] — {!Types.Slope_up}}
      {li [\\] — {!Types.Slope_down}}
      {li [f] — Fireboy spawn (tile becomes {!Types.Empty})}
      {li [w] — Watergirl spawn (tile becomes {!Types.Empty})}
      {li [R] — Fireboy diamond (tile becomes {!Types.Empty})}
      {li [B] — Watergirl diamond (tile becomes {!Types.Empty})}
      {li [p] — button (tile becomes {!Types.Empty})}
      {li [V] — lever (tile becomes {!Types.Empty})}
      {li [C] — pushable crate (tile becomes {!Types.Empty})}
      {li [.] — {!Types.Empty}}
    }

    Empty lines are ignored.  The first non-empty line after filtering is
    treated as row 0 (the bottom of the world). *)
val parse :
  ?gates:gate_spec list ->
  ?elevators:elev_spec list ->
  ?teleporters:tele_spec list ->
  ?fans:fan_spec list ->
  string list ->
  level_data

(** {1 Built-in levels} *)

(** Minimal test level used by the automated test suite.
    Its layout is stable and must not be changed without updating tests. *)
val test_level : level_data

(** Phase-4 demonstration level showing buttons, a lever, a gate, and an elevator. *)
val entities_level : level_data

(** Showcase level exercising every tile type and every overlay entity. *)
val level1 : level_data

(** "Button Cooperation" — each player's button opens the other's exit gate. *)
val level2 : level_data

(** "Lever & Teleport" — lever activates a fan; a teleporter pair shortcuts upper ledges. *)
val level3 : level_data

(** All playable levels in display order, as [(name, level_data)] pairs.
    The level-select menu indexes into this array by cursor position. *)
val levels : (string * level_data) array

(** {1 Helpers} *)

(** [get_tile level col row] returns the tile at column [col], row [row].
    Returns {!Types.Wall} for out-of-bounds coordinates. *)
val get_tile : level_data -> int -> int -> Types.tile

(** [tile_rect col row] returns the axis-aligned bounding rectangle
    [(x, y, width, height)] of the tile at [(col, row)] in world space. *)
val tile_rect : int -> int -> float * float * float * float
