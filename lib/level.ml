type diamond_spec = { kind : Types.player_kind; col : int; row : int }

(** Button: emits [id] each frame while any player AABB overlaps. *)
type button_spec = { id : string; col : int; row : int }

(** Lever: toggles [id] on interact; one 'p' char per cell. *)
type lever_spec = { id : string; col : int; row : int }

(** Gate: blocks movement unless [listener_ids] (interpreted as Any) is satisfied. *)
type gate_spec = {
  listener_ids : Types.signal_id list;
  col : int;
  row : int;
  w_tiles : int;
  h_tiles : int;
}

(** Elevator: moves between (col_a,row_a) and (col_b,row_b) at [speed] px/s. *)
type elev_spec = {
  col_a : int;
  row_a : int;
  col_b : int;
  row_b : int;
  w_tiles : int;
  speed : float;
}

(** Pushable crate (Phase 5); spawned at tile (col,row). *)
type crate_spec = { col : int; row : int }

(** Teleporter pair (Phase 5); linked tile pair at (col_a,row_a) ↔ (col_b,row_b). *)
type tele_spec = { col_a : int; row_a : int; col_b : int; row_b : int }

(** Fan (Phase 5); base at tile (col,row), column rises [height_tiles] above it,
    active while [listener_ids] (interpreted as Any; empty = always) is satisfied. *)
type fan_spec = {
  col : int;
  row : int;
  height_tiles : int;
  listener_ids : Types.signal_id list;
}

type level_data = {
  width : int;
  height : int;
  grid : Types.tile array array;
  fireboy_spawn : Vec2.t;
  watergirl_spawn : Vec2.t;
  diamonds : diamond_spec list;
  buttons : button_spec list;
  levers : lever_spec list;
  gates : gate_spec list;
  elevators : elev_spec list;
  crates : crate_spec list;
  teleporters : tele_spec list;
  fans : fan_spec list;
}

let char_to_tile = function
  | '.' | 'f' | 'w' | 'R' | 'B' | 'p' | 'V' | 'C' -> Types.Empty
  | '#' -> Types.Wall
  | 'F' -> Types.Fire
  | 'W' -> Types.Water
  | 'G' -> Types.Goo
  | 'r' -> Types.Fireboy_door
  | 'b' -> Types.Watergirl_door
  | '^' -> Types.Spikes
  | 'I' -> Types.Ice
  | '<' -> Types.Conveyor_left
  | '>' -> Types.Conveyor_right
  | '/' -> Types.Slope_up
  | '\\' -> Types.Slope_down
  | c -> failwith (Printf.sprintf "Unknown tile character: %c" c)

let parse
    ?(gates = [])
    ?(elevators = [])
    ?(teleporters = [])
    ?(fans = [])
    (lines : string list) : level_data =
  let lines = List.filter (fun s -> String.length s > 0) lines in
  let lines = List.rev lines in
  let height = List.length lines in
  let width =
    List.fold_left (fun acc s -> max acc (String.length s)) 0 lines
  in
  let grid = Array.make_matrix height width Types.Empty in
  let fireboy_spawn = ref Vec2.zero in
  let watergirl_spawn = ref Vec2.zero in
  let diamonds = ref [] in
  let buttons = ref [] in
  let levers = ref [] in
  let crates = ref [] in
  List.iteri
    (fun row line ->
      String.iteri
        (fun col c ->
          grid.(row).(col) <- char_to_tile c;
          let tile_size = float_of_int Tuning.tile_size in
          let spawn_x = (float_of_int col *. tile_size) +. (tile_size /. 2.) in
          let spawn_y = float_of_int row *. tile_size in
          match c with
          | 'f' -> fireboy_spawn := { Vec2.x = spawn_x; y = spawn_y }
          | 'w' -> watergirl_spawn := { Vec2.x = spawn_x; y = spawn_y }
          | 'R' -> diamonds := { kind = Types.Fireboy; col; row } :: !diamonds
          | 'B' -> diamonds := { kind = Types.Watergirl; col; row } :: !diamonds
          | 'p' ->
              let id = Printf.sprintf "P_%d_%d" col row in
              buttons := ({ id; col; row } : button_spec) :: !buttons
          | 'V' ->
              let id = Printf.sprintf "V_%d_%d" col row in
              levers := ({ id; col; row } : lever_spec) :: !levers
          | 'C' -> crates := ({ col; row } : crate_spec) :: !crates
          | _ -> ())
        line)
    lines;
  {
    width;
    height;
    grid;
    fireboy_spawn = !fireboy_spawn;
    watergirl_spawn = !watergirl_spawn;
    diamonds = List.rev !diamonds;
    buttons = List.rev !buttons;
    levers = List.rev !levers;
    gates;
    elevators;
    crates = List.rev !crates;
    teleporters;
    fans;
  }

(* ── Original test level (unchanged — existing tests depend on its layout) ── *)

let test_level_str =
  {|####################
#..................#
#..................#
#..r...........b...#
#..##.........##...#
#.......R..B.......#
#...FFFF....WWWW...#
#..................#
#....#........#....#
#....#..GGGG..#....#
#....#........#....#
#....##########....#
#..................#
#..f............w..#
####################|}

let test_level = parse (String.split_on_char '\n' test_level_str)

(* ── Phase-4 demo level — buttons, lever, gate, elevator ── *)

let entities_level_str =
  {|####################
#..................#
#.r..............b.#
#.##............##.#
#..................#
#..................#
#..................#
#..R............B..#
#..................#
#...FFFF..WWWW.....#
#..................#
#..................#
#.V................#
#pf..........w...p.#
####################|}

let entities_level =
  parse
    ~gates:
      [
        {
          listener_ids = [ "P_1_1"; "P_17_1"; "V_2_2" ];
          col = 9;
          row = 1;
          w_tiles = 1;
          h_tiles = 5;
        };
      ]
    ~elevators:
      [
        {
          col_a = 15;
          row_a = 3;
          col_b = 15;
          row_b = 8;
          w_tiles = 2;
          speed = 80.;
        };
      ]
    (String.split_on_char '\n' entities_level_str)

(* ── Default showcase level — every tile + every overlay entity ──
   Tile legend (see [char_to_tile]):
     # wall        F fire         W water        G goo
     r fireboy-door  b watergirl-door
     ^ spikes      I ice          < / > conveyors     / \ slopes
     f / w spawns  R / B diamonds
     V lever       p button       C pushable crate
   Overlay entities (specs below):
     gate (V_3_1 ∨ P_16_1), vertical elevator,
     teleporter pair, fan (on while lever is flipped). *)
let phase5_level_str =
  {|####################
#r..............b..#
####..........######
#..................#
#.F...R......B....W#
#.####....####..####
#..................#
#.........GG.......#
#......######......#
#./..............\.#
#.##............##.#
#.<<<....IIII...>>>#
#####...#####..#####
#f.V.C......^^..pw.#
####################|}

let phase5_level =
  parse
    ~gates:
      [
        {
          listener_ids = [ "V_3_1"; "P_16_1" ];
          col = 9;
          row = 11;
          w_tiles = 1;
          h_tiles = 2;
        };
      ]
    ~elevators:
      [
        {
          col_a = 6;
          row_a = 2;
          col_b = 6;
          row_b = 11;
          w_tiles = 2;
          speed = 50.;
        };
      ]
    ~teleporters:
      [ { col_a = 2; row_a = 8; col_b = 17; row_b = 8 } ]
    ~fans:
      [
        { col = 7; row = 1; height_tiles = 4; listener_ids = [ "V_3_1" ] };
      ]
    (String.split_on_char '\n' phase5_level_str)

let get_tile level col row =
  if col < 0 || col >= level.width || row < 0 || row >= level.height then
    Types.Wall
  else level.grid.(row).(col)

let tile_rect col row =
  let ts = float_of_int Tuning.tile_size in
  let x = float_of_int col *. ts in
  let y = float_of_int row *. ts in
  (x, y, ts, ts)
