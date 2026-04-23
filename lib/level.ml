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
}

let char_to_tile = function
  | '.' | 'f' | 'w' | 'R' | 'B' | 'p' | 'V' -> Types.Empty
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

let parse ?(gates = []) ?(elevators = []) (lines : string list) : level_data =
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

(* ── Phase-4 demo level — shows buttons, lever, gate, elevator ── *)

(* 20 cols × 15 rows.  Top of string = row 14; last line = row 0.
   Entity key:
     p  = pressure-plate button  (signal "P_col_row")
     V  = lever                  (signal "V_col_row")
   Gate and elevator defined below as static specs.

   Layout (visual, top = row 14):
     row 14  ####################
     row 13  #..................#
     row 12  #.r..............b.#   ← doors (r=col 2, b=col 17)
     row 11  #.##............##.#
     row 10  #..................#
     row  9  #..................#
     row  8  #..R............B..#   ← diamonds
     row  7  #..................#
     row  6  #..................#
     row  5  #...FFFF..WWWW.....#   ← hazards
     row  4  #..................#
     row  3  #..................#
     row  2  #.V................#   ← lever at col 2
     row  1  #pf..........w...p.#   ← buttons+spawns
     row  0  ####################

   Gate  : col 9, rows 2-5 (1 wide × 4 tall); opened by P_1_1 OR P_17_1 OR V_2_2.
   Elev  : cols 15-16 (2 wide), travels row 3 → row 8, speed 80 px/s.
*)
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
          row = 1;    (* starts at floor level so it blocks ground traversal *)
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

let get_tile level col row =
  if col < 0 || col >= level.width || row < 0 || row >= level.height then
    Types.Wall
  else level.grid.(row).(col)

let tile_rect col row =
  let ts = float_of_int Tuning.tile_size in
  let x = float_of_int col *. ts in
  let y = float_of_int row *. ts in
  (x, y, ts, ts)
