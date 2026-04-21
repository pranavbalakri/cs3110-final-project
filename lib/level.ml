type diamond_spec = { kind : Types.player_kind; col : int; row : int }

type level_data = {
  width : int;
  height : int;
  grid : Types.tile array array;
  fireboy_spawn : Vec2.t;
  watergirl_spawn : Vec2.t;
  diamonds : diamond_spec list;
}

let char_to_tile = function
  | '.' | 'f' | 'w' | 'R' | 'B' -> Types.Empty
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

let parse (lines : string list) : level_data =
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
  }

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

let get_tile level col row =
  if col < 0 || col >= level.width || row < 0 || row >= level.height then
    Types.Wall
  else level.grid.(row).(col)

let tile_rect col row =
  let ts = float_of_int Tuning.tile_size in
  let x = float_of_int col *. ts in
  let y = float_of_int row *. ts in
  (x, y, ts, ts)
