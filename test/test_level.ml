open OUnit2
open Fwg

let tile_to_string = function
  | Types.Empty -> "Empty"
  | Types.Wall -> "Wall"
  | Types.Fire -> "Fire"
  | Types.Water -> "Water"
  | Types.Goo -> "Goo"
  | Types.Firecaml_door -> "Firecaml_door"
  | Types.Watercaml_door -> "Watercaml_door"
  | Types.Ice -> "Ice"
  | Types.Conveyor_left -> "Conveyor_left"
  | Types.Conveyor_right -> "Conveyor_right"
  | Types.Spikes -> "Spikes"
  | Types.Slope_up -> "Slope_up"
  | Types.Slope_down -> "Slope_down"

let test_parse_dimensions _ =
  let level = Level.test_level in
  assert_equal ~printer:string_of_int 20 level.width;
  assert_equal ~printer:string_of_int 15 level.height

let test_parse_walls _ =
  let level = Level.test_level in
  assert_equal ~printer:tile_to_string Types.Wall (Level.get_tile level 0 0);
  assert_equal ~printer:tile_to_string Types.Wall (Level.get_tile level 19 0);
  assert_equal ~printer:tile_to_string Types.Wall (Level.get_tile level 0 14);
  assert_equal ~printer:tile_to_string Types.Wall (Level.get_tile level 19 14)

let test_parse_empty _ =
  let level = Level.test_level in
  assert_equal ~printer:tile_to_string Types.Empty (Level.get_tile level 1 1);
  assert_equal ~printer:tile_to_string Types.Empty (Level.get_tile level 10 10)

let test_parse_fire _ =
  let level = Level.test_level in
  assert_equal ~printer:tile_to_string Types.Fire (Level.get_tile level 4 8);
  assert_equal ~printer:tile_to_string Types.Fire (Level.get_tile level 5 8);
  assert_equal ~printer:tile_to_string Types.Fire (Level.get_tile level 6 8);
  assert_equal ~printer:tile_to_string Types.Fire (Level.get_tile level 7 8)

let test_parse_water _ =
  let level = Level.test_level in
  assert_equal ~printer:tile_to_string Types.Water (Level.get_tile level 12 8);
  assert_equal ~printer:tile_to_string Types.Water (Level.get_tile level 13 8);
  assert_equal ~printer:tile_to_string Types.Water (Level.get_tile level 14 8);
  assert_equal ~printer:tile_to_string Types.Water (Level.get_tile level 15 8)

let test_parse_goo _ =
  let level = Level.test_level in
  assert_equal ~printer:tile_to_string Types.Goo (Level.get_tile level 8 5);
  assert_equal ~printer:tile_to_string Types.Goo (Level.get_tile level 9 5);
  assert_equal ~printer:tile_to_string Types.Goo (Level.get_tile level 10 5);
  assert_equal ~printer:tile_to_string Types.Goo (Level.get_tile level 11 5)

let test_parse_doors _ =
  let level = Level.test_level in
  assert_equal ~printer:tile_to_string Types.Firecaml_door (Level.get_tile level 3 11);
  assert_equal ~printer:tile_to_string Types.Watercaml_door (Level.get_tile level 15 11)

let test_spawn_positions _ =
  let level = Level.test_level in
  let fb_spawn = level.firecaml_spawn in
  let wg_spawn = level.watercaml_spawn in
  assert_bool "firecaml spawn x > 0" (fb_spawn.x > 0.);
  assert_bool "firecaml spawn y > 0" (fb_spawn.y > 0.);
  assert_bool "watercaml spawn x > 0" (wg_spawn.x > 0.);
  assert_bool "watercaml spawn y > 0" (wg_spawn.y > 0.);
  assert_bool "spawns are different" (fb_spawn.x <> wg_spawn.x)

let test_diamonds _ =
  let level = Level.test_level in
  assert_equal ~printer:string_of_int 2 (List.length level.diamonds);
  let red_diamonds =
    List.filter (fun d -> d.Level.kind = Types.Firecaml) level.diamonds
  in
  let blue_diamonds =
    List.filter (fun d -> d.Level.kind = Types.Watercaml) level.diamonds
  in
  assert_equal ~printer:string_of_int 1 (List.length red_diamonds);
  assert_equal ~printer:string_of_int 1 (List.length blue_diamonds)

let test_out_of_bounds _ =
  let level = Level.test_level in
  assert_equal ~printer:tile_to_string Types.Wall (Level.get_tile level (-1) 0);
  assert_equal ~printer:tile_to_string Types.Wall (Level.get_tile level 0 (-1));
  assert_equal ~printer:tile_to_string Types.Wall (Level.get_tile level 100 0);
  assert_equal ~printer:tile_to_string Types.Wall (Level.get_tile level 0 100)

let suite =
  "Level tests"
  >::: [
         "parse_dimensions" >:: test_parse_dimensions;
         "parse_walls" >:: test_parse_walls;
         "parse_empty" >:: test_parse_empty;
         "parse_fire" >:: test_parse_fire;
         "parse_water" >:: test_parse_water;
         "parse_goo" >:: test_parse_goo;
         "parse_doors" >:: test_parse_doors;
         "spawn_positions" >:: test_spawn_positions;
         "diamonds" >:: test_diamonds;
         "out_of_bounds" >:: test_out_of_bounds;
       ]

let () = run_test_tt_main suite
