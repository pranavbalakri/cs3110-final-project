open OUnit2
open Fwg

let test_parse_dimensions _ =
  let level = Level.test_level in
  assert_equal 20 level.width;
  assert_equal 15 level.height

let test_parse_walls _ =
  let level = Level.test_level in
  assert_equal Types.Wall (Level.get_tile level 0 0);
  assert_equal Types.Wall (Level.get_tile level 19 0);
  assert_equal Types.Wall (Level.get_tile level 0 14);
  assert_equal Types.Wall (Level.get_tile level 19 14)

let test_parse_empty _ =
  let level = Level.test_level in
  assert_equal Types.Empty (Level.get_tile level 1 1);
  assert_equal Types.Empty (Level.get_tile level 10 10)

let test_parse_fire _ =
  let level = Level.test_level in
  assert_equal Types.Fire (Level.get_tile level 4 8);
  assert_equal Types.Fire (Level.get_tile level 5 8);
  assert_equal Types.Fire (Level.get_tile level 6 8);
  assert_equal Types.Fire (Level.get_tile level 7 8)

let test_parse_water _ =
  let level = Level.test_level in
  assert_equal Types.Water (Level.get_tile level 12 8);
  assert_equal Types.Water (Level.get_tile level 13 8);
  assert_equal Types.Water (Level.get_tile level 14 8);
  assert_equal Types.Water (Level.get_tile level 15 8)

let test_parse_goo _ =
  let level = Level.test_level in
  assert_equal Types.Goo (Level.get_tile level 8 5);
  assert_equal Types.Goo (Level.get_tile level 9 5);
  assert_equal Types.Goo (Level.get_tile level 10 5);
  assert_equal Types.Goo (Level.get_tile level 11 5)

let test_parse_doors _ =
  let level = Level.test_level in
  assert_equal Types.Fireboy_door (Level.get_tile level 3 11);
  assert_equal Types.Watergirl_door (Level.get_tile level 15 11)

let test_spawn_positions _ =
  let level = Level.test_level in
  let fb_spawn = level.fireboy_spawn in
  let wg_spawn = level.watergirl_spawn in
  assert_bool "fireboy spawn x > 0" (fb_spawn.x > 0.);
  assert_bool "fireboy spawn y > 0" (fb_spawn.y > 0.);
  assert_bool "watergirl spawn x > 0" (wg_spawn.x > 0.);
  assert_bool "watergirl spawn y > 0" (wg_spawn.y > 0.);
  assert_bool "spawns are different" (fb_spawn.x <> wg_spawn.x)

let test_diamonds _ =
  let level = Level.test_level in
  assert_equal 2 (List.length level.diamonds);
  let red_diamonds =
    List.filter (fun d -> d.Level.kind = Types.Fireboy) level.diamonds
  in
  let blue_diamonds =
    List.filter (fun d -> d.Level.kind = Types.Watergirl) level.diamonds
  in
  assert_equal 1 (List.length red_diamonds);
  assert_equal 1 (List.length blue_diamonds)

let test_out_of_bounds _ =
  let level = Level.test_level in
  assert_equal Types.Wall (Level.get_tile level (-1) 0);
  assert_equal Types.Wall (Level.get_tile level 0 (-1));
  assert_equal Types.Wall (Level.get_tile level 100 0);
  assert_equal Types.Wall (Level.get_tile level 0 100)

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
