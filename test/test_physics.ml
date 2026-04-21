open OUnit2
open Fwg

let test_is_solid _ =
  assert_bool "Wall is solid" (Physics.is_solid Types.Wall);
  assert_bool "Ice is solid" (Physics.is_solid Types.Ice);
  assert_bool "Conveyor_left is solid" (Physics.is_solid Types.Conveyor_left);
  assert_bool "Conveyor_right is solid" (Physics.is_solid Types.Conveyor_right);
  assert_bool "Empty is not solid" (not (Physics.is_solid Types.Empty));
  assert_bool "Fire is not solid" (not (Physics.is_solid Types.Fire));
  assert_bool "Water is not solid" (not (Physics.is_solid Types.Water));
  assert_bool "Goo is not solid" (not (Physics.is_solid Types.Goo))

let test_is_deadly_fireboy _ =
  assert_bool "Water deadly for Fireboy"
    (Physics.is_deadly_for Types.Fireboy Types.Water);
  assert_bool "Fire not deadly for Fireboy"
    (not (Physics.is_deadly_for Types.Fireboy Types.Fire));
  assert_bool "Goo deadly for Fireboy"
    (Physics.is_deadly_for Types.Fireboy Types.Goo);
  assert_bool "Spikes deadly for Fireboy"
    (Physics.is_deadly_for Types.Fireboy Types.Spikes);
  assert_bool "Wall not deadly for Fireboy"
    (not (Physics.is_deadly_for Types.Fireboy Types.Wall))

let test_is_deadly_watergirl _ =
  assert_bool "Fire deadly for Watergirl"
    (Physics.is_deadly_for Types.Watergirl Types.Fire);
  assert_bool "Water not deadly for Watergirl"
    (not (Physics.is_deadly_for Types.Watergirl Types.Water));
  assert_bool "Goo deadly for Watergirl"
    (Physics.is_deadly_for Types.Watergirl Types.Goo);
  assert_bool "Spikes deadly for Watergirl"
    (Physics.is_deadly_for Types.Watergirl Types.Spikes);
  assert_bool "Wall not deadly for Watergirl"
    (not (Physics.is_deadly_for Types.Watergirl Types.Wall))

let test_overlapping_tiles _ =
  let tiles = Physics.overlapping_tiles (0., 0., 40., 40.) in
  assert_equal 1 (List.length tiles);
  assert_bool "contains (0,0)" (List.mem (0, 0) tiles);
  let tiles2 = Physics.overlapping_tiles (20., 20., 40., 40.) in
  assert_equal 4 (List.length tiles2);
  assert_bool "contains (0,0)" (List.mem (0, 0) tiles2);
  assert_bool "contains (1,0)" (List.mem (1, 0) tiles2);
  assert_bool "contains (0,1)" (List.mem (0, 1) tiles2);
  assert_bool "contains (1,1)" (List.mem (1, 1) tiles2)

let suite =
  "Physics tests"
  >::: [
         "is_solid" >:: test_is_solid;
         "is_deadly_fireboy" >:: test_is_deadly_fireboy;
         "is_deadly_watergirl" >:: test_is_deadly_watergirl;
         "overlapping_tiles" >:: test_overlapping_tiles;
       ]

let () = run_test_tt_main suite
