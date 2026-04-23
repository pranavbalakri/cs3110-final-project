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

let step_vertical (p : Player.t) (inp : Input.player_input) =
  Player.apply_input p inp;
  Player.apply_gravity p;
  p.pos <- { p.pos with Vec2.y = p.pos.y +. p.vel.y }

let test_short_hop_vs_high_jump _ =
  let run held_frames =
    let p = Player.create Types.Fireboy Vec2.zero in
    p.on_ground <- true;
    let peak = ref p.pos.y in
    (* Launch frame *)
    step_vertical p { left = false; right = false; jump = held_frames > 0; jump_pressed = true };
    if p.pos.y > !peak then peak := p.pos.y;
    p.on_ground <- false;
    for i = 1 to 24 do
      let hold = i < held_frames in
      step_vertical p { left = false; right = false; jump = hold; jump_pressed = false };
      if p.pos.y > !peak then peak := p.pos.y
    done;
    !peak
  in
  let short_peak = run 1 in
  let high_peak = run Tuning.jump_hold_frames in
  assert_bool "holding jump should produce a higher jump"
    (high_peak > short_peak +. 4.)

let test_coyote_jump_after_walking_off _ =
  let p = Player.create Types.Fireboy Vec2.zero in
  p.on_ground <- true;
  (* Prime coyote timer from a grounded frame. *)
  Player.apply_input p { left = false; right = false; jump = false; jump_pressed = false };
  p.on_ground <- false;
  Player.apply_input p { left = false; right = false; jump = true; jump_pressed = true };
  assert_bool "jump should fire during coyote window" (p.vel.y > 0.)

let test_buffered_jump_while_falling _ =
  let p = Player.create Types.Fireboy Vec2.zero in
  p.on_ground <- false;
  (* Press jump while airborne: should buffer. *)
  Player.apply_input p { left = false; right = false; jump = true; jump_pressed = true };
  assert_bool "buffer should be set" (p.jump_buffer_left > 0);
  p.vel <- Vec2.zero;
  (* Simulate landing before buffer expires; buffered jump should auto-fire. *)
  p.on_ground <- true;
  Player.apply_input p { left = false; right = false; jump = true; jump_pressed = false };
  assert_bool "buffered jump should fire on ground-touch" (p.vel.y > 0.)

let test_slope_surface_height _ =
  let up = Physics.ramp_surface_y Physics.Up ~tile_x:0 ~tile_y:0 ~x:10. in
  let down = Physics.ramp_surface_y Physics.Down ~tile_x:0 ~tile_y:0 ~x:10. in
  match (up, down) with
  | Some uy, Some dy ->
      assert_bool "up slope y near x=10 should be ~10" (abs_float (uy -. 10.) < 1e-6);
      assert_bool "down slope y near x=10 should be ~30" (abs_float (dy -. 30.) < 1e-6)
  | _ -> assert_failure "expected slope sample points inside tile"

let test_slope_snap_on_move _ =
  let lvl = Level.parse [ "/." ] in
  let p = Player.create Types.Fireboy { Vec2.x = 10.; y = 0. } in
  p.on_ground <- true;
  p.vel <- { Vec2.x = 0.; y = 0. };
  Physics.move_player lvl [] [] p;
  assert_bool "slope snap should raise player bottom to ramp surface"
    (abs_float (p.pos.y -. 10.) < 1e-6)

let suite =
  "Physics tests"
  >::: [
         "is_solid" >:: test_is_solid;
         "is_deadly_fireboy" >:: test_is_deadly_fireboy;
         "is_deadly_watergirl" >:: test_is_deadly_watergirl;
         "overlapping_tiles" >:: test_overlapping_tiles;
         "short_hop_vs_high_jump" >:: test_short_hop_vs_high_jump;
         "coyote_jump_after_walking_off" >:: test_coyote_jump_after_walking_off;
         "buffered_jump_while_falling" >:: test_buffered_jump_while_falling;
         "slope_surface_height" >:: test_slope_surface_height;
         "slope_snap_on_move" >:: test_slope_snap_on_move;
       ]

let () = run_test_tt_main suite
