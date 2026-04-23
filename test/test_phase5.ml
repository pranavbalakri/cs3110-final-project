open OUnit2
open Fwg

let no_input : Input.player_input =
  { left = false; right = false; jump = false; jump_pressed = false }

let input_left : Input.player_input = { no_input with left = true }

let approx a b = abs_float (a -. b) < 0.0001

(* ── Surface-driven movement (Steps 25–26) ──────────────────────────────── *)

let test_normal_no_input_zeroes_vel _ =
  let p = Player.create Types.Fireboy Vec2.zero in
  p.vel <- { Vec2.x = 5.0; y = 0. };
  p.ground_surface <- Player.Normal;
  Player.apply_input p no_input;
  assert_bool "normal zeros vx" (approx p.vel.x 0.)

let test_ice_no_input_decays _ =
  let p = Player.create Types.Fireboy Vec2.zero in
  p.vel <- { Vec2.x = 5.0; y = 0. };
  p.ground_surface <- Player.Ice;
  Player.apply_input p no_input;
  assert_bool "ice decays vx" (approx p.vel.x (5.0 *. Tuning.ice_friction))

let test_ice_keeps_decaying _ =
  let p = Player.create Types.Fireboy Vec2.zero in
  p.vel <- { Vec2.x = 5.0; y = 0. };
  p.ground_surface <- Player.Ice;
  for _ = 1 to 5 do Player.apply_input p no_input done;
  let expected = 5.0 *. (Tuning.ice_friction ** 5.) in
  assert_bool "ice decays across frames" (approx p.vel.x expected)

let test_conveyor_no_input_uses_belt _ =
  let p = Player.create Types.Fireboy Vec2.zero in
  p.ground_surface <- Player.Conveyor_belt Tuning.conveyor_speed;
  Player.apply_input p no_input;
  assert_bool "conveyor carries at belt speed"
    (approx p.vel.x Tuning.conveyor_speed)

let test_conveyor_with_input_is_additive _ =
  let p = Player.create Types.Fireboy Vec2.zero in
  p.ground_surface <- Player.Conveyor_belt Tuning.conveyor_speed;
  Player.apply_input p input_left;
  let expected = -. Tuning.walk_speed +. Tuning.conveyor_speed in
  assert_bool "conveyor adds to input" (approx p.vel.x expected)

(* ── Fans (Step 27) ─────────────────────────────────────────────────────── *)

let test_fan_lifts_player _ =
  let p = Player.create Types.Fireboy Vec2.zero in
  p.in_fan <- true;
  p.vel <- Vec2.zero;
  Player.apply_gravity p;
  assert_bool "fan lifts upward" (p.vel.y > 0.);
  assert_bool "fan under max" (p.vel.y <= Tuning.fan_max_up_vy)

let test_fan_caps_upward_velocity _ =
  let p = Player.create Types.Fireboy Vec2.zero in
  p.in_fan <- true;
  p.vel <- { Vec2.x = 0.; y = Tuning.fan_max_up_vy };
  Player.apply_gravity p;
  assert_bool "fan cap" (approx p.vel.y Tuning.fan_max_up_vy)

let test_no_fan_applies_gravity _ =
  let p = Player.create Types.Fireboy Vec2.zero in
  p.in_fan <- false;
  p.vel <- Vec2.zero;
  Player.apply_gravity p;
  assert_bool "gravity pulls down" (p.vel.y < 0.)

(* ── Crates (Step 23) ───────────────────────────────────────────────────── *)

let simple_box_level =
  Level.parse
    (String.split_on_char '\n'
       "######\n#....#\n#....#\n######")

let test_crate_push_into_open_space _ =
  let c = Entities.crate_of_spec { Level.col = 2; row = 1 } in
  let start_x = c.pos.x in
  let moved = Physics.try_push_crate simple_box_level [] [ c ] c 5.0 in
  assert_bool "push succeeded" moved;
  assert_bool "crate moved right" (approx c.pos.x (start_x +. 5.0))

let test_crate_push_blocked_by_wall _ =
  (* Crate at col 4 sits 5 px from the right wall at col 5.
     Pushing more than that distance must be refused. *)
  let c = Entities.crate_of_spec { Level.col = 4; row = 1 } in
  let start_x = c.pos.x in
  let moved = Physics.try_push_crate simple_box_level [] [ c ] c 10.0 in
  assert_bool "push blocked" (not moved);
  assert_bool "crate did not move" (approx c.pos.x start_x)

let test_crate_push_blocked_by_other_crate _ =
  (* Adjacent crates: cols 2 and 3 leave 10 px between them.
     A push large enough to overlap the neighbour must be refused. *)
  let c1 = Entities.crate_of_spec { Level.col = 2; row = 1 } in
  let c2 = Entities.crate_of_spec { Level.col = 3; row = 1 } in
  let start_x = c1.pos.x in
  let moved =
    Physics.try_push_crate simple_box_level [] [ c1; c2 ] c1 15.0
  in
  assert_bool "recursive push refused" (not moved);
  assert_bool "c1 unchanged" (approx c1.pos.x start_x)

(* ── Teleporters (Step 24) ──────────────────────────────────────────────── *)

let test_teleporter_relocates_player _ =
  let tp =
    Entities.tele_of_spec { Level.col_a = 2; row_a = 1; col_b = 5; row_b = 1 }
  in
  let ts = float_of_int Tuning.tile_size in
  (* Place player's center inside tile A *)
  let p =
    Player.create Types.Fireboy
      { Vec2.x = (2. *. ts) +. (ts /. 2.); y = ts +. 2. }
  in
  Entities.update_teleporter tp [ p ];
  assert_bool "player at B"
    (p.pos.x > 5. *. ts && p.pos.x < 6. *. ts);
  assert_bool "cooldown set" (p.teleport_cooldown > 0.)

let test_teleporter_cooldown_prevents_retrigger _ =
  let tp =
    Entities.tele_of_spec { Level.col_a = 2; row_a = 1; col_b = 5; row_b = 1 }
  in
  let ts = float_of_int Tuning.tile_size in
  let p =
    Player.create Types.Fireboy
      { Vec2.x = (2. *. ts) +. (ts /. 2.); y = ts +. 2. }
  in
  Entities.update_teleporter tp [ p ];
  let pos_after_first = p.pos in
  (* Without ticking cooldown, player sitting at B should NOT teleport back. *)
  Entities.update_teleporter tp [ p ];
  assert_bool "no retrigger" (approx p.pos.x pos_after_first.x);
  assert_bool "still at B" (approx p.pos.y pos_after_first.y)

let test_teleporter_cooldown_ticks_down _ =
  let tp =
    Entities.tele_of_spec { Level.col_a = 2; row_a = 1; col_b = 5; row_b = 1 }
  in
  let ts = float_of_int Tuning.tile_size in
  let p =
    Player.create Types.Fireboy
      { Vec2.x = (2. *. ts) +. (ts /. 2.); y = ts +. 2. }
  in
  Entities.update_teleporter tp [ p ];
  let initial = p.teleport_cooldown in
  Entities.tick_teleport_cooldowns [ p ] 0.1;
  assert_bool "cooldown decreases" (p.teleport_cooldown < initial);
  Entities.tick_teleport_cooldowns [ p ] 10.0;
  assert_bool "cooldown clamped at 0" (approx p.teleport_cooldown 0.)

let suite =
  "Phase 5 tests"
  >::: [
         "normal_no_input_zeroes_vel" >:: test_normal_no_input_zeroes_vel;
         "ice_no_input_decays" >:: test_ice_no_input_decays;
         "ice_keeps_decaying" >:: test_ice_keeps_decaying;
         "conveyor_no_input_uses_belt" >:: test_conveyor_no_input_uses_belt;
         "conveyor_with_input_is_additive"
         >:: test_conveyor_with_input_is_additive;
         "fan_lifts_player" >:: test_fan_lifts_player;
         "fan_caps_upward_velocity" >:: test_fan_caps_upward_velocity;
         "no_fan_applies_gravity" >:: test_no_fan_applies_gravity;
         "crate_push_into_open_space" >:: test_crate_push_into_open_space;
         "crate_push_blocked_by_wall" >:: test_crate_push_blocked_by_wall;
         "crate_push_blocked_by_other_crate"
         >:: test_crate_push_blocked_by_other_crate;
         "teleporter_relocates_player" >:: test_teleporter_relocates_player;
         "teleporter_cooldown_prevents_retrigger"
         >:: test_teleporter_cooldown_prevents_retrigger;
         "teleporter_cooldown_ticks_down"
         >:: test_teleporter_cooldown_ticks_down;
       ]

let () = run_test_tt_main suite
