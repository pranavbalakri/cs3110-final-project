open OUnit2
open Fwg

let approx a b = abs_float (a -. b) < 0.01
let ts = float_of_int Tuning.tile_size

(* ── Helpers ───────────────────────────────────────────────────────── *)

let make_signals () = Signals.create ()

let player_at x y =
  Player.create Types.Fireboy { Vec2.x; y }

let dead_player_at x y =
  let p = player_at x y in
  p.alive <- false;
  p

(* ── Button ────────────────────────────────────────────────────────── *)

let test_button_pressed_by_player _ =
  let b = Entities.button_of_spec { Level.id = "btn"; col = 1; row = 1 } in
  let signals = make_signals () in
  (* player center at tile (1,1) world pos *)
  let p = player_at ts ts in
  Entities.update_button b [ p ] [] signals;
  assert_bool "button pressed" b.pressed;
  assert_bool "signal emitted" (Signals.eval signals (Signals.Lit "btn"))

let test_button_not_pressed_when_far _ =
  let b = Entities.button_of_spec { Level.id = "btn"; col = 0; row = 0 } in
  let signals = make_signals () in
  let p = player_at (ts *. 10.) (ts *. 10.) in
  Entities.update_button b [ p ] [] signals;
  assert_bool "button not pressed" (not b.pressed)

let test_button_not_pressed_by_dead_player _ =
  let b = Entities.button_of_spec { Level.id = "btn"; col = 0; row = 0 } in
  let signals = make_signals () in
  let p = dead_player_at 0. 0. in
  Entities.update_button b [ p ] [] signals;
  assert_bool "dead player does not press button" (not b.pressed)

let test_button_pressed_by_crate _ =
  let b = Entities.button_of_spec { Level.id = "btn"; col = 0; row = 0 } in
  let signals = make_signals () in
  let c = Entities.crate_of_spec { Level.col = 0; row = 0 } in
  let p = player_at (ts *. 10.) (ts *. 10.) in
  Entities.update_button b [ p ] [ c ] signals;
  assert_bool "crate presses button" b.pressed

(* ── Lever ─────────────────────────────────────────────────────────── *)

let test_lever_toggles_on_nearby_interact _ =
  let lv = Entities.lever_of_spec { Level.id = "lv"; col = 1; row = 1 } in
  let signals = make_signals () in
  let p = player_at ts ts in
  p.alive <- true;
  assert_bool "lever starts off" (not lv.state);
  Entities.update_lever lv [ (p, true) ] signals;
  assert_bool "lever on after interact" lv.state;
  assert_bool "lever signal emitted" (Signals.eval signals (Signals.Lit "lv"))

let test_lever_no_interact_stays_off _ =
  let lv = Entities.lever_of_spec { Level.id = "lv"; col = 1; row = 1 } in
  let signals = make_signals () in
  let p = player_at ts ts in
  Entities.update_lever lv [ (p, false) ] signals;
  assert_bool "no interact = stays off" (not lv.state)

let test_lever_toggle_twice_returns_off _ =
  let lv = Entities.lever_of_spec { Level.id = "lv"; col = 1; row = 1 } in
  let signals = make_signals () in
  let p = player_at ts ts in
  Entities.update_lever lv [ (p, true) ] signals;
  Entities.update_lever lv [ (p, true) ] signals;
  assert_bool "double toggle = off" (not lv.state)

let test_lever_far_player_no_toggle _ =
  let lv = Entities.lever_of_spec { Level.id = "lv"; col = 0; row = 0 } in
  let signals = make_signals () in
  let p = player_at (ts *. 10.) (ts *. 10.) in
  Entities.update_lever lv [ (p, true) ] signals;
  assert_bool "far player cannot toggle lever" (not lv.state)

(* ── Gate ──────────────────────────────────────────────────────────── *)

let test_gate_opens_on_signal _ =
  let g =
    Entities.gate_of_spec
      { Level.listener_ids = [ "s" ]; col = 0; row = 0; w_tiles = 1; h_tiles = 1 }
  in
  let signals = make_signals () in
  assert_bool "gate starts closed" (not g.is_open);
  Signals.emit signals "s";
  Entities.update_gate g signals;
  assert_bool "gate opens when signal active" g.is_open

let test_gate_closes_when_signal_gone _ =
  let g =
    Entities.gate_of_spec
      { Level.listener_ids = [ "s" ]; col = 0; row = 0; w_tiles = 1; h_tiles = 1 }
  in
  let signals = make_signals () in
  Signals.emit signals "s";
  Entities.update_gate g signals;
  Signals.clear signals;
  Entities.update_gate g signals;
  assert_bool "gate closes when signal gone" (not g.is_open)

let test_gate_multi_listener_any _ =
  let g =
    Entities.gate_of_spec
      {
        Level.listener_ids = [ "a"; "b" ];
        col = 0;
        row = 0;
        w_tiles = 1;
        h_tiles = 1;
      }
  in
  let signals = make_signals () in
  Signals.emit signals "a";
  Entities.update_gate g signals;
  assert_bool "gate opens when any listener fires" g.is_open

(* ── Elevator ──────────────────────────────────────────────────────── *)

let test_elevator_moves_toward_target _ =
  let e =
    Entities.elev_of_spec
      { Level.col_a = 0; row_a = 0; col_b = 5; row_b = 0; w_tiles = 2; speed = 100. }
  in
  let start_x = e.pos.x in
  Entities.update_elevator e (1.0 /. 60.);
  assert_bool "elevator moved right" (e.pos.x > start_x)

let test_elevator_records_frame_delta _ =
  let e =
    Entities.elev_of_spec
      { Level.col_a = 0; row_a = 0; col_b = 5; row_b = 0; w_tiles = 2; speed = 100. }
  in
  Entities.update_elevator e (1.0 /. 60.);
  assert_bool "frame_delta set" (e.frame_delta.x > 0.)

let test_elevator_reverses_at_target _ =
  let e =
    Entities.elev_of_spec
      { Level.col_a = 0; row_a = 0; col_b = 0; row_b = 3; w_tiles = 2; speed = 500. }
  in
  for _ = 1 to 60 do
    Entities.update_elevator e (1.0 /. 60.)
  done;
  assert_bool "elevator reversed direction" (not e.going_to_b)

(* ── Elevator riding ───────────────────────────────────────────────── *)

let test_player_rides_elevator _ =
  let e =
    Entities.elev_of_spec
      { Level.col_a = 0; row_a = 2; col_b = 5; row_b = 2; w_tiles = 2; speed = 100. }
  in
  let elev_top = e.pos.y +. e.h in
  let p = player_at ts elev_top in
  p.on_ground <- true;
  Entities.update_elevator e (1.0 /. 60.);
  let before = p.pos.x in
  Entities.apply_elevator_riding [| e |] p;
  assert_bool "player moved with elevator" (p.pos.x > before)

let test_dead_player_not_carried _ =
  let e =
    Entities.elev_of_spec
      { Level.col_a = 0; row_a = 2; col_b = 5; row_b = 2; w_tiles = 2; speed = 100. }
  in
  let elev_top = e.pos.y +. e.h in
  let p = player_at ts elev_top in
  p.on_ground <- true;
  p.alive <- false;
  Entities.update_elevator e (1.0 /. 60.);
  let before = p.pos.x in
  Entities.apply_elevator_riding [| e |] p;
  assert_bool "dead player not carried" (approx p.pos.x before)

(* ── Fan ───────────────────────────────────────────────────────────── *)

let test_fan_turns_on_from_signal _ =
  let f =
    Entities.fan_of_spec
      { Level.col = 0; row = 0; height_tiles = 3; listener_ids = [ "f" ] }
  in
  let signals = make_signals () in
  assert_bool "fan starts off" (not f.is_on);
  Signals.emit signals "f";
  Entities.update_fan f signals;
  assert_bool "fan turns on" f.is_on

let test_fan_always_on_with_no_listeners _ =
  let f =
    Entities.fan_of_spec
      { Level.col = 0; row = 0; height_tiles = 3; listener_ids = [] }
  in
  let signals = make_signals () in
  Entities.update_fan f signals;
  assert_bool "no-listener fan always on" f.is_on

let test_fan_multi_listener_any _ =
  let f =
    Entities.fan_of_spec
      { Level.col = 0; row = 0; height_tiles = 3; listener_ids = [ "a"; "b" ] }
  in
  let signals = make_signals () in
  Signals.emit signals "b";
  Entities.update_fan f signals;
  assert_bool "fan on when any listener fires" f.is_on

let test_fan_marks_player_in_column _ =
  let f =
    Entities.fan_of_spec
      { Level.col = 0; row = 0; height_tiles = 3; listener_ids = [] }
  in
  f.is_on <- true;
  (* player center inside fan column: col 0, above base row *)
  let p = player_at (ts /. 2.) (ts *. 1.5) in
  p.in_fan <- false;
  Entities.apply_fan_to_player f p;
  assert_bool "player marked in_fan" p.in_fan

let test_fan_off_does_not_mark_player _ =
  let f =
    Entities.fan_of_spec
      { Level.col = 0; row = 0; height_tiles = 3; listener_ids = [] }
  in
  f.is_on <- false;
  let p = player_at (ts /. 2.) (ts *. 1.5) in
  p.in_fan <- false;
  Entities.apply_fan_to_player f p;
  assert_bool "off fan does not mark player" (not p.in_fan)

let test_fan_out_of_column_not_marked _ =
  let f =
    Entities.fan_of_spec
      { Level.col = 0; row = 0; height_tiles = 3; listener_ids = [] }
  in
  f.is_on <- true;
  let p = player_at (ts *. 10.) (ts *. 1.5) in
  p.in_fan <- false;
  Entities.apply_fan_to_player f p;
  assert_bool "player outside column not marked" (not p.in_fan)

(* ── Resets ────────────────────────────────────────────────────────── *)

let test_reset_lever _ =
  let lv = Entities.lever_of_spec { Level.id = "lv"; col = 0; row = 0 } in
  lv.state <- true;
  Entities.reset_lever lv;
  assert_bool "lever reset to off" (not lv.state)

let test_reset_elevator _ =
  let e =
    Entities.elev_of_spec
      { Level.col_a = 0; row_a = 0; col_b = 5; row_b = 0; w_tiles = 2; speed = 100. }
  in
  for _ = 1 to 30 do Entities.update_elevator e (1.0 /. 60.) done;
  Entities.reset_elevator e;
  assert_bool "elevator at point_a x" (approx e.pos.x e.point_a.x);
  assert_bool "elevator at point_a y" (approx e.pos.y e.point_a.y);
  assert_bool "going_to_b reset" e.going_to_b;
  assert_bool "frame_delta zeroed" (approx e.frame_delta.x 0.)

let test_reset_crate _ =
  let c = Entities.crate_of_spec { Level.col = 2; row = 2 } in
  let orig_x = c.pos.x in
  c.pos <- { c.pos with Vec2.x = orig_x +. 100. };
  c.vel <- { Vec2.x = 5.; y = 3. };
  Entities.reset_crate c;
  assert_bool "crate x reset" (approx c.pos.x orig_x);
  assert_bool "crate vel reset" (approx c.vel.x 0. && approx c.vel.y 0.)

let test_reset_fan _ =
  let f =
    Entities.fan_of_spec
      { Level.col = 0; row = 0; height_tiles = 3; listener_ids = [] }
  in
  f.is_on <- true;
  Entities.reset_fan f;
  assert_bool "fan reset to off" (not f.is_on)

(* ── BBox helpers (direct) ─────────────────────────────────────────── *)

let test_bbox_of_gate _ =
  let g =
    Entities.gate_of_spec
      { Level.listener_ids = [ "s" ]; col = 1; row = 1; w_tiles = 2; h_tiles = 3 }
  in
  let (x, y, w, h) = Entities.bbox_of_gate g in
  assert_bool "gate bbox x" (approx x ts);
  assert_bool "gate bbox y" (approx y ts);
  assert_bool "gate bbox w" (approx w (2. *. ts));
  assert_bool "gate bbox h" (approx h (3. *. ts))

let test_bbox_of_elevator _ =
  let e =
    Entities.elev_of_spec
      { Level.col_a = 1; row_a = 1; col_b = 4; row_b = 1; w_tiles = 2; speed = 80. }
  in
  let (x, y, w, _h) = Entities.bbox_of_elevator e in
  assert_bool "elev bbox x" (approx x ts);
  assert_bool "elev bbox y" (approx y ts);
  assert_bool "elev bbox w" (approx w (2. *. ts))

let suite =
  "Entities tests"
  >::: [
         "button_pressed_by_player" >:: test_button_pressed_by_player;
         "button_not_pressed_when_far" >:: test_button_not_pressed_when_far;
         "button_not_pressed_by_dead_player"
         >:: test_button_not_pressed_by_dead_player;
         "button_pressed_by_crate" >:: test_button_pressed_by_crate;
         "lever_toggles_on_nearby_interact"
         >:: test_lever_toggles_on_nearby_interact;
         "lever_no_interact_stays_off" >:: test_lever_no_interact_stays_off;
         "lever_toggle_twice_returns_off" >:: test_lever_toggle_twice_returns_off;
         "lever_far_player_no_toggle" >:: test_lever_far_player_no_toggle;
         "gate_opens_on_signal" >:: test_gate_opens_on_signal;
         "gate_closes_when_signal_gone" >:: test_gate_closes_when_signal_gone;
         "gate_multi_listener_any" >:: test_gate_multi_listener_any;
         "elevator_moves_toward_target" >:: test_elevator_moves_toward_target;
         "elevator_records_frame_delta" >:: test_elevator_records_frame_delta;
         "elevator_reverses_at_target" >:: test_elevator_reverses_at_target;
         "player_rides_elevator" >:: test_player_rides_elevator;
         "dead_player_not_carried" >:: test_dead_player_not_carried;
         "fan_turns_on_from_signal" >:: test_fan_turns_on_from_signal;
         "fan_always_on_with_no_listeners" >:: test_fan_always_on_with_no_listeners;
         "fan_multi_listener_any" >:: test_fan_multi_listener_any;
         "fan_marks_player_in_column" >:: test_fan_marks_player_in_column;
         "fan_off_does_not_mark_player" >:: test_fan_off_does_not_mark_player;
         "fan_out_of_column_not_marked" >:: test_fan_out_of_column_not_marked;
         "reset_lever" >:: test_reset_lever;
         "reset_elevator" >:: test_reset_elevator;
         "reset_crate" >:: test_reset_crate;
         "reset_fan" >:: test_reset_fan;
         "bbox_of_gate" >:: test_bbox_of_gate;
         "bbox_of_elevator" >:: test_bbox_of_elevator;
       ]

let () = run_test_tt_main suite
