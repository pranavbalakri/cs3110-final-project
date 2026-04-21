open Fwg
open OUnit2

let eps = 1e-9
let aeq msg expected actual =
  assert_equal ~msg ~cmp:(fun a b -> abs_float (a -. b) < eps) expected actual

let test_zero _ =
  let v = Vec2.zero in
  aeq "zero x" 0. v.x;
  aeq "zero y" 0. v.y

let test_add _ =
  let a = { Vec2.x = 1.5; y = -2.0 } in
  let b = { Vec2.x = 2.5; y =  4.0 } in
  let v = Vec2.add a b in
  aeq "add x" 4.0 v.x;
  aeq "add y" 2.0 v.y

let test_sub _ =
  let a = { Vec2.x = 5.0; y = 1.25 } in
  let b = { Vec2.x = 3.0; y = 2.0  } in
  let v = Vec2.sub a b in
  aeq "sub x"    2.0  v.x;
  aeq "sub y" (-0.75) v.y

let test_scale _ =
  let v   = { Vec2.x = -3.0; y = 2.0 } in
  let out = Vec2.scale 2.5 v in
  aeq "scale x" (-7.5) out.x;
  aeq "scale y"   5.0  out.y

let test_length _ =
  let v = { Vec2.x = 3.0; y = 4.0 } in
  aeq "length" 5.0 (Vec2.length v)

let suite =
  "vec2" >::: [
    "zero"   >:: test_zero;
    "add"    >:: test_add;
    "sub"    >:: test_sub;
    "scale"  >:: test_scale;
    "length" >:: test_length;
  ]

let () = run_test_tt_main suite
