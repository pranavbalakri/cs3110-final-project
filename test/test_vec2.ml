open Fwg

let eps = 1e-9

let check_float msg expected actual =
  Alcotest.(check (float eps)) msg expected actual

let test_zero () =
  let v = Vec2.zero in
  check_float "zero x" 0.0 v.x;
  check_float "zero y" 0.0 v.y

let test_add () =
  let a = { Vec2.x = 1.5; y = -2.0 } in
  let b = { Vec2.x = 2.5; y = 4.0 } in
  let v = Vec2.add a b in
  check_float "add x" 4.0 v.x;
  check_float "add y" 2.0 v.y

let test_sub () =
  let a = { Vec2.x = 5.0; y = 1.25 } in
  let b = { Vec2.x = 3.0; y = 2.0 } in
  let v = Vec2.sub a b in
  check_float "sub x" 2.0 v.x;
  check_float "sub y" (-0.75) v.y

let test_scale () =
  let v = { Vec2.x = -3.0; y = 2.0 } in
  let out = Vec2.scale 2.5 v in
  check_float "scale x" (-7.5) out.x;
  check_float "scale y" 5.0 out.y

let test_length () =
  let v = { Vec2.x = 3.0; y = 4.0 } in
  check_float "length" 5.0 (Vec2.length v)

let () =
  Alcotest.run "vec2"
    [
      ( "ops",
        [
          Alcotest.test_case "zero" `Quick test_zero;
          Alcotest.test_case "add" `Quick test_add;
          Alcotest.test_case "sub" `Quick test_sub;
          Alcotest.test_case "scale" `Quick test_scale;
          Alcotest.test_case "length" `Quick test_length;
        ] );
    ]