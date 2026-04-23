open OUnit2
open Fwg

let make_tbl signals =
  let tbl = Signals.create () in
  List.iter (Signals.emit tbl) signals;
  tbl

let test_lit_true _ =
  let tbl = make_tbl [ "a" ] in
  assert_bool "Lit a true" (Signals.eval tbl (Signals.Lit "a"))

let test_lit_false _ =
  let tbl = make_tbl [] in
  assert_bool "Lit a false" (not (Signals.eval tbl (Signals.Lit "a")))

let test_any_one_true _ =
  let tbl = make_tbl [ "b" ] in
  assert_bool "Any [a;b] when b set"
    (Signals.eval tbl (Signals.Any [ Signals.Lit "a"; Signals.Lit "b" ]))

let test_any_none_true _ =
  let tbl = make_tbl [] in
  assert_bool "Any [] false" (not (Signals.eval tbl (Signals.Any [])));
  assert_bool "Any [a;b] false when neither set"
    (not (Signals.eval tbl (Signals.Any [ Signals.Lit "a"; Signals.Lit "b" ])))

let test_all_both_true _ =
  let tbl = make_tbl [ "a"; "b" ] in
  assert_bool "All [a;b] when both set"
    (Signals.eval tbl (Signals.All [ Signals.Lit "a"; Signals.Lit "b" ]))

let test_all_one_missing _ =
  let tbl = make_tbl [ "a" ] in
  assert_bool "All [a;b] false when b missing"
    (not (Signals.eval tbl (Signals.All [ Signals.Lit "a"; Signals.Lit "b" ])))

let test_not _ =
  let tbl_empty = make_tbl [] in
  let tbl_a = make_tbl [ "a" ] in
  assert_bool "Not a when a absent" (Signals.eval tbl_empty (Signals.Not (Signals.Lit "a")));
  assert_bool "Not a false when a present"
    (not (Signals.eval tbl_a (Signals.Not (Signals.Lit "a"))))

let test_chained _ =
  (* Any [a; All [b; Not c]] — true when a is set, OR (b set AND c not set) *)
  let expr =
    Signals.Any
      [ Signals.Lit "a"; Signals.All [ Signals.Lit "b"; Signals.Not (Signals.Lit "c") ] ]
  in
  assert_bool "a set" (Signals.eval (make_tbl [ "a" ]) expr);
  assert_bool "b set c not set" (Signals.eval (make_tbl [ "b" ]) expr);
  assert_bool "b and c set → false"
    (not (Signals.eval (make_tbl [ "b"; "c" ]) expr));
  assert_bool "none set → false" (not (Signals.eval (make_tbl []) expr))

let test_clear _ =
  let tbl = make_tbl [ "a" ] in
  assert_bool "a before clear" (Signals.eval tbl (Signals.Lit "a"));
  Signals.clear tbl;
  assert_bool "a after clear" (not (Signals.eval tbl (Signals.Lit "a")))

let suite =
  "Signals tests"
  >::: [
         "lit_true" >:: test_lit_true;
         "lit_false" >:: test_lit_false;
         "any_one_true" >:: test_any_one_true;
         "any_none_true" >:: test_any_none_true;
         "all_both_true" >:: test_all_both_true;
         "all_one_missing" >:: test_all_one_missing;
         "not" >:: test_not;
         "chained" >:: test_chained;
         "clear" >:: test_clear;
       ]

let () = run_test_tt_main suite
