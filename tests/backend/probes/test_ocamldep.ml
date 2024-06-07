(* Regression test for a bug where ocamldep wouldn't register a dependency on
   the payload of a probe expression.
*)

let () =
  [%probe "probe1" For_test_ocamldep1.x];
  [%probe "probe2" ~enabled_at_init:true For_test_ocamldep2.x];
  ()
