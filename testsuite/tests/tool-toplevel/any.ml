(* TEST
 exit_status = "2";
 reference = "${test_source_directory}/any.native.reference";
 toplevel.opt;
*)

(* This checks that things with layout "any" don't cause problems in [Tstr_eval] *)
assert false;;
