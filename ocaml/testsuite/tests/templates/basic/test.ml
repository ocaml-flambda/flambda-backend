(* TEST
 readonly_files = "bad_ref_direct.ml bad_ref_direct.reference monoid.mli ";
 setup-ocamlc.byte-build-env;
 flags = "-as-parameter";
 module = "monoid.mli";
 ocamlc.byte;
 module = "bad_ref_direct.ml";
 compiler_output = "bad_ref_direct.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 reason = "correct error message not yet implemented";
 skip;
 compiler_reference = "bad_ref_direct.reference";
 check-ocamlc.byte-output;
*)
