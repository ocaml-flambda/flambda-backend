(* TEST
 readonly_files = "bad_param_packed.reference";
 setup-ocamlc.byte-build-env;
 flags = "-as-parameter -for-pack Pack";
 module = "bad_param_packed.mli";
 compiler_output = "bad_param_packed.output";
 ocamlc_byte_exit_status = "2";
 ocamlc.byte;
 compiler_reference = "bad_param_packed.reference";
 check-ocamlc.byte-output;
*)
