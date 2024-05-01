(* TEST
 readonly_files = "bad_param_impl.mli bad_param_impl.reference";
 setup-ocamlc.byte-build-env;
 flags = "-as-parameter";
 module = "bad_param_impl.mli";
 ocamlc.byte;
 flags = "";
 module = "bad_param_impl.ml";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_param_impl.output";
 ocamlc.byte;
 reason = "error broken, will be fixed by #1764";
 skip;
 compiler_reference = "bad_param_impl.reference";
 check-ocamlc.byte-output;
*)
