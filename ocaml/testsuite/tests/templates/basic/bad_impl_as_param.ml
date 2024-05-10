(* TEST
 readonly_files = "bad_impl_as_param.reference";
 setup-ocamlc.byte-build-env;
 flags = "-as-parameter";
 modules = "bad_impl_as_param.ml";
 ocamlc_byte_exit_status = "2";
 compiler_output = "bad_impl_as_param.output";
 ocamlc.byte;
 compiler_reference = "bad_impl_as_param.reference";
 check-ocamlc.byte-output;
*)
