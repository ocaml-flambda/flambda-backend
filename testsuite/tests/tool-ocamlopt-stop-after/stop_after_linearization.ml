(* TEST
 native-compiler;
 setup-ocamlopt.byte-build-env;
 flags = "-stop-after linearization -S";
 ocamlopt_byte_exit_status = "0";
 ocamlopt.byte;
 check-ocamlopt.byte-output;
 script = "sh ${test_source_directory}/stop_after_linearization.sh";
 script;
*)

(* this file is just a test driver, the test does not contain real OCaml code *)
