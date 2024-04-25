(* TEST
 readonly_files = "common.mli common.ml test_common.c test_common.h";
 flambda2;
 setup-ocamlopt.opt-build-env;
 test_file = "${test_source_directory}/gen_test.ml";
 ocaml_script_as_argument = "true";
 arguments = "c";
 compiler_output = "stubs.c";
 ocaml;
 arguments = "ml";
 compiler_output = "main.ml";
 ocaml;
 ocamlopt_flags = "-extension-universe stable -cc '${cc} -msse4.2'";
 all_modules = "test_common.c stubs.c common.mli common.ml test0.ml test1.ml main.ml";
 ocamlopt.opt;
 run;
 check-program-output;
*)
(* We use flambda2 above as a proxy to indicate SIMD is supported *)
