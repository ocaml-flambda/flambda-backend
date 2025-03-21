(* TEST
   readonly_files = "generate_array_reinterp_tests.ml";
 (* This runs [generate_array_reinterp_tests.ml] to generate various tests
    of the array reinterpret operations in the files
   [generated_array_reinterp_tests.ml].  It doesn't actually run those tests;
   it just updates the generated test programs (which are separately
   run by the test harness).
  *)
 setup-ocamlopt.opt-build-env;
 program = "${test_source_directory}/generate.out";
 all_modules = "generate_array_reinterp_tests.ml";
 ocamlopt.opt;
 arguments = "21";
 output = "${test_source_directory}/generated_array_reinterp_tests.ml.corrected";
 run;
 output = "${test_source_directory}/generated_array_reinterp_tests.ml.corrected";
 reference = "${test_source_directory}/generated_array_reinterp_tests.ml";
 check-program-output;
*)
