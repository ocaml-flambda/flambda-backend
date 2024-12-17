(* TEST
 readonly_files = "generate_makearray_dynamic_tests.ml";
 (* Generate the bytecode/native code versions of
    [generate_makearray_dynamic_tests.ml]. This doesn't actually run the test;
    it just updates the generated test program (which is separately
    run by the test harness).
  *)

 {
   setup-ocamlopt.opt-build-env;
   program = "${test_source_directory}/generate.out";
   all_modules = "generate_makearray_dynamic_tests.ml";
   include stdlib_stable;
   include stdlib_upstream_compatible;
   ocamlopt.opt;
   arguments = "native";
   output = "${test_source_directory}/generated_native_test.ml.corrected";
   run;
   output = "${test_source_directory}/generated_native_test.ml.corrected";
   reference = "${test_source_directory}/generated_native_test.ml";
   check-program-output;
 }{
   (* Next, bytecode. *)
   setup-ocamlopt.opt-build-env;
   program = "${test_source_directory}/generate.out";
   all_modules = "generate_makearray_dynamic_tests.ml";
   include stdlib_stable;
   include stdlib_upstream_compatible;
   ocamlopt.opt;
   arguments = "byte";
   output = "${test_source_directory}/generated_byte_test.ml.corrected";
   run;
   output = "${test_source_directory}/generated_byte_test.ml.corrected";
   reference = "${test_source_directory}/generated_byte_test.ml";
   check-program-output;
 }
*)
