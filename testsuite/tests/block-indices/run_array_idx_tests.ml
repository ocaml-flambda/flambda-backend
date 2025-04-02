(* TEST
 readonly_files = "generate_array_idx_tests.ml";
 (* Generate the bytecode/native code versions of
    [generate_array_idx_tests.ml]. This doesn't actually run the test;
    it just updates the generated test program (which is separately
    run by the test harness).
  *)
 {
   setup-ocamlopt.opt-build-env;
   stack-allocation;
   program = "${test_source_directory}/generate.out";
   all_modules = "generate_array_idx_tests.ml";
   include stdlib_stable;
   include stdlib_upstream_compatible;
   ocamlopt.opt;
   arguments = "native";
   output = "${test_source_directory}/generated_array_idx_native_test.ml.corrected";
   run;
   output = "${test_source_directory}/generated_array_idx_native_test.ml.corrected";
   reference = "${test_source_directory}/generated_array_idx_native_test.ml";
   check-program-output;
 }
 {
   setup-ocamlopt.opt-build-env;
   stack-allocation;
   program = "${test_source_directory}/generate.out";
   all_modules = "generate_array_idx_tests.ml";
   include stdlib_stable;
   include stdlib_upstream_compatible;
   ocamlopt.opt;
   arguments = "bytecode";
   output = "${test_source_directory}/generated_array_idx_bytecode_test.ml.corrected";
   run;
   output = "${test_source_directory}/generated_array_idx_bytecode_test.ml.corrected";
   reference = "${test_source_directory}/generated_array_idx_bytecode_test.ml";
   check-program-output;
 }
*)
