(* TEST
 readonly_files = "generate_makearray_dynamic_tests.ml";
 (* Generate the bytecode/native code versions of
    [generate_makearray_dynamic_tests.ml]. This doesn't actually run the test;
    it just updates the generated test program (which is separately
    run by the test harness).
  *)

 {
   setup-ocamlopt.opt-build-env;
   stack-allocation;
   program = "${test_source_directory}/generate.out";
   all_modules = "generate_makearray_dynamic_tests.ml";
   include stdlib_stable;
   include stdlib_upstream_compatible;
   ocamlopt.opt;
   arguments = "byte";
   output = "${test_source_directory}/generated_test_byte.ml.corrected";
   run;
   output = "${test_source_directory}/generated_test_byte.ml.corrected";
   reference = "${test_source_directory}/generated_test_byte.ml";
   check-program-output;
 }
 {
   setup-ocamlopt.opt-build-env;
   stack-allocation;
   program = "${test_source_directory}/generate.out";
   all_modules = "generate_makearray_dynamic_tests.ml";
   include stdlib_stable;
   include stdlib_upstream_compatible;
   ocamlopt.opt;
   arguments = "native-oclassic";
   output = "${test_source_directory}/generated_test_native_oclassic.ml.corrected";
   run;
   output = "${test_source_directory}/generated_test_native_oclassic.ml.corrected";
   reference = "${test_source_directory}/generated_test_native_oclassic.ml";
   check-program-output;
 }
 {
   setup-ocamlopt.opt-build-env;
   stack-allocation;
   program = "${test_source_directory}/generate.out";
   all_modules = "generate_makearray_dynamic_tests.ml";
   include stdlib_stable;
   include stdlib_upstream_compatible;
   ocamlopt.opt;
   arguments = "native-o3";
   output = "${test_source_directory}/generated_test_native_o3.ml.corrected";
   run;
   output = "${test_source_directory}/generated_test_native_o3.ml.corrected";
   reference = "${test_source_directory}/generated_test_native_o3.ml";
   check-program-output;
 }
*)
