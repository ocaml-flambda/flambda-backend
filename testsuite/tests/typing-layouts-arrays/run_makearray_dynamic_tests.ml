(* TEST
 readonly_files = "generate_makearray_dynamic_tests.ml";
 (* Generate the bytecode/native code versions of
    [generate_makearray_dynamic_tests.ml]. This doesn't actually run the test;
    it just updates the generated test program (which is separately
    run by the test harness).

    The test is arbitrarily split across different files to improve overall test
    time.
  *)

 {
   setup-ocamlopt.opt-build-env;
   stack-allocation;
   program = "${test_source_directory}/generate.out";
   all_modules = "generate_makearray_dynamic_tests.ml";
   include stdlib_stable;
   include stdlib_upstream_compatible;
   ocamlopt.opt;
   arguments = "native 5 0";
   output = "${test_source_directory}/generated_test_0.ml.corrected";
   run;
   output = "${test_source_directory}/generated_test_0.ml.corrected";
   reference = "${test_source_directory}/generated_test_0.ml";
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
   arguments = "native 5 1";
   output = "${test_source_directory}/generated_test_1.ml.corrected";
   run;
   output = "${test_source_directory}/generated_test_1.ml.corrected";
   reference = "${test_source_directory}/generated_test_1.ml";
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
   arguments = "native 5 2";
   output = "${test_source_directory}/generated_test_2.ml.corrected";
   run;
   output = "${test_source_directory}/generated_test_2.ml.corrected";
   reference = "${test_source_directory}/generated_test_2.ml";
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
   arguments = "native 5 3";
   output = "${test_source_directory}/generated_test_3.ml.corrected";
   run;
   output = "${test_source_directory}/generated_test_3.ml.corrected";
   reference = "${test_source_directory}/generated_test_3.ml";
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
   arguments = "native 5 4";
   output = "${test_source_directory}/generated_test_4.ml.corrected";
   run;
   output = "${test_source_directory}/generated_test_4.ml.corrected";
   reference = "${test_source_directory}/generated_test_4.ml";
   check-program-output;
 }
*)
