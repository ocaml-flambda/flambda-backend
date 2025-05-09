(* TEST
 readonly_files = "metaprogramming.mli metaprogramming.ml generate_array_idx_tests.ml";
 (* Generate the bytecode/native code versions of
    [generate_array_idx_tests.ml]. This doesn't actually run the test;
    it just updates the generated test program (which is separately
    run by the test harness).
  *)
  setup-ocamlopt.opt-build-env;
  stack-allocation;
  program = "${test_source_directory}/generate.out";
  all_modules = "${readonly_files}";
  include stdlib_stable;
  include stdlib_upstream_compatible;
  ocamlopt.opt;
  set test_name = "";
  { test_name = "record_idx_access";
    {
      arguments = "native ${test_name}";
      output = "${test_source_directory}/generated_${test_name}_native_test.ml.corrected";
      run;
      output = "${test_source_directory}/generated_${test_name}_native_test.ml.corrected";
      reference = "${test_source_directory}/generated_${test_name}_native_test.ml";
      check-program-output;
    }{
      arguments = "bytecode ${test_name}";
      output = "${test_source_directory}/generated_${test_name}_bytecode_test.ml.corrected";
      run;
      output = "${test_source_directory}/generated_${test_name}_bytecode_test.ml.corrected";
      reference = "${test_source_directory}/generated_${test_name}_bytecode_test.ml";
      check-program-output;
    }}
  { test_name = "record_idx_access_local";
    {
      arguments = "native ${test_name}";
      output = "${test_source_directory}/generated_${test_name}_native_test.ml.corrected";
      run;
      output = "${test_source_directory}/generated_${test_name}_native_test.ml.corrected";
      reference = "${test_source_directory}/generated_${test_name}_native_test.ml";
      check-program-output;
    }{
      arguments = "bytecode ${test_name}";
      output = "${test_source_directory}/generated_${test_name}_bytecode_test.ml.corrected";
      run;
      output = "${test_source_directory}/generated_${test_name}_bytecode_test.ml.corrected";
      reference = "${test_source_directory}/generated_${test_name}_bytecode_test.ml";
      check-program-output;
    }}
  { test_name = "record_idx_deepening";
    {
      arguments = "native ${test_name}";
      output = "${test_source_directory}/generated_${test_name}_native_test.ml.corrected";
      run;
      output = "${test_source_directory}/generated_${test_name}_native_test.ml.corrected";
      reference = "${test_source_directory}/generated_${test_name}_native_test.ml";
      check-program-output;
    }{
      arguments = "bytecode ${test_name}";
      output = "${test_source_directory}/generated_${test_name}_bytecode_test.ml.corrected";
      run;
      output = "${test_source_directory}/generated_${test_name}_bytecode_test.ml.corrected";
      reference = "${test_source_directory}/generated_${test_name}_bytecode_test.ml";
      check-program-output;
    }}
  { test_name = "array_idx_access";
    {
      arguments = "native ${test_name}";
      output = "${test_source_directory}/generated_${test_name}_native_test.ml.corrected";
      run;
      output = "${test_source_directory}/generated_${test_name}_native_test.ml.corrected";
      reference = "${test_source_directory}/generated_${test_name}_native_test.ml";
      check-program-output;
    }{
      arguments = "bytecode ${test_name}";
      output = "${test_source_directory}/generated_${test_name}_bytecode_test.ml.corrected";
      run;
      output = "${test_source_directory}/generated_${test_name}_bytecode_test.ml.corrected";
      reference = "${test_source_directory}/generated_${test_name}_bytecode_test.ml";
      check-program-output;
    }}
  { test_name = "array_idx_access_local";
    {
      arguments = "native ${test_name}";
      output = "${test_source_directory}/generated_${test_name}_native_test.ml.corrected";
      run;
      output = "${test_source_directory}/generated_${test_name}_native_test.ml.corrected";
      reference = "${test_source_directory}/generated_${test_name}_native_test.ml";
      check-program-output;
    }{
      arguments = "bytecode ${test_name}";
      output = "${test_source_directory}/generated_${test_name}_bytecode_test.ml.corrected";
      run;
      output = "${test_source_directory}/generated_${test_name}_bytecode_test.ml.corrected";
      reference = "${test_source_directory}/generated_${test_name}_bytecode_test.ml";
      check-program-output;
    }}
  { test_name = "array_idx_deepening";
    {
      arguments = "native ${test_name}";
      output = "${test_source_directory}/generated_${test_name}_native_test.ml.corrected";
      run;
      output = "${test_source_directory}/generated_${test_name}_native_test.ml.corrected";
      reference = "${test_source_directory}/generated_${test_name}_native_test.ml";
      check-program-output;
    }{
      arguments = "bytecode ${test_name}";
      output = "${test_source_directory}/generated_${test_name}_bytecode_test.ml.corrected";
      run;
      output = "${test_source_directory}/generated_${test_name}_bytecode_test.ml.corrected";
      reference = "${test_source_directory}/generated_${test_name}_bytecode_test.ml";
      check-program-output;
    }}

*)
