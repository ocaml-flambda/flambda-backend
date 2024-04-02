(* TEST
   readonly_files = "generate_mixed_blocks_code.ml"

   (* Generate the bytecode/native code versions of
      [generate_mixed_blocks_code.ml]. This doesn't actually run the test;
      it just updates the generated test program (which is separately
      run by the test harness).
    *)

   (* First, native code. We test fewer mixed records in native code
      than bytecode because the bottleneck for this test's runtime
      is ocamlopt compilation time.
   *)

   * setup-ocamlopt.opt-build-env
   ** ocamlopt.opt
      program = "${test_source_directory}/generate.out"
      all_modules = "generate_mixed_blocks_code.ml"
   *** run
      arguments = "75 native"
      output = "${test_source_directory}/generated_native_test.ml.corrected"
   **** check-program-output
      output = "${test_source_directory}/generated_native_test.ml.corrected"
      reference = "${test_source_directory}/generated_native_test.ml"

   (* Next, bytecode. *)

   * setup-ocamlopt.opt-build-env
   ** ocamlopt.opt
       program = "${test_source_directory}/generate.out"
       all_modules = "generate_mixed_blocks_code.ml"
   *** run
      arguments = "300 byte"
      output = "${test_source_directory}/generated_byte_test.ml.corrected"
   **** check-program-output
      output = "${test_source_directory}/generated_byte_test.ml.corrected"
      reference = "${test_source_directory}/generated_byte_test.ml"
*)
