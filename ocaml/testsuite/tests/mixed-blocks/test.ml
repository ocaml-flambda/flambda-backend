(* TEST
   readonly_files = "generate_mixed_blocks_code.ml"

   (* Generate the bytecode/native code versions of
      [generate_mixed_blocks_code.ml] and run the corresponding programs. For
      both settings (bytecode/native code), we additionally generate a small
      example of such a program to check into source control.
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
      arguments = "1 native"
      output = "${test_source_directory}/generated_native_example.ml.corrected"
   **** check-program-output
      output = "${test_source_directory}/generated_native_example.ml.corrected"
      reference = "${test_source_directory}/generated_native_example.ml"
   *** run
      arguments = "75 native"
      output = "${test_source_directory}/generated_native.ml"
   **** runtime5
   ***** ocamlopt.opt
      all_modules = "${test_source_directory}/generated_native.ml"
      flags = "-extension layouts_alpha"
   ****** run
      stdout = "${test_source_directory}/generated_native_output.ml"
      stderr = "${test_source_directory}/generated_native_output.ml"
   ******* check-program-output
      output = "${test_source_directory}/generated_native_output.ml"
      reference = "${test_source_directory}/test.reference"

   (* Next, bytecode. *)

   * setup-ocamlc.opt-build-env
   ** ocamlc.opt
       program = "${test_source_directory}/generate.out"
       all_modules = "generate_mixed_blocks_code.ml"
   *** run
      arguments = "1 byte"
      output = "${test_source_directory}/generated_byte_example.ml.corrected"
   **** check-program-output
      output = "${test_source_directory}/generated_byte_example.ml.corrected"
      reference = "${test_source_directory}/generated_byte_example.ml"
   *** run
      arguments = "300 byte"
      output = "${test_source_directory}/generated_byte.ml"
   **** runtime5
   ***** ocamlc.opt
         all_modules = "${test_source_directory}/generated_byte.ml"
         flags = "-extension layouts_alpha"
   ****** run
      stdout = "${test_source_directory}/generated_byte_output.ml"
      stderr = "${test_source_directory}/generated_byte_output.ml"
   ******* check-program-output
      output = "${test_source_directory}/generated_byte_output.ml"
      reference = "${test_source_directory}/test.reference"
*)
