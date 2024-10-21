(* TEST
   readonly_files = "generate_stringlike_indexing.ml";
   setup-ocamlopt.opt-build-env;
   program = "${test_source_directory}/generate.out";
   all_modules = "generate_stringlike_indexing.ml";
   ocamlopt.opt;
   output = "${test_source_directory}/unboxed_int_stringlike_indexing.ml.corrected";
   run;
   output = "${test_source_directory}/unboxed_int_stringlike_indexing.ml.corrected";
   reference = "${test_source_directory}/unboxed_int_stringlike_indexing.ml";
   check-program-output;
*)
