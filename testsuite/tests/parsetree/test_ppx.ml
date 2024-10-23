(* TEST
 readonly_files = "source_jane_street.ml ppx_no_op.ml";
 include ocamlcommon;
 include stdlib_upstream_compatible;
 setup-ocamlc.byte-build-env;
 program = "${test_build_directory}/ppx_no_op.exe";
 all_modules = "ppx_no_op.ml";
 ocamlc.byte;
 module = "source_jane_street.ml";
 ocamlc_byte_exit_status = "2";
 flags = "-I ${test_build_directory} -w -26 -extension-universe alpha -ppx ${program}";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)

(* This test ensures that Jane Street syntax continues to be
   handled properly by the compiler even after applying a PPX rewriter. *)

(* source_jane_street doesn't actually compile, because it's useful to have
   failing code in there. That's fine. This still tests that the type-checker
   gives the error, instead of falling over during parsing after ppx processing.
*)
