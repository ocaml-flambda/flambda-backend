(* TEST
readonly_files = "source_jane_street.ml ppx_no_op.ml"
include ocamlcommon
* setup-ocamlc.byte-build-env
** ocamlc.byte
program = "${test_build_directory}/ppx_no_op.exe"
all_modules = "ppx_no_op.ml"
*** ocamlc.byte
module = "source_jane_street.ml"
flags = "-I ${test_build_directory} \
         -w -26 \
         -extension layouts \
         -extension comprehensions \
         -ppx ${program}"
**** check-ocamlc.byte-output
*)

(* This test ensures that Jane Street syntax continues to be
   handled properly by the compiler even after applying a PPX rewriter. *)
