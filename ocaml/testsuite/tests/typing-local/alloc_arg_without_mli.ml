(* TEST

readonly_files = "alloc_arg_with_mli.ml"

* stack-allocation
** native
compile_only = "false"
flags = "-o ${test_build_directory}/alloc_arg_without_mli.opt"
module = "alloc_arg_with_mli.ml"
*)

(* Check whether functions that *could* take their argument
   locally allow their callers to locally construct the argument.

   Among other things, this checks how mode variables are defaulted in the
   absence of an mli.

   See the [..._with_mli.ml] version of this test for how mode variables
   are defaulted in the presence of an mli.
 *)
