(* TEST

readonly_files = "common.mli common.ml test_common.c test_common.h"

* flambda2
** setup-ocamlopt.opt-build-env
*** ocaml
test_file = "${test_source_directory}/gen_test.ml"
ocaml_script_as_argument = "true"
arguments = "c"
compiler_output = "stubs.c"
**** ocaml
arguments = "ml"
compiler_output = "main.ml"
***** ocamlopt.opt
ocamlopt_flags = "-extension simd -extension small_numbers -cc '${cc} -msse4.2'"
all_modules = "test_common.c stubs.c common.mli common.ml test0.ml test1.ml test2.ml test3.ml test4.ml main.ml"
****** run
******* check-program-output

*)
(* We use flambda2 above as a proxy to indicate SIMD is supported *)
