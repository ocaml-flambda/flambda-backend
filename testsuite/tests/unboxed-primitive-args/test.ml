(* TEST

readonly_files = "common.mli common.ml test_common.c test_common.h"

* setup-ocamlopt.opt-build-env
** ocaml
test_file = "${test_source_directory}/gen_test.ml"
ocaml_script_as_argument = "true"
arguments = "c"
compiler_output = "stubs.c"
*** ocaml
arguments = "ml"
compiler_output = "main.ml"
**** script
script = "${cc} -msse4.2 -c test_common.c -I ../../../../../../../../runtime"
***** script
script = "${cc} -msse4.2 -c stubs.c -I ../../../../../../../../runtime"
****** ocamlopt.opt
ocamlopt_flags = "-fsimd"
all_modules = "test_common.o stubs.o common.mli common.ml main.ml"
******* run
******** check-program-output

*)
