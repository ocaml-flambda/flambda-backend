(* TEST

readonly_files="param.mli param2.mli make.ml use_make.ml"

* setup-ocamlc.byte-build-env
program = "${test_build_directory}/use_make.byte"
** ocamlc.byte
flags = "-as-parameter"
module = "param.mli"
*** ocamlc.byte
flags = "-as-parameter"
module = "param2.mli"
**** ocamlc.byte
flags = "-parameter Param -parameter Param2"
module = "make.ml"
***** ocamlc.byte
module = "use_make.ml"
flags = ""
program = "use_make.cmx"
all_modules = "make.cmo use_make.ml"
***** ocamlc.byte
module = ""
program = "${test_build_directory}/use_make.byte"
flags = ""
all_modules = "make.cmo use_make.cmo"
****** run
******* check-program-output
reference = "${test_source_directory}/use_make.reference"

* setup-ocamlopt.byte-build-env
program = "${test_build_directory}/use_make.asm"
** ocamlopt.byte
flags = "-as-parameter"
module = "param.mli"
*** ocamlopt.byte
flags = "-as-parameter"
module = "param2.mli"
**** ocamlopt.byte
flags = "-parameter Param -parameter Param2"
module = "make.ml"
***** ocamlopt.byte
module = "use_make.ml"
flags = ""
program = "use_make.cmx"
all_modules = "make.cmx use_make.ml"
***** ocamlopt.byte
module = ""
program = "${test_build_directory}/use_make.asm"
flags = ""
all_modules = "make.cmx use_make.cmx"
****** run
******* check-program-output
reference = "${test_source_directory}/use_make.reference"
*)
