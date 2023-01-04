(* TEST

readonly_files="param.mli make.ml make.mli use_make.ml"

* setup-ocamlc.byte-build-env
program = "${test_build_directory}/use_make.byte"
** ocamlc.byte
flags = "-parameter-of Make"
module = "param.mli"
*** ocamlc.byte
flags = "-parameter Param"
module = "make.mli"
**** ocamlc.byte
flags = "-parameter Param"
module = "make.ml"
***** ocamlc.byte
module = "use_make.ml"
flags = ""
****** ocamlc.byte
module = ""
program = "${test_build_directory}/use_make.byte"
flags = ""
all_modules = "make.cmo use_make.cmo"
******* run
******** check-program-output
reference = "${test_source_directory}/use_make.reference"


* setup-ocamlopt.byte-build-env
program = "${test_build_directory}/use_make.asm"
** ocamlopt.byte
flags = "-parameter-of Make"
module = "param.mli"
*** ocamlopt.byte
flags = "-parameter Param"
module = "make.mli"
**** ocamlopt.byte
flags = "-parameter Param"
module = "make.ml"
***** ocamlopt.byte
module = "use_make.ml"
flags = ""
****** ocamlopt.byte
module = ""
program = "${test_build_directory}/use_make.asm"
flags = ""
all_modules = "make.cmx use_make.cmx"
******* run
******** check-program-output
reference = "${test_source_directory}/use_make.reference"
*)
