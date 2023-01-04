(* TEST

readonly_files="param.mli make.ml use_pack.ml"

* setup-ocamlc.byte-build-env
program = "${test_build_directory}/use_pack.byte"
** ocamlc.byte
flags = "-as-parameter"
module = "param.mli"
*** ocamlc.byte
flags = "-parameter Param -for-pack Pack"
module = "make.ml"
**** ocamlc.byte
module = ""
flags = "-pack"
program = "pack.cmo"
all_modules = "make.cmo"
***** ocamlc.byte
program = "${test_build_directory}/use_pack.byte"
flags = ""
all_modules = "pack.cmo use_pack.ml"
****** run
******* check-program-output
reference = "${test_source_directory}/use_pack.reference"


* setup-ocamlopt.byte-build-env
program = "${test_build_directory}/use_pack.asm"
** ocamlopt.byte
flags = "-as-parameter"
module = "param.mli"
*** ocamlopt.byte
flags = "-parameter Param -for-pack Pack"
module = "make.ml"
**** ocamlopt.byte
module = ""
flags = "-pack"
program = "pack.cmx"
all_modules = "make.cmx"
***** ocamlopt.byte
program = "${test_build_directory}/use_pack.asm"
flags = ""
all_modules = "pack.cmx use_pack.ml"
****** run
******* check-program-output
reference = "${test_source_directory}/use_pack.reference"
*)
