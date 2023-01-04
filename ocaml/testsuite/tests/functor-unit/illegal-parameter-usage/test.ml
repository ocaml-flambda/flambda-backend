(* TEST
readonly_files="param.mli param.ml use_param.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-parameter-of Make -for-pack Pack"
module = "param.mli"
ocamlc_byte_exit_status = "2"
** ocamlc.byte
flags = "-parameter-of Make"
module = "param.mli"
*** ocamlc.byte
flags = "-parameter-of Make"
module = "param.ml"
ocamlc_byte_exit_status = "2"
*** ocamlc.byte
module = "use_param.ml"
program = "use_param.cmo"
all_modules = ""
ocamlc_byte_exit_status = "2"
compiler_reference = "${test_source_directory}/use_param.reference"
**** check-ocamlc.byte-output
*)
