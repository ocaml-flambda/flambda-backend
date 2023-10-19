(* TEST

readonly_files = "bad_param_impl.mli bad_param_impl.reference"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-as-parameter"
module = "bad_param_impl.mli"
*** ocamlc.byte
flags = ""
module = "bad_param_impl.ml"
ocamlc_byte_exit_status = "2"
compiler_output = "bad_param_impl.output"
**** skip
reason = "error broken, will be fixed by #1764"
***** check-ocamlc.byte-output
compiler_reference = "bad_param_impl.reference"
*)
