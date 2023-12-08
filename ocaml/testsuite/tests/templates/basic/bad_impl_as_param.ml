(* TEST

readonly_files = "bad_impl_as_param.reference"

* setup-ocamlc.byte-build-env
** ocamlc.byte
flags = "-as-parameter"
modules = "bad_impl_as_param.ml"
ocamlc_byte_exit_status = "2"
compiler_output = "bad_impl_as_param.output"
*** check-ocamlc.byte-output
compiler_reference = "bad_impl_as_param.reference" *)
