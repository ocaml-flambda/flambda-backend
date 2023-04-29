(* TEST
readonly_files = "missing.mli present.mli client.ml"
* setup-ocamlc.byte-build-env
** ocamlc.byte
module = "missing.mli"
*** ocamlc.byte
flags = "-open Missing"
module = "present.mli"
**** script
script = "rm missing.cmi"
***** ocamlc.byte
flags = "-c -i"
module = "client.ml"
ocamlc_byte_exit_status = "2"
****** check-ocamlc.byte-output
*)
