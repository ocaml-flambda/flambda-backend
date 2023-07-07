(* TEST
flags = "-extension labeled_tuples"
* setup-ocamlc.byte-build-env
** ocamlc.byte
ocamlc_byte_exit_status = "2"
*** check-ocamlc.byte-output
*)
let foo (just_x : (x : int)) = just_x
