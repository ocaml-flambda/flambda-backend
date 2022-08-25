(* TEST
flags = "-i -extension local"
* setup-ocamlc.byte-build-env
** ocamlc.byte
*** check-ocamlc.byte-output
*)

let f r = !r
let g () = 1 + f (ref 42)
