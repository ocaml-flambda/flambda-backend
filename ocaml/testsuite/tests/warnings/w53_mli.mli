(* TEST

flags = "-w +A-60-70"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

(* Just ensure that we're running the check on mli files too *)

val a1 : int [@deprecated]   (* rejected *)
val a2 : int [@@deprecated]  (* accepted *)

