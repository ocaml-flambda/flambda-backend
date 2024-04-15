(* TEST_BELOW
(* Blank lines added here to preserve locations. *)







*)

(* Just ensure that we're running the check on mli files too *)

val a1 : int [@deprecated]   (* rejected *)
val a2 : int [@@deprecated]  (* accepted *)


(* TEST
 flags = "-w +A-60-70";
 setup-ocamlc.byte-build-env;
 compile_only = "true";
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
