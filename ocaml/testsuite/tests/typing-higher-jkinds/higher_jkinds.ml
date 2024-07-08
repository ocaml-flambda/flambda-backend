(* TEST
 flags = " -g -extension layouts_alpha";
 setup-ocamlopt.byte-build-env;
 ocamlopt.byte;
 check-ocamlopt.byte-output;
*)

type t : value => value
