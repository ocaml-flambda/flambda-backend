(* TEST
   flags = "-stop-after parsing -dparsetree";
   setup-ocamlc.byte-build-env;
   ocamlc.byte;
   check-ocamlc.byte-output;
*)

(* This test is to check that (a) type decl kind annotations get printed
   appropriately and (b) product kinds are associated the way we think, and you
   can see that in the parse tree. *)

type t1 : (value mod global) & (value mod global);;

type t2 : value mod global & value mod global;;

type t3 : (value mod global & value) mod global
