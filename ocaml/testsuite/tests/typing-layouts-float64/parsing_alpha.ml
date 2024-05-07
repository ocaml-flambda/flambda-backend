(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* We can move this back to [parsing.ml] when mixed blocks
   exit alpha.
*)

type t = C of float#;;
[%%expect {|
type t = C of float#
|}];;

type t = C : float# -> t;;
[%%expect {|
type t = C : float# -> t
|}];;
