(* TEST
 flags = "-extension-universe alpha";
 expect;
*)

type t = float32;;
[%%expect{|
type t = float32
|}];;
