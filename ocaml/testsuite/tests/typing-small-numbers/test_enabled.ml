(* TEST
 flags = "-extension small_numbers";
 expect;
*)

type t = float32;;
[%%expect{|
type t = float32
|}];;
