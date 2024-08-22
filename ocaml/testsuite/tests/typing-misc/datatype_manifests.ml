(* TEST
 expect;
*)

type t = int * int = { foo : string }
[%%expect{|
type t = int * int = { foo : string; }
|}];;

let f (x : int * int) = (x : t)
[%%expect{|
val f : int * int -> t = <fun>
|}];;
