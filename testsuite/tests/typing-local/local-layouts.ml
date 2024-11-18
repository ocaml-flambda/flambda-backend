(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

let foo _t (type a) = exclave_ 1
let bar _t (type a : value) = exclave_ 2

[%%expect{|
val foo : ('a : value_or_null). 'a -> local_ int = <fun>
val bar : ('a : value_or_null). 'a -> local_ int = <fun>
|}]
