(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

let foo _t (type a) = local_ 1
let bar _t (type a : value) = local_ 2

[%%expect{|
val foo : 'a -> local_ int = <fun>
val bar : 'a -> local_ int = <fun>
|}]
