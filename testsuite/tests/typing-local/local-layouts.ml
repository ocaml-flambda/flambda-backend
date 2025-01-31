(* TEST
 expect;
*)

let foo _t (type a) = exclave_ 1
let bar _t (type a : value) = exclave_ 2

[%%expect{|
val foo : 'a -> local_ int @ unyielding = <fun>
val bar : 'a -> local_ int @ unyielding = <fun>
|}]
