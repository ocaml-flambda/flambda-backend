(* TEST
   * expect
   flags = "-extension layouts_beta"
*)

let foo _t (type a) = local_ 1
let bar _t (type a : value) = local_ 2

[%%expect{|
val foo : 'a -> int@local = <fun>
val bar : 'a -> int@local = <fun>
|}]
