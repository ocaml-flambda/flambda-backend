(* TEST
   * expect
   flags = "-extension layouts_beta"
*)

let foo _t (type a) = local_ 1
let bar _t (type a : value) = local_ 2

[%%expect{|
val foo : 'a -> local_ int = <fun>
Line 2, characters 30-38:
2 | let bar _t (type a : value) = local_ 2
                                  ^^^^^^^^
Error: This local value escapes its region
  Hint: Cannot return local value without an "exclave_" annotation
|}]
(* that's just wrong; the layout annotation shouldn't affect locality *)
