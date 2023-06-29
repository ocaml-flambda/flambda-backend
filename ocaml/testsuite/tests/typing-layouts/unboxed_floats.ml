(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

(* CR layouts v2: These tests will change when we actually typecheck
   unboxed literals.
 *)

let id : float# -> float# = fun x -> x;;

id #4.0;;
[%%expect {|
val id : float# -> float# = <fun>
Line 3, characters 3-7:
3 | id #4.0;;
       ^^^^
Error: Unboxed float literals aren't supported yet.
|}];;

(* CR layouts: We should actually add the numbers here when
   we support that.
*)
let add (x : float#) (y : float#) = x +. y;;

add #4.0 #5.0;;
[%%expect {|
Line 1, characters 36-37:
1 | let add (x : float#) (y : float#) = x +. y;;
                                        ^
Error: This expression has type float# but an expression was expected of type
         float
|}];;

let apply (f : float# -> float# -> float#) (x : float#) (y : float#) =
  f x y;;

apply add #4.0 #5.0;;
[%%expect {|
val apply : (float# -> float# -> float#) -> float# -> float# -> float# =
  <fun>
Line 4, characters 6-9:
4 | apply add #4.0 #5.0;;
          ^^^
Error: Unbound value add
|}];;

let x = #3.0
let x = +#3.0
let x = -#3.0

let x = #3L
let x = +#3L
let x = -#3L

[%%expect{|
Line 1, characters 8-12:
1 | let x = #3.0
            ^^^^
Error: Unboxed float literals aren't supported yet.
|}]

