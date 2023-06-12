(* TEST
   * expect
*)

let x = ~~(~x:1, ~y:2)

[%%expect{|
val x : x: int * y: int = (~x: 1, ~y: 2)
|}];;

let z = 5
let punned = 2
let _ = ~~( ~x: 5, 2, ~z, ~(punned:int))
[%%expect{|
val z : int = 5
val punned : int = 2
- : x: int * int * z: int * punned: int = (~x: 5, 2, ~z: 5, ~punned: 2)
|}]

type ('a, 'b) pair = Pair of 'a * 'b
let x = Pair (~~(~x: 5, 2))

[%%expect{|
type ('a, 'b) pair = Pair of 'a * 'b
Line 2, characters 8-27:
2 | let x = Pair (~~(~x: 5, 2))
            ^^^^^^^^^^^^^^^^^^^
Error: Constructors cannot receive labeled arguments. Consider using an inline record instead.
|}]

(* Happy case *)
let foo b = if b then
   ~~(~a: "s", 10, ~c: "hi")
else
   ~~(~a: "5", 10, ~c: "hi")
[%%expect{|
val foo : bool -> a: string * int * c: string = <fun>
|}]

(* Missing label (the type vars in the error aren't ideal, but the same thing happens when
   unifying normal tuples of different lengths) *)
let foo b = if b then
   ~~(~a: "s", 10, "hi")
else
   ~~(~a: "5", 10, ~c: "hi")
[%%expect{|
Line 4, characters 3-28:
4 |    ~~(~a: "5", 10, ~c: "hi")
       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type a: 'a * 'b * c: 'c
       but an expression was expected of type a: string * int * string
|}]

(* Missing labeled component *)
let foo b = if b then
   ~~(~a: "s", 10)
else
   ~~(~a: "5", 10, ~c: "hi")
[%%expect{|
Line 4, characters 3-28:
4 |    ~~(~a: "5", 10, ~c: "hi")
       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type a: 'a * 'b * c: 'c
       but an expression was expected of type a: string * int
|}]

(* Wrong label *)
let foo b = if b then
   ~~(~a: "s", 10, ~a: "hi")
else
   ~~(~a: "5", 10, ~c: "hi")
[%%expect{|
Line 4, characters 3-28:
4 |    ~~(~a: "5", 10, ~c: "hi")
       ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type a: 'a * 'b * c: 'c
       but an expression was expected of type a: string * int * a: string
|}]

(* Types in function argument/return *)
let default = ~~(~x: 1, ~y: 2)
let choose_pt replace_with_default pt =
   if replace_with_default then
      default
   else
      pt
[%%expect{|
val default : x: int * y: int = (~x: 1, ~y: 2)
val choose_pt : bool -> x: int * y: int -> x: int * y: int = <fun>
|}]

(* Application happy case *)
let a = choose_pt true (~~(~x: 5, ~y: 6))
[%%expect{|
val a : x: int * y: int = (~x: 1, ~y: 2)
|}]

(* CR labeled tuples: reordering should eventually work *)
let a = choose_pt true (~~(~y: 6, ~x: 5))
[%%expect{|
Line 1, characters 23-41:
1 | let a = choose_pt true (~~(~y: 6, ~x: 5))
                           ^^^^^^^^^^^^^^^^^^
Error: This expression has type y: 'a * x: 'b
       but an expression was expected of type x: int * y: int
|}]