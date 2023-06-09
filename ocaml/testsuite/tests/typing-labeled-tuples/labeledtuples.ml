(* TEST
   * expect
*)

let x = ~~(~x:1, ~y:2)

[%%expect{|
Line 1, characters 8-22:
1 | let x = ~~(~x:1, ~y:2)
            ^^^^^^^^^^^^^^
Error: Labeled tuples are not yet supported
|}];;

let z = 5
let punned = 2
let _ = ~~( ~x: 5, 2, ~z, ~(punned:int))
[%%expect{|
val z : int = 5
val punned : int = 2
Line 3, characters 8-40:
3 | let _ = ~~( ~x: 5, 2, ~z, ~(punned:int))
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Labeled tuples are not yet supported
|}]

type ('a, 'b) pair = Pair of 'a * 'b
let x = Pair (~~(~x: 5, 2))

[%%expect{|
type ('a, 'b) pair = Pair of 'a * 'b
Line 2, characters 8-27:
2 | let x = Pair (~~(~x: 5, 2))
            ^^^^^^^^^^^^^^^^^^^
Error: Constructors cannot receive labeled arguments
|}]
