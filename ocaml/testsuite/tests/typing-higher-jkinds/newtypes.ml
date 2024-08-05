(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

(* Basic tests *)

let x = (42, 1337)
[%%expect {|
val x : int * int = (42, 1337)
|}]

type 'a l = new int * 'a
let x1 = (x :> int l)
[%%expect {|
type 'a l = new int * 'a
val x1 : int l = (42, 1337)
|}]

type 'a r = new 'a * int
let x2 = (x :> int r)
[%%expect {|
type 'a r = new 'a * int
val x2 : int r = (42, 1337)
|}]

type 'a l' = new 'a l
let x1' = (x :> int l')
[%%expect {|
type 'a l' = new 'a l
val x1' : int l' = (42, 1337)
|}]

let x2_ = (x1' :> int r) (* expand on both sides *)
[%%expect {|
val x2_ : int r = (42, 1337)
|}]


(* New types don't unify *)

let id_l (x : int l) = x
let id_r (x : int r) = x
let id_l' (x : int l') = x
[%%expect {|
val id_l : int l -> int l = <fun>
val id_r : int r -> int r = <fun>
val id_l' : int l' -> int l' = <fun>
|}]

let y11 = id_l x1
[%%expect {|
val y11 : int l = (42, 1337)
|}]
let y11' = id_l x1'
[%%expect {|
Line 1, characters 16-19:
1 | let y11' = id_l x1'
                    ^^^
Error: This expression has type int l' but an expression was expected of type
         int l
|}]
let y12 = id_l x2
[%%expect {|
Line 1, characters 15-17:
1 | let y12 = id_l x2
                   ^^
Error: This expression has type int r but an expression was expected of type
         int l
|}]

(* Nested coercions *)

let xs = [(0, 1); (1, 2); (2, 3)]
let xs1 = (xs :> int l list)
[%%expect {|
val xs : (int * int) list = [(0, 1); (1, 2); (2, 3)]
val xs1 : int l list = [(0, 1); (1, 2); (2, 3)]
|}]

let xs1' = (xs :> int l' list)
[%%expect {|
val xs1' : int l' list = [(0, 1); (1, 2); (2, 3)]
|}]


(* Non-abstract types cannot be new *)

type 'a t = new { foo : 'a }
[%%expect {|
Line 1, characters 0-28:
1 | type 'a t = new { foo : 'a }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot define a non-abstract new type
|}]

type 'a t = new Foo | Bar
[%%expect {|
Line 1, characters 0-25:
1 | type 'a t = new Foo | Bar
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot define a non-abstract new type
|}]
