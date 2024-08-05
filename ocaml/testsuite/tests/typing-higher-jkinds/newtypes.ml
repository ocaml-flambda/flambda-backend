(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

let x = (42, 1337)
[%%expect {|
val x : int * int = (42, 1337)
|}]

type 'a l = new int * 'a
let x1 = (x :> _ l)
[%%expect {|
type 'a l = new int * 'a
Line 2, characters 10-11:
2 | let x1 = (x :> _ l)
              ^
Error: This expression cannot be coerced to type 'a l; it has type int * int
       but is here used with type 'a l
|}]

type 'a r = new 'a * int
let x2 = (x :> _ r)
[%%expect {|
type 'a r = new 'a * int
Line 2, characters 10-11:
2 | let x2 = (x :> _ r)
              ^
Error: This expression cannot be coerced to type 'a r; it has type int * int
       but is here used with type 'a r
|}]

type 'a l' = new 'a l
let x1' = (x :> _ l')
[%%expect {|
type 'a l' = new 'a l
Line 2, characters 11-12:
2 | let x1' = (x :> _ l')
               ^
Error: This expression cannot be coerced to type 'a l'; it has type int * int
       but is here used with type 'a l'
|}]

let xs = [(0, 1); (1, 2); (2, 3)]
let xs1 = (xs :> _ l list)
[%%expect {|
val xs : (int * int) list = [(0, 1); (1, 2); (2, 3)]
Line 2, characters 11-13:
2 | let xs1 = (xs :> _ l list)
               ^^
Error: This expression cannot be coerced to type 'a l list; it has type
         (int * int) list
       but is here used with type 'a l list
       Type int * int is not compatible with type 'a l
|}]

let xs1' = (xs :> _ l' list)
[%%expect {|
Line 1, characters 12-14:
1 | let xs1' = (xs :> _ l' list)
                ^^
Error: This expression cannot be coerced to type 'a l' list; it has type
         (int * int) list
       but is here used with type 'a l' list
       Type int * int is not compatible with type 'a l'
|}]

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
