(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

(* Should fail due to manifest arity mismatch,
   even with unapplied datatype constructors *)
type 'a t = Foo of 'a
type 'a s = t = Foo of 'a

[%%expect{|
type 'a t = Foo of 'a
Line 2, characters 12-13:
2 | type 'a s = t = Foo of 'a
                ^
Error: The type constructor t expects 1 argument(s),
       but is here applied to 0 argument(s)
|}]

(* Requires basic Tconstr-aware Tapp *)
type 'a t = Foo of 'a (t)

[%%expect{|
Uncaught exception: Failure("General type application is not implemented")

|}]

(* Requires unapplied datatype constructors *)
type t = list
let x : int t = []

[%%expect{|
Line 1, characters 9-13:
1 | type t = list
             ^^^^
Error: The type constructor list expects 1 argument(s),
       but is here applied to 0 argument(s)
|}]

(* Requires unapplied datatype constructors *)
type 'a t = 'a list = [] | (::) of 'a * 'a t
type ('a : value => value) s

let f (x : t s) : list s = x

[%%expect{|
type 'a t = 'a list = [] | (::) of 'a * 'a t
type ('a : value => value) s
Line 4, characters 11-12:
4 | let f (x : t s) : list s = x
               ^
Error: The type constructor t expects 1 argument(s),
       but is here applied to 0 argument(s)
|}]
