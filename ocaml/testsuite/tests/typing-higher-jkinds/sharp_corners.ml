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

(* Requires basic Tconstr-aware Tapp: this should
   *probably* fail [Typedecl.check_regularity]? *)
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
Line 3, characters 11-12:
3 | let f (x : t s) : list s = x
               ^
Error: The type constructor t expects 1 argument(s),
       but is here applied to 0 argument(s)
|}]


(* Over/under-application in [moregen] *)
type l : value => value
[%%expect{|
type l : value => value
|}]
module S : sig
  type t = l
end = struct
  type t = int l
end
[%%expect{|
Line 4, characters 11-16:
4 |   type t = int l
               ^^^^^
Error: The type constructor l expects 0 argument(s),
       but is here applied to 1 argument(s)
|}]
module S : sig
  type t = int l
end = struct
  type t = l
end
[%%expect{|
Line 4, characters 2-12:
4 |   type t = l
      ^^^^^^^^^^
Error: The kind of type l is ((value) => value) (...??)
       But the kind of type l must be a subkind of any
         because of the definition of t at line 4, characters 2-12.
|}]

(* Over/under-application in [subtype] and [build_subtype] *)
type l : value => value
[%%expect{|
type l : value => value
|}]
let f x = (x : l :> l)
let f x = (x : int l :> int l)
[%%expect{|
Uncaught exception: Jkind.Unexpected_higher_jkind("Coerced arrow to type jkind")

|}]
let f x = (x : l :> int l)
[%%expect{|
Line 1, characters 20-25:
1 | let f x = (x : l :> int l)
                        ^^^^^
Error: The type constructor l expects 0 argument(s),
       but is here applied to 1 argument(s)
|}]
let f x = (x : int l :> l)
[%%expect{|
Line 1, characters 15-20:
1 | let f x = (x : int l :> l)
                   ^^^^^
Error: The type constructor l expects 0 argument(s),
       but is here applied to 1 argument(s)
|}]
