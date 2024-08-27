(* TEST
  flags = "-extension layouts_alpha";
  expect;
*)

(* Should fail due to manifest arity mismatch *)
type 'a t = Foo of 'a
type 'a s = t = Foo of 'a
[%%expect{|
type 'a t = Foo of 'a
Line 2, characters 0-25:
2 | type 'a s = t = Foo of 'a
    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type t
       They have different arities.
|}]

(* Requires basic Tconstr-aware Tapp: this should
   *probably* fail [Typedecl.check_regularity]? *)
type 'a t = Foo of 'a (t)
[%%expect{|
Uncaught exception: Failure("General type application is not implemented")

|}]

(* Should work, bug due to higher-kinded substitution not working.
   Requires general higher-kinded type applications *)
type t : value => value = list
let x : int t = []
[%%expect{|
type t = list
Line 2, characters 16-18:
2 | let x : int t = []
                    ^^
Error: This expression has type 'a list
       but an expression was expected of type int t
|}]

(* As above, substitution in higher-kinded type applications *)
type 'a t = 'a list = [] | (::) of 'a * 'a t
type ('a : value => value) s
let f (x : t s) : list s = x
[%%expect{|
type 'a t = 'a list = [] | (::) of 'a * 'a t
type ('a : value => value) s
Line 3, characters 27-28:
3 | let f (x : t s) : list s = x
                               ^
Error: This expression has type t s but an expression was expected of type
         list s
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
Line 2, characters 2-12:
2 |   type t = l
      ^^^^^^^^^^
Error: The kind of type l is ((value) => value)
         because of the definition of l at line 1, characters 0-23.
       But the kind of type l must be a subkind of any
         because of the definition of t at line 2, characters 2-12.
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
Error: The kind of type l is ((value) => value)
         because of the definition of l at line 1, characters 0-23.
       But the kind of type l must be a subkind of any
         because of the definition of t at line 4, characters 2-12.
|}]

(* Over/under-application in [subtype] and [build_subtype]
   These should fail gracefully due to a jkind error (x is type-jkinded) *)
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
Uncaught exception: Invalid_argument("List.combine")

|}]
let f x = (x : int l :> l)
[%%expect{|
Uncaught exception: Invalid_argument("List.combine")

|}]

type ('a : float64) t = Foo of 'a
type ('f : value => value) s
let foo (x : t s) = x
[%%expect{|
Uncaught exception: Invalid_argument("List.combine")

|}]
