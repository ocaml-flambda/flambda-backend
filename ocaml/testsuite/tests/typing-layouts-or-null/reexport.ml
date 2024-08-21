(* TEST
 flags = "-extension layouts_alpha";
 expect;
*)

(* CR layouts v3: ['a or_null] can't be re-exported normally,
   because users can't define their own [Null]-like constructors. *)
module Or_null = struct
  type ('a : value) t : value_or_null = 'a or_null =
    | Null
    | This of 'a
end

[%%expect{|
Lines 2-4, characters 2-16:
2 | ..type ('a : value) t : value_or_null = 'a or_null =
3 |     | Null
4 |     | This of 'a
Error: This variant or record definition does not match that of type
         'a or_null
       Their internal representations differ:
       the original definition has a null constructor.
|}]

module Or_null = struct
  type ('a : value) t : value_or_null = 'a or_null
end

[%%expect{|
module Or_null : sig type 'a t = 'a or_null end
|}]

(* Omitting the type representation leaves constructors unexported. *)

let n = Or_null.Null

[%%expect{|
Line 1, characters 8-20:
1 | let n = Or_null.Null
            ^^^^^^^^^^^^
Error: Unbound constructor Or_null.Null
|}]

let t v = Or_null.This v

[%%expect{|
Line 1, characters 10-22:
1 | let t v = Or_null.This v
              ^^^^^^^^^^^^
Error: Unbound constructor Or_null.This
|}]

(* [@@or_null_reexport] re-exports those constructors. *)

module Or_null = struct
  type ('a : value) t : value_or_null = 'a or_null [@@or_null_reexport]
end
let n = Or_null.Null
let t v = Or_null.This v

[%%expect{|
module Or_null :
  sig
    type 'a t = 'a or_null : value_or_null = Null | This of 'a [@@unboxed]
  end
val n : 'a Or_null.t = Or_null.Null
val t : 'a -> 'a Or_null.t = <fun>
|}]

(* The jkind of [Or_null] is still correctly [value_or_null]. *)
let fail = Or_null.This (Or_null.This 5)

[%%expect{|
Line 1, characters 24-40:
1 | let fail = Or_null.This (Or_null.This 5)
                            ^^^^^^^^^^^^^^^^
Error: This expression has type 'a Or_null.t = 'a or_null
       but an expression was expected of type ('b : value)
       The kind of 'a Or_null.t is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of 'a Or_null.t must be a subkind of value
         because of the definition of t at line 2, characters 2-71.
|}]

(* Type annotations are not required. *)

module Or_null = struct
  type 'a t = 'a or_null [@@or_null_reexport]
end
let fail = Or_null.This (Or_null.This 5)

[%%expect{|
module Or_null :
  sig type 'a t = 'a or_null = Null | This of 'a [@@unboxed] end
Line 4, characters 24-40:
4 | let fail = Or_null.This (Or_null.This 5)
                            ^^^^^^^^^^^^^^^^
Error: This expression has type 'a Or_null.t = 'a or_null
       but an expression was expected of type ('b : value)
       The kind of 'a Or_null.t is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of 'a Or_null.t must be a subkind of value
         because of the definition of t at line 2, characters 2-45.
|}]

(* Incorrect annotations still cause errors. *)

type 'a t : value = 'a or_null [@@or_null_reexport]

[%%expect{|
Line 1, characters 0-51:
1 | type 'a t : value = 'a or_null [@@or_null_reexport]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The kind of type 'a or_null is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of type 'a or_null must be a subkind of value
         because of the definition of t at line 1, characters 0-51.
|}]

type 'a t : float64 = 'a or_null [@@or_null_reexport]

[%%expect{|
Line 1, characters 0-53:
1 | type 'a t : float64 = 'a or_null [@@or_null_reexport]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type 'a or_null is value
         because it is the primitive value_or_null type or_null.
       But the layout of type 'a or_null must be a sublayout of float64
         because of the definition of t at line 1, characters 0-53.
|}]

type ('a : float64) t = 'a or_null [@@or_null_reexport]

[%%expect{|
Line 1, characters 24-26:
1 | type ('a : float64) t = 'a or_null [@@or_null_reexport]
                            ^^
Error: This type ('a : value) should be an instance of type ('a0 : float64)
       The layout of 'a is float64
         because of the annotation on 'a in the declaration of the type t.
       But the layout of 'a must overlap with value
         because the type argument of or_null has layout value.
|}]

(* The behavior here is correct but confusing: ['a] is restricted to [value],
   which is a subjkind of [value_or_null]. *)

module Or_null = struct
  type ('a : value_or_null) t = 'a or_null [@@or_null_reexport]
end
let fail = Or_null.This (Or_null.This 5)

[%%expect{|
module Or_null :
  sig type 'a t = 'a or_null = Null | This of 'a [@@unboxed] end
Line 4, characters 24-40:
4 | let fail = Or_null.This (Or_null.This 5)
                            ^^^^^^^^^^^^^^^^
Error: This expression has type 'a Or_null.t = 'a or_null
       but an expression was expected of type ('b : value)
       The kind of 'a Or_null.t is value_or_null
         because it is the primitive value_or_null type or_null.
       But the kind of 'a Or_null.t must be a subkind of value
         because of the definition of t at line 2, characters 2-63.
|}]

(* This fails, just as [type t = int option = None | Some of int] would. *)

type t = int or_null [@@or_null_reexport]

[%%expect{|
Line 1, characters 0-41:
1 | type t = int or_null [@@or_null_reexport]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         int or_null
       They have different arities.
|}]

(* [@@or_null_reexport] requires a manifest. *)

type 'a t [@@or_null_reexport]

[%%expect{|
Line 1, characters 0-30:
1 | type 'a t [@@or_null_reexport]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid reexport declaration.
       Type t must be defined to be equal to or_null
       with no explicit representation.
|}]

(* [@@or_null_reexport] forbids explicit representation. *)

type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport]

[%%expect{|
Line 1, characters 0-63:
1 | type 'a t = 'a or_null = Null | This of 'a [@@or_null_reexport]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid reexport declaration.
       Type t must be defined to be equal to or_null
       with no explicit representation.
|}]

(* [@@or_null_reexport] requires the type to be equal to ['a null]. *)

type 'a t = 'a option [@@or_null_reexport]

[%%expect{|
Line 1, characters 0-42:
1 | type 'a t = 'a option [@@or_null_reexport]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Invalid reexport declaration.
       Type t must be defined to be equal to or_null
       with no explicit representation.
|}]

(* [@@or_null_reexport] behaves sanely in corner cases. *)

type 'a t = 'a or_null [@@or_null_reexport]
and t' = int or_null

[%%expect{|
type 'a t = 'a or_null = Null | This of 'a [@@unboxed]
and t' = int or_null
|}]

type 'a t = 'b or_null constraint 'b = int * 'a [@@or_null_reexport]

[%%expect{|
Line 1, characters 0-68:
1 | type 'a t = 'b or_null constraint 'b = int * 'a [@@or_null_reexport]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This variant or record definition does not match that of type
         (int * 'a) or_null
       Their parameters differ:
       The type int * 'a is not equal to the type 'a
|}]
