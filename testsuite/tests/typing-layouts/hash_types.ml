(* TEST
 flags = "-extension layouts_beta";
 expect;
*)

(* This test exercises the typechecking of [#] as an operator on type paths that
   accesses the unboxed version of a type.

   For tests of the behavior of the unboxed types themselves, see
   [typing-layouts-{bits32,bits64,float32,float64,word,products,etc.}]. *)

(****************************)
(* Basics and documentation *)

(* Some predefined numbers have unboxed versions, accessible with [#]. *)
type float_u : float64 = float#
[%%expect{|
type float_u = float#
|}]

(* Unboxed versions are accessible through aliases... *)
module Float = struct
  type t = float
end
type t = Float.t#
[%%expect{|
module Float : sig type t = float end
type t = Float.t#
|}]

(* ...but not if the alias is abstract. *)
module Float : sig
  type t
end = struct
  type t = float
end
[%%expect{|
module Float : sig type t end
|}]
type bad = Float.t#
[%%expect{|
Line 1, characters 11-19:
1 | type bad = Float.t#
               ^^^^^^^^
Error: "Float.t" has no unboxed version.
|}]

(* The alias can also have type parameters. *)
type f : float64 = unit ff#
and 'a ff = float
[%%expect{|
type f = unit ff#
and 'a ff = float
|}]

(* If a type with an unboxed version is shadowed by another, [#]
   also points to the new type. *)
type float = int32
type t : bits32 = float#
[%%expect{|
type float = int32
type t = float#
|}]

(* And if the type is shadowed by a type without an unboxed version,
   it's an error to reference the unboxed version. *)
type float
type bad = float#
[%%expect{|
type float
Line 2, characters 11-17:
2 | type bad = float#
               ^^^^^^
Error: "float" has no unboxed version.
|}]

type bad = non_existent#
[%%expect{|
Line 1, characters 11-24:
1 | type bad = non_existent#
               ^^^^^^^^^^^^^
Error: Unbound type constructor "non_existent"
|}]

(* Restore float for future tests *)
type float = unit ff
[%%expect{|
type float = unit ff
|}]

(*******************)
(* Type parameters *)

type 'a t = float
and itu = int t#
and stu = string t#
and it = int t
and st = string t
[%%expect{|
type 'a t = float
and itu = int t#
and stu = string t#
and it = int t
and st = string t
|}]


let id : itu -> float# = fun x -> x
[%%expect{|
val id : itu -> float# = <fun>
|}]

let id : itu -> stu = fun x -> x
[%%expect{|
val id : itu -> stu = <fun>
|}]

let id : it# -> st# = fun x -> x
[%%expect{|
val id : it# -> st# = <fun>
|}]

let id : itu -> it# = fun x -> x
[%%expect{|
val id : itu -> it# = <fun>
|}]

(* CR layouts v7.2: "float/2#" makes sense with the view of [#] as an operator,
   but we may aesthetically prefer "float#/2". *)
let bad : itu -> int32# = fun x -> x
[%%expect{|
Line 1, characters 35-36:
1 | let bad : itu -> int32# = fun x -> x
                                       ^
Error: This expression has type "itu" = "float/2#"
       but an expression was expected of type "int32#"
       Line 1, characters 0-20:
         Definition of type "float/1"
       File "_none_", line 1:
         Definition of type "float/2"
|}]

type bad = (int, int) t#
and 'a t = float
[%%expect{|
Line 1, characters 11-24:
1 | type bad = (int, int) t#
               ^^^^^^^^^^^^^
Error: The type constructor "t#" expects 1 argument(s),
       but is here applied to 2 argument(s)
|}]

type bad = t#
and 'a t = float
[%%expect{|
Line 1, characters 11-13:
1 | type bad = t#
               ^^
Error: The type constructor "t#" expects 1 argument(s),
       but is here applied to 0 argument(s)
|}]

(*******************************************)
(* Type recursion through unboxed versions *)

type t = int
and bad = t#
[%%expect{|
Line 2, characters 0-12:
2 | and bad = t#
    ^^^^^^^^^^^^
Error: "t" has no unboxed version.
|}]

type t = int
and bad = t# * t#
[%%expect{|
Line 2, characters 0-17:
2 | and bad = t# * t#
    ^^^^^^^^^^^^^^^^^
Error: "t" has no unboxed version.
|}]

type bad_a = X of bad_b#
and bad_b = Y of bad_a#
[%%expect{|
Line 1, characters 0-24:
1 | type bad_a = X of bad_b#
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: "bad_b" has no unboxed version.
|}]

type a = b
and b = int
type bad = a#
[%%expect{|
type a = b
and b = int
Line 3, characters 11-13:
3 | type bad = a#
               ^^
Error: "a" has no unboxed version.
|}]

(* Recursive modules *)

module rec M : sig
  type t = M2.t#
end = struct
  type t = M2.t#
end
and M2 : sig
  type t = float
end = struct
  type t = float
end
[%%expect{|
module rec M : sig type t = M2.t# end
and M2 : sig type t = float end
|}]

module rec Bad1 : sig
  type t = Bad2.t#
end = struct
  type t = Bad2.t#
end
and Bad2 : sig
  type t = int
end = struct
  type t = int
end
[%%expect{|
Line 2, characters 11-18:
2 |   type t = Bad2.t#
               ^^^^^^^
Error: "Bad2.t" has no unboxed version.
|}]

module rec Bad1 : sig
  type t = Bad2.t#
end = struct
  type t = Bad2.t#
end
and Bad2 : sig
  type t = Bad1.t#
end = struct
  type t = Bad1.t#
end
[%%expect{|
Line 2, characters 11-18:
2 |   type t = Bad2.t#
               ^^^^^^^
Error: "Bad2.t" has no unboxed version.
|}]

(********************)
(* Module inclusion *)

module M = struct
  type i32 = int32
end
[%%expect{|
module M : sig type i32 = int32 end
|}]

include M
type t : bits32 = i32#
[%%expect{|
type i32 = int32
type t = i32#
|}]

module Bad = struct
  module M = struct
    type int32
  end
  include M

  type bad = int32#
end
[%%expect{|
Line 7, characters 13-19:
7 |   type bad = int32#
                 ^^^^^^
Error: "int32" has no unboxed version.
|}]


(*************************)
(* With type constraints *)

module type S = sig
  type u = t
  and t

  module N : sig
    type t = u
  end
end with type t = float
[%%expect{|
module type S =
  sig type u = t and t = float module N : sig type t = u end end
|}]

module F(M : S) = struct
  let id : float# -> M.u# = fun x -> x
  let id : float# -> M.N.t# = fun x -> x
end
[%%expect{|
module F : functor (M : S) -> sig val id : float# -> M.N.t# end
|}]

(*******************************)
(* With type subst constraints *)

(* Substituting in an unboxed version *)
module S : sig
  type t = float
  and t2 = t
  type u = t#

  val f : t# -> t2#
  val g : u -> t#
  val h : t# -> float#
end with type u := float# = struct
  type t = float
  and t2 = float
  let f x = x
  let g x = x
  let h x = x
end
[%%expect{|
module S :
  sig
    type t = float
    and t2 = t
    val f : t# -> t2#
    val g : float# -> t#
    val h : t# -> float#
  end
|}]

(* Can't substitute abstract type with wrong layout *)
module type Bad = sig
  type t
end with type t := float#
[%%expect{|
Line 3, characters 9-25:
3 | end with type t := float#
             ^^^^^^^^^^^^^^^^
Error: The layout of type "float#" is float64
         because it is the unboxed version of the primitive type float.
       But the layout of type "float#" must be a sublayout of value
         because of the definition of t at line 2, characters 2-8.
|}]

(* Can't substitute a type for its unboxed version *)
module type Bad = sig
  type t = int32#
end with type t := float#
[%%expect{|
Lines 1-3, characters 18-25:
1 | ..................sig
2 |   type t = int32#
3 | end with type t := float#
Error: In this "with" constraint, the new definition of "t"
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t = float#
       is not included in
         type t = int32#
       The type "float/1#" = "float/2#" is not equal to the type "int32#"
       Line 1, characters 0-20:
         Definition of type "float/1"
       File "_none_", line 1:
         Definition of type "float/2"
|}]

(* Can't substitute an unboxed version for a different unboxed version *)
module type Bad = sig
  type t = float#
end with type t := int#
[%%expect{|
Line 3, characters 19-23:
3 | end with type t := int#
                       ^^^^
Error: "int" has no unboxed version.
|}]

(****************************)
(* Module type substitution *)

(* With type constraint *)

(* Unboxed number *)
module type S = sig
  module type x
  module M:x
end
with module type x = sig type t = float end
module F (M : S) = struct
  let id : M.M.t# -> float# = fun x -> x
end
[%%expect{|
module type S = sig module type x = sig type t = float end module M : x end
module F : functor (M : S) -> sig val id : M.M.t# -> float# end
|}]

(* No unboxed version *)
module type S = sig
  module type x
  module M:x
end
with module type x = sig type t = int end
module Bad (M : S) = struct
  let id : M.M.t# -> r# = fun x -> x
end
[%%expect{|
module type S = sig module type x = sig type t = int end module M : x end
Line 7, characters 11-17:
7 |   let id : M.M.t# -> r# = fun x -> x
               ^^^^^^
Error: "M.M.t" has no unboxed version.
|}]

(* Destructive substition *)

(* Unboxed number *)
module type S = sig
  module type x
  module M:x
end
with module type x := sig type t = float end
module F (M : S) = struct
  let id : M.M.t# -> float# = fun x -> x
  type u : float64 = M.M.t#
end
[%%expect{|
module type S = sig module M : sig type t = float end end
module F :
  functor (M : S) -> sig val id : M.M.t# -> float# type u = M.M.t# end
|}]

(* No unboxed version *)
module type S = sig
  module type x
  module M:x
end
with module type x := sig type t = int end
module Bad (M : S) = struct
  type u = M.M.t#
end
[%%expect{|
module type S = sig module M : sig type t = int end end
Line 7, characters 11-17:
7 |   type u = M.M.t#
               ^^^^^^
Error: "M.M.t" has no unboxed version.
|}]

(***********************)
(* Package constraints *)

(* Unboxed number *)
module type S = sig type t end
type m = (module S with type t = float)
module F (X : sig val x : m end) = struct
  module M = (val X.x)
  let id : M.t# -> float# = fun x -> x
  type u : float64 = M.t#
end
[%%expect{|
module type S = sig type t end
type m = (module S with type t = float)
module F :
  functor (X : sig val x : m end) ->
    sig
      module M : sig type t = float end
      val id : M.t# -> float#
      type u = M.t#
    end
|}]

(* No unboxed version *)
module type S = sig type t end
type m = (module S with type t = int)
module Bad (X : sig val x : m end) = struct
  module M = (val X.x)
  type u = M.t#
end
[%%expect{|
module type S = sig type t end
type m = (module S with type t = int)
Line 5, characters 11-15:
5 |   type u = M.t#
               ^^^^
Error: "M.t" has no unboxed version.
|}]
