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

(* Restore float *)
type float = unit ff
[%%expect{|
type float = unit ff
|}]

(* Same as previous test, except the type doing the shadowing has a manifest *)
type float = string
type bad = float#
[%%expect{|
type float = string
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

(* Hash versions are propagated through constraints *)

type 'a t  = 'a constraint 'a = float
type fh = float t#
[%%expect{|
type 'a t = 'a constraint 'a = float
type fh = float t#
|}]

type bad = int t#
[%%expect{|
Line 1, characters 11-14:
1 | type bad = int t#
               ^^^
Error: This type "int" should be an instance of type "float/1" = "float/2"
|}]

type 'a t = float as 'a
type fh = float t#
[%%expect{|
type 'a t = 'a constraint 'a = float
type fh = float t#
|}, Principal{|
type 'a t = float constraint 'a = float
type fh = float t#
|}]

(* Unboxed versions are even propagated through type equations *)
type 'a rep = Float : float rep
let f (type a) (x : a rep) =
    match x with
    | Float ->
      let module M = struct type t = a# end in 0;;
[%%expect{|
type 'a rep = Float : float rep
val f : 'a rep -> int = <fun>
|}]

(*******************)
(* Type parameters *)

type 'a t = float
and itu = int t#
and stu = string t#
and it = int t
and st = string t
and ith = it#
[%%expect{|
type 'a t = float
and itu = int t#
and stu = string t#
and it = int t
and st = string t
and ith = it#
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

(* Can't substitute an unboxed version for a different unboxed version *)
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

(* Can't substitute an unboxed version for a nonexistent unboxed version *)
module type Bad = sig
  type t = float#
end with type t := int#
[%%expect{|
Line 3, characters 19-23:
3 | end with type t := int#
                       ^^^^
Error: "int" has no unboxed version.
|}]

(* Test subst when a decl's type_unboxed_version over-approximately [None]
   (regression test for the initial implementation of [Subst] *)
module type S = sig
  type t = float
  type s = t#
end
type 'a t = 'b constraint 'a = < m : 'b >
module type T = S with type t := < m : float > t
[%%expect{|
module type S = sig type t = float type s = t# end
type 'a t = 'b constraint 'a = < m : 'b >
Line 6, characters 16-48:
6 | module type T = S with type t := < m : float > t
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this instantiated signature: "t" has no unboxed version.
|}]

(* Standalone destructive substitution *)

module type S = sig
  type t := float
  type s = t#
end
[%%expect{|
module type S = sig type s = float# end
|}]

module type S = sig
  type 'a t = float
  type s := int t
  type u = s#
end
[%%expect{|
module type S = sig type 'a t = float type u = int t# end
|}]

module type S = sig
  type t := float
  type s := t#
  type u = s
end
[%%expect{|
module type S = sig type u = float# end
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

(******************************)
(* Signature avoidance checks *)

(* Functor application and destructive substitution allows unboxed versions of
   types to go out of scope, because [float] = [float id] for the purposes of
   substitutions, but only [float] has an unboxed version.

   To prevent this, when we do such a substitution, we make sure that there are
   no uses of [id#].
*)
type 'a id = 'a
module FloatId = struct type t = float id end
module type FloatId_S = sig type t = float id end
[%%expect{|
type 'a id = 'a
module FloatId : sig type t = float id end
module type FloatId_S = sig type t = float id end
|}]

(* Here's the simplest case: we replace a type with an unboxed version with
   one without, but we use that unboxed version in the same signature. *)
module type Bad = sig
  type t = float
  type u = t#
end with type t := float id
[%%expect{|
Lines 1-4, characters 18-27:
1 | ..................sig
2 |   type t = float
3 |   type u = t#
4 | end with type t := float id
Error: In this instantiated signature: "id" has no unboxed version.
|}]

(* We can also remove an unboxed version through functor application *)
module F(M : sig type t = float end) = struct
  type u = M.t#
end
module Bad = F(FloatId)
[%%expect{|
module F : functor (M : sig type t = float end) -> sig type u = M.t# end
Line 4, characters 13-23:
4 | module Bad = F(FloatId)
                 ^^^^^^^^^^
Error: In the signature of this functor application:
       "FloatId.t" has no unboxed version.
|}]

(* ..and module substitution... *)
module type Bad = sig
  module Float : sig
    type t = float
  end
  type u = Float.t#
end with module Float := FloatId
[%%expect{|
Lines 1-6, characters 18-32:
1 | ..................sig
2 |   module Float : sig
3 |     type t = float
4 |   end
5 |   type u = Float.t#
6 | end with module Float := FloatId
Error: In this instantiated signature: "FloatId.t" has no unboxed version.
|}]

(* ..and module type substitution. *)
module type Bad = sig
  module type Float_S = sig
    type t = float
  end
  module F(M : Float_S) : sig
    type u = M.t#
  end
end with module type Float_S := FloatId_S
[%%expect{|
Lines 1-8, characters 18-41:
1 | ..................sig
2 |   module type Float_S = sig
3 |     type t = float
4 |   end
5 |   module F(M : Float_S) : sig
6 |     type u = M.t#
7 |   end
8 | end with module type Float_S := FloatId_S
Error: In this instantiated signature: "M.t" has no unboxed version.
|}]

(* The check for bad unboxed paths looks deeply through manifests *)
module type Bad = sig
  type t = float
  type uu = #(t# * t#)
end with type t := float id
[%%expect{|
Lines 1-4, characters 18-27:
1 | ..................sig
2 |   type t = float
3 |   type uu = #(t# * t#)
4 | end with type t := float id
Error: In this instantiated signature: "id" has no unboxed version.
|}]

(* The check for bad unboxed paths looks deeply through kinds *)
module type Bad = sig
  type t = float
  type uu = #{ uu : #(t# * t#) }
end with type t := float id
[%%expect{|
Lines 1-4, characters 18-27:
1 | ..................sig
2 |   type t = float
3 |   type uu = #{ uu : #(t# * t#) }
4 | end with type t := float id
Error: In this instantiated signature: "id" has no unboxed version.
|}]

(* The check for bad unboxed paths looks through nested modules *)
module type Bad = sig
  module M : sig
    type t = float
  end
  type u = M.t#
end with type M.t := float id
[%%expect{|
Lines 1-6, characters 18-29:
1 | ..................sig
2 |   module M : sig
3 |     type t = float
4 |   end
5 |   type u = M.t#
6 | end with type M.t := float id
Error: In this instantiated signature: "id" has no unboxed version.
|}]

(* Still check when the original unboxed version comes from a constraint *)
module type Bad = sig
  type 'a t = 'a constraint float = 'a
  type u = float t#
end with type 'a t := 'a id
[%%expect{|
Lines 1-4, characters 18-27:
1 | ..................sig
2 |   type 'a t = 'a constraint float = 'a
3 |   type u = float t#
4 | end with type 'a t := 'a id
Error: In this instantiated signature: "id" has no unboxed version.
|}]

(* Nested functor application *)
module F(M : sig type t = float end) = struct
  type u = #(M.t# * M.t#)
end
module G(M : sig type t = float end) = struct
  module N = struct
    type t = M.t
  end
  module O = F(N)
end
module Bad = G(struct type t = float id end)
[%%expect{|
module F :
  functor (M : sig type t = float end) -> sig type u = #(M.t# * M.t#) end
module G :
  functor (M : sig type t = float end) ->
    sig
      module N : sig type t = M.t end
      module O : sig type u = #(N.t# * N.t#) end
    end
Line 10, characters 13-44:
10 | module Bad = G(struct type t = float id end)
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In the signature of this functor application:
       "N.t" has no unboxed version.
|}]

(* Chain of two aliases that lose unboxed versions *)
module F(M : sig type t = float end) = struct
  type s = M.t
  type u = #(s# * s#)
end
module Bad = F(FloatId)
[%%expect{|
module F :
  functor (M : sig type t = float end) ->
    sig type s = M.t type u = #(s# * s#) end
Line 5, characters 13-23:
5 | module Bad = F(FloatId)
                 ^^^^^^^^^^
Error: In the signature of this functor application:
       "s" has no unboxed version.
|}]

(* Mutually recursive aliases that lose unboxed versions *)
module F(M : sig type t = float end) = struct
  type u = #(s# * s#)
  and s = M.t
end
module Bad = F(FloatId)
[%%expect{|
module F :
  functor (M : sig type t = float end) ->
    sig type u = #(s# * s#) and s = M.t end
Line 5, characters 13-23:
5 | module Bad = F(FloatId)
                 ^^^^^^^^^^
Error: In the signature of this functor application:
       "s" has no unboxed version.
|}]

(* Make sure our check isn't too restrictive. We allow a module with a
   destructive substitution and an unrelated usage of an unboxed path. *)
module type Good = sig
  type t = float
  type m = float
  type mu = m#
end with type t := float id
[%%expect{|
module type Good = sig type m = float type mu = m# end
|}]

(* Make sure our check isn't too restrictive. We allow a functor application
   that replaces an unboxed type with one without an unboxed type. *)
module F(M : sig type t = float end) = struct
  type s = M.t
end
module M = F(FloatId)
[%%expect{|
module F : functor (M : sig type t = float end) -> sig type s = M.t end
module M : sig type s = FloatId.t end
|}]
