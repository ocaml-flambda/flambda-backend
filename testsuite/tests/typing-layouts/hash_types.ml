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
Error: The type "Float.t" has no unboxed version.
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
Error: The type "float" has no unboxed version.
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
Error: The type "float" has no unboxed version.
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

(***********************************)
(* Implicit unboxed records basics *)

(* Boxed, including mixed-block, records get implicit unboxed records *)
type r = { i : int ; s : string }
type u : immediate & value = r#
[%%expect{|
type r = { i : int; s : string; }
type u = r#
|}]
type r = { s : string ; f : float# }
type u = r#
[%%expect{|
type r = { s : string; f : float#; }
type u = r#
|}]

(* But not float or [@@unboxed] records *)
type r = { f : float ; f2 : float }
type bad = r#
[%%expect{|
type r = { f : float; f2 : float; }
Line 2, characters 11-13:
2 | type bad = r#
               ^^
Error: The type "r" has no unboxed version.
|}]
type r = { i : int } [@@unboxed]
type bad = r#
[%%expect{|
type r = { i : int; } [@@unboxed]
Line 2, characters 11-13:
2 | type bad = r#
               ^^
Error: The type "r" has no unboxed version.
|}]
type ('a : float64) t = { i : 'a ; j : 'a }
type floatu_t : float64 & float64 = float t#
[%%expect{|
type ('a : float64) t = { i : 'a; j : 'a; }
Line 2, characters 42-44:
2 | type floatu_t : float64 & float64 = float t#
                                              ^^
Error: The type "t" has no unboxed version.
|}]

(* A type can get an unboxed version from both the manifest and kind *)
type r = { i : int ; s : string }
type r2 = r = { i : int ; s : string }
let id : r# -> r2# = fun x -> x
[%%expect{|
type r = { i : int; s : string; }
type r2 = r = { i : int; s : string; }
val id : r# -> r2# = <fun>
|}]

(* The alias above was necessary for [id] to typecheck: *)
type r = { i : int ; s : string }
type r2 = { i : int ; s : string }
let bad_id : r# -> r2# = fun x -> x
[%%expect{|
type r = { i : int; s : string; }
type r2 = { i : int; s : string; }
Line 3, characters 34-35:
3 | let bad_id : r# -> r2# = fun x -> x
                                      ^
Error: This expression has type "r#" but an expression was expected of type "r2#"
|}]

(* Mutable fields imply modalities *)
type r = { i : int ; mutable s : string }
type u = r# = #{ i : int ; s : string @@ global many aliased unyielding }
[%%expect{|
type r = { i : int; mutable s : string; }
type u = r# = #{ i : int; s : string @@ global many aliased; }
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
Error: The type "t" has no unboxed version.
|}]

type t = int
and bad = t# * t#
[%%expect{|
Line 2, characters 0-17:
2 | and bad = t# * t#
    ^^^^^^^^^^^^^^^^^
Error: The type "t" has no unboxed version.
|}]

type bad_a = X of bad_b#
and bad_b = Y of bad_a#
[%%expect{|
Line 1, characters 0-24:
1 | type bad_a = X of bad_b#
    ^^^^^^^^^^^^^^^^^^^^^^^^
Error: The type "bad_b" has no unboxed version.
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
Error: The type "a" has no unboxed version.
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
Error: The type "Bad2.t" has no unboxed version.
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
Error: The type "Bad2.t" has no unboxed version.
|}]

(* Implicit unboxed records *)

type ('a : value & float64) t
[%%expect{|
type ('a : value & float64) t
|}]

type s = r#
and r = {x:int; y:float}
[%%expect{|
type s = r#
and r = { x : int; y : float; }
|}]


type s = q
and r = {x:int; y:float}
and q = r#
[%%expect{|
type s = q
and r = { x : int; y : float; }
and q = r#
|}]

type u = r# t
and r = {x:int; y:float#}
[%%expect{|
type u = r# t
and r = { x : int; y : float#; }
|}]
type s = u t
and r = {x:int; y:float#}
and u = r#
[%%expect{|
type s = u t
and r = { x : int; y : float#; }
and u = r#
|}]

(* CR layouts v7.2: improve this error message *)
type s_bad = r# t
and r = {x:int; y:bool}
[%%expect{|
Line 2, characters 0-23:
2 | and r = {x:int; y:bool}
    ^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The kind of r# is value_or_null & float64
         because it is an unboxed record.
       But the kind of r# must be a subkind of value & float64
         because of the definition of t at line 1, characters 0-29.
|}]

type s_bad = q t
and r = {x:int; y:bool}
and q = r#
[%%expect{|
Line 3, characters 0-10:
3 | and q = r#
    ^^^^^^^^^^
Error:
       The kind of q is value_or_null & float64
         because it is an unboxed record.
       But the kind of q must be a subkind of value & float64
         because of the definition of t at line 1, characters 0-29.
|}]

module rec M : sig
  type t = M2.t
end = struct
  type t = M2.t
end
and M2 : sig
  type t = { f : float }
end = struct
  type t = { f : float }
end
[%%expect{|
module rec M : sig type t = M2.t end
and M2 : sig type t = { f : float; } end
|}]

type bad = M.t#
[%%expect{|
Line 1, characters 11-15:
1 | type bad = M.t#
               ^^^^
Error: The type "M.t" has no unboxed version.
|}]

type t = s and s = t = { x : int }
[%%expect{|
Line 1, characters 0-10:
1 | type t = s and s = t = { x : int }
    ^^^^^^^^^^
Error: The type abbreviation "t" is cyclic:
         "t" = "s",
         "s" = "t"
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
Error: The type "int32" has no unboxed version.
|}]

(*************************)
(* Module type inclusion *)

module type S = sig
  type t = { foo : int }
end
[%%expect{|
module type S = sig type t = { foo : int; } end
|}]

(* Shadowing a boxed record type also shadows the ghost unboxed record type *)
module type Shadowed = sig
  include S
  type t
end
[%%expect {|
module type Shadowed = sig type t end
|}]

(* ...so we can't reference the derived type... *)
module M(Shadowed : Shadowed) = struct
  type t = Shadowed.t#
end
[%%expect{|
Line 2, characters 11-22:
2 |   type t = Shadowed.t#
               ^^^^^^^^^^^
Error: The type "Shadowed.t" has no unboxed version.
|}]

(* ...or its labels. *)
module M(Shadowed : Shadowed) = struct
  let r = #{ foo = 1 }
end
[%%expect{|
Line 2, characters 13-16:
2 |   let r = #{ foo = 1 }
                 ^^^
Error: Unbound unboxed record field "foo"
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

(******************************************************)
(* With type constraints for implicit unboxed records *)

module type S = sig
  type t = { i : int }
  val dummy : int
  val f : t# -> t#
end

type t' = { i : int }
[%%expect{|
module type S =
  sig type t = { i : int; } val dummy : int val f : t# -> t# end
type t' = { i : int; }
|}]

module type S' = S with type t = t'
[%%expect{|
module type S' =
  sig type t = t' = { i : int; } val dummy : int val f : t# -> t# end
|}]

type t = { i : int ; j : int }


(* Check that the constraint was added (we can't check this by printing because
   t# is hidden) *)
module Check_constraints
  (M: sig
        type t = { i : int ; j : int }
      end with type t = t) =
struct
  let f : t -> M.t = fun x -> x
  let f : t# -> M.t# = fun x -> x
end
[%%expect{|
type t = { i : int; j : int; }
module Check_constraints :
  functor (M : sig type t = t = { i : int; j : int; } end) ->
    sig val f : t# -> M.t# end
|}]

module type S = sig
  type t = float
  type u = t#
end with type t := float
[%%expect{|
module type S = sig type u = float# end
|}]

(* Make sure that [with type] doesn't remove an existing kind *)
type orig = { i : int }
module Config : sig
  type t = { i : int }
end with type t = orig = struct
  type t = orig = { i : int }
end
type t = Config.t = { i : int }
[%%expect{|
type orig = { i : int; }
module Config : sig type t = orig = { i : int; } end
type t = Config.t = { i : int; }
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
Error: The type "int" has no unboxed version.
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
Error: In this instantiated signature: The type "t" has no unboxed version.
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

(* Regression test: make sure substs can compose *)
module type S = sig
  type f := int -> int
  type x := f
end
[%%expect{|
module type S = sig end
|}]

(* Implicit unboxed record #-type *)

type t = { i : int ; j : string }

module type S = sig
  type t_to_replace = { i : int ; j : string }

  type r = t_to_replace
  type ru = t_to_replace#
end with type t_to_replace := t
[%%expect{|
type t = { i : int; j : string; }
module type S = sig type r = t type ru = t# end
|}]

module CheckSubsted(M : S) = struct
  let f : t -> M.r = fun x -> x
  let f : t# -> M.ru = fun x -> x
end
[%%expect{|
module CheckSubsted : functor (M : S) -> sig val f : t# -> M.ru end
|}]

module Bad(M : S) = struct
  type bad = M.t_to_replace#
end
[%%expect{|
Line 2, characters 13-28:
2 |   type bad = M.t_to_replace#
                 ^^^^^^^^^^^^^^^
Error: Unbound type constructor "M.t_to_replace"
|}]

(* Make sure the variance is recomputed for unboxed versions for with
   constraints. (Regression test for an early version of linked boxed-unboxed
   versions of types) *)
module M = struct
  type 'a t = private { f : 'a -> int }
end

type 'a t = 'a M.t = private { f : 'a -> int }
module type S = module type of M with type 'a t := 'a t
[%%expect{|
module M : sig type 'a t = private { f : 'a -> int; } end
type 'a t = 'a M.t = private { f : 'a -> int; }
module type S = sig end
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

(* Unboxed record *)
type r = { i : int ; j : int }
module type S = sig
  module type x
  module M:x
end
with module type x := sig type t = r end
module F (M : S) = struct
  let id : M.M.t# -> r# = fun x -> x
  type u : immediate & immediate = M.M.t#
end
[%%expect{|
type r = { i : int; j : int; }
module type S = sig module M : sig type t = r end end
module F : functor (M : S) -> sig val id : M.M.t# -> r# type u = M.M.t# end
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
Error: The type "M.M.t" has no unboxed version.
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
Error: The type "M.M.t" has no unboxed version.
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

(* Unboxed record *)
module type S = sig type t end
type r = { i : int; j : int }
type m = (module S with type t = r)
module F (X : sig val x : m end) = struct
  module M = (val X.x)
  let id : M.t# -> r# = fun x -> x
  type u : immediate & immediate = M.t#
end
[%%expect{|
module type S = sig type t end
type r = { i : int; j : int; }
type m = (module S with type t = r)
module F :
  functor (X : sig val x : m end) ->
    sig module M : sig type t = r end val id : M.t# -> r# type u = M.t# end
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
Error: The type "M.t" has no unboxed version.
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
Error: In this instantiated signature: The type "id" has no unboxed version.
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
       The type "FloatId.t" has no unboxed version.
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
Error: In this instantiated signature:
       The type "FloatId.t" has no unboxed version.
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
Error: In this instantiated signature: The type "M.t" has no unboxed version.
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
Error: In this instantiated signature: The type "id" has no unboxed version.
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
Error: In this instantiated signature: The type "id" has no unboxed version.
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
Error: In this instantiated signature: The type "id" has no unboxed version.
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
Error: In this instantiated signature: The type "id" has no unboxed version.
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
       The type "N.t" has no unboxed version.
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
       The type "s" has no unboxed version.
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
       The type "s" has no unboxed version.
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

(*********************************************)
(* Separability approximation for hash types *)

(* We are conservative when computing the separability of aliases of unboxed
   versions. This should be fixed when unboxed versions of types are represented
   as an operator on types.

   This isn't actually too conservative, though: inaccuracy requires a
   singleton record containing one of multiple type parameters. We only
   erroneously reject the last example here.
*)

type 'a t = int32
type packed = P : 'a t# -> packed [@@unboxed]
[%%expect{|
type 'a t = int32
type packed = P : 'a t# -> packed [@@unboxed]
|}]

type ('a, 'b) t = { a : 'a; b : 'b }
type ('a, 'b) s = ('a, 'b) t
type packed = T : ('a, 'b) s# -> packed [@@unboxed]
[%%expect{|
type ('a, 'b) t = { a : 'a; b : 'b; }
type ('a, 'b) s = ('a, 'b) t
type packed = T : ('a, 'b) s# -> packed [@@unboxed]
|}]

(* This one is rejected, but it should be *)
type 'a t = { i : 'a }
and bad = P : 'a t# -> bad [@@unboxed]
[%%expect{|
Line 2, characters 0-38:
2 | and bad = P : 'a t# -> bad [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'a".
       You should annotate it with "[@@ocaml.boxed]".
|}]

(* CR layouts 7.2: accept the following program. *)
type ('a, 'b) t = { a : 'a }
type ('a, 'b) s = ('a, 'b) t
type packed = T : (int, 'b) s# -> packed [@@unboxed]
[%%expect{|
type ('a, 'b) t = { a : 'a; }
type ('a, 'b) s = ('a, 'b) t
Line 3, characters 0-52:
3 | type packed = T : (int, 'b) s# -> packed [@@unboxed]
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This type cannot be unboxed because
       it might contain both float and non-float values,
       depending on the instantiation of the existential variable "'b".
       You should annotate it with "[@@ocaml.boxed]".
|}]
