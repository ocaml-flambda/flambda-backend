(* TEST
   flags = "-extension layouts_beta"
   * expect
*)

type t_value : value
type t_imm   : immediate
type t_imm64 : immediate64;;
[%%expect {|
type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
|}];;

type t_any   : any;;
[%%expect{|
Line 1, characters 15-18:
1 | type t_any   : any;;
                   ^^^
Error: Layout any is used here, but the appropriate layouts extension is not enabled
|}];;

type t_void  : void;;
[%%expect{|
Line 1, characters 15-19:
1 | type t_void  : void;;
                   ^^^^
Error: Layout void is used here, but the appropriate layouts extension is not enabled
|}];;

(*********************************************************)
(* Test 1: Simple with type constraints respect layouts. *)

(* CR layouts v2.5: parts of this test moved to [modules_alpha.ml] because they
   need a non-value layout.  Bring back here when we have one. *)
module type S1 = sig
  type ('a : void) t
  type s
end;;
[%%expect {|
Line 2, characters 13-17:
2 |   type ('a : void) t
                 ^^^^
Error: Layout void is more experimental than allowed by -extension layouts_beta.
       You must enable -extension layouts_alpha to use this feature.
|}];;

module type S1_2 = sig
  type ('a : immediate) t
end

module type S1_2' = S1_2 with type 'a t = 'a list

module M1_2' : S1_2' = struct
  type ('a : immediate) t = 'a list
end;;
[%%expect{|
module type S1_2 = sig type ('a : immediate) t end
module type S1_2' = sig type ('a : immediate) t = 'a list end
module M1_2' : S1_2'
|}]

(* CR layouts - annoyingly, the immediate annotation on 'a is required.  We
   can probably relax this so you don't have to label the parameter explcitly
   and the layout is determined from the signature.  But we anticipate it'll
   require non-trivial refactoring of eqtype, so we've put it off for now. *)
module M1_2'': S1_2' = struct
  type 'a t = 'a list
end;;
[%%expect{|
Lines 1-3, characters 23-3:
1 | .......................struct
2 |   type 'a t = 'a list
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a list end
       is not included in
         S1_2'
       Type declarations do not match:
         type 'a t = 'a list
       is not included in
         type ('a : immediate) t = 'a list
       The type ('a : value) is not equal to the type ('a0 : immediate)
       because their layouts are different.
|}]

module M1_2''' : S1_2 = struct
  type 'a t = 'a list
end;;
[%%expect{|
Lines 1-3, characters 24-3:
1 | ........................struct
2 |   type 'a t = 'a list
3 | end..
Error: Signature mismatch:
       Modules do not match:
         sig type 'a t = 'a list end
       is not included in
         S1_2
       Type declarations do not match:
         type 'a t = 'a list
       is not included in
         type ('a : immediate) t
       Their parameters differ:
       The type ('a : value) is not equal to the type ('a0 : immediate)
       because their layouts are different.
|}]

(************************************************************************)
(* Test 2: with type constraints for fixed types (the complicated case of
   Type_mod.merge_constraint) *)
module type S2 = sig
  type ('a : immediate) t
end

type ('a : immediate) r2 = R
type ('a : immediate) s2 = private [> `A of 'a r2]

module type T2 = S2 with type 'a t = 'a s2

module F2 (X : T2) = struct
  let f () : 'a X.t = `A R
end;;
[%%expect{|
module type S2 = sig type ('a : immediate) t end
type ('a : immediate) r2 = R
type (!'a : immediate) s2 = private [> `A of 'a r2 ]
module type T2 = sig type ('a : immediate) t = 'a s2 end
module F2 :
  functor (X : T2) -> sig val f : ('a : immediate). unit -> 'a X.t end
|}]

type ('a : immediate) s2' = private [> `B of 'a]
module type T2' = S2 with type 'a t = 'a s2'

module F2' (X : T2') = struct
  let f () : 'a X.t = `B "bad"
end
[%%expect{|
type (!'a : immediate) s2' = private [> `B of 'a ]
module type T2' = sig type ('a : immediate) t = 'a s2' end
Line 5, characters 25-30:
5 |   let f () : 'a X.t = `B "bad"
                             ^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : immediate)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-25.
|}]

(******************************************************************)
(* Test 3: Recursive modules, with and without layout annotations *)
module rec Foo3 : sig
  val create : Bar3.t -> unit
end = struct
  let create _ = ()
end

and Bar3 : sig
  type t
end = struct
  type t = unit
end;;
[%%expect {|
module rec Foo3 : sig val create : Bar3.t -> unit end
and Bar3 : sig type t end
|}];;

(* CR layouts v2.5: parts of this test moved to [modules_alpha.ml] because they
   need a non-value layout.  Bring back here when we have one. *)
module rec Foo3 : sig
  val create : Bar3.t -> unit
end = struct
  let create _ = ()
end

and Bar3 : sig
  type t : void
end = struct
  type t : void
end;;
[%%expect {|
Line 8, characters 11-15:
8 |   type t : void
               ^^^^
Error: Layout void is used here, but the appropriate layouts extension is not enabled
|}];;

module rec Foo3 : sig
  type t : immediate = Bar3.t
end = struct
  type t = Bar3.t
end

and Bar3 : sig
  type t : value
end = struct
  type t = A
end;;
[%%expect {|
Line 2, characters 2-29:
2 |   type t : immediate = Bar3.t
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type Bar3.t is value, because
         of the annotation on the declaration of the type t.
       But the layout of type Bar3.t must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-29.
|}];;

module rec Foo3 : sig
  type t : immediate = Bar3.t
end = struct
  type t = Bar3.t
end

and Bar3 : sig
  type t : immediate
end = struct
  type t = A
end;;
[%%expect {|
module rec Foo3 : sig type t = Bar3.t end
and Bar3 : sig type t : immediate end
|}];;

(* CR layouts v2.5: more bits moved to [modules_alpha.ml] from down here. *)

(*************************************************************************)
(* Test 4: Nondep typedecl layout approximation in the Nondep_cannot_erase
   case. *)

(* CR layouts v2.5: The interesting parts of this test need a non-value layout and
   have been moved to modules_alpha.ml.  Bring back those parts once we have a
   non-value layout.  I've just commented them out below. *)
module F4(X : sig type t end) = struct
  type s = Foo of X.t
end

module M4 = F4(struct type t = T end)

type ('a : value) t4_val
(* type ('a : void) t4_void *)

type t4 = M4.s t4_val;;
[%%expect {|
module F4 : functor (X : sig type t end) -> sig type s = Foo of X.t end
module M4 : sig type s end
type 'a t4_val
type t4 = M4.s t4_val
|}]

(*
type t4' = M4.s t4_void;;
[%%expect {|
Line 1, characters 11-15:
1 | type t4' = M4.s t4_void;;
               ^^^^
Error: This type M4.s should be an instance of type ('a : void)
       M4.s has layout value, which is not a sublayout of void.
|}]
*)
module F4'(X : sig type t : immediate end) = struct
  type s : immediate = Foo of X.t [@@unboxed]
end

module M4' = F4'(struct type t = T end)

type ('a : immediate) t4_imm

type t4 = M4'.s t4_imm;;
[%%expect{|
module F4' :
  functor (X : sig type t : immediate end) ->
    sig type s : immediate = Foo of X.t [@@unboxed] end
module M4' : sig type s : immediate end
type ('a : immediate) t4_imm
type t4 = M4'.s t4_imm
|}];;

(*
type t4 = M4'.s t4_void;;
[%%expect{|
Line 1, characters 10-15:
1 | type t4 = M4'.s t4_void;;
              ^^^^^
Error: This type M4'.s should be an instance of type ('a : void)
       M4'.s has layout immediate, which is not a sublayout of void.
|}];;
*)

(************************************)
(* Test 5: Destructive substitution *)
module type S3_1 = sig
  type ('a : immediate) t
  val f : 'a -> 'a t
end

module type S3_1' = S3_1 with type 'a t := 'a list

module M3_1 : S3_1' = struct
  let f x = [x]
end

let x3 = M3_1.f 42

let x3' = M3_1.f "test";;
[%%expect{|
module type S3_1 =
  sig type ('a : immediate) t val f : ('a : immediate). 'a -> 'a t end
module type S3_1' = sig val f : ('a : immediate). 'a -> 'a list end
module M3_1 : S3_1'
val x3 : int list = [42]
Line 14, characters 17-23:
14 | let x3' = M3_1.f "test";;
                      ^^^^^^
Error: This expression has type string but an expression was expected of type
         ('a : immediate)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of f at line 3, characters 2-20.
|}]

module type S3_2 = sig
  type t : immediate
end

module type S3_2' = S3_2 with type t := string;;
[%%expect{|
module type S3_2 = sig type t : immediate end
Line 5, characters 30-46:
5 | module type S3_2' = S3_2 with type t := string;;
                                  ^^^^^^^^^^^^^^^^
Error: The layout of type string is value, because
         it is the primitive value type string.
       But the layout of type string must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-20.
|}]

(*****************************************)
(* Test 6: With constraints on packages. *)

(* CR layouts v2.5: The first part of this test needs a non-value layout and has
   been moved to modules_alpha.ml.  Bring it back once we have a non-value
   layout. *)
module type S6_1 = sig
  type t : void
end
[%%expect{|
Line 2, characters 11-15:
2 |   type t : void
               ^^^^
Error: Layout void is used here, but the appropriate layouts extension is not enabled
|}]

module type S6_5 = sig
  type t : immediate
end

module type S6_6 = sig
  val m : (module S6_5 with type t = string)
end
[%%expect{|
module type S6_5 = sig type t : immediate end
Line 6, characters 10-44:
6 |   val m : (module S6_5 with type t = string)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t
       is not included in
         type t : immediate
       The layout of the first is value, because
         it's used as an element in a first-class module.
       But the layout of the first must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-20.
|}];;

module type S6_6' = sig
  type s
  val m : (module S6_5 with type t = s)
end
[%%expect{|
Line 3, characters 10-39:
3 |   val m : (module S6_5 with type t = s)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t
       is not included in
         type t : immediate
       The layout of the first is value, because
         it's used as an element in a first-class module.
       But the layout of the first must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-20.
|}];;

(* CR layouts: S6_6'' should be fixed *)
module type S6_6'' = sig
  type s = int
  val m : (module S6_5 with type t = int)
end;;
[%%expect{|
Line 3, characters 10-41:
3 |   val m : (module S6_5 with type t = int)
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: In this `with' constraint, the new definition of t
       does not match its original definition in the constrained signature:
       Type declarations do not match:
         type t
       is not included in
         type t : immediate
       The layout of the first is value, because
         it's used as an element in a first-class module.
       But the layout of the first must be a sublayout of immediate, because
         of the definition of t at line 2, characters 2-20.
|}];;

(*****************************************)
(* Test 7: "any" is allowed in Tstr_eval *)
module F (_ : sig end) = struct
  assert false
end;;
[%%expect {|
module F : sig end -> sig end
|}];;

(****************************************)
(* Test 8: [val]s must be representable *)

(* CR layouts: Bring this test back from modules_alpha *)
