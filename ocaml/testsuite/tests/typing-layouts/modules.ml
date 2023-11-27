(* TEST
   * expect
   flags = "-extension layouts"
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

(* CR layouts: parts of this test moved to [modules_alpha.ml] because they
   need a non-value layout.  Bring back here when we have one enabled by
   default. *)
module type S1 = sig
  type ('a : void) t
  type s
end;;
[%%expect {|
Line 2, characters 13-17:
2 |   type ('a : void) t
                 ^^^^
Error: Layout void is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_alpha to use this feature.
|}];;

(* CR layouts: parts of this test moved to [modules_beta.ml] because they need
   immediate annotations on type parameters.  Bring back here when we turn that
   on by default. *)
module type S1_2 = sig
  type ('a : immediate) t
end;;
[%%expect {|
Line 2, characters 13-22:
2 |   type ('a : immediate) t
                 ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}];;

(************************************************************************)
(* Test 2: with type constraints for fixed types (the complicated case of
   Type_mod.merge_constraint) *)

(* CR layouts: this test moved to [modules_beta.ml] because it needs immediate
   annotations on type parameters.  Bring back here when we turn that on by
   default. *)
module type S2 = sig
  type ('a : immediate) t
end;;
[%%expect{|
Line 2, characters 13-22:
2 |   type ('a : immediate) t
                 ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}];;

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

(* CR layouts: parts of this test moved to [modules_alpha.ml] because they
   need a non-value layout.  Bring back here when we have one enabled by
   default. *)
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

(* CR layouts: more bits moved to [modules_alpha.ml] from down here. *)

(*************************************************************************)
(* Test 4: Nondep typedecl layout approximation in the Nondep_cannot_erase
   case. *)

(* CR layouts: This test moved to [modules_beta.ml] and [modules_alpha.ml].
   Parts of it can come back when we have the ability to annotate type parameter
   layouts without extension flags, and other parts need a non-value layout. *)

(************************************)
(* Test 5: Destructive substitution *)

(* CR layouts: The first part of this test has been moved to [modules_beta.ml].
   It can come back when we have the ability to annotate layout parameters
   without extensions. *)

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

(* CR layouts: The first part of this test needs a non-value layout and has
   been moved to modules_alpha.ml.  Bring it back once we have a non-value
   layout enabled by default. *)
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

(* CR layouts: this is broken because of the package with-type hack.  It was
   already broken before layouts, but it would be nice to fix.  See the comment
   on See the comments in the [Ptyp_package] case of
   [Typetexp.transl_type_aux]. *)
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
