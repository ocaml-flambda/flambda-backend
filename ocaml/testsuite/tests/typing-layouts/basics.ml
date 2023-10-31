(* TEST
   * expect
   flags = "-extension layouts"
*)
type t_value : value
type t_imm   : immediate
type t_imm64 : immediate64;;

[%%expect{|
type t_value : value
type t_imm : immediate
type t_imm64 : immediate64
|}]

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


(************************************************************)
(* Test 1: Disallow non-representable function args/returns *)

(* CR layouts v3: moved to layouts alpha.  Bring here when we have
   non-representable layouts enabled by default. *)

(*****************************************************)
(* Test 2: Permit representable function arg/returns *)

(* CR layouts v3: much of this test moved to basics_alpha.  Add #float versions
   and bring them here when #float is allowed by default. *)
module type S = sig
  val f1 : t_value -> t_value
  val f2 : t_imm -> t_imm64
end;;

[%%expect{|

module type S = sig val f1 : t_value -> t_value val f2 : t_imm -> t_imm64 end
|}];;

(**************************************)
(* Test 3: basic annotated parameters *)

(* CR layouts: mostly moved to [basics_beta.ml].  Bring back here when we allow
   annotations on parameters by default. *)
type ('a : immediate) imm_id = 'a;;
[%%expect{|

Line 1, characters 11-20:
1 | type ('a : immediate) imm_id = 'a;;
               ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}];;

(************************************)
(* Test 4: parameters and recursion *)

(* CR layouts: immediate bits moved to [basics_beta.ml] and void bits to
   [basics_alpha.ml].  Bring each part back here when we allow the relevant
   layout annotations on parameters by default. *)

(************************************************************)
(* Test 5: You can touch a void, but not return it directly *)

(* CR layouts v5: these tests moved to [basics_alpha.ml].  Bring them back here
   when we allow void by default.  Also the tests will change because we'll
   allow returning void. *)

(****************************************)
(* Test 6: explicitly polymorphic types *)

(* CR layouts: These tests can come back from [layouts_beta.ml] when we allow parameter
   jkind annotations by default. *)

(*****************************************)
(* Test 7: the layout check in unify_var *)

(* CR layouts: This test can come back from [layouts_beta.ml] when we allow
   parameter layout annotations by default. *)

(**********************************************************)
(* Test 8: Polymorphic variants take value args (for now) *)

(* CR layouts: these tests moved to [basics_alphs.ml] because they need a
   non-value layout.  Similar tests should be added here once we have another
   sort enabled by default (though we will probably chose to allow it as an arg
   to polymorphic variants, not ban it). *)

(************************************************)
(* Test 9: Tuples only work on values (for now) *)

(* CR layouts: these tests moved to [basics_alphs.ml] because they need a
   non-value layout.  Similar tests should be added here once we have another
   sort enabled by default. *)

(*************************************************)
(* Test 10: jkinds are checked by "more general" *)

(* CR layouts: These tests moved to [basics_beta.ml] because they use annotated
   type parameters.  Bring them back here when we allow this by default. *)

module M10_1 : sig
  val x : string
end = struct
  type ('a : immediate) t = 'a

  let f : 'a t -> 'a = fun x -> x

  let x = f (assert false)
end;;
[%%expect{|

Line 4, characters 13-22:
4 |   type ('a : immediate) t = 'a
                 ^^^^^^^^^
Error: Layout immediate is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_beta to use this feature.
|}]

(**************************************************************)
(* Test 11: objects are values and methods take/return values *)

(* CR layouts: These tests moved to [basics_alpha.ml] as they need a non-value
   sort.  Bring back here when we have one enabled by default. *)
module M11_1 = struct
  type ('a : void) t = { x : int; v : 'a }

  let f t =
    t.v # baz11
end;;
[%%expect{|

Line 2, characters 13-17:
2 |   type ('a : void) t = { x : int; v : 'a }
                 ^^^^
Error: Layout void is more experimental than allowed by -extension layouts.
       You must enable -extension layouts_alpha to use this feature.
|}]

(*******************************************************************)
(* Test 12: class parameters and bound vars must have layout value *)

(* CR layouts: These tests moved to [basics_alpha.ml] as they need a non-value
   sort.  Bring back here when we have one enabled by default. *)

(***********************************************************)
(* Test 13: built-in type constructors work only on values *)

(* CR layouts: These tests moved to [basics_alpha.ml] as they need a non-value
   sort.  Bring back here when we have one enabled by default. *)

(****************************************************************************)
(* Test 14: Examples motivating the trick with the manifest in [enter_type] *)
type t14 = foo14 list
and foo14 = string;;
[%%expect{|

type t14 = foo14 list
and foo14 = string
|}];;

(* CR layouts: Part of this test moved to [basics_alpha.ml] as it needs a
   non-value sort.  Bring back here when we have one enabled by default. *)

(****************************************************)
(* Test 15: Type aliases need not have layout value *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(********************************************************)
(* Test 16: seperability: [msig_of_external_type] logic *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

type 'a t_void_16 : void;;
[%%expect{|

Line 1, characters 20-24:
1 | type 'a t_void_16 : void;;
                        ^^^^
Error: Layout void is used here, but the appropriate layouts extension is not enabled
|}];;

(**************************************************************************)
(* Test 17: incremental layout checking of @@unboxed types - see comment on
   [constrain_type_layout]. *)

type 'a t17 = 'a list
type s17 = { lbl : s17 t17 } [@@unboxed];;

[%%expect{|

type 'a t17 = 'a list

type s17 = { lbl : s17 t17; } [@@unboxed]
|}];;

(*****************************************)
(* Test 18: expansion in [check_univars] *)
(* This test isn't really layouts-specific, but it checks that the layout checks
   we've added in [Typecore.check_univars] don't choke when expansion is needed
   to see a variable *)
type 'a t18 = 'a

let id18 (x : 'a t18) = x

let f18 : 'a . 'a -> 'a = fun x -> id18 x;;

[%%expect{|

type 'a t18 = 'a

val id18 : 'a t18 -> 'a t18 = <fun>

val f18 : 'a -> 'a = <fun>
|}];;

(********************************)
(* Test 19: non-value coercions *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(********************************************)
(* Test 20: Non-value bodies for let module *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(**********************************)
(* Test 21: Non-value unpack body *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(***************************************************************)
(* Test 22: approx_type catch-all can't be restricted to value *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

type t_void : void;;
[%%expect{|

Line 1, characters 14-18:
1 | type t_void : void;;
                  ^^^^
Error: Layout void is used here, but the appropriate layouts extension is not enabled
|}];;

(* CR layouts v5: Once we allow non-value top-level module definitions, add
   tests showing that things get defaulted to value.
*)

(********************************************************************)
(* Test 23: checking the error message from impossible GADT matches *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(*****************************************************)
(* Test 24: Polymorphic parameter with exotic layout *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(**************************************************)
(* Test 25: Optional parameter with exotic layout *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(*********************************************************)
(* Test 26: Inferring an application to an exotic layout *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(******************************************)
(* Test 27: Exotic layouts in approx_type *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(************************************)
(* Test 28: Exotic layouts in letop *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(*******************************************)
(* Test 29: [external]s default to [value] *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(**************************************)
(* Test 30: [val]s default to [value] *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(**************************************************)
(* Test 31: checking that #poly_var patterns work *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(*********************************************************)
(* Test 32: Polymorphic variant constructors take values *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one enabled by default. *)

(******************************************************)
(* Test 33: Externals must have representable types *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a
   non-representable layout.  Bring it back here when we can mention [t_any] in
   [-extension layouts]. *)

(****************************************************)
(* Test 34: Layout clash in polymorphic record type *)

(* CR layouts: This test moved to [basics_beta.ml] as it needs an immediate
   type parameter.  Bring back here when we have one enabled by default. *)

(****************************************************)
(* Test 35: check bad layout error in filter_arrow *)

(* CR layouts: This test moved to [basics_beta.ml] as it needs an immediate
   type parameter.  Bring back here when we have one enabled by default. *)

(**************************************************)
(* Test 36: Disallow non-representable statements *)

(* CR layouts: This test moved to [basics_beta.ml]. Bring here when we have
   non-representable layouts enabled by default. *)
