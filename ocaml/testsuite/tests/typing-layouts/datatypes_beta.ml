(* TEST
   flags = "-extension layouts_beta"
   * expect
*)

(* Tests for layouts in algebraic datatypes *)

(* CR layouts v5: add mixed block restriction tests. *)

type t_value : value
type t_immediate : immediate;;
[%%expect {|
type t_value : value
type t_immediate : immediate
|}];;

type t_any : any;;
[%%expect{|
Line 1, characters 13-16:
1 | type t_any : any;;
                 ^^^
Error: Layout any is used here, but the appropriate layouts extension is not enabled
|}];;

type t_void : void;;
[%%expect{|
Line 1, characters 14-18:
1 | type t_void : void;;
                  ^^^^
Error: Layout void is used here, but the appropriate layouts extension is not enabled
|}];;

(***************************************************)
(* Test 1: constructor arguments may have any sort *)

(* CR layouts v2.5: Needs non-value layout - moved to [datatypes_alpha.ml] *)

(************************************)
(* Test 2: but not the "any" layout *)

(* CR layouts: Needs the ability to talk about any - moved to
   [datatypes_alpha.ml] *)

(******************************************************)
(* Test 3: void allowed in records, but not by itself *)

(* CR layouts v5: Needs void - moved to [datatypes_alpha.ml].  Will change by
   the time we add back void anyway. *)

(**************************)
(* Test 4: but any is not *)

(* CR layouts: Needs the ability to talk about any - moved to
   [datatypes_alpha.ml] *)

(*********************************************************)
(* Test 5: These same rules apply to extensible variants *)

(* CR layouts: Needs void and the ability to talk about any - moved to
   [datatypes_alpha.ml] *)

(**************************************************************************)
(* Test 6: fields in all-float records get layout value.  may change in the
   future, but record fields must at least be representable. *)
type t6 = { fld6 : float }
type ('a : immediate) s6 = S6 of 'a

let f6 x =
  let { fld6 = fld6 } = x in fld6

let f6' x =
  let { fld6 = fld6 } = x in S6 fld6;;
[%%expect {|
type t6 = { fld6 : float; }
type ('a : immediate) s6 = S6 of 'a
val f6 : t6 -> float = <fun>
Line 8, characters 32-36:
8 |   let { fld6 = fld6 } = x in S6 fld6;;
                                    ^^^^
Error: This expression has type float but an expression was expected of type
         ('a : immediate)
       The layout of float is value, because
         it is the primitive value type float.
       But the layout of float must be a sublayout of immediate, because
         of the definition of s6 at line 2, characters 0-35.
|}];;

(*****************************************************)
(* Test 7: Recursive propagation of immediacy checks *)

(* CR layouts: copy test from datatypes_alpha with float64 when available *)

(***********************************************************************)
(* Test 8: Type parameters in the presence of recursive concrete usage *)

(* CR layouts: copy test from datatypes_alpha with float64 when available *)
