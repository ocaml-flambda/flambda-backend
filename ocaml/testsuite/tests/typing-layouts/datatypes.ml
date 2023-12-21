(* TEST
   * expect
   flags = "-extension layouts"
   * expect
   flags = "-extension layouts_beta"
*)

(* Tests for jkinds in algebraic datatypes *)

(* CR layouts v5: add mixed block restriction tests. *)

type t_value : value
type t_immediate : immediate;;
type t_any : any;;
[%%expect {|
type t_value : value
type t_immediate : immediate
type t_any : any
|}];;

type t_void : void;;
[%%expect{|
Line 1, characters 14-18:
1 | type t_void : void;;
                  ^^^^
Error: Layout void is more experimental than allowed by the enabled layouts extension.
       You must enable -extension layouts_alpha to use this feature.
|}];;

(********************************************************)
(* Test 1: constructor arguments may be values or voids *)

(* CR layouts v5: Needs void - moved to [datatypes_alpha.ml] *)

(************************************)
(* Test 2: but not the "any" layout *)

type t2_any1 = T2_any1 of t_any
[%%expect {|
Line 1, characters 15-31:
1 | type t2_any1 = T2_any1 of t_any
                   ^^^^^^^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t2_any2 = T2_any2 of t_immediate * t_any
[%%expect {|
Line 1, characters 15-45:
1 | type t2_any2 = T2_any2 of t_immediate * t_any
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t2_any3 = T2_any3 of t_any * t_value
[%%expect {|
Line 1, characters 15-41:
1 | type t2_any3 = T2_any3 of t_any * t_value
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type 'a t1_constraint = T1_con of 'a constraint 'a = 'b t1_constraint'
and 'b t1_constraint' = t_any
[%%expect {|
Line 2, characters 0-29:
2 | and 'b t1_constraint' = t_any
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       'b t1_constraint' has layout any, which is not representable.
|}]

(******************************************************)
(* Test 3: void allowed in records, but not by itself *)

(* CR layouts v5: Needs void - moved to [datatypes_alpha.ml].  Will change by
   the time we add back void anyway. *)

(**************************)
(* Test 4: but any is not *)

type t4_any1 = { x : t_any }
[%%expect {|
Line 1, characters 17-26:
1 | type t4_any1 = { x : t_any }
                     ^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t4_any2 = { x : t_immediate; y : t_any }
[%%expect {|
Line 1, characters 34-43:
1 | type t4_any2 = { x : t_immediate; y : t_any }
                                      ^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t4_any3 =  { x : t_any; y : t_value }
[%%expect {|
Line 1, characters 18-28:
1 | type t4_any3 =  { x : t_any; y : t_value }
                      ^^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t4_cany1 = C of { x : t_any }
[%%expect {|
Line 1, characters 23-32:
1 | type t4_cany1 = C of { x : t_any }
                           ^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t4_cany2 = C of { x : t_immediate; y : t_any }
[%%expect {|
Line 1, characters 40-49:
1 | type t4_cany2 = C of { x : t_immediate; y : t_any }
                                            ^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t4_cany3 = C of { x : t_any; y : t_value }
[%%expect {|
Line 1, characters 23-33:
1 | type t4_cany3 = C of { x : t_any; y : t_value }
                           ^^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

(*********************************************************)
(* Test 5: These same rules apply to extensible variants *)

(* CR layouts v5: void parts of this test from datatypes_alpha. *)
type t4_any1 = { x : t_any }
[%%expect {|
Line 1, characters 17-26:
1 | type t4_any1 = { x : t_any }
                     ^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t4_any2 = { x : t_immediate; y : t_any }
[%%expect {|
Line 1, characters 34-43:
1 | type t4_any2 = { x : t_immediate; y : t_any }
                                      ^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t4_any3 =  { x : t_any; y : t_value }
[%%expect {|
Line 1, characters 18-28:
1 | type t4_any3 =  { x : t_any; y : t_value }
                      ^^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t4_cany1 = C of { x : t_any }
[%%expect {|
Line 1, characters 23-32:
1 | type t4_cany1 = C of { x : t_any }
                           ^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t4_cany2 = C of { x : t_immediate; y : t_any }
[%%expect {|
Line 1, characters 40-49:
1 | type t4_cany2 = C of { x : t_immediate; y : t_any }
                                            ^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t4_cany3 = C of { x : t_any; y : t_value }
[%%expect {|
Line 1, characters 23-33:
1 | type t4_cany3 = C of { x : t_any; y : t_value }
                           ^^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

(*********************************************************)
(* Test 5: These same rules apply to extensible variants *)

(* CR layouts v5: void parts of this test from [datatypes_alpha] *)
type t5 = ..

type t5 += T5_2 of t_value
type t5 += T5_3 of t_immediate

type t5 += T5_6 of t_value * t_immediate;;
[%%expect{|
type t5 = ..
type t5 += T5_2 of t_value
type t5 += T5_3 of t_immediate
type t5 += T5_6 of t_value * t_immediate
|}]


type t5 += T5_7 of t_any
[%%expect {|
Line 1, characters 11-24:
1 | type t5 += T5_7 of t_any
               ^^^^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t5 += T5_8 of t_immediate * t_any
[%%expect {|
Line 1, characters 11-38:
1 | type t5 += T5_8 of t_immediate * t_any
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t5 += T5_9 of t_any * t_value
[%%expect {|
Line 1, characters 11-34:
1 | type t5 += T5_9 of t_any * t_value
               ^^^^^^^^^^^^^^^^^^^^^^^
Error: Constructor argument types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

type t5 += T5_11 of { x : t_value }
type t5 += T5_12 of { x : t_immediate }

type t5 += T5_15 of { x : t_value; y : t_immediate };;
[%%expect{|
type t5 += T5_11 of { x : t_value; }
type t5 += T5_12 of { x : t_immediate; }
type t5 += T5_15 of { x : t_value; y : t_immediate; }
|}];;

type t5 += T5_17 of { x : t_immediate; y : t_any }
[%%expect {|
Line 1, characters 39-48:
1 | type t5 += T5_17 of { x : t_immediate; y : t_any }
                                           ^^^^^^^^^
Error: Record element types must have a representable layout.
        t_any has layout any, which is not representable.
|}];;

(**************************************************************************)
(* Test 6: fields in all-float records get jkind value.  may change in the
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
       float has layout value, which is not a sublayout of immediate.
|}];;

(*****************************************************)
(* Test 7: Recursive propagation of immediacy checks *)

(* CR layouts v5: copy test from datatypes_alpha when non-values can go in
   general datatype declarations. *)

(***********************************************************************)
(* Test 8: Type parameters in the presence of recursive concrete usage *)

(* CR layouts: copy test from datatypes_alpha with float64 when available *)

type t_float64 : float64
type ('a : float64) float64_t

[%%expect {|
type t_float64 : float64
type ('a : float64) float64_t
|}]

type 'b t = 'b float64_t * t2
and t2 = t_float64 float64_t

[%%expect {|
type ('b : float64) t = 'b float64_t * t2
and t2 = t_float64 float64_t
|}]

type 'b t = 'b float64_t * t2
and t2 = Mk1 of t_float64 t | Mk2

[%%expect {|
type ('b : float64) t = 'b float64_t * t2
and t2 = Mk1 of t_float64 t | Mk2
|}]

type 'a t8_5 = { x : 'a t8_6; y : string}
and 'a t8_6 = 'a float64_t;;
[%%expect {|
Line 1, characters 21-28:
1 | type 'a t8_5 = { x : 'a t8_6; y : string}
                         ^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         'a has layout float64, which does not overlap with value.
       The fix will likely be to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}]


(*****************************************************************************)
(* Test 9: Looking through polytypes in mutually recursive type declarations *)

type 'a t9_1 = unit
and t9_2 = { x : string t9_1 }
and t9_3 = { x : 'a. 'a t9_1 }

[%%expect {|
type 'a t9_1 = unit
and t9_2 = { x : string t9_1; }
and t9_3 = { x : 'a. 'a t9_1; }
|}]

type 'a floaty = float#
and t9_4 = { x : float#; y : string floaty }
and t9_5 = { x : float#; y : 'a. 'a floaty }

[%%expect {|
type 'a floaty = float#
and t9_4 = { x : float#; y : string floaty; }
and t9_5 = { x : float#; y : 'a. 'a floaty; }
|}]

(*****************************************************)
(* Test 10: Constraints and parameter kind inference *)

module M : sig
  type ('a : any) t constraint 'a = int
end = struct
  type ('a : value) t = 'a constraint 'a = int
end

[%%expect {|
module M : sig type 'a t constraint 'a = int end
|}]

module M : sig
  type 'a t1 : value constraint 'a = 'b t2
  and (!'c : any) t2
end = struct
  type 'a t1 = 'b constraint 'a = 'b t2
  and (!'c : any) t2
end

type t3 = t_any M.t2
type t4 = t_any M.t2 M.t1

[%%expect {|
module M :
  sig type 'a t1 : value constraint 'a = 'b t2 and (!'c : any) t2 end
type t3 = t_any M.t2
Line 10, characters 10-20:
10 | type t4 = t_any M.t2 M.t1
               ^^^^^^^^^^
Error: This type t_any M.t2 should be an instance of type 'a M.t2
       t_any has layout any, which is not a sublayout of value.
|}]

module M : sig
  type (!'c : any) t2
  and 'a t1 : value constraint 'a = 'b t2
end = struct
  type (!'c : any) t2
  and 'a t1 = 'b constraint 'a = 'b t2
end

type t3 = t_any M.t2
type t4 = t_any M.t2 M.t1

[%%expect {|
module M :
  sig type (!'c : any) t2 and 'a t1 : value constraint 'a = 'b t2 end
type t3 = t_any M.t2
Line 10, characters 10-20:
10 | type t4 = t_any M.t2 M.t1
               ^^^^^^^^^^
Error: This type t_any M.t2 should be an instance of type 'a M.t2
       t_any has layout any, which is not a sublayout of value.
|}]

module M : sig
  type (!'c : any) t2
  type 'a t1 : value constraint 'a = 'b t2
end = struct
  type (!'c : any) t2
  type 'a t1 = 'b constraint 'a = 'b t2
end

type t3 = t_any M.t2
type t4 = t_any M.t2 M.t1

[%%expect {|
module M :
  sig type (!'c : any) t2 type 'a t1 : value constraint 'a = 'b t2 end
type t3 = t_any M.t2
Line 10, characters 10-20:
10 | type t4 = t_any M.t2 M.t1
               ^^^^^^^^^^
Error: This type t_any M.t2 should be an instance of type 'a M.t2
       t_any has layout any, which is not a sublayout of value.
|}]

