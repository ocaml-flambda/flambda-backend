(* TEST
   flags = "-extension layouts_beta"
   * expect
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
   non-representable layouts in beta. *)

(*****************************************************)
(* Test 2: Permit representable function arg/returns *)

(* CR layouts v2.5: much of this test moved to basics_alpha.  Add float# versions
   and bring them here. *)
module type S = sig
  val f1 : t_value -> t_value
  val f2 : t_imm -> t_imm64
end;;

[%%expect{|
module type S = sig val f1 : t_value -> t_value val f2 : t_imm -> t_imm64 end
|}];;

(**************************************)
(* Test 3: basic annotated parameters *)
type ('a : immediate) imm_id = 'a

[%%expect{|
type ('a : immediate) imm_id = 'a
|}];;

type my_int = int imm_id
let plus_3 (x : my_int) = x + 3
let plus_3' (x : int imm_id) = x + 3;;

[%%expect{|
type my_int = int imm_id
val plus_3 : my_int -> int = <fun>
val plus_3' : int imm_id -> int = <fun>
|}];;

let string_id (x : string imm_id) = x;;
[%%expect{|
Line 1, characters 19-25:
1 | let string_id (x : string imm_id) = x;;
                       ^^^^^^
Error: This type string should be an instance of type ('a : immediate)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of imm_id at line 1, characters 0-33.
|}];;

let id_for_imms (x : 'a imm_id) = x

let three = id_for_imms 3
let true_ = id_for_imms true;;
[%%expect{|
val id_for_imms : ('a : immediate). 'a imm_id -> 'a imm_id = <fun>
val three : int imm_id = 3
val true_ : bool imm_id = true
|}]

let not_helloworld = id_for_imms "hello world";;
[%%expect{|
Line 1, characters 33-46:
1 | let not_helloworld = id_for_imms "hello world";;
                                     ^^^^^^^^^^^^^
Error: This expression has type string but an expression was expected of type
         'a imm_id = ('a : immediate)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of id_for_imms at line 1, characters 16-35.
|}]

(************************************)
(* Test 4: parameters and recursion *)
type ('a : immediate) t4
and s4 = string t4;;

[%%expect{|
Line 2, characters 9-15:
2 | and s4 = string t4;;
             ^^^^^^
Error: This type string should be an instance of type ('a : immediate)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the annotation on 'a in the declaration of the type t4.
|}];;

type s4 = string t4
and ('a : immediate) t4;;

[%%expect{|
Line 1, characters 10-16:
1 | type s4 = string t4
              ^^^^^^
Error: This type string should be an instance of type ('a : immediate)
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the annotation on 'a in the declaration of the type t4.
|}]

type s4 = int t4
and ('a : immediate) t4;;

[%%expect{|
type s4 = int t4
and ('a : immediate) t4
|}]

type s4 = s5 t4
and ('a : immediate) t4
and s5 = int;;

[%%expect{|
type s4 = s5 t4
and ('a : immediate) t4
and s5 = int
|}]

type s4 = s5 t4
and ('a : immediate) t4
and s5 = string;;

[%%expect{|
Line 3, characters 0-15:
3 | and s5 = string;;
    ^^^^^^^^^^^^^^^
Error:
       The layout of s5 is value, because
         it is the primitive value type string.
       But the layout of s5 must be a sublayout of immediate, because
         of the annotation on 'a in the declaration of the type t4.
|}]

(* CR layouts: bring [: any] and [: void] bits back here from [basics_alpha.ml] when we allow
   them in beta. *)
type ('a : any) t4 = 'a
and s4 = string t4;;
[%%expect{|
type ('a : any) t4 = 'a
and s4 = string t4
|}];;

type s4 = string t4
and ('a : any) t4;;
[%%expect{|
type s4 = string t4
and ('a : any) t4
|}];;

type ('a : void) void4 = Void4  of 'a;;
[%%expect{|
Line 1, characters 11-15:
1 | type ('a : void) void4 = Void4  of 'a;;
               ^^^^
Error: Layout void is more experimental than allowed by -extension layouts_beta.
       You must enable -extension layouts_alpha to use this feature.
|}];;

type ('a : any) any4 = Any4 of 'a
[%%expect{|
type 'a any4 = Any4 of 'a
|}];;

(************************************************************)
(* Test 5: You can touch a void, but not return it directly *)

(* CR layouts v5: these tests moved to [basics_alpha.ml].  Bring them here.
   Also the tests will change because we'll allow returning void. *)

(****************************************)
(* Test 6: explicitly polymorphic types *)
type ('a : immediate) t6_imm = T6imm of 'a
type ('a : value) t6_val = T6val of 'a;;
[%%expect{|
type ('a : immediate) t6_imm = T6imm of 'a
type 'a t6_val = T6val of 'a
|}];;

let ignore_val6 : 'a . 'a -> unit =
  fun a -> let _ = T6val a in ();;
[%%expect{|
val ignore_val6 : 'a -> unit = <fun>
|}];;

let ignore_imm6 : 'a . 'a -> unit =
  fun a -> let _ = T6imm a in ();;
[%%expect{|
Line 2, characters 2-32:
2 |   fun a -> let _ = T6imm a in ();;
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This definition has type 'b -> unit which is less general than
         'a. 'a -> unit
       The layout of 'a is value, because
         it's an unannotated universal variable.
       But the layout of 'a must be a sublayout of immediate, because
         of the definition of t6_imm at line 1, characters 0-42.
|}];;

let o6 = object
  method ignore_imm6 : 'a . 'a -> unit =
    fun a -> let _ = T6imm a in ()
end;;
[%%expect{|
Line 3, characters 4-34:
3 |     fun a -> let _ = T6imm a in ()
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: This method has type 'b -> unit which is less general than
         'a. 'a -> unit
       The layout of 'a is value, because
         it's an unannotated universal variable.
       But the layout of 'a must be a sublayout of immediate, because
         of the definition of t6_imm at line 1, characters 0-42.
|}];;

(* CR layouts v1.5: add more tests here once you can annotate these types with
   layouts. *)

(*****************************************)
(* Test 7: the layout check in unify_var *)

type ('a : immediate) t7 = Foo7 of 'a

type t7' = (int * int) t7;;
[%%expect{|
type ('a : immediate) t7 = Foo7 of 'a
Line 3, characters 12-21:
3 | type t7' = (int * int) t7;;
                ^^^^^^^^^
Error: This type int * int should be an instance of type ('a : immediate)
       The layout of int * int is value, because
         it's a tuple type.
       But the layout of int * int must be a sublayout of immediate, because
         of the definition of t7 at line 1, characters 0-37.
|}]

(**********************************************************)
(* Test 8: Polymorphic variants take value args (for now) *)

(* CR layouts: these tests moved to [basics_alphs.ml] because they use void.
   Similar tests should be added here once we have another sort (though we
   will probably chose to allow it as an arg to polymorphic variants, not
   ban it). *)

(************************************************)
(* Test 9: Tuples only work on values (for now) *)

(* CR layouts: these tests moved to [basics_alphs.ml] because they need a
   non-value layout.  Similar tests should be added here once we have another
   sort. *)

(*************************************************)
(* Test 10: layouts are checked by "more general" *)

(* This hits the first linktype in moregen (no expansion required to see it's a
   var) *)
module M10_1 : sig
  val x : string
end = struct
  type ('a : immediate) t = 'a

  let f : 'a t -> 'a = fun x -> x

  let x = f (assert false)
end;;
[%%expect {|
Lines 3-9, characters 6-3:
3 | ......struct
4 |   type ('a : immediate) t = 'a
5 |
6 |   let f : 'a t -> 'a = fun x -> x
7 |
8 |   let x = f (assert false)
9 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a : immediate) t = 'a
           val f : ('a : immediate). 'a t -> 'a
           val x : ('a : immediate). 'a
         end
       is not included in
         sig val x : string end
       Values do not match:
         val x : ('a : immediate). 'a
       is not included in
         val x : string
       The type string is not compatible with the type string
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of x at line 8, characters 10-26.
|}];;

(* This hits the second linktype in moregen (requires expansion to see it's a
   var) *)
module M10_2 : sig
  val x : string
end = struct
  type ('a : immediate) t = 'a

  let f (x : 'a t) : 'a t = x

  let x = f (assert false)
end;;
[%%expect {|
Lines 3-9, characters 6-3:
3 | ......struct
4 |   type ('a : immediate) t = 'a
5 |
6 |   let f (x : 'a t) : 'a t = x
7 |
8 |   let x = f (assert false)
9 | end..
Error: Signature mismatch:
       Modules do not match:
         sig
           type ('a : immediate) t = 'a
           val f : ('a : immediate). 'a t -> 'a t
           val x : ('a : immediate). 'a t
         end
       is not included in
         sig val x : string end
       Values do not match:
         val x : ('a : immediate). 'a t
       is not included in
         val x : string
       The type string t = string is not compatible with the type string
       The layout of string is value, because
         it is the primitive value type string.
       But the layout of string must be a sublayout of immediate, because
         of the definition of x at line 8, characters 10-26.
|}]

(**************************************************************)
(* Test 11: objects are values and methods take/return values *)

(* CR layouts v2.5: These tests moved to [basics_alpha.ml] as they need a
   non-value sort.  Bring back here when we have one (and update to use that
   sort instead of void). *)
module M11_1 = struct
  type ('a : void) t = { x : int; v : 'a }

  let f t =
    t.v # baz11
end;;
[%%expect{|
Line 2, characters 13-17:
2 |   type ('a : void) t = { x : int; v : 'a }
                 ^^^^
Error: Layout void is more experimental than allowed by -extension layouts_beta.
       You must enable -extension layouts_alpha to use this feature.
|}]

(*******************************************************************)
(* Test 12: class parameters and bound vars must have layout value *)

(* CR layouts v2.5: These tests moved to [basics_alpha.ml] as they need a
   non-value sort.  Bring back here when we have one (and update to use that
   sort instead of void). *)

(***********************************************************)
(* Test 13: built-in type constructors work only on values *)

(* CR layouts v2.5: These tests moved to [basics_alpha.ml] as they need a
   non-value sort.  Bring back here when we have one (and update to use that
   sort instead of void). *)

(****************************************************************************)
(* Test 14: Examples motivating the trick with the manifest in [enter_type] *)
type t14 = foo14 list
and foo14 = string;;
[%%expect{|
type t14 = foo14 list
and foo14 = string
|}];;

(* CR layouts v2.5: Part of this test moved to [basics_alpha.ml] as it needs a
   non-value sort.  Bring back here when we have one. *)

(****************************************************)
(* Test 15: Type aliases need not have layout value *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(********************************************************)
(* Test 16: seperability: [msig_of_external_type] logic *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

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

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(********************************************)
(* Test 20: Non-value bodies for let module *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(**********************************)
(* Test 21: Non-value unpack body *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(***************************************************************)
(* Test 22: approx_type catch-all can't be restricted to value *)

(* CR layouts: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

type t_void : void;;
[%%expect{|
Line 1, characters 14-18:
1 | type t_void : void;;
                  ^^^^
Error: Layout void is used here, but the appropriate layouts extension is not enabled
|}];;

(********************************************************************)
(* Test 23: checking the error message from impossible GADT matches *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(*****************************************************)
(* Test 24: Polymorphic parameter with exotic layout *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(**************************************************)
(* Test 25: Optional parameter with exotic layout *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(*********************************************************)
(* Test 26: Inferring an application to an exotic layout *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(******************************************)
(* Test 27: Exotic layouts in approx_type *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(************************************)
(* Test 28: Exotic layouts in letop *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(*******************************************)
(* Test 29: [external]s default to [value] *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(**************************************)
(* Test 30: [val]s default to [value] *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(**************************************************)
(* Test 31: checking that #poly_var patterns work *)

(* CR layouts: This test moves to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(*********************************************************)
(* Test 32: Polymorphic variant constructors take values *)

(* CR layouts: This test moves to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)

(******************************************************)
(* Test 33: Externals must have representable types *)

(* CR layouts v5: This test moved to [basics_alpha.ml] as it needs a
   non-representable layout.  Bring it back here when we can mention [t_any] in
   [-extension layouts_beta]. *)

(****************************************************)
(* Test 34: Layout clash in polymorphic record type *)

type ('a : immediate) t2_imm

type s = { f : ('a : value) . 'a -> 'a u }
and 'a u = 'a t2_imm

[%%expect {|
type ('a : immediate) t2_imm
Line 3, characters 15-40:
3 | type s = { f : ('a : value) . 'a -> 'a u }
                   ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type 'a is value, because
         of the annotation on the universal variable 'a.
       But the layout of type 'a must be a sublayout of immediate, because
         of the definition of t2_imm at line 1, characters 0-28.
|}]

(****************************************************)
(* Test 35: unannotated type parameter defaults to layout value *)

(* CR layouts v2.5: This test moved to [basics_alpha.ml] as it needs a non-value
   sort.  Bring back here when we have one. *)
