(* TEST
   flags = "-extension layouts_alpha"
   * expect
*)

(* This file contains typing tests for the layout [float64].

   Runtime tests for the type [float#] can be found in the [unboxed_float] and
   [alloc] tests in this directory.  The type [float#] here is used as a
   convenient example of a concrete [float64] type in some tests, but its
   behavior isn't the primary purpose of this test. *)

type t_float64 [@@float64]
type ('a : float64) t_float64_id = 'a

(*********************************)
(* Test 1: The identity function *)

let f1_1 (x : t_float64) = x;;
let f1_2 (x : 'a t_float64_id) = x;;
let f1_3 (x : float#) = x;;
[%%expect{|
type t_float64 [@@float64]
type ('a : float64) t_float64_id = 'a
val f1_1 : t_float64 -> t_float64 = <fun>
val f1_2 : 'a t_float64_id -> 'a t_float64_id = <fun>
val f1_3 : float# -> float# = <fun>
|}];;

(*****************************************)
(* Test 2: You can let-bind them locally *)
let f2_1 (x : t_float64) =
  let y = x in
  y;;

let f2_2 (x : 'a t_float64_id) =
  let y = x in
  y;;

let f2_3 (x : float#) =
  let y = x in
  y;;
[%%expect{|
val f2_1 : t_float64 -> t_float64 = <fun>
val f2_2 : 'a t_float64_id -> 'a t_float64_id = <fun>
val f2_3 : float# -> float# = <fun>
|}];;

(**************************************)
(* Test 3: No top-level bindings yet. *)

let x3_1 : t_float64 = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_1 : t_float64 = assert false;;
        ^^^^
Error: Top-level module bindings must have layout value, but x3_1 has layout
       float64.
|}];;

let x3_2 : 'a t_float64_id = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_2 : 'a t_float64_id = assert false;;
        ^^^^
Error: Top-level module bindings must have layout value, but x3_2 has layout
       float64.
|}];;

let x3_3 : float# = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_3 : float# = assert false;;
        ^^^^
Error: Top-level module bindings must have layout value, but x3_3 has layout
       float64.
|}];;


(*************************************)
(* Test 4: No putting them in tuples *)

let f4_1 (x : t_float64) = x, false;;
[%%expect{|
Line 1, characters 27-28:
1 | let f4_1 (x : t_float64) = x, false;;
                               ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       t_float64 has layout float64, which is not a sublayout of value.
|}];;

let f4_2 (x : 'a t_float64_id) = x, false;;
[%%expect{|
Line 1, characters 33-34:
1 | let f4_2 (x : 'a t_float64_id) = x, false;;
                                     ^
Error: This expression has type 'a t_float64_id = ('a : float64)
       but an expression was expected of type ('b : value)
       'a t_float64_id has layout float64, which does not overlap with value.
|}];;

let f4_3 (x : float#) = x, false;;
[%%expect{|
Line 1, characters 24-25:
1 | let f4_3 (x : float#) = x, false;;
                            ^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       float# has layout float64, which is not a sublayout of value.
|}];;


(****************************************************)
(* Test 5: Can't be put in structures in typedecls. *)

type t5_1 = { x : t_float64 };;
[%%expect{|
Line 1, characters 14-27:
1 | type t5_1 = { x : t_float64 };;
                  ^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_2 = { y : int; x : t_float64 };;
[%%expect{|
Line 1, characters 23-36:
1 | type t5_2 = { y : int; x : t_float64 };;
                           ^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_3 = { x : t_float64 } [@@unboxed];;
[%%expect{|
Line 1, characters 14-27:
1 | type t5_3 = { x : t_float64 } [@@unboxed];;
                  ^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_4 = A of t_float64;;
[%%expect{|
Line 1, characters 12-26:
1 | type t5_4 = A of t_float64;;
                ^^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_5 = A of int * t_float64;;
[%%expect{|
Line 1, characters 12-32:
1 | type t5_5 = A of int * t_float64;;
                ^^^^^^^^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

type t5_6 = A of t_float64 [@@unboxed];;
[%%expect{|
Line 1, characters 12-26:
1 | type t5_6 = A of t_float64 [@@unboxed];;
                ^^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Types of this layout are not yet allowed in blocks (like records or variants).
|}];;

(****************************************************)
(* Test 6: Can't be put at top level of signatures. *)
module type S6_1 = sig val x : t_float64 end

let f6 (m : (module S6_1)) = let module M6 = (val m) in M6.x;;
[%%expect{|
Line 1, characters 31-40:
1 | module type S6_1 = sig val x : t_float64 end
                                   ^^^^^^^^^
Error: This type signature for x is not a value type.
       x has layout float64, which is not a sublayout of value.
|}];;

module type S6_2 = sig val x : 'a t_float64_id end
[%%expect{|
Line 1, characters 31-46:
1 | module type S6_2 = sig val x : 'a t_float64_id end
                                   ^^^^^^^^^^^^^^^
Error: This type signature for x is not a value type.
       x has layout float64, which does not overlap with value.
|}];;

module type S6_3 = sig val x : float# end
[%%expect{|
Line 1, characters 31-37:
1 | module type S6_3 = sig val x : float# end
                                   ^^^^^^
Error: This type signature for x is not a value type.
       x has layout float64, which is not a sublayout of value.
|}];;


(*********************************************************)
(* Test 7: Can't be used as polymorphic variant argument *)
let f7_1 (x : t_float64) = `A x;;
[%%expect{|
Line 1, characters 30-31:
1 | let f7_1 (x : t_float64) = `A x;;
                                  ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       t_float64 has layout float64, which is not a sublayout of value.
|}];;

let f7_2 (x : 'a t_float64_id) = `A x;;
[%%expect{|
Line 1, characters 36-37:
1 | let f7_2 (x : 'a t_float64_id) = `A x;;
                                        ^
Error: This expression has type 'a t_float64_id = ('a : float64)
       but an expression was expected of type ('b : value)
       'a t_float64_id has layout float64, which does not overlap with value.
|}];;

let f7_3 (x : float#) = `A x;;
[%%expect{|
Line 1, characters 27-28:
1 | let f7_3 (x : float#) = `A x;;
                               ^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       float# has layout float64, which is not a sublayout of value.
|}];;

(************************************************************)
(* Test 8: Normal polymorphic functions don't work on them. *)

let make_t_float64 () : t_float64 = assert false
let make_t_float64_id () : 'a t_float64_id = assert false
let make_floatu () : float# = assert false

let id_value x = x;;
[%%expect{|
val make_t_float64 : unit -> t_float64 = <fun>
val make_t_float64_id : unit -> 'a t_float64_id = <fun>
val make_floatu : unit -> float# = <fun>
val id_value : 'a -> 'a = <fun>
|}];;

let x8_1 = id_value (make_t_float64 ());;
[%%expect{|
Line 1, characters 20-39:
1 | let x8_1 = id_value (make_t_float64 ());;
                        ^^^^^^^^^^^^^^^^^^^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       t_float64 has layout float64, which is not a sublayout of value.
|}];;

let x8_2 = id_value (make_t_float64_id ());;
[%%expect{|
Line 1, characters 20-42:
1 | let x8_2 = id_value (make_t_float64_id ());;
                        ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type 'a t_float64_id = ('a : float64)
       but an expression was expected of type ('b : value)
       'a t_float64_id has layout float64, which does not overlap with value.
|}];;

let x8_3 = id_value (make_floatu ());;
[%%expect{|
Line 1, characters 20-36:
1 | let x8_3 = id_value (make_floatu ());;
                        ^^^^^^^^^^^^^^^^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       float# has layout float64, which is not a sublayout of value.
|}];;

(*************************************)
(* Test 9: But float64 functions do. *)

let twice f (x : 'a t_float64_id) = f (f x)

let f9_1 () = twice f1_1 (make_t_float64 ())
let f9_2 () = twice f1_2 (make_t_float64_id ())
let f9_3 () = twice f1_3 (make_floatu ());;
[%%expect{|
val twice :
  ('a t_float64_id -> 'a t_float64_id) -> 'a t_float64_id -> 'a t_float64_id =
  <fun>
val f9_1 : unit -> t_float64 t_float64_id = <fun>
val f9_2 : unit -> 'a t_float64_id = <fun>
val f9_3 : unit -> float# t_float64_id = <fun>
|}];;
