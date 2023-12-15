(* TEST
   * expect
   flags = "-extension layouts_alpha"
   * expect
   flags = "-extension layouts_beta"
*)

(* This file contains typing tests for the layout [bits32].

   Runtime tests for the type [int32#] can be found in the
   [unboxed_int32], [alloc], and [stdlib__int32_u] tests in this
   directory.  The type [int32#] here is used as a convenient example of a
   concrete [bits32] type in some tests, but its behavior isn't the primary
   purpose of this test. *)

type t_bits32 : bits32
type ('a : bits32) t_bits32_id = 'a

(*********************************)
(* Test 1: The identity function *)

let f1_1 (x : t_bits32) = x;;
let f1_2 (x : 'a t_bits32_id) = x;;
let f1_3 (x : int32#) = x;;
[%%expect{|
type t_bits32 : bits32
type ('a : bits32) t_bits32_id = 'a
val f1_1 : t_bits32 -> t_bits32 = <fun>
val f1_2 : ('a : bits32). 'a t_bits32_id -> 'a t_bits32_id = <fun>
val f1_3 : int32# -> int32# = <fun>
|}];;

(*****************************************)
(* Test 2: You can let-bind them locally *)
let f2_1 (x : t_bits32) =
  let y = x in
  y;;

let f2_2 (x : 'a t_bits32_id) =
  let y = x in
  y;;

let f2_3 (x : int32#) =
  let y = x in
  y;;
[%%expect{|
val f2_1 : t_bits32 -> t_bits32 = <fun>
val f2_2 : ('a : bits32). 'a t_bits32_id -> 'a t_bits32_id = <fun>
val f2_3 : int32# -> int32# = <fun>
|}];;

(*****************************************)
(* Test 3: No module-level bindings yet. *)

let x3_1 : t_bits32 = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_1 : t_bits32 = assert false;;
        ^^^^
Error: Top-level module bindings must have layout value, but x3_1 has layout
       bits32.
|}];;

let x3_2 : 'a t_bits32_id = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_2 : 'a t_bits32_id = assert false;;
        ^^^^
Error: Top-level module bindings must have layout value, but x3_2 has layout
       bits32.
|}];;

let x3_3 : int32# = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_3 : int32# = assert false;;
        ^^^^
Error: Top-level module bindings must have layout value, but x3_3 has layout
       bits32.
|}];;

module M3_4 = struct
  let x : t_bits32 = assert false
end
[%%expect{|
Line 2, characters 6-7:
2 |   let x : t_bits32 = assert false
          ^
Error: Top-level module bindings must have layout value, but x has layout
       bits32.
|}];;

module M3_5 = struct
  let f (x : int32#) = x

  let y = f (assert false)
end
[%%expect{|
Line 4, characters 6-7:
4 |   let y = f (assert false)
          ^
Error: Top-level module bindings must have layout value, but y has layout
       bits32.
|}];;

(*************************************)
(* Test 4: No putting them in tuples *)

let f4_1 (x : t_bits32) = x, false;;
[%%expect{|
Line 1, characters 26-27:
1 | let f4_1 (x : t_bits32) = x, false;;
                              ^
Error: This expression has type t_bits32
       but an expression was expected of type ('a : value)
       t_bits32 has layout bits32, which is not a sublayout of value.
|}];;

let f4_2 (x : 'a t_bits32_id) = x, false;;
[%%expect{|
Line 1, characters 32-33:
1 | let f4_2 (x : 'a t_bits32_id) = x, false;;
                                    ^
Error: This expression has type 'a t_bits32_id = ('a : bits32)
       but an expression was expected of type ('b : value)
       'a t_bits32_id has layout bits32, which does not overlap with value.
|}];;

let f4_3 (x : int32#) = x, false;;
[%%expect{|
Line 1, characters 24-25:
1 | let f4_3 (x : int32#) = x, false;;
                            ^
Error: This expression has type int32# but an expression was expected of type
         ('a : value)
       int32# has layout bits32, which is not a sublayout of value.
|}];;

type t4_4 = t_bits32 * string;;
[%%expect{|
Line 1, characters 12-20:
1 | type t4_4 = t_bits32 * string;;
                ^^^^^^^^
Error: Tuple element types must have layout value.
        t_bits32 has layout bits32, which is not a sublayout of value.
|}];;

type t4_5 = int * int32#;;
[%%expect{|
Line 1, characters 18-24:
1 | type t4_5 = int * int32#;;
                      ^^^^^^
Error: Tuple element types must have layout value.
        int32# has layout bits32, which is not a sublayout of value.
|}];;

type ('a : bits32) t4_6 = 'a * 'a
[%%expect{|
Line 1, characters 26-28:
1 | type ('a : bits32) t4_6 = 'a * 'a
                              ^^
Error: This type ('a : value) should be an instance of type ('a0 : bits32)
       'a has layout bits32, which does not overlap with value.
|}];;

(* check for layout propagation *)
type ('a : bits32, 'b) t4_7 = ('a as 'b) -> ('b * 'b);;
[%%expect{|
Line 1, characters 31-33:
1 | type ('a : bits32, 'b) t4_7 = ('a as 'b) -> ('b * 'b);;
                                   ^^
Error: This type ('b : value) should be an instance of type ('a : bits32)
       'a has layout bits32, which does not overlap with value.
|}]

(****************************************************)
(* Test 5: Can't be put in structures in typedecls. *)

type t5_1 = { x : t_bits32 };;
[%%expect{|
Line 1, characters 14-26:
1 | type t5_1 = { x : t_bits32 };;
                  ^^^^^^^^^^^^
Error: Type t_bits32 has layout bits32.
       Records may not yet contain types of this layout.
|}];;

(* CR layouts v5: this should work *)
type t5_2 = { y : int; x : t_bits32 };;
[%%expect{|
Line 1, characters 23-35:
1 | type t5_2 = { y : int; x : t_bits32 };;
                           ^^^^^^^^^^^^
Error: Type t_bits32 has layout bits32.
       Records may not yet contain types of this layout.
|}];;

(* CR layouts: this runs afoul of the mixed block restriction, but should work
   once we relax that. *)
type t5_2' = { y : string; x : t_bits32 };;
[%%expect{|
Line 1, characters 27-39:
1 | type t5_2' = { y : string; x : t_bits32 };;
                               ^^^^^^^^^^^^
Error: Type t_bits32 has layout bits32.
       Records may not yet contain types of this layout.
|}];;

(* CR layouts 2.5: allow this *)
type t5_3 = { x : t_bits32 } [@@unboxed];;
[%%expect{|
Line 1, characters 14-26:
1 | type t5_3 = { x : t_bits32 } [@@unboxed];;
                  ^^^^^^^^^^^^
Error: Type t_bits32 has layout bits32.
       Unboxed records may not yet contain types of this layout.
|}];;

type t5_4 = A of t_bits32;;
[%%expect{|
Line 1, characters 12-25:
1 | type t5_4 = A of t_bits32;;
                ^^^^^^^^^^^^^
Error: Type t_bits32 has layout bits32.
       Variants may not yet contain types of this layout.
|}];;

type t5_5 = A of int * t_bits32;;
[%%expect{|
Line 1, characters 12-31:
1 | type t5_5 = A of int * t_bits32;;
                ^^^^^^^^^^^^^^^^^^^
Error: Type t_bits32 has layout bits32.
       Variants may not yet contain types of this layout.
|}];;

type t5_6 = A of t_bits32 [@@unboxed];;
[%%expect{|
Line 1, characters 12-25:
1 | type t5_6 = A of t_bits32 [@@unboxed];;
                ^^^^^^^^^^^^^
Error: Type t_bits32 has layout bits32.
       Variants may not yet contain types of this layout.
|}];;

type ('a : bits32) t5_7 = A of int
type ('a : bits32) t5_8 = A of 'a;;
[%%expect{|
type ('a : bits32) t5_7 = A of int
Line 2, characters 26-33:
2 | type ('a : bits32) t5_8 = A of 'a;;
                              ^^^^^^^
Error: Type 'a has layout bits32.
       Variants may not yet contain types of this layout.
|}]

(****************************************************)
(* Test 6: Can't be put at top level of signatures. *)
module type S6_1 = sig val x : t_bits32 end

let f6 (m : (module S6_1)) = let module M6 = (val m) in M6.x;;
[%%expect{|
Line 1, characters 31-39:
1 | module type S6_1 = sig val x : t_bits32 end
                                   ^^^^^^^^
Error: This type signature for x is not a value type.
       x has layout bits32, which is not a sublayout of value.
|}];;

module type S6_2 = sig val x : 'a t_bits32_id end
[%%expect{|
Line 1, characters 31-45:
1 | module type S6_2 = sig val x : 'a t_bits32_id end
                                   ^^^^^^^^^^^^^^
Error: This type signature for x is not a value type.
       x has layout bits32, which does not overlap with value.
|}];;

module type S6_3 = sig val x : int32# end
[%%expect{|
Line 1, characters 31-37:
1 | module type S6_3 = sig val x : int32# end
                                   ^^^^^^
Error: This type signature for x is not a value type.
       x has layout bits32, which is not a sublayout of value.
|}];;


(*********************************************************)
(* Test 7: Can't be used as polymorphic variant argument *)
let f7_1 (x : t_bits32) = `A x;;
[%%expect{|
Line 1, characters 29-30:
1 | let f7_1 (x : t_bits32) = `A x;;
                                 ^
Error: This expression has type t_bits32
       but an expression was expected of type ('a : value)
       t_bits32 has layout bits32, which is not a sublayout of value.
|}];;

let f7_2 (x : 'a t_bits32_id) = `A x;;
[%%expect{|
Line 1, characters 35-36:
1 | let f7_2 (x : 'a t_bits32_id) = `A x;;
                                       ^
Error: This expression has type 'a t_bits32_id = ('a : bits32)
       but an expression was expected of type ('b : value)
       'a t_bits32_id has layout bits32, which does not overlap with value.
|}];;

let f7_3 (x : int32#) = `A x;;
[%%expect{|
Line 1, characters 27-28:
1 | let f7_3 (x : int32#) = `A x;;
                               ^
Error: This expression has type int32# but an expression was expected of type
         ('a : value)
       int32# has layout bits32, which is not a sublayout of value.
|}];;

type f7_4 = [ `A of t_bits32 ];;
[%%expect{|
Line 1, characters 20-28:
1 | type f7_4 = [ `A of t_bits32 ];;
                        ^^^^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
        t_bits32 has layout bits32, which is not a sublayout of value.
|}];;

type ('a : bits32) f7_5 = [ `A of 'a ];;
[%%expect{|
Line 1, characters 34-36:
1 | type ('a : bits32) f7_5 = [ `A of 'a ];;
                                      ^^
Error: This type ('a : value) should be an instance of type ('a0 : bits32)
       'a has layout bits32, which does not overlap with value.
|}];;
(* CR layouts v2.9: This error could be improved *)

(************************************************************)
(* Test 8: Normal polymorphic functions don't work on them. *)

let make_t_bits32 () : t_bits32 = assert false
let make_t_bits32_id () : 'a t_bits32_id = assert false
let make_int32u () : int32# = assert false

let id_value x = x;;
[%%expect{|
val make_t_bits32 : unit -> t_bits32 = <fun>
val make_t_bits32_id : ('a : bits32). unit -> 'a t_bits32_id = <fun>
val make_int32u : unit -> int32# = <fun>
val id_value : 'a -> 'a = <fun>
|}];;

let x8_1 = id_value (make_t_bits32 ());;
[%%expect{|
Line 1, characters 20-38:
1 | let x8_1 = id_value (make_t_bits32 ());;
                        ^^^^^^^^^^^^^^^^^^
Error: This expression has type t_bits32
       but an expression was expected of type ('a : value)
       t_bits32 has layout bits32, which is not a sublayout of value.
|}];;

let x8_2 = id_value (make_t_bits32_id ());;
[%%expect{|
Line 1, characters 20-41:
1 | let x8_2 = id_value (make_t_bits32_id ());;
                        ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type 'a t_bits32_id = ('a : bits32)
       but an expression was expected of type ('b : value)
       'a t_bits32_id has layout bits32, which does not overlap with value.
|}];;

let x8_3 = id_value (make_int32u ());;
[%%expect{|
Line 1, characters 20-36:
1 | let x8_3 = id_value (make_int32u ());;
                        ^^^^^^^^^^^^^^^^
Error: This expression has type int32# but an expression was expected of type
         ('a : value)
       int32# has layout bits32, which is not a sublayout of value.
|}];;

(*************************************)
(* Test 9: But bits32 functions do. *)

let twice f (x : 'a t_bits32_id) = f (f x)

let f9_1 () = twice f1_1 (make_t_bits32 ())
let f9_2 () = twice f1_2 (make_t_bits32_id ())
let f9_3 () = twice f1_3 (make_int32u ());;
[%%expect{|
val twice :
  ('a : bits32).
    ('a t_bits32_id -> 'a t_bits32_id) -> 'a t_bits32_id -> 'a t_bits32_id =
  <fun>
val f9_1 : unit -> t_bits32 t_bits32_id = <fun>
val f9_2 : ('a : bits32). unit -> 'a t_bits32_id = <fun>
val f9_3 : unit -> int32# t_bits32_id = <fun>
|}];;

(**************************************************)
(* Test 10: Invalid uses of bits32 and externals *)

(* Valid uses of bits32 in externals are tested elsewhere - this is just a test
   for uses the typechecker should reject.  In particular
   - if using a non-value layout in an external, you must supply separate
     bytecode and native code implementations,
   - unboxed types can't be unboxed more.
*)

external f10_1 : int -> bool -> int32# = "foo";;
[%%expect{|
Line 1, characters 0-46:
1 | external f10_1 : int -> bool -> int32# = "foo";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}];;

external f10_2 : t_bits32 -> int = "foo";;
[%%expect{|
Line 1, characters 0-40:
1 | external f10_2 : t_bits32 -> int = "foo";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}];;

external f10_6 : (int32#[@unboxed]) -> bool -> string  = "foo" "bar";;
[%%expect{|
Line 1, characters 18-24:
1 | external f10_6 : (int32#[@unboxed]) -> bool -> string  = "foo" "bar";;
                      ^^^^^^
Error: Don't know how to unbox this type.
       Only float, int32, int64, nativeint, and vector primitives can be unboxed.
|}];;

external f10_7 : string -> (int32#[@unboxed])  = "foo" "bar";;
[%%expect{|
Line 1, characters 28-34:
1 | external f10_7 : string -> (int32#[@unboxed])  = "foo" "bar";;
                                ^^^^^^
Error: Don't know how to unbox this type.
       Only float, int32, int64, nativeint, and vector primitives can be unboxed.
|}];;

external f10_8 : int32 -> int32#  = "foo" "bar" [@@unboxed];;
[%%expect{|
Line 1, characters 26-32:
1 | external f10_8 : int32 -> int32#  = "foo" "bar" [@@unboxed];;
                              ^^^^^^
Error: Don't know how to unbox this type.
       Only float, int32, int64, nativeint, and vector primitives can be unboxed.
|}];;

(*******************************************************)
(* Test 11: Don't allow bits32 in extensible variants *)

type t11_1 = ..

type t11_1 += A of t_bits32;;
[%%expect{|
type t11_1 = ..
Line 3, characters 14-27:
3 | type t11_1 += A of t_bits32;;
                  ^^^^^^^^^^^^^
Error: Type t_bits32 has layout bits32.
       Variants may not yet contain types of this layout.
|}]

type t11_1 += B of int32#;;
[%%expect{|
Line 1, characters 14-25:
1 | type t11_1 += B of int32#;;
                  ^^^^^^^^^^^
Error: Type int32# has layout bits32.
       Variants may not yet contain types of this layout.
|}]

type ('a : bits32) t11_2 = ..

type 'a t11_2 += A of int

type 'a t11_2 += B of 'a;;

[%%expect{|
type ('a : bits32) t11_2 = ..
type 'a t11_2 += A of int
Line 5, characters 17-24:
5 | type 'a t11_2 += B of 'a;;
                     ^^^^^^^
Error: Type 'a has layout bits32.
       Variants may not yet contain types of this layout.
|}]

(***************************************)
(* Test 12: bits32 in objects/classes *)

(* First, disallowed uses: in object types, class parameters, etc. *)
type t12_1 = < x : t_bits32 >;;
[%%expect{|
Line 1, characters 15-27:
1 | type t12_1 = < x : t_bits32 >;;
                   ^^^^^^^^^^^^
Error: Object field types must have layout value.
        t_bits32 has layout bits32, which is not a sublayout of value.
|}];;

type ('a : bits32) t12_2 = < x : 'a >;;
[%%expect{|
Line 1, characters 33-35:
1 | type ('a : bits32) t12_2 = < x : 'a >;;
                                     ^^
Error: This type ('a : value) should be an instance of type ('a0 : bits32)
       'a has layout bits32, which does not overlap with value.
|}]

class c12_3 = object method x : t_bits32 = assert false end;;
[%%expect{|
Line 1, characters 21-55:
1 | class c12_3 = object method x : t_bits32 = assert false end;;
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The method x has type t_bits32 but is expected to have type
         ('a : value)
       t_bits32 has layout bits32, which is not a sublayout of value.
|}];;

class ['a] c12_4 = object
  method x : 'a t_bits32_id -> 'a t_bits32_id = assert false
end;;
[%%expect{|
Line 2, characters 13-15:
2 |   method x : 'a t_bits32_id -> 'a t_bits32_id = assert false
                 ^^
Error: This type ('a : bits32) should be an instance of type ('a0 : value)
       'a has layout value, which does not overlap with bits32.
|}];;
(* CR layouts v2.9: Error could be improved *)

class c12_5 = object val x : t_bits32 = assert false end;;
[%%expect{|
Line 1, characters 25-26:
1 | class c12_5 = object val x : t_bits32 = assert false end;;
                             ^
Error: Variables bound in a class must have layout value.
       x has layout bits32, which is not a sublayout of value.
|}];;

class type c12_6 = object method x : int32# end;;
[%%expect{|
Line 1, characters 26-43:
1 | class type c12_6 = object method x : int32# end;;
                              ^^^^^^^^^^^^^^^^^
Error: The method x has type int32# but is expected to have type ('a : value)
       int32# has layout bits32, which is not a sublayout of value.
|}];;
(* CR layouts v2.9: Error could be improved *)

class type c12_7 = object val x : int32# end
[%%expect{|
Line 1, characters 26-40:
1 | class type c12_7 = object val x : int32# end
                              ^^^^^^^^^^^^^^
Error: Variables bound in a class must have layout value.
       x has layout bits32, which is not a sublayout of value.
|}];;

class type ['a] c12_8 = object
  val x : 'a t_bits32_id -> 'a t_bits32_id
end
[%%expect{|
Line 2, characters 10-12:
2 |   val x : 'a t_bits32_id -> 'a t_bits32_id
              ^^
Error: This type ('a : bits32) should be an instance of type ('a0 : value)
       'a has layout value, which does not overlap with bits32.
|}];;

(* Second, allowed uses: as method parameters / returns *)
type t12_8 = < f : t_bits32 -> t_bits32 >
let f12_9 (o : t12_8) x = o#f x
let f12_10 o (y : t_bits32) : t_bits32 = o#baz y y y;;
class ['a] c12_11 = object
  method x : t_bits32 -> 'a = assert false
end;;
class ['a] c12_12 = object
  method x : 'a -> t_bits32 = assert false
end;;
[%%expect{|
type t12_8 = < f : t_bits32 -> t_bits32 >
val f12_9 : t12_8 -> t_bits32 -> t_bits32 = <fun>
val f12_10 :
  < baz : t_bits32 -> t_bits32 -> t_bits32 -> t_bits32; .. > ->
  t_bits32 -> t_bits32 = <fun>
class ['a] c12_11 : object method x : t_bits32 -> 'a end
class ['a] c12_12 : object method x : 'a -> t_bits32 end
|}];;

(* Third, another disallowed use: capture in an object. *)
let f12_13 m1 m2 = object
  val f = fun () ->
    let _ = f1_1 m1 in
    let _ = f1_1 m2 in
    ()
end;;
[%%expect{|
Line 3, characters 17-19:
3 |     let _ = f1_1 m1 in
                     ^^
Error: This expression has type ('a : value)
       but an expression was expected of type t_bits32
       t_bits32 has layout bits32, which is not a sublayout of value.
|}];;

let f12_14 (m1 : t_bits32) (m2 : t_bits32) = object
  val f = fun () ->
    let _ = f1_1 m1 in
    let _ = f1_1 m2 in
    ()
end;;
[%%expect{|
Line 3, characters 17-19:
3 |     let _ = f1_1 m1 in
                     ^^
Error: m1 must have a type of layout value because it is captured by an object.
       t_bits32 has layout bits32, which is not a sublayout of value.
|}];;

(*********************************************************************)
(* Test 13: Ad-hoc polymorphic operations don't work on bits32 yet. *)

(* CR layouts v5: Remember to handle the case of calling these on structures
   containing other layouts. *)

let f13_1 (x : t_bits32) = x = x;;
[%%expect{|
Line 1, characters 27-28:
1 | let f13_1 (x : t_bits32) = x = x;;
                               ^
Error: This expression has type t_bits32
       but an expression was expected of type ('a : value)
       t_bits32 has layout bits32, which is not a sublayout of value.
|}];;

let f13_2 (x : t_bits32) = compare x x;;
[%%expect{|
Line 1, characters 35-36:
1 | let f13_2 (x : t_bits32) = compare x x;;
                                       ^
Error: This expression has type t_bits32
       but an expression was expected of type ('a : value)
       t_bits32 has layout bits32, which is not a sublayout of value.
|}];;

let f13_3 (x : t_bits32) = Marshal.to_bytes x;;
[%%expect{|
Line 1, characters 44-45:
1 | let f13_3 (x : t_bits32) = Marshal.to_bytes x;;
                                                ^
Error: This expression has type t_bits32
       but an expression was expected of type ('a : value)
       t_bits32 has layout bits32, which is not a sublayout of value.
|}];;

let f13_4 (x : t_bits32) = Hashtbl.hash x;;
[%%expect{|
Line 1, characters 40-41:
1 | let f13_4 (x : t_bits32) = Hashtbl.hash x;;
                                            ^
Error: This expression has type t_bits32
       but an expression was expected of type ('a : value)
       t_bits32 has layout bits32, which is not a sublayout of value.
|}];;
