(* TEST
 {
   flags = "-extension layouts_beta -extension small_numbers_beta";
   expect;
 }
*)

(* This file contains typing tests for the layout [bits16].

   Runtime tests for the type [int16#] can be found in the
   [unboxed_int16], [alloc], and [test_int16_u] tests in this
   directory.  The type [int16#] here is used as a convenient example of a
   concrete [bits16] type in some tests, but its behavior isn't the primary
   purpose of this test. *)

type t_bits16 : bits16
type ('a : bits16) t_bits16_id = 'a

(*********************************)
(* Test 1: The identity function *)

let f1_1 (x : t_bits16) = x;;
let f1_2 (x : 'a t_bits16_id) = x;;
let f1_3 (x : int16#) = x;;
[%%expect{|
type t_bits16 : bits16
type ('a : bits16) t_bits16_id = 'a
val f1_1 : t_bits16 -> t_bits16 = <fun>
val f1_2 : ('a : bits16). 'a t_bits16_id -> 'a t_bits16_id = <fun>
val f1_3 : int16# -> int16# = <fun>
|}];;

(*****************************************)
(* Test 2: You can let-bind them locally *)
let f2_1 (x : t_bits16) =
  let y = x in
  y;;

let f2_2 (x : 'a t_bits16_id) =
  let y = x in
  y;;

let f2_3 (x : int16#) =
  let y = x in
  y;;
[%%expect{|
val f2_1 : t_bits16 -> t_bits16 = <fun>
val f2_2 : ('a : bits16). 'a t_bits16_id -> 'a t_bits16_id = <fun>
val f2_3 : int16# -> int16# = <fun>
|}];;

(*****************************************)
(* Test 3: No module-level bindings yet. *)

let x3_1 : t_bits16 = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_1 : t_bits16 = assert false;;
        ^^^^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x3_1" has layout "bits16".
|}];;

let x3_2 : 'a t_bits16_id = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_2 : 'a t_bits16_id = assert false;;
        ^^^^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x3_2" has layout "bits16".
|}];;

let x3_3 : int16# = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_3 : int16# = assert false;;
        ^^^^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x3_3" has layout "bits16".
|}];;

module M3_4 = struct
  let x : t_bits16 = assert false
end
[%%expect{|
Line 2, characters 6-7:
2 |   let x : t_bits16 = assert false
          ^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x" has layout "bits16".
|}];;

module M3_5 = struct
  let f (x : int16#) = x

  let y = f (assert false)
end
[%%expect{|
Line 4, characters 6-7:
4 |   let y = f (assert false)
          ^
Error: Types of top-level module bindings must have layout "value", but
       the type of "y" has layout "bits16".
|}];;

(*************************************)
(* Test 4: No putting them in tuples *)

let f4_1 (x : t_bits16) = x, false;;
[%%expect{|
Line 1, characters 26-27:
1 | let f4_1 (x : t_bits16) = x, false;;
                              ^
Error: This expression has type "t_bits16"
       but an expression was expected of type "('a : value_or_null)"
       The layout of t_bits16 is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of t_bits16 must be a sublayout of value
         because it's the type of a tuple element.
|}];;

let f4_2 (x : 'a t_bits16_id) = x, false;;
[%%expect{|
Line 1, characters 32-33:
1 | let f4_2 (x : 'a t_bits16_id) = x, false;;
                                    ^
Error: This expression has type "'a t_bits16_id" = "('a : bits16)"
       but an expression was expected of type "('b : value_or_null)"
       The layout of 'a t_bits16_id is bits16
         because of the definition of t_bits16_id at line 2, characters 0-35.
       But the layout of 'a t_bits16_id must be a sublayout of value
         because it's the type of a tuple element.
|}];;

let f4_3 (x : int16#) = x, false;;
[%%expect{|
Line 1, characters 24-25:
1 | let f4_3 (x : int16#) = x, false;;
                            ^
Error: This expression has type "int16#" but an expression was expected of type
         "('a : value_or_null)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because it's the type of a tuple element.
|}];;

type t4_4 = t_bits16 * string;;
[%%expect{|
Line 1, characters 12-20:
1 | type t4_4 = t_bits16 * string;;
                ^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of "t_bits16" is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of "t_bits16" must be a sublayout of value
         because it's the type of a tuple element.
|}];;

type t4_5 = int * int16#;;
[%%expect{|
Line 1, characters 18-24:
1 | type t4_5 = int * int16#;;
                      ^^^^^^
Error: Tuple element types must have layout value.
       The layout of "int16#" is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of "int16#" must be a sublayout of value
         because it's the type of a tuple element.
|}];;

type ('a : bits16) t4_6 = 'a * 'a
[%%expect{|
Line 1, characters 26-28:
1 | type ('a : bits16) t4_6 = 'a * 'a
                              ^^
Error: Tuple element types must have layout value.
       The layout of "'a" is bits16
         because of the annotation on 'a in the declaration of the type t4_6.
       But the layout of "'a" must overlap with value
         because it's the type of a tuple element.
|}];;

(* check for layout propagation *)
type ('a : bits16, 'b) t4_7 = ('a as 'b) -> ('b * 'b);;
[%%expect{|
Line 1, characters 45-47:
1 | type ('a : bits16, 'b) t4_7 = ('a as 'b) -> ('b * 'b);;
                                                 ^^
Error: Tuple element types must have layout value.
       The layout of "'a" is bits16
         because of the annotation on 'a in the declaration of the type t4_7.
       But the layout of "'a" must overlap with value
         because it's the type of a tuple element.
|}]

(*********************************************************)
(* Test 5: Allowed in some structures in typedecls. *)

type t5_1 = { x : t_bits16 };;
[%%expect{|
type t5_1 = { x : t_bits16; }
|}];;

type t5_2 = { y : int; x : t_bits16 };;
[%%expect{|
type t5_2 = { y : int; x : t_bits16; }
|}];;

type t5_2' = { y : string; x : t_bits16 };;
[%%expect{|
type t5_2' = { y : string; x : t_bits16; }
|}];;

type t5_3 = { x : t_bits16 } [@@unboxed];;
[%%expect{|
type t5_3 = { x : t_bits16; } [@@unboxed]
|}];;


type t5_4 = A of t_bits16;;
[%%expect{|
type t5_4 = A of t_bits16
|}];;

type t5_5 = A of int * t_bits16;;
[%%expect{|
type t5_5 = A of int * t_bits16
|}];;

type ('a : bits16) t5_7 = A of int
type ('a : bits16) t5_8 = A of 'a;;
[%%expect{|
type ('a : bits16) t5_7 = A of int
type ('a : bits16) t5_8 = A of 'a
|}]

(* No mixed block restriction: the compiler reorders the block for you, moving
   the unboxed type to the flat suffix. *)
type 'a t_reordered = A of t_bits16 * 'a

[%%expect{|
type 'a t_reordered = A of t_bits16 * 'a
|}]

type t5_6 = A of t_bits16 [@@unboxed];;
[%%expect{|
type t5_6 = A of t_bits16 [@@unboxed]
|}];;

type t5_6_1 = A of { x : t_bits16 } [@@unboxed];;
[%%expect{|
type t5_6_1 = A of { x : t_bits16; } [@@unboxed]
|}];;

(****************************************************)
(* Test 6: Can't be put at top level of signatures. *)
module type S6_1 = sig val x : t_bits16 end

let f6 (m : (module S6_1)) = let module M6 = (val m) in M6.x;;
[%%expect{|
Line 1, characters 31-39:
1 | module type S6_1 = sig val x : t_bits16 end
                                   ^^^^^^^^
Error: This type signature for "x" is not a value type.
       The layout of type t_bits16 is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of type t_bits16 must be a sublayout of value
         because it's the type of something stored in a module structure.
|}];;

module type S6_2 = sig val x : 'a t_bits16_id end
[%%expect{|
Line 1, characters 31-45:
1 | module type S6_2 = sig val x : 'a t_bits16_id end
                                   ^^^^^^^^^^^^^^
Error: This type signature for "x" is not a value type.
       The layout of type 'a t_bits16_id is bits16
         because of the definition of t_bits16_id at line 2, characters 0-35.
       But the layout of type 'a t_bits16_id must be a sublayout of value
         because it's the type of something stored in a module structure.
|}];;

module type S6_3 = sig val x : int16# end
[%%expect{|
Line 1, characters 31-37:
1 | module type S6_3 = sig val x : int16# end
                                   ^^^^^^
Error: This type signature for "x" is not a value type.
       The layout of type int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of type int16# must be a sublayout of value
         because it's the type of something stored in a module structure.
|}];;


(*********************************************************)
(* Test 7: Can't be used as polymorphic variant argument *)
let f7_1 (x : t_bits16) = `A x;;
[%%expect{|
Line 1, characters 29-30:
1 | let f7_1 (x : t_bits16) = `A x;;
                                 ^
Error: This expression has type "t_bits16"
       but an expression was expected of type "('a : value_or_null)"
       The layout of t_bits16 is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of t_bits16 must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}];;

let f7_2 (x : 'a t_bits16_id) = `A x;;
[%%expect{|
Line 1, characters 35-36:
1 | let f7_2 (x : 'a t_bits16_id) = `A x;;
                                       ^
Error: This expression has type "'a t_bits16_id" = "('a : bits16)"
       but an expression was expected of type "('b : value_or_null)"
       The layout of 'a t_bits16_id is bits16
         because of the definition of t_bits16_id at line 2, characters 0-35.
       But the layout of 'a t_bits16_id must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}];;

let f7_3 (x : int16#) = `A x;;
[%%expect{|
Line 1, characters 27-28:
1 | let f7_3 (x : int16#) = `A x;;
                               ^
Error: This expression has type "int16#" but an expression was expected of type
         "('a : value_or_null)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}];;

type f7_4 = [ `A of t_bits16 ];;
[%%expect{|
Line 1, characters 20-28:
1 | type f7_4 = [ `A of t_bits16 ];;
                        ^^^^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of "t_bits16" is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of "t_bits16" must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}];;

type ('a : bits16) f7_5 = [ `A of 'a ];;
[%%expect{|
Line 1, characters 34-36:
1 | type ('a : bits16) f7_5 = [ `A of 'a ];;
                                      ^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of "'a" is bits16
         because of the annotation on 'a in the declaration of the type f7_5.
       But the layout of "'a" must overlap with value
         because it's the type of the field of a polymorphic variant.
|}];;

(************************************************************)
(* Test 8: Normal polymorphic functions don't work on them. *)

let make_t_bits16 () : t_bits16 = assert false
let make_t_bits16_id () : 'a t_bits16_id = assert false
let make_int16u () : int16# = assert false

let id_value x = x;;
[%%expect{|
val make_t_bits16 : unit -> t_bits16 = <fun>
val make_t_bits16_id : ('a : bits16). unit -> 'a t_bits16_id = <fun>
val make_int16u : unit -> int16# = <fun>
val id_value : 'a -> 'a = <fun>
|}];;

let x8_1 = id_value (make_t_bits16 ());;
[%%expect{|
Line 1, characters 20-38:
1 | let x8_1 = id_value (make_t_bits16 ());;
                        ^^^^^^^^^^^^^^^^^^
Error: This expression has type "t_bits16"
       but an expression was expected of type "('a : value_or_null)"
       The layout of t_bits16 is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of t_bits16 must be a sublayout of value
         because of the definition of id_value at line 5, characters 13-18.
|}];;

let x8_2 = id_value (make_t_bits16_id ());;
[%%expect{|
Line 1, characters 20-41:
1 | let x8_2 = id_value (make_t_bits16_id ());;
                        ^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "'a t_bits16_id" = "('a : bits16)"
       but an expression was expected of type "('b : value_or_null)"
       The layout of 'a t_bits16_id is bits16
         because of the definition of t_bits16_id at line 2, characters 0-35.
       But the layout of 'a t_bits16_id must be a sublayout of value
         because of the definition of id_value at line 5, characters 13-18.
|}];;

let x8_3 = id_value (make_int16u ());;
[%%expect{|
Line 1, characters 20-36:
1 | let x8_3 = id_value (make_int16u ());;
                        ^^^^^^^^^^^^^^^^
Error: This expression has type "int16#" but an expression was expected of type
         "('a : value_or_null)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because of the definition of id_value at line 5, characters 13-18.
|}];;

(*************************************)
(* Test 9: But bits16 functions do. *)

let twice f (x : 'a t_bits16_id) = f (f x)

let f9_1 () = twice f1_1 (make_t_bits16 ())
let f9_2 () = twice f1_2 (make_t_bits16_id ())
let f9_3 () = twice f1_3 (make_int16u ());;
[%%expect{|
val twice :
  ('a : bits16).
    ('a t_bits16_id -> 'a t_bits16_id) -> 'a t_bits16_id -> 'a t_bits16_id =
  <fun>
val f9_1 : unit -> t_bits16 t_bits16_id = <fun>
val f9_2 : ('a : bits16). unit -> 'a t_bits16_id = <fun>
val f9_3 : unit -> int16# t_bits16_id = <fun>
|}];;

(**************************************************)
(* Test 10: Invalid uses of bits16 and externals *)

(* Valid uses of bits16 in externals are tested elsewhere - this is just a test
   for uses the typechecker should reject.  In particular
   - if using a non-value layout in an external, you must supply separate
     bytecode and native code implementations,
   - [@unboxed] is allowed on unboxed types but has no effect. Same is not
     true for [@untagged].
*)

external f10_1 : int -> bool -> int16# = "foo";;
[%%expect{|
Line 1, characters 0-46:
1 | external f10_1 : int -> bool -> int16# = "foo";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}];;

external f10_2 : t_bits16 -> int = "foo";;
[%%expect{|
Line 1, characters 0-40:
1 | external f10_2 : t_bits16 -> int = "foo";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}];;

external f10_6 : (int16#[@unboxed]) -> bool -> string  = "foo" "bar";;
[%%expect{|
external f10_6 : int16# -> bool -> string = "foo" "bar"
|}];;

external f10_7 : string -> (int16#[@unboxed])  = "foo" "bar";;
[%%expect{|
external f10_7 : string -> int16# = "foo" "bar"
|}];;

external f10_8 : int16 -> int16#  = "foo" "bar" [@@unboxed];;
[%%expect{|
external f10_8 : (int16 [@untagged]) -> int16# = "foo" "bar"
|}];;

external f10_9 : (int16#[@untagged]) -> bool -> string  = "foo" "bar";;
[%%expect{|
Line 1, characters 18-24:
1 | external f10_9 : (int16#[@untagged]) -> bool -> string  = "foo" "bar";;
                      ^^^^^^
Error: Don't know how to untag this type. Only "int8", "int16", "int", and
       other immediate types can be untagged.
|}];;

external f10_10 : string -> (int16#[@untagged])  = "foo" "bar";;
[%%expect{|
Line 1, characters 29-35:
1 | external f10_10 : string -> (int16#[@untagged])  = "foo" "bar";;
                                 ^^^^^^
Error: Don't know how to untag this type. Only "int8", "int16", "int", and
       other immediate types can be untagged.
|}];;

(*************************************************)
(* Test 11: bits16 banned in extensible variants *)

type t11_1 = ..

type t11_1 += A of t_bits16;;
[%%expect{|
type t11_1 = ..
Line 3, characters 14-27:
3 | type t11_1 += A of t_bits16;;
                  ^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type.
       Consider wrapping the unboxed fields in a record.
|}]

type t11_1 += B of int16#;;
[%%expect{|
Line 1, characters 14-25:
1 | type t11_1 += B of int16#;;
                  ^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type.
       Consider wrapping the unboxed fields in a record.
|}]

type ('a : bits16) t11_2 = ..

type 'a t11_2 += A of int

type 'a t11_2 += B of 'a;;

[%%expect{|
type ('a : bits16) t11_2 = ..
type 'a t11_2 += A of int
Line 5, characters 17-24:
5 | type 'a t11_2 += B of 'a;;
                     ^^^^^^^
Error: Extensible types can't have fields of unboxed type.
       Consider wrapping the unboxed fields in a record.
|}]

(* not allowed: extensible variant with unboxed field *)
type 'a t11_2 += C : 'a * 'b -> 'a t11_2

[%%expect{|
Line 1, characters 17-40:
1 | type 'a t11_2 += C : 'a * 'b -> 'a t11_2
                     ^^^^^^^^^^^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type.
       Consider wrapping the unboxed fields in a record.
|}]

(***************************************)
(* Test 12: bits16 in objects/classes *)

(* First, disallowed uses: in object types, class parameters, etc. *)
type t12_1 = < x : t_bits16 >;;
[%%expect{|
Line 1, characters 15-27:
1 | type t12_1 = < x : t_bits16 >;;
                   ^^^^^^^^^^^^
Error: Object field types must have layout value.
       The layout of "t_bits16" is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of "t_bits16" must be a sublayout of value
         because it's the type of an object field.
|}];;

type ('a : bits16) t12_2 = < x : 'a >;;
[%%expect{|
Line 1, characters 29-35:
1 | type ('a : bits16) t12_2 = < x : 'a >;;
                                 ^^^^^^
Error: Object field types must have layout value.
       The layout of "'a" is bits16
         because of the annotation on 'a in the declaration of the type t12_2.
       But the layout of "'a" must overlap with value
         because it's the type of an object field.
|}]

class c12_3 = object method x : t_bits16 = assert false end;;
[%%expect{|
Line 1, characters 21-55:
1 | class c12_3 = object method x : t_bits16 = assert false end;;
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The method "x" has type "t_bits16" but is expected to have type
         "('a : value)"
       The layout of t_bits16 is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of t_bits16 must be a sublayout of value
         because it's the type of an object field.
|}];;

class ['a] c12_4 = object
  method x : 'a t_bits16_id -> 'a t_bits16_id = assert false
end;;
[%%expect{|
Line 2, characters 13-15:
2 |   method x : 'a t_bits16_id -> 'a t_bits16_id = assert false
                 ^^
Error: This type "('a : value)" should be an instance of type "('b : bits16)"
       The layout of 'a is value
         because it's a type argument to a class constructor.
       But the layout of 'a must overlap with bits16
         because of the definition of t_bits16_id at line 2, characters 0-35.
|}];;

class c12_5 = object val x : t_bits16 = assert false end;;
[%%expect{|
Line 1, characters 25-26:
1 | class c12_5 = object val x : t_bits16 = assert false end;;
                             ^
Error: Variables bound in a class must have layout value.
       The layout of x is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of x must be a sublayout of value
         because it's the type of a class field.
|}];;

class type c12_6 = object method x : int16# end;;
[%%expect{|
Line 1, characters 26-43:
1 | class type c12_6 = object method x : int16# end;;
                              ^^^^^^^^^^^^^^^^^
Error: The method "x" has type "int16#" but is expected to have type "('a : value)"
       The layout of int16# is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of int16# must be a sublayout of value
         because it's the type of an object field.
|}];;

class type c12_7 = object val x : int16# end
[%%expect{|
Line 1, characters 26-40:
1 | class type c12_7 = object val x : int16# end
                              ^^^^^^^^^^^^^^
Error: Variables bound in a class must have layout value.
       The layout of x is bits16
         because it is the unboxed version of the primitive type int16.
       But the layout of x must be a sublayout of value
         because it's the type of an instance variable.
|}];;

class type ['a] c12_8 = object
  val x : 'a t_bits16_id -> 'a t_bits16_id
end
[%%expect{|
Line 2, characters 10-12:
2 |   val x : 'a t_bits16_id -> 'a t_bits16_id
              ^^
Error: This type "('a : value)" should be an instance of type "('b : bits16)"
       The layout of 'a is value
         because it's a type argument to a class constructor.
       But the layout of 'a must overlap with bits16
         because of the definition of t_bits16_id at line 2, characters 0-35.
|}];;

(* Second, allowed uses: as method parameters / returns *)
type t12_8 = < f : t_bits16 -> t_bits16 >
let f12_9 (o : t12_8) x = o#f x
let f12_10 o (y : t_bits16) : t_bits16 = o#baz y y y;;
class ['a] c12_11 = object
  method x : t_bits16 -> 'a = assert false
end;;
class ['a] c12_12 = object
  method x : 'a -> t_bits16 = assert false
end;;
[%%expect{|
type t12_8 = < f : t_bits16 -> t_bits16 >
val f12_9 : t12_8 -> t_bits16 -> t_bits16 = <fun>
val f12_10 :
  < baz : t_bits16 -> t_bits16 -> t_bits16 -> t_bits16; .. > ->
  t_bits16 -> t_bits16 = <fun>
class ['a] c12_11 : object method x : t_bits16 -> 'a end
class ['a] c12_12 : object method x : 'a -> t_bits16 end
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
Error: This expression has type "('a : value_or_null)"
       but an expression was expected of type "t_bits16"
       The layout of t_bits16 is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of t_bits16 must be a sublayout of value
         because it's the type of a variable captured in an object.
|}];;

let f12_14 (m1 : t_bits16) (m2 : t_bits16) = object
  val f = fun () ->
    let _ = f1_1 m1 in
    let _ = f1_1 m2 in
    ()
end;;
[%%expect{|
Line 3, characters 17-19:
3 |     let _ = f1_1 m1 in
                     ^^
Error: "m1" must have a type of layout value because it is captured by an object.
       The layout of t_bits16 is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of t_bits16 must be a sublayout of value
         because it's the type of a variable captured in an object.
|}];;

(*********************************************************************)
(* Test 13: Ad-hoc polymorphic operations don't work on bits16 yet. *)

(* CR layouts v5: Remember to handle the case of calling these on structures
   containing other layouts. *)

let f13_1 (x : t_bits16) = x = x;;
[%%expect{|
Line 1, characters 27-28:
1 | let f13_1 (x : t_bits16) = x = x;;
                               ^
Error: This expression has type "t_bits16"
       but an expression was expected of type "('a : value_or_null)"
       The layout of t_bits16 is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of t_bits16 must be a sublayout of value.
|}];;

let f13_2 (x : t_bits16) = compare x x;;
[%%expect{|
Line 1, characters 35-36:
1 | let f13_2 (x : t_bits16) = compare x x;;
                                       ^
Error: This expression has type "t_bits16"
       but an expression was expected of type "('a : value_or_null)"
       The layout of t_bits16 is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of t_bits16 must be a sublayout of value.
|}];;

let f13_3 (x : t_bits16) = Marshal.to_bytes x;;
[%%expect{|
Line 1, characters 44-45:
1 | let f13_3 (x : t_bits16) = Marshal.to_bytes x;;
                                                ^
Error: This expression has type "t_bits16"
       but an expression was expected of type "('a : value_or_null)"
       The layout of t_bits16 is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of t_bits16 must be a sublayout of value.
|}];;

let f13_4 (x : t_bits16) = Hashtbl.hash x;;
[%%expect{|
Line 1, characters 40-41:
1 | let f13_4 (x : t_bits16) = Hashtbl.hash x;;
                                            ^
Error: This expression has type "t_bits16"
       but an expression was expected of type "('a : value)"
       The layout of t_bits16 is bits16
         because of the definition of t_bits16 at line 1, characters 0-22.
       But the layout of t_bits16 must be a sublayout of value.
|}];;
