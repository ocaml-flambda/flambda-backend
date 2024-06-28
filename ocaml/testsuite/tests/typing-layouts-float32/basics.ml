(* TEST
 flambda2;
 {
   flags = "-extension small_numbers";
   expect;
 }
*)

(* This file contains typing tests for the layout [float32].

   Runtime tests for the type [float32#] can be found in the [unboxed_float] and
   [alloc] tests in this directory.  The type [float32#] here is used as a
   convenient example of a concrete [float32] type in some tests, but its
   behavior isn't the primary purpose of this test. *)

type t_float32 : float32
type ('a : float32) t_float32_id = 'a

(*********************************)
(* Test 1: The identity function *)

let f1_1 (x : t_float32) = x;;
let f1_2 (x : 'a t_float32_id) = x;;
let f1_3 (x : float32#) = x;;
[%%expect{|
type t_float32 : float32
type ('a : float32) t_float32_id = 'a
val f1_1 : t_float32 -> t_float32 = <fun>
val f1_2 : ('a : float32). 'a t_float32_id -> 'a t_float32_id = <fun>
val f1_3 : float32# -> float32# = <fun>
|}];;

(*****************************************)
(* Test 2: You can let-bind them locally *)
let f2_1 (x : t_float32) =
  let y = x in
  y;;

let f2_2 (x : 'a t_float32_id) =
  let y = x in
  y;;

let f2_3 (x : float32#) =
  let y = x in
  y;;
[%%expect{|
val f2_1 : t_float32 -> t_float32 = <fun>
val f2_2 : ('a : float32). 'a t_float32_id -> 'a t_float32_id = <fun>
val f2_3 : float32# -> float32# = <fun>
|}];;

(*****************************************)
(* Test 3: No module-level bindings yet. *)

let x3_1 : t_float32 = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_1 : t_float32 = assert false;;
        ^^^^
Error: Types of top-level module bindings must have layout value, but
       the type of x3_1 has layout float32.
|}];;

let x3_2 : 'a t_float32_id = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_2 : 'a t_float32_id = assert false;;
        ^^^^
Error: Types of top-level module bindings must have layout value, but
       the type of x3_2 has layout float32.
|}];;

let x3_3 : float32# = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_3 : float32# = assert false;;
        ^^^^
Error: Types of top-level module bindings must have layout value, but
       the type of x3_3 has layout float32.
|}];;

module M3_4 = struct
  let x : t_float32 = assert false
end
[%%expect{|
Line 2, characters 6-7:
2 |   let x : t_float32 = assert false
          ^
Error: Types of top-level module bindings must have layout value, but
       the type of x has layout float32.
|}];;

module M3_5 = struct
  let f (x : float32#) = x

  let y = f (assert false)
end
[%%expect{|
Line 4, characters 6-7:
4 |   let y = f (assert false)
          ^
Error: Types of top-level module bindings must have layout value, but
       the type of y has layout float32.
|}];;

(*************************************)
(* Test 4: No putting them in tuples *)

let f4_1 (x : t_float32) = x, false;;
[%%expect{|
Line 1, characters 27-28:
1 | let f4_1 (x : t_float32) = x, false;;
                               ^
Error: This expression has type t_float32
       but an expression was expected of type ('a : value)
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because it's the type of a tuple element.
|}];;

let f4_2 (x : 'a t_float32_id) = x, false;;
[%%expect{|
Line 1, characters 33-34:
1 | let f4_2 (x : 'a t_float32_id) = x, false;;
                                     ^
Error: This expression has type 'a t_float32_id = ('a : float32)
       but an expression was expected of type ('b : value)
       The layout of 'a t_float32_id is float32
         because of the definition of t_float32_id at line 2, characters 0-37.
       But the layout of 'a t_float32_id must overlap with value
         because it's the type of a tuple element.
|}];;

let f4_3 (x : float32#) = x, false;;
[%%expect{|
Line 1, characters 26-27:
1 | let f4_3 (x : float32#) = x, false;;
                              ^
Error: This expression has type float32#
       but an expression was expected of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because it's the type of a tuple element.
|}];;

type t4_4 = t_float32 * string;;
[%%expect{|
Line 1, characters 12-21:
1 | type t4_4 = t_float32 * string;;
                ^^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because it's the type of a tuple element.
|}];;

type t4_5 = int * float32#;;
[%%expect{|
Line 1, characters 18-26:
1 | type t4_5 = int * float32#;;
                      ^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because it's the type of a tuple element.
|}];;

type ('a : float32) t4_6 = 'a * 'a
[%%expect{|
Line 1, characters 27-29:
1 | type ('a : float32) t4_6 = 'a * 'a
                               ^^
Error: This type ('a : value) should be an instance of type ('a0 : float32)
       The layout of 'a is float32
         because of the annotation on 'a in the declaration of the type t4_6.
       But the layout of 'a must overlap with value
         because it's the type of a tuple element.
|}];;

(* check for layout propagation *)
type ('a : float32, 'b) t4_7 = ('a as 'b) -> ('b * 'b);;
[%%expect{|
Line 1, characters 32-34:
1 | type ('a : float32, 'b) t4_7 = ('a as 'b) -> ('b * 'b);;
                                    ^^
Error: This type ('b : value) should be an instance of type ('a : float32)
       The layout of 'a is float32, because
         of the annotation on 'a in the declaration of the type t4_7.
       But the layout of 'a must overlap with value, because
         it instantiates an unannotated type parameter of t4_7, defaulted to layout value.
|}]

(*****************************************)
(* Test 5: float32 in structures *)

(* all-float32 records are allowed, as are some records that mix float32 and
   value fields. See [tests/typing-layouts/mixed_records.ml] for tests of mixed
   records. *)
type t5_1 = { x : t_float32 };;
[%%expect{|
type t5_1 = { x : t_float32; }
|}];;

(* CR layouts 2.5: allow this *)
type t5_3 = { x : t_float32 } [@@unboxed];;
[%%expect{|
Line 1, characters 14-27:
1 | type t5_3 = { x : t_float32 } [@@unboxed];;
                  ^^^^^^^^^^^^^
Error: Type t_float32 has layout float32.
       Unboxed records may not yet contain types of this layout.
|}];;

type t5_4 = A of t_float32;;
[%%expect{|
type t5_4 = A of t_float32
|}];;

type t5_5 = A of int * t_float32;;
[%%expect{|
type t5_5 = A of int * t_float32
|}];;

type t5_6 = A of t_float32 [@@unboxed];;
[%%expect{|
Line 1, characters 12-26:
1 | type t5_6 = A of t_float32 [@@unboxed];;
                ^^^^^^^^^^^^^^
Error: Type t_float32 has layout float32.
       Unboxed variants may not yet contain types of this layout.
|}];;

type t5_6_1 = A of { x : t_float32 } [@@unboxed];;
[%%expect{|
Line 1, characters 21-34:
1 | type t5_6_1 = A of { x : t_float32 } [@@unboxed];;
                         ^^^^^^^^^^^^^
Error: Type t_float32 has layout float32.
       Unboxed inlined records may not yet contain types of this layout.
|}];;

type ('a : float32) t5_7 = A of int
type ('a : float32) t5_8 = A of 'a;;
[%%expect{|
type ('a : float32) t5_7 = A of int
type ('a : float32) t5_8 = A of 'a
|}]

type ('a : float32, 'b : float32) t5_9 = {x : 'a; y : 'b; z : 'a}

type 'a t5_10 = 'a t_float32_id
and 'a t5_11 = {x : 'a t5_10; y : 'a}
[%%expect{|
type ('a : float32, 'b : float32) t5_9 = { x : 'a; y : 'b; z : 'a; }
Line 4, characters 20-28:
4 | and 'a t5_11 = {x : 'a t5_10; y : 'a}
                        ^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a is float32
           because of the definition of t_float32_id at line 2, characters 0-37.
         But the layout of 'a must overlap with value
           because it instantiates an unannotated type parameter of t5_11,
           defaulted to layout value.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}];;

type ('a : float32) t5_12 = {x : 'a; y : float32#};;
[%%expect{|
type ('a : float32) t5_12 = { x : 'a; y : float32#; }
|}];;

type ('a : float32) t5_13 = {x : 'a; y : float32#};;
[%%expect{|
type ('a : float32) t5_13 = { x : 'a; y : float32#; }
|}];;

type 'a t5_14 = {x : 'a; y : float32#};;
[%%expect{|
type 'a t5_14 = { x : 'a; y : float32#; }
|}];;

type ufref = { mutable contents : float32# };;
[%%expect{|
type ufref = { mutable contents : float32#; }
|}];;

(****************************************************)
(* Test 6: Can't be put at top level of signatures. *)
module type S6_1 = sig val x : t_float32 end

let f6 (m : (module S6_1)) = let module M6 = (val m) in M6.x;;
[%%expect{|
Line 1, characters 31-40:
1 | module type S6_1 = sig val x : t_float32 end
                                   ^^^^^^^^^
Error: This type signature for x is not a value type.
       The layout of type t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of type t_float32 must be a sublayout of value
         because it's the type of something stored in a module structure.
|}];;

module type S6_2 = sig val x : 'a t_float32_id end
[%%expect{|
Line 1, characters 31-46:
1 | module type S6_2 = sig val x : 'a t_float32_id end
                                   ^^^^^^^^^^^^^^^
Error: This type signature for x is not a value type.
       The layout of type 'a t_float32_id is float32
         because of the definition of t_float32_id at line 2, characters 0-37.
       But the layout of type 'a t_float32_id must be a sublayout of value
         because it's the type of something stored in a module structure.
|}];;

module type S6_3 = sig val x : float32# end
[%%expect{|
Line 1, characters 31-39:
1 | module type S6_3 = sig val x : float32# end
                                   ^^^^^^^^
Error: This type signature for x is not a value type.
       The layout of type float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of type float32# must be a sublayout of value
         because it's the type of something stored in a module structure.
|}];;


(*********************************************************)
(* Test 7: Can't be used as polymorphic variant argument *)
let f7_1 (x : t_float32) = `A x;;
[%%expect{|
Line 1, characters 30-31:
1 | let f7_1 (x : t_float32) = `A x;;
                                  ^
Error: This expression has type t_float32
       but an expression was expected of type ('a : value)
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}];;

let f7_2 (x : 'a t_float32_id) = `A x;;
[%%expect{|
Line 1, characters 36-37:
1 | let f7_2 (x : 'a t_float32_id) = `A x;;
                                        ^
Error: This expression has type 'a t_float32_id = ('a : float32)
       but an expression was expected of type ('b : value)
       The layout of 'a t_float32_id is float32
         because of the definition of t_float32_id at line 2, characters 0-37.
       But the layout of 'a t_float32_id must overlap with value
         because it's the type of the field of a polymorphic variant.
|}];;

let f7_3 (x : float32#) = `A x;;
[%%expect{|
Line 1, characters 29-30:
1 | let f7_3 (x : float32#) = `A x;;
                                 ^
Error: This expression has type float32#
       but an expression was expected of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}];;

type f7_4 = [ `A of t_float32 ];;
[%%expect{|
Line 1, characters 20-29:
1 | type f7_4 = [ `A of t_float32 ];;
                        ^^^^^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}];;

type ('a : float32) f7_5 = [ `A of 'a ];;
[%%expect{|
Line 1, characters 35-37:
1 | type ('a : float32) f7_5 = [ `A of 'a ];;
                                       ^^
Error: This type ('a : value) should be an instance of type ('a0 : float32)
       The layout of 'a is float32
         because of the annotation on 'a in the declaration of the type f7_5.
       But the layout of 'a must overlap with value
         because it's the type of the field of a polymorphic variant.
|}];;

(************************************************************)
(* Test 8: Normal polymorphic functions don't work on them. *)

let make_t_float32 () : t_float32 = assert false
let make_t_float32_id () : 'a t_float32_id = assert false
let make_floatu () : float32# = assert false

let id_value x = x;;
[%%expect{|
val make_t_float32 : unit -> t_float32 = <fun>
val make_t_float32_id : ('a : float32). unit -> 'a t_float32_id = <fun>
val make_floatu : unit -> float32# = <fun>
val id_value : 'a -> 'a = <fun>
|}];;

let x8_1 = id_value (make_t_float32 ());;
[%%expect{|
Line 1, characters 20-39:
1 | let x8_1 = id_value (make_t_float32 ());;
                        ^^^^^^^^^^^^^^^^^^^
Error: This expression has type t_float32
       but an expression was expected of type ('a : value)
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because of the definition of id_value at line 5, characters 13-18.
|}];;

let x8_2 = id_value (make_t_float32_id ());;
[%%expect{|
Line 1, characters 20-42:
1 | let x8_2 = id_value (make_t_float32_id ());;
                        ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type 'a t_float32_id = ('a : float32)
       but an expression was expected of type ('b : value)
       The layout of 'a t_float32_id is float32
         because of the definition of make_t_float32_id at line 2, characters 22-57.
       But the layout of 'a t_float32_id must overlap with value
         because of the definition of id_value at line 5, characters 13-18.
|}];;

let x8_3 = id_value (make_floatu ());;
[%%expect{|
Line 1, characters 20-36:
1 | let x8_3 = id_value (make_floatu ());;
                        ^^^^^^^^^^^^^^^^
Error: This expression has type float32#
       but an expression was expected of type ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because of the definition of id_value at line 5, characters 13-18.
|}];;

(*************************************)
(* Test 9: But float32 functions do. *)

let twice f (x : 'a t_float32_id) = f (f x)

let f9_1 () = twice f1_1 (make_t_float32 ())
let f9_2 () = twice f1_2 (make_t_float32_id ())
let f9_3 () = twice f1_3 (make_floatu ());;
[%%expect{|
val twice :
  ('a : float32).
    ('a t_float32_id -> 'a t_float32_id) ->
    'a t_float32_id -> 'a t_float32_id =
  <fun>
val f9_1 : unit -> t_float32 t_float32_id = <fun>
val f9_2 : ('a : float32). unit -> 'a t_float32_id = <fun>
val f9_3 : unit -> float32# t_float32_id = <fun>
|}];;

(**************************************************)
(* Test 10: Invalid uses of float32 and externals *)

(* Valid uses of float32 in externals are tested elsewhere - this is just a test
   for uses the typechecker should reject.  In particular
   - if using a non-value layout in an external, you must supply separate
     bytecode and native code implementations,
   - if using a non-value layout in an external, you may not use the old-style
     unboxed float directive, and
   - [@unboxed] is allowed on unboxed types but has no effect. Same is not
     true for [@untagged].
*)

external f10_1 : int -> bool -> float32# = "foo";;
[%%expect{|
Line 1, characters 0-48:
1 | external f10_1 : int -> bool -> float32# = "foo";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}];;

external f10_2 : t_float32 -> int = "foo";;
[%%expect{|
Line 1, characters 0-41:
1 | external f10_2 : t_float32 -> int = "foo";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}];;

external f10_3 : float32 -> t_float32  = "foo" "bar" "float";;
[%%expect{|
Line 1, characters 0-60:
1 | external f10_3 : float32 -> t_float32  = "foo" "bar" "float";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot use "float" in conjunction with types of non-value layouts.
|}];;

external f10_4 : int -> float32# -> float32  = "foo" "bar" "float";;
[%%expect{|
Line 1, characters 0-66:
1 | external f10_4 : int -> float32# -> float32  = "foo" "bar" "float";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot use "float" in conjunction with types of non-value layouts.
|}];;

external f10_5 : float32# -> bool -> string  = "foo" "bar" "float";;
[%%expect{|
Line 1, characters 0-66:
1 | external f10_5 : float32# -> bool -> string  = "foo" "bar" "float";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot use "float" in conjunction with types of non-value layouts.
|}];;

external f10_6 : (float32#[@unboxed]) -> bool -> string  = "foo" "bar";;
[%%expect{|
external f10_6 : float32# -> bool -> string = "foo" "bar"
|}];;

external f10_7 : string -> (float32#[@unboxed])  = "foo" "bar";;
[%%expect{|
external f10_7 : string -> float32# = "foo" "bar"
|}];;

external f10_8 : float -> float32#  = "foo" "bar" [@@unboxed];;
[%%expect{|
external f10_8 : (float [@unboxed]) -> float32# = "foo" "bar"
|}];;

external f10_9 : (float32#[@untagged]) -> bool -> string  = "foo" "bar";;
[%%expect{|
Line 1, characters 18-26:
1 | external f10_9 : (float32#[@untagged]) -> bool -> string  = "foo" "bar";;
                      ^^^^^^^^
Error: Don't know how to untag this type. Only int can be untagged.
|}];;

external f10_10 : string -> (float32#[@untagged])  = "foo" "bar";;
[%%expect{|
Line 1, characters 29-37:
1 | external f10_10 : string -> (float32#[@untagged])  = "foo" "bar";;
                                 ^^^^^^^^
Error: Don't know how to untag this type. Only int can be untagged.
|}];;

(**************************************************)
(* Test 11: float32 banned in extensible variants *)

type t11_1 = ..

type t11_1 += A of t_float32;;
[%%expect{|
type t11_1 = ..
Line 3, characters 14-28:
3 | type t11_1 += A of t_float32;;
                  ^^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type. Consider wrapping the unboxed fields in a record.
|}]

type t11_1 += B of float32#;;
[%%expect{|
Line 1, characters 14-27:
1 | type t11_1 += B of float32#;;
                  ^^^^^^^^^^^^^
Error: Extensible types can't have fields of unboxed type. Consider wrapping the unboxed fields in a record.
|}]

type ('a : float32) t11_2 = ..

type 'a t11_2 += A of int

type 'a t11_2 += B of 'a;;

[%%expect{|
type ('a : float32) t11_2 = ..
type 'a t11_2 += A of int
Line 5, characters 17-24:
5 | type 'a t11_2 += B of 'a;;
                     ^^^^^^^
Error: Extensible types can't have fields of unboxed type. Consider wrapping the unboxed fields in a record.
|}]

type t11_1 += C of t_float32 * string;;

[%%expect{|
Line 1, characters 14-37:
1 | type t11_1 += C of t_float32 * string;;
                  ^^^^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       t_float32, but found boxed argument, string.
|}]

(***************************************)
(* Test 12: float32 in objects/classes *)

(* First, disallowed uses: in object types, class parameters, etc. *)
type t12_1 = < x : t_float32 >;;
[%%expect{|
Line 1, characters 15-28:
1 | type t12_1 = < x : t_float32 >;;
                   ^^^^^^^^^^^^^
Error: Object field types must have layout value.
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because it's the type of an object field.
|}];;

type ('a : float32) t12_2 = < x : 'a >;;
[%%expect{|
Line 1, characters 34-36:
1 | type ('a : float32) t12_2 = < x : 'a >;;
                                      ^^
Error: This type ('a : value) should be an instance of type ('a0 : float32)
       The layout of 'a is float32
         because of the annotation on 'a in the declaration of the type t12_2.
       But the layout of 'a must overlap with value
         because it's the type of an object field.
|}]

class c12_3 = object method x : t_float32 = assert false end;;
[%%expect{|
Line 1, characters 21-56:
1 | class c12_3 = object method x : t_float32 = assert false end;;
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The method x has type t_float32 but is expected to have type
         ('a : value)
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because it's the type of an object field.
|}];;

class ['a] c12_4 = object
  method x : 'a t_float32_id -> 'a t_float32_id = assert false
end;;
[%%expect{|
Line 2, characters 13-15:
2 |   method x : 'a t_float32_id -> 'a t_float32_id = assert false
                 ^^
Error: This type ('a : float32) should be an instance of type ('a0 : value)
       The layout of 'a is value
         because it's a type argument to a class constructor.
       But the layout of 'a must overlap with float32
         because of the definition of t_float32_id at line 2, characters 0-37.
|}];;

class c12_5 = object val x : t_float32 = assert false end;;
[%%expect{|
Line 1, characters 25-26:
1 | class c12_5 = object val x : t_float32 = assert false end;;
                             ^
Error: Variables bound in a class must have layout value.
       The layout of x is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of x must be a sublayout of value
         because it's the type of a class field.
|}];;

class type c12_6 = object method x : float32# end;;
[%%expect{|
Line 1, characters 26-45:
1 | class type c12_6 = object method x : float32# end;;
                              ^^^^^^^^^^^^^^^^^^^
Error: The method x has type float32# but is expected to have type
         ('a : value)
       The layout of float32# is float32
         because it is the primitive float32 type float32#.
       But the layout of float32# must be a sublayout of value
         because it's the type of an object field.
|}];;

class type c12_7 = object val x : float32# end
[%%expect{|
Line 1, characters 26-42:
1 | class type c12_7 = object val x : float32# end
                              ^^^^^^^^^^^^^^^^
Error: Variables bound in a class must have layout value.
       The layout of x is float32
         because it is the primitive float32 type float32#.
       But the layout of x must be a sublayout of value
         because it's the type of an instance variable.
|}];;

class type ['a] c12_8 = object
  val x : 'a t_float32_id -> 'a t_float32_id
end
[%%expect{|
Line 2, characters 10-12:
2 |   val x : 'a t_float32_id -> 'a t_float32_id
              ^^
Error: This type ('a : float32) should be an instance of type ('a0 : value)
       The layout of 'a is value
         because it's a type argument to a class constructor.
       But the layout of 'a must overlap with float32
         because of the definition of t_float32_id at line 2, characters 0-37.
|}];;

(* Second, allowed uses: as method parameters / returns *)
type t12_8 = < f : t_float32 -> t_float32 >
let f12_9 (o : t12_8) x = o#f x
let f12_10 o (y : t_float32) : t_float32 = o#baz y y y;;
class ['a] c12_11 = object
  method x : t_float32 -> 'a = assert false
end;;
class ['a] c12_12 = object
  method x : 'a -> t_float32 = assert false
end;;
[%%expect{|
type t12_8 = < f : t_float32 -> t_float32 >
val f12_9 : t12_8 -> t_float32 -> t_float32 = <fun>
val f12_10 :
  < baz : t_float32 -> t_float32 -> t_float32 -> t_float32; .. > ->
  t_float32 -> t_float32 = <fun>
class ['a] c12_11 : object method x : t_float32 -> 'a end
class ['a] c12_12 : object method x : 'a -> t_float32 end
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
       but an expression was expected of type t_float32
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because it's the type of a variable captured in an object.
|}];;

let f12_14 (m1 : t_float32) (m2 : t_float32) = object
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
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because it's the type of a variable captured in an object.
|}];;

(*********************************************************************)
(* Test 13: Ad-hoc polymorphic operations don't work on float32 yet. *)

(* CR layouts v5: Remember to handle the case of calling these on structures
   containing other layouts. *)

let f13_1 (x : t_float32) = x = x;;
[%%expect{|
Line 1, characters 28-29:
1 | let f13_1 (x : t_float32) = x = x;;
                                ^
Error: This expression has type t_float32
       but an expression was expected of type ('a : value)
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because of layout requirements from an imported definition.
|}];;

let f13_2 (x : t_float32) = compare x x;;
[%%expect{|
Line 1, characters 36-37:
1 | let f13_2 (x : t_float32) = compare x x;;
                                        ^
Error: This expression has type t_float32
       but an expression was expected of type ('a : value)
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because of layout requirements from an imported definition.
|}];;

let f13_3 (x : t_float32) = Marshal.to_bytes x;;
[%%expect{|
Line 1, characters 45-46:
1 | let f13_3 (x : t_float32) = Marshal.to_bytes x;;
                                                 ^
Error: This expression has type t_float32
       but an expression was expected of type ('a : value)
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because of layout requirements from an imported definition.
|}];;

let f13_4 (x : t_float32) = Hashtbl.hash x;;
[%%expect{|
Line 1, characters 41-42:
1 | let f13_4 (x : t_float32) = Hashtbl.hash x;;
                                             ^
Error: This expression has type t_float32
       but an expression was expected of type ('a : value)
       The layout of t_float32 is float32
         because of the definition of t_float32 at line 1, characters 0-24.
       But the layout of t_float32 must be a sublayout of value
         because of layout requirements from an imported definition.
|}];;

(***********************************************************)
(* Test 14: unboxed float32 records work like normal records *)

module FU = struct

  external of_float32 : (float32[@local_opt]) -> float32# = "%unbox_float32"

  external to_float32 : float32# -> (float32[@local_opt]) = "%box_float32"

  external sub :
  (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
  = "%subfloat32"

  external add :
  (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
  = "%addfloat32"

  let[@inline always] sub x y = of_float32 (sub (to_float32 x) (to_float32 y))

  let[@inline always] add x y = of_float32 (add (to_float32 x) (to_float32 y))
end

type t14_1 = { x : float32#; y : float32# }

(* pattern matching *)
let f14_1 {x;y} = FU.sub x y

(* construction *)
let r14 = { x = #3.14s; y = #2.72s }

let sum14_1 = FU.to_float32 (f14_1 r14)

(* projection *)
let f14_2 ({y;_} as r) = FU.sub r.x y

let sum14_2 = FU.to_float32 (f14_1 r14)

type t14_2 = { mutable a : float32#; b : float32#; mutable c : float32# }

let f14_3 ({b; c; _} as r) =
  (* pure record update *)
  let r' = { r with b = #20.0s; c = r.a } in
  (* mutation *)
  r.a <- FU.sub r.a r'.b;
  r'.a <- #42.0s;
  r'

let a, b, c, a', b', c' =
  let r = {a = #3.1s; b = -#0.42s; c = #27.7s } in
  let r' = f14_3 r in
  FU.to_float32 r.a,
  FU.to_float32 r.b,
  FU.to_float32 r.c,
  FU.to_float32 r'.a,
  FU.to_float32 r'.b,
  FU.to_float32 r'.c

let f14_4 r =
  let {x; y} = r in
  FU.add x y


[%%expect{|
module FU :
  sig
    external of_float32 : (float32 [@local_opt]) -> float32#
      = "%unbox_float32"
    external to_float32 : float32# -> (float32 [@local_opt]) = "%box_float32"
    val sub : float32# -> float32# -> float32#
    val add : float32# -> float32# -> float32#
  end
type t14_1 = { x : float32#; y : float32#; }
val f14_1 : t14_1 -> float32# = <fun>
val r14 : t14_1 = {x = <abstr>; y = <abstr>}
val sum14_1 : float32 = 0.420000076s
val f14_2 : t14_1 -> float32# = <fun>
val sum14_2 : float32 = 0.420000076s
type t14_2 = { mutable a : float32#; b : float32#; mutable c : float32#; }
val f14_3 : t14_2 -> t14_2 = <fun>
val a : float32 = -16.8999996s
val b : float32 = -0.419999987s
val c : float32 = 27.7000008s
val a' : float32 = 42.s
val b' : float32 = 20.s
val c' : float32 = 3.0999999s
val f14_4 : t14_1 -> float32# = <fun>
|}]
