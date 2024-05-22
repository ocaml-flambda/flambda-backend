(* TEST
 include stable;
 flambda2;
 {
   flags = "-extension layouts_alpha";
   expect;
 }{
   flags = "-extension layouts_beta";
   expect;
 }
*)

(* This test is almost entirely a copy of [basics.ml], except
   with different output for some layouts-beta features.
   You should diff this file against [basics.ml] to see what's
   different.
*)

(* This file contains typing tests for the layout [float64].

   Runtime tests for the type [float#] can be found in the [unboxed_float] and
   [alloc] tests in this directory.  The type [float#] here is used as a
   convenient example of a concrete [float64] type in some tests, but its
   behavior isn't the primary purpose of this test. *)

type t_float64 : float64
type ('a : float64) t_float64_id = 'a

(*********************************)
(* Test 1: The identity function *)

let f1_1 (x : t_float64) = x;;
let f1_2 (x : 'a t_float64_id) = x;;
let f1_3 (x : float#) = x;;
[%%expect{|
type t_float64 : float64
type ('a : float64) t_float64_id = 'a
val f1_1 : t_float64 -> t_float64 = <fun>
val f1_2 : ('a : float64). 'a t_float64_id -> 'a t_float64_id = <fun>
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
val f2_2 : ('a : float64). 'a t_float64_id -> 'a t_float64_id = <fun>
val f2_3 : float# -> float# = <fun>
|}];;

(*****************************************)
(* Test 3: No module-level bindings yet. *)

let x3_1 : t_float64 = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_1 : t_float64 = assert false;;
        ^^^^
Error: Types of top-level module bindings must have layout value, but
       the type of x3_1 has layout float64.
|}];;

let x3_2 : 'a t_float64_id = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_2 : 'a t_float64_id = assert false;;
        ^^^^
Error: Types of top-level module bindings must have layout value, but
       the type of x3_2 has layout float64.
|}];;

let x3_3 : float# = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_3 : float# = assert false;;
        ^^^^
Error: Types of top-level module bindings must have layout value, but
       the type of x3_3 has layout float64.
|}];;

module M3_4 = struct
  let x : t_float64 = assert false
end
[%%expect{|
Line 2, characters 6-7:
2 |   let x : t_float64 = assert false
          ^
Error: Types of top-level module bindings must have layout value, but
       the type of x has layout float64.
|}];;

module M3_5 = struct
  let f (x : float#) = x

  let y = f (assert false)
end
[%%expect{|
Line 4, characters 6-7:
4 |   let y = f (assert false)
          ^
Error: Types of top-level module bindings must have layout value, but
       the type of y has layout float64.
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
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of a tuple element.
|}];;

let f4_2 (x : 'a t_float64_id) = x, false;;
[%%expect{|
Line 1, characters 33-34:
1 | let f4_2 (x : 'a t_float64_id) = x, false;;
                                     ^
Error: This expression has type 'a t_float64_id = ('a : float64)
       but an expression was expected of type ('b : value)
       The layout of 'a t_float64_id is float64, because
         of the definition of t_float64_id at line 2, characters 0-37.
       But the layout of 'a t_float64_id must overlap with value, because
         it's the type of a tuple element.
|}];;

let f4_3 (x : float#) = x, false;;
[%%expect{|
Line 1, characters 24-25:
1 | let f4_3 (x : float#) = x, false;;
                            ^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       The layout of float# is float64, because
         it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value, because
         it's the type of a tuple element.
|}];;

type t4_4 = t_float64 * string;;
[%%expect{|
Line 1, characters 12-21:
1 | type t4_4 = t_float64 * string;;
                ^^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of a tuple element.
|}];;

type t4_5 = int * float#;;
[%%expect{|
Line 1, characters 18-24:
1 | type t4_5 = int * float#;;
                      ^^^^^^
Error: Tuple element types must have layout value.
       The layout of float# is float64, because
         it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value, because
         it's the type of a tuple element.
|}];;

type ('a : float64) t4_6 = 'a * 'a
[%%expect{|
Line 1, characters 27-29:
1 | type ('a : float64) t4_6 = 'a * 'a
                               ^^
Error: This type ('a : value) should be an instance of type ('a0 : float64)
       The layout of 'a is float64, because
         of the annotation on 'a in the declaration of the type t4_6.
       But the layout of 'a must overlap with value, because
         it's the type of a tuple element.
|}];;

(* check for layout propagation *)
type ('a : float64, 'b) t4_7 = ('a as 'b) -> ('b * 'b);;
[%%expect{|
Line 1, characters 32-34:
1 | type ('a : float64, 'b) t4_7 = ('a as 'b) -> ('b * 'b);;
                                    ^^
Error: This type ('b : value) should be an instance of type ('a : float64)
       The layout of 'a is float64, because
         of the annotation on 'a in the declaration of the type t4_7.
       But the layout of 'a must overlap with value, because
         it's the type of a tuple element.
|}]

(*****************************************)
(* Test 5: Can be put in some structures *)

(* all-float64 records are allowed, as are some records that mix float64 and
   value fields. See [tests/typing-layouts/mixed_records.ml] for tests of mixed
   records. *)
type t5_1 = { x : t_float64 };;
[%%expect{|
type t5_1 = { x : t_float64; }
|}];;

(* CR layouts 2.5: allow this *)
type t5_3 = { x : t_float64 } [@@unboxed];;
[%%expect{|
Line 1, characters 14-27:
1 | type t5_3 = { x : t_float64 } [@@unboxed];;
                  ^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Unboxed records may not yet contain types of this layout.
|}];;

(* all-float64 constructor args are also allowed, as are some constructors that
   mix float64 and value fields. These are only allowed in alpha, though. See
   [tests/typing-layouts/mixed_constructor_args.ml] for tests of mixed
   constructor args. *)
type t5_4 = A of t_float64;;
[%%expect{|
type t5_4 = A of t_float64
|}];;

type t5_5 = A of int * t_float64;;
[%%expect{|
type t5_5 = A of int * t_float64
|}];;

type t5_6 = A of t_float64 [@@unboxed];;
[%%expect{|
Line 1, characters 12-26:
1 | type t5_6 = A of t_float64 [@@unboxed];;
                ^^^^^^^^^^^^^^
Error: Type t_float64 has layout float64.
       Unboxed variants may not yet contain types of this layout.
|}];;

type ('a : float64) t5_7 = A of int
type ('a : float64) t5_8 = A of 'a;;
[%%expect{|
type ('a : float64) t5_7 = A of int
type ('a : float64) t5_8 = A of 'a
|}]

type ('a : float64, 'b : float64) t5_9 = {x : 'a; y : 'b; z : 'a}

type 'a t5_10 = 'a t_float64_id
and 'a t5_11 = {x : 'a t5_10; y : 'a}
[%%expect{|
type ('a : float64, 'b : float64) t5_9 = { x : 'a; y : 'b; z : 'a; }
Line 4, characters 20-28:
4 | and 'a t5_11 = {x : 'a t5_10; y : 'a}
                        ^^^^^^^^
Error: Layout mismatch in final type declaration consistency check.
       This is most often caused by the fact that type inference is not
       clever enough to propagate layouts through variables in different
       declarations. It is also not clever enough to produce a good error
       message, so we'll say this instead:
         The layout of 'a is float64, because
           of the definition of t_float64_id at line 2, characters 0-37.
         But the layout of 'a must overlap with value, because
           it instantiates an unannotated type parameter of t5_11, defaulted to layout value.
       A good next step is to add a layout annotation on a parameter to
       the declaration where this error is reported.
|}];;

type ('a : float64) t5_12 = {x : 'a; y : float#};;
[%%expect{|
type ('a : float64) t5_12 = { x : 'a; y : float#; }
|}];;

type ('a : float64) t5_13 = {x : 'a; y : float#};;
[%%expect{|
type ('a : float64) t5_13 = { x : 'a; y : float#; }
|}];;

type ufref = { mutable contents : float# };;
[%%expect{|
type ufref = { mutable contents : float#; }
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
       The layout of type t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of type t_float64 must be a sublayout of value, because
         it's the type of something stored in a module structure.
|}];;

module type S6_2 = sig val x : 'a t_float64_id end
[%%expect{|
Line 1, characters 31-46:
1 | module type S6_2 = sig val x : 'a t_float64_id end
                                   ^^^^^^^^^^^^^^^
Error: This type signature for x is not a value type.
       The layout of type 'a t_float64_id is float64, because
         of the definition of t_float64_id at line 2, characters 0-37.
       But the layout of type 'a t_float64_id must be a sublayout of value, because
         it's the type of something stored in a module structure.
|}];;

module type S6_3 = sig val x : float# end
[%%expect{|
Line 1, characters 31-37:
1 | module type S6_3 = sig val x : float# end
                                   ^^^^^^
Error: This type signature for x is not a value type.
       The layout of type float# is float64, because
         it is the primitive float64 type float#.
       But the layout of type float# must be a sublayout of value, because
         it's the type of something stored in a module structure.
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
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of the field of a polymorphic variant.
|}];;

let f7_2 (x : 'a t_float64_id) = `A x;;
[%%expect{|
Line 1, characters 36-37:
1 | let f7_2 (x : 'a t_float64_id) = `A x;;
                                        ^
Error: This expression has type 'a t_float64_id = ('a : float64)
       but an expression was expected of type ('b : value)
       The layout of 'a t_float64_id is float64, because
         of the definition of t_float64_id at line 2, characters 0-37.
       But the layout of 'a t_float64_id must overlap with value, because
         it's the type of the field of a polymorphic variant.
|}];;

let f7_3 (x : float#) = `A x;;
[%%expect{|
Line 1, characters 27-28:
1 | let f7_3 (x : float#) = `A x;;
                               ^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       The layout of float# is float64, because
         it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value, because
         it's the type of the field of a polymorphic variant.
|}];;

type f7_4 = [ `A of t_float64 ];;
[%%expect{|
Line 1, characters 20-29:
1 | type f7_4 = [ `A of t_float64 ];;
                        ^^^^^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of the field of a polymorphic variant.
|}];;

type ('a : float64) f7_5 = [ `A of 'a ];;
[%%expect{|
Line 1, characters 35-37:
1 | type ('a : float64) f7_5 = [ `A of 'a ];;
                                       ^^
Error: This type ('a : value) should be an instance of type ('a0 : float64)
       The layout of 'a is float64, because
         of the annotation on 'a in the declaration of the type f7_5.
       But the layout of 'a must overlap with value, because
         it's the type of the field of a polymorphic variant.
|}];;

(************************************************************)
(* Test 8: Normal polymorphic functions don't work on them. *)

let make_t_float64 () : t_float64 = assert false
let make_t_float64_id () : 'a t_float64_id = assert false
let make_floatu () : float# = assert false

let id_value x = x;;
[%%expect{|
val make_t_float64 : unit -> t_float64 = <fun>
val make_t_float64_id : ('a : float64). unit -> 'a t_float64_id = <fun>
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
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         of the definition of id_value at line 5, characters 13-18.
|}];;

let x8_2 = id_value (make_t_float64_id ());;
[%%expect{|
Line 1, characters 20-42:
1 | let x8_2 = id_value (make_t_float64_id ());;
                        ^^^^^^^^^^^^^^^^^^^^^^
Error: This expression has type 'a t_float64_id = ('a : float64)
       but an expression was expected of type ('b : value)
       The layout of 'a t_float64_id is float64, because
         of the definition of make_t_float64_id at line 2, characters 22-57.
       But the layout of 'a t_float64_id must overlap with value, because
         of the definition of id_value at line 5, characters 13-18.
|}];;

let x8_3 = id_value (make_floatu ());;
[%%expect{|
Line 1, characters 20-36:
1 | let x8_3 = id_value (make_floatu ());;
                        ^^^^^^^^^^^^^^^^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       The layout of float# is float64, because
         it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value, because
         of the definition of id_value at line 5, characters 13-18.
|}];;

(*************************************)
(* Test 9: But float64 functions do. *)

let twice f (x : 'a t_float64_id) = f (f x)

let f9_1 () = twice f1_1 (make_t_float64 ())
let f9_2 () = twice f1_2 (make_t_float64_id ())
let f9_3 () = twice f1_3 (make_floatu ());;
[%%expect{|
val twice :
  ('a : float64).
    ('a t_float64_id -> 'a t_float64_id) ->
    'a t_float64_id -> 'a t_float64_id =
  <fun>
val f9_1 : unit -> t_float64 t_float64_id = <fun>
val f9_2 : ('a : float64). unit -> 'a t_float64_id = <fun>
val f9_3 : unit -> float# t_float64_id = <fun>
|}];;

(**************************************************)
(* Test 10: Invalid uses of float64 and externals *)

(* Valid uses of float64 in externals are tested elsewhere - this is just a test
   for uses the typechecker should reject.  In particular
   - if using a non-value layout in an external, you must supply separate
     bytecode and native code implementations,
   - if using a non-value layout in an external, you may not use the old-style
     unboxed float directive, and
   - [@unboxed] is allowed on unboxed types but has no effect. Same is not
     true for [@untagged].
*)

external f10_1 : int -> bool -> float# = "foo";;
[%%expect{|
Line 1, characters 0-46:
1 | external f10_1 : int -> bool -> float# = "foo";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}];;

external f10_2 : t_float64 -> int = "foo";;
[%%expect{|
Line 1, characters 0-41:
1 | external f10_2 : t_float64 -> int = "foo";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}];;

external f10_3 : float -> t_float64  = "foo" "bar" "float";;
[%%expect{|
Line 1, characters 0-58:
1 | external f10_3 : float -> t_float64  = "foo" "bar" "float";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot use "float" in conjunction with types of non-value layouts.
|}];;

external f10_4 : int -> float# -> float  = "foo" "bar" "float";;
[%%expect{|
Line 1, characters 0-62:
1 | external f10_4 : int -> float# -> float  = "foo" "bar" "float";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot use "float" in conjunction with types of non-value layouts.
|}];;

external f10_5 : float# -> bool -> string  = "foo" "bar" "float";;
[%%expect{|
Line 1, characters 0-64:
1 | external f10_5 : float# -> bool -> string  = "foo" "bar" "float";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Cannot use "float" in conjunction with types of non-value layouts.
|}];;

external f10_6 : (float#[@unboxed]) -> bool -> string  = "foo" "bar";;
[%%expect{|
external f10_6 : float# -> bool -> string = "foo" "bar"
|}];;

external f10_7 : string -> (float#[@unboxed])  = "foo" "bar";;
[%%expect{|
external f10_7 : string -> float# = "foo" "bar"
|}];;

external f10_8 : float -> float#  = "foo" "bar" [@@unboxed];;
[%%expect{|
external f10_8 : (float [@unboxed]) -> float# = "foo" "bar"
|}];;

external f10_9 : (float#[@untagged]) -> bool -> string  = "foo" "bar";;
[%%expect{|
Line 1, characters 18-24:
1 | external f10_9 : (float#[@untagged]) -> bool -> string  = "foo" "bar";;
                      ^^^^^^
Error: Don't know how to untag this type. Only int can be untagged.
|}];;

external f10_10 : string -> (float#[@untagged])  = "foo" "bar";;
[%%expect{|
Line 1, characters 29-35:
1 | external f10_10 : string -> (float#[@untagged])  = "foo" "bar";;
                                 ^^^^^^
Error: Don't know how to untag this type. Only int can be untagged.
|}];;

(******************************************************)
(* Test 11: Allow float64 in some extensible variants *)

(* Currently these are only supported in alpha *)

type t11_1 = ..

type t11_1 += A of t_float64;;
[%%expect{|
type t11_1 = ..
type t11_1 += A of t_float64
|}]

type t11_1 += B of float#;;
[%%expect{|
type t11_1 += B of float#
|}]

type ('a : float64) t11_2 = ..

type 'a t11_2 += A of int

type 'a t11_2 += B of 'a;;

[%%expect{|
type ('a : float64) t11_2 = ..
type 'a t11_2 += A of int
type 'a t11_2 += B of 'a
|}]

(* Some extensible variants aren't supported, though. *)

type t11_1 += C of t_float64 * string;;

[%%expect{|
Line 1, characters 14-37:
1 | type t11_1 += C of t_float64 * string;;
                  ^^^^^^^^^^^^^^^^^^^^^^^
Error: Expected all flat constructor arguments after non-value argument,
       t_float64, but found boxed argument, string.
|}]

(***************************************)
(* Test 12: float64 in objects/classes *)

(* First, disallowed uses: in object types, class parameters, etc. *)
type t12_1 = < x : t_float64 >;;
[%%expect{|
Line 1, characters 15-28:
1 | type t12_1 = < x : t_float64 >;;
                   ^^^^^^^^^^^^^
Error: Object field types must have layout value.
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of an object field.
|}];;

type ('a : float64) t12_2 = < x : 'a >;;
[%%expect{|
Line 1, characters 34-36:
1 | type ('a : float64) t12_2 = < x : 'a >;;
                                      ^^
Error: This type ('a : value) should be an instance of type ('a0 : float64)
       The layout of 'a is float64, because
         of the annotation on 'a in the declaration of the type t12_2.
       But the layout of 'a must overlap with value, because
         it's the type of an object field.
|}]

class c12_3 = object method x : t_float64 = assert false end;;
[%%expect{|
Line 1, characters 21-56:
1 | class c12_3 = object method x : t_float64 = assert false end;;
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The method x has type t_float64 but is expected to have type
         ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of an object field.
|}];;

class ['a] c12_4 = object
  method x : 'a t_float64_id -> 'a t_float64_id = assert false
end;;
[%%expect{|
Line 2, characters 13-15:
2 |   method x : 'a t_float64_id -> 'a t_float64_id = assert false
                 ^^
Error: This type ('a : float64) should be an instance of type ('a0 : value)
       The layout of 'a is value, because
         it's a type argument to a class constructor.
       But the layout of 'a must overlap with float64, because
         of the definition of t_float64_id at line 2, characters 0-37.
|}];;

class c12_5 = object val x : t_float64 = assert false end;;
[%%expect{|
Line 1, characters 25-26:
1 | class c12_5 = object val x : t_float64 = assert false end;;
                             ^
Error: Variables bound in a class must have layout value.
       The layout of x is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of x must be a sublayout of value, because
         it's the type of a class field.
|}];;

class type c12_6 = object method x : float# end;;
[%%expect{|
Line 1, characters 26-43:
1 | class type c12_6 = object method x : float# end;;
                              ^^^^^^^^^^^^^^^^^
Error: The method x has type float# but is expected to have type ('a : value)
       The layout of float# is float64, because
         it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value, because
         it's the type of an object field.
|}];;

class type c12_7 = object val x : float# end
[%%expect{|
Line 1, characters 26-40:
1 | class type c12_7 = object val x : float# end
                              ^^^^^^^^^^^^^^
Error: Variables bound in a class must have layout value.
       The layout of x is float64, because
         it is the primitive float64 type float#.
       But the layout of x must be a sublayout of value, because
         it's the type of an instance variable.
|}];;

class type ['a] c12_8 = object
  val x : 'a t_float64_id -> 'a t_float64_id
end
[%%expect{|
Line 2, characters 10-12:
2 |   val x : 'a t_float64_id -> 'a t_float64_id
              ^^
Error: This type ('a : float64) should be an instance of type ('a0 : value)
       The layout of 'a is value, because
         it's a type argument to a class constructor.
       But the layout of 'a must overlap with float64, because
         of the definition of t_float64_id at line 2, characters 0-37.
|}];;

(* Second, allowed uses: as method parameters / returns *)
type t12_8 = < f : t_float64 -> t_float64 >
let f12_9 (o : t12_8) x = o#f x
let f12_10 o (y : t_float64) : t_float64 = o#baz y y y;;
class ['a] c12_11 = object
  method x : t_float64 -> 'a = assert false
end;;
class ['a] c12_12 = object
  method x : 'a -> t_float64 = assert false
end;;
[%%expect{|
type t12_8 = < f : t_float64 -> t_float64 >
val f12_9 : t12_8 -> t_float64 -> t_float64 = <fun>
val f12_10 :
  < baz : t_float64 -> t_float64 -> t_float64 -> t_float64; .. > ->
  t_float64 -> t_float64 = <fun>
class ['a] c12_11 : object method x : t_float64 -> 'a end
class ['a] c12_12 : object method x : 'a -> t_float64 end
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
       but an expression was expected of type t_float64
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of a variable captured in an object.
|}];;

let f12_14 (m1 : t_float64) (m2 : t_float64) = object
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
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         it's the type of a variable captured in an object.
|}];;

(*********************************************************************)
(* Test 13: Ad-hoc polymorphic operations don't work on float64 yet. *)

(* CR layouts v5: Remember to handle the case of calling these on structures
   containing other layouts. *)

let f13_1 (x : t_float64) = x = x;;
[%%expect{|
Line 1, characters 28-29:
1 | let f13_1 (x : t_float64) = x = x;;
                                ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         of layout requirements from an imported definition.
|}];;

let f13_2 (x : t_float64) = compare x x;;
[%%expect{|
Line 1, characters 36-37:
1 | let f13_2 (x : t_float64) = compare x x;;
                                        ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         of layout requirements from an imported definition.
|}];;

let f13_3 (x : t_float64) = Marshal.to_bytes x;;
[%%expect{|
Line 1, characters 45-46:
1 | let f13_3 (x : t_float64) = Marshal.to_bytes x;;
                                                 ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         of layout requirements from an imported definition.
|}];;

let f13_4 (x : t_float64) = Hashtbl.hash x;;
[%%expect{|
Line 1, characters 41-42:
1 | let f13_4 (x : t_float64) = Hashtbl.hash x;;
                                             ^
Error: This expression has type t_float64
       but an expression was expected of type ('a : value)
       The layout of t_float64 is float64, because
         of the definition of t_float64 at line 1, characters 0-24.
       But the layout of t_float64 must be a sublayout of value, because
         of layout requirements from an imported definition.
|}];;

(***********************************************************)
(* Test 14: unboxed float records work like normal records *)

module FU = Stable.Float_u

type t14_1 = { x : float#; y : float# }

(* pattern matching *)
let f14_1 {x;y} = FU.sub x y

(* construction *)
let r14 = { x = #3.14; y = #2.72 }

let sum14_1 = FU.to_float (f14_1 r14)

(* projection *)
let f14_2 ({y;_} as r) = FU.sub r.x y

let sum14_2 = FU.to_float (f14_1 r14)

type t14_2 = { mutable a : float#; b : float#; mutable c : float# }

let f14_3 ({b; c; _} as r) =
  (* pure record update *)
  let r' = { r with b = #20.0; c = r.a } in
  (* mutation *)
  r.a <- FU.sub r.a r'.b;
  r'.a <- #42.0;
  r'

let a, b, c, a', b', c' =
  let r = {a = #3.1; b = -#0.42; c = #27.7 } in
  let r' = f14_3 r in
  FU.to_float r.a,
  FU.to_float r.b,
  FU.to_float r.c,
  FU.to_float r'.a,
  FU.to_float r'.b,
  FU.to_float r'.c

let f14_4 r =
  let {x; y} = r in
  FU.add x y


[%%expect{|
module FU = Stable.Float_u
type t14_1 = { x : float#; y : float#; }
val f14_1 : t14_1 -> FU.t = <fun>
val r14 : t14_1 = {x = <abstr>; y = <abstr>}
val sum14_1 : float = 0.419999999999999929
val f14_2 : t14_1 -> FU.t = <fun>
val sum14_2 : float = 0.419999999999999929
type t14_2 = { mutable a : float#; b : float#; mutable c : float#; }
val f14_3 : t14_2 -> t14_2 = <fun>
val a : float = -16.9
val b : float = -0.42
val c : float = 27.7
val a' : float = 42.
val b' : float = 20.
val c' : float = 3.1
val f14_4 : t14_1 -> FU.t = <fun>
|}]
