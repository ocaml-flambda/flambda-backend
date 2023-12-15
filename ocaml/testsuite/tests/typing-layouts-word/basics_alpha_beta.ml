(* TEST
   * expect
   flags = "-extension layouts_alpha"
   * expect
   flags = "-extension layouts_beta"
*)

(* This file contains typing tests for the layout [word].

   Runtime tests for the type [nativeint#] can be found in the
   [unboxed_nativeint], [alloc], and [stdlib__nativeint_u] tests in this
   directory.  The type [nativeint#] here is used as a convenient example of a
   concrete [word] type in some tests, but its behavior isn't the primary
   purpose of this test. *)

type t_word : word
type ('a : word) t_word_id = 'a

(*********************************)
(* Test 1: The identity function *)

let f1_1 (x : t_word) = x;;
let f1_2 (x : 'a t_word_id) = x;;
let f1_3 (x : nativeint#) = x;;
[%%expect{|
type t_word : word
type ('a : word) t_word_id = 'a
val f1_1 : t_word -> t_word = <fun>
val f1_2 : ('a : word). 'a t_word_id -> 'a t_word_id = <fun>
val f1_3 : nativeint# -> nativeint# = <fun>
|}];;

(*****************************************)
(* Test 2: You can let-bind them locally *)
let f2_1 (x : t_word) =
  let y = x in
  y;;

let f2_2 (x : 'a t_word_id) =
  let y = x in
  y;;

let f2_3 (x : nativeint#) =
  let y = x in
  y;;
[%%expect{|
val f2_1 : t_word -> t_word = <fun>
val f2_2 : ('a : word). 'a t_word_id -> 'a t_word_id = <fun>
val f2_3 : nativeint# -> nativeint# = <fun>
|}];;

(*****************************************)
(* Test 3: No module-level bindings yet. *)

let x3_1 : t_word = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_1 : t_word = assert false;;
        ^^^^
Error: Top-level module bindings must have layout value, but x3_1 has layout
       word.
|}];;

let x3_2 : 'a t_word_id = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_2 : 'a t_word_id = assert false;;
        ^^^^
Error: Top-level module bindings must have layout value, but x3_2 has layout
       word.
|}];;

let x3_3 : nativeint# = assert false;;
[%%expect{|
Line 1, characters 4-8:
1 | let x3_3 : nativeint# = assert false;;
        ^^^^
Error: Top-level module bindings must have layout value, but x3_3 has layout
       word.
|}];;

module M3_4 = struct
  let x : t_word = assert false
end
[%%expect{|
Line 2, characters 6-7:
2 |   let x : t_word = assert false
          ^
Error: Top-level module bindings must have layout value, but x has layout
       word.
|}];;

module M3_5 = struct
  let f (x : nativeint#) = x

  let y = f (assert false)
end
[%%expect{|
Line 4, characters 6-7:
4 |   let y = f (assert false)
          ^
Error: Top-level module bindings must have layout value, but y has layout
       word.
|}];;

(*************************************)
(* Test 4: No putting them in tuples *)

let f4_1 (x : t_word) = x, false;;
[%%expect{|
Line 1, characters 24-25:
1 | let f4_1 (x : t_word) = x, false;;
                            ^
Error: This expression has type t_word but an expression was expected of type
         ('a : value)
       t_word has layout word, which is not a sublayout of value.
|}];;

let f4_2 (x : 'a t_word_id) = x, false;;
[%%expect{|
Line 1, characters 30-31:
1 | let f4_2 (x : 'a t_word_id) = x, false;;
                                  ^
Error: This expression has type 'a t_word_id = ('a : word)
       but an expression was expected of type ('b : value)
       'a t_word_id has layout word, which does not overlap with value.
|}];;

let f4_3 (x : nativeint#) = x, false;;
[%%expect{|
Line 1, characters 28-29:
1 | let f4_3 (x : nativeint#) = x, false;;
                                ^
Error: This expression has type nativeint#
       but an expression was expected of type ('a : value)
       nativeint# has layout word, which is not a sublayout of value.
|}];;

type t4_4 = t_word * string;;
[%%expect{|
Line 1, characters 12-18:
1 | type t4_4 = t_word * string;;
                ^^^^^^
Error: Tuple element types must have layout value.
        t_word has layout word, which is not a sublayout of value.
|}];;

type t4_5 = int * nativeint#;;
[%%expect{|
Line 1, characters 18-28:
1 | type t4_5 = int * nativeint#;;
                      ^^^^^^^^^^
Error: Tuple element types must have layout value.
        nativeint# has layout word, which is not a sublayout of value.
|}];;

type ('a : word) t4_6 = 'a * 'a
[%%expect{|
Line 1, characters 24-26:
1 | type ('a : word) t4_6 = 'a * 'a
                            ^^
Error: This type ('a : value) should be an instance of type ('a0 : word)
       'a has layout word, which does not overlap with value.
|}];;

(* check for layout propagation *)
type ('a : word, 'b) t4_7 = ('a as 'b) -> ('b * 'b);;
[%%expect{|
Line 1, characters 29-31:
1 | type ('a : word, 'b) t4_7 = ('a as 'b) -> ('b * 'b);;
                                 ^^
Error: This type ('b : value) should be an instance of type ('a : word)
       'a has layout word, which does not overlap with value.
|}]

(****************************************************)
(* Test 5: Can't be put in structures in typedecls. *)

type t5_1 = { x : t_word };;
[%%expect{|
Line 1, characters 14-24:
1 | type t5_1 = { x : t_word };;
                  ^^^^^^^^^^
Error: Type t_word has layout word.
       Records may not yet contain types of this layout.
|}];;

(* CR layouts v5: this should work *)
type t5_2 = { y : int; x : t_word };;
[%%expect{|
Line 1, characters 23-33:
1 | type t5_2 = { y : int; x : t_word };;
                           ^^^^^^^^^^
Error: Type t_word has layout word.
       Records may not yet contain types of this layout.
|}];;

(* CR layouts: this runs afoul of the mixed block restriction, but should work
   once we relax that. *)
type t5_2' = { y : string; x : t_word };;
[%%expect{|
Line 1, characters 27-37:
1 | type t5_2' = { y : string; x : t_word };;
                               ^^^^^^^^^^
Error: Type t_word has layout word.
       Records may not yet contain types of this layout.
|}];;

(* CR layouts 2.5: allow this *)
type t5_3 = { x : t_word } [@@unboxed];;
[%%expect{|
Line 1, characters 14-24:
1 | type t5_3 = { x : t_word } [@@unboxed];;
                  ^^^^^^^^^^
Error: Type t_word has layout word.
       Unboxed records may not yet contain types of this layout.
|}];;

type t5_4 = A of t_word;;
[%%expect{|
Line 1, characters 12-23:
1 | type t5_4 = A of t_word;;
                ^^^^^^^^^^^
Error: Type t_word has layout word.
       Variants may not yet contain types of this layout.
|}];;

type t5_5 = A of int * t_word;;
[%%expect{|
Line 1, characters 12-29:
1 | type t5_5 = A of int * t_word;;
                ^^^^^^^^^^^^^^^^^
Error: Type t_word has layout word.
       Variants may not yet contain types of this layout.
|}];;

type t5_6 = A of t_word [@@unboxed];;
[%%expect{|
Line 1, characters 12-23:
1 | type t5_6 = A of t_word [@@unboxed];;
                ^^^^^^^^^^^
Error: Type t_word has layout word.
       Variants may not yet contain types of this layout.
|}];;

type ('a : word) t5_7 = A of int
type ('a : word) t5_8 = A of 'a;;
[%%expect{|
type ('a : word) t5_7 = A of int
Line 2, characters 24-31:
2 | type ('a : word) t5_8 = A of 'a;;
                            ^^^^^^^
Error: Type 'a has layout word.
       Variants may not yet contain types of this layout.
|}]

(****************************************************)
(* Test 6: Can't be put at top level of signatures. *)
module type S6_1 = sig val x : t_word end

let f6 (m : (module S6_1)) = let module M6 = (val m) in M6.x;;
[%%expect{|
Line 1, characters 31-37:
1 | module type S6_1 = sig val x : t_word end
                                   ^^^^^^
Error: This type signature for x is not a value type.
       x has layout word, which is not a sublayout of value.
|}];;

module type S6_2 = sig val x : 'a t_word_id end
[%%expect{|
Line 1, characters 31-43:
1 | module type S6_2 = sig val x : 'a t_word_id end
                                   ^^^^^^^^^^^^
Error: This type signature for x is not a value type.
       x has layout word, which does not overlap with value.
|}];;

module type S6_3 = sig val x : nativeint# end
[%%expect{|
Line 1, characters 31-41:
1 | module type S6_3 = sig val x : nativeint# end
                                   ^^^^^^^^^^
Error: This type signature for x is not a value type.
       x has layout word, which is not a sublayout of value.
|}];;


(*********************************************************)
(* Test 7: Can't be used as polymorphic variant argument *)
let f7_1 (x : t_word) = `A x;;
[%%expect{|
Line 1, characters 27-28:
1 | let f7_1 (x : t_word) = `A x;;
                               ^
Error: This expression has type t_word but an expression was expected of type
         ('a : value)
       t_word has layout word, which is not a sublayout of value.
|}];;

let f7_2 (x : 'a t_word_id) = `A x;;
[%%expect{|
Line 1, characters 33-34:
1 | let f7_2 (x : 'a t_word_id) = `A x;;
                                     ^
Error: This expression has type 'a t_word_id = ('a : word)
       but an expression was expected of type ('b : value)
       'a t_word_id has layout word, which does not overlap with value.
|}];;

let f7_3 (x : nativeint#) = `A x;;
[%%expect{|
Line 1, characters 31-32:
1 | let f7_3 (x : nativeint#) = `A x;;
                                   ^
Error: This expression has type nativeint#
       but an expression was expected of type ('a : value)
       nativeint# has layout word, which is not a sublayout of value.
|}];;

type f7_4 = [ `A of t_word ];;
[%%expect{|
Line 1, characters 20-26:
1 | type f7_4 = [ `A of t_word ];;
                        ^^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
        t_word has layout word, which is not a sublayout of value.
|}];;

type ('a : word) f7_5 = [ `A of 'a ];;
[%%expect{|
Line 1, characters 32-34:
1 | type ('a : word) f7_5 = [ `A of 'a ];;
                                    ^^
Error: This type ('a : value) should be an instance of type ('a0 : word)
       'a has layout word, which does not overlap with value.
|}];;
(* CR layouts v2.9: This error could be improved *)

(************************************************************)
(* Test 8: Normal polymorphic functions don't work on them. *)

let make_t_word () : t_word = assert false
let make_t_word_id () : 'a t_word_id = assert false
let make_nativeintu () : nativeint# = assert false

let id_value x = x;;
[%%expect{|
val make_t_word : unit -> t_word = <fun>
val make_t_word_id : ('a : word). unit -> 'a t_word_id = <fun>
val make_nativeintu : unit -> nativeint# = <fun>
val id_value : 'a -> 'a = <fun>
|}];;

let x8_1 = id_value (make_t_word ());;
[%%expect{|
Line 1, characters 20-36:
1 | let x8_1 = id_value (make_t_word ());;
                        ^^^^^^^^^^^^^^^^
Error: This expression has type t_word but an expression was expected of type
         ('a : value)
       t_word has layout word, which is not a sublayout of value.
|}];;

let x8_2 = id_value (make_t_word_id ());;
[%%expect{|
Line 1, characters 20-39:
1 | let x8_2 = id_value (make_t_word_id ());;
                        ^^^^^^^^^^^^^^^^^^^
Error: This expression has type 'a t_word_id = ('a : word)
       but an expression was expected of type ('b : value)
       'a t_word_id has layout word, which does not overlap with value.
|}];;

let x8_3 = id_value (make_nativeintu ());;
[%%expect{|
Line 1, characters 20-40:
1 | let x8_3 = id_value (make_nativeintu ());;
                        ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type nativeint#
       but an expression was expected of type ('a : value)
       nativeint# has layout word, which is not a sublayout of value.
|}];;

(*************************************)
(* Test 9: But word functions do. *)

let twice f (x : 'a t_word_id) = f (f x)

let f9_1 () = twice f1_1 (make_t_word ())
let f9_2 () = twice f1_2 (make_t_word_id ())
let f9_3 () = twice f1_3 (make_nativeintu ());;
[%%expect{|
val twice :
  ('a : word). ('a t_word_id -> 'a t_word_id) -> 'a t_word_id -> 'a t_word_id =
  <fun>
val f9_1 : unit -> t_word t_word_id = <fun>
val f9_2 : ('a : word). unit -> 'a t_word_id = <fun>
val f9_3 : unit -> nativeint# t_word_id = <fun>
|}];;

(**************************************************)
(* Test 10: Invalid uses of word and externals *)

(* Valid uses of word in externals are tested elsewhere - this is just a test
   for uses the typechecker should reject.  In particular
   - if using a non-value layout in an external, you must supply separate
     bytecode and native code implementations,
   - unboxed types can't be unboxed more.
*)

external f10_1 : int -> bool -> nativeint# = "foo";;
[%%expect{|
Line 1, characters 0-50:
1 | external f10_1 : int -> bool -> nativeint# = "foo";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}];;

external f10_2 : t_word -> int = "foo";;
[%%expect{|
Line 1, characters 0-38:
1 | external f10_2 : t_word -> int = "foo";;
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The native code version of the primitive is mandatory
       for types with non-value layouts.
|}];;

external f10_6 : (nativeint#[@unboxed]) -> bool -> string  = "foo" "bar";;
[%%expect{|
Line 1, characters 18-28:
1 | external f10_6 : (nativeint#[@unboxed]) -> bool -> string  = "foo" "bar";;
                      ^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only float, int32, int64, nativeint, and vector primitives can be unboxed.
|}];;

external f10_7 : string -> (nativeint#[@unboxed])  = "foo" "bar";;
[%%expect{|
Line 1, characters 28-38:
1 | external f10_7 : string -> (nativeint#[@unboxed])  = "foo" "bar";;
                                ^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only float, int32, int64, nativeint, and vector primitives can be unboxed.
|}];;

external f10_8 : nativeint -> nativeint#  = "foo" "bar" [@@unboxed];;
[%%expect{|
Line 1, characters 30-40:
1 | external f10_8 : nativeint -> nativeint#  = "foo" "bar" [@@unboxed];;
                                  ^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only float, int32, int64, nativeint, and vector primitives can be unboxed.
|}];;

(*******************************************************)
(* Test 11: Don't allow word in extensible variants *)

type t11_1 = ..

type t11_1 += A of t_word;;
[%%expect{|
type t11_1 = ..
Line 3, characters 14-25:
3 | type t11_1 += A of t_word;;
                  ^^^^^^^^^^^
Error: Type t_word has layout word.
       Variants may not yet contain types of this layout.
|}]

type t11_1 += B of nativeint#;;
[%%expect{|
Line 1, characters 14-29:
1 | type t11_1 += B of nativeint#;;
                  ^^^^^^^^^^^^^^^
Error: Type nativeint# has layout word.
       Variants may not yet contain types of this layout.
|}]

type ('a : word) t11_2 = ..

type 'a t11_2 += A of int

type 'a t11_2 += B of 'a;;

[%%expect{|
type ('a : word) t11_2 = ..
type 'a t11_2 += A of int
Line 5, characters 17-24:
5 | type 'a t11_2 += B of 'a;;
                     ^^^^^^^
Error: Type 'a has layout word.
       Variants may not yet contain types of this layout.
|}]

(***************************************)
(* Test 12: word in objects/classes *)

(* First, disallowed uses: in object types, class parameters, etc. *)
type t12_1 = < x : t_word >;;
[%%expect{|
Line 1, characters 15-25:
1 | type t12_1 = < x : t_word >;;
                   ^^^^^^^^^^
Error: Object field types must have layout value.
        t_word has layout word, which is not a sublayout of value.
|}];;

type ('a : word) t12_2 = < x : 'a >;;
[%%expect{|
Line 1, characters 31-33:
1 | type ('a : word) t12_2 = < x : 'a >;;
                                   ^^
Error: This type ('a : value) should be an instance of type ('a0 : word)
       'a has layout word, which does not overlap with value.
|}]

class c12_3 = object method x : t_word = assert false end;;
[%%expect{|
Line 1, characters 21-53:
1 | class c12_3 = object method x : t_word = assert false end;;
                         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The method x has type t_word but is expected to have type ('a : value)
       t_word has layout word, which is not a sublayout of value.
|}];;

class ['a] c12_4 = object
  method x : 'a t_word_id -> 'a t_word_id = assert false
end;;
[%%expect{|
Line 2, characters 13-15:
2 |   method x : 'a t_word_id -> 'a t_word_id = assert false
                 ^^
Error: This type ('a : word) should be an instance of type ('a0 : value)
       'a has layout value, which does not overlap with word.
|}];;
(* CR layouts v2.9: Error could be improved *)

class c12_5 = object val x : t_word = assert false end;;
[%%expect{|
Line 1, characters 25-26:
1 | class c12_5 = object val x : t_word = assert false end;;
                             ^
Error: Variables bound in a class must have layout value.
       x has layout word, which is not a sublayout of value.
|}];;

class type c12_6 = object method x : nativeint# end;;
[%%expect{|
Line 1, characters 26-47:
1 | class type c12_6 = object method x : nativeint# end;;
                              ^^^^^^^^^^^^^^^^^^^^^
Error: The method x has type nativeint# but is expected to have type
         ('a : value)
       nativeint# has layout word, which is not a sublayout of value.
|}];;
(* CR layouts v2.9: Error could be improved *)

class type c12_7 = object val x : nativeint# end
[%%expect{|
Line 1, characters 26-44:
1 | class type c12_7 = object val x : nativeint# end
                              ^^^^^^^^^^^^^^^^^^
Error: Variables bound in a class must have layout value.
       x has layout word, which is not a sublayout of value.
|}];;

class type ['a] c12_8 = object
  val x : 'a t_word_id -> 'a t_word_id
end
[%%expect{|
Line 2, characters 10-12:
2 |   val x : 'a t_word_id -> 'a t_word_id
              ^^
Error: This type ('a : word) should be an instance of type ('a0 : value)
       'a has layout value, which does not overlap with word.
|}];;

(* Second, allowed uses: as method parameters / returns *)
type t12_8 = < f : t_word -> t_word >
let f12_9 (o : t12_8) x = o#f x
let f12_10 o (y : t_word) : t_word = o#baz y y y;;
class ['a] c12_11 = object
  method x : t_word -> 'a = assert false
end;;
class ['a] c12_12 = object
  method x : 'a -> t_word = assert false
end;;
[%%expect{|
type t12_8 = < f : t_word -> t_word >
val f12_9 : t12_8 -> t_word -> t_word = <fun>
val f12_10 :
  < baz : t_word -> t_word -> t_word -> t_word; .. > -> t_word -> t_word =
  <fun>
class ['a] c12_11 : object method x : t_word -> 'a end
class ['a] c12_12 : object method x : 'a -> t_word end
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
       but an expression was expected of type t_word
       t_word has layout word, which is not a sublayout of value.
|}];;

let f12_14 (m1 : t_word) (m2 : t_word) = object
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
       t_word has layout word, which is not a sublayout of value.
|}];;

(*********************************************************************)
(* Test 13: Ad-hoc polymorphic operations don't work on word yet. *)

(* CR layouts v5: Remember to handle the case of calling these on structures
   containing other layouts. *)

let f13_1 (x : t_word) = x = x;;
[%%expect{|
Line 1, characters 25-26:
1 | let f13_1 (x : t_word) = x = x;;
                             ^
Error: This expression has type t_word but an expression was expected of type
         ('a : value)
       t_word has layout word, which is not a sublayout of value.
|}];;

let f13_2 (x : t_word) = compare x x;;
[%%expect{|
Line 1, characters 33-34:
1 | let f13_2 (x : t_word) = compare x x;;
                                     ^
Error: This expression has type t_word but an expression was expected of type
         ('a : value)
       t_word has layout word, which is not a sublayout of value.
|}];;

let f13_3 (x : t_word) = Marshal.to_bytes x;;
[%%expect{|
Line 1, characters 42-43:
1 | let f13_3 (x : t_word) = Marshal.to_bytes x;;
                                              ^
Error: This expression has type t_word but an expression was expected of type
         ('a : value)
       t_word has layout word, which is not a sublayout of value.
|}];;

let f13_4 (x : t_word) = Hashtbl.hash x;;
[%%expect{|
Line 1, characters 38-39:
1 | let f13_4 (x : t_word) = Hashtbl.hash x;;
                                          ^
Error: This expression has type t_word but an expression was expected of type
         ('a : value)
       t_word has layout word, which is not a sublayout of value.
|}];;
