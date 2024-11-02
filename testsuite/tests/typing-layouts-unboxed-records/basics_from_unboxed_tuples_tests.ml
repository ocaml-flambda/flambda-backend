(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 {
   expect;
 }
*)

(* These tests are adapted from the tuple tests in
   [testsuite/tests/typing-layouts-products/basics.ml] *)

open Stdlib_upstream_compatible

(**********************************************************)
(* Test 1: Basic unboxed product layouts and record types. *)

type t2 = #{ s : string; f : float#; i : int }
[%%expect{|
type t2 = #{ s : string; f : float#; i : int; }
|}]

(* You can put unboxed and normal products inside unboxed products *)
type t4_inner2 = #{ b : bool; i : int }
type t4_inner = #{ i : int; t4_inner2 : t4_inner2; co : char option }
type t4 = #{ s : string; t4_inner : t4_inner }
[%%expect{|
type t4_inner2 = #{ b : bool; i : int; }
type t4_inner = #{ i : int; t4_inner2 : t4_inner2; co : char option; }
type t4 = #{ s : string; t4_inner : t4_inner; }
|}]

(* But you can't put unboxed products into tuples (yet) *)
type t_nope_inner = #{ s : string; b : bool }
type t_nope = string * t_nope_inner
[%%expect{|
type t_nope_inner = #{ s : string; b : bool; }
Line 2, characters 23-35:
2 | type t_nope = string * t_nope_inner
                           ^^^^^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of "t_nope_inner" is value & value
         because of the definition of t_nope_inner at line 1, characters 0-45.
       But the layout of "t_nope_inner" must be a sublayout of value
         because it's the type of a tuple element.
|}]

(********************************************)
(* Test 2: Simple kind annotations on types *)

type t1 : float64 & value = #{ f : float#; b : bool }
type t2 : value & (float64 & value) = #(string option * t1)
[%%expect{|
type t1 = #{ f : float#; b : bool; }
type t2 = #(string option * t1)
|}]

type t2_wrong : value & float64 & value = #{ so : string option; t1 : t1 }
[%%expect{|
Line 1, characters 0-74:
1 | type t2_wrong : value & float64 & value = #{ so : string option; t1 : t1 }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "t2_wrong" is value & (float64 & value)
         because it is an unboxed record.
       But the layout of type "t2_wrong" must be a sublayout of value & float64 & value
         because of the annotation on the declaration of the type t2_wrong.
|}]

type ('a : value & bits64) t3 = 'a
type t4_inner = #{ i : int; i64 : int64# }
type t4 = t4_inner t3
type t5 = t4 t3
[%%expect{|
type ('a : value & bits64) t3 = 'a
type t4_inner = #{ i : int; i64 : int64#; }
type t4 = t4_inner t3
type t5 = t4 t3
|}]

type t4_wrong_inner = #{ i1 : int; i2 : int }
type t4_wrong = t4_wrong_inner t3
[%%expect{|
type t4_wrong_inner = #{ i1 : int; i2 : int; }
Line 2, characters 16-30:
2 | type t4_wrong = t4_wrong_inner t3
                    ^^^^^^^^^^^^^^
Error: This type "t4_wrong_inner" should be an instance of type
         "('a : value & bits64)"
       The layout of t4_wrong_inner is value & value
         because of the definition of t4_wrong_inner at line 1, characters 0-45.
       But the layout of t4_wrong_inner must be a sublayout of value & bits64
         because of the definition of t3 at line 1, characters 0-34.
|}]

(* some mutually recusive types *)
type ('a : value & bits64) t6 = 'a t7
and 'a t7 = { x : 'a t6 }
[%%expect{|
type ('a : value & bits64) t6 = 'a t7
and ('a : value & bits64) t7 = { x : 'a t6; }
|}]

type t9_record = #{ i : int; i64 : int64# }
type t9 = t9_record t7
type t10 = bool t6
[%%expect{|
type t9_record = #{ i : int; i64 : int64#; }
type t9 = t9_record t7
Line 3, characters 11-15:
3 | type t10 = bool t6
               ^^^^
Error: This type "bool" should be an instance of type "('a : value & bits64)"
       The layout of bool is value
         because it's an enumeration variant type (all constructors are constant).
       But the layout of bool must be a sublayout of value & bits64
         because of the definition of t6 at line 1, characters 0-37.
|}]

(* CR rtjoa: claims kind of t6_wrong_inner_record is value *)
type t6_wrong_inner_record = #{ i : int; i64 : int64 }
and ('a : value & bits64) t6_wrong = 'a t7_wrong
and 'a t7_wrong = { x : t6_wrong_inner_record t6_wrong }
[%%expect{|
Line 1, characters 0-54:
1 | type t6_wrong_inner_record = #{ i : int; i64 : int64 }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of t6_wrong_inner_record is value
         because it is the primitive type int64.
       But the layout of t6_wrong_inner_record must be a sublayout of bits64
         because of the annotation on 'a in the declaration of the type
                                      t6_wrong.
|}]

(* Just like t6/t7, but with the annotation on the other (the order doesn't
   matter) *)
type 'a t11 = 'a t12
and ('a : value & bits64) t12 = { x : 'a t11 }
[%%expect{|
type ('a : value & bits64) t11 = 'a t12
and ('a : value & bits64) t12 = { x : 'a t11; }
|}]

(*********************************************************************)
(* Test 3: Unboxed records are allowed in function args and returns *)

type t1_left = #{ i : int; b : bool }
type t1_right_inner = #{ i64 : int64#; so : string option }
type t1_right = #{ i : int; f : float#; inner : t1_right_inner }
type t1 = t1_left -> t1_right
[%%expect{|
type t1_left = #{ i : int; b : bool; }
type t1_right_inner = #{ i64 : int64#; so : string option; }
type t1_right = #{ i : int; f : float#; inner : t1_right_inner; }
type t1 = t1_left -> t1_right
|}]

type make_record_result = #{ f : float#; s : string }
let f_make_an_unboxed_record (x : string) (y : float#) = #{ f = y; s = x }

type inner = #{ f1 : float#; f2 : float# }
type t = #{ s : string; inner : inner }
let f_pull_apart_an_unboxed_record (x : t) =
  match x with
  | #{ s; inner = #{ f1; f2 } } ->
    if s = "mul" then
      Float_u.mul f1 f2
    else
      Float_u.add f1 f2
[%%expect{|
type make_record_result = #{ f : float#; s : string; }
val f_make_an_unboxed_record : string -> float# -> make_record_result = <fun>
type inner = #{ f1 : float#; f2 : float#; }
type t = #{ s : string; inner : inner; }
val f_pull_apart_an_unboxed_record :
  t -> Stdlib_upstream_compatible.Float_u.t = <fun>
|}]


module type S = sig
  type a
  type b
  type c
  type d
  type e
  type f
  type g
  type h
end

module F(X : S) = struct
  include X
  type mix_input_inner2 = #{ d : d; e : e }
  type mix_input_inner = #{ c : c; inner2 : mix_input_inner2 }
  type mix_input = #{ a : a; b : b; inner : mix_input_inner; f : f }
  type mix_output_inner2 = #{ f : f; e : e }
  type mix_output_inner = #{ c : c; inner2 : mix_output_inner2 }
  type mix_output = #{ b : b; inner : mix_output_inner; a : a; d : d }
  let f_mix_up_an_unboxed_record (x : mix_input) =
    let #{ a; b; inner = #{ c; inner2 = #{ d; e } }; f } = x in
    #{ b = b; inner = #{ c = c; inner2 = #{ f = f; e = e } }; a = a; d = d }

  type take_few_input1 = #{ a : a; b : b }
  type take_few_input3 = #{ d : d; e : e }
  type take_few_input5 = #{ g : g; h : h }
  type take_few_output = #{ h : h; g2 : g; x4 : f; e2 : e; d : d; x2 : c; b : b; a2 : a }

  let f_take_a_few_unboxed_records (x1 : take_few_input1) x2 (x3 : take_few_input3) x4 (x5 : take_few_input5) =
    let #{ a; b } = x1 in
    let #{ d; e } = x3 in
    let #{ g; h } = x5 in
    #{ h = h; g2 = g; x4 = x4; e2 = e; d = d; x2 = x2; b = b; a2 = a }
end
[%%expect{|
module type S =
  sig type a type b type c type d type e type f type g type h end
module F :
  functor (X : S) ->
    sig
      type a = X.a
      type b = X.b
      type c = X.c
      type d = X.d
      type e = X.e
      type f = X.f
      type g = X.g
      type h = X.h
      type mix_input_inner2 = #{ d : d; e : e; }
      type mix_input_inner = #{ c : c; inner2 : mix_input_inner2; }
      type mix_input = #{ a : a; b : b; inner : mix_input_inner; f : f; }
      type mix_output_inner2 = #{ f : f; e : e; }
      type mix_output_inner = #{ c : c; inner2 : mix_output_inner2; }
      type mix_output = #{ b : b; inner : mix_output_inner; a : a; d : d; }
      val f_mix_up_an_unboxed_record : mix_input -> mix_output
      type take_few_input1 = #{ a : a; b : b; }
      type take_few_input3 = #{ d : d; e : e; }
      type take_few_input5 = #{ g : g; h : h; }
      type take_few_output = #{
        h : h;
        g2 : g;
        x4 : f;
        e2 : e;
        d : d;
        x2 : c;
        b : b;
        a2 : a;
      }
      val f_take_a_few_unboxed_records :
        take_few_input1 ->
        c -> take_few_input3 -> f -> take_few_input5 -> take_few_output
    end
|}]

[%%expect{|
|}]

(***************************************************)
(* Test 4: Unboxed products don't go in structures *)

type poly_var_inner = #{ i : int; b : bool }
type poly_var_type = [ `Foo of poly_var_inner ]
[%%expect{|
type poly_var_inner = #{ i : int; b : bool; }
Line 2, characters 31-45:
2 | type poly_var_type = [ `Foo of poly_var_inner ]
                                   ^^^^^^^^^^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of "poly_var_inner" is value & value
         because of the definition of poly_var_inner at line 1, characters 0-44.
       But the layout of "poly_var_inner" must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}]

type poly_var_term_record = #{ i : int; i2 : int }
let poly_var_term = `Foo #{ i = 1; i2 = 2 }
[%%expect{|
type poly_var_term_record = #{ i : int; i2 : int; }
Line 2, characters 25-43:
2 | let poly_var_term = `Foo #{ i = 1; i2 = 2 }
                             ^^^^^^^^^^^^^^^^^^
Error: This expression has type "poly_var_term_record"
       but an expression was expected of type "('a : value)"
       The layout of poly_var_term_record is value & value
         because of the definition of poly_var_term_record at line 1, characters 0-50.
       But the layout of poly_var_term_record must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}]

type record_inner = #{ b : bool; f : float# }
type tuple_type = (int * record_inner)
[%%expect{|
type record_inner = #{ b : bool; f : float#; }
Line 2, characters 25-37:
2 | type tuple_type = (int * record_inner)
                             ^^^^^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of "record_inner" is value & float64
         because of the definition of record_inner at line 1, characters 0-45.
       But the layout of "record_inner" must be a sublayout of value
         because it's the type of a tuple element.
|}]

type record = #{ i : int; i2 : int }
let record_term = ("hi", #{ i = 1; i2 = 2 })
[%%expect{|
type record = #{ i : int; i2 : int; }
Line 2, characters 25-43:
2 | let record_term = ("hi", #{ i = 1; i2 = 2 })
                             ^^^^^^^^^^^^^^^^^^
Error: This expression has type "record" but an expression was expected of type
         "('a : value)"
       The layout of record is value & value
         because of the definition of record at line 1, characters 0-36.
       But the layout of record must be a sublayout of value
         because it's the type of a tuple element.
|}]

type record_inner = #{ i : int; b : bool }
type record = { x : record_inner }
[%%expect{|
type record_inner = #{ i : int; b : bool; }
Line 2, characters 0-34:
2 | type record = { x : record_inner }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "record_inner" has layout "value & value".
       Records may not yet contain types of this layout.
|}]

type inlined_inner = #{ i : int; b : bool }
type inlined_record = A of { x : inlined_inner }
[%%expect{|
type inlined_inner = #{ i : int; b : bool; }
Line 2, characters 22-48:
2 | type inlined_record = A of { x : inlined_inner }
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "inlined_inner" has layout "value & value".
       Inlined records may not yet contain types of this layout.
|}]

type variant_inner = #{ i : int; b : bool }
type variant = A of variant_inner
[%%expect{|
type variant_inner = #{ i : int; b : bool; }
Line 2, characters 15-33:
2 | type variant = A of variant_inner
                   ^^^^^^^^^^^^^^^^^^
Error: Type "variant_inner" has layout "value & value".
       Variants may not yet contain types of this layout.
|}]

type sig_inner = #{ i : int; b : bool }
module type S = sig
  val x : sig_inner
end
[%%expect{|
type sig_inner = #{ i : int; b : bool; }
Line 3, characters 10-19:
3 |   val x : sig_inner
              ^^^^^^^^^
Error: This type signature for "x" is not a value type.
       The layout of type sig_inner is value & value
         because of the definition of sig_inner at line 1, characters 0-39.
       But the layout of type sig_inner must be a sublayout of value
         because it's the type of something stored in a module structure.
|}]

type m_record = #{ i1 : int; i2 : int }
module M = struct
  let x = #{ i1 = 1; i2 = 2 }
end
[%%expect{|
type m_record = #{ i1 : int; i2 : int; }
Line 3, characters 6-7:
3 |   let x = #{ i1 = 1; i2 = 2 }
          ^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x" has layout "value & value".
|}]

(* This one is okay, because the record has layout vlue *)
type m_record = #{ i1 : int }
module M = struct
  let x = #{ i1 = 1 }
end
[%%expect{|
type m_record = #{ i1 : int; }
module M : sig val x : m_record end
|}]

type object_inner = #{ i : int; b : bool }
type object_type = < x : object_inner >
[%%expect{|
type object_inner = #{ i : int; b : bool; }
Line 2, characters 21-37:
2 | type object_type = < x : object_inner >
                         ^^^^^^^^^^^^^^^^
Error: Object field types must have layout value.
       The layout of "object_inner" is value & value
         because of the definition of object_inner at line 1, characters 0-42.
       But the layout of "object_inner" must be a sublayout of value
         because it's the type of an object field.
|}]

type object_term_record = #{ i1 : int; i2 : int }
let object_term = object val x = #{ i1 = 1; i2 = 2 } end
[%%expect{|
type object_term_record = #{ i1 : int; i2 : int; }
Line 2, characters 29-30:
2 | let object_term = object val x = #{ i1 = 1; i2 = 2 } end
                                 ^
Error: Variables bound in a class must have layout value.
       The layout of x is value & value
         because of the definition of object_term_record at line 1, characters 0-49.
       But the layout of x must be a sublayout of value
         because it's the type of a class field.
|}]

type class_record = #{ i1 : int; i2 : int }
class class_ =
  object
    method x = #{ i1 = 1; i2 = 2 }
  end
[%%expect{|
type class_record = #{ i1 : int; i2 : int; }
Line 4, characters 15-34:
4 |     method x = #{ i1 = 1; i2 = 2 }
                   ^^^^^^^^^^^^^^^^^^^
Error: This expression has type "class_record"
       but an expression was expected of type "('a : value)"
       The layout of class_record is value & value
         because of the definition of class_record at line 1, characters 0-43.
       But the layout of class_record must be a sublayout of value
         because it's the type of an object field.
|}]

type capture_record = #{ x : int; y : int }
let capture_in_object utup = object
  val f = fun () ->
    let #{ x; y } = utup in
    x + y
end;;
[%%expect{|
type capture_record = #{ x : int; y : int; }
Line 4, characters 20-24:
4 |     let #{ x; y } = utup in
                        ^^^^
Error: This expression has type "('a : value)"
       but an expression was expected of type "capture_record"
       The layout of capture_record is value & value
         because of the definition of capture_record at line 1, characters 0-43.
       But the layout of capture_record must be a sublayout of value
         because it's the type of a variable captured in an object.
|}];;

(****************************************************)
(* Test 5: Methods may take/return unboxed products *)

type method_input = #{ a : int; b : int }
type method_output = #{ sum_a : int; sum_b : int }

class class_with_urecord_manipulating_method =
  object
    method f (x : method_input) (y : method_input) =
      let #{ a; b } = x in
      let #{ a = c; b = d } = y in
      #{ sum_a = a + c; sum_b = b + d }
  end
[%%expect{|
type method_input = #{ a : int; b : int; }
type method_output = #{ sum_a : int; sum_b : int; }
class class_with_urecord_manipulating_method :
  object method f : method_input -> method_input -> method_output end
|}]

(*******************************************)
(* Test 6: Nested expansion in kind checks *)

(* This test shows that the [check_coherence] check in Typedecl can look deeply
   into a product kind. That check is reached in this case because the
   algorithm in typedecl assumes the annotation is correct initially, and then
   it is checked in [check_coherence]. This relies on [type_jkind] doing
   deep expansion, as [check_coherence] calls it and then [Jkind.sub], rather
   than using [check_type_jkind]. *)
(* CR rtjoa: these succeed for unboxed tuples, but fail for records 11 *)
module type S_coherence_deep = sig
  type t1 : any
  type t2 = #{ i : int; t1 : t1 }
end
[%%expect{|
Line 3, characters 24-31:
3 |   type t2 = #{ i : int; t1 : t1 }
                            ^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1 is any
         because of the definition of t1 at line 2, characters 2-15.
       But the layout of t1 must be representable
         because it is the type of record field t1.
|}]


module type S_coherence_deep' = S_coherence_deep with type t1 = float#

module F(X : S_coherence_deep') = struct
  type r : value & float64 = X.t2
end
[%%expect{|
Line 1, characters 32-48:
1 | module type S_coherence_deep' = S_coherence_deep with type t1 = float#
                                    ^^^^^^^^^^^^^^^^
Error: Unbound module type "S_coherence_deep"
|}]

module type S_coherence_deeper = sig
  type t1 : any
  type t2 = #{ i : int; t1 : t1 }
  type t3 = #{ t2 : t2; b : bool; i64 : int64# }
  type t4 = #{ f : float#; t3 : t3; i : int }
end

module type S_coherence_deeper' = S_coherence_deeper with type t1 = float#

module F(X : S_coherence_deeper') = struct
  type r : float64 & ((value & float64) & value & bits64) & value = X.t4
end
[%%expect{|
Line 3, characters 24-31:
3 |   type t2 = #{ i : int; t1 : t1 }
                            ^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1/2 is any
         because of the definition of t1 at line 2, characters 2-15.
       But the layout of t1/2 must be representable
         because it is the type of record field t1.
|}]

(* Like the above, but hitting the nested expansion case in
   [constrain_type_jkind] *)
module type S_constrain_type_jkind_deep = sig
  type t1 : any
  type t2_record = #{ i : int; t1 : t1 }
  type t2 = t2_record
end

module type S_constrain_type_jkind_deep' =
  S_constrain_type_jkind_deep with type t1 = float#

type ('a : value & float64) t_constraint

module F(X : S_constrain_type_jkind_deep') = struct
  type r = X.t2 t_constraint
end
[%%expect{|
Line 3, characters 31-38:
3 |   type t2_record = #{ i : int; t1 : t1 }
                                   ^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1/3 is any
         because of the definition of t1 at line 2, characters 2-15.
       But the layout of t1/3 must be representable
         because it is the type of record field t1.
|}]

module type S_constrain_type_jkind_deeper = sig
  type t1 : any
  type t2 = #{ i : int; t1 : t1 }
  type t3 = #{ t2 : t2; b : bool; i64 : int64# }
  type t4 = #{ f : float#; t3 : t3; i : int }
end

module type S_constrain_type_jkind_deeper' =
  S_constrain_type_jkind_deeper with type t1 = float#

type ('a : float64 & ((value & float64) & value & bits64) & value) t_constraint

module F(X : S_constrain_type_jkind_deeper') = struct
  type r = X.t4 t_constraint
end
[%%expect{|
Line 3, characters 24-31:
3 |   type t2 = #{ i : int; t1 : t1 }
                            ^^^^^^^
Error: Unboxed record element types must have a representable layout.
       The layout of t1/4 is any
         because of the definition of t1 at line 2, characters 2-15.
       But the layout of t1/4 must be representable
         because it is the type of record field t1.
|}]

(***********************************************)
(* Test 7: modal kinds for unboxed tuple types *)

type local_cross1 = #{ i1 : int; i2 : int }
let f_external_urecord_mode_crosses_local_1
  : local_ local_cross1 -> local_cross1 = fun x -> x
[%%expect{|
type local_cross1 = #{ i1 : int; i2 : int; }
val f_external_urecord_mode_crosses_local_1 :
  local_ local_cross1 -> local_cross1 = <fun>
|}]

type local_nocross1 = #{ i : int; s : string }
let f_internal_urecord_does_not_mode_cross_local_1
  : local_ local_nocross1 -> local_nocross1 = fun x -> x
[%%expect{|
type local_nocross1 = #{ i : int; s : string; }
Line 3, characters 55-56:
3 |   : local_ local_nocross1 -> local_nocross1 = fun x -> x
                                                           ^
Error: This value escapes its region.
|}]

type local_cross2_inner = #{ b : bool; i : int }
type local_cross2 = #{ i : int; inner : local_cross2_inner }
let f_external_urecord_mode_crosses_local_2
  : local_ local_cross2 -> local_cross2 = fun x -> x
[%%expect{|
type local_cross2_inner = #{ b : bool; i : int; }
type local_cross2 = #{ i : int; inner : local_cross2_inner; }
val f_external_urecord_mode_crosses_local_2 :
  local_ local_cross2 -> local_cross2 = <fun>
|}]

type local_nocross2_inner = #{ b : bool; s : string }
type local_nocross2 = #{ i : int; inner : local_nocross2_inner }
let f_internal_urecord_does_not_mode_cross_local_2
  : local_ local_nocross2 -> local_nocross2 = fun x -> x
[%%expect{|
type local_nocross2_inner = #{ b : bool; s : string; }
type local_nocross2 = #{ i : int; inner : local_nocross2_inner; }
Line 4, characters 55-56:
4 |   : local_ local_nocross2 -> local_nocross2 = fun x -> x
                                                           ^
Error: This value escapes its region.
|}]

type t = #{ i1 : int; i2 : int }
type local_cross3_inner = #{ t : t; i : int }
type local_cross3 = #{ i : int; inner : local_cross3_inner }
let f_external_urecord_mode_crosses_local_3
  : local_ local_cross3 -> local_cross3 = fun x -> x
[%%expect{|
type t = #{ i1 : int; i2 : int; }
type local_cross3_inner = #{ t : t; i : int; }
type local_cross3 = #{ i : int; inner : local_cross3_inner; }
val f_external_urecord_mode_crosses_local_3 :
  local_ local_cross3 -> local_cross3 = <fun>
|}]

type t = #{ s : string; i : int }
type local_nocross3_inner = #{ t : t; b : bool }
type local_nocross3 = #{ i : int; inner : local_nocross3_inner }
let f_internal_urecord_does_not_mode_cross_local_3
  : local_ local_nocross3 -> local_nocross3 = fun x -> x
[%%expect{|
type t = #{ s : string; i : int; }
type local_nocross3_inner = #{ t : t; b : bool; }
type local_nocross3 = #{ i : int; inner : local_nocross3_inner; }
Line 5, characters 55-56:
5 |   : local_ local_nocross3 -> local_nocross3 = fun x -> x
                                                           ^
Error: This value escapes its region.
|}]

(****************************************************)
(* Test 8: modal kinds for product kind annotations *)

(* Nothing unique to unboxed records here *)

(*********************)
(* Test 9: externals *)

type t_product : value & value

type ext_record_arg_record = #{ i : int; b : bool }
external ext_record_arg : ext_record_arg_record -> int = "foo" "bar"
[%%expect{|
type t_product : value & value
type ext_record_arg_record = #{ i : int; b : bool; }
Line 4, characters 26-54:
4 | external ext_record_arg : ext_record_arg_record -> int = "foo" "bar"
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

type ext_record_arg_attr_record = #{ i : int; b : bool }
external ext_record_arg_with_attr : (ext_record_arg_attr_record [@unboxed]) -> int = "foo"
[%%expect{|
type ext_record_arg_attr_record = #{ i : int; b : bool; }
Line 2, characters 37-63:
2 | external ext_record_arg_with_attr : (ext_record_arg_attr_record [@unboxed]) -> int = "foo"
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
|}]

external ext_product_arg : t_product -> int = "foo" "bar"
[%%expect{|
Line 1, characters 27-43:
1 | external ext_product_arg : t_product -> int = "foo" "bar"
                               ^^^^^^^^^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external ext_product_arg_with_attr : (t_product [@unboxed]) -> int = "foo"
[%%expect{|
Line 1, characters 38-47:
1 | external ext_product_arg_with_attr : (t_product [@unboxed]) -> int = "foo"
                                          ^^^^^^^^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
|}]

type t = #{ i : int; b : bool }
external ext_record_return : int -> t = "foo" "bar"
[%%expect{|
type t = #{ i : int; b : bool; }
Line 2, characters 29-37:
2 | external ext_record_return : int -> t = "foo" "bar"
                                 ^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

type t = #{ i : int; b : bool }
external ext_record_return_with_attr : int -> (t [@unboxed]) = "foo"
[%%expect{|
type t = #{ i : int; b : bool; }
Line 2, characters 47-48:
2 | external ext_record_return_with_attr : int -> (t [@unboxed]) = "foo"
                                                   ^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
|}]

external ext_product_return : int -> t_product = "foo" "bar"
[%%expect{|
Line 1, characters 30-46:
1 | external ext_product_return : int -> t_product = "foo" "bar"
                                  ^^^^^^^^^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external ext_product_return_with_attr : int -> (t_product [@unboxed]) = "foo"
[%%expect{|
Line 1, characters 48-57:
1 | external ext_product_return_with_attr : int -> (t_product [@unboxed]) = "foo"
                                                    ^^^^^^^^^
Error: Don't know how to unbox this type.
       Only "float", "int32", "int64", "nativeint", vector primitives, and
       the corresponding unboxed types can be marked unboxed.
|}]

external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"

type id_record = #{ x : int; y : int }
let sum =
  let #{ x; y } = id #{ x = 1; y = 2 } in
  x + y
[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
type id_record = #{ x : int; y : int; }
val sum : int = 3
|}]

(***********************************)
(* Test 9: not allowed in let recs *)

(* An example that is allowed on tuples but not unboxed products *)
let[@warning "-26"] e1 = let rec x = (1, y) and y = 42 in ()

type letrec_record = #{ i1 : int; i2 : int }
let[@warning "-26"] e2 = let rec x = #{ i1 = 1; i2 = y } and y = 42 in ()
[%%expect{|
val e1 : unit = ()
type letrec_record = #{ i1 : int; i2 : int; }
Line 4, characters 37-56:
4 | let[@warning "-26"] e2 = let rec x = #{ i1 = 1; i2 = y } and y = 42 in ()
                                         ^^^^^^^^^^^^^^^^^^^
Error: This expression has type "letrec_record"
       but an expression was expected of type "('a : value)"
       The layout of letrec_record is value & value
         because of the definition of letrec_record at line 3, characters 0-44.
       But the layout of letrec_record must be a sublayout of value
         because it's the type of the recursive variable x.
|}]

(* Unboxed records of kind value are also disallowed: *)
type letrec_record = #{ i : int }
let e2 = let rec x = #{ i = y } and y = 42 in ()
[%%expect{|
type letrec_record = #{ i : int; }
Line 2, characters 21-31:
2 | let e2 = let rec x = #{ i = y } and y = 42 in ()
                         ^^^^^^^^^^
Error: This kind of expression is not allowed as right-hand side of "let rec"
|}]

(* This example motivates having a check in [type_let], because
   [Value_rec_check] is not set up to reject it, but we don't support even this
   limited form of unboxed let rec (yet). *)
type letrec_simple = #{ i1 : int; i2 : int }
let _ = let rec _x = #{ i1 = 3; i2 = 10 } and _y = 42 in 42
[%%expect{|
type letrec_simple = #{ i1 : int; i2 : int; }
Line 2, characters 21-41:
2 | let _ = let rec _x = #{ i1 = 3; i2 = 10 } and _y = 42 in 42
                         ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "letrec_simple"
       but an expression was expected of type "('a : value)"
       The layout of letrec_simple is value & value
         because of the definition of letrec_simple at line 1, characters 0-44.
       But the layout of letrec_simple must be a sublayout of value
         because it's the type of the recursive variable _x.
|}]

(**********************************************************)
(* Test 10: unboxed products not allowed in [@@unboxed] declarations (yet) *)

type unboxed_record = #{ i1 : int; i2 : int }
type t = A of unboxed_record [@@unboxed]
[%%expect{|
type unboxed_record = #{ i1 : int; i2 : int; }
Line 2, characters 9-28:
2 | type t = A of unboxed_record [@@unboxed]
             ^^^^^^^^^^^^^^^^^^^
Error: Type "unboxed_record" has layout "value & value".
       Unboxed variants may not yet contain types of this layout.
|}]

type ('a : value & value) t = A of { x : 'a } [@@unboxed]
[%%expect{|
Line 1, characters 37-43:
1 | type ('a : value & value) t = A of { x : 'a } [@@unboxed]
                                         ^^^^^^
Error: Type "'a" has layout "value & value".
       [@@unboxed] inlined records may not yet contain types of this layout.
|}]

type unboxed_inline_record = #{ i1 : int; i2 : int }
type t = A of { x : unboxed_inline_record } [@@unboxed]
[%%expect{|
type unboxed_inline_record = #{ i1 : int; i2 : int; }
Line 2, characters 16-41:
2 | type t = A of { x : unboxed_inline_record } [@@unboxed]
                    ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "unboxed_inline_record" has layout "value & value".
       [@@unboxed] inlined records may not yet contain types of this layout.
|}]

(* Unboxed records of kind value are allowed *)

type unboxed_record = #{ i : int }
type t = A of unboxed_record [@@unboxed]
[%%expect{|
type unboxed_record = #{ i : int; }
type t = A of unboxed_record [@@unboxed]
|}]

type t = A of { x : unboxed_record } [@@unboxed]
[%%expect{|
type t = A of { x : unboxed_record; } [@@unboxed]
|}]


(**************************************)
(* Test 11: Unboxed records and arrays *)

(* You can write the type of an array of unboxed records, but not create
   one. Soon, you can do both. *)
type ('a : value & value) t1 = 'a array
type ('a : bits64 & (value & float64)) t2 = 'a array

type t3_record = #{ i : int; b : bool }
type t3 = t3_record array

type t4_inner = #{ f : float#; bo : bool option }
type t4_record = #{ s : string; inner : t4_inner }
type t4 = t4_record array
[%%expect{|
type ('a : value & value) t1 = 'a array
type ('a : bits64 & (value & float64)) t2 = 'a array
type t3_record = #{ i : int; b : bool; }
type t3 = t3_record array
type t4_inner = #{ f : float#; bo : bool option; }
type t4_record = #{ s : string; inner : t4_inner; }
type t4 = t4_record array
|}]

type array_record = #{ i1 : int; i2 : int }
let _ = [| #{ i1 = 1; i2 = 2 } |]
[%%expect{|
type array_record = #{ i1 : int; i2 : int; }
Line 2, characters 8-33:
2 | let _ = [| #{ i1 = 1; i2 = 2 } |]
            ^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed products are not yet supported with array primitives.
       Here, layout value & value was used.
|}]

type array_init_record = #{ i1 : int; i2 : int }
let _ = Array.init 3 (fun _ -> #{ i1 = 1; i2 = 2 })
[%%expect{|
type array_init_record = #{ i1 : int; i2 : int; }
Line 2, characters 31-50:
2 | let _ = Array.init 3 (fun _ -> #{ i1 = 1; i2 = 2 })
                                   ^^^^^^^^^^^^^^^^^^^
Error: This expression has type "array_init_record"
       but an expression was expected of type "('a : value)"
       The layout of array_init_record is value & value
         because of the definition of array_init_record at line 1, characters 0-48.
       But the layout of array_init_record must be a sublayout of value.
|}]

(* Arrays of unboxed records of kind value *are* allowed *)
type array_record = #{ i : int  }
let _ = [| #{ i = 1 } |]
[%%expect{|
type array_record = #{ i : int; }
- : array_record array = [|#{i = 1}|]
|}]

let _ = Array.init 3 (fun i -> #{ i })
[%%expect{|
- : array_record array = [|#{i = 0}; #{i = 1}; #{i = 2}|]
|}]

(***********************************************************)
(* Test 12: Unboxed products are not allowed as class args *)

type class_arg_record = #{ a : int; b : int }
class product_instance_variable x =
  let sum = let #{ a; b } = x in a + b in
  object
    method y = sum
  end;;
[%%expect{|
type class_arg_record = #{ a : int; b : int; }
Line 3, characters 28-29:
3 |   let sum = let #{ a; b } = x in a + b in
                                ^
Error: This expression has type "('a : value)"
       but an expression was expected of type "class_arg_record"
       The layout of class_arg_record is value & value
         because of the definition of class_arg_record at line 1, characters 0-45.
       But the layout of class_arg_record must be a sublayout of value
         because it's the type of a term-level argument to a class constructor.
|}]

(* But unboxed records of kind value are: *)
type class_arg_record = #{ a : string }
class product_instance_variable x =
  let s = let #{ a } = x in a in
  object
    method y = s
  end;;
[%%expect{|
type class_arg_record = #{ a : string; }
class product_instance_variable :
  class_arg_record -> object method y : string end
|}]


(*****************************************)
(* Test 13: No lazy unboxed products yet *)

type lazy_record = #{ i1 : int; i2 : int }
let x = lazy #{ i1 = 1; i2 = 2 }
[%%expect{|
type lazy_record = #{ i1 : int; i2 : int; }
Line 2, characters 13-32:
2 | let x = lazy #{ i1 = 1; i2 = 2 }
                 ^^^^^^^^^^^^^^^^^^^
Error: This expression has type "lazy_record"
       but an expression was expected of type "('a : value)"
       The layout of lazy_record is value & value
         because of the definition of lazy_record at line 1, characters 0-42.
       But the layout of lazy_record must be a sublayout of value
         because it's the type of a lazy expression.
|}]

type lazy_t_record = #{ i1 : int; i2 : int }
type t = lazy_t_record lazy_t
[%%expect{|
type lazy_t_record = #{ i1 : int; i2 : int; }
Line 2, characters 9-22:
2 | type t = lazy_t_record lazy_t
             ^^^^^^^^^^^^^
Error: This type "lazy_t_record" should be an instance of type "('a : value)"
       The layout of lazy_t_record is value & value
         because of the definition of lazy_t_record at line 1, characters 0-44.
       But the layout of lazy_t_record must be a sublayout of value
         because the type argument of lazy_t has layout value.
|}]

(* Again, unboxed records of kind value can be: *)

type t = #{ i : int }
let x = lazy #{ i = 1 }
[%%expect{|
type t = #{ i : int; }
val x : t lazy_t = <lazy>
|}]

type t2 = t lazy_t
[%%expect{|
type t2 = t lazy_t
|}]

(*********************************************)
(* Test 14: Unboxed records can't be coerced *)

type t = private int

type coerce_record = #{ t1 : t; t2 : t }
type coerce_int_record = #{ i1 : int; i2 : int }
let f (x : coerce_record) =
  let #{ i1 = a; i2 = b } = (x :> coerce_int_record) in a + b
[%%expect{|
type t = private int
type coerce_record = #{ t1 : t; t2 : t; }
type coerce_int_record = #{ i1 : int; i2 : int; }
Line 6, characters 28-52:
6 |   let #{ i1 = a; i2 = b } = (x :> coerce_int_record) in a + b
                                ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "coerce_record" is not a subtype of "coerce_int_record"
|}]

(************************************************)
(* Test 15: Not allowed as an optional argument *)

type optional_record = #{ i1 : int; i2 : int }
let f_optional_urecord ?(x = #{ i1 = 1; i2 = 2 }) () = x
[%%expect{|
type optional_record = #{ i1 : int; i2 : int; }
Line 2, characters 29-48:
2 | let f_optional_urecord ?(x = #{ i1 = 1; i2 = 2 }) () = x
                                 ^^^^^^^^^^^^^^^^^^^
Error: This expression has type "optional_record"
       but an expression was expected of type "('a : value)"
       The layout of optional_record is value & value
         because of the definition of optional_record at line 1, characters 0-46.
       But the layout of optional_record must be a sublayout of value
         because the type argument of option has layout value.
|}]

(******************************)
(* Test 16: Decomposing [any] *)

type ('a : value) u = U of 'a [@@unboxed]
type ('a : value) t = #{ u1 : 'a u; u2 : 'a u }

type ('a : any mod global) needs_any_mod_global

type should_work = int t needs_any_mod_global
[%%expect{|
type 'a u = U of 'a [@@unboxed]
type 'a t = #{ u1 : 'a u; u2 : 'a u; }
type ('a : any mod global) needs_any_mod_global
type should_work = int t needs_any_mod_global
|}]

type should_fail = string t needs_any_mod_global
[%%expect{|
Line 1, characters 19-27:
1 | type should_fail = string t needs_any_mod_global
                       ^^^^^^^^
Error: This type "string t" should be an instance of type "('a : any mod global)"
       The kind of string t is immutable_data
         because it is the primitive type string.
       But the kind of string t must be a subkind of any mod global
         because of the definition of needs_any_mod_global at line 4, characters 0-47.
|}]

type ('a : any mod external_) t

type s_record = #{ i1 : int; s : string; i2 : int }
type s = s_record t
[%%expect{|
type ('a : any mod external_) t
type s_record = #{ i1 : int; s : string; i2 : int; }
Line 4, characters 9-17:
4 | type s = s_record t
             ^^^^^^^^
Error: This type "s_record" should be an instance of type
         "('a : any mod external_)"
       The kind of s_record is immutable_data
         because it is the primitive type string.
       But the kind of s_record must be a subkind of any mod external_
         because of the definition of t at line 1, characters 0-31.
|}]
(* CR layouts v7.1: Both the above have very bad error messages. *)
