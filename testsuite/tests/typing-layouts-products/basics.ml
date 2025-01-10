(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 {
   flags = "-extension layouts_beta";
   expect;
 }
*)

open Stdlib_upstream_compatible

(****************************************************)
(* Test 1: Basic unboxed product layouts and types. *)

type t1 : float64 & value
type t2 = #(string * float# * int)
type t2 = #{ s : string; f : float#; i : int }
[%%expect{|
type t1 : float64 & value
type t2 = #(string * float# * int)
type t2 = #{ s : string; f : float#; i : int; }
|}]

(* You can put unboxed and normal products inside unboxed products *)
type t3 : value & (bits64 & (value & float32))
type t4 = #(string * #(int * (bool * int) * char option))
[%%expect{|
type t3 : value & (bits64 & (value & float32))
type t4 = #(string * #(int * (bool * int) * char option))
|}]

type t4_inner2 = #{ b : bool; i : int }
type t4_inner = #{ i : int; t4_inner2 : t4_inner2; co : char option }
type t4 = #{ s : string; t4_inner : t4_inner }
[%%expect{|
type t4_inner2 = #{ b : bool; i : int; }
type t4_inner = #{ i : int; t4_inner2 : t4_inner2; co : char option; }
type t4 = #{ s : string; t4_inner : t4_inner; }
|}]

(* But you can't put unboxed products into normal tuples (yet) *)
type t_nope = string * #(string * bool)
[%%expect{|
Line 1, characters 23-39:
1 | type t_nope = string * #(string * bool)
                           ^^^^^^^^^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of "#(string * bool)" is value & value
         because it is an unboxed tuple.
       But the layout of "#(string * bool)" must be a sublayout of value
         because it's the type of a tuple element.
|}]

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

type t1 : float64 & value = #(float# * bool)
type t2 : value & (float64 & value) = #(string option * t1)
[%%expect{|
type t1 = #(float# * bool)
type t2 = #(string option * t1)
|}]

type t1 : float64 & value = #{ f : float#; b : bool }
type t2 : value & (float64 & value) = #{ so : string option ; t1 : t1 }
[%%expect{|
type t1 = #{ f : float#; b : bool; }
type t2 = #{ so : string option; t1 : t1; }
|}]

type t2_wrong : value & float64 & value = #(string option * t1)
[%%expect{|
Line 1, characters 0-63:
1 | type t2_wrong : value & float64 & value = #(string option * t1)
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The layout of type "#(string option * t1)" is value & (float64 & value)
         because it is an unboxed tuple.
       But the layout of type "#(string option * t1)" must be a sublayout of
         value & float64 & value
         because of the definition of t2_wrong at line 1, characters 0-63.
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
type t4 = #(int * int64#) t3
type t5 = t4 t3
[%%expect{|
type ('a : value & bits64) t3 = 'a
type t4 = #(int * int64#) t3
type t5 = t4 t3
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

type t4_wrong = #(int * int) t3
[%%expect{|
Line 1, characters 16-28:
1 | type t4_wrong = #(int * int) t3
                    ^^^^^^^^^^^^
Error: This type "#(int * int)" should be an instance of type
         "('a : value & bits64)"
       The layout of #(int * int) is value & value
         because it is an unboxed tuple.
       But the layout of #(int * int) must be a sublayout of value & bits64
         because of the definition of t3 at line 1, characters 0-34.
|}]
(* CR layouts v7.1: The above error should identify the component of the product
   that is problematic. *)

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

type t9 = #(int * int64#) t7
type t10 = bool t6
[%%expect{|
type t9 = #(int * int64#) t7
Line 2, characters 11-15:
2 | type t10 = bool t6
               ^^^^
Error: This type "bool" should be an instance of type "('a : value & bits64)"
       The layout of bool is value
         because it is the primitive type bool.
       But the layout of bool must be a sublayout of value & bits64
         because of the definition of t6 at line 1, characters 0-37.
|}]

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
         because it is the primitive type bool.
       But the layout of bool must be a sublayout of value & bits64
         because of the definition of t6 at line 1, characters 0-37.
|}]

type ('a : value & bits64) t6_wrong = 'a t7_wrong
and 'a t7_wrong = { x : #(int * int64) t6_wrong }
[%%expect{|
Line 2, characters 24-38:
2 | and 'a t7_wrong = { x : #(int * int64) t6_wrong }
                            ^^^^^^^^^^^^^^
Error: This type "#(int * int64)" should be an instance of type
         "('a : value & bits64)"
       The layout of #(int * int64) is value & value
         because it is an unboxed tuple.
       But the layout of #(int * int64) must be a sublayout of value & bits64
         because of the annotation on 'a in the declaration of the type
                                      t6_wrong.
|}]
(* CR layouts v7.1: The above error should identify the component of the product
   that is problematic. *)

type t6_wrong_inner_record = #{ i : int; i64 : int64 }
and ('a : value & bits64) t6_wrong = 'a t7_wrong
and 'a t7_wrong = { x : t6_wrong_inner_record t6_wrong }
[%%expect{|
Line 1, characters 0-54:
1 | type t6_wrong_inner_record = #{ i : int; i64 : int64 }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error:
       The layout of t6_wrong_inner_record is any & any
         because it is an unboxed record.
       But the layout of t6_wrong_inner_record must be a sublayout of
         value & bits64
         because of the annotation on 'a in the declaration of the type
                                      t6_wrong.
|}]
(* CR layouts v7.2: The above has a very bad error message. *)

(* Just like t6/t7, but with the annotation on the other (the order doesn't
   matter) *)
type 'a t11 = 'a t12
and ('a : value & bits64) t12 = { x : 'a t11 }
[%%expect{|
type ('a : value & bits64) t11 = 'a t12
and ('a : value & bits64) t12 = { x : 'a t11; }
|}]

type 'a t11 = 'a t12
and ('a : value & bits64) t12 = { x : 'a t11 }
[%%expect{|
type ('a : value & bits64) t11 = 'a t12
and ('a : value & bits64) t12 = { x : 'a t11; }
|}]

(* You can make a universal variable have a product layout, but you have to ask
   for it *)
type ('a : float64 & value) t = 'a

let f_uvar_good : ('a : float64 & value) . 'a -> 'a t = fun x -> x
let f_uvar_ok : 'a -> 'a t = fun x -> x

let f_uvar_bad : 'a . 'a -> 'a t = fun x -> x
[%%expect{|
type ('a : float64 & value) t = 'a
val f_uvar_good : ('a : float64 & value). 'a -> 'a t = <fun>
val f_uvar_ok : ('a : float64 & value). 'a -> 'a t = <fun>
Line 6, characters 28-30:
6 | let f_uvar_bad : 'a . 'a -> 'a t = fun x -> x
                                ^^
Error: This type "('a : value)" should be an instance of type
         "('b : float64 & value)"
       The layout of 'a is value
         because it is or unifies with an unannotated universal variable.
       But the layout of 'a must overlap with float64 & value
         because of the definition of t at line 1, characters 0-34.
|}]

(*********************************************************************)
(* Test 3: Unboxed products are allowed in function args and returns *)

type t1 = #(int * bool) -> #(int * float# * #(int64# * string option))
type t2 : value & float64
type t3 : value & (float64 & immediate) & float64
type t4 = t2 -> (t3 -> t3) -> t2
[%%expect{|
type t1 = #(int * bool) -> #(int * float# * #(int64# * string option))
type t2 : value & float64
type t3 : value & (float64 & value) & float64
type t4 = t2 -> (t3 -> t3) -> t2
|}]

let f_make_an_unboxed_tuple (x : string) (y : float#) = #(y, x)

let f_pull_apart_an_unboxed_tuple (x : #(string * #(float# * float#))) =
  match x with
  | #(s, #(f1, f2)) ->
    if s = "mul" then
      Float_u.mul f1 f2
    else
      Float_u.add f1 f2
[%%expect{|
val f_make_an_unboxed_tuple : string -> float# -> #(float# * string) = <fun>
val f_pull_apart_an_unboxed_tuple :
  #(string * #(float# * float#)) -> Stdlib_upstream_compatible.Float_u.t =
  <fun>
|}]

let f_mix_up_an_unboxed_tuple x =
  let #(a, b, #(c, #(d, e)), f) = x in
  #(b, #(c, (f, e)), a, d)
[%%expect{|
val f_mix_up_an_unboxed_tuple :
  #('a * 'b * #('c * #('d * 'e)) * 'f) -> #('b * #('c * ('f * 'e)) * 'a * 'd) =
  <fun>
|}]

let f_take_a_few_unboxed_tuples x1 x2 x3 x4 x5 =
  let #(a, b) = x1 in
  let #(d, e) = x3 in
  let #(g, h) = x5 in
  #(h, g, x4, e, d, x2, b, a)
[%%expect{|
val f_take_a_few_unboxed_tuples :
  #('a * 'b) ->
  'c ->
  #('d * 'e) -> 'f -> #('g * 'h) -> #('h * 'g * 'f * 'e * 'd * 'c * 'b * 'a) =
  <fun>
|}]

(* Unboxed records version of the same test *)

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
  type take_few_output =
    #{ h : h; g2 : g; x4 : f; e2 : e; d : d; x2 : c; b : b; a2 : a }

  let f_take_a_few_unboxed_records (x1 : take_few_input1) x2
        (x3 : take_few_input3) x4 (x5 : take_few_input5) =
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

(***************************************************)
(* Test 4: Unboxed products don't go in structures *)

type poly_var_type = [ `Foo of #(int * bool) ]
[%%expect{|
Line 1, characters 31-44:
1 | type poly_var_type = [ `Foo of #(int * bool) ]
                                   ^^^^^^^^^^^^^
Error: Polymorphic variant constructor argument types must have layout value.
       The layout of "#(int * bool)" is value & value
         because it is an unboxed tuple.
       But the layout of "#(int * bool)" must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}]

let poly_var_term = `Foo #(1,2)
[%%expect{|
Line 1, characters 25-31:
1 | let poly_var_term = `Foo #(1,2)
                             ^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value_or_null)"
       The layout of #('a * 'b) is '_representable_layout_1 & '_representable_layout_2
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because it's the type of the field of a polymorphic variant.
|}]

type tuple_type = (int * #(bool * float#))
[%%expect{|
Line 1, characters 25-41:
1 | type tuple_type = (int * #(bool * float#))
                             ^^^^^^^^^^^^^^^^
Error: Tuple element types must have layout value.
       The layout of "#(bool * float#)" is value & float64
         because it is an unboxed tuple.
       But the layout of "#(bool * float#)" must be a sublayout of value
         because it's the type of a tuple element.
|}]

let tuple_term = ("hi", #(1, 2))
[%%expect{|
Line 1, characters 24-31:
1 | let tuple_term = ("hi", #(1, 2))
                            ^^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value_or_null)"
       The layout of #('a * 'b) is '_representable_layout_3 & '_representable_layout_4
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because it's the type of a tuple element.
|}]

type record = { x : #(int * bool) }
[%%expect{|
Line 1, characters 0-35:
1 | type record = { x : #(int * bool) }
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "#(int * bool)" has layout "value & value".
       Records may not yet contain types of this layout.
|}]

type inlined_record = A of { x : #(int * bool) }
[%%expect{|
Line 1, characters 22-48:
1 | type inlined_record = A of { x : #(int * bool) }
                          ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Type "#(int * bool)" has layout "value & value".
       Inlined records may not yet contain types of this layout.
|}]

type variant = A of #(int * bool)
[%%expect{|
Line 1, characters 15-33:
1 | type variant = A of #(int * bool)
                   ^^^^^^^^^^^^^^^^^^
Error: Type "#(int * bool)" has layout "value & value".
       Variants may not yet contain types of this layout.
|}]

module type S = sig
  val x : #(int * bool)
end
[%%expect{|
Line 2, characters 10-23:
2 |   val x : #(int * bool)
              ^^^^^^^^^^^^^
Error: This type signature for "x" is not a value type.
       The layout of type #(int * bool) is value & value
         because it is an unboxed tuple.
       But the layout of type #(int * bool) must be a sublayout of value
         because it's the type of something stored in a module structure.
|}]

module M = struct
  let x = #(1, 2)
end
[%%expect{|
Line 2, characters 6-7:
2 |   let x = #(1, 2)
          ^
Error: Types of top-level module bindings must have layout "value", but
       the type of "x" has layout "value & value".
|}]

type object_type = < x : #(int * bool) >
[%%expect{|
Line 1, characters 21-38:
1 | type object_type = < x : #(int * bool) >
                         ^^^^^^^^^^^^^^^^^
Error: Object field types must have layout value.
       The layout of "#(int * bool)" is value & value
         because it is an unboxed tuple.
       But the layout of "#(int * bool)" must be a sublayout of value
         because it's the type of an object field.
|}]

let object_term = object val x = #(1, 2) end
[%%expect{|
Line 1, characters 29-30:
1 | let object_term = object val x = #(1, 2) end
                                 ^
Error: Variables bound in a class must have layout value.
       The layout of x is value & value
         because it is an unboxed tuple.
       But the layout of x must be a sublayout of value
         because it's the type of a class field.
|}]

class class_ =
  object
    method x = #(1,2)
  end
[%%expect{|
Line 3, characters 15-21:
3 |     method x = #(1,2)
                   ^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value)"
       The layout of #('a * 'b) is '_representable_layout_5 & '_representable_layout_6
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because it's the type of an object field.
|}]

let capture_in_object utup = object
  val f = fun () ->
    let #(x,y) = utup in
    x + y
end;;
[%%expect{|
Line 3, characters 17-21:
3 |     let #(x,y) = utup in
                     ^^^^
Error: This expression has type "('a : value_or_null)"
       but an expression was expected of type "#('b * 'c)"
       The layout of #('a * 'b) is '_representable_layout_7 & '_representable_layout_8
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because it's the type of a variable captured in an object.
|}];;

(* Unboxed records version of the same test *)

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
       but an expression was expected of type "('a : value_or_null)"
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
let tuple_term = ("hi", #{ i = 1; i2 = 2 })
[%%expect{|
type record = #{ i : int; i2 : int; }
Line 2, characters 24-42:
2 | let tuple_term = ("hi", #{ i = 1; i2 = 2 })
                            ^^^^^^^^^^^^^^^^^^
Error: This expression has type "record" but an expression was expected of type
         "('a : value_or_null)"
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
Error: This expression has type "('a : value_or_null)"
       but an expression was expected of type "capture_record"
       The layout of capture_record is value & value
         because of the definition of capture_record at line 1, characters 0-43.
       But the layout of capture_record must be a sublayout of value
         because it's the type of a variable captured in an object.
|}];;

(****************************************************)
(* Test 5: Methods may take/return unboxed products *)

class class_with_utuple_manipulating_method =
  object
    method f #(a, b) #(c, d) = #(a + c, b + d)
  end
[%%expect{|
class class_with_utuple_manipulating_method :
  object method f : #(int * int) -> #(int * int) -> #(int * int) end
|}]

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
module type S_coherence_deep = sig
  type t1 : any
  type t2 = #(int * t1)
end

module type S_coherence_deep' = S_coherence_deep with type t1 = float#

module F(X : S_coherence_deep') = struct
  type r : value & float64 = X.t2
end
[%%expect{|
module type S_coherence_deep = sig type t1 : any type t2 = #(int * t1) end
module type S_coherence_deep' =
  sig type t1 = float# type t2 = #(int * t1) end
module F : functor (X : S_coherence_deep') -> sig type r = X.t2 end
|}]

module type S_coherence_deeper = sig
  type t1 : any
  type t2 = #(int * t1)
  type t3 = #(t2 * bool * int64#)
  type t4 = #(float# * t3 * int)
end

module type S_coherence_deeper' = S_coherence_deeper with type t1 = float#

module F(X : S_coherence_deeper') = struct
  type r : float64 & ((value & float64) & value & bits64) & value = X.t4
end
[%%expect{|
module type S_coherence_deeper =
  sig
    type t1 : any
    type t2 = #(int * t1)
    type t3 = #(t2 * bool * int64#)
    type t4 = #(float# * t3 * int)
  end
module type S_coherence_deeper' =
  sig
    type t1 = float#
    type t2 = #(int * t1)
    type t3 = #(t2 * bool * int64#)
    type t4 = #(float# * t3 * int)
  end
module F : functor (X : S_coherence_deeper') -> sig type r = X.t4 end
|}]

(* Like the above, but hitting the nested expansion case in
   [constrain_type_jkind] *)
module type S_constrain_type_jkind_deep = sig
  type t1 : any
  type t2 = #(int * t1)
end

module type S_constrain_type_jkind_deep' =
  S_constrain_type_jkind_deep with type t1 = float#

type ('a : value & float64) t_constraint

module F(X : S_constrain_type_jkind_deep') = struct
  type r = X.t2 t_constraint
end
[%%expect{|
module type S_constrain_type_jkind_deep =
  sig type t1 : any type t2 = #(int * t1) end
module type S_constrain_type_jkind_deep' =
  sig type t1 = float# type t2 = #(int * t1) end
type ('a : value & float64) t_constraint
module F :
  functor (X : S_constrain_type_jkind_deep') ->
    sig type r = X.t2 t_constraint end
|}]

module type S_constrain_type_jkind_deeper = sig
  type t1 : any
  type t2 = #(int * t1)
  type t3 = #(t2 * bool * int64#)
  type t4 = #(float# * t3 * int)
end

module type S_constrain_type_jkind_deeper' =
  S_constrain_type_jkind_deeper with type t1 = float#

type ('a : float64 & ((value & float64) & value & bits64) & value) t_constraint

module F(X : S_constrain_type_jkind_deeper') = struct
  type r = X.t4 t_constraint
end
[%%expect{|
module type S_constrain_type_jkind_deeper =
  sig
    type t1 : any
    type t2 = #(int * t1)
    type t3 = #(t2 * bool * int64#)
    type t4 = #(float# * t3 * int)
  end
module type S_constrain_type_jkind_deeper' =
  sig
    type t1 = float#
    type t2 = #(int * t1)
    type t3 = #(t2 * bool * int64#)
    type t4 = #(float# * t3 * int)
  end
type ('a : float64 & ((value & float64) & value & bits64) & value)
     t_constraint
module F :
  functor (X : S_constrain_type_jkind_deeper') ->
    sig type r = X.t4 t_constraint end
|}]

(* This typechecks for unboxed tuples, but fail for [@@unboxed], unboxed, and
   boxed records, in the same way as below.

   CR layouts v7.2: These should typecheck for all record forms.
*)
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

module type S_coherence_deep = sig
  type t1 : any
  type t2 = { t1 : t1 } [@@unboxed]
end
[%%expect{|
Line 3, characters 14-21:
3 |   type t2 = { t1 : t1 } [@@unboxed]
                  ^^^^^^^
Error: [@@unboxed] record element types must have a representable layout.
       The layout of t1/2 is any
         because of the definition of t1 at line 2, characters 2-15.
       But the layout of t1/2 must be representable
         because it is the type of record field t1.
|}]

(*************************************************)
(* Test 7: modal kinds for unboxed product types *)

let f_external_utuple_mode_crosses_local_1
  : local_ #(int * int) -> #(int * int) = fun x -> x
[%%expect{|
val f_external_utuple_mode_crosses_local_1 :
  local_ #(int * int) -> #(int * int) = <fun>
|}]

let f_internal_utuple_does_not_mode_cross_local_1
  : local_ #(int * string) -> #(int * string) = fun x -> x
[%%expect{|
Line 2, characters 57-58:
2 |   : local_ #(int * string) -> #(int * string) = fun x -> x
                                                             ^
Error: This value escapes its region.
|}]

let f_external_utuple_mode_crosses_local_2
  : local_ #(int * #(bool * int)) -> #(int * #(bool * int)) = fun x -> x
[%%expect{|
val f_external_utuple_mode_crosses_local_2 :
  local_ #(int * #(bool * int)) -> #(int * #(bool * int)) = <fun>
|}]

let f_internal_utuple_does_not_mode_cross_local_2
  : local_ #(int * #(bool * string)) -> #(int * #(bool * string)) = fun x -> x
[%%expect{|
Line 2, characters 77-78:
2 |   : local_ #(int * #(bool * string)) -> #(int * #(bool * string)) = fun x -> x
                                                                                 ^
Error: This value escapes its region.
|}]

type t = #(int * int)
let f_external_utuple_mode_crosses_local_3
  : local_ #(int * #(t * int)) -> #(int * #(t * int)) = fun x -> x
[%%expect{|
type t = #(int * int)
val f_external_utuple_mode_crosses_local_3 :
  local_ #(int * #(t * int)) -> #(int * #(t * int)) = <fun>
|}]

type t = #(string * int)
let f_internal_utuple_does_not_mode_cross_local_3
  : local_ #(int * #(t * bool)) -> #(int * #(t * bool)) = fun x -> x
[%%expect{|
type t = #(string * int)
Line 3, characters 67-68:
3 |   : local_ #(int * #(t * bool)) -> #(int * #(t * bool)) = fun x -> x
                                                                       ^
Error: This value escapes its region.
|}]

(* Unboxed records version of the same test *)

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

type t : float64 & float64
let f_external_kind_annot_mode_crosses_local_1
  : local_ t -> t = fun x -> x
[%%expect{|
type t : float64 & float64
val f_external_kind_annot_mode_crosses_local_1 : local_ t -> t = <fun>
|}]

type t : float64 & value
let f_internal_kind_annot_does_not_mode_cross_local_1
  : local_ t -> t = fun x -> x
[%%expect{|
type t : float64 & value
Line 3, characters 29-30:
3 |   : local_ t -> t = fun x -> x
                                 ^
Error: This value escapes its region.
|}]

type t : immediate & (float64 & immediate)
let f_external_kind_annot_mode_crosses_local_2
  : local_ t -> t = fun x -> x
[%%expect{|
type t : immediate & (float64 & immediate)
val f_external_kind_annot_mode_crosses_local_2 : local_ t -> t = <fun>
|}]

type t : immediate & (value & float64)
let f_internal_kind_annot_does_not_mode_cross_local_2
  : local_ t -> t = fun x -> x
[%%expect{|
type t : value & (value & float64)
Line 3, characters 29-30:
3 |   : local_ t -> t = fun x -> x
                                 ^
Error: This value escapes its region.
|}]

(*********************)
(* Test 9: externals *)

type t_product : value & value

external ext_tuple_arg : #(int * bool) -> int = "foo" "bar"
[%%expect{|
type t_product : value & value
Line 3, characters 25-45:
3 | external ext_tuple_arg : #(int * bool) -> int = "foo" "bar"
                             ^^^^^^^^^^^^^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external ext_tuple_arg_with_attr : (#(int * bool) [@unboxed]) -> int = "foo"
[%%expect{|
Line 1, characters 36-49:
1 | external ext_tuple_arg_with_attr : (#(int * bool) [@unboxed]) -> int = "foo"
                                        ^^^^^^^^^^^^^
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

external ext_tuple_return : int -> #(int * bool) = "foo" "bar"
[%%expect{|
Line 1, characters 28-48:
1 | external ext_tuple_return : int -> #(int * bool) = "foo" "bar"
                                ^^^^^^^^^^^^^^^^^^^^
Error: The primitive [foo] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external ext_tuple_return_with_attr : int -> (#(int * bool) [@unboxed]) = "foo"
[%%expect{|
Line 1, characters 46-59:
1 | external ext_tuple_return_with_attr : int -> (#(int * bool) [@unboxed]) = "foo"
                                                  ^^^^^^^^^^^^^
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

let sum =
  let #(x,y) = id #(1,2) in
  x + y
[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
val sum : int = 3
|}]

(* Unboxed records version of the same test *)

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
(* Test 10: not allowed in let recs *)

(* An example that is allowed on tuples but not unboxed products *)
let[@warning "-26"] e1 = let rec x = (1, y) and y = 42 in ()
let[@warning "-26"] e2 = let rec x = #(1, y) and y = 42 in ()
[%%expect{|
val e1 : unit = ()
Line 2, characters 37-44:
2 | let[@warning "-26"] e2 = let rec x = #(1, y) and y = 42 in ()
                                         ^^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value_or_null)"
       The layout of #('a * 'b) is '_representable_layout_9 & '_representable_layout_10
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because it's the type of the recursive variable x.
|}]

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
       but an expression was expected of type "('a : value_or_null)"
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

(* These examples motivate having a check in [type_let], because
   [Value_rec_check] is not set up to reject it, but we don't support even this
   limited form of unboxed let rec (yet). *)
let _ = let rec _x = #(3, 10) and _y = 42 in 42
[%%expect{|
Line 1, characters 21-29:
1 | let _ = let rec _x = #(3, 10) and _y = 42 in 42
                         ^^^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value_or_null)"
       The layout of #('a * 'b) is '_representable_layout_11 & '_representable_layout_12
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because it's the type of the recursive variable _x.
|}]

type letrec_simple = #{ i1 : int; i2 : int }
let _ = let rec _x = #{ i1 = 3; i2 = 10 } and _y = 42 in 42
[%%expect{|
type letrec_simple = #{ i1 : int; i2 : int; }
Line 2, characters 21-41:
2 | let _ = let rec _x = #{ i1 = 3; i2 = 10 } and _y = 42 in 42
                         ^^^^^^^^^^^^^^^^^^^^
Error: This expression has type "letrec_simple"
       but an expression was expected of type "('a : value_or_null)"
       The layout of letrec_simple is value & value
         because of the definition of letrec_simple at line 1, characters 0-44.
       But the layout of letrec_simple must be a sublayout of value
         because it's the type of the recursive variable _x.
|}]

(**********************************************************)
(* Test 11: not allowed in [@@unboxed] declarations (yet) *)

type ('a : value & value) t = A of 'a [@@unboxed]
[%%expect{|
Line 1, characters 30-37:
1 | type ('a : value & value) t = A of 'a [@@unboxed]
                                  ^^^^^^^
Error: Type "'a" has layout "value & value".
       Unboxed variants may not yet contain types of this layout.
|}]

type t = A of #(int * int) [@@unboxed]
[%%expect{|
Line 1, characters 9-26:
1 | type t = A of #(int * int) [@@unboxed]
             ^^^^^^^^^^^^^^^^^
Error: Type "#(int * int)" has layout "value & value".
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

type t = A of { x : #(int * int) } [@@unboxed]
[%%expect{|
Line 1, characters 16-32:
1 | type t = A of { x : #(int * int) } [@@unboxed]
                    ^^^^^^^^^^^^^^^^
Error: Type "#(int * int)" has layout "value & value".
       [@@unboxed] inlined records may not yet contain types of this layout.
|}]

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
(* Test 12: Unboxed tuples and arrays *)

(* You can write the type of an array of unboxed tuples, but not create
   one. Soon, you can do both. *)
type ('a : value & value) t1 = 'a array
type ('a : bits64 & (value & float64)) t2 = 'a array
type t3 = #(int * bool) array
type t4 = #(string * #(float# * bool option)) array
[%%expect{|
type ('a : value & value) t1 = 'a array
type ('a : bits64 & (value & float64)) t2 = 'a array
type t3 = #(int * bool) array
type t4 = #(string * #(float# * bool option)) array
|}]

(* CR layouts v7.1: The error below should be improved when we move product
   arrays to beta. *)
let _ = [| #(1,2) |]
[%%expect{|
- : #(int * int) array = [|#(1, 2)|]
|}]

let _ = Array.init 3 (fun _ -> #(1,2))
[%%expect{|
Line 1, characters 31-37:
1 | let _ = Array.init 3 (fun _ -> #(1,2))
                                   ^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value)"
       The layout of #('a * 'b) is '_representable_layout_13 & '_representable_layout_14
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value.
|}]

external make : ('a : value & value) . int -> 'a -> 'a array =
  "caml_make_vect" "caml_make_vect"
[%%expect{|
Line 1, characters 16-60:
1 | external make : ('a : value & value) . int -> 'a -> 'a array =
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [caml_make_vect] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
|}]

external[@layout_poly] make : ('a : any_non_null) . int -> 'a -> 'a array =
  "caml_make_vect"

let _ = make 3 #(1,2)
[%%expect{|
Lines 1-2, characters 0-18:
1 | external[@layout_poly] make : ('a : any_non_null) . int -> 'a -> 'a array =
2 |   "caml_make_vect"
Error: Attribute "[@layout_poly]" can only be used on built-in primitives.
|}]

(* CR layouts v7.1: The two errors below should be improved when we move product
   arrays to beta. *)
external[@layout_poly] array_get : ('a : any_non_null) . 'a array -> int -> 'a =
  "%array_safe_get"
let f x : #(int * int) = array_get x 3
[%%expect{|
external array_get : ('a : any_non_null). 'a array -> int -> 'a
  = "%array_safe_get" [@@layout_poly]
val f : #(int * int) array -> #(int * int) = <fun>
|}]

external[@layout_poly] array_set : ('a : any_non_null) . 'a array -> int -> 'a -> unit =
  "%array_safe_set"
let f x = array_set x 3 #(1,2)
[%%expect{|
external array_set : ('a : any_non_null). 'a array -> int -> 'a -> unit
  = "%array_safe_set" [@@layout_poly]
val f : #(int * int) array -> unit = <fun>
|}]

(* You can write the type of an array of unboxed records and create one. *)
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
- : array_record array = [|#{i1 = 1; i2 = 2}|]
|}]

(* However, such records can't be passed to [Array.init]. *)
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

(* Arrays of unboxed records of kind value *are* allowed in all cases *)
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
(* Test 13: Unboxed products are not allowed as class args *)

class product_instance_variable x =
  let sum = let #(a,b) = x in a + b in
  object
    method y = sum
  end;;
[%%expect{|
Line 2, characters 25-26:
2 |   let sum = let #(a,b) = x in a + b in
                             ^
Error: This expression has type "('a : value)"
       but an expression was expected of type "#('b * 'c)"
       The layout of #('a * 'b) is '_representable_layout_15 & '_representable_layout_16
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because it's the type of a term-level argument to a class constructor.
|}]

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
(* Test 14: No lazy unboxed products yet *)

let x = lazy #(1,2)

[%%expect{|
Line 1, characters 13-19:
1 | let x = lazy #(1,2)
                 ^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value)"
       The layout of #('a * 'b) is '_representable_layout_17 & '_representable_layout_18
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because it's the type of a lazy expression.
|}]

type t = #(int * int) lazy_t

[%%expect{|
Line 1, characters 9-21:
1 | type t = #(int * int) lazy_t
             ^^^^^^^^^^^^
Error: This type "#(int * int)" should be an instance of type "('a : value)"
       The layout of #(int * int) is value & value
         because it is an unboxed tuple.
       But the layout of #(int * int) must be a sublayout of value
         because the type argument of lazy_t has layout value.
|}]

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


(***************************************)
(* Test 15: Coercions work covariantly *)

type t = private int

let f (x : #(t * t)) =
  let #(a,b) = (x :> #(int * int)) in a + b
[%%expect{|
type t = private int
val f : #(t * t) -> int = <fun>
|}]

let g (x : #(int * int)) = f (x :> #(t * t))
[%%expect{|
Line 1, characters 29-44:
1 | let g (x : #(int * int)) = f (x :> #(t * t))
                                 ^^^^^^^^^^^^^^^
Error: Type "#(int * int)" is not a subtype of "#(t * t)"
       Type "int" is not a subtype of "t"
|}]

(* Unboxed records can't be coerced *)

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
(* Test 16: Not allowed as an optional argument *)

let f_optional_utuple ?(x = #(1,2)) () = x
[%%expect{|
Line 1, characters 28-34:
1 | let f_optional_utuple ?(x = #(1,2)) () = x
                                ^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value)"
       The layout of #('a * 'b) is '_representable_layout_19 & '_representable_layout_20
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because the type argument of option has layout value.
|}]

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
(* Test 17: Decomposing [any] *)

type ('a : value) u = U of 'a [@@unboxed]
type ('a : value) t = #('a u * 'a u)

type ('a : any mod global) needs_any_mod_global

type should_work = int t needs_any_mod_global

[%%expect{|
type 'a u = U of 'a [@@unboxed]
type 'a t = #('a u * 'a u)
type ('a : any mod global) needs_any_mod_global
type should_work = int t needs_any_mod_global
|}]

type should_fail = string t needs_any_mod_global

[%%expect{|
Line 1, characters 19-27:
1 | type should_fail = string t needs_any_mod_global
                       ^^^^^^^^
Error: This type "string t" = "#(string u * string u)"
       should be an instance of type "('a : any mod global)"
       The kind of string t is value & value
         because it is an unboxed tuple.
       But the kind of string t must be a subkind of any mod global
         because of the definition of needs_any_mod_global at line 4, characters 0-47.
|}]

type ('a : any mod external_) t

type s = #(int * string * int) t
[%%expect{|
type ('a : any mod external_) t
Line 3, characters 9-30:
3 | type s = #(int * string * int) t
             ^^^^^^^^^^^^^^^^^^^^^
Error: This type "#(int * string * int)" should be an instance of type
         "('a : any mod external_)"
       The kind of #(int * string * int) is
         immutable_data & immutable_data & immutable_data
         because it is an unboxed tuple.
       But the kind of #(int * string * int) must be a subkind of
         any mod external_
         because of the definition of t at line 1, characters 0-31.
|}]
(* CR layouts v7.1: The appearance of [immutable_data] above is regrettable. *)

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
       The kind of string t is value & value
         because of the definition of t at line 2, characters 0-47.
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
       The kind of s_record is
         immutable_data & immutable_data & immutable_data
         because of the definition of s_record at line 3, characters 0-51.
       But the kind of s_record must be a subkind of any mod external_
         because of the definition of t at line 1, characters 0-31.
|}]
(* CR layouts v7.1: Both the above have very bad error messages. *)

(********************************************)
(* Test 18: Subkinding with sorts and [any] *)

(* CR layouts: Change to use [any] instead of [any_non_null] when doing so
   won't cause trouble with the [alpha] check. *)

(* test intersection *)
let rec f : ('a : any_non_null & value). unit -> 'a -> 'a = fun () -> f ()

let g (x : 'a) = f () x

[%%expect{|
val f : ('a : any_non_null & value). unit -> 'a -> 'a = <fun>
val g : ('a : value & value). 'a -> 'a = <fun>
|}]

(* test subjkinding *)
let rec f : ('a : any_non_null & value). unit -> 'a -> 'a = fun () -> f ()

let g (type a) (x : a) = f () x

[%%expect{|
val f : ('a : any_non_null & value). unit -> 'a -> 'a = <fun>
Line 3, characters 30-31:
3 | let g (type a) (x : a) = f () x
                                  ^
Error: This expression has type "a" but an expression was expected of type
         "('a : '_representable_layout_21 & value)"
       The layout of a is value
         because it is or unifies with an unannotated universal variable.
       But the layout of a must be representable
         because we must know concretely how to pass a function argument.
|}]
