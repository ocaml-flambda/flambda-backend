(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 {
   expect;
 }
*)

open Stdlib_upstream_compatible

(**********************************************************)
(* Test 1: Basic unboxed product layouts and tuple types. *)

type t1 : float64 & value
type t2 = #(string * float# * int)
[%%expect{|
type t1 : float64 & value
type t2 = #(string * float# * int)
|}]

(* You can put unboxed and normal products inside unboxed products *)
type t3 : value & (bits64 & (value & float32))
type t4 = #(string * #(int * (bool * int) * char option))
[%%expect{|
type t3 : value & (bits64 & (value & float32))
type t4 = #(string * #(int * (bool * int) * char option))
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

(********************************************)
(* Test 2: Simple kind annotations on types *)

type t1 : float64 & value = #(float# * bool)
type t2 : value & (float64 & value) = #(string option * t1)
[%%expect{|
type t1 = #(float# * bool)
type t2 = #(string option * t1)
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

type ('a : value & bits64) t3 = 'a
type t4 = #(int * int64#) t3
type t5 = t4 t3
[%%expect{|
type ('a : value & bits64) t3 = 'a
type t4 = #(int * int64#) t3
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
         because it's an enumeration variant type (all constructors are constant).
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

(* Just like t6/t7, but with the annotation on the other (the order doesn't
   matter) *)
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
type t3 : value & (float64 & immediate) & float64
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
       but an expression was expected of type "('c : value)"
       The layout of #('a * 'b) is '_representable_layout_158 & '_representable_layout_159
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
       but an expression was expected of type "('c : value)"
       The layout of #('a * 'b) is '_representable_layout_165 & '_representable_layout_166
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
       The layout of #('a * 'b) is '_representable_layout_194 & '_representable_layout_195
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
Error: This expression has type "('a : value)"
       but an expression was expected of type "#('b * 'c)"
       The layout of #('a * 'b) is '_representable_layout_206 & '_representable_layout_207
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
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

(***********************************************)
(* Test 7: modal kinds for unboxed tuple types *)

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
type t : immediate & (value & float64)
Line 3, characters 29-30:
3 |   : local_ t -> t = fun x -> x
                                 ^
Error: This value escapes its region.
|}]

(*********************)
(* Test 9: externals *)

(* CR layouts v7.1: Unboxed products should be allowed for some primitives, like
   %identity *)

type t_product : value & value

external ext_tuple_arg : #(int * bool) -> int = "foo"
[%%expect{|
type t_product : value & value
Line 3, characters 25-38:
3 | external ext_tuple_arg : #(int * bool) -> int = "foo"
                             ^^^^^^^^^^^^^
Error: Unboxed product layouts are not supported in external declarations
|}]

external ext_tuple_arg_with_attr : (#(int * bool) [@unboxed]) -> int = "foo"
[%%expect{|
Line 1, characters 36-49:
1 | external ext_tuple_arg_with_attr : (#(int * bool) [@unboxed]) -> int = "foo"
                                        ^^^^^^^^^^^^^
Error: Unboxed product layouts are not supported in external declarations
|}]

external ext_product_arg : t_product -> int = "foo"
[%%expect{|
Line 1, characters 27-36:
1 | external ext_product_arg : t_product -> int = "foo"
                               ^^^^^^^^^
Error: Unboxed product layouts are not supported in external declarations
|}]

external ext_product_arg_with_attr : (t_product [@unboxed]) -> int = "foo"
[%%expect{|
Line 1, characters 38-47:
1 | external ext_product_arg_with_attr : (t_product [@unboxed]) -> int = "foo"
                                          ^^^^^^^^^
Error: Unboxed product layouts are not supported in external declarations
|}]

external ext_tuple_return : int -> #(int * bool) = "foo"
[%%expect{|
Line 1, characters 35-48:
1 | external ext_tuple_return : int -> #(int * bool) = "foo"
                                       ^^^^^^^^^^^^^
Error: Unboxed product layouts are not supported in external declarations
|}]

external ext_tuple_return_with_attr : int -> (#(int * bool) [@unboxed]) = "foo"
[%%expect{|
Line 1, characters 46-59:
1 | external ext_tuple_return_with_attr : int -> (#(int * bool) [@unboxed]) = "foo"
                                                  ^^^^^^^^^^^^^
Error: Unboxed product layouts are not supported in external declarations
|}]

external ext_product_return : int -> t_product = "foo"
[%%expect{|
Line 1, characters 37-46:
1 | external ext_product_return : int -> t_product = "foo"
                                         ^^^^^^^^^
Error: Unboxed product layouts are not supported in external declarations
|}]

external ext_product_return_with_attr : int -> (t_product [@unboxed]) = "foo"
[%%expect{|
Line 1, characters 48-57:
1 | external ext_product_return_with_attr : int -> (t_product [@unboxed]) = "foo"
                                                    ^^^^^^^^^
Error: Unboxed product layouts are not supported in external declarations
|}]

external[@layout_poly] id : ('a : any). 'a -> 'a = "%identity"

let sum =
  let #(x,y) = id #(1,2) in
  x + y
[%%expect{|
external id : ('a : any). 'a -> 'a = "%identity" [@@layout_poly]
Line 4, characters 18-24:
4 |   let #(x,y) = id #(1,2) in
                      ^^^^^^
Error: Unboxed product layouts are not yet supported as arguments to
       layout polymorphic externals.
       The layout of this argument is value & value.
|}]

(* CR layouts v7.1: Unboxed products should be allowed for some primitives, like
   %identity *)

(***********************************)
(* Test 9: not allowed in let recs *)

(* An example that is allowed on tuples but not unboxed tuples *)
let[@warning "-26"] e1 = let rec x = (1, y) and y = 42 in ()
let[@warning "-26"] e2 = let rec x = #(1, y) and y = 42 in ()
[%%expect{|
val e1 : unit = ()
Line 2, characters 37-44:
2 | let[@warning "-26"] e2 = let rec x = #(1, y) and y = 42 in ()
                                         ^^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value)"
       The layout of #('a * 'b) is '_representable_layout_366 & '_representable_layout_367
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because it's the type of the recursive variable x.
|}]

(* This example motivates having a check in [type_let], because
   [Value_rec_check] is not set up to reject it, but we don't support even this
   limited form of unboxed let rec (yet). *)
let _ = let rec _x = #(3, 10) and _y = 42 in 42
[%%expect{|
Line 1, characters 21-29:
1 | let _ = let rec _x = #(3, 10) and _y = 42 in 42
                         ^^^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value)"
       The layout of #('a * 'b) is '_representable_layout_373 & '_representable_layout_374
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because it's the type of the recursive variable _x.
|}]

(**********************************************************)
(* Test 10: not allowed in [@@unboxed] declarations (yet) *)

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
       Unboxed inlined records may not yet contain types of this layout.
|}]

type t = A of { x : #(int * int) } [@@unboxed]
[%%expect{|
Line 1, characters 16-32:
1 | type t = A of { x : #(int * int) } [@@unboxed]
                    ^^^^^^^^^^^^^^^^
Error: Type "#(int * int)" has layout "value & value".
       Unboxed inlined records may not yet contain types of this layout.
|}]

(**************************************)
(* Test 11: Unboxed tuples and arrays *)

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

(* CR layouts v7.1: This example demonstrates a bug in type inference that we
   should fix.  The issue is the way typing of tuple expressions works - we
   create type variables at generic level and then constrain them by the
   expected type.  This will fail if it requires to refine the kind, because we
   don't allow refining kinds at generic level.  We need to remove the
   restriction that kinds of things at generic level can't be modified, but that
   is orthogonal to unboxed tuples so we leave in this sad bug for now. *)
let _ = [| #(1,2) |]
[%%expect{|
Line 1, characters 11-17:
1 | let _ = [| #(1,2) |]
               ^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type
         "('c : '_representable_layout_397 & '_representable_layout_398)"
       The kind of #('a * 'b) is
         '_representable_layout_397 & '_representable_layout_398
         because it is an unboxed tuple.
       But the kind of #('a * 'b) must be a subkind of
         '_representable_layout_397 & '_representable_layout_398
         because it's the type of an array element.
|}]

let _ = Array.init 3 (fun _ -> #(1,2))
[%%expect{|
Line 1, characters 31-37:
1 | let _ = Array.init 3 (fun _ -> #(1,2))
                                   ^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value)"
       The layout of #('a * 'b) is '_representable_layout_404 & '_representable_layout_405
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because of layout requirements from an imported definition.
|}]

external make : ('a : value & value) . int -> 'a -> 'a array = "caml_make_vect"
[%%expect{|
Line 1, characters 46-48:
1 | external make : ('a : value & value) . int -> 'a -> 'a array = "caml_make_vect"
                                                  ^^
Error: Unboxed product layouts are not supported in external declarations
|}]

external[@layout_poly] make : ('a : any) . int -> 'a -> 'a array =
  "caml_make_vect"

let _ = make 3 #(1,2)
[%%expect{|
Lines 1-2, characters 0-18:
1 | external[@layout_poly] make : ('a : any) . int -> 'a -> 'a array =
2 |   "caml_make_vect"
Error: Attribute "[@layout_poly]" can only be used on built-in primitives.
|}]

external[@layout_poly] array_get : ('a : any) . 'a array -> int -> 'a =
  "%array_safe_get"
let f x : #(int * int) = array_get x 3
[%%expect{|
external array_get : ('a : any). 'a array -> int -> 'a = "%array_safe_get"
  [@@layout_poly]
Line 3, characters 25-38:
3 | let f x : #(int * int) = array_get x 3
                             ^^^^^^^^^^^^^
Error: Unboxed product layouts are not yet supported as arguments to
       layout polymorphic externals.
       The layout of this argument is value & value.
|}]

external[@layout_poly] array_set : ('a : any) . 'a array -> int -> 'a -> unit =
  "%array_safe_set"
let f x = array_set x 3 #(1,2)
[%%expect{|
external array_set : ('a : any). 'a array -> int -> 'a -> unit
  = "%array_safe_set" [@@layout_poly]
Line 3, characters 24-30:
3 | let f x = array_set x 3 #(1,2)
                            ^^^^^^
Error: Unboxed product layouts are not yet supported as arguments to
       layout polymorphic externals.
       The layout of this argument is value & value.
|}]


(***********************************************************)
(* Test 12: Unboxed products are not allowed as class args *)

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
       The layout of #('a * 'b) is '_representable_layout_450 & '_representable_layout_451
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because it's the type of a term-level argument to a class constructor.
|}]

(*****************************************)
(* Test 13: No lazy unboxed products yet *)

let x = lazy #(1,2)

[%%expect{|
Line 1, characters 13-19:
1 | let x = lazy #(1,2)
                 ^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value)"
       The layout of #('a * 'b) is '_representable_layout_455 & '_representable_layout_456
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

(***************************************)
(* Test 14: Coercions work covariantly *)

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

(************************************************)
(* Test 15: Not allowed as an optional argument *)

let f_optional_utuple ?(x = #(1,2)) () = x
[%%expect{|
Line 1, characters 28-34:
1 | let f_optional_utuple ?(x = #(1,2)) () = x
                                ^^^^^^
Error: This expression has type "#('a * 'b)"
       but an expression was expected of type "('c : value)"
       The layout of #('a * 'b) is '_representable_layout_485 & '_representable_layout_486
         because it is an unboxed tuple.
       But the layout of #('a * 'b) must be a sublayout of value
         because the type argument of option has layout value.
|}]
