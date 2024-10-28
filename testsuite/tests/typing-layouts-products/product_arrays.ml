(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_alpha";
 {
   expect;
 }
*)

(* Basic typing tests. Mainly we are checking that all array primitives reject
   illegally mixed tuples. *)

(* CR layouts v7.1: The PR with middle-end support for product arrays can move
   this test to beta. *)

(* CR layouts v7.1: Everywhere this file says "any_non_null" it should instead
   say any. This is caused by [any] meaning different things alpha and beta - we
   can fix it when we move this test to beta. *)

(* CR layouts v12: If we add void before killing the scannable/ignorable
   products distinction, we should test here that it's allowed in both. *)

(*******************************************************)
(* Test 1: Some allowed scannable product array types. *)
type t1 = #(int * bool option) array
type t2 = #(int * string) array
type t3 = #(string * int * int option) array

type t4 : immediate & value
type t4a = t4 array
type t5 : value & immediate
type t5a = t5 array
type t6 : value & immediate & value & immediate
type t6a = t6 array
[%%expect{|
type t1 = #(int * bool option) array
type t2 = #(int * string) array
type t3 = #(string * int * int option) array
type t4 : immediate & value
type t4a = t4 array
type t5 : value & immediate
type t5a = t5 array
type t6 : value & immediate & value & immediate
type t6a = t6 array
|}]

(*******************************************************)
(* Test 2: Some allowed ignorable product array types. *)
type t1 = #(int * int) array
type t2 = #(int * float#) array
type t3 = #(float# * int * int64# * bool) array

type t4 : immediate & immediate
type t4a = t4 array
type t5 : immediate & float64
type t5a = t5 array
type t6 : bits64 & immediate & float64 & immediate
type t6a = t6 array
[%%expect{|
type t1 = #(int * int) array
type t2 = #(int * float#) array
type t3 = #(float# * int * int64# * bool) array
type t4 : immediate & immediate
type t4a = t4 array
type t5 : immediate & float64
type t5a = t5 array
type t6 : bits64 & immediate & float64 & immediate
type t6a = t6 array
|}]

(******************************************************************************)
(* Test 3: Some array types that are allowed even though you can't make them. *)
type t1 = #(float# * string) array
type t2 = #(string * int64#) array
type t3 = #(string * int64# * int) array
type t4 = #(int * int64# * string) array

type t5 : value & float64
type t5a = t5 array
type t6 : bits64 & value
type t6a = t6 array
type t7 : value & bits64 & immediate
type t7a = t7 array
type t8 : immediate & bits64 & value
type t8a = t8 array

[%%expect{|
type t1 = #(float# * string) array
type t2 = #(string * int64#) array
type t3 = #(string * int64# * int) array
type t4 = #(int * int64# * string) array
type t5 : value & float64
type t5a = t5 array
type t6 : bits64 & value
type t6a = t6 array
type t7 : value & bits64 & immediate
type t7a = t7 array
type t8 : immediate & bits64 & value
type t8a = t8 array
|}]

(*****************************)
(* Test 4: makearray_dynamic *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] make_vect : ('a : any_non_null) . int -> 'a -> 'a array =
  "%makearray_dynamic"

let f_scannable (x : #(int * float * string)) = make_vect 42 x

let f_ignorable (x : #(float# * int * int64# * bool)) = make_vect 42 x
[%%expect{|
external make_vect : ('a : any_non_null). int -> 'a -> 'a array
  = "%makearray_dynamic" [@@layout_poly]
val f_scannable : #(int * float * string) -> #(int * float * string) array =
  <fun>
val f_ignorable :
  #(float# * int * int64# * bool) -> #(float# * int * int64# * bool) array =
  <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#)) = make_vect 42 x
[%%expect{|
Line 1, characters 37-51:
1 | let f_bad (x : #(string * float#)) = make_vect 42 x
                                         ^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external make_scannable :
  int -> #(int * float * string) -> #(int * float * string) array =
  "%makearray_dynamic"
let make_scannable_app x = make_scannable x

external make_ignorable :
  int -> #(float# * int * int64# * bool)
  -> #(float# * int * int64# * bool) array =
  "%makearray_dynamic"
let make_ignorable_app x = make_ignorable x
[%%expect{|
external make_scannable :
  int -> #(int * float * string) -> #(int * float * string) array
  = "%makearray_dynamic"
val make_scannable_app :
  int -> #(int * float * string) -> #(int * float * string) array = <fun>
external make_ignorable :
  int ->
  #(float# * int * int64# * bool) -> #(float# * int * int64# * bool) array
  = "%makearray_dynamic"
val make_ignorable_app :
  int ->
  #(float# * int * int64# * bool) -> #(float# * int * int64# * bool) array =
  <fun>
|}]

external make_bad : int -> #(string * float#) -> #(string * float#) array =
  "%makearray_dynamic"
let make_bad_app x = make_bad x
[%%expect{|
external make_bad : int -> #(string * float#) -> #(string * float#) array
  = "%makearray_dynamic"
Line 3, characters 21-29:
3 | let make_bad_app x = make_bad x
                         ^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#)) = make_vect 42 x
[%%expect{|
Line 1, characters 36-50:
1 | let f_bad (x : #(int * int32x4#)) = make_vect 42 x
                                        ^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external make_ignorable_with_vec :
  int -> #(int * int32x4#) -> #(int * int32x4#) array = "%makearray_dynamic"
let make_ignorable_with_vec_app x = make_ignorable_with_vec x
[%%expect{|
external make_ignorable_with_vec :
  int -> #(int * int32x4#) -> #(int * int32x4#) array = "%makearray_dynamic"
Line 3, characters 36-59:
3 | let make_ignorable_with_vec_app x = make_ignorable_with_vec x
                                        ^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(************************)
(* Test 5: array length *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] len : ('a : any_non_null) . 'a array -> int =
  "%array_length"

let f_scannable (x : #(int * float * string) array) = len x

let f_ignorable (x : #(float# * int * int64# * bool) array) = len x
[%%expect{|
external len : ('a : any_non_null). 'a array -> int = "%array_length"
  [@@layout_poly]
val f_scannable : #(int * float * string) array -> int = <fun>
val f_ignorable : #(float# * int * int64# * bool) array -> int = <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = len x
[%%expect{|
Line 1, characters 43-48:
1 | let f_bad (x : #(string * float#) array) = len x
                                               ^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external len_scannable : #(int * float * string) array -> int = "%array_length"
let len_scannable_app x = len_scannable x

external len_ignorable : #(float# * int * int64# * bool) array -> int =
  "%array_length"
let len_ignorable_app x = len_ignorable x
[%%expect{|
external len_scannable : #(int * float * string) array -> int
  = "%array_length"
val len_scannable_app : #(int * float * string) array -> int = <fun>
external len_ignorable : #(float# * int * int64# * bool) array -> int
  = "%array_length"
val len_ignorable_app : #(float# * int * int64# * bool) array -> int = <fun>
|}]

external len_bad : #(string * float#) array -> int = "%array_length"
let len_bad_app x = len_bad x
[%%expect{|
external len_bad : #(string * float#) array -> int = "%array_length"
Line 2, characters 20-29:
2 | let len_bad_app x = len_bad x
                        ^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) = len x
[%%expect{|
Line 1, characters 42-47:
1 | let f_bad (x : #(int * int32x4#) array) = len x
                                              ^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external len_ignorable_with_vec :
  #(int * int32x4#) array -> int = "%array_length"
let len_ignorable_with_vec_app x = len_ignorable_with_vec x
[%%expect{|
external len_ignorable_with_vec : #(int * int32x4#) array -> int
  = "%array_length"
Line 3, characters 35-59:
3 | let len_ignorable_with_vec_app x = len_ignorable_with_vec x
                                       ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(********************)
(* Test 6: safe get *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] get : ('a : any_non_null) . 'a array -> int -> 'a =
  "%array_safe_get"

let f_scannable (x : #(int * float * string) array) = get x 42
let f_ignorable (x : #(float# * int * int64# * bool) array) = get x 42
[%%expect{|
external get : ('a : any_non_null). 'a array -> int -> 'a = "%array_safe_get"
  [@@layout_poly]
val f_scannable : #(int * float * string) array -> #(int * float * string) =
  <fun>
val f_ignorable :
  #(float# * int * int64# * bool) array -> #(float# * int * int64# * bool) =
  <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = get x 42
[%%expect{|
Line 1, characters 43-51:
1 | let f_bad (x : #(string * float#) array) = get x 42
                                               ^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external get_scannable :
  #(int * float * string) array -> int -> #(int * float * string) =
  "%array_safe_get"
let get_scannable_app a i = get_scannable a i

external get_ignorable :
  #(float# * int * int64# * bool) array -> int
  -> #(float# * int * int64# * bool) =
  "%array_safe_get"
let get_ignorable_app a i = get_ignorable a i
[%%expect{|
external get_scannable :
  #(int * float * string) array -> int -> #(int * float * string)
  = "%array_safe_get"
val get_scannable_app :
  #(int * float * string) array -> int -> #(int * float * string) = <fun>
external get_ignorable :
  #(float# * int * int64# * bool) array ->
  int -> #(float# * int * int64# * bool) = "%array_safe_get"
val get_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int -> #(float# * int * int64# * bool) = <fun>
|}]

external get_bad : #(string * float#) array -> int -> #(string * float#) =
  "%array_safe_get"
let get_bad_app a i = get_bad a i
[%%expect{|
external get_bad : #(string * float#) array -> int -> #(string * float#)
  = "%array_safe_get"
Line 3, characters 22-33:
3 | let get_bad_app a i = get_bad a i
                          ^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) = get x 42
[%%expect{|
Line 1, characters 42-50:
1 | let f_bad (x : #(int * int32x4#) array) = get x 42
                                              ^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external get_ignorable_with_vec :
  #(int * int32x4#) array -> int -> #(int * int32x4#) = "%array_safe_get"
let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
[%%expect{|
external get_ignorable_with_vec :
  #(int * int32x4#) array -> int -> #(int * int32x4#) = "%array_safe_get"
Line 3, characters 37-63:
3 | let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(********************)
(* Test 7: safe set *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] set :
  ('a : any_non_null) . 'a array -> int -> 'a -> unit = "%array_safe_set"

let f_scannable (x : #(int * float * string) array) = set x 42 #(1, 2.0, "3")

let f_ignorable (x : #(float# * int * int64# * bool) array) =
  set x 42 #(#1.0, 2, #3L, true)
[%%expect{|
external set : ('a : any_non_null). 'a array -> int -> 'a -> unit
  = "%array_safe_set" [@@layout_poly]
val f_scannable : #(int * float * string) array -> unit = <fun>
val f_ignorable : #(float# * int * int64# * bool) array -> unit = <fun>
|}]

let f_bad (x : #(string * float#) array) = set x 42 #("1", #2.0)
[%%expect{|
Line 1, characters 43-64:
1 | let f_bad (x : #(string * float#) array) = set x 42 #("1", #2.0)
                                               ^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external set_scannable :
  #(int * float * string) array -> int -> #(int * float * string) -> unit =
  "%array_safe_set"
let set_scannable_app a i x = set_scannable a i x

external set_ignorable :
  #(float# * int * int64# * bool) array -> int
  -> #(float# * int * int64# * bool) -> unit =
  "%array_safe_set"
let set_ignorable_app a i x = set_ignorable a i x
[%%expect{|
external set_scannable :
  #(int * float * string) array -> int -> #(int * float * string) -> unit
  = "%array_safe_set"
val set_scannable_app :
  #(int * float * string) array -> int -> #(int * float * string) -> unit =
  <fun>
external set_ignorable :
  #(float# * int * int64# * bool) array ->
  int -> #(float# * int * int64# * bool) -> unit = "%array_safe_set"
val set_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int -> #(float# * int * int64# * bool) -> unit = <fun>
|}]

external set_bad :
  #(string * float#) array -> int -> #(string * float#) -> unit =
  "%array_safe_set"
let set_bad_app a i x = set_bad a i x
[%%expect{|
external set_bad :
  #(string * float#) array -> int -> #(string * float#) -> unit
  = "%array_safe_set"
Line 4, characters 24-37:
4 | let set_bad_app a i x = set_bad a i x
                            ^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) v = set x 42 #(1, v)
[%%expect{|
Line 1, characters 44-60:
1 | let f_bad (x : #(int * int32x4#) array) v = set x 42 #(1, v)
                                                ^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external set_ignorable_with_vec :
  #(int * int32x4#) array -> int -> #(int * int32x4#) -> unit =
  "%array_safe_set"
let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
[%%expect{|
external set_ignorable_with_vec :
  #(int * int32x4#) array -> int -> #(int * int32x4#) -> unit
  = "%array_safe_set"
Line 4, characters 39-73:
4 | let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(**********************)
(* Test 8: unsafe get *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] get : ('a : any_non_null) . 'a array -> int -> 'a =
  "%array_unsafe_get"

let f_scannable (x : #(int * float * string) array) = get x 42
let f_ignorable (x : #(float# * int * int64# * bool) array) = get x 42
[%%expect{|
external get : ('a : any_non_null). 'a array -> int -> 'a
  = "%array_unsafe_get" [@@layout_poly]
val f_scannable : #(int * float * string) array -> #(int * float * string) =
  <fun>
val f_ignorable :
  #(float# * int * int64# * bool) array -> #(float# * int * int64# * bool) =
  <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = get x 42
[%%expect{|
Line 1, characters 43-51:
1 | let f_bad (x : #(string * float#) array) = get x 42
                                               ^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external get_scannable :
  #(int * float * string) array -> int -> #(int * float * string) =
  "%array_unsafe_get"
let get_scannable_app a i = get_scannable a i

external get_ignorable :
  #(float# * int * int64# * bool) array -> int
  -> #(float# * int * int64# * bool) =
  "%array_unsafe_get"
let get_ignorable_app a i = get_ignorable a i
[%%expect{|
external get_scannable :
  #(int * float * string) array -> int -> #(int * float * string)
  = "%array_unsafe_get"
val get_scannable_app :
  #(int * float * string) array -> int -> #(int * float * string) = <fun>
external get_ignorable :
  #(float# * int * int64# * bool) array ->
  int -> #(float# * int * int64# * bool) = "%array_unsafe_get"
val get_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int -> #(float# * int * int64# * bool) = <fun>
|}]

external get_bad : #(string * float#) array -> int -> #(string * float#) =
  "%array_unsafe_get"
let get_bad_app a i = get_bad a i
[%%expect{|
external get_bad : #(string * float#) array -> int -> #(string * float#)
  = "%array_unsafe_get"
Line 3, characters 22-33:
3 | let get_bad_app a i = get_bad a i
                          ^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) = get x 42
[%%expect{|
Line 1, characters 42-50:
1 | let f_bad (x : #(int * int32x4#) array) = get x 42
                                              ^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external get_ignorable_with_vec :
  #(int * int32x4#) array -> int -> #(int * int32x4#) =
  "%array_unsafe_get"
let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
[%%expect{|
external get_ignorable_with_vec :
  #(int * int32x4#) array -> int -> #(int * int32x4#) = "%array_unsafe_get"
Line 4, characters 37-63:
4 | let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(**********************)
(* Test 9: unsafe set *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] set :
  ('a : any_non_null) . 'a array -> int -> 'a -> unit = "%array_unsafe_set"

let f_scannable (x : #(int * float * string) array) = set x 42 #(1, 2.0, "3")

let f_ignorable (x : #(float# * int * int64# * bool) array) =
  set x 42 #(#1.0, 2, #3L, true)
[%%expect{|
external set : ('a : any_non_null). 'a array -> int -> 'a -> unit
  = "%array_unsafe_set" [@@layout_poly]
val f_scannable : #(int * float * string) array -> unit = <fun>
val f_ignorable : #(float# * int * int64# * bool) array -> unit = <fun>
|}]

let f_bad (x : #(string * float#) array) = set x 42 #("1", #2.0)
[%%expect{|
Line 1, characters 43-64:
1 | let f_bad (x : #(string * float#) array) = set x 42 #("1", #2.0)
                                               ^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external set_scannable :
  #(int * float * string) array -> int -> #(int * float * string) -> unit =
  "%array_unsafe_set"
let set_scannable_app a i x = set_scannable a i x

external set_ignorable :
  #(float# * int * int64# * bool) array -> int
  -> #(float# * int * int64# * bool) -> unit =
  "%array_unsafe_set"
let set_ignorable_app a i x = set_ignorable a i x
[%%expect{|
external set_scannable :
  #(int * float * string) array -> int -> #(int * float * string) -> unit
  = "%array_unsafe_set"
val set_scannable_app :
  #(int * float * string) array -> int -> #(int * float * string) -> unit =
  <fun>
external set_ignorable :
  #(float# * int * int64# * bool) array ->
  int -> #(float# * int * int64# * bool) -> unit = "%array_unsafe_set"
val set_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int -> #(float# * int * int64# * bool) -> unit = <fun>
|}]

external set_bad :
  #(string * float#) array -> int -> #(string * float#) -> unit =
  "%array_unsafe_set"
let set_bad_app a i x = set_bad a i x
[%%expect{|
external set_bad :
  #(string * float#) array -> int -> #(string * float#) -> unit
  = "%array_unsafe_set"
Line 4, characters 24-37:
4 | let set_bad_app a i x = set_bad a i x
                            ^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) v = set x 42 #(1, v)
[%%expect{|
Line 1, characters 44-60:
1 | let f_bad (x : #(int * int32x4#) array) v = set x 42 #(1, v)
                                                ^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external set_ignorable_with_vec :
  #(int * int32x4#) array -> int -> #(int * int32x4#) -> unit =
  "%array_unsafe_set"
let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
[%%expect{|
external set_ignorable_with_vec :
  #(int * int32x4#) array -> int -> #(int * int32x4#) -> unit
  = "%array_unsafe_set"
Line 4, characters 39-73:
4 | let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(***************************************)
(* Test 10: safe get indexed by int64# *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] get : ('a : any_non_null) . 'a array -> int64# -> 'a =
  "%array_safe_get_indexed_by_int64#"

let f_scannable (x : #(int * float * string) array) = get x #42L
let f_ignorable (x : #(float# * int * int64# * bool) array) = get x #42L
[%%expect{|
external get : ('a : any_non_null). 'a array -> int64# -> 'a
  = "%array_safe_get_indexed_by_int64#" [@@layout_poly]
val f_scannable : #(int * float * string) array -> #(int * float * string) =
  <fun>
val f_ignorable :
  #(float# * int * int64# * bool) array -> #(float# * int * int64# * bool) =
  <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = get x #42L
[%%expect{|
Line 1, characters 43-53:
1 | let f_bad (x : #(string * float#) array) = get x #42L
                                               ^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external get_scannable :
  #(int * float * string) array -> int64# -> #(int * float * string) =
  "%array_safe_get_indexed_by_int64#"
let get_scannable_app a i = get_scannable a i

external get_ignorable :
  #(float# * int * int64# * bool) array -> int64#
  -> #(float# * int * int64# * bool) =
  "%array_safe_get_indexed_by_int64#"
let get_ignorable_app a i = get_ignorable a i
[%%expect{|
external get_scannable :
  #(int * float * string) array -> int64# -> #(int * float * string)
  = "%array_safe_get_indexed_by_int64#"
val get_scannable_app :
  #(int * float * string) array -> int64# -> #(int * float * string) = <fun>
external get_ignorable :
  #(float# * int * int64# * bool) array ->
  int64# -> #(float# * int * int64# * bool)
  = "%array_safe_get_indexed_by_int64#"
val get_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int64# -> #(float# * int * int64# * bool) = <fun>
|}]

external get_bad : #(string * float#) array -> int64# -> #(string * float#) =
  "%array_safe_get_indexed_by_int64#"
let get_bad_app a i = get_bad a i
[%%expect{|
external get_bad : #(string * float#) array -> int64# -> #(string * float#)
  = "%array_safe_get_indexed_by_int64#"
Line 3, characters 22-33:
3 | let get_bad_app a i = get_bad a i
                          ^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) = get x #42L
[%%expect{|
Line 1, characters 42-52:
1 | let f_bad (x : #(int * int32x4#) array) = get x #42L
                                              ^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external get_ignorable_with_vec :
  #(int * int32x4#) array -> int64# -> #(int * int32x4#) =
  "%array_safe_get_indexed_by_int64#"
let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
[%%expect{|
external get_ignorable_with_vec :
  #(int * int32x4#) array -> int64# -> #(int * int32x4#)
  = "%array_safe_get_indexed_by_int64#"
Line 4, characters 37-63:
4 | let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(***************************************)
(* Test 11: safe set indexed by int64# *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] set :
  ('a : any_non_null) . 'a array -> int64# -> 'a -> unit =
  "%array_safe_set_indexed_by_int64#"

let f_scannable (x : #(int * float * string) array) = set x #42L #(1, 2.0, "3")

let f_ignorable (x : #(float# * int * int64# * bool) array) =
  set x #42L #(#1.0, 2, #3L, true)
[%%expect{|
external set : ('a : any_non_null). 'a array -> int64# -> 'a -> unit
  = "%array_safe_set_indexed_by_int64#" [@@layout_poly]
val f_scannable : #(int * float * string) array -> unit = <fun>
val f_ignorable : #(float# * int * int64# * bool) array -> unit = <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = set x #42L #("1", #2.0)
[%%expect{|
Line 1, characters 43-66:
1 | let f_bad (x : #(string * float#) array) = set x #42L #("1", #2.0)
                                               ^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external set_scannable :
  #(int * float * string) array -> int64# -> #(int * float * string) -> unit =
  "%array_safe_set_indexed_by_int64#"
let set_scannable_app a i x = set_scannable a i x

external set_ignorable :
  #(float# * int * int64# * bool) array -> int64#
  -> #(float# * int * int64# * bool) -> unit =
  "%array_safe_set_indexed_by_int64#"
let set_ignorable_app a i x = set_ignorable a i x
[%%expect{|
external set_scannable :
  #(int * float * string) array -> int64# -> #(int * float * string) -> unit
  = "%array_safe_set_indexed_by_int64#"
val set_scannable_app :
  #(int * float * string) array -> int64# -> #(int * float * string) -> unit =
  <fun>
external set_ignorable :
  #(float# * int * int64# * bool) array ->
  int64# -> #(float# * int * int64# * bool) -> unit
  = "%array_safe_set_indexed_by_int64#"
val set_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int64# -> #(float# * int * int64# * bool) -> unit = <fun>
|}]

external set_bad :
  #(string * float#) array -> int64# -> #(string * float#) -> unit =
  "%array_safe_set_indexed_by_int64#"
let set_bad_app a i x = set_bad a i x
[%%expect{|
external set_bad :
  #(string * float#) array -> int64# -> #(string * float#) -> unit
  = "%array_safe_set_indexed_by_int64#"
Line 4, characters 24-37:
4 | let set_bad_app a i x = set_bad a i x
                            ^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) v = set x #42L #(1, v)
[%%expect{|
Line 1, characters 44-62:
1 | let f_bad (x : #(int * int32x4#) array) v = set x #42L #(1, v)
                                                ^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external set_ignorable_with_vec :
  #(int * int32x4#) array -> int64# -> #(int * int32x4#) -> unit =
  "%array_safe_set_indexed_by_int64#"
let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
[%%expect{|
external set_ignorable_with_vec :
  #(int * int32x4#) array -> int64# -> #(int * int32x4#) -> unit
  = "%array_safe_set_indexed_by_int64#"
Line 4, characters 39-73:
4 | let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(*****************************************)
(* Test 12: unsafe get indexed by int64# *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] get : ('a : any_non_null) . 'a array -> int64# -> 'a =
  "%array_unsafe_get_indexed_by_int64#"

let f_scannable (x : #(int * float * string) array) = get x #42L
let f_ignorable (x : #(float# * int * int64# * bool) array) = get x #42L
[%%expect{|
external get : ('a : any_non_null). 'a array -> int64# -> 'a
  = "%array_unsafe_get_indexed_by_int64#" [@@layout_poly]
val f_scannable : #(int * float * string) array -> #(int * float * string) =
  <fun>
val f_ignorable :
  #(float# * int * int64# * bool) array -> #(float# * int * int64# * bool) =
  <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = get x #42L
[%%expect{|
Line 1, characters 43-53:
1 | let f_bad (x : #(string * float#) array) = get x #42L
                                               ^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external get_scannable :
  #(int * float * string) array -> int64# -> #(int * float * string) =
  "%array_unsafe_get_indexed_by_int64#"
let get_scannable_app a i = get_scannable a i

external get_ignorable :
  #(float# * int * int64# * bool) array -> int64#
  -> #(float# * int * int64# * bool) =
  "%array_unsafe_get_indexed_by_int64#"
let get_ignorable_app a i = get_ignorable a i
[%%expect{|
external get_scannable :
  #(int * float * string) array -> int64# -> #(int * float * string)
  = "%array_unsafe_get_indexed_by_int64#"
val get_scannable_app :
  #(int * float * string) array -> int64# -> #(int * float * string) = <fun>
external get_ignorable :
  #(float# * int * int64# * bool) array ->
  int64# -> #(float# * int * int64# * bool)
  = "%array_unsafe_get_indexed_by_int64#"
val get_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int64# -> #(float# * int * int64# * bool) = <fun>
|}]

external get_bad : #(string * float#) array -> int64# -> #(string * float#) =
  "%array_unsafe_get_indexed_by_int64#"
let get_bad_app a i = get_bad a i
[%%expect{|
external get_bad : #(string * float#) array -> int64# -> #(string * float#)
  = "%array_unsafe_get_indexed_by_int64#"
Line 3, characters 22-33:
3 | let get_bad_app a i = get_bad a i
                          ^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) = get x #42L
[%%expect{|
Line 1, characters 42-52:
1 | let f_bad (x : #(int * int32x4#) array) = get x #42L
                                              ^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external get_ignorable_with_vec :
  #(int * int32x4#) array -> int64# -> #(int * int32x4#) =
  "%array_unsafe_get_indexed_by_int64#"
let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
[%%expect{|
external get_ignorable_with_vec :
  #(int * int32x4#) array -> int64# -> #(int * int32x4#)
  = "%array_unsafe_get_indexed_by_int64#"
Line 4, characters 37-63:
4 | let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(*****************************************)
(* Test 13: unsafe set indexed by int64# *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] set : ('a : any_non_null) . 'a array -> int64# -> 'a -> unit =
  "%array_unsafe_set_indexed_by_int64#"

let f_scannable (x : #(int * float * string) array) = set x #42L #(1, 2.0, "3")

let f_ignorable (x : #(float# * int * int64# * bool) array) =
  set x #42L #(#1.0, 2, #3L, true)
[%%expect{|
external set : ('a : any_non_null). 'a array -> int64# -> 'a -> unit
  = "%array_unsafe_set_indexed_by_int64#" [@@layout_poly]
val f_scannable : #(int * float * string) array -> unit = <fun>
val f_ignorable : #(float# * int * int64# * bool) array -> unit = <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = set x #42L #("1", #2.0)
[%%expect{|
Line 1, characters 43-66:
1 | let f_bad (x : #(string * float#) array) = set x #42L #("1", #2.0)
                                               ^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external set_scannable :
  #(int * float * string) array -> int64# -> #(int * float * string) -> unit =
  "%array_unsafe_set_indexed_by_int64#"
let set_scannable_app a i x = set_scannable a i x

external set_ignorable :
  #(float# * int * int64# * bool) array -> int64#
  -> #(float# * int * int64# * bool) -> unit =
  "%array_unsafe_set_indexed_by_int64#"
let set_ignorable_app a i x = set_ignorable a i x
[%%expect{|
external set_scannable :
  #(int * float * string) array -> int64# -> #(int * float * string) -> unit
  = "%array_unsafe_set_indexed_by_int64#"
val set_scannable_app :
  #(int * float * string) array -> int64# -> #(int * float * string) -> unit =
  <fun>
external set_ignorable :
  #(float# * int * int64# * bool) array ->
  int64# -> #(float# * int * int64# * bool) -> unit
  = "%array_unsafe_set_indexed_by_int64#"
val set_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int64# -> #(float# * int * int64# * bool) -> unit = <fun>
|}]

external set_bad :
  #(string * float#) array -> int64# -> #(string * float#) -> unit =
  "%array_unsafe_set_indexed_by_int64#"
let set_bad_app a i x = set_bad a i x
[%%expect{|
external set_bad :
  #(string * float#) array -> int64# -> #(string * float#) -> unit
  = "%array_unsafe_set_indexed_by_int64#"
Line 4, characters 24-37:
4 | let set_bad_app a i x = set_bad a i x
                            ^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) v = set x #42L #(1, v)
[%%expect{|
Line 1, characters 44-62:
1 | let f_bad (x : #(int * int32x4#) array) v = set x #42L #(1, v)
                                                ^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external set_ignorable_with_vec :
  #(int * int32x4#) array -> int64# -> #(int * int32x4#) -> unit =
  "%array_unsafe_set_indexed_by_int64#"
let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
[%%expect{|
external set_ignorable_with_vec :
  #(int * int32x4#) array -> int64# -> #(int * int32x4#) -> unit
  = "%array_unsafe_set_indexed_by_int64#"
Line 4, characters 39-73:
4 | let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(***************************************)
(* Test 14: safe get indexed by int32# *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] get : ('a : any_non_null) . 'a array -> int32# -> 'a =
  "%array_safe_get_indexed_by_int32#"

let f_scannable (x : #(int * float * string) array) = get x #42l
let f_ignorable (x : #(float# * int * int64# * bool) array) = get x #42l
[%%expect{|
external get : ('a : any_non_null). 'a array -> int32# -> 'a
  = "%array_safe_get_indexed_by_int32#" [@@layout_poly]
val f_scannable : #(int * float * string) array -> #(int * float * string) =
  <fun>
val f_ignorable :
  #(float# * int * int64# * bool) array -> #(float# * int * int64# * bool) =
  <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = get x #42l
[%%expect{|
Line 1, characters 43-53:
1 | let f_bad (x : #(string * float#) array) = get x #42l
                                               ^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external get_scannable :
  #(int * float * string) array -> int32# -> #(int * float * string) =
  "%array_safe_get_indexed_by_int32#"
let get_scannable_app a i = get_scannable a i

external get_ignorable :
  #(float# * int * int64# * bool) array -> int32#
  -> #(float# * int * int64# * bool) =
  "%array_safe_get_indexed_by_int32#"
let get_ignorable_app a i = get_ignorable a i
[%%expect{|
external get_scannable :
  #(int * float * string) array -> int32# -> #(int * float * string)
  = "%array_safe_get_indexed_by_int32#"
val get_scannable_app :
  #(int * float * string) array -> int32# -> #(int * float * string) = <fun>
external get_ignorable :
  #(float# * int * int64# * bool) array ->
  int32# -> #(float# * int * int64# * bool)
  = "%array_safe_get_indexed_by_int32#"
val get_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int32# -> #(float# * int * int64# * bool) = <fun>
|}]

external get_bad : #(string * float#) array -> int32# -> #(string * float#) =
  "%array_safe_get_indexed_by_int32#"
let get_bad_app a i = get_bad a i
[%%expect{|
external get_bad : #(string * float#) array -> int32# -> #(string * float#)
  = "%array_safe_get_indexed_by_int32#"
Line 3, characters 22-33:
3 | let get_bad_app a i = get_bad a i
                          ^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) = get x #42l
[%%expect{|
Line 1, characters 42-52:
1 | let f_bad (x : #(int * int32x4#) array) = get x #42l
                                              ^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external get_ignorable_with_vec :
  #(int * int32x4#) array -> int32# -> #(int * int32x4#) =
  "%array_safe_get_indexed_by_int32#"
let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
[%%expect{|
external get_ignorable_with_vec :
  #(int * int32x4#) array -> int32# -> #(int * int32x4#)
  = "%array_safe_get_indexed_by_int32#"
Line 4, characters 37-63:
4 | let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(***************************************)
(* Test 15: safe set indexed by int32# *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] set :
  ('a : any_non_null) . 'a array -> int32# -> 'a -> unit =
  "%array_safe_set_indexed_by_int32#"

let f_scannable (x : #(int * float * string) array) = set x #42l #(1, 2.0, "3")

let f_ignorable (x : #(float# * int * int64# * bool) array) =
  set x #42l #(#1.0, 2, #3L, true)
[%%expect{|
external set : ('a : any_non_null). 'a array -> int32# -> 'a -> unit
  = "%array_safe_set_indexed_by_int32#" [@@layout_poly]
val f_scannable : #(int * float * string) array -> unit = <fun>
val f_ignorable : #(float# * int * int64# * bool) array -> unit = <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = set x #42l #("1", #2.0)
[%%expect{|
Line 1, characters 43-66:
1 | let f_bad (x : #(string * float#) array) = set x #42l #("1", #2.0)
                                               ^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external set_scannable :
  #(int * float * string) array -> int32# -> #(int * float * string) -> unit =
  "%array_safe_set_indexed_by_int32#"
let set_scannable_app a i x = set_scannable a i x

external set_ignorable :
  #(float# * int * int64# * bool) array -> int32#
  -> #(float# * int * int64# * bool) -> unit =
  "%array_safe_set_indexed_by_int32#"
let set_ignorable_app a i x = set_ignorable a i x
[%%expect{|
external set_scannable :
  #(int * float * string) array -> int32# -> #(int * float * string) -> unit
  = "%array_safe_set_indexed_by_int32#"
val set_scannable_app :
  #(int * float * string) array -> int32# -> #(int * float * string) -> unit =
  <fun>
external set_ignorable :
  #(float# * int * int64# * bool) array ->
  int32# -> #(float# * int * int64# * bool) -> unit
  = "%array_safe_set_indexed_by_int32#"
val set_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int32# -> #(float# * int * int64# * bool) -> unit = <fun>
|}]

external set_bad :
  #(string * float#) array -> int32# -> #(string * float#) -> unit =
  "%array_safe_set_indexed_by_int32#"
let set_bad_app a i x = set_bad a i x
[%%expect{|
external set_bad :
  #(string * float#) array -> int32# -> #(string * float#) -> unit
  = "%array_safe_set_indexed_by_int32#"
Line 4, characters 24-37:
4 | let set_bad_app a i x = set_bad a i x
                            ^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) v = set x #42l #(1, v)
[%%expect{|
Line 1, characters 44-62:
1 | let f_bad (x : #(int * int32x4#) array) v = set x #42l #(1, v)
                                                ^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external set_ignorable_with_vec :
  #(int * int32x4#) array -> int32# -> #(int * int32x4#) -> unit =
  "%array_safe_set_indexed_by_int32#"
let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
[%%expect{|
external set_ignorable_with_vec :
  #(int * int32x4#) array -> int32# -> #(int * int32x4#) -> unit
  = "%array_safe_set_indexed_by_int32#"
Line 4, characters 39-73:
4 | let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(*****************************************)
(* Test 16: unsafe get indexed by int32# *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] get : ('a : any_non_null) . 'a array -> int32# -> 'a =
  "%array_unsafe_get_indexed_by_int32#"

let f_scannable (x : #(int * float * string) array) = get x #42l
let f_ignorable (x : #(float# * int * int64# * bool) array) = get x #42l
[%%expect{|
external get : ('a : any_non_null). 'a array -> int32# -> 'a
  = "%array_unsafe_get_indexed_by_int32#" [@@layout_poly]
val f_scannable : #(int * float * string) array -> #(int * float * string) =
  <fun>
val f_ignorable :
  #(float# * int * int64# * bool) array -> #(float# * int * int64# * bool) =
  <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = get x #42l
[%%expect{|
Line 1, characters 43-53:
1 | let f_bad (x : #(string * float#) array) = get x #42l
                                               ^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external get_scannable :
  #(int * float * string) array -> int32# -> #(int * float * string) =
  "%array_unsafe_get_indexed_by_int32#"
let get_scannable_app a i = get_scannable a i

external get_ignorable :
  #(float# * int * int64# * bool) array -> int32#
  -> #(float# * int * int64# * bool) =
  "%array_unsafe_get_indexed_by_int32#"
let get_ignorable_app a i = get_ignorable a i
[%%expect{|
external get_scannable :
  #(int * float * string) array -> int32# -> #(int * float * string)
  = "%array_unsafe_get_indexed_by_int32#"
val get_scannable_app :
  #(int * float * string) array -> int32# -> #(int * float * string) = <fun>
external get_ignorable :
  #(float# * int * int64# * bool) array ->
  int32# -> #(float# * int * int64# * bool)
  = "%array_unsafe_get_indexed_by_int32#"
val get_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int32# -> #(float# * int * int64# * bool) = <fun>
|}]

external get_bad : #(string * float#) array -> int32# -> #(string * float#) =
  "%array_unsafe_get_indexed_by_int32#"
let get_bad_app a i = get_bad a i
[%%expect{|
external get_bad : #(string * float#) array -> int32# -> #(string * float#)
  = "%array_unsafe_get_indexed_by_int32#"
Line 3, characters 22-33:
3 | let get_bad_app a i = get_bad a i
                          ^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) = get x #42l
[%%expect{|
Line 1, characters 42-52:
1 | let f_bad (x : #(int * int32x4#) array) = get x #42l
                                              ^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external get_ignorable_with_vec :
  #(int * int32x4#) array -> int32# -> #(int * int32x4#) =
  "%array_unsafe_get_indexed_by_int32#"
let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
[%%expect{|
external get_ignorable_with_vec :
  #(int * int32x4#) array -> int32# -> #(int * int32x4#)
  = "%array_unsafe_get_indexed_by_int32#"
Line 4, characters 37-63:
4 | let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(*****************************************)
(* Test 17: unsafe set indexed by int32# *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] set :
  ('a : any_non_null) . 'a array -> int32# -> 'a -> unit =
  "%array_unsafe_set_indexed_by_int32#"

let f_scannable (x : #(int * float * string) array) = set x #42l #(1, 2.0, "3")

let f_ignorable (x : #(float# * int * int64# * bool) array) =
  set x #42l #(#1.0, 2, #3L, true)
[%%expect{|
external set : ('a : any_non_null). 'a array -> int32# -> 'a -> unit
  = "%array_unsafe_set_indexed_by_int32#" [@@layout_poly]
val f_scannable : #(int * float * string) array -> unit = <fun>
val f_ignorable : #(float# * int * int64# * bool) array -> unit = <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = set x #42l #("1", #2.0)
[%%expect{|
Line 1, characters 43-66:
1 | let f_bad (x : #(string * float#) array) = set x #42l #("1", #2.0)
                                               ^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external set_scannable :
  #(int * float * string) array -> int32# -> #(int * float * string) -> unit =
  "%array_unsafe_set_indexed_by_int32#"
let set_scannable_app a i x = set_scannable a i x

external set_ignorable :
  #(float# * int * int64# * bool) array -> int32#
  -> #(float# * int * int64# * bool) -> unit =
  "%array_unsafe_set_indexed_by_int32#"
let set_ignorable_app a i x = set_ignorable a i x
[%%expect{|
external set_scannable :
  #(int * float * string) array -> int32# -> #(int * float * string) -> unit
  = "%array_unsafe_set_indexed_by_int32#"
val set_scannable_app :
  #(int * float * string) array -> int32# -> #(int * float * string) -> unit =
  <fun>
external set_ignorable :
  #(float# * int * int64# * bool) array ->
  int32# -> #(float# * int * int64# * bool) -> unit
  = "%array_unsafe_set_indexed_by_int32#"
val set_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int32# -> #(float# * int * int64# * bool) -> unit = <fun>
|}]

external set_bad :
  #(string * float#) array -> int32# -> #(string * float#) -> unit =
  "%array_unsafe_set_indexed_by_int32#"
let set_bad_app a i x = set_bad a i x
[%%expect{|
external set_bad :
  #(string * float#) array -> int32# -> #(string * float#) -> unit
  = "%array_unsafe_set_indexed_by_int32#"
Line 4, characters 24-37:
4 | let set_bad_app a i x = set_bad a i x
                            ^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) v = set x #42l #(1, v)
[%%expect{|
Line 1, characters 44-62:
1 | let f_bad (x : #(int * int32x4#) array) v = set x #42l #(1, v)
                                                ^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external set_ignorable_with_vec :
  #(int * int32x4#) array -> int32# -> #(int * int32x4#) -> unit =
  "%array_unsafe_set_indexed_by_int32#"
let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
[%%expect{|
external set_ignorable_with_vec :
  #(int * int32x4#) array -> int32# -> #(int * int32x4#) -> unit
  = "%array_unsafe_set_indexed_by_int32#"
Line 4, characters 39-73:
4 | let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(*******************************************)
(* Test 18: safe get indexed by nativeint# *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] get :
  ('a : any_non_null) . 'a array -> nativeint# -> 'a =
  "%array_safe_get_indexed_by_nativeint#"

let f_scannable (x : #(int * float * string) array) = get x #42n
let f_ignorable (x : #(float# * int * int64# * bool) array) = get x #42n
[%%expect{|
external get : ('a : any_non_null). 'a array -> nativeint# -> 'a
  = "%array_safe_get_indexed_by_nativeint#" [@@layout_poly]
val f_scannable : #(int * float * string) array -> #(int * float * string) =
  <fun>
val f_ignorable :
  #(float# * int * int64# * bool) array -> #(float# * int * int64# * bool) =
  <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = get x #42n
[%%expect{|
Line 1, characters 43-53:
1 | let f_bad (x : #(string * float#) array) = get x #42n
                                               ^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external get_scannable :
  #(int * float * string) array -> nativeint# -> #(int * float * string) =
  "%array_safe_get_indexed_by_nativeint#"
let get_scannable_app a i = get_scannable a i

external get_ignorable :
  #(float# * int * int64# * bool) array -> nativeint#
  -> #(float# * int * int64# * bool) =
  "%array_safe_get_indexed_by_nativeint#"
let get_ignorable_app a i = get_ignorable a i
[%%expect{|
external get_scannable :
  #(int * float * string) array -> nativeint# -> #(int * float * string)
  = "%array_safe_get_indexed_by_nativeint#"
val get_scannable_app :
  #(int * float * string) array -> nativeint# -> #(int * float * string) =
  <fun>
external get_ignorable :
  #(float# * int * int64# * bool) array ->
  nativeint# -> #(float# * int * int64# * bool)
  = "%array_safe_get_indexed_by_nativeint#"
val get_ignorable_app :
  #(float# * int * int64# * bool) array ->
  nativeint# -> #(float# * int * int64# * bool) = <fun>
|}]

external get_bad :
  #(string * float#) array -> nativeint# -> #(string * float#) =
  "%array_safe_get_indexed_by_nativeint#"
let get_bad_app a i = get_bad a i
[%%expect{|
external get_bad :
  #(string * float#) array -> nativeint# -> #(string * float#)
  = "%array_safe_get_indexed_by_nativeint#"
Line 4, characters 22-33:
4 | let get_bad_app a i = get_bad a i
                          ^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) = get x #42n
[%%expect{|
Line 1, characters 42-52:
1 | let f_bad (x : #(int * int32x4#) array) = get x #42n
                                              ^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external get_ignorable_with_vec :
  #(int * int32x4#) array -> nativeint# -> #(int * int32x4#) =
  "%array_safe_get_indexed_by_nativeint#"
let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
[%%expect{|
external get_ignorable_with_vec :
  #(int * int32x4#) array -> nativeint# -> #(int * int32x4#)
  = "%array_safe_get_indexed_by_nativeint#"
Line 4, characters 37-63:
4 | let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(*******************************************)
(* Test 19: safe set indexed by nativeint# *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] set :
  ('a : any_non_null) . 'a array -> nativeint# -> 'a -> unit =
  "%array_safe_set_indexed_by_nativeint#"

let f_scannable (x : #(int * float * string) array) = set x #42n #(1, 2.0, "3")

let f_ignorable (x : #(float# * int * int64# * bool) array) =
  set x #42n #(#1.0, 2, #3L, true)
[%%expect{|
external set : ('a : any_non_null). 'a array -> nativeint# -> 'a -> unit
  = "%array_safe_set_indexed_by_nativeint#" [@@layout_poly]
val f_scannable : #(int * float * string) array -> unit = <fun>
val f_ignorable : #(float# * int * int64# * bool) array -> unit = <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = set x #42n #("1", #2.0)
[%%expect{|
Line 1, characters 43-66:
1 | let f_bad (x : #(string * float#) array) = set x #42n #("1", #2.0)
                                               ^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external set_scannable :
  #(int * float * string) array -> nativeint# -> #(int * float * string)
  -> unit =
  "%array_safe_set_indexed_by_nativeint#"
let set_scannable_app a i x = set_scannable a i x

external set_ignorable :
  #(float# * int * int64# * bool) array -> nativeint#
  -> #(float# * int * int64# * bool) -> unit =
  "%array_safe_set_indexed_by_nativeint#"
let set_ignorable_app a i x = set_ignorable a i x
[%%expect{|
external set_scannable :
  #(int * float * string) array ->
  nativeint# -> #(int * float * string) -> unit
  = "%array_safe_set_indexed_by_nativeint#"
val set_scannable_app :
  #(int * float * string) array ->
  nativeint# -> #(int * float * string) -> unit = <fun>
external set_ignorable :
  #(float# * int * int64# * bool) array ->
  nativeint# -> #(float# * int * int64# * bool) -> unit
  = "%array_safe_set_indexed_by_nativeint#"
val set_ignorable_app :
  #(float# * int * int64# * bool) array ->
  nativeint# -> #(float# * int * int64# * bool) -> unit = <fun>
|}]

external set_bad :
  #(string * float#) array -> nativeint# -> #(string * float#) -> unit =
  "%array_safe_set_indexed_by_nativeint#"
let set_bad_app a i x = set_bad a i x
[%%expect{|
external set_bad :
  #(string * float#) array -> nativeint# -> #(string * float#) -> unit
  = "%array_safe_set_indexed_by_nativeint#"
Line 4, characters 24-37:
4 | let set_bad_app a i x = set_bad a i x
                            ^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) v = set x #42n #(1, v)
[%%expect{|
Line 1, characters 44-62:
1 | let f_bad (x : #(int * int32x4#) array) v = set x #42n #(1, v)
                                                ^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external set_ignorable_with_vec :
  #(int * int32x4#) array -> nativeint# -> #(int * int32x4#) -> unit =
  "%array_safe_set_indexed_by_nativeint#"
let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
[%%expect{|
external set_ignorable_with_vec :
  #(int * int32x4#) array -> nativeint# -> #(int * int32x4#) -> unit
  = "%array_safe_set_indexed_by_nativeint#"
Line 4, characters 39-73:
4 | let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(*********************************************)
(* Test 20: unsafe get indexed by nativeint# *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] get :
  ('a : any_non_null) . 'a array -> nativeint# -> 'a =
  "%array_unsafe_get_indexed_by_nativeint#"

let f_scannable (x : #(int * float * string) array) = get x #42n
let f_ignorable (x : #(float# * int * int64# * bool) array) = get x #42n
[%%expect{|
external get : ('a : any_non_null). 'a array -> nativeint# -> 'a
  = "%array_unsafe_get_indexed_by_nativeint#" [@@layout_poly]
val f_scannable : #(int * float * string) array -> #(int * float * string) =
  <fun>
val f_ignorable :
  #(float# * int * int64# * bool) array -> #(float# * int * int64# * bool) =
  <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = get x #42n
[%%expect{|
Line 1, characters 43-53:
1 | let f_bad (x : #(string * float#) array) = get x #42n
                                               ^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external get_scannable :
  #(int * float * string) array -> nativeint# -> #(int * float * string) =
  "%array_unsafe_get_indexed_by_nativeint#"
let get_scannable_app a i = get_scannable a i

external get_ignorable :
  #(float# * int * int64# * bool) array -> nativeint#
  -> #(float# * int * int64# * bool) =
  "%array_unsafe_get_indexed_by_nativeint#"
let get_ignorable_app a i = get_ignorable a i
[%%expect{|
external get_scannable :
  #(int * float * string) array -> nativeint# -> #(int * float * string)
  = "%array_unsafe_get_indexed_by_nativeint#"
val get_scannable_app :
  #(int * float * string) array -> nativeint# -> #(int * float * string) =
  <fun>
external get_ignorable :
  #(float# * int * int64# * bool) array ->
  nativeint# -> #(float# * int * int64# * bool)
  = "%array_unsafe_get_indexed_by_nativeint#"
val get_ignorable_app :
  #(float# * int * int64# * bool) array ->
  nativeint# -> #(float# * int * int64# * bool) = <fun>
|}]

external get_bad :
  #(string * float#) array -> nativeint# -> #(string * float#) =
  "%array_unsafe_get_indexed_by_nativeint#"
let get_bad_app a i = get_bad a i
[%%expect{|
external get_bad :
  #(string * float#) array -> nativeint# -> #(string * float#)
  = "%array_unsafe_get_indexed_by_nativeint#"
Line 4, characters 22-33:
4 | let get_bad_app a i = get_bad a i
                          ^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) = get x #42n
[%%expect{|
Line 1, characters 42-52:
1 | let f_bad (x : #(int * int32x4#) array) = get x #42n
                                              ^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external get_ignorable_with_vec :
  #(int * int32x4#) array -> nativeint# -> #(int * int32x4#) =
  "%array_unsafe_get_indexed_by_nativeint#"
let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
[%%expect{|
external get_ignorable_with_vec :
  #(int * int32x4#) array -> nativeint# -> #(int * int32x4#)
  = "%array_unsafe_get_indexed_by_nativeint#"
Line 4, characters 37-63:
4 | let get_ignorable_with_vec_app x i = get_ignorable_with_vec x i
                                         ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(*********************************************)
(* Test 21: unsafe set indexed by nativeint# *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] set :
  ('a : any_non_null) . 'a array -> nativeint# -> 'a -> unit =
  "%array_unsafe_set_indexed_by_nativeint#"

let f_scannable (x : #(int * float * string) array) = set x #42n #(1, 2.0, "3")

let f_ignorable (x : #(float# * int * int64# * bool) array) =
  set x #42n #(#1.0, 2, #3L, true)
[%%expect{|
external set : ('a : any_non_null). 'a array -> nativeint# -> 'a -> unit
  = "%array_unsafe_set_indexed_by_nativeint#" [@@layout_poly]
val f_scannable : #(int * float * string) array -> unit = <fun>
val f_ignorable : #(float# * int * int64# * bool) array -> unit = <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = set x #42n #("1", #2.0)
[%%expect{|
Line 1, characters 43-66:
1 | let f_bad (x : #(string * float#) array) = set x #42n #("1", #2.0)
                                               ^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external set_scannable :
  #(int * float * string) array -> nativeint# -> #(int * float * string)
  -> unit =
  "%array_unsafe_set_indexed_by_nativeint#"
let set_scannable_app a i x = set_scannable a i x

external set_ignorable :
  #(float# * int * int64# * bool) array -> nativeint#
  -> #(float# * int * int64# * bool) -> unit =
  "%array_unsafe_set_indexed_by_nativeint#"
let set_ignorable_app a i x = set_ignorable a i x
[%%expect{|
external set_scannable :
  #(int * float * string) array ->
  nativeint# -> #(int * float * string) -> unit
  = "%array_unsafe_set_indexed_by_nativeint#"
val set_scannable_app :
  #(int * float * string) array ->
  nativeint# -> #(int * float * string) -> unit = <fun>
external set_ignorable :
  #(float# * int * int64# * bool) array ->
  nativeint# -> #(float# * int * int64# * bool) -> unit
  = "%array_unsafe_set_indexed_by_nativeint#"
val set_ignorable_app :
  #(float# * int * int64# * bool) array ->
  nativeint# -> #(float# * int * int64# * bool) -> unit = <fun>
|}]

external set_bad :
  #(string * float#) array -> nativeint# -> #(string * float#) -> unit =
  "%array_unsafe_set_indexed_by_nativeint#"
let set_bad_app a i x = set_bad a i x
[%%expect{|
external set_bad :
  #(string * float#) array -> nativeint# -> #(string * float#) -> unit
  = "%array_unsafe_set_indexed_by_nativeint#"
Line 4, characters 24-37:
4 | let set_bad_app a i x = set_bad a i x
                            ^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) v = set x #42n #(1, v)
[%%expect{|
Line 1, characters 44-62:
1 | let f_bad (x : #(int * int32x4#) array) v = set x #42n #(1, v)
                                                ^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external set_ignorable_with_vec :
  #(int * int32x4#) array -> nativeint# -> #(int * int32x4#) -> unit =
  "%array_unsafe_set_indexed_by_nativeint#"
let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
[%%expect{|
external set_ignorable_with_vec :
  #(int * int32x4#) array -> nativeint# -> #(int * int32x4#) -> unit
  = "%array_unsafe_set_indexed_by_nativeint#"
Line 4, characters 39-73:
4 | let set_ignorable_with_vec_app x i v = set_ignorable_with_vec x i #(1, v)
                                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(**********************)
(* Test 22: arrayblit *)

(* An array poly version works at valid product layouts. *)
external[@layout_poly] blit :
  ('a : any_non_null) . 'a array -> int -> 'a array -> int -> int -> unit =
  "%arrayblit"

let f_scannable (x : #(int * float * string) array) = blit x 0 x 2 3

let f_ignorable (x : #(float# * int * int64# * bool) array) = blit x 0 x 2 3
[%%expect{|
external blit :
  ('a : any_non_null). 'a array -> int -> 'a array -> int -> int -> unit
  = "%arrayblit" [@@layout_poly]
val f_scannable : #(int * float * string) array -> unit = <fun>
val f_ignorable : #(float# * int * int64# * bool) array -> unit = <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = blit x 0 x 2 3
[%%expect{|
Line 1, characters 43-57:
1 | let f_bad (x : #(string * float#) array) = blit x 0 x 2 3
                                               ^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* And similarly if we specialize it at declaration time. *)
external blit_scannable :
  #(int * float * string) array -> int -> #(int * float * string) array
  -> int -> int -> unit =
  "%arrayblit"
let blit_scannable_app a1 i1 a2 i2 len = blit_scannable a1 i2 a2 i2 len

external blit_ignorable :
  #(float# * int * int64# * bool) array -> int
  -> #(float# * int * int64# * bool) array -> int -> int -> unit =
  "%arrayblit"
let blit_ignorable_app a1 i1 a2 i2 len = blit_ignorable a1 i1 a2 i2 len
[%%expect{|
external blit_scannable :
  #(int * float * string) array ->
  int -> #(int * float * string) array -> int -> int -> unit = "%arrayblit"
val blit_scannable_app :
  ('a : value_or_null).
    #(int * float * string) array ->
    'a -> #(int * float * string) array -> int -> int -> unit =
  <fun>
external blit_ignorable :
  #(float# * int * int64# * bool) array ->
  int -> #(float# * int * int64# * bool) array -> int -> int -> unit
  = "%arrayblit"
val blit_ignorable_app :
  #(float# * int * int64# * bool) array ->
  int -> #(float# * int * int64# * bool) array -> int -> int -> unit = <fun>
|}]

external blit_bad :
  #(string * float#) array -> int -> #(string * float#) array
  -> int -> int -> unit =
  "%arrayblit"
let blit_bad_app a1 i1 a2 i2 len = blit_bad a1 i1 a2 i2 len
[%%expect{|
external blit_bad :
  #(string * float#) array ->
  int -> #(string * float#) array -> int -> int -> unit = "%arrayblit"
Line 5, characters 35-59:
5 | let blit_bad_app a1 i1 a2 i2 len = blit_bad a1 i1 a2 i2 len
                                       ^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* Unboxed vectors are also rejected. *)
let f_bad (x : #(int * int32x4#) array) = blit x 0 x 2 3
[%%expect{|
Line 1, characters 42-56:
1 | let f_bad (x : #(int * int32x4#) array) = blit x 0 x 2 3
                                              ^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

external blit_ignorable_with_vec :
  #(int * int32x4#) array -> int -> #(int * int32x4#) array
  -> int -> int -> unit =
  "%arrayblit"
let blit_ignorable_with_vec_app x = blit_ignorable_with_vec x 0 x 2 3
[%%expect{|
external blit_ignorable_with_vec :
  #(int * int32x4#) array ->
  int -> #(int * int32x4#) array -> int -> int -> unit = "%arrayblit"
Line 5, characters 36-69:
5 | let blit_ignorable_with_vec_app x = blit_ignorable_with_vec x 0 x 2 3
                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed vector types are not yet supported in arrays of unboxed
       products.
|}]

(************************************************)
(* Test 23: Abstract [value mod external] types *)

(* These should work like [int] - be allowed in both product arrays. *)

external[@layout_poly] get : ('a : any_non_null) . 'a array -> int -> 'a =
  "%array_safe_get"

let f1 (type a : value mod external_) (x : #(float# * a * int * int64#) array) =
  get x 42
[%%expect{|
external get : ('a : any_non_null). 'a array -> int -> 'a = "%array_safe_get"
  [@@layout_poly]
val f1 :
  ('a : value mod external_).
    #(float# * 'a * int * int64#) array -> #(float# * 'a * int * int64#) =
  <fun>
|}]

let f2 (type a : value mod external_) (x : #(string * a * bool option) array) =
  get x 42
[%%expect{|
val f2 :
  ('a : value mod external_).
    #(string * 'a * bool option) array -> #(string * 'a * bool option) =
  <fun>
|}]

(***********************************)
(* Test 24: any is always rejected *)

(* Even just for length it must be rejected - we wouldn't know what to divide
   by. *)

(* CR layouts v7.1: change these tests to be about just "any" once we move to
   product arrays to beta. *)
external[@layout_poly] len : ('a : any_non_null) . 'a array -> int =
  "%array_length"

let f_any_1 (type a : any_non_null) (x : #(float# * a * int * int64#) array) =
  len x
[%%expect{|
external len : ('a : any_non_null). 'a array -> int = "%array_length"
  [@@layout_poly]
Line 5, characters 6-7:
5 |   len x
          ^
Error: This expression has type "#(float# * a * int * int64#) array"
       but an expression was expected of type "'a array"
       The layout of #(float# * a * int * int64#) is float64 & any & value & bits64
         because it is an unboxed tuple.
       But the layout of #(float# * a * int * int64#) must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

let f_any_2 (type a : any_non_null) (x : #(string * a * bool option) array) =
  len x
[%%expect{|
Line 2, characters 6-7:
2 |   len x
          ^
Error: This expression has type "#(string * a * bool option) array"
       but an expression was expected of type "'a array"
       The layout of #(string * a * bool option) is value & any & value
         because it is an unboxed tuple.
       But the layout of #(string * a * bool option) must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

let f_any_external_1 (type a : any_non_null mod external_)
      (x : #(float# * a * int * int64#) array) = len x
[%%expect{|
Line 2, characters 53-54:
2 |       (x : #(float# * a * int * int64#) array) = len x
                                                         ^
Error: This expression has type "#(float# * a * int * int64#) array"
       but an expression was expected of type "'a array"
       The layout of #(float# * a * int * int64#) is float64 & any & value & bits64
         because it is an unboxed tuple.
       But the layout of #(float# * a * int * int64#) must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

let f_any_external_2 (type a : any_non_null mod external_)
      (x : #(string * a * bool option) array) = len x
[%%expect{|
Line 2, characters 52-53:
2 |       (x : #(string * a * bool option) array) = len x
                                                        ^
Error: This expression has type "#(string * a * bool option) array"
       but an expression was expected of type "'a array"
       The layout of #(string * a * bool option) is value & any & value
         because it is an unboxed tuple.
       But the layout of #(string * a * bool option) must be representable
         because it's the layout polymorphic type in an external declaration
         ([@layout_poly] forces all variables of layout 'any' to be
         representable at call sites).
|}]

(****************************************************)
(* Test 25: literal expressions have the same rules *)

let f_scannable_literal (type a : value mod external_)
      (x : int) (y : a) (z : bool option) = [| #(x, y, z) |]
let f_scannable_empty_literal (type a : value mod external_)
  : #(int * a * bool option) array = [| |]
[%%expect{|
val f_scannable_literal :
  ('a : value mod external_).
    int -> 'a -> bool option -> #(int * 'a * bool option) array =
  <fun>
val f_scannable_empty_literal :
  ('a : value mod external_). #(int * 'a * bool option) array = [||]
|}]

let f_ignorable_literal (type a : value mod external_)
      (x : int) (y : a) (z : #(int64# * float#)) = [| #(x, y, z) |]
let f_ignorable_empty_literal (type a : value mod external_)
  : #(int * a * #(int64# * float#)) array = [| |]
[%%expect{|
val f_ignorable_literal :
  ('a : value mod external_).
    int -> 'a -> #(int64# * float#) -> #(int * 'a * #(int64# * float#)) array =
  <fun>
val f_ignorable_empty_literal :
  ('a : value mod external_). #(int * 'a * #(int64# * float#)) array =
  [||]
|}]

let f_illegal_literal (type a : value mod external_)
      (x : float#) (y : a) (z : bool option) = [| #(x, y, z) |]
[%%expect{|
Line 2, characters 47-63:
2 |       (x : float#) (y : a) (z : bool option) = [| #(x, y, z) |]
                                                   ^^^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

let f_illegal_empty_literal (type a : value mod external_)
  : #(float# * a * bool option) array = [| |]
[%%expect{|
Lines 1-2, characters 28-45:
1 | ............................(type a : value mod external_)
2 |   : #(float# * a * bool option) array = [| |]
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(*************************************************)
(* Test 26: literal patterns have the same rules *)

let f_scannable_literal arr : #(bool option * string * int) =
  match arr with
  | [| |] -> #(None, "hi", 42)
  | [| #(x, y, z) |] -> #(z, y, x)
  | _ -> assert false
[%%expect{|
val f_scannable_literal :
  #(int * string * bool option) array -> #(bool option * string * int) =
  <fun>
|}]

let f_ignorable_literal arr : #(#(int64# * float#) * int32# * int) =
  match arr with
  | [| |] -> #(#(#42L, #3.14), #10l, 43)
  | [| #(x, y, #(z, q)) |] -> #(#(q, z), y, x)
  | _ -> assert false
[%%expect{|
val f_ignorable_literal :
  #(int * int32# * #(float# * int64#)) array ->
  #(#(int64# * float#) * int32# * int) = <fun>
|}]

let f_illegal_literal : #(float# * bool option * int) array -> int =
  function
  | [| #(a,b,c) |] -> 1
  | _ -> 0
[%%expect{|
Line 3, characters 4-18:
3 |   | [| #(a,b,c) |] -> 1
        ^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

let f_illegal_empty_literal : #(float# * bool option * int) array -> int =
  function
  | [| |] -> 0
  | _ -> 1
[%%expect{|
Line 3, characters 4-9:
3 |   | [| |] -> 0
        ^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]
