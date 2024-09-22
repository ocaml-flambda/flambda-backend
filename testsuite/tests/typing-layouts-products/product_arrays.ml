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

let f_scannable (x : #(int * float * string) array) = make_vect 42 x

let f_ignorable (x : #(float# * int * int64# * bool) array) = make_vect 42 x
[%%expect{|
external make_vect : ('a : any_non_null). int -> 'a -> 'a array
  = "%makearray_dynamic" [@@layout_poly]
val f_scannable :
  #(int * float * string) array -> #(int * float * string) array array =
  <fun>
val f_ignorable :
  #(float# * int * int64# * bool) array ->
  #(float# * int * int64# * bool) array array = <fun>
|}]

(* But not on the bad ones. *)
let f_bad (x : #(string * float#) array) = make_vect 42 x
[%%expect{|
Line 1, characters 10-40:
1 | let f_bad (x : #(string * float#) array) = make_vect 42 x
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
let f_bad (x : #(int * int32x4#) array) = make_vect 42 x
[%%expect{|
Line 1, characters 10-39:
1 | let f_bad (x : #(int * int32x4#) array) = make_vect 42 x
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
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
  "%array_safe_set_indexed_by_int64#"
let set_bad_app a i x = set_bad a i x
[%%expect{|
Line 2, characters 2-66:
2 |   #(string * float#) array -> int32# -> #(string * float#) -> unit =
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%array_safe_set_indexed_by_int64#] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
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
  "%array_safe_set_indexed_by_int64#"
let set_bad_app a i x = set_bad a i x
[%%expect{|
Line 2, characters 2-70:
2 |   #(string * float#) array -> nativeint# -> #(string * float#) -> unit =
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: The primitive [%array_safe_set_indexed_by_int64#] is used in an invalid declaration.
       The declaration contains argument/return types with the wrong layout.
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
