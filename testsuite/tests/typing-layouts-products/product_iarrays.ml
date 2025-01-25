(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 {
   expect;
 }
*)

(* This test checks that you get an error if you attempts to manipulate iarrays
   of unboxed products are rejected. *)
(* CR layouts v7.1: Support iarrays of unboxed products. *)


(* makearray_dynamic *)
external[@layout_poly] make_vect : ('a : any_non_null) . int -> 'a -> 'a iarray
  = "%makearray_dynamic"

let make_scannable (x : #(int * string)) = make_vect 42 x
[%%expect{|
external make_vect : ('a : any_non_null). int -> 'a -> 'a iarray
  = "%makearray_dynamic" [@@layout_poly]
Line 4, characters 43-57:
4 | let make_scannable (x : #(int * string)) = make_vect 42 x
                                               ^^^^^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let make_ignorable (x : #(int * float#)) = make_vect 42 x
[%%expect{|
Line 1, characters 43-57:
1 | let make_ignorable (x : #(int * float#)) = make_vect 42 x
                                               ^^^^^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let make_bad (x : #(string * float#)) = make_vect 42 x

[%%expect{|
Line 1, characters 40-54:
1 | let make_bad (x : #(string * float#)) = make_vect 42 x
                                            ^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* array length *)
external[@layout_poly] len : ('a : any_non_null) . 'a iarray -> int =
  "%array_length"

let length_scannable (x : #(int * string) iarray) = len x
[%%expect{|
external len : ('a : any_non_null). 'a iarray -> int = "%array_length"
  [@@layout_poly]
Line 4, characters 52-57:
4 | let length_scannable (x : #(int * string) iarray) = len x
                                                        ^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let length_ignorable (x : #(int * float#) iarray) = len x
[%%expect{|
Line 1, characters 52-57:
1 | let length_ignorable (x : #(int * float#) iarray) = len x
                                                        ^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let length_bad (x : #(string * float#) iarray) = len x
[%%expect{|
Line 1, characters 49-54:
1 | let length_bad (x : #(string * float#) iarray) = len x
                                                     ^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* safe get *)
external[@layout_poly] get : ('a : any_non_null) . 'a iarray -> int -> 'a =
  "%array_safe_get"

let get_scannable (x : #(int * string) iarray) = get x 42
[%%expect{|
external get : ('a : any_non_null). 'a iarray -> int -> 'a
  = "%array_safe_get" [@@layout_poly]
Line 4, characters 49-57:
4 | let get_scannable (x : #(int * string) iarray) = get x 42
                                                     ^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_ignorable (x : #(int * float#) iarray) = get x 42
[%%expect{|
Line 1, characters 49-57:
1 | let get_ignorable (x : #(int * float#) iarray) = get x 42
                                                     ^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_bad (x : #(string * float#) iarray) = get x 42
[%%expect{|
Line 1, characters 46-54:
1 | let get_bad (x : #(string * float#) iarray) = get x 42
                                                  ^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* unsafe get *)
external[@layout_poly] get : ('a : any_non_null) . 'a iarray -> int -> 'a =
  "%array_unsafe_get"

let get_scannable (x : #(int * string) iarray) = get x 42
[%%expect{|
external get : ('a : any_non_null). 'a iarray -> int -> 'a
  = "%array_unsafe_get" [@@layout_poly]
Line 4, characters 49-57:
4 | let get_scannable (x : #(int * string) iarray) = get x 42
                                                     ^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_ignorable (x : #(int * float#) iarray) = get x 42
[%%expect{|
Line 1, characters 49-57:
1 | let get_ignorable (x : #(int * float#) iarray) = get x 42
                                                     ^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_bad (x : #(string * float#) iarray) = get x 42
[%%expect{|
Line 1, characters 46-54:
1 | let get_bad (x : #(string * float#) iarray) = get x 42
                                                  ^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* safe get indexed by int64# *)
external[@layout_poly] get : ('a : any_non_null) . 'a iarray -> int64# -> 'a =
  "%array_safe_get_indexed_by_int64#"

let get_scannable (x : #(int * string) iarray) = get x #42L
[%%expect{|
external get : ('a : any_non_null). 'a iarray -> int64# -> 'a
  = "%array_safe_get_indexed_by_int64#" [@@layout_poly]
Line 4, characters 49-59:
4 | let get_scannable (x : #(int * string) iarray) = get x #42L
                                                     ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_ignorable (x : #(int * float#) iarray) = get x #42L
[%%expect{|
Line 1, characters 49-59:
1 | let get_ignorable (x : #(int * float#) iarray) = get x #42L
                                                     ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_bad (x : #(string * float#) iarray) = get x #42L
[%%expect{|
Line 1, characters 46-56:
1 | let get_bad (x : #(string * float#) iarray) = get x #42L
                                                  ^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* unsafe get indexed by int64# *)
external[@layout_poly] get : ('a : any_non_null) . 'a iarray -> int64# -> 'a =
  "%array_unsafe_get_indexed_by_int64#"

let get_scannable (x : #(int * string) iarray) = get x #42L
[%%expect{|
external get : ('a : any_non_null). 'a iarray -> int64# -> 'a
  = "%array_unsafe_get_indexed_by_int64#" [@@layout_poly]
Line 4, characters 49-59:
4 | let get_scannable (x : #(int * string) iarray) = get x #42L
                                                     ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_ignorable (x : #(int * float#) iarray) = get x #42L
[%%expect{|
Line 1, characters 49-59:
1 | let get_ignorable (x : #(int * float#) iarray) = get x #42L
                                                     ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_bad (x : #(string * float#) iarray) = get x #42L
[%%expect{|
Line 1, characters 46-56:
1 | let get_bad (x : #(string * float#) iarray) = get x #42L
                                                  ^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* safe get indexed by int32# *)
external[@layout_poly] get : ('a : any_non_null) . 'a iarray -> int32# -> 'a =
  "%array_safe_get_indexed_by_int32#"

let get_scannable (x : #(int * string) iarray) = get x #42l
[%%expect{|
external get : ('a : any_non_null). 'a iarray -> int32# -> 'a
  = "%array_safe_get_indexed_by_int32#" [@@layout_poly]
Line 4, characters 49-59:
4 | let get_scannable (x : #(int * string) iarray) = get x #42l
                                                     ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_ignorable (x : #(int * float#) iarray) = get x #42l
[%%expect{|
Line 1, characters 49-59:
1 | let get_ignorable (x : #(int * float#) iarray) = get x #42l
                                                     ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_bad (x : #(string * float#) iarray) = get x #42l
[%%expect{|
Line 1, characters 46-56:
1 | let get_bad (x : #(string * float#) iarray) = get x #42l
                                                  ^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* unsafe get indexed by int32# *)
external[@layout_poly] get : ('a : any_non_null) . 'a iarray -> int32# -> 'a =
  "%array_unsafe_get_indexed_by_int32#"

let get_scannable (x : #(int * string) iarray) = get x #42l
[%%expect{|
external get : ('a : any_non_null). 'a iarray -> int32# -> 'a
  = "%array_unsafe_get_indexed_by_int32#" [@@layout_poly]
Line 4, characters 49-59:
4 | let get_scannable (x : #(int * string) iarray) = get x #42l
                                                     ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_ignorable (x : #(int * float#) iarray) = get x #42l
[%%expect{|
Line 1, characters 49-59:
1 | let get_ignorable (x : #(int * float#) iarray) = get x #42l
                                                     ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_bad (x : #(string * float#) iarray) = get x #42l
[%%expect{|
Line 1, characters 46-56:
1 | let get_bad (x : #(string * float#) iarray) = get x #42l
                                                  ^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* safe get indexed by nativeint# *)
external[@layout_poly] get :
  ('a : any_non_null) . 'a iarray -> nativeint# -> 'a =
  "%array_safe_get_indexed_by_nativeint#"

let get_scannable (x : #(int * string) iarray) = get x #42n
[%%expect{|
external get : ('a : any_non_null). 'a iarray -> nativeint# -> 'a
  = "%array_safe_get_indexed_by_nativeint#" [@@layout_poly]
Line 5, characters 49-59:
5 | let get_scannable (x : #(int * string) iarray) = get x #42n
                                                     ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_ignorable (x : #(int * float#) iarray) = get x #42n
[%%expect{|
Line 1, characters 49-59:
1 | let get_ignorable (x : #(int * float#) iarray) = get x #42n
                                                     ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_bad (x : #(string * float#) iarray) = get x #42n
[%%expect{|
Line 1, characters 46-56:
1 | let get_bad (x : #(string * float#) iarray) = get x #42n
                                                  ^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* unsafe get indexed by nativeint# *)
external[@layout_poly] get :
  ('a : any_non_null) . 'a iarray -> nativeint# -> 'a =
  "%array_unsafe_get_indexed_by_nativeint#"

let get_scannable (x : #(int * string) iarray) = get x #42n
[%%expect{|
external get : ('a : any_non_null). 'a iarray -> nativeint# -> 'a
  = "%array_unsafe_get_indexed_by_nativeint#" [@@layout_poly]
Line 5, characters 49-59:
5 | let get_scannable (x : #(int * string) iarray) = get x #42n
                                                     ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_ignorable (x : #(int * float#) iarray) = get x #42n
[%%expect{|
Line 1, characters 49-59:
1 | let get_ignorable (x : #(int * float#) iarray) = get x #42n
                                                     ^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let get_bad (x : #(string * float#) iarray) = get x #42n
[%%expect{|
Line 1, characters 46-56:
1 | let get_bad (x : #(string * float#) iarray) = get x #42n
                                                  ^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* expression literals *)
let f_scannable_literal (type a : value mod external_)
      (x : int) (y : a) (z : bool option) = [: #(x, y, z) :]
[%%expect{|
Line 2, characters 44-60:
2 |       (x : int) (y : a) (z : bool option) = [: #(x, y, z) :]
                                                ^^^^^^^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let f_scannable_empty_literal (type a : value mod external_)
  : #(int * a * bool option) iarray = [: :]
[%%expect{|
Lines 1-2, characters 30-43:
1 | ..............................(type a : value mod external_)
2 |   : #(int * a * bool option) iarray = [: :]
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let f_ignorable_literal (type a : value mod external_)
      (x : int) (y : a) (z : #(int64# * float#)) = [: #(x, y, z) :]
[%%expect{|
Line 2, characters 51-67:
2 |       (x : int) (y : a) (z : #(int64# * float#)) = [: #(x, y, z) :]
                                                       ^^^^^^^^^^^^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let f_ignorable_empty_literal (type a : value mod external_)
  : #(int * a * #(int64# * float#)) iarray = [: :]
[%%expect{|
Lines 1-2, characters 30-50:
1 | ..............................(type a : value mod external_)
2 |   : #(int * a * #(int64# * float#)) iarray = [: :]
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let f_illegal_literal (type a : value mod external_)
      (x : float#) (y : a) (z : bool option) = [: #(x, y, z) :]
[%%expect{|
Line 2, characters 47-63:
2 |       (x : float#) (y : a) (z : bool option) = [: #(x, y, z) :]
                                                   ^^^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

let f_illegal_empty_literal (type a : value mod external_)
  : #(float# * a * bool option) iarray = [: :]
[%%expect{|
Lines 1-2, characters 28-46:
1 | ............................(type a : value mod external_)
2 |   : #(float# * a * bool option) iarray = [: :]
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

(* pattern literals *)
let f_scannable_literal arr : #(bool option * string * int) =
  match arr with
  | [: :] -> #(None, "hi", 42)
  | [: #(x, y, z) :] -> #(z, y, x)
  | _ -> assert false
[%%expect{|
Line 3, characters 4-9:
3 |   | [: :] -> #(None, "hi", 42)
        ^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let f_ignorable_literal arr : #(#(int64# * float#) * int32# * int) =
  match arr with
  | [: :] -> #(#(#42L, #3.14), #10l, 43)
  | [: #(x, y, #(z, q)) :] -> #(#(q, z), y, x)
  | _ -> assert false
[%%expect{|
Line 3, characters 4-9:
3 |   | [: :] -> #(#(#42L, #3.14), #10l, 43)
        ^^^^^
Error: Immutable arrays of unboxed products are not yet supported.
|}]

let f_illegal_literal : #(float# * bool option * int) iarray -> int =
  function
  | [: #(a,b,c) :] -> 1
  | _ -> 0
[%%expect{|
Line 3, characters 4-18:
3 |   | [: #(a,b,c) :] -> 1
        ^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]

let f_illegal_empty_literal : #(float# * bool option * int) iarray -> int =
  function
  | [: :] -> 0
  | _ -> 1
[%%expect{|
Line 3, characters 4-9:
3 |   | [: :] -> 0
        ^^^^^
Error: Unboxed product array elements must be external or contain all gc
       scannable types. The product type this function is applied at is
       not external but contains an element of sort float64.
|}]
