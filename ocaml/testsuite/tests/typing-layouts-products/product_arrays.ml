(* TEST
 flambda2;
 include stdlib_upstream_compatible;
 flags = "-extension layouts_beta";
 {
   expect;
 }
*)

(* Extremely basic typing tests.  This feature will need executable tests too.
   And even the bytecode support isn't right yet (must prevent marshalling) *)

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
type t6 : bits64 & immediate & float64 & immediate (* ??? *)
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

(************************)
(* Test 4: array length *)

(* XXX for all of these primitives, I should have tests showing the
   non-layout-poly versions also work on these layouts. *)

external[@layout_poly] len : ('a : any) . 'a array -> int = "%array_length"

let f_scannable (x : #(int * float * string) array) = len x

let f_ignorable (x : #(float# * int * int64# * bool) array) = len x

let f_bad (x : #(string * float#) array) = len x

[%%expect{|
external len : ('a : any). 'a array -> int = "%array_length" [@@layout_poly]
val f_scannable : #(int * float * string) array -> int = <fun>
val f_ignorable : #(float# * int * int64# * bool) array -> int = <fun>
Line 7, characters 43-48:
7 | let f_bad (x : #(string * float#) array) = len x
                                               ^^^^^
Error: Unboxed product array elements must be external or contain all gc scannable types.
       This product is not external but contains an element of sort float64.
|}]

(********************)
(* Test 5: safe get *)

external[@layout_poly] get : ('a : any) . 'a array -> int -> 'a =
  "%array_safe_get"

let f_scannable (x : #(int * float * string) array) = get x 42

let f_ignorable (x : #(float# * int * int64# * bool) array) = get x 42

let f_bad (x : #(string * float#) array) = get x 42

[%%expect{|
external get : ('a : any). 'a array -> int -> 'a = "%array_safe_get"
  [@@layout_poly]
val f_scannable : #(int * float * string) array -> #(int * float * string) =
  <fun>
val f_ignorable :
  #(float# * int * int64# * bool) array -> #(float# * int * int64# * bool) =
  <fun>
Line 8, characters 43-51:
8 | let f_bad (x : #(string * float#) array) = get x 42
                                               ^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc scannable types.
       This product is not external but contains an element of sort float64.
|}]

(********************)
(* Test 6: safe set *)

external[@layout_poly] set : ('a : any) . 'a array -> int -> 'a -> unit =
  "%array_safe_set"

let f_scannable (x : #(int * float * string) array) = set x 42 #(1, 2.0, "3")

let f_ignorable (x : #(float# * int * int64# * bool) array) =
  set x 42 #(#1.0, 2, #3L, true)

let f_bad (x : #(string * float#) array) = set x 42 #("1", #2.0)

[%%expect{|
external set : ('a : any). 'a array -> int -> 'a -> unit = "%array_safe_set"
  [@@layout_poly]
val f_scannable : #(int * float * string) array -> unit = <fun>
val f_ignorable : #(float# * int * int64# * bool) array -> unit = <fun>
Line 9, characters 43-64:
9 | let f_bad (x : #(string * float#) array) = set x 42 #("1", #2.0)
                                               ^^^^^^^^^^^^^^^^^^^^^
Error: Unboxed product array elements must be external or contain all gc scannable types.
       This product is not external but contains an element of sort float64.
|}]

