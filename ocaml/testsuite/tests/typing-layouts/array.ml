(* TEST
   * flambda2
   ** expect
     flags = "-extension layouts_alpha"
   ** expect
     flags = "-extension layouts_beta"
*)

(*******************************************)
(* Test 1: Support unboxed types in arrays *)

type t_any : any

type t1 = float# array
type t2 = int32# array
type t3 = int64# array
type t4 = nativeint# array
type t5 = t_any array

type ('a : float64) t1' = 'a array
type ('a : bits32) t2' = 'a array
type ('a : bits64) t3' = 'a array
type ('a : word) t4' = 'a array
type ('a : any) t5' = 'a array

[%%expect{|
type t_any : any
type t1 = float# array
type t2 = int32# array
type t3 = int64# array
type t4 = nativeint# array
type t5 = t_any array
type ('a : float64) t1' = 'a array
type ('a : bits32) t2' = 'a array
type ('a : bits64) t3' = 'a array
type ('a : word) t4' = 'a array
type ('a : any) t5' = 'a array
|}];;

(*****************************)
(* Test 2: array expressions *)

let v1 = [| #1. |]
[%%expect{|
Line 1, characters 12-15:
1 | let v1 = [| #1. |]
                ^^^
Error: This expression has type float# but an expression was expected of type
         ('a : value)
       The layout of float# is float64, because
         it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value, because
         it's the type of an array element.
|}];;


let v2 = [| #1l |]
[%%expect{|
Line 1, characters 12-15:
1 | let v2 = [| #1l |]
                ^^^
Error: This expression has type int32# but an expression was expected of type
         ('a : value)
       The layout of int32# is bits32, because
         it is the primitive bits32 type int32#.
       But the layout of int32# must be a sublayout of value, because
         it's the type of an array element.
|}];;


let v3 = [| #1L |]
[%%expect{|
Line 1, characters 12-15:
1 | let v3 = [| #1L |]
                ^^^
Error: This expression has type int64# but an expression was expected of type
         ('a : value)
       The layout of int64# is bits64, because
         it is the primitive bits64 type int64#.
       But the layout of int64# must be a sublayout of value, because
         it's the type of an array element.
|}];;


let v4 = [| #1n |]
[%%expect{|
Line 1, characters 12-15:
1 | let v4 = [| #1n |]
                ^^^
Error: This expression has type nativeint#
       but an expression was expected of type ('a : value)
       The layout of nativeint# is word, because
         it is the primitive word type nativeint#.
       But the layout of nativeint# must be a sublayout of value, because
         it's the type of an array element.
|}];;

(****************************************)
(* Test 3: Array operations do not work *)

let f (x : float# array) = x.(0)
[%%expect{|
Line 1, characters 27-28:
1 | let f (x : float# array) = x.(0)
                               ^
Error: This expression has type float# array
       but an expression was expected of type 'a array
       The layout of float# is float64, because
         it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value, because
         of layout requirements from an imported definition.
|}];;

let f (x : float# array) = Array.length x
[%%expect{|
Line 1, characters 40-41:
1 | let f (x : float# array) = Array.length x
                                            ^
Error: This expression has type float# array
       but an expression was expected of type 'a array
       The layout of float# is float64, because
         it is the primitive float64 type float#.
       But the layout of float# must be a sublayout of value, because
         of layout requirements from an imported definition.
|}];;

(*****************************************************************)
(* Test 4: Calling wrong primitives on unboxed array kinds fails *)

external get : float# array -> int -> float = "%floatarray_safe_get"
let d (x : float# array) = get x 0

[%%expect{|
external get : float# array -> int -> float = "%floatarray_safe_get"
Line 2, characters 27-34:
2 | let d (x : float# array) = get x 0
                               ^^^^^^^
Error: Array kind unboxed_float can only be operated on using its own primitives
       and those primitives can only work on unboxed_float
|}];;

external get : floatarray -> int -> float# = "%float_u_array_safe_get"
let d (x : floatarray) = get x 0

[%%expect{|
external get : floatarray -> int -> float# = "%float_u_array_safe_get"
Line 2, characters 25-32:
2 | let d (x : floatarray) = get x 0
                             ^^^^^^^
Error: Array kind unboxed_float can only be operated on using its own primitives
       and those primitives can only work on unboxed_float
|}];;

(* Doesn't prevent the use of [Obj.magic] *)
external get : floatarray -> int -> float = "%floatarray_safe_get"
let d (x : float# array) = get (Obj.magic x : floatarray) 0

[%%expect{|
external get : floatarray -> int -> float = "%floatarray_safe_get"
val d : float# array -> float = <fun>
|}];;

external get : ('a : any). 'a array -> int -> float = "%floatarray_safe_get"
let d (x : 'a array) = get x 0

[%%expect{|
external get : ('a : any). 'a array -> int -> float = "%floatarray_safe_get"
Line 2, characters 23-30:
2 | let d (x : 'a array) = get x 0
                           ^^^^^^^
Error: A representable jkind is required here.
|}];;

external get : int32# array -> int -> float = "%floatarray_safe_get"
let d (x : int32# array) = get x 0

[%%expect{|
external get : int32# array -> int -> float = "%floatarray_safe_get"
Line 2, characters 27-34:
2 | let d (x : int32# array) = get x 0
                               ^^^^^^^
Error: Layout bits32 is not supported yet.
|}];;

external get : int64# array -> int -> float = "%floatarray_safe_get"
let d (x : int64# array) = get x 0

[%%expect{|
external get : int64# array -> int -> float = "%floatarray_safe_get"
Line 2, characters 27-34:
2 | let d (x : int64# array) = get x 0
                               ^^^^^^^
Error: Layout bits64 is not supported yet.
|}];;

external get : nativeint# array -> int -> float = "%floatarray_safe_get"
let d (x : nativeint# array) = get x 0

[%%expect{|
external get : nativeint# array -> int -> float = "%floatarray_safe_get"
Line 2, characters 31-38:
2 | let d (x : nativeint# array) = get x 0
                                   ^^^^^^^
Error: Layout word is not supported yet.
|}];;
