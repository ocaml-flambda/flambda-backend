(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 {
   flags = "-extension layouts_alpha -extension small_numbers";
   expect;
 }
*)
(* Tests around type-checking arrays of unboxed types. Tests around
   compilation correctness should go somewhere else. *)

(*******************************************)
(* Test 1: Support unboxed types in arrays *)

type t_any_non_null : any_non_null

type t1 = float# array
type t2 = int32# array
type t3 = int64# array
type t4 = nativeint# array
type t5 = t_any_non_null array
type t6 = float32# array

type ('a : float64) t1' = 'a array
type ('a : bits32) t2' = 'a array
type ('a : bits64) t3' = 'a array
type ('a : word) t4' = 'a array
type ('a : any_non_null) t5' = 'a array
type ('a : float32) t6' = 'a array

[%%expect{|
type t_any_non_null : any_non_null
type t1 = float# array
type t2 = int32# array
type t3 = int64# array
type t4 = nativeint# array
type t5 = t_any_non_null array
type t6 = float32# array
type ('a : float64) t1' = 'a array
type ('a : bits32) t2' = 'a array
type ('a : bits64) t3' = 'a array
type ('a : word) t4' = 'a array
type ('a : any_non_null) t5' = 'a array
type ('a : float32) t6' = 'a array
|}];;

(*****************************)
(* Test 2: array expressions *)

let v1 = [| #1. |]
[%%expect{|
val v1 : float# array = [|<abstr>|]
|}];;


let v2 = [| #1l |]
[%%expect{|
val v2 : int32# array = [|<abstr>|]
|}];;


let v3 = [| #1L |]
[%%expect{|
val v3 : int64# array = [|<abstr>|]
|}];;


let v4 = [| #1n |]
[%%expect{|
val v4 : nativeint# array = [|<abstr>|]
|}];;

let v5 = [| #1.s |]
[%%expect{|
val v5 : float32# array = [|<abstr>|]
|}];;

(****************************************)
(* Test 3: Array operations do not work *)

let f (x : float# array) = x.(0)
[%%expect{|
Line 1, characters 27-28:
1 | let f (x : float# array) = x.(0)
                               ^
Error: This expression has type "float# array"
       but an expression was expected of type "'a array"
       The layout of float# is float64
         because it is unboxed version of the primitive type float.
       But the layout of float# must be a sublayout of value.
|}];;

let f (x : float# array) = Array.length x
[%%expect{|
Line 1, characters 40-41:
1 | let f (x : float# array) = Array.length x
                                            ^
Error: This expression has type "float# array"
       but an expression was expected of type "'a array"
       The layout of float# is float64
         because it is unboxed version of the primitive type float.
       But the layout of float# must be a sublayout of value.
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
Error: Floatarray primitives can't be used on arrays containing
       unboxed types.
|}];;


(* [Obj.magic] can bypass the error but this should be discouraged *)
external get : floatarray -> int -> float = "%floatarray_safe_get"
let d (x : float# array) = get (Obj.magic x : floatarray) 0

[%%expect{|
external get : floatarray -> int -> float = "%floatarray_safe_get"
val d : float# array -> float = <fun>
|}];;

external get : ('a : any_non_null). 'a array -> int -> float = "%floatarray_safe_get"
let d (x : 'a array) = get x 0

[%%expect{|
external get : ('a : any_non_null). 'a array -> int -> float
  = "%floatarray_safe_get"
val d : 'a array -> float = <fun>
|}];;

external get : int32# array -> int -> float = "%floatarray_safe_get"
let d (x : int32# array) = get x 0

[%%expect{|
external get : int32# array -> int -> float = "%floatarray_safe_get"
Line 2, characters 27-34:
2 | let d (x : int32# array) = get x 0
                               ^^^^^^^
Error: Floatarray primitives can't be used on arrays containing
       unboxed types.
|}];;

external get : int64# array -> int -> float = "%floatarray_safe_get"
let d (x : int64# array) = get x 0

[%%expect{|
external get : int64# array -> int -> float = "%floatarray_safe_get"
Line 2, characters 27-34:
2 | let d (x : int64# array) = get x 0
                               ^^^^^^^
Error: Floatarray primitives can't be used on arrays containing
       unboxed types.
|}];;

external get : nativeint# array -> int -> float = "%floatarray_safe_get"
let d (x : nativeint# array) = get x 0

[%%expect{|
external get : nativeint# array -> int -> float = "%floatarray_safe_get"
Line 2, characters 31-38:
2 | let d (x : nativeint# array) = get x 0
                                   ^^^^^^^
Error: Floatarray primitives can't be used on arrays containing
       unboxed types.
|}];;

external get : float32# array -> int -> float = "%floatarray_safe_get"
let d (x : float32# array) = get x 0

[%%expect{|
external get : float32# array -> int -> float = "%floatarray_safe_get"
Line 2, characters 29-36:
2 | let d (x : float32# array) = get x 0
                                 ^^^^^^^
Error: Floatarray primitives can't be used on arrays containing
       unboxed types.
|}];;

(**************************)
(* Test 5: [@layout_poly] *)

external[@layout_poly] get : ('a : any_non_null). 'a array -> int -> 'a = "%array_safe_get"
let f1 (x : float# array) = get x 0
let f2 (x : int32# array) = get x 0
let f3 (x : int64# array) = get x 0
let f4 (x : nativeint# array) = get x 0
let f5 (x : float32# array) = get x 0

[%%expect{|
external get : ('a : any_non_null). 'a array -> int -> 'a = "%array_safe_get"
  [@@layout_poly]
val f1 : float# array -> float# = <fun>
val f2 : int32# array -> int32# = <fun>
val f3 : int64# array -> int64# = <fun>
val f4 : nativeint# array -> nativeint# = <fun>
val f5 : float32# array -> float32# = <fun>
|}];;

external[@layout_poly] set : ('a : any_non_null). 'a array -> int -> 'a -> unit = "%array_safe_set"
let f1 (x : float# array) v = set x 0 v
let f2 (x : int32# array) v = set x 0 v
let f3 (x : int64# array) v = set x 0 v
let f4 (x : nativeint# array) v = set x 0 v
let f5 (x : float32# array) v = set x 0 v

[%%expect{|
external set : ('a : any_non_null). 'a array -> int -> 'a -> unit
  = "%array_safe_set" [@@layout_poly]
val f1 : float# array -> float# -> unit = <fun>
val f2 : int32# array -> int32# -> unit = <fun>
val f3 : int64# array -> int64# -> unit = <fun>
val f4 : nativeint# array -> nativeint# -> unit = <fun>
val f5 : float32# array -> float32# -> unit = <fun>
|}]

(***********************************)
(* Test 6: sort variable inference *)

module M6_1 = struct
  (* sort var in pat *)

  let get_third arr =
    match arr with
    | [| _; _; z |] -> z
    | _ -> assert false

  let _ =  assert (Stdlib_upstream_compatible.Int32_u.equal #42l (get_third [| #0l; #1l; #42l |]))

  let _ =  assert (Stdlib_upstream_compatible.Int64_u.equal #42L (get_third [| #0L; #1L; #42L |]))
end

[%%expect{|
Line 11, characters 79-82:
11 |   let _ =  assert (Stdlib_upstream_compatible.Int64_u.equal #42L (get_third [| #0L; #1L; #42L |]))
                                                                                    ^^^
Error: This expression has type "int64#" but an expression was expected of type
         "('a : bits32)"
       The layout of int64# is bits64
         because it is unboxed version of the primitive type int64.
       But the layout of int64# must be a sublayout of bits32
         because of the definition of get_third at lines 4-7, characters 16-23.
|}]

module M6_2 = struct
  (* sort var in exp *)

  external[@layout_poly] get : ('a : any_non_null). 'a array -> int -> 'a = "%array_safe_get"

  let arr = [||]

  let f1 idx : float# = get arr idx
  let f2 idx : int32# = get arr idx
end

(* CR layouts v2.8: The jkind in the error message is wrong. It should really be
   ('a : layout float64) *)
[%%expect{|
Line 9, characters 24-35:
9 |   let f2 idx : int32# = get arr idx
                            ^^^^^^^^^^^
Error: This expression has type "('a : float64)"
       but an expression was expected of type "int32#"
       The layout of int32# is bits32
         because it is unboxed version of the primitive type int32.
       But the layout of int32# must be a sublayout of float64
         because of the definition of arr at line 6, characters 12-16.
|}]

(*********************)
(* Test 7: rec check *)

(* See upstream PR #6939 *)

let _ =
  let[@warning "-10"] rec x = [| x |]; #42.0 in
  ();;
[%%expect{|
Line 2, characters 39-44:
2 |   let[@warning "-10"] rec x = [| x |]; #42.0 in
                                           ^^^^^
Error: This expression has type "float#" but an expression was expected of type
         "('a : value)"
       The layout of float# is float64
         because it is unboxed version of the primitive type float.
       But the layout of float# must be a sublayout of value
         because it's the type of an array element,
         chosen to have layout value.
|}]

let _ =
  let[@warning "-10"] rec x = [| x |]; #42l in
  ();;

[%%expect{|
Line 2, characters 39-43:
2 |   let[@warning "-10"] rec x = [| x |]; #42l in
                                           ^^^^
Error: This expression has type "int32#" but an expression was expected of type
         "('a : value)"
       The layout of int32# is bits32
         because it is unboxed version of the primitive type int32.
       But the layout of int32# must be a sublayout of value
         because it's the type of an array element,
         chosen to have layout value.
|}]

let _ =
  let[@warning "-10"] rec x = [| x |]; #42L in
  ();;

[%%expect{|
Line 2, characters 39-43:
2 |   let[@warning "-10"] rec x = [| x |]; #42L in
                                           ^^^^
Error: This expression has type "int64#" but an expression was expected of type
         "('a : value)"
       The layout of int64# is bits64
         because it is unboxed version of the primitive type int64.
       But the layout of int64# must be a sublayout of value
         because it's the type of an array element,
         chosen to have layout value.
|}]

let _ =
  let[@warning "-10"] rec x = [| x |]; #42n in
  ();;

[%%expect{|
Line 2, characters 39-43:
2 |   let[@warning "-10"] rec x = [| x |]; #42n in
                                           ^^^^
Error: This expression has type "nativeint#"
       but an expression was expected of type "('a : value)"
       The layout of nativeint# is word
         because it is unboxed version of the primitive type nativeint.
       But the layout of nativeint# must be a sublayout of value
         because it's the type of an array element,
         chosen to have layout value.
|}]

let _ =
  let[@warning "-10"] rec x = [| x |]; #42.0s in
  ();;

[%%expect{|
Line 2, characters 39-45:
2 |   let[@warning "-10"] rec x = [| x |]; #42.0s in
                                           ^^^^^^
Error: This expression has type "float32#"
       but an expression was expected of type "('a : value)"
       The layout of float32# is float32
         because it is unboxed version of the primitive type float32.
       But the layout of float32# must be a sublayout of value
         because it's the type of an array element,
         chosen to have layout value.
|}]

(* Test 8: makearraydynamic_uninit *)

external[@layout_poly] makearray_dynamic_uninit_local
  : ('a : any_non_null) . int -> 'a array @ local = "%makearray_dynamic_uninit"

external[@layout_poly] makearray_dynamic_uninit
  : ('a : any_non_null) . int -> 'a array = "%makearray_dynamic_uninit"
[%%expect{|
external makearray_dynamic_uninit_local :
  ('a : any_non_null). int -> local_ 'a array = "%makearray_dynamic_uninit"
  [@@layout_poly]
external makearray_dynamic_uninit : ('a : any_non_null). int -> 'a array
  = "%makearray_dynamic_uninit" [@@layout_poly]
|}]

type ('a : any) with_i64s = #( int64# * 'a * int64# )

type ok_1 = #(int64# * int32#)
type ok_2 = float# with_i64s

type bad_1 = #(int * int32#)
type bad_2 = int
type bad_3 = A | B | C
type bad_4 = #{ a: int64# ; enum : bad_3 }
type bad_5 = bad_3 with_i64s
type bad_6 = #(float * #(float * float) * #(float * #(float * float * float)))
type bad_7 = #{ i : int64# ; bad_4 : bad_4 ; j : int64# }
[%%expect{|
type ('a : any) with_i64s = #(int64# * 'a * int64#)
type ok_1 = #(int64# * int32#)
type ok_2 = float# with_i64s
type bad_1 = #(int * int32#)
type bad_2 = int
type bad_3 = A | B | C
type bad_4 = #{ a : int64#; enum : bad_3; }
type bad_5 = bad_3 with_i64s
type bad_6 =
    #(float * #(float * float) * #(float * #(float * float * float)))
type bad_7 = #{ i : int64#; bad_4 : bad_4; j : int64#; }
|}]

(* Allowed usages *)

let _ =
 (makearray_dynamic_uninit 0 : float# array)
[%%expect{|
- : float# array = [||]
|}]

let _ =
 (makearray_dynamic_uninit 0 : ok_1 array)
[%%expect{|
- : ok_1 array = [||]
|}]

let _ =
 (makearray_dynamic_uninit 0 : ok_2 array)
[%%expect{|
- : ok_2 array = [||]
|}]

(* Disallowed usages *)

let _ =
 (makearray_dynamic_uninit 0 : int array)
[%%expect{|
Line 2, characters 2-28:
2 |  (makearray_dynamic_uninit 0 : int array)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 (makearray_dynamic_uninit 0 : float array)
[%%expect{|
Line 2, characters 2-28:
2 |  (makearray_dynamic_uninit 0 : float array)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 (makearray_dynamic_uninit 0 : #(int64# * int) array)
[%%expect{|
Line 2, characters 2-28:
2 |  (makearray_dynamic_uninit 0 : #(int64# * int) array)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 (makearray_dynamic_uninit 0 : bad_1 array)
[%%expect{|
Line 2, characters 2-28:
2 |  (makearray_dynamic_uninit 0 : bad_1 array)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 (makearray_dynamic_uninit 0 : bad_2 array)
[%%expect{|
Line 2, characters 2-28:
2 |  (makearray_dynamic_uninit 0 : bad_2 array)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 (makearray_dynamic_uninit 0 : bad_3 array)
[%%expect{|
Line 2, characters 2-28:
2 |  (makearray_dynamic_uninit 0 : bad_3 array)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 (makearray_dynamic_uninit 0 : bad_4 array)
[%%expect{|
Line 2, characters 2-28:
2 |  (makearray_dynamic_uninit 0 : bad_4 array)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 (makearray_dynamic_uninit 0 : bad_5 array)
[%%expect{|
Line 2, characters 2-28:
2 |  (makearray_dynamic_uninit 0 : bad_5 array)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 (makearray_dynamic_uninit 0 : bad_6 array)
[%%expect{|
Line 2, characters 2-28:
2 |  (makearray_dynamic_uninit 0 : bad_6 array)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
  (makearray_dynamic_uninit 0 : bad_7 array)
[%%expect{|
Line 2, characters 3-29:
2 |   (makearray_dynamic_uninit 0 : bad_7 array)
       ^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

(* Allowed usages (local) *)

let _ =
 let _ = (makearray_dynamic_uninit_local 0 : ok_1 array) in
 ()
[%%expect{|
- : unit = ()
|}]

let _ =
 let _ = (makearray_dynamic_uninit_local 0 : ok_2 array) in
 ()
[%%expect{|
- : unit = ()
|}]

(* Disallowed usages (local) *)

let _ =
 let _ = (makearray_dynamic_uninit_local 0 : int array) in
 ()
[%%expect{|
Line 2, characters 10-42:
2 |  let _ = (makearray_dynamic_uninit_local 0 : int array) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 let _ = (makearray_dynamic_uninit_local 0 : #(int64# * int) array) in
 ()
[%%expect{|
Line 2, characters 10-42:
2 |  let _ = (makearray_dynamic_uninit_local 0 : #(int64# * int) array) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 let _ = (makearray_dynamic_uninit_local 0 : bad_1 array) in
 ()
[%%expect{|
Line 2, characters 10-42:
2 |  let _ = (makearray_dynamic_uninit_local 0 : bad_1 array) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 let _ = (makearray_dynamic_uninit_local 0 : bad_2 array) in
 ()
[%%expect{|
Line 2, characters 10-42:
2 |  let _ = (makearray_dynamic_uninit_local 0 : bad_2 array) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 let _ = (makearray_dynamic_uninit_local 0 : bad_3 array) in
 ()
[%%expect{|
Line 2, characters 10-42:
2 |  let _ = (makearray_dynamic_uninit_local 0 : bad_3 array) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 let _ = (makearray_dynamic_uninit_local 0 : bad_4 array) in
 ()
[%%expect{|
Line 2, characters 10-42:
2 |  let _ = (makearray_dynamic_uninit_local 0 : bad_4 array) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 let _ = (makearray_dynamic_uninit_local 0 : bad_5 array) in
 ()
[%%expect{|
Line 2, characters 10-42:
2 |  let _ = (makearray_dynamic_uninit_local 0 : bad_5 array) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
 let _ = (makearray_dynamic_uninit_local 0 : bad_6 array) in
 ()
[%%expect{|
Line 2, characters 10-42:
2 |  let _ = (makearray_dynamic_uninit_local 0 : bad_6 array) in
              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]

let _ =
  let _ = (makearray_dynamic_uninit_local 0 : bad_7 array) in
  ()
[%%expect{|
Line 2, characters 11-43:
2 |   let _ = (makearray_dynamic_uninit_local 0 : bad_7 array) in
               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Error: %makearray_dynamic_uninit can only be used for GC-ignorable arrays
       not involving tagged immediates; and arrays of unboxed numbers.
       Use %makearray instead, providing an initializer.
|}]
