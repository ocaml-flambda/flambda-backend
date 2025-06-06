(* TEST
 include stdlib_upstream_compatible;
 flambda2;
 expect;
*)

type ('a : any_non_null) t

external unsafe_get : ('a : any_non_null). 'a t -> int -> 'a
  = "%array_unsafe_get"
[@@layout_poly]
external unsafe_set : ('a : any_non_null). 'a t -> int -> 'a -> unit
  = "%array_unsafe_set"
[@@layout_poly]
external safe_get : ('a : any_non_null). 'a t -> int -> 'a
  = "%array_safe_get"
[@@layout_poly]
external safe_set : ('a : any_non_null). 'a t -> int -> 'a -> unit
  = "%array_safe_set"
[@@layout_poly]
external length : ('a : any_non_null). 'a t -> int
  = "%array_length"
[@@layout_poly]
external size_in_bytes : ('a : any_non_null). 'a t -> int
  = "%array_element_size_in_bytes"
[@@layout_poly]
[%%expect{|
type ('a : any_non_null) t
external unsafe_get : ('a : any_non_null). 'a t -> int -> 'a
  = "%array_unsafe_get" [@@layout_poly]
external unsafe_set : ('a : any_non_null). 'a t -> int -> 'a -> unit
  = "%array_unsafe_set" [@@layout_poly]
external safe_get : ('a : any_non_null). 'a t -> int -> 'a
  = "%array_safe_get" [@@layout_poly]
external safe_set : ('a : any_non_null). 'a t -> int -> 'a -> unit
  = "%array_safe_set" [@@layout_poly]
external length : ('a : any_non_null). 'a t -> int = "%array_length"
  [@@layout_poly]
external size_in_bytes : ('a : any_non_null). 'a t -> int
  = "%array_element_size_in_bytes" [@@layout_poly]
|}]

(* Because [%array_unsafe_get] cannot see the array type kind from [a], its
   return type must be value. So this is allowed: *)
let f a i : string = unsafe_get a i
[%%expect{|
val f : string t -> int -> string = <fun>
|}]

(* But this is not: *)
let bad a i : int64# = unsafe_get a i
[%%expect{|
Line 1, characters 23-37:
1 | let bad a i : int64# = unsafe_get a i
                           ^^^^^^^^^^^^^^
Error: This array operation cannot tell whether int64# t is an array type,
       possibly because it is abstract. In this case, the element type
       int64# must be a value:

       The layout of int64# is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of int64# must be a sublayout of value
         because it's the element type for an array operation with an opaque
         array type.
|}]

(* [%array_safe_get] is similiar *)
let f a i : string = safe_get a i
[%%expect{|
val f : string t -> int -> string = <fun>
|}]

let bad a i : int64# = safe_get a i
[%%expect{|
Line 1, characters 23-35:
1 | let bad a i : int64# = safe_get a i
                           ^^^^^^^^^^^^
Error: This array operation cannot tell whether int64# t is an array type,
       possibly because it is abstract. In this case, the element type
       int64# must be a value:

       The layout of int64# is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of int64# must be a sublayout of value
         because it's the element type for an array operation with an opaque
         array type.
|}]

(* [%array_unsafe_set] looks at the third parameter type to determine the array
   type kind *)
let f a i (v : string) = unsafe_set a i v
[%%expect{|
val f : string t -> int -> string -> unit = <fun>
|}]

let bad a i (v : int64#) = unsafe_set a i v
[%%expect{|
Line 1, characters 27-43:
1 | let bad a i (v : int64#) = unsafe_set a i v
                               ^^^^^^^^^^^^^^^^
Error: This array operation cannot tell whether int64# t is an array type,
       possibly because it is abstract. In this case, the element type
       int64# must be a value:

       The layout of int64# is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of int64# must be a sublayout of value
         because it's the element type for an array operation with an opaque
         array type.
|}]

(* [%array_safe_set] is similiar *)
let f a i (v : string) = safe_set a i v
[%%expect{|
val f : string t -> int -> string -> unit = <fun>
|}]

let bad a i (v : int64#) = safe_set a i v
[%%expect{|
Line 1, characters 27-41:
1 | let bad a i (v : int64#) = safe_set a i v
                               ^^^^^^^^^^^^^^
Error: This array operation cannot tell whether int64# t is an array type,
       possibly because it is abstract. In this case, the element type
       int64# must be a value:

       The layout of int64# is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of int64# must be a sublayout of value
         because it's the element type for an array operation with an opaque
         array type.
|}]

(* [%array_length] and [%array_element_size_in_bytes] require that the array
   type be visible *)
let bad a = length a
[%%expect{|
Line 1, characters 12-20:
1 | let bad a = length a
                ^^^^^^^^
Error: This array operation expects an array type, but 'a t does not appear
       to be one. (Hint: it is abstract?)
|}]

let bad a = size_in_bytes a
[%%expect{|
Line 1, characters 12-27:
1 | let bad a = size_in_bytes a
                ^^^^^^^^^^^^^^^
Error: This array operation expects an array type, but 'a t does not appear
       to be one. (Hint: it is abstract?)
|}]

(* Here is a more realistic failing example: *)
module M : sig
  type t
  val t : t
  external unsafe_get : t -> int -> int64# = "%array_unsafe_get"
end = struct
  type t = int64# array
  let t = [| #0L |]
  external unsafe_get : t -> int -> int64# = "%array_unsafe_get"
end

let get () : int -> int64# = M.unsafe_get M.t
[%%expect{|
module M :
  sig
    type t
    val t : t
    external unsafe_get : t -> int -> int64# = "%array_unsafe_get"
  end
Line 11, characters 29-41:
11 | let get () : int -> int64# = M.unsafe_get M.t
                                  ^^^^^^^^^^^^
Error: This array operation cannot tell whether M.t is an array type,
       possibly because it is abstract. In this case, the element type
       int64# must be a value:

       The layout of int64# is bits64
         because it is the unboxed version of the primitive type int64.
       But the layout of int64# must be a sublayout of value
         because it's the element type for an array operation with an opaque
         array type.
|}]
