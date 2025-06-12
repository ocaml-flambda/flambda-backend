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
val bad : int64# t -> int -> int64# = <fun>
|}]

(* [%array_safe_get] is similiar *)
let f a i : string = safe_get a i
[%%expect{|
val f : string t -> int -> string = <fun>
|}]

let bad a i : int64# = safe_get a i
[%%expect{|
val bad : int64# t -> int -> int64# = <fun>
|}]

(* [%array_unsafe_set] looks at the third parameter type to determine the array
   type kind *)
let f a i (v : string) = unsafe_set a i v
[%%expect{|
val f : string t -> int -> string -> unit = <fun>
|}]

let bad a i (v : int64#) = unsafe_set a i v
[%%expect{|
val bad : int64# t -> int -> int64# -> unit = <fun>
|}]

(* [%array_safe_set] is similiar *)
let f a i (v : string) = safe_set a i v
[%%expect{|
val f : string t -> int -> string -> unit = <fun>
|}]

let bad a i (v : int64#) = safe_set a i v
[%%expect{|
val bad : int64# t -> int -> int64# -> unit = <fun>
|}]

(* [%array_length] and [%array_element_size_in_bytes] require that the array
   type be visible *)
let bad a = length a
[%%expect{|
val bad : 'a t -> int = <fun>
|}]

let bad a = size_in_bytes a
[%%expect{|
val bad : 'a t -> int = <fun>
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
val get : unit -> int -> int64# = <fun>
|}]
