(* exercises block_set *)
type t = { mutable n : int }

let set t = t.n <- 42

(* exercises string_load *)
external unsafe_get : string -> int -> char = "%string_unsafe_get"
let nth_char s n = unsafe_get s n

(* exercises begin_try_region *)
external opaque_identity : 'a -> 'a = "%opaque"
let needs_try_region () =
  match opaque_identity 42 with
  | exception Not_found -> 0
  | n -> 1

(* exercises invalid *)
type _ s =
  | A1 : [> `a ] s
  | A2 : [> `a ] s
  | B : int -> [> `b ] s
let gadt_match (x : [ `a ] s) n =
  match x with
  | A1 -> n
  | A2 -> opaque_identity n
