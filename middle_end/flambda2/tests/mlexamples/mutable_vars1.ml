(* Reduced from Oprint.escape_string *)

external length : string -> int = "%string_length"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"
type 'a ref = { mutable contents : 'a; }
external ref : 'a -> 'a ref = "%makemutable"
external ( ! ) : 'a ref -> 'a = "%field0"
external unsafe_get : string -> int -> char = "%string_unsafe_get"
external incr : int ref -> unit = "%incr"

let escape_string s s' =
  let n = ref 0 in
  for i = 0 to length s do
    begin match unsafe_get s i with
    | '\x00' -> incr n
    | c -> unsafe_set s' !n c
    end;
    incr n
  done
