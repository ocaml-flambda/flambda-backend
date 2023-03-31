(* Exercise empty array *)
let list_to_array = function
  | [] -> [| |]
  | _ :: _ -> assert false

(* Exercise %int_comp Eq (generates %int_comp imm untagged = 2i) *)
type t_imm = A of int | B of int | C of int | D of int
let is_c x = function C _ -> true | _ -> false

(* Exercise bytes_or_bigstring_set *)
external unsafe_set : bytes -> int -> char -> unit = "%bytes_unsafe_set"
let set_to_x b i = unsafe_set b i 'x'

(* Exercise Boolean_not *)
external not : bool -> bool = "%boolnot"
let classical_id b = not (not b)

(* Exercise unary int arithmetic *)
external bswap : int -> int = "%bswap16"
let swapped () = bswap 0x11223344

external negint : int -> int = "%negint"
let negate x = negint x

(* Exercise string literals with escape characters *)
let my_string = " \\ \n "
