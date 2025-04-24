(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.flambda_o3]

open! Stdlib

type t = int8

let size = 8

external of_int : int -> int8 = "%int8_of_int"

external to_int : int8 -> int = "%int_of_int8"

external ( < ) : int8 -> int8 -> bool = "%int8_lessthan"

let zero = of_int 0

let one = of_int 1

let minus_one = of_int (-1)

external neg : int8 -> int8 = "%int8_neg"

external add : int8 -> int8 -> int8 = "%int8_add"

external sub : int8 -> int8 -> int8 = "%int8_sub"

external mul : int8 -> int8 -> int8 = "%int8_mul"

external div : int8 -> int8 -> int8 = "%int8_div"

external rem : int8 -> int8 -> int8 = "%int8_mod"

external succ : int8 -> int8 = "%int8_succ"

external pred : int8 -> int8 = "%int8_pred"

external logand : int8 -> int8 -> int8 = "%int8_and"

external logor : int8 -> int8 -> int8 = "%int8_or"

external logxor : int8 -> int8 -> int8 = "%int8_xor"

let[@inline] lognot x = logxor x minus_one

external shift_left : int8 -> int -> int8 = "%int8_lsl"

external shift_right : int8 -> int -> int8 = "%int8_asr"

external shift_right_logical : int8 -> int -> int8 = "%int8_lsr"

let[@inline] abs x = if x < zero then neg x else x

external equal : int8 -> int8 -> bool = "%int8_equal"

external compare : int8 -> int8 -> int = "%int8_compare"

let[@inline] min x y = if x < y then x else y

let[@inline] max x y = if x < y then y else x

external of_float : float -> int8 = "%int8_of_float"

external to_float : int8 -> float = "%float_of_int8"

let[@inline] to_string t = Int.to_string (to_int t)

let max_int = shift_right_logical minus_one 1

let min_int = succ max_int

let[@inline] unsigned_to_int t = to_int t land ((1 lsl size) - 1)

external unsigned_compare : int8 -> int8 -> int = "%int8_unsigned_compare"

external unsigned_lt : int8 -> int8 -> bool = "%int8_unsigned_lessthan"

(* Unsigned division from signed division of the same bitness. See Warren Jr.,
   Henry S. (2013). Hacker's Delight (2 ed.), Sec 9-3. *)
let[@inline] unsigned_div n d =
  if d < zero
  then if unsigned_lt n d then zero else one
  else
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if unsigned_lt r d then q else succ q

let[@inline] unsigned_rem n d = sub n (mul (unsigned_div n d) d)

let seeded_hash seed x = Stdlib.Hashtbl.seeded_hash seed (to_int x)

let hash x = Stdlib.Hashtbl.hash (to_int x)
