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

type t = int16#

let size = 16

external of_int16 : int16 -> int16# = "%int16#_of_int16"

external to_int16 : int16# -> int16 = "%int16_of_int16#"

external of_int : int -> int16# = "%int16#_of_int"

external to_int : int16# -> int = "%int_of_int16#"

external ( < ) : int16# -> int16# -> bool = "%int16#_lessthan"

let zero () = of_int 0

let one () = of_int 1

let minus_one () = of_int (-1)

external neg : int16# -> int16# = "%int16#_neg"

external add : int16# -> int16# -> int16# = "%int16#_add"

external sub : int16# -> int16# -> int16# = "%int16#_sub"

external mul : int16# -> int16# -> int16# = "%int16#_mul"

external div : int16# -> int16# -> int16# = "%int16#_div"

external rem : int16# -> int16# -> int16# = "%int16#_mod"

external succ : int16# -> int16# = "%int16#_succ"

external pred : int16# -> int16# = "%int16#_pred"

external logand : int16# -> int16# -> int16# = "%int16#_and"

external logor : int16# -> int16# -> int16# = "%int16#_or"

external logxor : int16# -> int16# -> int16# = "%int16#_xor"

let[@inline] lognot x = logxor x (minus_one ())

external shift_left : int16# -> int -> int16# = "%int16#_lsl"

external shift_right : int16# -> int -> int16# = "%int16#_asr"

external shift_right_logical : int16# -> int -> int16# = "%int16#_lsr"

let[@inline] abs x = if x < zero () then neg x else x

external equal : int16# -> int16# -> bool = "%int16#_equal"

external compare : int16# -> int16# -> int = "%int16#_compare"

let[@inline] min x y = if x < y then x else y

let[@inline] max x y = if x < y then y else x

external of_float : float -> int16# = "%int16#_of_float"

external to_float : int16# -> float = "%float_of_int16#"

let[@inline] to_string t = Int.to_string (to_int t)

let max_int () = shift_right_logical (minus_one ()) 1

let min_int () = succ (max_int ())

let[@inline] unsigned_to_int t = to_int t land ((1 lsl size) - 1)

external unsigned_compare : int16# -> int16# -> int = "%int16#_unsigned_compare"

external unsigned_lt : int16# -> int16# -> bool = "%int16#_unsigned_lessthan"

(* Unsigned division from signed division of the same bitness. See Warren Jr.,
   Henry S. (2013). Hacker's Delight (2 ed.), Sec 9-3. *)
let[@inline] unsigned_div n d =
  if d < zero ()
  then if unsigned_lt n d then zero () else one ()
  else
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if unsigned_lt r d then q else succ q

let[@inline] unsigned_rem n d = sub n (mul (unsigned_div n d) d)

let seeded_hash seed x = Stdlib.Hashtbl.seeded_hash seed (to_int x)

let hash x = Stdlib.Hashtbl.hash (to_int x)
