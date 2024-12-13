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

open! Stdlib

type t = int16

[@@@ocaml.flambda_o3]

external to_int : t -> int = "%identity"

external unsafe_of_int : int -> t = "%identity"

let int_size = 16

let max_int = (1 lsl (int_size - 1)) - 1

let min_int = -1 lsl (int_size - 1)

let mask = (1 lsl int_size) - 1

let unused_bits = Sys.int_size - int_size

let[@inline] of_int i = unsafe_of_int ((i lsl unused_bits) asr unused_bits)

let zero = of_int 0

let one = of_int 1

let minus_one = of_int (-1)

let[@inline always] neg x = of_int (Int.neg (to_int x))

let[@inline always] add x y = of_int (Int.add (to_int x) (to_int y))

let[@inline always] sub x y = of_int (Int.sub (to_int x) (to_int y))

let[@inline always] mul x y = of_int (Int.mul (to_int x) (to_int y))

let[@inline always] div x y = of_int (Int.div (to_int x) (to_int y))

let[@inline always] rem x y = of_int (Int.rem (to_int x) (to_int y))

let[@inline always] succ x = of_int (Int.succ (to_int x))

let[@inline always] pred x = of_int (Int.pred (to_int x))

let[@inline always] abs x = of_int (Int.abs (to_int x))

let[@inline always] logand x y =
  unsafe_of_int (Int.logand (to_int x) (to_int y))

let[@inline always] logor x y = unsafe_of_int (Int.logor (to_int x) (to_int y))

let[@inline always] logxor x y =
  unsafe_of_int (Int.logxor (to_int x) (to_int y))

let[@inline always] lognot x = unsafe_of_int (Int.lognot (to_int x))

let[@inline always] shift_left x y = of_int (Int.shift_left (to_int x) y)

let[@inline always] shift_right x y = of_int (Int.shift_right (to_int x) y)

let[@inline always] shift_right_logical x y =
  of_int (Int.shift_right_logical (to_int x land mask) y)

let[@inline always] equal x y = Int.equal (to_int x) (to_int y)

let[@inline always] compare x y = Int.compare (to_int x) (to_int y)

let[@inline always] min x y = if to_int x > to_int y then y else x

let[@inline always] max x y = if to_int x < to_int y then y else x

let[@inline always] of_float f =
  let i = Int.of_float f in
  if min_int <= i && i <= max_int then unsafe_of_int i else zero

let[@inline always] to_float t = Int.to_float (to_int t)

let[@inline always] to_string t = Int.to_string (to_int t)

let max_int = of_int max_int

let min_int = of_int min_int
