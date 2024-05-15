# 1 "int32_u.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Antal Spector-Zabusky, Jane Street, New York             *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

[@@@ocaml.flambda_o3]

type t = int32#

external to_int32 : t -> (int32[@local_opt]) = "%box_int32"

external of_int32 : (int32[@local_opt]) -> t = "%unbox_int32"

let[@inline always] neg x = of_int32 (Int32.neg (to_int32 x))

let[@inline always] add x y = of_int32 (Int32.add (to_int32 x) (to_int32 y))

let[@inline always] sub x y = of_int32 (Int32.sub (to_int32 x) (to_int32 y))

let[@inline always] mul x y = of_int32 (Int32.mul (to_int32 x) (to_int32 y))

let[@inline always] div x y = of_int32 (Int32.div (to_int32 x) (to_int32 y))

let[@inline always] unsigned_div x y =
  of_int32 ((Int32.unsigned_div[@inlined]) (to_int32 x) (to_int32 y))

let[@inline always] rem x y = of_int32 (Int32.rem (to_int32 x) (to_int32 y))

let[@inline always] unsigned_rem x y =
  of_int32 ((Int32.unsigned_rem[@inlined]) (to_int32 x) (to_int32 y))

let[@inline always] succ x = of_int32 ((Int32.succ[@inlined]) (to_int32 x))

let[@inline always] pred x = of_int32 ((Int32.pred[@inlined]) (to_int32 x))

let[@inline always] abs x = of_int32 ((Int32.abs[@inlined]) (to_int32 x))

let[@inline always] logand x y =
  of_int32 (Int32.logand (to_int32 x) (to_int32 y))

let[@inline always] logor x y = of_int32 (Int32.logor (to_int32 x) (to_int32 y))

let[@inline always] logxor x y =
  of_int32 (Int32.logxor (to_int32 x) (to_int32 y))

let[@inline always] lognot x = of_int32 ((Int32.lognot[@inlined]) (to_int32 x))

let[@inline always] shift_left x y = of_int32 (Int32.shift_left (to_int32 x) y)

let[@inline always] shift_right x y =
  of_int32 (Int32.shift_right (to_int32 x) y)

let[@inline always] shift_right_logical x y =
  of_int32 (Int32.shift_right_logical (to_int32 x) y)

let[@inline always] of_int x = of_int32 (Int32.of_int x)

let[@inline always] to_int x = Int32.to_int (to_int32 x)

let[@inline always] unsigned_to_int x =
  (Int32.unsigned_to_int[@inlined]) (to_int32 x)

let[@inline always] of_float x = of_int32 (Int32.of_float x)

let[@inline always] to_float x = Int32.to_float (to_int32 x)

let[@inline always] bits_of_float x = of_int32 (Int32.bits_of_float x)

let[@inline always] float_of_bits x = Int32.float_of_bits (to_int32 x)

let[@inline always] of_string x = of_int32 (Int32.of_string x)

let[@inline always] to_string x = (Int32.to_string[@inlined]) (to_int32 x)

let[@inline always] compare x y =
  (Int32.compare[@inlined]) (to_int32 x) (to_int32 y)

let[@inline always] unsigned_compare x y =
  (Int32.unsigned_compare[@inlined]) (to_int32 x) (to_int32 y)

let[@inline always] equal x y =
  (Int32.equal[@inlined]) (to_int32 x) (to_int32 y)

let[@inline always] min x y =
  of_int32 ((Int32.min[@inlined]) (to_int32 x) (to_int32 y))

let[@inline always] max x y =
  of_int32 ((Int32.max[@inlined]) (to_int32 x) (to_int32 y))
