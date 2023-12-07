# 1 "int64_u.ml"
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

external to_int64 : int64# -> (int64[@local_opt]) = "%box_int64"

external of_int64 : (int64[@local_opt]) -> int64# = "%unbox_int64"

let[@inline always] neg x = of_int64 (Int64.neg (to_int64 x))

let[@inline always] add x y = of_int64 (Int64.add (to_int64 x) (to_int64 y))

let[@inline always] sub x y = of_int64 (Int64.sub (to_int64 x) (to_int64 y))

let[@inline always] mul x y = of_int64 (Int64.mul (to_int64 x) (to_int64 y))

let[@inline always] div x y = of_int64 (Int64.div (to_int64 x) (to_int64 y))

let[@inline always] unsigned_div x y =
  of_int64 ((Int64.unsigned_div[@inlined]) (to_int64 x) (to_int64 y))

let[@inline always] rem x y = of_int64 (Int64.rem (to_int64 x) (to_int64 y))

let[@inline always] unsigned_rem x y =
  of_int64 ((Int64.unsigned_rem[@inlined]) (to_int64 x) (to_int64 y))

let[@inline always] succ x = of_int64 ((Int64.succ[@inlined]) (to_int64 x))

let[@inline always] pred x = of_int64 ((Int64.pred[@inlined]) (to_int64 x))

let[@inline always] abs x = of_int64 ((Int64.abs[@inlined]) (to_int64 x))

let[@inline always] logand x y =
  of_int64 (Int64.logand (to_int64 x) (to_int64 y))

let[@inline always] logor x y = of_int64 (Int64.logor (to_int64 x) (to_int64 y))

let[@inline always] logxor x y =
  of_int64 (Int64.logxor (to_int64 x) (to_int64 y))

let[@inline always] lognot x = of_int64 ((Int64.lognot[@inlined]) (to_int64 x))

let[@inline always] shift_left x y = of_int64 (Int64.shift_left (to_int64 x) y)

let[@inline always] shift_right x y =
  of_int64 (Int64.shift_right (to_int64 x) y)

let[@inline always] shift_right_logical x y =
  of_int64 (Int64.shift_right_logical (to_int64 x) y)

let[@inline always] of_int x = of_int64 (Int64.of_int x)

let[@inline always] to_int x = Int64.to_int (to_int64 x)

let[@inline always] unsigned_to_int x =
  (Int64.unsigned_to_int[@inlined]) (to_int64 x)

let[@inline always] of_float x = of_int64 (Int64.of_float x)

let[@inline always] to_float x = Int64.to_float (to_int64 x)

let[@inline always] of_int32 x = of_int64 (Int64.of_int32 x)

let[@inline always] to_int32 x = Int64.to_int32 (to_int64 x)

let[@inline always] of_nativeint x = of_int64 (Int64.of_nativeint x)

let[@inline always] to_nativeint x = Int64.to_nativeint (to_int64 x)

let[@inline always] of_int32_u x = of_int64 (Int64.of_int32 (Stdlib__Int32_u.to_int32 x))

let[@inline always] to_int32_u x = Stdlib__Int32_u.of_int32 (Int64.to_int32 (to_int64 x))

let[@inline always] of_nativeint_u x =
  of_int64 (Int64.of_nativeint (Stdlib__Nativeint_u.to_nativeint x))

let[@inline always] to_nativeint_u x =
  Stdlib__Nativeint_u.of_nativeint (Int64.to_nativeint (to_int64 x))

let[@inline always] bits_of_float x = of_int64 (Int64.bits_of_float x)

let[@inline always] float_of_bits x = Int64.float_of_bits (to_int64 x)

let[@inline always] of_string x = of_int64 (Int64.of_string x)

let[@inline always] to_string x = (Int64.to_string[@inlined]) (to_int64 x)

type t = int64#

let[@inline always] compare x y = Int64.compare (to_int64 x) (to_int64 y)

let[@inline always] unsigned_compare x y =
  Int64.unsigned_compare (to_int64 x) (to_int64 y)

let[@inline always] equal x y = Int64.equal (to_int64 x) (to_int64 y)

let[@inline always] min x y = of_int64 (Int64.min (to_int64 x) (to_int64 y))

let[@inline always] max x y = of_int64 (Int64.max (to_int64 x) (to_int64 y))
