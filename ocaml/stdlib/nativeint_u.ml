# 1 "nativeint_u.ml"
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

external to_nativeint : nativeint# -> (nativeint[@local_opt]) = "%box_nativeint"

external of_nativeint : (nativeint[@local_opt]) -> nativeint# =
  "%unbox_nativeint"

let[@inline always] neg x = of_nativeint (Nativeint.neg (to_nativeint x))

let[@inline always] add x y =
  of_nativeint (Nativeint.add (to_nativeint x) (to_nativeint y))

let[@inline always] sub x y =
  of_nativeint (Nativeint.sub (to_nativeint x) (to_nativeint y))

let[@inline always] mul x y =
  of_nativeint (Nativeint.mul (to_nativeint x) (to_nativeint y))

let[@inline always] div x y =
  of_nativeint (Nativeint.div (to_nativeint x) (to_nativeint y))

let[@inline always] unsigned_div x y =
  of_nativeint (Nativeint.unsigned_div (to_nativeint x) (to_nativeint y))

let[@inline always] rem x y =
  of_nativeint (Nativeint.rem (to_nativeint x) (to_nativeint y))

let[@inline always] unsigned_rem x y =
  of_nativeint (Nativeint.unsigned_rem (to_nativeint x) (to_nativeint y))

let[@inline always] succ x = of_nativeint (Nativeint.succ (to_nativeint x))

let[@inline always] pred x = of_nativeint (Nativeint.pred (to_nativeint x))

let[@inline always] abs x = of_nativeint (Nativeint.abs (to_nativeint x))

let size = Nativeint.size

let[@inline always] logand x y =
  of_nativeint (Nativeint.logand (to_nativeint x) (to_nativeint y))

let[@inline always] logor x y =
  of_nativeint (Nativeint.logor (to_nativeint x) (to_nativeint y))

let[@inline always] logxor x y =
  of_nativeint (Nativeint.logxor (to_nativeint x) (to_nativeint y))

let[@inline always] lognot x = of_nativeint (Nativeint.lognot (to_nativeint x))

let[@inline always] shift_left x y =
  of_nativeint (Nativeint.shift_left (to_nativeint x) y)

let[@inline always] shift_right x y =
  of_nativeint (Nativeint.shift_right (to_nativeint x) y)

let[@inline always] shift_right_logical x y =
  of_nativeint (Nativeint.shift_right_logical (to_nativeint x) y)

let[@inline always] of_int x = of_nativeint (Nativeint.of_int x)

let[@inline always] to_int x = Nativeint.to_int (to_nativeint x)

let[@inline always] unsigned_to_int x =
  Nativeint.unsigned_to_int (to_nativeint x)

let[@inline always] of_float x = of_nativeint (Nativeint.of_float x)

let[@inline always] to_float x = Nativeint.to_float (to_nativeint x)

let[@inline always] of_int32 x = of_nativeint (Nativeint.of_int32 x)

let[@inline always] to_int32 x = Nativeint.to_int32 (to_nativeint x)

let[@inline always] of_string x = of_nativeint (Nativeint.of_string x)

let[@inline always] to_string x = Nativeint.to_string (to_nativeint x)

type t = nativeint#

let[@inline always] compare x y =
  Nativeint.compare (to_nativeint x) (to_nativeint y)

let[@inline always] unsigned_compare x y =
  Nativeint.unsigned_compare (to_nativeint x) (to_nativeint y)

let[@inline always] equal x y =
  Nativeint.equal (to_nativeint x) (to_nativeint y)

let[@inline always] min x y =
  of_nativeint (Nativeint.min (to_nativeint x) (to_nativeint y))

let[@inline always] max x y =
  of_nativeint (Nativeint.max (to_nativeint x) (to_nativeint y))
