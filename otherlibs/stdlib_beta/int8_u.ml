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

type t = int8#

[@@@ocaml.flambda_o3]

(** Tag a [int8#] *)
external to_int8 : int8# -> int8 = "%tag_int8" [@@warning "-187"]

(** Untag a tagged [int8] *)
external of_int8 : int8 -> int8# = "%untag_int8" [@@warning "-187"]

let size = Int8.size

let[@inline always] zero () = of_int8 Int8.zero

let[@inline always] one () = of_int8 Int8.one

let[@inline always] minus_one () = of_int8 Int8.minus_one

let[@inline always] max_int () = of_int8 Int8.max_int

let[@inline always] min_int () = of_int8 Int8.min_int

let[@inline always] neg x = of_int8 (Int8.neg (to_int8 x))

let[@inline always] add x y = of_int8 (Int8.add (to_int8 x) (to_int8 y))

let[@inline always] sub x y = of_int8 (Int8.sub (to_int8 x) (to_int8 y))

let[@inline always] mul x y = of_int8 (Int8.mul (to_int8 x) (to_int8 y))

let[@inline always] div x y = of_int8 (Int8.div (to_int8 x) (to_int8 y))

let[@inline always] rem x y = of_int8 (Int8.rem (to_int8 x) (to_int8 y))

let[@inline always] succ x = of_int8 (Int8.succ (to_int8 x))

let[@inline always] pred x = of_int8 (Int8.pred (to_int8 x))

let[@inline always] abs x = of_int8 (Int8.abs (to_int8 x))

let[@inline always] logand x y = of_int8 (Int8.logand (to_int8 x) (to_int8 y))

let[@inline always] logor x y = of_int8 (Int8.logor (to_int8 x) (to_int8 y))

let[@inline always] logxor x y = of_int8 (Int8.logxor (to_int8 x) (to_int8 y))

let[@inline always] lognot x = of_int8 (Int8.lognot (to_int8 x))

let[@inline always] shift_left x y = of_int8 (Int8.shift_left (to_int8 x) y)

let[@inline always] shift_right x y = of_int8 (Int8.shift_right (to_int8 x) y)

let[@inline always] shift_right_logical x y = of_int8 (Int8.shift_right_logical (to_int8 x) y)

let[@inline always] equal x y = Int8.equal (to_int8 x) (to_int8 y)

let[@inline always] compare x y = Int8.compare (to_int8 x) (to_int8 y)

let[@inline always] min x y = of_int8 (Int8.min (to_int8 x) (to_int8 y))

let[@inline always] max x y = of_int8 (Int8.max (to_int8 x) (to_int8 y))

let[@inline always] of_float f = of_int8 (Int8.of_float f)

let[@inline always] to_float t = Int8.to_float (to_int8 t)

let[@inline always] to_string t = Int8.to_string (to_int8 t)

let[@inline always] to_int t = Int8.to_int (to_int8 t)

let[@inline always] of_int i = of_int8 (Int8.of_int i)
