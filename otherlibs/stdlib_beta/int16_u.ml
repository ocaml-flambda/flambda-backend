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

type t = int16#

[@@@ocaml.flambda_o3]

(** Tag a [int16#] *)
external to_int16 : int16# -> int16 = "%tag_int16" [@@warning "-187"]

(** Untag a tagged [int16] *)
external of_int16 : int16 -> int16# = "%untag_int16" [@@warning "-187"]

let size = Int16.size

let[@inline always] zero () = of_int16 Int16.zero

let[@inline always] one () = of_int16 Int16.one

let[@inline always] minus_one () = of_int16 Int16.minus_one

let[@inline always] max_int () = of_int16 Int16.max_int

let[@inline always] min_int () = of_int16 Int16.min_int

let[@inline always] neg x = of_int16 (Int16.neg (to_int16 x))

let[@inline always] add x y = of_int16 (Int16.add (to_int16 x) (to_int16 y))

let[@inline always] sub x y = of_int16 (Int16.sub (to_int16 x) (to_int16 y))

let[@inline always] mul x y = of_int16 (Int16.mul (to_int16 x) (to_int16 y))

let[@inline always] div x y = of_int16 (Int16.div (to_int16 x) (to_int16 y))

let[@inline always] rem x y = of_int16 (Int16.rem (to_int16 x) (to_int16 y))

let[@inline always] succ x = of_int16 (Int16.succ (to_int16 x))

let[@inline always] pred x = of_int16 (Int16.pred (to_int16 x))

let[@inline always] abs x = of_int16 (Int16.abs (to_int16 x))

let[@inline always] logand x y = of_int16 (Int16.logand (to_int16 x) (to_int16 y))

let[@inline always] logor x y = of_int16 (Int16.logor (to_int16 x) (to_int16 y))

let[@inline always] logxor x y = of_int16 (Int16.logxor (to_int16 x) (to_int16 y))

let[@inline always] lognot x = of_int16 (Int16.lognot (to_int16 x))

let[@inline always] shift_left x y = of_int16 (Int16.shift_left (to_int16 x) y)

let[@inline always] shift_right x y = of_int16 (Int16.shift_right (to_int16 x) y)

let[@inline always] shift_right_logical x y = of_int16 (Int16.shift_right_logical (to_int16 x) y)

let[@inline always] equal x y = Int16.equal (to_int16 x) (to_int16 y)

let[@inline always] compare x y = Int16.compare (to_int16 x) (to_int16 y)

let[@inline always] min x y = of_int16 (Int16.min (to_int16 x) (to_int16 y))

let[@inline always] max x y = of_int16 (Int16.max (to_int16 x) (to_int16 y))

let[@inline always] of_float f = of_int16 (Int16.of_float f)

let[@inline always] to_float t = Int16.to_float (to_int16 t)

let[@inline always] to_string t = Int16.to_string (to_int16 t)

let[@inline always] to_int t = Int16.to_int (to_int16 t)

let[@inline always] of_int i = of_int16 (Int16.of_int i)
