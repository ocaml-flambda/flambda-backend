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

let int_size = Int8.int_size

let zero () = of_int8 Int8.zero

let one () = of_int8 Int8.one

let minus_one () = of_int8 Int8.minus_one

let max_int () = of_int8 Int8.max_int

let min_int () = of_int8 Int8.min_int

let neg x = of_int8 (Int8.neg (to_int8 x))

let add x y = of_int8 (Int8.add (to_int8 x) (to_int8 y))

let sub x y = of_int8 (Int8.sub (to_int8 x) (to_int8 y))

let mul x y = of_int8 (Int8.mul (to_int8 x) (to_int8 y))

let div x y = of_int8 (Int8.div (to_int8 x) (to_int8 y))

let rem x y = of_int8 (Int8.rem (to_int8 x) (to_int8 y))

let succ x = of_int8 (Int8.succ (to_int8 x))

let pred x = of_int8 (Int8.pred (to_int8 x))

let abs x = of_int8 (Int8.abs (to_int8 x))

let logand x y = of_int8 (Int8.logand (to_int8 x) (to_int8 y))

let logor x y = of_int8 (Int8.logor (to_int8 x) (to_int8 y))

let logxor x y = of_int8 (Int8.logxor (to_int8 x) (to_int8 y))

let lognot x = of_int8 (Int8.lognot (to_int8 x))

let shift_left x y = of_int8 (Int8.shift_left (to_int8 x) y)

let shift_right x y = of_int8 (Int8.shift_right (to_int8 x) y)

let shift_right_logical x y = of_int8 (Int8.shift_right_logical (to_int8 x) y)

let equal x y = Int8.equal (to_int8 x) (to_int8 y)

let compare x y = Int8.compare (to_int8 x) (to_int8 y)

let min x y = of_int8 (Int8.min (to_int8 x) (to_int8 y))

let max x y = of_int8 (Int8.max (to_int8 x) (to_int8 y))

let of_float f = of_int8 (Int8.of_float f)

let to_float t = Int8.to_float (to_int8 t)

let to_string t = Int8.to_string (to_int8 t)

let to_int t = Int8.to_int (to_int8 t)

let of_int i = of_int8 (Int8.of_int i)
