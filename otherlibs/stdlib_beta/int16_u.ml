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

let int_size = Int16.int_size

let zero () = of_int16 Int16.zero

let one () = of_int16 Int16.one

let minus_one () = of_int16 Int16.minus_one

let max_int () = of_int16 Int16.max_int

let min_int () = of_int16 Int16.min_int

let neg x = of_int16 (Int16.neg (to_int16 x))

let add x y = of_int16 (Int16.add (to_int16 x) (to_int16 y))

let sub x y = of_int16 (Int16.sub (to_int16 x) (to_int16 y))

let mul x y = of_int16 (Int16.mul (to_int16 x) (to_int16 y))

let div x y = of_int16 (Int16.div (to_int16 x) (to_int16 y))

let rem x y = of_int16 (Int16.rem (to_int16 x) (to_int16 y))

let succ x = of_int16 (Int16.succ (to_int16 x))

let pred x = of_int16 (Int16.pred (to_int16 x))

let abs x = of_int16 (Int16.abs (to_int16 x))

let logand x y = of_int16 (Int16.logand (to_int16 x) (to_int16 y))

let logor x y = of_int16 (Int16.logor (to_int16 x) (to_int16 y))

let logxor x y = of_int16 (Int16.logxor (to_int16 x) (to_int16 y))

let lognot x = of_int16 (Int16.lognot (to_int16 x))

let shift_left x y = of_int16 (Int16.shift_left (to_int16 x) y)

let shift_right x y = of_int16 (Int16.shift_right (to_int16 x) y)

let shift_right_logical x y = of_int16 (Int16.shift_right_logical (to_int16 x) y)

let equal x y = Int16.equal (to_int16 x) (to_int16 y)

let compare x y = Int16.compare (to_int16 x) (to_int16 y)

let min x y = of_int16 (Int16.min (to_int16 x) (to_int16 y))

let max x y = of_int16 (Int16.max (to_int16 x) (to_int16 y))

let of_float f = of_int16 (Int16.of_float f)

let to_float t = Int16.to_float (to_int16 t)

let to_string t = Int16.to_string (to_int16 t)

let to_int t = Int16.to_int (to_int16 t)

let of_int i = of_int16 (Int16.of_int i)
