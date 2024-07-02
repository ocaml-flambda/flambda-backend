(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Chris Casinghino, Jane Street, New York                *)
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

type t = float32#

external box_float : float# -> (float[@local_opt]) = "%box_float"

external unbox_float : (float[@local_opt]) -> float# = "%unbox_float"

external box_int32 : int32# -> (int32[@local_opt]) = "%box_int32"

external unbox_int32 : (int32[@local_opt]) -> int32# = "%unbox_int32"

external box_int64 : int64# -> (int64[@local_opt]) = "%box_int64"

external unbox_int64 : (int64[@local_opt]) -> int64# = "%unbox_int64"

external to_float32 : t -> (float32[@local_opt]) = "%box_float32"

external of_float32 : (float32[@local_opt]) -> t = "%unbox_float32"

(* CR layouts: Investigate whether it's worth making these things externals.
   Are there situations where the middle-end won't inline them and remove the
   boxing/unboxing? *)

let[@inline always] neg x = of_float32 (Float32.neg (to_float32 x))

let[@inline always] add x y = of_float32 (Float32.add (to_float32 x) (to_float32 y))

let[@inline always] sub x y = of_float32 (Float32.sub (to_float32 x) (to_float32 y))

let[@inline always] mul x y = of_float32 (Float32.mul (to_float32 x) (to_float32 y))

let[@inline always] div x y = of_float32 (Float32.div (to_float32 x) (to_float32 y))

let[@inline always] pow x y = of_float32 (Float32.pow (to_float32 x) (to_float32 y))

module Operators = struct
  let[@inline always] ( ~-. ) x = of_float32 (Float32.neg (to_float32 x))

  let[@inline always] ( +. ) x y = of_float32 (Float32.add (to_float32 x) (to_float32 y))

  let[@inline always] ( -. ) x y = of_float32 (Float32.sub (to_float32 x) (to_float32 y))

  let[@inline always] ( *. ) x y = of_float32 (Float32.mul (to_float32 x) (to_float32 y))

  let[@inline always] ( /. ) x y = of_float32 (Float32.div (to_float32 x) (to_float32 y))

  let[@inline always] ( ** ) x y = of_float32 (Float32.pow (to_float32 x) (to_float32 y))
end

let[@inline always] fma x y z = of_float32 (Float32.fma (to_float32 x) (to_float32 y) (to_float32 z))

let[@inline always] rem x y = of_float32 (Float32.rem (to_float32 x) (to_float32 y))

let[@inline always] succ x = of_float32 (Float32.succ (to_float32 x))

let[@inline always] pred x = of_float32 (Float32.pred (to_float32 x))

let[@inline always] abs x = of_float32 (Float32.abs (to_float32 x))

let[@inline always] is_finite x = Float32.is_finite (to_float32 x)

let[@inline always] is_infinite x = Float32.is_infinite (to_float32 x)

let[@inline always] is_nan x = Float32.is_nan (to_float32 x)

let[@inline always] is_integer x = Float32.is_integer (to_float32 x)

let[@inline always] of_int x = of_float32 (Float32.of_int x)

let[@inline always] to_int x = Float32.to_int (to_float32 x)

let[@inline always] of_int64 x = of_float32 (Float32.of_int64 (box_int64 x))

let[@inline always] to_int64 x = unbox_int64 (Float32.to_int64 (to_float32 x))
let[@inline always] of_float x = of_float32 (Float32.of_float (box_float x))

let[@inline always] to_float x = unbox_float (Float32.to_float (to_float32 x))

let[@inline always] of_bits x = of_float32 (Float32.of_bits (box_int32 x))

let[@inline always] to_bits x = unbox_int32 (Float32.to_bits (to_float32 x))

let[@inline always] of_string x = of_float32 (Float32.of_string x)

let[@inline always] to_string x = Float32.to_string (to_float32 x)

type fpclass = Stdlib.fpclass =
    FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

let[@inline always] classify_float x = Float32.classify_float (to_float32 x)

let[@inline always] sqrt x = of_float32 (Float32.sqrt (to_float32 x))

let[@inline always] cbrt x = of_float32 (Float32.cbrt (to_float32 x))

let[@inline always] exp x = of_float32 (Float32.exp (to_float32 x))

let[@inline always] exp2 x = of_float32 (Float32.exp2 (to_float32 x))

let[@inline always] log x = of_float32 (Float32.log (to_float32 x))

let[@inline always] log10 x = of_float32 (Float32.log10 (to_float32 x))

let[@inline always] log2 x = of_float32 (Float32.log2 (to_float32 x))

let[@inline always] expm1 x = of_float32 (Float32.expm1 (to_float32 x))

let[@inline always] log1p x = of_float32 (Float32.log1p (to_float32 x))

let[@inline always] cos x = of_float32 (Float32.cos (to_float32 x))

let[@inline always] sin x = of_float32 (Float32.sin (to_float32 x))

let[@inline always] tan x = of_float32 (Float32.tan (to_float32 x))

let[@inline always] acos x = of_float32 (Float32.acos (to_float32 x))

let[@inline always] asin x = of_float32 (Float32.asin (to_float32 x))

let[@inline always] atan x = of_float32 (Float32.atan (to_float32 x))

let[@inline always] atan2 x y = of_float32 (Float32.atan2 (to_float32 x) (to_float32 y))

let[@inline always] hypot x y = of_float32 (Float32.hypot (to_float32 x) (to_float32 y))

let[@inline always] cosh x = of_float32 (Float32.cosh (to_float32 x))

let[@inline always] sinh x = of_float32 (Float32.sinh (to_float32 x))

let[@inline always] tanh x = of_float32 (Float32.tanh (to_float32 x))

let[@inline always] acosh x = of_float32 (Float32.acosh (to_float32 x))

let[@inline always] asinh x = of_float32 (Float32.asinh (to_float32 x))

let[@inline always] atanh x = of_float32 (Float32.atanh (to_float32 x))

let[@inline always] erf x = of_float32 (Float32.erf (to_float32 x))

let[@inline always] erfc x = of_float32 (Float32.erfc (to_float32 x))

let[@inline always] trunc x = of_float32 (Float32.trunc (to_float32 x))

let[@inline always] round x = of_float32 (Float32.round (to_float32 x))

let[@inline always] ceil x = of_float32 (Float32.ceil (to_float32 x))

let[@inline always] floor x = of_float32 (Float32.floor (to_float32 x))

let[@inline always] next_after x y = of_float32 (Float32.next_after (to_float32 x) (to_float32 y))

let[@inline always] copy_sign x y = of_float32 (Float32.copy_sign (to_float32 x) (to_float32 y))

let[@inline always] sign_bit x = Float32.sign_bit (to_float32 x)

let[@inline always] ldexp x i = of_float32 (Float32.ldexp (to_float32 x) i)

let[@inline always] compare x y = Float32.compare (to_float32 x) (to_float32 y)

let[@inline always] equal x y = Float32.equal (to_float32 x) (to_float32 y)

let[@inline always] min x y = of_float32 (Float32.min (to_float32 x) (to_float32 y))

let[@inline always] max x y = of_float32 (Float32.max (to_float32 x) (to_float32 y))

module With_weird_nan_behavior = struct
  let[@inline always] min x y = of_float32 (Float32.With_weird_nan_behavior.min (to_float32 x) (to_float32 y))

  let[@inline always] max x y = of_float32 (Float32.With_weird_nan_behavior.max (to_float32 x) (to_float32 y))
end

let[@inline always] min_num x y = of_float32 (Float32.min_num (to_float32 x) (to_float32 y))

let[@inline always] max_num x y = of_float32 (Float32.max_num (to_float32 x) (to_float32 y))

let iround_half_to_even x = unbox_int64 (Float32.iround_half_to_even (to_float32 x))

let round_half_to_even x = of_float32 (Float32.round_half_to_even (to_float32 x))

let round_down x = of_float32 (Float32.round_down (to_float32 x))

let round_up x = of_float32 (Float32.round_up (to_float32 x))

let round_towards_zero x = of_float32 (Float32.round_towards_zero (to_float32 x))
