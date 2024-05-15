# 1 "float_u.ml"
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

type t = float#

external to_float : t -> (float[@local_opt]) = "%box_float"

external of_float : (float[@local_opt]) -> t = "%unbox_float"

(* CR layouts: Investigate whether it's worth making these things externals.
   Are there situations where the middle-end won't inline them and remove the
   boxing/unboxing? *)

let[@inline always] neg x = of_float (Float.neg (to_float x))

let[@inline always] add x y = of_float (Float.add (to_float x) (to_float y))

let[@inline always] sub x y = of_float (Float.sub (to_float x) (to_float y))

let[@inline always] mul x y = of_float (Float.mul (to_float x) (to_float y))

let[@inline always] div x y = of_float (Float.div (to_float x) (to_float y))

let[@inline always] fma x y z = of_float (Float.fma (to_float x) (to_float y) (to_float z))

let[@inline always] rem x y = of_float (Float.rem (to_float x) (to_float y))

let[@inline always] succ x = of_float (Float.succ (to_float x))

let[@inline always] pred x = of_float (Float.pred (to_float x))

let[@inline always] abs x = of_float (Float.abs (to_float x))

let[@inline always] is_finite x = Float.is_finite (to_float x)

let[@inline always] is_infinite x = Float.is_infinite (to_float x)

let[@inline always] is_nan x = Float.is_nan (to_float x)

let[@inline always] is_integer x = Float.is_integer (to_float x)

let[@inline always] of_int x = of_float (Float.of_int x)

let[@inline always] to_int x = Float.to_int (to_float x)

let[@inline always] of_string x = of_float (Float.of_string x)

let[@inline always] to_string x = Float.to_string (to_float x)

type fpclass = Stdlib.fpclass =
    FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

let[@inline always] classify_float x = Float.classify_float (to_float x)

let[@inline always] pow x y = of_float (Float.pow (to_float x) (to_float y))

let[@inline always] sqrt x = of_float (Float.sqrt (to_float x))

let[@inline always] cbrt x = of_float (Float.cbrt (to_float x))

let[@inline always] exp x = of_float (Float.exp (to_float x))

let[@inline always] exp2 x = of_float (Float.exp2 (to_float x))

let[@inline always] log x = of_float (Float.log (to_float x))

let[@inline always] log10 x = of_float (Float.log10 (to_float x))

let[@inline always] log2 x = of_float (Float.log2 (to_float x))

let[@inline always] expm1 x = of_float (Float.expm1 (to_float x))

let[@inline always] log1p x = of_float (Float.log1p (to_float x))

let[@inline always] cos x = of_float (Float.cos (to_float x))

let[@inline always] sin x = of_float (Float.sin (to_float x))

let[@inline always] tan x = of_float (Float.tan (to_float x))

let[@inline always] acos x = of_float (Float.acos (to_float x))

let[@inline always] asin x = of_float (Float.asin (to_float x))

let[@inline always] atan x = of_float (Float.atan (to_float x))

let[@inline always] atan2 x y = of_float (Float.atan2 (to_float x) (to_float y))

let[@inline always] hypot x y = of_float (Float.hypot (to_float x) (to_float y))

let[@inline always] cosh x = of_float (Float.cosh (to_float x))

let[@inline always] sinh x = of_float (Float.sinh (to_float x))

let[@inline always] tanh x = of_float (Float.tanh (to_float x))

let[@inline always] acosh x = of_float (Float.acosh (to_float x))

let[@inline always] asinh x = of_float (Float.asinh (to_float x))

let[@inline always] atanh x = of_float (Float.atanh (to_float x))

let[@inline always] erf x = of_float (Float.erf (to_float x))

let[@inline always] erfc x = of_float (Float.erfc (to_float x))

let[@inline always] trunc x = of_float (Float.trunc (to_float x))

let[@inline always] round x = of_float (Float.round (to_float x))

let[@inline always] ceil x = of_float (Float.ceil (to_float x))

let[@inline always] floor x = of_float (Float.floor (to_float x))

let[@inline always] next_after x y = of_float (Float.next_after (to_float x) (to_float y))

let[@inline always] copy_sign x y = of_float (Float.copy_sign (to_float x) (to_float y))

let[@inline always] sign_bit x = Float.sign_bit (to_float x)

let[@inline always] ldexp x i = of_float (Float.ldexp (to_float x) i)

let[@inline always] compare x y = Float.compare (to_float x) (to_float y)

let[@inline always] equal x y = Float.equal (to_float x) (to_float y)

let[@inline always] min x y = of_float (Float.min (to_float x) (to_float y))

let[@inline always] max x y = of_float (Float.max (to_float x) (to_float y))

let[@inline always] min_num x y = of_float (Float.min_num (to_float x) (to_float y))

let[@inline always] max_num x y = of_float (Float.max_num (to_float x) (to_float y))
