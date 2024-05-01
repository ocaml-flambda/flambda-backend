(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.flambda_o3]

type t = float32

external float32_of_bits : int32 -> float32
  = "caml_float32_of_bits_bytecode" "caml_float32_of_bits"
  [@@unboxed] [@@noalloc]

external neg : (float32[@local_opt]) -> (float32[@local_opt]) = "%negfloat32"

external add :
  (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
  = "%addfloat32"

external sub :
  (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
  = "%subfloat32"

external mul :
  (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
  = "%mulfloat32"

external div :
  (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
  = "%divfloat32"

external pow : float32 -> float32 -> float32
  = "caml_power_float32_bytecode" "powf"
  [@@unboxed] [@@noalloc]

module Operators = struct
  external ( ~-. ) : (float32[@local_opt]) -> (float32[@local_opt])
    = "%negfloat32"

  external ( +. ) :
    (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
    = "%addfloat32"

  external ( -. ) :
    (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
    = "%subfloat32"

  external ( *. ) :
    (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
    = "%mulfloat32"

  external ( /. ) :
    (float32[@local_opt]) -> (float32[@local_opt]) -> (float32[@local_opt])
    = "%divfloat32"

  external ( ** ) : float32 -> float32 -> float32
    = "caml_power_float32_bytecode" "powf"
    [@@unboxed] [@@noalloc]
end

external fma : float32 -> float32 -> float32 -> float32
  = "caml_fma_float32_bytecode" "fmaf"
  [@@unboxed] [@@noalloc]

external rem : float32 -> float32 -> float32
  = "caml_fmod_float32_bytecode" "fmodf"
  [@@unboxed] [@@noalloc]

external abs : (float32[@local_opt]) -> (float32[@local_opt]) = "%absfloat32"

let zero = 0.s
let one = 1.s
let minus_one = -1.s
let infinity = float32_of_bits 0x7f800000l
let neg_infinity = float32_of_bits 0xff800000l
let nan = float32_of_bits 0x7f800001l
let is_finite (x : float32) = sub x x = 0.s
let is_infinite (x : float32) = div 1.s x = 0.s
let is_nan (x : float32) = x <> x
let pi = 0x1.921fb6p+1s
let max_float = float32_of_bits 0x7f7fffffl
let min_float = float32_of_bits 0x00800000l
let epsilon = float32_of_bits 0x34000000l

external of_int : int -> float32 = "%float32ofint"
external to_int : (float32[@local_opt]) -> int = "%intoffloat32"
external of_float : (float[@local_opt]) -> float32 = "%float32offloat"
external to_float : (float32[@local_opt]) -> float = "%floatoffloat32"

external of_bits : (int32[@local_opt]) -> float32
  = "caml_float32_of_bits_bytecode" "caml_float32_of_bits"
  [@@unboxed] [@@noalloc]

external to_bits : (float32[@local_opt]) -> int32
  = "caml_float32_to_bits_bytecode" "caml_float32_to_bits"
  [@@unboxed] [@@noalloc]

external of_string : string -> float32 = "caml_float32_of_string"

let of_string_opt s = try Some (of_string s) with Failure _ -> None

external format : string -> float32 -> string = "caml_format_float32"

let to_string f = Stdlib.valid_float_lexem (format "%.9g" f)

type fpclass = Stdlib.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

external classify_float : (float32[@unboxed]) -> fpclass
  = "caml_classify_float32_bytecode" "caml_classify_float32"
  [@@noalloc]

external sqrt : float32 -> float32 = "caml_sqrt_float32_bytecode" "sqrtf"
  [@@unboxed] [@@noalloc]

external cbrt : float32 -> float32 = "caml_cbrt_float32_bytecode" "cbrtf"
  [@@unboxed] [@@noalloc]

external exp : float32 -> float32 = "caml_exp_float32_bytecode" "expf"
  [@@unboxed] [@@noalloc]

external exp2 : float32 -> float32 = "caml_exp2_float32_bytecode" "exp2f"
  [@@unboxed] [@@noalloc]

external log : float32 -> float32 = "caml_log_float32_bytecode" "logf"
  [@@unboxed] [@@noalloc]

external log10 : float32 -> float32 = "caml_log10_float32_bytecode" "log10f"
  [@@unboxed] [@@noalloc]

external log2 : float32 -> float32 = "caml_log2_float32_bytecode" "log2f"
  [@@unboxed] [@@noalloc]

external expm1 : float32 -> float32 = "caml_expm1_float32_bytecode" "expm1f"
  [@@unboxed] [@@noalloc]

external log1p : float32 -> float32 = "caml_log1p_float32_bytecode" "log1pf"
  [@@unboxed] [@@noalloc]

external cos : float32 -> float32 = "caml_cos_float32_bytecode" "cosf"
  [@@unboxed] [@@noalloc]

external sin : float32 -> float32 = "caml_sin_float32_bytecode" "sinf"
  [@@unboxed] [@@noalloc]

external tan : float32 -> float32 = "caml_tan_float32_bytecode" "tanf"
  [@@unboxed] [@@noalloc]

external acos : float32 -> float32 = "caml_acos_float32_bytecode" "acosf"
  [@@unboxed] [@@noalloc]

external asin : float32 -> float32 = "caml_asin_float32_bytecode" "asinf"
  [@@unboxed] [@@noalloc]

external atan : float32 -> float32 = "caml_atan_float32_bytecode" "atanf"
  [@@unboxed] [@@noalloc]

external atan2 : float32 -> float32 -> float32
  = "caml_atan2_float32_bytecode" "atan2f"
  [@@unboxed] [@@noalloc]

external hypot : float32 -> float32 -> float32
  = "caml_hypot_float32_bytecode" "hypotf"
  [@@unboxed] [@@noalloc]

external cosh : float32 -> float32 = "caml_cosh_float32_bytecode" "coshf"
  [@@unboxed] [@@noalloc]

external sinh : float32 -> float32 = "caml_sinh_float32_bytecode" "sinhf"
  [@@unboxed] [@@noalloc]

external tanh : float32 -> float32 = "caml_tanh_float32_bytecode" "tanhf"
  [@@unboxed] [@@noalloc]

external acosh : float32 -> float32 = "caml_acosh_float32_bytecode" "acoshf"
  [@@unboxed] [@@noalloc]

external asinh : float32 -> float32 = "caml_asinh_float32_bytecode" "asinhf"
  [@@unboxed] [@@noalloc]

external atanh : float32 -> float32 = "caml_atanh_float32_bytecode" "atanhf"
  [@@unboxed] [@@noalloc]

external erf : float32 -> float32 = "caml_erf_float32_bytecode" "erff"
  [@@unboxed] [@@noalloc]

external erfc : float32 -> float32 = "caml_erfc_float32_bytecode" "erfcf"
  [@@unboxed] [@@noalloc]

external trunc : float32 -> float32 = "caml_trunc_float32_bytecode" "truncf"
  [@@unboxed] [@@noalloc]

external round : float32 -> float32 = "caml_round_float32_bytecode" "roundf"
  [@@unboxed] [@@noalloc]

external ceil : float32 -> float32 = "caml_ceil_float32_bytecode" "ceilf"
  [@@unboxed] [@@noalloc]

external floor : float32 -> float32 = "caml_floor_float32_bytecode" "floorf"
  [@@unboxed] [@@noalloc]

let is_integer x = x = trunc x && is_finite x

external next_after : float32 -> float32 -> float32
  = "caml_nextafter_float32_bytecode" "nextafterf"
  [@@unboxed] [@@noalloc]

let succ x = next_after x infinity
let pred x = next_after x neg_infinity

external copy_sign : float32 -> float32 -> float32
  = "caml_copysign_float32_bytecode" "copysignf"
  [@@unboxed] [@@noalloc]

external sign_bit : (float32[@unboxed]) -> bool
  = "caml_signbit_float32_bytecode" "caml_signbit_float32"
  [@@noalloc]

external frexp : float32 -> float32 * int = "caml_frexp_float32"

external ldexp : (float32[@unboxed]) -> (int[@untagged]) -> (float32[@unboxed])
  = "caml_ldexp_float32_bytecode" "caml_ldexp_float32"
  [@@noalloc]

external modf : float32 -> float32 * float32 = "caml_modf_float32"
external compare : float32 -> float32 -> int = "%compare"

let equal x y = compare x y = 0

let[@inline] min (x : float32) (y : float32) =
  if y > x || ((not (sign_bit y)) && sign_bit x) then if is_nan y then y else x
  else if is_nan x then x
  else y

let[@inline] max (x : float32) (y : float32) =
  if y > x || ((not (sign_bit y)) && sign_bit x) then if is_nan x then x else y
  else if is_nan y then y
  else x

let[@inline] min_max (x : float32) (y : float32) =
  if is_nan x || is_nan y then (nan, nan)
  else if y > x || ((not (sign_bit y)) && sign_bit x) then (x, y)
  else (y, x)

let[@inline] min_num (x : float32) (y : float32) =
  if y > x || ((not (sign_bit y)) && sign_bit x) then if is_nan x then y else x
  else if is_nan y then x
  else y

let[@inline] max_num (x : float32) (y : float32) =
  if y > x || ((not (sign_bit y)) && sign_bit x) then if is_nan y then x else y
  else if is_nan x then y
  else x

let[@inline] min_max_num (x : float32) (y : float32) =
  if is_nan x then (y, y)
  else if is_nan y then (x, x)
  else if y > x || ((not (sign_bit y)) && sign_bit x) then (x, y)
  else (y, x)

external seeded_hash_param : int -> int -> int -> 'a -> int = "caml_hash_exn"
  [@@noalloc]

let seeded_hash seed x = seeded_hash_param 10 100 seed x
let hash x = seeded_hash_param 10 100 0 x
