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

external float32_of_bits : int32 -> t
  = "caml_float32_of_bits_bytecode" "caml_float32_of_bits"
  [@@unboxed] [@@noalloc]

external neg : (t[@local_opt]) -> (t[@local_opt]) = "%negfloat32"

external add : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
  = "%addfloat32"

external sub : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
  = "%subfloat32"

external mul : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
  = "%mulfloat32"

external div : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
  = "%divfloat32"

external pow : t -> t -> t = "caml_power_float32_bytecode" "powf"
  [@@unboxed] [@@noalloc]

module Operators = struct
  external ( ~-. ) : (t[@local_opt]) -> (t[@local_opt]) = "%negfloat32"

  external ( +. ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
    = "%addfloat32"

  external ( -. ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
    = "%subfloat32"

  external ( *. ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
    = "%mulfloat32"

  external ( /. ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
    = "%divfloat32"

  external ( ** ) : t -> t -> t = "caml_power_float32_bytecode" "powf"
    [@@unboxed] [@@noalloc]
end

external fma : t -> t -> t -> t = "caml_fma_float32_bytecode" "fmaf"
  [@@unboxed] [@@noalloc]

external rem : t -> t -> t = "caml_fmod_float32_bytecode" "fmodf"
  [@@unboxed] [@@noalloc]

external abs : (t[@local_opt]) -> (t[@local_opt]) = "%absfloat32"

let zero = 0.s
let one = 1.s
let minus_one = -1.s
let infinity = float32_of_bits 0x7f800000l
let neg_infinity = float32_of_bits 0xff800000l
let quiet_nan = float32_of_bits 0x7fc00001l
let signaling_nan = float32_of_bits 0x7f800001l
let nan = quiet_nan
let is_finite (x : t) = sub x x = 0.s
let is_infinite (x : t) = div 1.s x = 0.s
let is_nan (x : t) = x <> x
let pi = 0x1.921fb6p+1s
let max_float = float32_of_bits 0x7f7fffffl
let min_float = float32_of_bits 0x00800000l
let epsilon = float32_of_bits 0x34000000l

external of_int : int -> t = "%float32ofint"
external to_int : (t[@local_opt]) -> int = "%intoffloat32"
external of_float : (float[@local_opt]) -> t = "%float32offloat"
external to_float : (t[@local_opt]) -> float = "%floatoffloat32"

external of_int64 : (int64[@local_opt]) -> t
  = "caml_float32_of_int64_bytecode" "caml_float32_of_int64"
  [@@unboxed] [@@noalloc] [@@builtin]

external to_int64 : (t[@local_opt]) -> int64
  = "caml_float32_to_int64_bytecode" "caml_float32_to_int64"
  [@@unboxed] [@@noalloc] [@@builtin]

external of_bits : (int32[@local_opt]) -> t
  = "caml_float32_of_bits_bytecode" "caml_float32_of_bits"
  [@@unboxed] [@@noalloc] [@@builtin]

external to_bits : (t[@local_opt]) -> int32
  = "caml_float32_to_bits_bytecode" "caml_float32_to_bits"
  [@@unboxed] [@@noalloc] [@@builtin]

external of_string : string -> t = "caml_float32_of_string"

let of_string_opt s = try Some (of_string s) with Failure _ -> None

external format : string -> t -> string = "caml_format_float32"

let to_string f = Stdlib.valid_float_lexem (format "%.9g" f)

type fpclass = Stdlib.fpclass =
  | FP_normal
  | FP_subnormal
  | FP_zero
  | FP_infinite
  | FP_nan

external classify_float : (t[@unboxed]) -> fpclass
  = "caml_classify_float32_bytecode" "caml_classify_float32"
  [@@noalloc]

external sqrt : t -> t = "caml_sqrt_float32_bytecode" "sqrtf"
  [@@unboxed] [@@noalloc] [@@builtin]

external cbrt : t -> t = "caml_cbrt_float32_bytecode" "cbrtf"
  [@@unboxed] [@@noalloc]

external exp : t -> t = "caml_exp_float32_bytecode" "expf"
  [@@unboxed] [@@noalloc]

external exp2 : t -> t = "caml_exp2_float32_bytecode" "exp2f"
  [@@unboxed] [@@noalloc]

external log : t -> t = "caml_log_float32_bytecode" "logf"
  [@@unboxed] [@@noalloc]

external log10 : t -> t = "caml_log10_float32_bytecode" "log10f"
  [@@unboxed] [@@noalloc]

external log2 : t -> t = "caml_log2_float32_bytecode" "log2f"
  [@@unboxed] [@@noalloc]

external expm1 : t -> t = "caml_expm1_float32_bytecode" "expm1f"
  [@@unboxed] [@@noalloc]

external log1p : t -> t = "caml_log1p_float32_bytecode" "log1pf"
  [@@unboxed] [@@noalloc]

external cos : t -> t = "caml_cos_float32_bytecode" "cosf"
  [@@unboxed] [@@noalloc]

external sin : t -> t = "caml_sin_float32_bytecode" "sinf"
  [@@unboxed] [@@noalloc]

external tan : t -> t = "caml_tan_float32_bytecode" "tanf"
  [@@unboxed] [@@noalloc]

external acos : t -> t = "caml_acos_float32_bytecode" "acosf"
  [@@unboxed] [@@noalloc]

external asin : t -> t = "caml_asin_float32_bytecode" "asinf"
  [@@unboxed] [@@noalloc]

external atan : t -> t = "caml_atan_float32_bytecode" "atanf"
  [@@unboxed] [@@noalloc]

external atan2 : t -> t -> t = "caml_atan2_float32_bytecode" "atan2f"
  [@@unboxed] [@@noalloc]

external hypot : t -> t -> t = "caml_hypot_float32_bytecode" "hypotf"
  [@@unboxed] [@@noalloc]

external cosh : t -> t = "caml_cosh_float32_bytecode" "coshf"
  [@@unboxed] [@@noalloc]

external sinh : t -> t = "caml_sinh_float32_bytecode" "sinhf"
  [@@unboxed] [@@noalloc]

external tanh : t -> t = "caml_tanh_float32_bytecode" "tanhf"
  [@@unboxed] [@@noalloc]

external acosh : t -> t = "caml_acosh_float32_bytecode" "acoshf"
  [@@unboxed] [@@noalloc]

external asinh : t -> t = "caml_asinh_float32_bytecode" "asinhf"
  [@@unboxed] [@@noalloc]

external atanh : t -> t = "caml_atanh_float32_bytecode" "atanhf"
  [@@unboxed] [@@noalloc]

external erf : t -> t = "caml_erf_float32_bytecode" "erff"
  [@@unboxed] [@@noalloc]

external erfc : t -> t = "caml_erfc_float32_bytecode" "erfcf"
  [@@unboxed] [@@noalloc]

external trunc : t -> t = "caml_trunc_float32_bytecode" "truncf"
  [@@unboxed] [@@noalloc]

external round : t -> t = "caml_round_float32_bytecode" "roundf"
  [@@unboxed] [@@noalloc]

external ceil : t -> t = "caml_ceil_float32_bytecode" "ceilf"
  [@@unboxed] [@@noalloc]

external floor : t -> t = "caml_floor_float32_bytecode" "floorf"
  [@@unboxed] [@@noalloc]

let is_integer x = x = trunc x && is_finite x

external next_after : t -> t -> t
  = "caml_nextafter_float32_bytecode" "nextafterf"
  [@@unboxed] [@@noalloc]

let succ x = next_after x infinity
let pred x = next_after x neg_infinity

external copy_sign : t -> t -> t = "caml_copysign_float32_bytecode" "copysignf"
  [@@unboxed] [@@noalloc]

external sign_bit : (t[@unboxed]) -> bool
  = "caml_signbit_float32_bytecode" "caml_signbit_float32"
  [@@noalloc]

external frexp : t -> t * int = "caml_frexp_float32"

external ldexp : (t[@unboxed]) -> (int[@untagged]) -> (t[@unboxed])
  = "caml_ldexp_float32_bytecode" "caml_ldexp_float32"
  [@@noalloc]

external modf : t -> t * t = "caml_modf_float32"
external compare : t -> t -> int = "%compare"

let equal x y = compare x y = 0

let[@inline] min (x : t) (y : t) =
  if y > x || ((not (sign_bit y)) && sign_bit x) then if is_nan y then y else x
  else if is_nan x then x
  else y

let[@inline] max (x : t) (y : t) =
  if y > x || ((not (sign_bit y)) && sign_bit x) then if is_nan x then x else y
  else if is_nan y then y
  else x

let[@inline] min_max (x : t) (y : t) =
  if is_nan x || is_nan y then (nan, nan)
  else if y > x || ((not (sign_bit y)) && sign_bit x) then (x, y)
  else (y, x)

let[@inline] min_num (x : t) (y : t) =
  if y > x || ((not (sign_bit y)) && sign_bit x) then if is_nan x then y else x
  else if is_nan y then x
  else y

let[@inline] max_num (x : t) (y : t) =
  if y > x || ((not (sign_bit y)) && sign_bit x) then if is_nan y then x else y
  else if is_nan x then y
  else x

let[@inline] min_max_num (x : t) (y : t) =
  if is_nan x then (y, y)
  else if is_nan y then (x, x)
  else if y > x || ((not (sign_bit y)) && sign_bit x) then (x, y)
  else (y, x)

external seeded_hash_param : int -> int -> int -> 'a -> int = "caml_hash_exn"
  [@@noalloc]

let seeded_hash seed x = seeded_hash_param 10 100 seed x
let hash x = seeded_hash_param 10 100 0 x
