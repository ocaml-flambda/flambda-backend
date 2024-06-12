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

(** Floating-point arithmetic.

    OCaml's floating-point numbers follow the
    IEEE 754 standard, using single precision (32 bits) numbers.
    Floating-point operations never raise an exception on overflow,
    underflow, division by zero, etc.  Instead, special IEEE numbers
    are returned as appropriate, such as [infinity] for [1.0s /. 0.0s],
    [neg_infinity] for [-1.0s /. 0.0s], and [nan] ('not a number')
    for [0.0s /. 0.0s].  These special numbers then propagate through
    floating-point computations as expected: for instance,
    [1.0s /. infinity] is [0.0s], basic arithmetic operations
    ([+.], [-.], [*.], [/.]) with [nan] as an argument return [nan], ...
*)

type t = float32
(** An alias for the type of 32-bit floating-point numbers. *)

val zero : t
(** The floating point 0.s *)

val one : t
(** The floating-point 1.s *)

val minus_one : t
(** The floating-point -1.s *)

external neg : (t[@local_opt]) -> (t[@local_opt]) = "%negfloat32"
(** Unary negation. *)

external add : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
  = "%addfloat32"
(** Floating-point addition. *)

external sub : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
  = "%subfloat32"
(** Floating-point subtraction. *)

external mul : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
  = "%mulfloat32"
(** Floating-point multiplication. *)

external div : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
  = "%divfloat32"
(** Floating-point division. *)

external pow : t -> t -> t = "caml_power_float32_bytecode" "powf"
  [@@unboxed] [@@noalloc]
(** Exponentiation. *)

(** Floating-point arithmetic operator overloads. *)
module Operators : sig
  external ( ~-. ) : (t[@local_opt]) -> (t[@local_opt]) = "%negfloat32"
  (** Unary negation. *)

  external ( +. ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
    = "%addfloat32"
  (** Floating-point addition. *)

  external ( -. ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
    = "%subfloat32"
  (** Floating-point subtraction. *)

  external ( *. ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
    = "%mulfloat32"
  (** Floating-point multiplication. *)

  external ( /. ) : (t[@local_opt]) -> (t[@local_opt]) -> (t[@local_opt])
    = "%divfloat32"
  (** Floating-point division. *)

  external ( ** ) : t -> t -> t = "caml_power_float32_bytecode" "powf"
    [@@unboxed] [@@noalloc]
  (** Exponentiation. *)
end

external fma : t -> t -> t -> t = "caml_fma_float32_bytecode" "fmaf"
  [@@unboxed] [@@noalloc]
(** [fma x y z] returns [x * y + z], with a best effort for computing
   this expression with a single rounding, using either hardware
   instructions (providing full IEEE compliance) or a software
   emulation. *)

external rem : t -> t -> t = "caml_fmod_float32_bytecode" "fmodf"
  [@@unboxed] [@@noalloc]
(** [rem a b] returns the remainder of [a] with respect to [b].  The returned
    value is [a -. n *. b], where [n] is the quotient [a /. b] rounded towards
    zero to an integer. *)

val succ : t -> t
(** [succ x] returns the floating point number right after [x] i.e.,
   the smallest floating-point number greater than [x].  See also
   {!next_after}. *)

val pred : t -> t
(** [pred x] returns the floating-point number right before [x] i.e.,
   the greatest floating-point number smaller than [x].  See also
   {!next_after}. *)

external abs : (t[@local_opt]) -> (t[@local_opt]) = "%absfloat32"
(** [abs f] returns the absolute value of [f]. *)

val infinity : t
(** Positive infinity. *)

val neg_infinity : t
(** Negative infinity. *)

val nan : t
(** A special floating-point value denoting the result of an
    undefined operation such as [0.0s /. 0.0s].  Stands for
    'not a number'.  Any floating-point operation with [nan] as
    argument returns [nan] as result, unless otherwise specified in
    IEEE 754 standard.  As for floating-point comparisons,
    [=], [<], [<=], [>] and [>=] return [false] and [<>] returns [true]
    if one or both of their arguments is [nan].

    Equivalent to [quiet_nan]. *)

val signaling_nan : t
(** Signaling NaN. The corresponding signals do not raise OCaml exception,
    but the value can be useful for interoperability with C libraries. *)

val quiet_nan : t
(** Quiet NaN. *)

val pi : t
(** The constant pi. *)

val max_float : t
(** The largest positive finite value of type [float32]. *)

val min_float : t
(** The smallest positive, non-zero, non-denormalized value of type [float32]. *)

val epsilon : t
(** The difference between [1.0s] and the smallest exactly representable
    floating-point number greater than [1.0s]. *)

val is_finite : t -> bool
(** [is_finite x] is [true] if and only if [x] is finite i.e., not infinite and
   not {!nan}. *)

val is_infinite : t -> bool
(** [is_infinite x] is [true] if and only if [x] is {!infinity} or
    {!neg_infinity}. *)

val is_nan : t -> bool
(** [is_nan x] is [true] if and only if [x] is not a number (see {!nan}). *)

val is_integer : t -> bool
(** [is_integer x] is [true] if and only if [x] is an integer. *)

external of_float : (float[@local_opt]) -> t = "%float32offloat"
(** Convert a 64-bit float to the nearest representable 32-bit float. *)

external to_float : (t[@local_opt]) -> float = "%floatoffloat32"
(** Convert a 32-bit float to a 64-bit float. *)

external of_int : int -> t = "%float32ofint"
(** Convert an integer to floating-point. *)

external to_int : (t[@local_opt]) -> int = "%intoffloat32"
(** Truncate the given floating-point number to an integer.
    The result is unspecified if the argument is [nan] or falls outside the
    range of representable integers. *)

external of_int64 : (int64[@local_opt]) -> t
  = "caml_float32_of_int64_bytecode" "caml_float32_of_int64"
  [@@unboxed] [@@noalloc] [@@builtin]
(** Convert the given 64-bit integer to the nearest representable 32-bit float.
    The amd64 flambda-backend compiler translates this call to CVTSI2SS. *)

external to_int64 : (t[@local_opt]) -> int64
  = "caml_float32_to_int64_bytecode" "caml_float32_to_int64"
  [@@unboxed] [@@noalloc] [@@builtin]
(** Convert the given 32-bit float to a 64-bit integer,
    discarding the fractional part (truncate towards 0).
    If the truncated floating-point number is outside the range
    \[{!Int64.min_int}, {!Int64.max_int}\], no exception is raised, and
    an unspecified, platform-dependent integer is returned.
    The amd64 flambda-backend compiler translates this call to CVTTSS2SI. *)

external of_bits : (int32[@local_opt]) -> t
  = "caml_float32_of_bits_bytecode" "caml_float32_of_bits"
  [@@unboxed] [@@noalloc] [@@builtin]
(** Convert a 32-bit float to a 32-bit integer, preserving the value's
    bit pattern.
    The amd64 flambda-backend compiler translates this call to MOVD. *)

external to_bits : (t[@local_opt]) -> int32
  = "caml_float32_to_bits_bytecode" "caml_float32_to_bits"
  [@@unboxed] [@@noalloc] [@@builtin]
(** Convert a 32-bit integer to a 32-bit float, preserving the value's
    bit pattern.
    The amd64 flambda-backend compiler translates this call to MOVD. *)

external of_string : string -> t = "caml_float32_of_string"
(** Convert the given string to a float.  The string is read in decimal
    (by default) or in hexadecimal (marked by [0x] or [0X]).
    The format of decimal floating-point numbers is
    [ [-] dd.ddd (e|E) [+|-] dd ], where [d] stands for a decimal digit.
    The format of hexadecimal floating-point numbers is
    [ [-] 0(x|X) hh.hhh (p|P) [+|-] dd ], where [h] stands for an
    hexadecimal digit and [d] for a decimal digit.
    In both cases, at least one of the integer and fractional parts must be
    given; the exponent part is optional.
    The [_] (underscore) character can appear anywhere in the string
    and is ignored.
    Depending on the execution platforms, other representations of
    floating-point numbers can be accepted, but should not be relied upon.
    @raise Failure if the given string is not a valid
    representation of a float32. *)

val of_string_opt : string -> t option
(** Same as [of_string], but returns [None] instead of raising. *)

val to_string : t -> string
(** Return a string representation of a floating-point number.

    This conversion does not involve a loss of precision. *)

(** The five classes of floating-point numbers, as determined by
    the {!classify_float} function. *)
type fpclass = Stdlib.fpclass =
  | FP_normal  (** Normal number, none of the below *)
  | FP_subnormal  (** Number very close to 0.0s, has reduced precision *)
  | FP_zero  (** Number is 0.0s or -0.0s *)
  | FP_infinite  (** Number is positive or negative infinity *)
  | FP_nan  (** Not a number: result of an undefined operation *)

external classify_float : (t[@unboxed]) -> fpclass
  = "caml_classify_float32_bytecode" "caml_classify_float32"
  [@@noalloc]
(** Return the class of the given floating-point number:
    normal, subnormal, zero, infinite, or not a number. *)

external sqrt : t -> t = "caml_sqrt_float32_bytecode" "sqrtf"
  [@@unboxed] [@@noalloc] [@@builtin]
(** Square root.
    The amd64 flambda-backend compiler translates this call to SQRTSS. *)

external cbrt : t -> t = "caml_cbrt_float32_bytecode" "cbrtf"
  [@@unboxed] [@@noalloc]
(** Cube root. *)

external exp : t -> t = "caml_exp_float32_bytecode" "expf"
  [@@unboxed] [@@noalloc]
(** Exponential. *)

external exp2 : t -> t = "caml_exp2_float32_bytecode" "exp2f"
  [@@unboxed] [@@noalloc]
(** Base 2 exponential function. *)

external log : t -> t = "caml_log_float32_bytecode" "logf"
  [@@unboxed] [@@noalloc]
(** Natural logarithm. *)

external log10 : t -> t = "caml_log10_float32_bytecode" "log10f"
  [@@unboxed] [@@noalloc]
(** Base 10 logarithm. *)

external log2 : t -> t = "caml_log2_float32_bytecode" "log2f"
  [@@unboxed] [@@noalloc]
(** Base 2 logarithm. *)

external expm1 : t -> t = "caml_expm1_float32_bytecode" "expm1f"
  [@@unboxed] [@@noalloc]
(** [expm1 x] computes [exp x -. 1.0], giving numerically-accurate results
    even if [x] is close to [0.0]. *)

external log1p : t -> t = "caml_log1p_float32_bytecode" "log1pf"
  [@@unboxed] [@@noalloc]
(** [log1p x] computes [log(1.0 +. x)] (natural logarithm),
    giving numerically-accurate results even if [x] is close to [0.0]. *)

external cos : t -> t = "caml_cos_float32_bytecode" "cosf"
  [@@unboxed] [@@noalloc]
(** Cosine.  Argument is in radians. *)

external sin : t -> t = "caml_sin_float32_bytecode" "sinf"
  [@@unboxed] [@@noalloc]
(** Sine.  Argument is in radians. *)

external tan : t -> t = "caml_tan_float32_bytecode" "tanf"
  [@@unboxed] [@@noalloc]
(** Tangent.  Argument is in radians. *)

external acos : t -> t = "caml_acos_float32_bytecode" "acosf"
  [@@unboxed] [@@noalloc]
(** Arc cosine.  The argument must fall within the range [[-1.0, 1.0]].
    Result is in radians and is between [0.0] and [pi]. *)

external asin : t -> t = "caml_asin_float32_bytecode" "asinf"
  [@@unboxed] [@@noalloc]
(** Arc sine.  The argument must fall within the range [[-1.0, 1.0]].
    Result is in radians and is between [-pi/2] and [pi/2]. *)

external atan : t -> t = "caml_atan_float32_bytecode" "atanf"
  [@@unboxed] [@@noalloc]
(** Arc tangent.
    Result is in radians and is between [-pi/2] and [pi/2]. *)

external atan2 : t -> t -> t = "caml_atan2_float32_bytecode" "atan2f"
  [@@unboxed] [@@noalloc]
(** [atan2 y x] returns the arc tangent of [y /. x].  The signs of [x]
    and [y] are used to determine the quadrant of the result.
    Result is in radians and is between [-pi] and [pi]. *)

external hypot : t -> t -> t = "caml_hypot_float32_bytecode" "hypotf"
  [@@unboxed] [@@noalloc]
(** [hypot x y] returns [sqrt(x *. x +. y *. y)], that is, the length
    of the hypotenuse of a right-angled triangle with sides of length
    [x] and [y], or, equivalently, the distance of the point [(x,y)]
    to origin.  If one of [x] or [y] is infinite, returns [infinity]
    even if the other is [nan]. *)

external cosh : t -> t = "caml_cosh_float32_bytecode" "coshf"
  [@@unboxed] [@@noalloc]
(** Hyperbolic cosine.  Argument is in radians. *)

external sinh : t -> t = "caml_sinh_float32_bytecode" "sinhf"
  [@@unboxed] [@@noalloc]
(** Hyperbolic sine.  Argument is in radians. *)

external tanh : t -> t = "caml_tanh_float32_bytecode" "tanhf"
  [@@unboxed] [@@noalloc]
(** Hyperbolic tangent.  Argument is in radians. *)

external acosh : t -> t = "caml_acosh_float32_bytecode" "acoshf"
  [@@unboxed] [@@noalloc]
(** Hyperbolic arc cosine.  The argument must fall within the range
    [[1.0, inf]].
    Result is in radians and is between [0.0] and [inf]. *)

external asinh : t -> t = "caml_asinh_float32_bytecode" "asinhf"
  [@@unboxed] [@@noalloc]
(** Hyperbolic arc sine.  The argument and result range over the entire
    real line.
    Result is in radians. *)

external atanh : t -> t = "caml_atanh_float32_bytecode" "atanhf"
  [@@unboxed] [@@noalloc]
(** Hyperbolic arc tangent.  The argument must fall within the range
    [[-1.0, 1.0]].
    Result is in radians and ranges over the entire real line. *)

external erf : t -> t = "caml_erf_float32_bytecode" "erff"
  [@@unboxed] [@@noalloc]
(** Error function.  The argument ranges over the entire real line.
    The result is always within [[-1.0, 1.0]]. *)

external erfc : t -> t = "caml_erfc_float32_bytecode" "erfcf"
  [@@unboxed] [@@noalloc]
(** Complementary error function ([erfc x = 1 - erf x]).
    The argument ranges over the entire real line.
    The result is always within [[-1.0, 1.0]]. *)

external trunc : t -> t = "caml_trunc_float32_bytecode" "truncf"
  [@@unboxed] [@@noalloc]
(** [trunc x] rounds [x] to the nearest integer whose absolute value is
    less than or equal to [x]. *)

external round : t -> t = "caml_round_float32_bytecode" "roundf"
  [@@unboxed] [@@noalloc]
(** [round x] rounds [x] to the nearest integer with ties (fractional
    values of 0.5s) rounded away from zero, regardless of the current
    rounding direction.  If [x] is an integer, [+0.s], [-0.s], [nan], or
    infinite, [x] itself is returned. *)

external ceil : t -> t = "caml_ceil_float32_bytecode" "ceilf"
  [@@unboxed] [@@noalloc]
(** Round above to an integer value.
    [ceil f] returns the least integer value greater than or equal to [f].
    The result is returned as a float32. *)

external floor : t -> t = "caml_floor_float32_bytecode" "floorf"
  [@@unboxed] [@@noalloc]
(** Round below to an integer value.
    [floor f] returns the greatest integer value less than or
    equal to [f].
    The result is returned as a float32. *)

external next_after : t -> t -> t
  = "caml_nextafter_float32_bytecode" "nextafterf"
  [@@unboxed] [@@noalloc]
(** [next_after x y] returns the next representable floating-point
   value following [x] in the direction of [y].  More precisely, if
   [y] is greater (resp. less) than [x], it returns the smallest
   (resp. largest) representable number greater (resp. less) than [x].
   If [x] equals [y], the function returns [y].  If [x] or [y] is
   [nan], a [nan] is returned.
   Note that [next_after max_float infinity = infinity] and that
   [next_after 0.s infinity] is the smallest denormalized positive number.
   If [x] is the smallest denormalized positive number,
   [next_after x 0.s = 0.s] *)

external copy_sign : t -> t -> t = "caml_copysign_float32_bytecode" "copysignf"
  [@@unboxed] [@@noalloc]
(** [copy_sign x y] returns a float whose absolute value is that of [x]
    and whose sign is that of [y].  If [x] is [nan], returns [nan].
    If [y] is [nan], returns either [x] or [-. x], but it is not
    specified which. *)

external sign_bit : (t[@unboxed]) -> bool
  = "caml_signbit_float32_bytecode" "caml_signbit_float32"
  [@@noalloc]
(** [sign_bit x] is [true] if and only if the sign bit of [x] is set.
    For example [sign_bit 1.] and [signbit 0.] are [false] while
    [sign_bit (-1.)] and [sign_bit (-0.)] are [true]. *)

external frexp : t -> t * int = "caml_frexp_float32"
(** [frexp f] returns the pair of the significant
    and the exponent of [f].  When [f] is zero, the
    significant [x] and the exponent [n] of [f] are equal to
    zero.  When [f] is non-zero, they are defined by
    [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)

external ldexp : (t[@unboxed]) -> (int[@untagged]) -> (t[@unboxed])
  = "caml_ldexp_float32_bytecode" "caml_ldexp_float32"
  [@@noalloc]
(** [ldexp x n] returns [x *. 2 ** n]. *)

external modf : t -> t * t = "caml_modf_float32"
(** [modf f] returns the pair of the fractional and integral
    part of [f]. *)

external compare : t -> t -> int = "%compare"
(** [compare x y] returns [0] if [x] is equal to [y], a negative integer if [x]
    is less than [y], and a positive integer if [x] is greater than
    [y]. [compare] treats [nan] as equal to itself and less than any other float
    value.  This treatment of [nan] ensures that [compare] defines a total
    ordering relation.  *)

val equal : t -> t -> bool
(** The equal function for floating-point numbers, compared using {!compare}. *)

val min : t -> t -> t
(** [min x y] returns the minimum of [x] and [y].  It returns [nan]
   when [x] or [y] is [nan].  Moreover [min (-0.s) (+0.s) = -0.s] *)

val max : t -> t -> t
(** [max x y] returns the maximum of [x] and [y].  It returns [nan]
   when [x] or [y] is [nan].  Moreover [max (-0.s) (+0.s) = +0.s] *)

module With_weird_nan_behavior : sig
  external min : t -> t -> t
    = "caml_sse_float32_min_bytecode" "caml_sse_float32_min"
    [@@noalloc] [@@unboxed] [@@builtin]
  (** [min x y] returns the minimum of [x] and [y].
      If either [x] or [y] is [nan], [y] is returned.
      If both [x] and [y] equal zero, [y] is returned.
      The amd64 flambda-backend compiler translates this call to MINSS. *)

  external max : t -> t -> t
    = "caml_sse_float32_max_bytecode" "caml_sse_float32_max"
    [@@noalloc] [@@unboxed] [@@builtin]
  (** [max x y] returns the maximum of [x] and [y].  It returns [x]
      If either [x] or [y] is [nan], [y] is returned.
      If both [x] and [y] equal zero, [y] is returned.
      The amd64 flambda-backend compiler translates this call to MAXSS. *)
end

val min_max : t -> t -> t * t
(** [min_max x y] is [(min x y, max x y)], just more efficient. *)

val min_num : t -> t -> t
(** [min_num x y] returns the minimum of [x] and [y] treating [nan] as
   missing values.  If both [x] and [y] are [nan], [nan] is returned.
   Moreover [min_num (-0.s) (+0.s) = -0.s] *)

val max_num : t -> t -> t
(** [max_num x y] returns the maximum of [x] and [y] treating [nan] as
   missing values.  If both [x] and [y] are [nan] [nan] is returned.
   Moreover [max_num (-0.s) (+0.s) = +0.s] *)

val min_max_num : t -> t -> t * t
(** [min_max_num x y] is [(min_num x y, max_num x y)], just more
   efficient.  Note that in particular [min_max_num x nan = (x, x)]
   and [min_max_num nan y = (y, y)]. *)

external iround_half_to_even : t -> int64
  = "caml_sse_cast_float32_int64_bytecode" "caml_sse_cast_float32_int64"
  [@@noalloc] [@@unboxed] [@@builtin]
(** Rounds a [float32] to an [int64] using the current rounding mode. The default
    rounding mode is "round half to even", and we expect that no program will
    change the rounding mode.
    If the argument is NaN or infinite or if the rounded value cannot be
    represented, then the result is unspecified.
    The amd64 flambda-backend compiler translates this call to CVTSS2SI. *)

val round_half_to_even : t -> t
(** Rounds a [float32] to an integer [float32] using the current rounding
    mode.  The default rounding mode is "round half to even", and we
    expect that no program will change the rounding mode.
    The amd64 flambda-backend compiler translates this call to ROUNDSS. *)

val round_down : t -> t
(** Rounds a [float32] down to the next integer [float32] toward negative infinity.
    The amd64 flambda-backend compiler translates this call to ROUNDSS.*)

val round_up : t -> t
(** Rounds a [float32] up to the next integer [float32] toward positive infinity.
    The amd64 flambda-backend compiler translates this call to ROUNDSS.*)

val round_towards_zero : t -> t
(** Rounds a [float32] to the next integer [float32] toward zero.
    The amd64 flambda-backend compiler translates this call to ROUNDSS.*)

val seeded_hash : int -> t -> int
(** A seeded hash function for floats, with the same output value as
    {!Hashtbl.seeded_hash}. This function allows this module to be passed as
    argument to the functor {!Hashtbl.MakeSeeded}. *)

val hash : t -> int
(** An unseeded hash function for floats, with the same output value as
    {!Hashtbl.hash}. This function allows this module to be passed as argument
    to the functor {!Hashtbl.Make}. *)
