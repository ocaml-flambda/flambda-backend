(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                 Chris Casinghino, Jane Street, New York                *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

(** Unboxed 32-bit floating-point arithmetic.  This file primarily duplicates
    functionality from the [Float32] module, but for [float32#].

    OCaml's 32-bit floating-point numbers follow the
    IEEE 754 standard, using single precision (32 bits) numbers.
    Floating-point operations never raise an exception on overflow,
    underflow, division by zero, etc.  Instead, special IEEE numbers
    are returned as appropriate, such as [infinity] for [#1.0s /. #0.0s],
    [neg_infinity] for [#-1.0s /. #0.0s], and [nan] ('not a number')
    for [#0.0s /. #0.0s].  These special numbers then propagate through
    floating-point computations as expected: for instance,
    [#1.0s /. infinity] is [#0.0s], basic arithmetic operations
    ([+.], [-.], [*.], [/.]) with [nan] as an argument return [nan], ...
*)

type t = float32#
(** An alias for the type of unboxed 32-bit floating-point numbers. *)

(* CR layouts v5: add back all the constants in this module (e.g., [zero] and
   [infinity]) when we we support [float32]s in structures. *)

(* Unboxed-specific stuff at the top. *)
external to_float32 : t -> (float32[@local_opt]) = "%box_float32"
(** Box a [float32#] *)

external of_float32 : (float32[@local_opt]) -> t = "%unbox_float32"
(** Unbox a boxed [float32] *)

(* Below here, everything also appears in [Float32], though most things are
   externals in that module. *)

val neg : t -> t
(** Unary negation. *)

val add : t -> t -> t
(** Floating-point addition. *)

val sub : t -> t -> t
(** Floating-point subtraction. *)

val mul : t -> t -> t
(** Floating-point multiplication. *)

val div : t -> t -> t
(** Floating-point division. *)

val pow : t -> t -> t
(** Exponentiation. *)

(** Floating-point arithmetic operator overloads. *)
module Operators : sig
  val ( ~-. ) : t -> t
  (** Unary negation. *)

  val ( +. ) : t -> t -> t
  (** Floating-point addition. *)

  val ( -. ) : t -> t -> t
  (** Floating-point subtraction. *)

  val ( *. ) : t -> t -> t
  (** Floating-point multiplication. *)

  val ( /. ) : t -> t -> t
  (** Floating-point division. *)

  val ( ** ) : t -> t -> t
  (** Exponentiation. *)
end

val fma : t -> t -> t -> t
(** [fma x y z] returns [x * y + z], with a best effort for computing
   this expression with a single rounding, using either hardware
   instructions (providing full IEEE compliance) or a software
   emulation. *)

val rem : t -> t -> t
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

val abs : t -> t
(** [abs f] returns the absolute value of [f]. *)

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

val of_int : int -> t
(** Convert an integer to floating-point. *)

val to_int : t -> int
(** Truncate the given floating-point number to an integer.
    The result is unspecified if the argument is [nan] or falls outside the
    range of representable integers. *)

val of_int64 : int64# -> t
(** Convert the given 64-bit integer to the nearest representable 32-bit float. *)

val to_int64 : t -> int64#
(** Convert the given 32-bit float to a 64-bit integer,
    discarding the fractional part (truncate towards 0).
    If the truncated floating-point number is outside the range
    \[{!Int64.min_int}, {!Int64.max_int}\], no exception is raised, and
    an unspecified, platform-dependent integer is returned. *)

val of_float : float# -> t
(** Convert a 64-bit float to the nearest 32-bit float. *)

val to_float : t -> float#
(** Convert a 32-bit float to a 64-bit float. *)

val of_bits : int32# -> t
(** Convert a 32-bit float to a 32-bit integer, preserving the value's
    bit pattern. *)

val to_bits : t -> int32#
(** Convert a 32-bit integer to a 32-bit float, preserving the value's
    bit pattern. *)

val of_string : string -> t
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
    representation of a float. *)

(* CR layouts v5: Add [of_string_opt] when we allow float32s in structures. *)

val to_string : t -> string
(** Return a string representation of a floating-point number.

    This conversion can involve a loss of precision. For greater control over
    the manner in which the number is printed, see {!Printf}.

    This function is an alias for {!Stdlib.string_of_float}. *)

type fpclass = Stdlib.fpclass =
    FP_normal           (** Normal number, none of the below *)
  | FP_subnormal        (** Number very close to 0.0s, has reduced precision *)
  | FP_zero             (** Number is 0.0s or -0.0s *)
  | FP_infinite         (** Number is positive or negative infinity *)
  | FP_nan              (** Not a number: result of an undefined operation *)
(** The five classes of floating-point numbers, as determined by
    the {!classify_float} function. *)

val classify_float : t -> fpclass
(** Return the class of the given floating-point number:
    normal, subnormal, zero, infinite, or not a number. *)

val sqrt : t -> t
(** Square root. *)

val cbrt : t -> t
(** Cube root. *)

val exp : t -> t
(** Exponential. *)

val exp2 : t -> t
(** Base 2 exponential function. *)

val log : t -> t
(** Natural logarithm. *)

val log10 : t -> t
(** Base 10 logarithm. *)

val log2 : t -> t
(** Base 2 logarithm. *)

val expm1 : t -> t
(** [expm1 x] computes [exp x -. #1.0s], giving numerically-accurate results
    even if [x] is close to [#0.0s]. *)

val log1p : t -> t
(** [log1p x] computes [log(#1.0s +. x)] (natural logarithm),
    giving numerically-accurate results even if [x] is close to [#0.0s]. *)

val cos : t -> t
(** Cosine.  Argument is in radians. *)

val sin : t -> t
(** Sine.  Argument is in radians. *)

val tan : t -> t
(** Tangent.  Argument is in radians. *)

val acos : t -> t
(** Arc cosine.  The argument must fall within the range [[-1.0s, 1.0s]].
    Result is in radians and is between [0.0s] and [pi]. *)

val asin : t -> t
(** Arc sine.  The argument must fall within the range [[-1.0s, 1.0s]].
    Result is in radians and is between [-pi/2] and [pi/2]. *)

val atan : t -> t
(** Arc tangent.
    Result is in radians and is between [-pi/2] and [pi/2]. *)

val atan2 : t -> t -> t
(** [atan2 y x] returns the arc tangent of [y /. x].  The signs of [x]
    and [y] are used to determine the quadrant of the result.
    Result is in radians and is between [-pi] and [pi]. *)

val hypot : t -> t -> t
(** [hypot x y] returns [sqrt(x *. x + y *. y)], that is, the length
    of the hypotenuse of a right-angled triangle with sides of length
    [x] and [y], or, equivalently, the distance of the point [(x,y)]
    to origin.  If one of [x] or [y] is infinite, returns [infinity]
    even if the other is [nan]. *)

val cosh : t -> t
(** Hyperbolic cosine.  Argument is in radians. *)

val sinh : t -> t
(** Hyperbolic sine.  Argument is in radians. *)

val tanh : t -> t
(** Hyperbolic tangent.  Argument is in radians. *)

val acosh : t -> t
(** Hyperbolic arc cosine.  The argument must fall within the range
    [[1.0s, inf]].
    Result is in radians and is between [0.0s] and [inf]. *)

val asinh : t -> t
(** Hyperbolic arc sine.  The argument and result range over the entire
    real line.
    Result is in radians. *)

val atanh : t -> t
(** Hyperbolic arc tangent.  The argument must fall within the range
    [[-1.0s, 1.0s]].
    Result is in radians and ranges over the entire real line. *)

val erf : t -> t
(** Error function.  The argument ranges over the entire real line.
    The result is always within [[-1.0s, 1.0s]]. *)

val erfc : t -> t
(** Complementary error function ([erfc x = 1 - erf x]).
    The argument ranges over the entire real line.
    The result is always within [[-1.0s, 1.0s]]. *)

val trunc : t -> t
(** [trunc x] rounds [x] to the nearest integer whose absolute value is
    less than or equal to [x]. *)

val round : t -> t
(** [round x] rounds [x] to the nearest integer with ties (fractional
   values of 0.5) rounded away from zero, regardless of the current
   rounding direction.  If [x] is an integer, [#+0.s], [#-0.s], [nan], or
   infinite, [x] itself is returned.

   On 64-bit mingw-w64, this function may be emulated owing to a bug in the
   C runtime library (CRT) on this platform. *)

val ceil : t -> t
(** Round above to an integer value.
    [ceil f] returns the least integer value greater than or equal to [f].
    The result is returned as a float. *)

val floor : t -> t
(** Round below to an integer value.
    [floor f] returns the greatest integer value less than or
    equal to [f].
    The result is returned as a float. *)

val next_after : t -> t -> t
(** [next_after x y] returns the next representable floating-point
   value following [x] in the direction of [y].  More precisely, if
   [y] is greater (resp. less) than [x], it returns the smallest
   (resp. largest) representable number greater (resp. less) than [x].
   If [x] equals [y], the function returns [y].  If [x] or [y] is
   [nan], a [nan] is returned.
   Note that [next_after max_float infinity = infinity] and that
   [next_after #0. infinity] is the smallest denormalized positive number.
   If [x] is the smallest denormalized positive number,
   [next_after x #0. = #0.] *)

val copy_sign : t -> t -> t
(** [copy_sign x y] returns a float whose absolute value is that of [x]
    and whose sign is that of [y].  If [x] is [nan], returns [nan].
    If [y] is [nan], returns either [x] or [-. x], but it is not
    specified which. *)

val sign_bit : t -> bool
(** [sign_bit x] is [true] if and only if the sign bit of [x] is set.
    For example [sign_bit #1.] and [signbit #0.] are [false] while
    [sign_bit #-1.] and [sign_bit #-0.] are [true]. *)

(* CR layouts v5: add back [frexp], [modf], [min_max] and [min_max_num] when we
   have float32s in structures. *)

val ldexp : t -> int -> t
(** [ldexp x n] returns [x *. #2 ** n]. *)

val compare: t -> t -> int
(** [compare x y] returns [0] if [x] is equal to [y], a negative integer if [x]
    is less than [y], and a positive integer if [x] is greater than
    [y]. [compare] treats [nan] as equal to itself and less than any other float
    value.  This treatment of [nan] ensures that [compare] defines a total
    ordering relation.  *)

val equal: t -> t -> bool
(** The equal function for floating-point numbers, compared using {!compare}. *)

val min : t -> t -> t
(** [min x y] returns the minimum of [x] and [y].  It returns [nan]
    when [x] or [y] is [nan].  Moreover [min #-0.s #+0.s = #-0.s] *)

val max : t -> t -> t
(** [max x y] returns the maximum of [x] and [y].  It returns [nan]
    when [x] or [y] is [nan].  Moreover [max #-0.s #+0.s = #+0.s] *)

module With_weird_nan_behavior : sig
    val min : t -> t -> t
    (** [min x y] returns the minimum of [x] and [y].
        If either [x] or [y] is [nan], [y] is returned.
        If both [x] and [y] equal zero, [y] is returned.
        The amd64 flambda-backend compiler translates this call to MINSS. *)

    val max : t -> t -> t
    (** [max x y] returns the maximum of [x] and [y].  It returns [x]
        If either [x] or [y] is [nan], [y] is returned.
        If both [x] and [y] equal zero, [y] is returned.
        The amd64 flambda-backend compiler translates this call to MAXSS. *)
end

val min_num : t -> t -> t
(** [min_num x y] returns the minimum of [x] and [y] treating [nan] as
    missing values.  If both [x] and [y] are [nan], [nan] is returned.
    Moreover [min_num #-0.s #+0.s = #-0.s] *)

val max_num : t -> t -> t
(** [max_num x y] returns the maximum of [x] and [y] treating [nan] as
    missing values.  If both [x] and [y] are [nan] [nan] is returned.
    Moreover [max_num #-0.s #+0.s = #+0.s] *)

val iround_half_to_even : t -> int64#
(** Rounds a [float32#] to an [int64#] using the current rounding mode. The default
    rounding mode is "round half to even", and we expect that no program will
    change the rounding mode.
    If the argument is NaN or infinite or if the rounded value cannot be
    represented, then the result is unspecified.
    The amd64 flambda-backend compiler translates this call to CVTSS2SI. *)

val round_half_to_even : t -> t
(** Rounds a [float32#] to an integer [float32#] using the current rounding
    mode.  The default rounding mode is "round half to even", and we
    expect that no program will change the rounding mode.
    The amd64 flambda-backend compiler translates this call to ROUNDSS. *)

val round_down : t -> t
(** Rounds a [float32#] down to the next integer [float32#] toward negative infinity.
    The amd64 flambda-backend compiler translates this call to ROUNDSS.*)

val round_up : t -> t
(** Rounds a [float32#] up to the next integer [float32#] toward positive infinity.
    The amd64 flambda-backend compiler translates this call to ROUNDSS.*)

val round_towards_zero : t -> t
(** Rounds a [float32#] to the next integer [float32#] toward zero.
    The amd64 flambda-backend compiler translates this call to ROUNDSS.*)

(* CR layouts v5: add back hash when we deal with the ad-hoc polymorphic
   functions. *)
