# 1 "float_u.mli"
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

open! Stdlib

(* CR layouts v4: This file is based on [float.mli], which itself is generated
   from [float.template.mli].  That file is generated to deal with labels in the
   Array submodule, which we don't have here, so I haven't done something
   similar.  If we have an Array submodule here in the future, reconsider. *)

(* CR layouts: This module is not included in the [Stdlib] module, much like
   IArray.  This is intended to be a speed bump so that people won't
   accidentally rely on it in public release code.  Eventually, we plan to move
   it out of the stdlib library and into a new otherlib. *)

(** Unboxed floating-point arithmetic.  This file primarily duplicates
    functionality from the [Float] module, but for [float#].

    OCaml's floating-point numbers follow the
    IEEE 754 standard, using double precision (64 bits) numbers.
    Floating-point operations never raise an exception on overflow,
    underflow, division by zero, etc.  Instead, special IEEE numbers
    are returned as appropriate, such as [infinity] for [1.0 /. 0.0],
    [neg_infinity] for [-1.0 /. 0.0], and [nan] ('not a number')
    for [0.0 /. 0.0].  These special numbers then propagate through
    floating-point computations as expected: for instance,
    [1.0 /. infinity] is [0.0], basic arithmetic operations
    ([+.], [-.], [*.], [/.]) with [nan] as an argument return [nan], ...
*)

(* CR layouts v5: add back all the constants in this module (e.g., [zero] and
   [infinity]) when we we support [float64]s in structures. *)

(* Unboxed-specific stuff at the top. *)
external to_float : float# -> (float[@local_opt]) = "%box_float"
(** Box a [float#] *)

external of_float : (float[@local_opt]) -> float# = "%unbox_float"
(** Unbox a boxed [float] *)

(* Below here, everything also appears in [Float], though most things are
   externals in that module. *)

val neg : float# -> float#
(** Unary negation. *)

val add : float# -> float# -> float#
(** Floating-point addition. *)

val sub : float# -> float# -> float#
(** Floating-point subtraction. *)

val mul : float# -> float# -> float#
(** Floating-point multiplication. *)

val div : float# -> float# -> float#
(** Floating-point division. *)

val fma : float# -> float# -> float# -> float#
(** [fma x y z] returns [x * y + z], with a best effort for computing
   this expression with a single rounding, using either hardware
   instructions (providing full IEEE compliance) or a software
   emulation.

   On 64-bit Cygwin, 64-bit mingw-w64 and MSVC 2017 and earlier, this function
   may be emulated owing to known bugs on limitations on these platforms.
   Note: since software emulation of the fma is costly, make sure that you are
   using hardware fma support if performance matters. *)

val rem : float# -> float# -> float#
(** [rem a b] returns the remainder of [a] with respect to [b].  The returned
    value is [a -. n *. b], where [n] is the quotient [a /. b] rounded towards
    zero to an integer. *)

val succ : float# -> float#
(** [succ x] returns the floating point number right after [x] i.e.,
   the smallest floating-point number greater than [x].  See also
   {!next_after}. *)

val pred : float# -> float#
(** [pred x] returns the floating-point number right before [x] i.e.,
   the greatest floating-point number smaller than [x].  See also
   {!next_after}. *)

val abs : float# -> float#
(** [abs f] returns the absolute value of [f]. *)

val is_finite : float# -> bool
(** [is_finite x] is [true] if and only if [x] is finite i.e., not infinite and
    not {!nan}. *)

val is_infinite : float# -> bool
(** [is_infinite x] is [true] if and only if [x] is {!infinity} or
    {!neg_infinity}. *)

val is_nan : float# -> bool
(** [is_nan x] is [true] if and only if [x] is not a number (see {!nan}). *)


val is_integer : float# -> bool
(** [is_integer x] is [true] if and only if [x] is an integer. *)


val of_int : int -> float#
(** Convert an integer to floating-point. *)

val to_int : float# -> int
(** Truncate the given floating-point number to an integer.
    The result is unspecified if the argument is [nan] or falls outside the
    range of representable integers. *)

val of_string : string -> float#
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

(* CR layouts v5: Add [of_string_opt] when we allow float64s in structures. *)

val to_string : float# -> string
(** Return a string representation of a floating-point number.

    This conversion can involve a loss of precision. For greater control over
    the manner in which the number is printed, see {!Printf}.

    This function is an alias for {!Stdlib.string_of_float}. *)

type fpclass = Stdlib.fpclass =
    FP_normal           (** Normal number, none of the below *)
  | FP_subnormal        (** Number very close to 0.0, has reduced precision *)
  | FP_zero             (** Number is 0.0 or -0.0 *)
  | FP_infinite         (** Number is positive or negative infinity *)
  | FP_nan              (** Not a number: result of an undefined operation *)
(** The five classes of floating-point numbers, as determined by
    the {!classify_float} function. *)

val classify_float : float# -> fpclass
(** Return the class of the given floating-point number:
    normal, subnormal, zero, infinite, or not a number. *)

val pow : float# -> float# -> float#
(** Exponentiation. *)

val sqrt : float# -> float#
(** Square root. *)

val cbrt : float# -> float#
(** Cube root. *)

val exp : float# -> float#
(** Exponential. *)

val exp2 : float# -> float#
(** Base 2 exponential function. *)

val log : float# -> float#
(** Natural logarithm. *)

val log10 : float# -> float#
(** Base 10 logarithm. *)

val log2 : float# -> float#
(** Base 2 logarithm. *)

val expm1 : float# -> float#
(** [expm1 x] computes [exp x -. 1.0], giving numerically-accurate results
    even if [x] is close to [0.0]. *)

val log1p : float# -> float#
(** [log1p x] computes [log(1.0 +. x)] (natural logarithm),
    giving numerically-accurate results even if [x] is close to [0.0]. *)

val cos : float# -> float#
(** Cosine.  Argument is in radians. *)

val sin : float# -> float#
(** Sine.  Argument is in radians. *)

val tan : float# -> float#
(** Tangent.  Argument is in radians. *)

val acos : float# -> float#
(** Arc cosine.  The argument must fall within the range [[-1.0, 1.0]].
    Result is in radians and is between [0.0] and [pi]. *)

val asin : float# -> float#
(** Arc sine.  The argument must fall within the range [[-1.0, 1.0]].
    Result is in radians and is between [-pi/2] and [pi/2]. *)

val atan : float# -> float#
(** Arc tangent.
    Result is in radians and is between [-pi/2] and [pi/2]. *)

val atan2 : float# -> float# -> float#
(** [atan2 y x] returns the arc tangent of [y /. x].  The signs of [x]
    and [y] are used to determine the quadrant of the result.
    Result is in radians and is between [-pi] and [pi]. *)

val hypot : float# -> float# -> float#
(** [hypot x y] returns [sqrt(x *. x + y *. y)], that is, the length
    of the hypotenuse of a right-angled triangle with sides of length
    [x] and [y], or, equivalently, the distance of the point [(x,y)]
    to origin.  If one of [x] or [y] is infinite, returns [infinity]
    even if the other is [nan]. *)

val cosh : float# -> float#
(** Hyperbolic cosine.  Argument is in radians. *)

val sinh : float# -> float#
(** Hyperbolic sine.  Argument is in radians. *)

val tanh : float# -> float#
(** Hyperbolic tangent.  Argument is in radians. *)

val acosh : float# -> float#
(** Hyperbolic arc cosine.  The argument must fall within the range
    [[1.0, inf]].
    Result is in radians and is between [0.0] and [inf]. *)

val asinh : float# -> float#
(** Hyperbolic arc sine.  The argument and result range over the entire
    real line.
    Result is in radians. *)

val atanh : float# -> float#
(** Hyperbolic arc tangent.  The argument must fall within the range
    [[-1.0, 1.0]].
    Result is in radians and ranges over the entire real line. *)

val erf : float# -> float#
(** Error function.  The argument ranges over the entire real line.
    The result is always within [[-1.0, 1.0]]. *)

val erfc : float# -> float#
(** Complementary error function ([erfc x = 1 - erf x]).
    The argument ranges over the entire real line.
    The result is always within [[-1.0, 1.0]]. *)

val trunc : float# -> float#
(** [trunc x] rounds [x] to the nearest integer whose absolute value is
    less than or equal to [x]. *)

val round : float# -> float#
(** [round x] rounds [x] to the nearest integer with ties (fractional
   values of 0.5) rounded away from zero, regardless of the current
   rounding direction.  If [x] is an integer, [+0.], [-0.], [nan], or
   infinite, [x] itself is returned.

   On 64-bit mingw-w64, this function may be emulated owing to a bug in the
   C runtime library (CRT) on this platform. *)

val ceil : float# -> float#
(** Round above to an integer value.
    [ceil f] returns the least integer value greater than or equal to [f].
    The result is returned as a float. *)

val floor : float# -> float#
(** Round below to an integer value.
    [floor f] returns the greatest integer value less than or
    equal to [f].
    The result is returned as a float. *)

val next_after : float# -> float# -> float#
(** [next_after x y] returns the next representable floating-point
   value following [x] in the direction of [y].  More precisely, if
   [y] is greater (resp. less) than [x], it returns the smallest
   (resp. largest) representable number greater (resp. less) than [x].
   If [x] equals [y], the function returns [y].  If [x] or [y] is
   [nan], a [nan] is returned.
   Note that [next_after max_float infinity = infinity] and that
   [next_after 0. infinity] is the smallest denormalized positive number.
   If [x] is the smallest denormalized positive number,
   [next_after x 0. = 0.] *)

val copy_sign : float# -> float# -> float#
(** [copy_sign x y] returns a float whose absolute value is that of [x]
    and whose sign is that of [y].  If [x] is [nan], returns [nan].
    If [y] is [nan], returns either [x] or [-. x], but it is not
    specified which. *)

val sign_bit : float# -> bool
(** [sign_bit x] is [true] if and only if the sign bit of [x] is set.
    For example [sign_bit 1.] and [signbit 0.] are [false] while
    [sign_bit (-1.)] and [sign_bit (-0.)] are [true]. *)

(* CR layouts v5: add back [frexp], [modf], [min_max] and [min_max_num] when we
   have floats in structures. *)

val ldexp : float# -> int -> float#
(** [ldexp x n] returns [x *. 2 ** n]. *)

type t = float#
(** An alias for the type of floating-point numbers. *)

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
    when [x] or [y] is [nan].  Moreover [min (-0.) (+0.) = -0.] *)

val max : float# -> float# -> float#
(** [max x y] returns the maximum of [x] and [y].  It returns [nan]
    when [x] or [y] is [nan].  Moreover [max (-0.) (+0.) = +0.] *)

val min_num : t -> t -> t
(** [min_num x y] returns the minimum of [x] and [y] treating [nan] as
    missing values.  If both [x] and [y] are [nan], [nan] is returned.
    Moreover [min_num (-0.) (+0.) = -0.] *)

val max_num : t -> t -> t
(** [max_num x y] returns the maximum of [x] and [y] treating [nan] as
    missing values.  If both [x] and [y] are [nan] [nan] is returned.
    Moreover [max_num (-0.) (+0.) = +0.] *)

(* CR layouts v5: add back hash when we deal with the ad-hoc polymorphic
   functions. *)
