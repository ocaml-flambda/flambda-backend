# 1 "int64_u.mli"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*               Antal Spector-Zabusky, Jane Street, New York             *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Stdlib

(** Unboxed 64-bit integers.  This file primarily duplicates
    functionality from the [Int64] module, but for [int64#].

   This module provides operations on the type [int64#] of
   unboxed signed 64-bit integers.  Unlike the built-in [int] type,
   the type [int64#] is guaranteed to be exactly 64-bit wide on all
   platforms.  All arithmetic operations over [int64#] are wrapping, not
   truncating.
*)

type t = int64#
(** An alias for the type of unboxed 64-bit integers. *)

(* CR layouts v2.1: add back this comment to the above when we support
   literals.

    Literals for unboxed 64-bit integers are prefixed by # and suffixed by L:
    {[
     let zero: int64# = #0L
     let one: int64# = #1L
     let m_one: int64# = #-1L
    ]}
*)

(* Unboxed-specific stuff at the top. *)
external to_int64 : t -> (int64[@local_opt]) = "%box_int64"
(** Box a [int64#] *)

external of_int64 : (int64[@local_opt]) -> t = "%unbox_int64"
(** Unbox a boxed [int64] *)

(* Below here, everything also appears in [Int64], though most things are
   externals in that module. *)

(* CR layouts v5: add back all the commented-out values in this module when we
   support [bits64]s in structures.  This includes constants (e.g., [zero] and
   [one]) and functions that return things like [int64#] (e.g.,
   [of_string_opt]). *)

(* val zero : int64#
 * (** The unboxed 64-bit integer 0.*)
 *
 * val one : int64#
 * (** The unboxed 64-bit integer 1.*)
 *
 * val minus_one : int64#
 * (** The unboxed 64-bit integer -1.*) *)

val neg : t -> t
(** Unary negation. *)

val add : t -> t -> t
(** Addition. *)

val sub : t -> t -> t
(** Subtraction. *)

val mul : t -> t -> t
(** Multiplication. *)

val div : t -> t -> t
(** Integer division. This division rounds the real quotient of
   its arguments towards zero, as specified for {!Stdlib.(/)}.

   @raise Division_by_zero if the second
   argument is zero. *)

val unsigned_div : t -> t -> t
(** Same as {!div}, except that arguments and result are interpreted as {e
    unsigned} unboxed 64-bit integers. *)

val rem : t -> t -> t
(** Integer remainder.  If [y] is not zero, the result
   of [Int64_u.rem x y] satisfies the following property:
   [x = Int64_u.add (Int64_u.mul (Int64_u.div x y) y) (Int64_u.rem x y)].
   If [y = 0], [Int64_u.rem x y] raises [Division_by_zero]. *)

val unsigned_rem : t -> t -> t
(** Same as {!rem}, except that arguments and result are interpreted as {e
    unsigned} unboxed 64-bit integers. *)

val succ : t -> t
(** Successor.
   [Int64_u.succ x] is [Int64_u.add x Int64_u.one]. *)

val pred : t -> t
(** Predecessor.
   [Int64_u.pred x] is [Int64_u.sub x Int64_u.one]. *)

val abs : t -> t
(** Return the absolute value of its argument. *)

(* val max_int : int64#
 * (** The greatest representable unboxed 64-bit integer, 2{^63} - 1. *)
 *
 * val min_int : int64#
 * (** The smallest representable unboxed 64-bit integer, -2{^63}. *)
 *)

val logand : t -> t -> t
(** Bitwise logical and. *)

val logor : t -> t -> t
(** Bitwise logical or. *)

val logxor : t -> t -> t
(** Bitwise logical exclusive or. *)

val lognot : t -> t
(** Bitwise logical negation. *)

val shift_left : t -> int -> t
(** [Int64_u.shift_left x y] shifts [x] to the left by [y] bits.
   The result is unspecified if [y < 0] or [y >= 64]. *)

val shift_right : t -> int -> t
(** [Int64_u.shift_right x y] shifts [x] to the right by [y] bits.
   This is an arithmetic shift: the sign bit of [x] is replicated
   and inserted in the vacated bits.
   The result is unspecified if [y < 0] or [y >= 64]. *)

val shift_right_logical : t -> int -> t
(** [Int64_u.shift_right_logical x y] shifts [x] to the right
   by [y] bits.
   This is a logical shift: zeroes are inserted in the vacated bits
   regardless of the sign of [x].
   The result is unspecified if [y < 0] or [y >= 64]. *)

val of_int : int -> t
(** Convert the given integer (type [int]) to an unboxed 64-bit integer
   (type [int64#]). *)

val to_int : t -> int
(** Convert the given unboxed 64-bit integer (type [int64#]) to an
   integer (type [int]).  On 64-bit platforms, the 64-bit integer
   is taken modulo 2{^63}, i.e. the high-order bit is lost
   during the conversion.  On 32-bit platforms, the 64-bit integer
   is taken modulo 2{^31}, i.e. the top 33 bits are lost
   during the conversion. *)

val unsigned_to_int : t -> int option
(** Same as {!to_int}, but interprets the argument as an {e unsigned} integer.
    Returns [None] if the unsigned value of the argument cannot fit into an
    [int]. *)

val of_float : float -> t
(** Convert the given floating-point number to an unboxed 64-bit integer,
   discarding the fractional part (truncate towards 0).
   If the truncated floating-point number is outside the range
   \[{!Int64_u.min_int}, {!Int64_u.max_int}\], no exception is raised,
   and an unspecified, platform-dependent integer is returned. *)

val to_float : t -> float
(** Convert the given unboxed 64-bit integer to a floating-point number. *)

val of_int32 : int32 -> t
(** Convert the given 32-bit integer (type [int32])
   to an unboxed 64-bit integer (type [int64]). *)

val to_int32 : t -> int32
(** Convert the given unboxed 64-bit integer (type [int64]) to a
   32-bit integer (type [int32]).  The 64-bit integer
   is taken modulo 2{^32}, i.e. the top 32 bits are lost
   during the conversion.  *)

val of_nativeint : nativeint -> t
(** Convert the given native integer (type [nativeint])
   to an unboxed 64-bit integer (type [int64#]). *)

val to_nativeint : t -> nativeint
(** Convert the given unboxed 64-bit integer (type [int64#]) to a
   native integer.  On 32-bit platforms, the 64-bit integer
   is taken modulo 2{^32}.  On 64-bit platforms,
   the conversion is exact. *)

val of_int32_u : int32# -> t
(** Convert the given unboxed 32-bit integer (type [int32])
   to an unboxed 64-bit integer (type [int64]). *)

val to_int32_u : t -> int32#
(** Convert the given unboxed 64-bit integer (type [int64]) to an
   unboxed 32-bit integer (type [int32]).  The 64-bit integer
   is taken modulo 2{^32}, i.e. the top 32 bits are lost
   during the conversion.  *)

val of_nativeint_u : nativeint# -> t
(** Convert the given unboxed native integer (type [nativeint])
   to an unboxed 64-bit integer (type [int64#]). *)

val to_nativeint_u : t -> nativeint#
(** Convert the given unboxed 64-bit integer (type [int64#]) to an
   unboxed native integer.  On 32-bit platforms, the 64-bit integer
   is taken modulo 2{^32}.  On 64-bit platforms,
   the conversion is exact. *)

val of_string : string -> t
(** Convert the given string to an unboxed 64-bit integer.
   The string is read in decimal (by default, or if the string
   begins with [0u]) or in hexadecimal, octal or binary if the
   string begins with [0x], [0o] or [0b] respectively.

   The [0u] prefix reads the input as an unsigned integer in the range
   [[0, 2*Int64_u.max_int+1]].  If the input exceeds {!Int64_u.max_int}
   it is converted to the signed integer
   [Int64.min_int + input - Int64_u.max_int - 1].

   The [_] (underscore) character can appear anywhere in the string
   and is ignored.
   @raise Failure if the given string is not
   a valid representation of an integer, or if the integer represented
   exceeds the range of integers representable in type [int64]. *)

(* val of_string_opt: string -> t option
 * (** Same as [of_string], but return [None] instead of raising. *) *)

val to_string : t -> string
(** Return the string representation of its argument, in decimal. *)

val bits_of_float : float -> t
(** Return the internal representation of the given float according
   to the IEEE 754 floating-point 'double format' bit layout.
   Bit 63 of the result represents the sign of the float;
   bits 62 to 52 represent the (biased) exponent; bits 51 to 0
   represent the mantissa. *)

val float_of_bits : t -> float
(** Return the floating-point number whose internal representation,
   according to the IEEE 754 floating-point 'double format' bit layout,
   is the given [int64#]. *)

val compare: t -> t -> int
(** The comparison function for unboxed 64-bit integers, with the same
    specification as {!Stdlib.compare}. *)
(* CR layouts v2.1: Restore the below comment when we have an appropriate
   functor *)
(* Along with the type [t], this function [compare]
    allows the module [Int64_u] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val unsigned_compare: t -> t -> int
(** Same as {!compare}, except that arguments are interpreted as {e unsigned}
    unboxed 64-bit integers. *)

val equal: t -> t -> bool
(** The equal function for unboxed 64-bit ints. *)

val min: t -> t -> t
(** Return the smaller of the two arguments. *)

val max: t -> t -> t
(** Return the greater of the two arguments. *)
