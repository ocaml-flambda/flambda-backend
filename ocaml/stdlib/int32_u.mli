# 1 "int32_u.mli"
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

(** Unboxed 32-bit integers.  This file primarily duplicates
    functionality from the [Int32] module, but for [int32#].

   This module provides operations on the type [int32#]
   of unboxed signed 32-bit integers.  Unlike the built-in [int] type,
   the type [int32#] is guaranteed to be exactly 32-bits wide on all
   platforms.  All arithmetic operations over [int32#] are wrapping, not
   truncating.
*)

(* CR layouts v2.1: add back this comment to the above when we support
   literals.

    Literals for unboxed 32-bit integers are prefixed by # and suffixed by l:
    {[
     let zero: int32# = #0l
     let one: int32# = #1l
     let m_one: int32# = #-1l
    ]}
*)

(* Unboxed-specific stuff at the top. *)
external to_int32 : int32# -> (int32[@local_opt]) = "%box_int32"
(** Box a [int32#] *)

external of_int32 : (int32[@local_opt]) -> int32# = "%unbox_int32"
(** Unbox a boxed [int32] *)

(* Below here, everything also appears in [Int32], though most things are
   externals in that module. *)

(* CR layouts v5: add back all the commented-out values in this module when we
   support [bits32]s in structures.  This includes constants (e.g., [zero] and
   [one]) and functions that return things like [int32#] (e.g.,
   [of_string_opt]). *)

(* val zero : int32#
 * (** The unboxed 32-bit integer 0.*)
 *
 * val one : int32#
 * (** The unboxed 32-bit integer 1.*)
 *
 * val minus_one : int32#
 * (** The unboxed 32-bit integer -1.*) *)

val neg : int32# -> int32#
(** Unary negation. *)

val add : int32# -> int32# -> int32#
(** Addition. *)

val sub : int32# -> int32# -> int32#
(** Subtraction. *)

val mul : int32# -> int32# -> int32#
(** Multiplication. *)

val div : int32# -> int32# -> int32#
(** Integer division. This division rounds the real quotient of
   its arguments towards zero, as specified for {!Stdlib.(/)}.

   @raise Division_by_zero if the second
   argument is zero. *)

val unsigned_div : int32# -> int32# -> int32#
(** Same as {!div}, except that arguments and result are interpreted as {e
    unsigned} unboxed 32-bit integers. *)

val rem : int32# -> int32# -> int32#
(** Integer remainder.  If [y] is not zero, the result
   of [Int32_u.rem x y] satisfies the following property:
   [x = Int32_u.add (Int32_u.mul (Int32_u.div x y) y) (Int32_u.rem x y)].
   If [y = 0], [Int32_u.rem x y] raises [Division_by_zero]. *)

val unsigned_rem : int32# -> int32# -> int32#
(** Same as {!rem}, except that arguments and result are interpreted as {e
    unsigned} unboxed 32-bit integers. *)

val succ : int32# -> int32#
(** Successor.
   [Int32_u.succ x] is [Int32_u.add x Int32_u.one]. *)

val pred : int32# -> int32#
(** Predecessor.
   [Int32_u.pred x] is [Int32_u.sub x Int32_u.one]. *)

val abs : int32# -> int32#
(** Return the absolute value of its argument. *)

(* val max_int : int32#
 * (** The greatest representable unboxed 32-bit integer, 2{^31} - 1. *)
 *
 * val min_int : int32#
 * (** The smallest representable unboxed 32-bit integer, -2{^31}. *)
 *)

val logand : int32# -> int32# -> int32#
(** Bitwise logical and. *)

val logor : int32# -> int32# -> int32#
(** Bitwise logical or. *)

val logxor : int32# -> int32# -> int32#
(** Bitwise logical exclusive or. *)

val lognot : int32# -> int32#
(** Bitwise logical negation. *)

val shift_left : int32# -> int -> int32#
(** [Int32_u.shift_left x y] shifts [x] to the left by [y] bits.
   The result is unspecified if [y < 0] or [y >= 32]. *)

val shift_right : int32# -> int -> int32#
(** [Int32_u.shift_right x y] shifts [x] to the right by [y] bits.
   This is an arithmetic shift: the sign bit of [x] is replicated
   and inserted in the vacated bits.
   The result is unspecified if [y < 0] or [y >= 32]. *)

val shift_right_logical : int32# -> int -> int32#
(** [Int32_u.shift_right_logical x y] shifts [x] to the right
   by [y] bits.
   This is a logical shift: zeroes are inserted in the vacated bits
   regardless of the sign of [x].
   The result is unspecified if [y < 0] or [y >= 32]. *)

val of_int : int -> int32#
(** Convert the given integer (type [int]) to an unboxed 32-bit integer
   (type [int32#]). On 64-bit platforms, the argument is taken
    modulo 2{^32}. *)

val to_int : int32# -> int
(** Convert the given unboxed 32-bit integer (type [int32#]) to an
   integer (type [int]).  On 32-bit platforms, the 32-bit integer
   is taken modulo 2{^31}, i.e. the high-order bit is lost
   during the conversion.  On 64-bit platforms, the conversion
   is exact. *)

val unsigned_to_int : int32# -> int option
(** Same as {!to_int}, but interprets the argument as an {e unsigned} integer.
    Returns [None] if the unsigned value of the argument cannot fit into an
    [int]. *)

val of_float : float -> int32#
(** Convert the given floating-point number to an unboxed 32-bit integer,
   discarding the fractional part (truncate towards 0).
   If the truncated floating-point number is outside the range
   \[{!Int32_u.min_int}, {!Int32_u.max_int}\], no exception is raised,
   and an unspecified, platform-dependent integer is returned. *)

val to_float : int32# -> float
(** Convert the given unboxed 32-bit integer to a floating-point number. *)

val of_string : string -> int32#
(** Convert the given string to an unboxed 32-bit integer.
   The string is read in decimal (by default, or if the string
   begins with [0u]) or in hexadecimal, octal or binary if the
   string begins with [0x], [0o] or [0b] respectively.

   The [0u] prefix reads the input as an unsigned integer in the range
   [[0, 2*Int32_u.max_int+1]].  If the input exceeds {!Int32_u.max_int}
   it is converted to the signed integer
   [Int32.min_int + input - Int32_u.max_int - 1].

   The [_] (underscore) character can appear anywhere in the string
   and is ignored.
   @raise Failure if the given string is not
   a valid representation of an integer, or if the integer represented
   exceeds the range of integers representable in type [int32]. *)

(* val of_string_opt: string -> int32# option
 * (** Same as [of_string], but return [None] instead of raising. *) *)

val to_string : int32# -> string
(** Return the string representation of its argument, in signed decimal. *)

val bits_of_float : float -> int32#
(** Return the internal representation of the given float according
   to the IEEE 754 floating-point 'single format' bit layout.
   Bit 31 of the result represents the sign of the float;
   bits 30 to 23 represent the (biased) exponent; bits 22 to 0
   represent the mantissa. *)

val float_of_bits : int32# -> float
(** Return the floating-point number whose internal representation,
   according to the IEEE 754 floating-point 'single format' bit layout,
   is the given [int32#]. *)

type t = int32#
(** An alias for the type of unboxed 32-bit integers. *)

val compare: t -> t -> int
(** The comparison function for unboxed 32-bit integers, with the same
    specification as {!Stdlib.compare}. *)
(* CR layouts XXX ASZ: Restore the below comment when we have an appropriate
   functor *)
(* Along with the type [t], this function [compare]
    allows the module [Int32_u] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val unsigned_compare: t -> t -> int
(** Same as {!compare}, except that arguments are interpreted as {e unsigned}
    unboxed 32-bit integers. *)

val equal: t -> t -> bool
(** The equal function for unboxed 32-bit ints. *)

val min: t -> t -> t
(** Return the smaller of the two arguments. *)

val max: t -> t -> t
(** Return the greater of the two arguments. *)
