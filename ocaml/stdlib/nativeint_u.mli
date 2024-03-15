# 1 "nativeint_u.mli"
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

(** Unboxed processor-native integers.  This file primarily duplicates
    functionality from the [Nativeint] module, but for [nativeint#].

   This module provides operations on the type [nativeint#] of
   unboxed signed 32-bit integers (on 32-bit platforms) or
   unboxed signed 64-bit integers (on 64-bit platforms).
   This integer type has exactly the same width as that of a
   pointer type in the C compiler.  All arithmetic operations over
   [nativeint#] are wrapping, not truncating.
*)

(* CR layouts v2.1: add back this comment to the above when we support
   literals.

    Literals for unboxed native integers are prefixed by # and suffixed by n:
    {[
     let zero: nativeint# = #0n
     let one: nativeint# = #1n
     let m_one: nativeint# = #-1n
    ]}
*)

(* Unboxed-specific stuff at the top. *)
external to_nativeint : nativeint# -> (nativeint[@local_opt]) = "%box_nativeint"
(** Box a [nativeint#] *)

external of_nativeint : (nativeint[@local_opt]) -> nativeint# =
  "%unbox_nativeint"
(** Unbox a boxed [nativeint] *)

(* Below here, everything also appears in [Nativeint], though most things are
   externals in that module. *)

(* CR layouts v5: add back all the commented-out values in this module when we
   support [word]s in structures.  This includes constants (e.g., [zero] and
   [one]) and functions that return things like [nativeint#] (e.g.,
   [of_string_opt]). *)

(* val zero : nativeint#
 * (** The unboxed native integer 0.*)
 *
 * val one : nativeint#
 * (** The unboxed native integer 1.*)
 *
 * val minus_one : nativeint#
 * (** The unboxed native integer -1.*) *)

val neg : nativeint# -> nativeint#
(** Unary negation. *)

val add : nativeint# -> nativeint# -> nativeint#
(** Addition. *)

val sub : nativeint# -> nativeint# -> nativeint#
(** Subtraction. *)

val mul : nativeint# -> nativeint# -> nativeint#
(** Multiplication. *)

val div : nativeint# -> nativeint# -> nativeint#
(** Integer division. This division rounds the real quotient of
   its arguments towards zero, as specified for {!Stdlib.(/)}.

   @raise Division_by_zero if the second
   argument is zero. *)

val unsigned_div : nativeint# -> nativeint# -> nativeint#
(** Same as {!div}, except that arguments and result are interpreted as {e
    unsigned} unboxed native integers. *)

val rem : nativeint# -> nativeint# -> nativeint#
(** Integer remainder.  If [y] is not zero, the result
   of [Nativeint_u.rem x y] satisfies the following properties:
   [Nativeint_u.zero <= Nativeint_u.rem x y < Nativeint_u.abs y] and
   [x = Nativeint_u.add (Nativeint_u.mul (Nativeint_u.div x y) y)
                        (Nativeint_u.rem x y)].
   If [y = 0], [Nativeint_u.rem x y] raises [Division_by_zero]. *)

val unsigned_rem : nativeint# -> nativeint# -> nativeint#
(** Same as {!rem}, except that arguments and result are interpreted as {e
    unsigned} unboxed native integers. *)

val succ : nativeint# -> nativeint#
(** Successor.
   [Nativeint_u.succ x] is [Nativeint_u.add x Nativeint_u.one]. *)

val pred : nativeint# -> nativeint#
(** Predecessor.
   [Nativeint_u.pred x] is [Nativeint_u.sub x Nativeint_u.one]. *)

val abs : nativeint# -> nativeint#
(** Return the absolute value of its argument. *)

val size : int
(** The size in bits of an unboxed native integer.  This is equal to [32]
   on a 32-bit platform and to [64] on a 64-bit platform. *)

(* val max_int : nativeint#
 * (** The greatest representable unboxed native integer,
 *    either 2{^31} - 1 on a 32-bit platform,
 *    or 2{^63} - 1 on a 64-bit platform. *)
 *
 * val min_int : nativeint#
 * (** The smallest representable unboxed native integer,
 *    either -2{^31} on a 32-bit platform,
 *    or -2{^63} on a 64-bit platform. *) *)

val logand : nativeint# -> nativeint# -> nativeint#
(** Bitwise logical and. *)

val logor : nativeint# -> nativeint# -> nativeint#
(** Bitwise logical or. *)

val logxor : nativeint# -> nativeint# -> nativeint#
(** Bitwise logical exclusive or. *)

val lognot : nativeint# -> nativeint#
(** Bitwise logical negation. *)

val shift_left : nativeint# -> int -> nativeint#
(** [Nativeint_u.shift_left x y] shifts [x] to the left by [y] bits.
   The result is unspecified if [y < 0] or [y >= bitsize],
   where [bitsize] is [#32] on a 32-bit platform and
   [#64] on a 64-bit platform. *)

val shift_right : nativeint# -> int -> nativeint#
(** [Nativeint_u.shift_right x y] shifts [x] to the right by [y] bits.
   This is an arithmetic shift: the sign bit of [x] is replicated
   and inserted in the vacated bits.
   The result is unspecified if [y < 0] or [y >= bitsize]. *)

val shift_right_logical : nativeint# -> int -> nativeint#
(** [Nativeint_u.shift_right_logical x y] shifts [x] to the right
   by [y] bits.
   This is a logical shift: zeroes are inserted in the vacated bits
   regardless of the sign of [x].
   The result is unspecified if [y < 0] or [y >= bitsize]. *)


val of_int : int -> nativeint#
(** Convert the given integer (type [int]) to an unboxed native integer
   (type [nativeint#]). *)

val to_int : nativeint# -> int
(** Convert the given unboxed native integer (type [nativeint#]) to an
   integer (type [int]).  The high-order bit is lost during
   the conversion. *)

val unsigned_to_int : nativeint# -> int option
(** Same as {!to_int}, but interprets the argument as an {e unsigned} integer.
    Returns [None] if the unsigned value of the argument cannot fit into an
    [int]. *)

val of_float : float -> nativeint#
(** Convert the given floating-point number to an unboxed native integer,
   discarding the fractional part (truncate towards 0).
   If the truncated floating-point number is outside the range
   \[{!Nativeint_u.min_int}, {!Nativeint_u.max_int}\], no exception is raised,
   and an unspecified, platform-dependent integer is returned. *)

val to_float : nativeint# -> float
(** Convert the given unboxed native integer to a floating-point number. *)

val of_int32 : int32 -> nativeint#
(** Convert the given 32-bit integer (type [int32])
   to an unboxed native integer. *)

val to_int32 : nativeint# -> int32
(** Convert the given unboxed native integer to a
   32-bit integer (type [int32]).  On 64-bit platforms,
   the 64-bit unboxed native integer is taken modulo 2{^32},
   i.e. the top 32 bits are lost.  On 32-bit platforms,
   the conversion is exact. *)

val of_int32_u : int32# -> nativeint#
(** Convert the given unboxed 32-bit integer (type [int32])
   to an unboxed native integer. *)

val to_int32_u : nativeint# -> int32#
(** Convert the given unboxed native integer to an unboxed
   32-bit integer (type [int32]).  On 64-bit platforms,
   the 64-bit unboxed native integer is taken modulo 2{^32},
   i.e. the top 32 bits are lost.  On 32-bit platforms,
   the conversion is exact. *)

val of_string : string -> nativeint#
(** Convert the given string to an unboxed native integer.
   The string is read in decimal (by default, or if the string
   begins with [0u]) or in hexadecimal, octal or binary if the
   string begins with [0x], [0o] or [0b] respectively.

   The [0u] prefix reads the input as an unsigned integer in the range
   [[0, 2*Nativeint_u.max_int+1]].  If the input exceeds {!Nativeint_u.max_int}
   it is converted to the signed integer
   [Int64.min_int + input - Nativeint_u.max_int - 1].

   @raise Failure if the given string is not
   a valid representation of an integer, or if the integer represented
   exceeds the range of integers representable in type [nativeint]. *)

(* val of_string_opt: string -> nativeint# option
 * (** Same as [of_string], but return [None] instead of raising. *) *)

val to_string : nativeint# -> string
(** Return the string representation of its argument, in decimal. *)

type t = nativeint#
(** An alias for the type of unboxed native integers. *)

val compare: t -> t -> int
(** The comparison function for unboxed native integers, with the same
    specification as {!Stdlib.compare}. *)
(* CR layouts v2.1: Restore the below comment when we have an appropriate
   functor *)
(* Along with the type [t], this function [compare]
    allows the module [Nativeint_u] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. *)

val unsigned_compare: t -> t -> int
(** Same as {!compare}, except that arguments are interpreted as {e unsigned}
    unboxed native integers. *)

val equal: t -> t -> bool
(** The equal function for unboxed native ints. *)

val min: t -> t -> t
(** Return the smaller of the two arguments. *)

val max: t -> t -> t
(** Return the greater of the two arguments. *)
