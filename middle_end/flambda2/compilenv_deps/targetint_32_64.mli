(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                        Nicolas Ojeda Bar, LexiFi                       *)
(*                    Mark Shinwell, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2016 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Target processor-native integers.

    This module provides operations on the type of signed 32-bit integers (on
    32-bit target platforms) or signed 64-bit integers (on 64-bit target
    platforms). This integer type has exactly the same width as that of a
    pointer type in the C compiler. All arithmetic operations over are taken
    modulo 2{^32} or 2{^64} depending on the word size of the target
    architecture.

    {b Warning:} this module is unstable and part of
    {{!Compiler_libs}compiler-libs}. *)

(** The type of target integers. *)
type t

type targetint = t

(** The target integer 0.*)
val zero : t

(** The target integer 1.*)
val one : t

(** The target integer -1.*)
val minus_one : t

(** Unary negation. *)
val neg : t -> t

(** Addition. *)
val add : t -> t -> t

(** Subtraction. *)
val sub : t -> t -> t

(** Multiplication. *)
val mul : t -> t -> t

(** Integer division. Raise [Division_by_zero] if the second argument is zero.
    This division rounds the real quotient of its arguments towards zero, as
    specified for {!Stdlib.(/)}. *)
val div : t -> t -> t

(** Same as {!div}, except that arguments and result are interpreted as {e
    unsigned} integers. *)
val unsigned_div : t -> t -> t

(** Integer remainder. If [y] is not zero, the result * of [Targetint_32_64.rem
    x y] satisfies the following properties: * [Targetint_32_64.zero <=
    Nativeint.rem x y < Targetint_32_64.abs y] and * [x = Targetint_32_64.add
    (Targetint_32_64.mul (Targetint_32_64.div x y) y) * (Targetint_32_64.rem x
    y)]. * If [y = 0], [Targetint_32_64.rem x y] raises [Division_by_zero]. *)
val rem : t -> t -> t

(** Same as {!rem}, except that arguments and result are interpreted as {e
    unsigned} integers. *)
val unsigned_rem : t -> t -> t

(** Successor. [Targetint_32_64.succ x] is [Targetint_32_64.add x
    Targetint_32_64.one]. *)
val succ : t -> t

(** Predecessor. [Targetint_32_64.pred x] is [Targetint_32_64.sub x
    Targetint_32_64.one]. *)
val pred : t -> t

(** Return the absolute value of its argument. *)
val abs : t -> t

(** The size in bits of a target native integer. *)
val size : int

(** The possible numbers of bits of a target native integer. *)
type num_bits = Thirty_two | Sixty_four
(**)

val num_bits : num_bits
(* The number of bits of a target native integer. *)

(** The greatest representable target integer, either 2{^31} - 1 on a 32-bit
    platform, or 2{^63} - 1 on a 64-bit platform. *)
val max_int : t

(** The smallest representable target integer, either -2{^31} on a 32-bit
    platform, or -2{^63} on a 64-bit platform. *)
val min_int : t

(** Bitwise logical and. *)
val logand : t -> t -> t

(** Bitwise logical or. *)
val logor : t -> t -> t

(** Bitwise logical exclusive or. *)
val logxor : t -> t -> t

(** Bitwise logical negation. *)
val lognot : t -> t

(** [Targetint_32_64.shift_left x y] shifts [x] to the left by [y] bits.

    The result is unspecified if [y < 0] or [y >= bitsize], where [bitsize] is
    [32] on a 32-bit platform and [64] on a 64-bit platform. *)
val shift_left : t -> int -> t

(** [Targetint_32_64.shift_right x y] shifts [x] to the right by [y] bits.

    This is an arithmetic shift: the sign bit of [x] is replicated and inserted
    in the vacated bits.

    The result is unspecified if [y < 0] or [y >= bitsize]. *)
val shift_right : t -> int -> t

(** [Targetint_32_64.shift_right_logical x y] shifts [x] to the right by [y]
    bits.

    This is a logical shift: zeroes are inserted in the vacated bits regardless
    of the sign of [x].

    The result is unspecified if [y < 0] or [y >= bitsize]. *)
val shift_right_logical : t -> int -> t

(** Convert the given integer (type [int]) to a target integer (type [t]),
    modulo the target word size. *)
val of_int : int -> t

(** Convert the given integer (type [int]) to a target integer (type [t]).
    Raises a fatal error if the conversion is not exact. *)
val of_int_exn : int -> t

(** Convert the given target integer (type [t]) to an integer (type [int]). The
    high-order bit is lost during the conversion. *)
val to_int : t -> int

(** Convert the given floating-point number to a target integer, discarding the
    fractional part (truncate towards 0).

    The result of the conversion is undefined if, after truncation, the number
    is outside the range \[{!Targetint_32_64.min_int},
    {!Targetint_32_64.max_int}\]. *)
val of_float : float -> t

(** Convert the given target integer to a floating-point number. *)
val to_float : t -> float

(** Convert the given 32-bit integer (type [int32]) to a target integer. *)
val of_int32 : int32 -> t

(** Convert the given target integer to a 32-bit integer (type [int32]).

    On 64-bit platforms, the 64-bit native integer is taken modulo 2{^32}, i.e.
    the top 32 bits are lost. On 32-bit platforms, the conversion is exact. *)
val to_int32 : t -> int32

(** Convert the given 64-bit integer (type [int64]) to a target integer, modulo
    the target word size. *)
val of_int64 : int64 -> t

(** Convert the given target integer to a 64-bit integer (type [int64]). *)
val to_int64 : t -> int64

(** Convert the given string to a target integer.

    The string is read in decimal (by default) or in hexadecimal, octal or
    binary if the string begins with [0x], [0o] or [0b] respectively.

    Raise [Failure "int_of_string"] if the given string is not a valid
    representation of an integer, or if the integer represented exceeds the
    range of integers representable in type [nativeint]. *)
val of_string : string -> t

(** Return the string representation of its argument, in decimal. *)
val to_string : t -> string

(** Same as {!compare}, except that arguments are interpreted as {e unsigned}
    integers. *)
val unsigned_compare : t -> t -> int

type repr = Int32 of int32 | Int64 of int64

(** The concrete representation of a native integer. *)
val repr : t -> repr

(** Returns the smaller integer. *)
val min : t -> t -> t

(** Returns the larger integer. *)
val max : t -> t -> t

(** Extract the least significant 16 bits from the given target integer,
    exchange the order of the two bytes extracted, then form a new target
    integer by zero-extending those two bytes. *)
val get_least_significant_16_bits_then_byte_swap : t -> t

val swap_byte_endianness : t -> t

include Container_types.S with type t := t

module Targetint_set = Set

module Pair : sig
  type nonrec t = t * t

  include Container_types.S with type t := t
end

val cross_product : Set.t -> Set.t -> Pair.Set.t
