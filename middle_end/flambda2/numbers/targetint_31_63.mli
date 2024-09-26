(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Operations using the semantics of the OCaml type "int" on the target
    machine. That is to say, 31-bit arithmetic on 32-bit targets; and 63-bit
    arithmetic on 64-bit targets. *)

type t

include Container_types.S with type t := t

(** The minimum integer representable on the target. *)
val min_value : t

(** The maximum integer representable on the target. *)
val max_value : t

(** The OCaml integer -1 *)
val minus_one : t

(** The OCaml integer 0. *)
val zero : t

(** The OCaml integer 1. *)
val one : t

(** The OCaml integer 10. *)
val ten : t

(** The OCaml integer 0xff. *)
val hex_ff : t

(** The set {-1, 0, 1}. *)
val zero_one_and_minus_one : Set.t

(** Boolean values. *)
val bool : bool -> t

val bool_true : t

val bool_false : t

val all_bools : Set.t

(** Comparison functions. *)
val ( <= ) : t -> t -> bool

val ( >= ) : t -> t -> bool

val ( < ) : t -> t -> bool

(** Returns the 8 least significant bits of the OCaml integer as a host caml
    integer (cannot overflow). *)
val bottom_byte_to_int : t -> int

(** Returns the OCaml integer corresponding to the ASCII code of the given
    character. *)
val of_char : char -> t

(** Convert the given integer (type [int]) to a OCaml integer (type [t]), modulo
    the target word size minus one (for the tag bit). *)
val of_int : int -> t

(** Returns [None] iff the given [int] cannot be represented as a target
    "int"-width integer, else returns the same as {!of_int}. *)
val of_int_option : int -> t option

(** Convert the given OCaml integer (type [t]) to an integer (type [int]),
    modulo the [int] size, i.e. high-order bits are lost during the conversion. *)
val to_int : t -> int

(** Convert the given OCaml integer (type [t]) to an integer (type [int]).
    Returns [None] if the original OCaml integer does not fit into an integer *)
val to_int_option : t -> int option

(** Convert the given OCaml integer (type [t]) to an integer (type [int]).

    @raise Fatal_error if the original OCaml integer does not fit into an
    integer *)
val to_int_exn : t -> int

(** Convert the given 32-bit integer (type [int32]) to a OCaml integer, modulo
    the size of a OCaml integer. *)
val of_int32 : int32 -> t

(** Convert the given OCaml integer to a 32-bit integer (type [int32]). On
    64-bit platforms, the 64-bit native integer is taken modulo 2{^ 32}, i.e.
    the top 32 bits are lost. On 32-bit platforms, the conversion is exact. *)
val to_int32 : t -> int32

(** Convert the given 64-bit integer (type [int64]) to a target native integer,
    modulo the size of a OCaml integer. *)
val of_int64 : int64 -> t

(** Convert the given OCaml integer to a 64-bit integer (type [int64]). *)
val to_int64 : t -> int64

(** Convert the given target native integer (type [Targetint_32_64.t]) to an
    OCaml integer, modulo the size of an OCaml integer. *)
val of_targetint : Targetint_32_64.t -> t

(** Convert the given OCaml integer (type [t]) to a target native integer (type
    [Targetint_32_64.t]). *)
val to_targetint : t -> Targetint_32_64.t

(** Convert the given floating-point number to an OCaml integer, discarding the
    fractional part (truncate towards 0). The result of the conversion is
    undefined if, after truncation, the number is outside the range
    \[{!Targetint_31_63.min_value}, {!Targetint_31_63.max_value}\]. *)
val of_float : float -> t

(** Convert the given OCaml integer to a floating-point number. *)
val to_float : t -> float

(** Unary negation. *)
val neg : t -> t

(** Extract the least significant 16 bits from the given OCaml integer, exchange
    the order of the two bytes extracted, then form a new target integer by
    zero-extending those two bytes. *)
val get_least_significant_16_bits_then_byte_swap : t -> t

(** Addition. *)
val add : t -> t -> t

(** Subtraction. *)
val sub : t -> t -> t

(** Multiplication. *)
val mul : t -> t -> t

val mod_ : t -> t -> t

(** Integer division and modulo. Raise [Division_by_zero] if the second argument
    is zero. This division rounds the real quotient of its arguments towards
    zero, as specified for {!Stdlib.(/)}. *)
val div : t -> t -> t

(** Bitwise logical and. *)
val and_ : t -> t -> t

(** Bitwise logical or. *)
val or_ : t -> t -> t

(** Bitwise logical exclusive or. *)
val xor : t -> t -> t

(** [shift_left x y] shifts [x] to the left by [y] bits. The result is
    unspecified if [y < 0] or [y >= bitsize], where [bitsize] is [31] on a
    32-bit platform and [63] on a 64-bit platform. *)
val shift_left : t -> int -> t

(** [Targetint_32_64.shift_right x y] shifts [x] to the right by [y] bits. This
    is an arithmetic shift: the sign bit of [x] is replicated and inserted in
    the vacated bits. The result is unspecified if [y < 0] or [y >= bitsize]. *)
val shift_right : t -> int -> t

(** [Targetint_32_64.shift_right_logical x y] shifts [x] to the right by [y]
    bits. This is a logical shift: zeroes are inserted in the vacated bits
    regardless of the sign of [x]. The result is unspecified if [y < 0] or [y >=
    bitsize]. *)
val shift_right_logical : t -> int -> t

(** Returns the smaller integer. *)
val min : t -> t -> t

(** Returns the larger integer. *)
val max : t -> t -> t

val is_non_negative : t -> bool

module Pair : sig
  type nonrec t = t * t

  include Container_types.S with type t := t
end

val cross_product : Set.t -> Set.t -> Pair.Set.t
