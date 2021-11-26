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

(** Operations using the semantics of the type "int" on the target machine. That
    is to say, 31-bit arithmetic on 32-bit targets; and 63-bit arithmetic on
    64-bit targets. *)

(* CR mshinwell: Remove this [Imm] module *)
module Imm : sig
  type t

  (* CR mshinwell: Maybe this should move somewhere else let max_array_length =
     max_wosize () let max_string_length = word_size / 8 * max_array_length -
     1 *)

  (** The minimum ocaml integer representable on the target. *)
  val min_value : t

  (** The maximum ocaml integer representable on the target. *)
  val max_value : t

  (** The maximum string length on the target. *)
  val max_string_length : t

  (** The ocaml target integer -1 *)
  val minus_one : t

  (** The ocaml target integer 0. *)
  val zero : t

  (** The ocaml target integer 1. *)
  val one : t

  (** The ocaml target integer 10. *)
  val ten : t

  (** The ocaml target integer 0xff. *)
  val hex_ff : t

  val ( <= ) : t -> t -> bool

  (** Comparisons functions on ocaml target integers. *)
  val ( < ) : t -> t -> bool

  (** Returns the 8 least significant bits of the ocaml target integer as a host
      caml integer (cannot overflow). *)
  val bottom_byte_to_int : t -> int

  (** Returns the ocaml target integer corresponding to the ASCII code of the
      given character. *)
  val of_char : char -> t

  (** Convert the given integer (type [int]) to a target ocaml integer (type
      [t]), modulo the target word size minus one (for the tag bit). *)
  val of_int : int -> t

  (** Returns [None] iff the given [int] cannot be represented as a target
      "int"-width integer, else returns the same as {!of_int}. *)
  val of_int_option : int -> t option

  (** Convert the given target integer (type [t]) to an integer (type [int]),
      modulo the [int] size, i.e. high-order bits are lost during the
      conversion. *)
  val to_int : t -> int

  (** Convert the given ocaml target integer (type [t]) to an integer (type
      [int]). Returns [None] if the original ocaml target integer does not fit
      into an integer *)
  val to_int_option : t -> int option

  (** Convert the given ocaml target integer (type [t]) to an integer (type
      [int]).

      @raise Fatal_error if the original ocaml target integer does not fit into
      an integer *)
  val to_int_exn : t -> int

  (** Convert the given 32-bit integer (type [int32]) to a target ocaml integer,
      modulo the size of a target ocaml integer. *)
  val of_int32 : int32 -> t

  (** Convert the given ocaml target integer to a 32-bit integer (type [int32]).
      On 64-bit platforms, the 64-bit native integer is taken modulo 2{^ 32},
      i.e. the top 32 bits are lost. On 32-bit platforms, the conversion is
      exact. *)
  val to_int32 : t -> int32

  (** Convert the given 64-bit integer (type [int64]) to a target native
      integer, modulo the size of a target ocaml integer. *)
  val of_int64 : int64 -> t

  (** Convert the given ocaml target integer to a 64-bit integer (type
      [int64]). *)
  val to_int64 : t -> int64

  (** Convert the given target native integer (type [Targetint_32_64.t]) to an
      ocaml target integer, modulo the size of an ocaml target integer. *)
  val of_targetint : Targetint_32_64.t -> t

  (** Convert the given ocaml target integer (type [t]) to a target native
      integer (type [Targetint_32_64.t]). *)
  val to_targetint : t -> Targetint_32_64.t

  (** Convert the given floating-point number to an ocaml target integer,
      discarding the fractional part (truncate towards 0). The result of the
      conversion is undefined if, after truncation, the number is outside the
      range \[{!Targetint_31_63.Imm.min_value},
      {!Targetint_31_63.Imm.max_value}\]. *)
  val of_float : float -> t

  (** Convert the given target integer to a floating-point number. *)
  val to_float : t -> float

  (** Unary negation. *)
  val neg : t -> t

  (** Extract the least significant 16 bits from the given ocaml target integer,
      exchange the order of the two bytes extracted, then form a new target
      integer by zero-extending those two bytes. *)
  val get_least_significant_16_bits_then_byte_swap : t -> t

  (** Addition. *)
  val add : t -> t -> t

  (** Subtraction. *)
  val sub : t -> t -> t

  (** Multiplication. *)
  val mul : t -> t -> t

  val mod_ : t -> t -> t

  (** Integer division and modulo. Raise [Division_by_zero] if the second
      argument is zero. This division rounds the real quotient of its arguments
      towards zero, as specified for {!Stdlib.(/)}. *)
  val div : t -> t -> t

  (** Bitwise logical and. *)
  val and_ : t -> t -> t

  (** Bitwise logical or. *)
  val or_ : t -> t -> t

  (** Bitwise logical exclusive or. *)
  val xor : t -> t -> t

  (** [shift_left x y] shifts [x] to the left by [y] bits. The result is
      unspecified if [y < 0] or [y >= bitsize], where [bitsize] is [31] on a
      32-bit platform and [61] on a 64-bit platform. *)
  val shift_left : t -> int -> t

  (** [Targetint_32_64.shift_right x y] shifts [x] to the right by [y] bits.
      This is an arithmetic shift: the sign bit of [x] is replicated and
      inserted in the vacated bits. The result is unspecified if [y < 0] or [y
      >= bitsize]. *)
  val shift_right : t -> int -> t

  (** [Targetint_32_64.shift_right_logical x y] shifts [x] to the right by [y]
      bits. This is a logical shift: zeroes are inserted in the vacated bits
      regardless of the sign of [x]. The result is unspecified if [y < 0] or [y
      >= bitsize]. *)
  val shift_right_logical : t -> int -> t

  (** Returns the smaller integer. *)
  val min : t -> t -> t

  (** Returns the larger integer. *)
  val max : t -> t -> t

  (* CR mshinwell: Add an [Array] module *)

  include Container_types.S with type t := t

  val to_string : t -> string
end

type 'a or_wrong = private
  | Ok of 'a
  | Wrong

type t = private
  { value : Imm.t;
    print_as_char : bool
  }

type immediate = t

(** The comparison function for type [t] ignores [print_as_char]. *)
include Container_types.S with type t := t

module Lmap : Lmap.S with type key := t

val one : t

val zero : t

val minus_one : t

val join : t -> t -> t or_wrong

val join_set : Set.t -> Set.t -> Set.t

val bool_true : t

val bool_false : t

val bool : bool -> t

val int : Imm.t -> t

val char : char -> t

val map : t -> f:(Imm.t -> Imm.t) -> t

val is_non_negative : t -> bool

(* CR mshinwell: bad names *)
val to_targetint : t -> Imm.t

val to_targetint' : t -> Targetint_32_64.t

val set_to_targetint_set : Set.t -> Imm.Set.t

val set_to_targetint_set' : Set.t -> Targetint_32_64.Set.t

val neg : t -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val mod_ : t -> t -> t

val and_ : t -> t -> t

val or_ : t -> t -> t

val xor : t -> t -> t

val shift_left : t -> int -> t

val shift_right : t -> int -> t

val shift_right_logical : t -> int -> t

val get_least_significant_16_bits_then_byte_swap : t -> t

(** The set consisting of the representations of constant [true] and [false]. *)
val all_bools : Set.t

val zero_one_and_minus_one : Set.t

module Pair : sig
  type nonrec t = t * t

  include Container_types.S with type t := t
end

val cross_product : Set.t -> Set.t -> Pair.Set.t
