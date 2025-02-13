(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Signed 16-bit integer values.

    These integers are {16} bits wide and use two's complement representation.
    All operations are taken modulo 2{^16}. They do not fail on overflow. *)

(** {1:ints 16-bit Integers} *)

(* CR layouts v5: add back all the thunked constants in this module when we
   support unboxed integers in structures. *)

(** The type for 16-bit integer values. *)
type t = int16#

(** The number of bits in an integer of type {!int16#}. *)
val size : int

(** The 16-bit integer 0. *)
val zero : unit -> int16#

(** The 16-bit integer 1. *)
val one : unit -> int16#

(** The 16-bit integer -1. *)
val minus_one : unit -> int16#

(** Unary negation. *)
external neg : int16# -> int16# = "%int16#_neg"

(** Addition. *)
external add : int16# -> int16# -> int16# = "%int16#_add"

(** Subtraction. *)
external sub : int16# -> int16# -> int16# = "%int16#_sub"

(** Multiplication. *)
external mul : int16# -> int16# -> int16# = "%int16#_mul"

(** Integer division. This division rounds the real quotient of
    its arguments towards zero, as specified for {!Stdlib.(/)}.
    @raise Division_by_zero if the second argument is zero. *)
external div : int16# -> int16# -> int16# = "%int16#_div"

(** Same as {!div}, except that arguments and result are interpreted as {e
    unsigned} integers. *)
val unsigned_div : int16# -> int16# -> int16#

(** Integer remainder. If [y] is not zero, [rem x y = sub x (mul (div x y)
    y)]. If [y] is zero, [rem x y] raises [Division_by_zero]. *)
external rem : int16# -> int16# -> int16# = "%int16#_mod"

(** Same as {!rem}, except that arguments and result are interpreted as {e
    unsigned} integers. *)
val unsigned_rem : int16# -> int16# -> int16#

(** [succ x] is [add x 1]. *)
external succ : int16# -> int16# = "%int16#_succ"

(** [pred x] is [sub x 1]. *)
external pred : int16# -> int16# = "%int16#_pred"

(** [abs x] is the absolute value of [x]. That is [x] if [x] is positive and
    [neg x] if [x] is negative. {b Warning.} This may be negative if the
    argument is {!min_int}. *)
val abs : int16# -> int16#

(** [max_int] is the greatest representable integer,
    [2{^[size - 1]} - 1]. *)
val max_int : unit -> int16#

(** [min_int] is the smallest representable integer,
    [-2{^[size - 1]}]. *)
val min_int : unit -> int16#

(** Bitwise logical and. *)
external logand : int16# -> int16# -> int16# = "%int16#_and"

(** Bitwise logical or. *)
external logor : int16# -> int16# -> int16# = "%int16#_or"

(** Bitwise logical exclusive or. *)
external logxor : int16# -> int16# -> int16# = "%int16#_xor"

(** Bitwise logical negation. *)
val lognot : int16# -> int16#

(** [shift_left x n] shifts [x] to the left by [n] bits. The result
    is unspecified if [n < 0] or [n >= ]{!size}. *)
external shift_left : int16# -> int -> int16# = "%int16#_lsl"

(** [shift_right x n] shifts [x] to the right by [n] bits. This is an
    arithmetic shift: the sign bit of [x] is replicated and inserted
    in the vacated bits. The result is unspecified if [n < 0] or
    [n >=]{!size}. *)
external shift_right : int16# -> int -> int16# = "%int16#_asr"

(** [shift_right x n] shifts [x] to the right by [n] bits. This is a
    logical shift: zeroes are inserted in the vacated bits regardless
    of the sign of [x]. The result is unspecified if [n < 0] or
    [n >=]{!size}. *)
external shift_right_logical : int16# -> int -> int16# = "%int16#_lsr"

(** {1:preds Predicates and comparisons} *)

(** [equal x y] is [true] if and only if [x = y]. *)
external equal : int16# -> int16# -> bool = "%int16#_equal"

(** [compare x y] is {!Stdlib.compare}[ x y] but more efficient. *)
external compare : int16# -> int16# -> int = "%int16#_compare"

(** Same as {!compare}, except that arguments are interpreted as {e unsigned} integers. *)
val unsigned_compare : int16# -> int16# -> int

(** Return the lesser of the two arguments. *)
val min : int16# -> int16# -> int16#

(** Return the greater of the two arguments. *)
val max : int16# -> int16# -> int16#

(** {1:convert Converting} *)

(** [to_int x] is [x] as an {!int}. *)
external to_int : int16# -> int = "%int_of_int16#"

(** [of_int x] truncates the representation of [x] to fit in {!int16#}. *)
external of_int : int -> int16# = "%int16#_of_int"

(** Same as {!to_int}, but interprets the argument as an {e unsigned} integer. *)
val unsigned_to_int : int16# -> int

(** [to_float x] is [x] as a floating point number. *)
external to_float : int16# -> float = "%float_of_int16#"

(** [of_float x] truncates [x] to an integer. The result is
    unspecified if the argument is [nan] or falls outside the range of
    representable integers. *)
external of_float : float -> int16# = "%int16#_of_float"

(** [to_string x] is the written representation of [x] in decimal. *)
val to_string : int16# -> string

(** A seeded hash function for ints, with the same output value as
    {!Hashtbl.seeded_hash}. This function allows this module to be passed as
    argument to the functor {!Hashtbl.MakeSeeded}. *)
val seeded_hash : int -> int16# -> int

(** An unseeded hash function for ints, with the same output value as
    {!Hashtbl.hash}. This function allows this module to be passed as argument
    to the functor {!Hashtbl.Make}. *)
val hash : int16# -> int
