(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                         The OCaml programmers                          *)
(*                 Jacob Van Buren, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2018 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Signed 8-bit integer values.

    These integers are {8} bits wide and use two's complement
    representation. All operations are taken modulo
    2{^8}. They do not fail on overflow. *)

(** {1:ints 8-bit Integers} *)

(** The type for 8-bit integer values. *)
type t = int8

(** [int_size] is the number of bits in an integer (i.e., 8). *)
val int_size : int

(** [zero] is the integer [0]. *)
val zero : int8

(** [one] is the integer [1]. *)
val one : int8

(** [minus_one] is the integer [-1]. *)
val minus_one : int8

(** [neg x] is [~-x]. *)
val neg : int8 -> int8

(** [add x y] is the addition [x + y]. *)
val add : int8 -> int8 -> int8

(** [sub x y] is the subtraction [x - y]. *)
val sub : int8 -> int8 -> int8

(** [mul x y] is the multiplication [x * y]. *)
val mul : int8 -> int8 -> int8

(** [div x y] is the division [x / y]. See {!Stdlib.( / )} for details. *)
val div : int8 -> int8 -> int8

(** [rem x y] is the remainder [x mod y]. See {!Stdlib.( mod )} for details. *)
val rem : int8 -> int8 -> int8

(** [succ x] is [add x 1]. *)
val succ : int8 -> int8

(** [pred x] is [sub x 1]. *)
val pred : int8 -> int8

(** [abs x] is the absolute value of [x]. That is [x] if [x] is positive
    and [neg x] if [x] is negative. {b Warning.} This may be negative if
    the argument is {!min_int}. *)
val abs : int8 -> int8

(** [max_int] is the greatest representable integer,
    [2{^[8 - 1]} - 1]. *)
val max_int : int8

(** [min_int] is the smallest representable integer,
    [-2{^[8 - 1]}]. *)
val min_int : int8

(** [logand x y] is the bitwise logical and of [x] and [y]. *)
val logand : int8 -> int8 -> int8

(** [logor x y] is the bitwise logical or of [x] and [y]. *)
val logor : int8 -> int8 -> int8

(** [logxor x y] is the bitwise logical exclusive or of [x] and [y]. *)
val logxor : int8 -> int8 -> int8

(** [lognot x] is the bitwise logical negation of [x]. *)
val lognot : int8 -> int8

(** [shift_left x n] shifts [x] to the left by [n] bits. The result
    is unspecified if [n < 0] or [n > ]{!8}. *)
val shift_left : int8 -> int -> int8

(** [shift_right x n] shifts [x] to the right by [n] bits. This is an
    arithmetic shift: the sign bit of [x] is replicated and inserted
    in the vacated bits. The result is unspecified if [n < 0] or
    [n > ]{!8}. *)
val shift_right : int8 -> int -> int8

(** [shift_right x n] shifts [x] to the right by [n] bits. This is a
    logical shift: zeroes are inserted in the vacated bits regardless
    of the sign of [x]. The result is unspecified if [n < 0] or
    [n > ]{!8}. *)
val shift_right_logical : int8 -> int -> int8

(** {1:preds Predicates and comparisons} *)

(** [equal x y] is [true] if and only if [x = y]. *)
val equal : int8 -> int8 -> bool

(** [compare x y] is {!Stdlib.compare}[ x y] but more efficient. *)
val compare : int8 -> int8 -> int

(** Return the smaller of the two arguments. *)
val min : int8 -> int8 -> int8

(** Return the greater of the two arguments. *)
val max : int8 -> int8 -> int8

(** {1:convert Converting} *)

(** [to_int x] is [x] as an {!int}. *)
val to_int : int8 -> int

(** [of_int x] represents [x] as an 8-bit integer. *)
val of_int : int -> int8

(** [to_float x] is [x] as a floating point number. *)
val to_float : int8 -> float

(** [of_float x] truncates [x] to an integer. The result is
    unspecified if the argument is [nan] or falls outside the range of
    representable integers. *)
val of_float : float -> int8

(** [to_string x] is the written representation of [x] in decimal. *)
val to_string : int8 -> string
