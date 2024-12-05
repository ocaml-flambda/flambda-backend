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
val zero : t

(** [one] is the integer [1]. *)
val one : t

(** [minus_one] is the integer [-1]. *)
val minus_one : t

(** [neg x] is [~-x]. *)
val neg : t -> t

(** [add x y] is the addition [x + y]. *)
val add : t -> t -> t

(** [sub x y] is the subtraction [x - y]. *)
val sub : t -> t -> t

(** [mul x y] is the multiplication [x * y]. *)
val mul : t -> t -> t

(** [div x y] is the division [x / y]. See {!Stdlib.( / )} for details. *)
val div : t -> t -> t

(** [rem x y] is the remainder [x mod y]. See {!Stdlib.( mod )} for details. *)
val rem : t -> t -> t

(** [succ x] is [add x 1]. *)
val succ : t -> t

(** [pred x] is [sub x 1]. *)
val pred : t -> t

(** [abs x] is the absolute value of [x]. That is [x] if [x] is positive
    and [neg x] if [x] is negative. {b Warning.} This may be negative if
    the argument is {!min_int}. *)
val abs : t -> t

(** [max_int] is the greatest representable integer,
    [2{^[8 - 1]} - 1]. *)
val max_int : t

(** [min_int] is the smallest representable integer,
    [-2{^[8 - 1]}]. *)
val min_int : t

(** [logand x y] is the bitwise logical and of [x] and [y]. *)
val logand : t -> t -> t

(** [logor x y] is the bitwise logical or of [x] and [y]. *)
val logor : t -> t -> t

(** [logxor x y] is the bitwise logical exclusive or of [x] and [y]. *)
val logxor : t -> t -> t

(** [lognot x] is the bitwise logical negation of [x]. *)
val lognot : t -> t

(** [shift_left x n] shifts [x] to the left by [n] bits. The result
    is unspecified if [n < 0] or [n > ]{!8}. *)
val shift_left : t -> int -> t

(** [shift_right x n] shifts [x] to the right by [n] bits. This is an
    arithmetic shift: the sign bit of [x] is replicated and inserted
    in the vacated bits. The result is unspecified if [n < 0] or
    [n > ]{!8}. *)
val shift_right : t -> int -> t

(** [shift_right x n] shifts [x] to the right by [n] bits. This is a
    logical shift: zeroes are inserted in the vacated bits regardless
    of the sign of [x]. The result is unspecified if [n < 0] or
    [n > ]{!8}. *)
val shift_right_logical : t -> int -> t

(** {1:preds Predicates and comparisons} *)

(** [equal x y] is [true] if and only if [x = y]. *)
val equal : t -> t -> bool

(** [compare x y] is {!Stdlib.compare}[ x y] but more efficient. *)
val compare : t -> t -> int

(** Return the smaller of the two arguments. *)
val min : t -> t -> t

(** Return the greater of the two arguments. *)
val max : t -> t -> t

(** {1:convert Converting} *)

(** [to_int x] is [x] as an {!int}. *)
val to_int : t -> int

(** [of_int x] represents [x] as an 8-bit integer. *)
val of_int : int -> t

(** [to_float x] is [x] as a floating point number. *)
val to_float : t -> float

(** [of_float x] truncates [x] to an integer. The result is
    unspecified if the argument is [nan] or falls outside the range of
    representable integers. *)
val of_float : float -> t

(** [to_string x] is the written representation of [x] in decimal. *)
val to_string : t -> string
