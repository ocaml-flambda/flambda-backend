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

(** The type for 16-bit integer values. *)
type t = int16 [@@immediate]

(** @inline *)
include Int.S with type t := int16
