(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                        Max Slater, Jane Street                         *)
(*                                                                        *)
(*   Copyright 2013--2024 OCamlPro SAS                                    *)
(*   Copyright 2014--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** IEE754 32-bit floats represented as 32-bit integers.
    Operations are implemented in C stubs. *)

type t

val neg : t -> t

val abs : t -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val mod_ : t -> t -> t

val compare : t -> t -> int

val equal : t -> t -> bool

(** Parse 32-bit float literal. *)
val of_string : string -> t

(** Convert from a 64-bit float; rounds to the nearest 32-bit float. *)
val of_float : float -> t

(** Convert to a 64-bit float; exact. *)
val to_float : t -> float

(** Bit-cast to 32-bit integer. *)
val to_bits : t -> int32

(** Bit-cast from 32-bit integer. *)
val of_bits : int32 -> t
