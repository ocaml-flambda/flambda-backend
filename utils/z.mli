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

(** Arbitrary-precision arithmetic *)

type t

val compare : t -> t -> int

val equal : t -> t -> bool

val zero : t

val one : t

val minus_one : t

val succ : t -> t

val pred : t -> t

val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t

val rem : t -> t -> t

val neg : t -> t

val sign_extend : t -> bits:int -> t

val zero_extend : t -> bits:int -> t

val byteswap : t -> bytes:int -> t

val logand : t -> t -> t

val logor : t -> t -> t

val logxor : t -> t -> t

val lognot : t -> t

val min_int : ?unsigned:bool -> unit -> bits:int -> t

val max_int : ?unsigned:bool -> unit -> bits:int -> t

val to_string : t -> string

val of_string : string -> t

val of_int : ?unsigned:bool -> int -> t

val to_int : t -> int

val to_int_exn : ?unsigned:bool -> t -> int

val of_nativeint : ?unsigned:bool -> nativeint -> t

val to_nativeint : t -> nativeint

val to_nativeint_exn : ?unsigned:bool -> t -> nativeint

val of_int64 : ?unsigned:bool -> int64 -> t

val sign : t -> int

val of_float : float -> t

val to_float : t -> float
