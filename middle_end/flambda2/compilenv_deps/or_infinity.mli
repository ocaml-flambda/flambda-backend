(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type 'a t = Finite of 'a | Infinity

val compare : f:('a -> 'a -> int) -> 'a t -> 'a t -> int

val equal : f:('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val hash : f:('a -> int) -> 'a t -> int

val print :
  f:(Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
