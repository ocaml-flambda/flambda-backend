(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Augmented version of [Shape.Uid.t] that can track variables forming parts
    of unboxed products. *)

type t = private
  | Uid of Shape.Uid.t
  | Proj of Shape.Uid.t * int

val internal_not_actually_unique : t

val uid : Shape.Uid.t -> t

val proj : Shape.Uid.t -> field:int -> t

include Identifiable.S with type t := t
