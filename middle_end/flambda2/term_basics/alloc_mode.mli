(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t = private
  | Heap  (** Normal allocation on the OCaml heap. *)
  | Local  (** Allocation on the local allocation stack. *)

type without_region = t

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

val heap : t

(** Returns [Heap] if stack allocation is disabled! *)
val local : unit -> t

val from_lambda : Lambda.alloc_mode -> t

val to_lambda : t -> Lambda.alloc_mode

module With_region : sig
  type t =
    | Heap  (** Normal allocation on the OCaml heap. *)
    | Local of { region : Variable.t }
        (** Allocation on the local allocation stack in the given region. *)

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val without_region : t -> without_region

  val from_lambda : Lambda.alloc_mode -> current_region:Variable.t -> t

  val to_lambda : t -> Lambda.alloc_mode

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t
end
