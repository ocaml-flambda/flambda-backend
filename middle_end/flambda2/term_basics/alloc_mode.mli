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

module For_types : sig
  (** Constraints on the location of allocated values *)
  type t = private
    | Heap  (** Normal allocation on the OCaml heap. *)
    | Local  (** Allocation on the local allocation stack. *)
    | Heap_or_local  (** Allocation with unknown location *)

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val heap : t

  (** Returns [Heap] if stack allocation is disabled! *)
  val local : unit -> t

  (** Returns [Heap] if stack allocation is disabled! *)
  val unknown : unit -> t

  (** Maps [Alloc_local] to [Heap_or_local], as all Lambda annotations that we
      transform into constraints have this semantics *)
  val from_lambda : Lambda.alloc_mode -> t

  (** Symmetric to [from_lambda], so [Heap_or_local] is mapped to [Alloc_local] *)
  val to_lambda : t -> Lambda.alloc_mode
end

module For_applications : sig
  (** Decisions on allocation locations for application expressions. *)
  type t = private
    | Heap  (** Normal allocation on the OCaml heap. *)
    | Local of
        { region : Variable.t;
          ghost_region : Variable.t
        }  (** Allocation on the local allocation stack in the given region. *)

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val heap : t

  (** Returns [Heap] if stack allocation is disabled! *)
  val local : region:Variable.t -> ghost_region:Variable.t -> t

  val as_type : t -> For_types.t

  val from_lambda :
    Lambda.alloc_mode ->
    current_region:Variable.t ->
    current_ghost_region:Variable.t ->
    t

  val to_lambda : t -> Lambda.alloc_mode

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t
end

module For_allocations : sig
  (** Decisions on allocation locations *)
  type t = private
    | Heap  (** Normal allocation on the OCaml heap. *)
    | Local of { region : Variable.t }
        (** Allocation on the local allocation stack in the given region. *)

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val heap : t

  (** Returns [Heap] if stack allocation is disabled! *)
  val local : region:Variable.t -> t

  val as_type : t -> For_types.t

  val from_lambda : Lambda.alloc_mode -> current_region:Variable.t -> t

  val to_lambda : t -> Lambda.alloc_mode

  include Contains_names.S with type t := t

  include Contains_ids.S with type t := t
end

module For_assignments : sig
  (** Decisions on assignment locations *)
  type t = private
    | Heap
    | Local

  val print : Format.formatter -> t -> unit

  val compare : t -> t -> int

  val heap : t

  (* Returns [Heap] if stack allocation is disabled! *)
  val local : unit -> t

  val from_lambda : Lambda.modify_mode -> t

  val to_lambda : t -> Lambda.modify_mode
end
