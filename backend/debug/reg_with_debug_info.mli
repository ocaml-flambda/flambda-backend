(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Registers equipped with information used for generating debugging
    information. *)

module Debug_info : sig
  type t

  val compare : t -> t -> int

  (** The identifier that the register holds (part of) the value of. *)
  val holds_value_of : t -> Backend_var.t

  val part_of_value : t -> int

  val num_parts_of_value : t -> int

  (** If the register corresponds to a function parameter, the value returned is
      the zero-based index of said parameter; otherwise it is [None]. *)
  val which_parameter : t -> int option

  val provenance : t -> Backend_var.Provenance.t option

  val is_parameter : t -> Is_parameter.t
end

type t

type reg_with_debug_info = t

val create :
  reg:Reg.t ->
  holds_value_of:Backend_var.t ->
  part_of_value:int ->
  num_parts_of_value:int ->
  which_parameter:int option ->
  provenance:Backend_var.Provenance.t option ->
  t

val create_with_debug_info : reg:Reg.t -> debug_info:Debug_info.t option -> t

val create_without_debug_info : reg:Reg.t -> t

val create_copying_debug_info : reg:Reg.t -> debug_info_from:t -> t

val reg : t -> Reg.t

val location : t -> Reg.location

val debug_info : t -> Debug_info.t option

(** [at_same_location t reg] holds iff the register [t] corresponds to the same
    (physical or pseudoregister) location as the register [reg], which is not
    equipped with debugging information. [register_class] should be
    [Proc.register_class]. *)
val at_same_location :
  t ->
  Reg.t ->
  register_class:(Reg.t -> int) ->
  stack_class:(Reg.t -> Stack_class.t) ->
  bool

val holds_pointer : t -> bool

val holds_non_pointer : t -> bool

(** [assigned_to_stack t] holds iff the location of [t] is a hard stack slot.
    (This returns false for Domainstate slots.) *)
val assigned_to_stack : t -> bool

val clear_debug_info : t -> t

module Set_distinguishing_names_and_locations : Set.S with type elt = t

module Map_distinguishing_names_and_locations : Map.S with type key = t

module Set : sig
  include Set.S with type elt = t

  val of_array : reg_with_debug_info array -> t

  val mem_reg : t -> Reg.t -> bool

  val find_reg_exn : t -> Reg.t -> reg_with_debug_info

  val filter_reg : t -> Reg.t -> t

  val forget_debug_info : t -> Reg.Set.t

  val without_debug_info : Reg.Set.t -> t

  (** [made_unavailable_by_clobber t ~regs_clobbered ~register_class] returns
      the largest subset of [t] whose locations do not overlap with any
      registers in [regs_clobbered]. (Think of [t] as a set of available
      registers.) [register_class] should always be [Proc.register_class]. *)
  val made_unavailable_by_clobber :
    t ->
    regs_clobbered:Reg.t array ->
    register_class:(Reg.t -> int) ->
    stack_class:(Reg.t -> Stack_class.t) ->
    t
end

val print :
  print_reg:(Format.formatter -> Reg.t -> unit) -> Format.formatter -> t -> unit

val compare : t -> t -> int
