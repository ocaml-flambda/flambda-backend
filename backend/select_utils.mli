(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

type trap_stack_info =
  | Unreachable
  | Reachable of Simple_operation.trap_stack

type static_handler =
  { regs : Reg.t array list;
    traps_ref : trap_stack_info ref;
    label : Label.t
  }

type environment =
  { vars :
      (Reg.t array * Backend_var.Provenance.t option * Asttypes.mutable_flag)
      Backend_var.Map.t;
    static_exceptions : static_handler Numbers.Int.Map.t;
        (** Which registers must be populated when jumping to the given
          handler. *)
    trap_stack : Simple_operation.trap_stack;
    regs_for_exception_extra_args : Reg.t array Numbers.Int.Map.t
  }

val env_add :
  ?mut:Asttypes.mutable_flag ->
  Backend_var.With_provenance.t ->
  Reg.t array ->
  environment ->
  environment

val env_add_static_exception :
  Lambda.static_label ->
  Reg.t array list ->
  environment ->
  Label.t ->
  environment * trap_stack_info ref

val env_find : Backend_var.t -> environment -> Reg.t array

val env_find_mut :
  Backend_var.t -> environment -> Reg.t array * Backend_var.Provenance.t option

val env_find_static_exception :
  Lambda.static_label -> environment -> static_handler

val env_enter_trywith :
  environment -> Cmm.trywith_shared_label -> Label.t -> environment

val env_add_regs_for_exception_extra_args :
  Cmm.trywith_shared_label -> Reg.t array -> environment -> environment

val env_find_regs_for_exception_extra_args :
  Cmm.trywith_shared_label -> environment -> Reg.t array

val env_set_trap_stack :
  environment -> Simple_operation.trap_stack -> environment

val set_traps :
  Lambda.static_label ->
  trap_stack_info ref ->
  Simple_operation.trap_stack ->
  Cmm.trap_action list ->
  unit

val set_traps_for_raise : environment -> unit

val trap_stack_is_empty : environment -> bool

val pop_all_traps : environment -> Cmm.trap_action list

val env_empty : environment

val size_component : Cmm.machtype_component -> int

val size_expr : environment -> Cmm.expression -> int

val select_mutable_flag : Asttypes.mutable_flag -> Simple_operation.mutable_flag

val oper_result_type : Cmm.operation -> Cmm.machtype

val swap_intcomp :
  Simple_operation.integer_comparison -> Simple_operation.integer_comparison

val name_regs : Backend_var.With_provenance.t -> Reg.t array -> unit

val current_function_name : string ref

val current_function_is_check_enabled : bool ref

module Effect : sig
  type t =
    | None
    | Raise
    | Arbitrary
end

module Coeffect : sig
  type t =
    | None
    | Read_mutable
    | Arbitrary
end

module Effect_and_coeffect : sig
  type t

  val none : t

  val arbitrary : t

  val effect : t -> Effect.t

  val coeffect : t -> Coeffect.t

  val pure_and_copure : t -> bool

  val effect_only : Effect.t -> t

  val coeffect_only : Coeffect.t -> t

  val create : Effect.t -> Coeffect.t -> t

  val join : t -> t -> t

  val join_list_map : 'a list -> ('a -> t) -> t
end

val select_effects : Cmm.effects -> Effect.t

val select_coeffects : Cmm.coeffects -> Coeffect.t
