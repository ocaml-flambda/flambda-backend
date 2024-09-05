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
  | Reachable of Mach.trap_stack

type 'a static_handler =
  { regs : Reg.t array list;
    traps_ref : trap_stack_info ref;
    extra : 'a
  }

type 'a environment =
  { vars :
      (Reg.t array * Backend_var.Provenance.t option * Asttypes.mutable_flag)
      Backend_var.Map.t;
    static_exceptions : 'a static_handler Numbers.Int.Map.t;
        (** Which registers must be populated when jumping to the given
          handler. *)
    trap_stack : Mach.trap_stack
  }

val env_add :
  ?mut:Asttypes.mutable_flag ->
  Backend_var.With_provenance.t ->
  Reg.t array ->
  'a environment ->
  'a environment

val env_add_static_exception :
  Lambda.static_label ->
  Reg.t array list ->
  'a environment ->
  'a ->
  'a environment * trap_stack_info ref

val env_find : Backend_var.t -> 'a environment -> Reg.t array

val env_find_mut :
  Backend_var.t ->
  'a environment ->
  Reg.t array * Backend_var.Provenance.t option

val env_find_static_exception :
  Lambda.static_label -> 'a environment -> 'a static_handler

val env_enter_trywith :
  'a environment -> Cmm.trywith_shared_label -> 'a -> 'a environment

val env_set_trap_stack : 'a environment -> Mach.trap_stack -> 'a environment

val set_traps :
  Lambda.static_label ->
  trap_stack_info ref ->
  Mach.trap_stack ->
  Cmm.trap_action list ->
  unit

val set_traps_for_raise : 'a environment -> unit

val trap_stack_is_empty : 'a environment -> bool

val pop_all_traps : 'a environment -> Cmm.trap_action list

val env_empty : 'a environment

val size_component : Cmm.machtype_component -> int

val size_expr : 'a environment -> Cmm.expression -> int

val select_mutable_flag : Asttypes.mutable_flag -> Mach.mutable_flag

val oper_result_type : Cmm.operation -> Cmm.machtype

val swap_intcomp : Mach.integer_comparison -> Mach.integer_comparison

val all_regs_anonymous : Reg.t array -> bool

val name_regs : Backend_var.With_provenance.t -> Reg.t array -> unit

val join :
  'a ->
  Reg.t array option ->
  (< insert_debug :
       'a ->
       Mach.instruction_desc ->
       Debuginfo.t ->
       'c array ->
       'd array ->
       unit
   ; insert_move : 'a -> Reg.t -> Reg.t -> unit
   ; .. >
   as
   'b) ->
  Reg.t array option ->
  'b ->
  bound_name:Backend_var.With_provenance.t option ->
  Reg.t array option

val join_array :
  'a ->
  (Reg.t array option
  * < insert_debug :
        'a ->
        Mach.instruction_desc ->
        Debuginfo.t ->
        'b array ->
        'c array ->
        unit
    ; insert_moves : 'a -> Reg.t array -> Reg.t array -> unit
    ; .. >)
  array ->
  bound_name:Backend_var.With_provenance.t option ->
  Reg.t array option

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

class virtual ['env, 'op, 'instr] common_selector :
  object ('self)
    method virtual is_store : 'op -> bool

    method virtual lift_op : 'op -> 'instr

    method virtual make_store :
      Cmm.memory_chunk -> Arch.addressing_mode -> bool -> 'instr

    method virtual make_stack_offset : int -> 'instr

    method virtual make_name_for_debugger :
      ident:Backend_var.t ->
      which_parameter:int option ->
      provenance:Backend_var.Provenance.t option ->
      is_assignment:bool ->
      regs:Reg.t array ->
      'instr

    method virtual make_const_int : nativeint -> 'op

    method virtual make_const_float32 : int32 -> 'op

    method virtual make_const_float : int64 -> 'op

    method virtual make_const_vec128 : Cmm.vec128_bits -> 'op

    method virtual make_const_symbol : Cmm.symbol -> 'op

    method virtual make_opaque : unit -> 'op

    method is_simple_expr : Cmm.expression -> bool

    method effects_of : Cmm.expression -> Effect_and_coeffect.t

    method is_immediate : Mach.integer_operation -> int -> bool

    method virtual is_immediate_test : Mach.integer_comparison -> int -> bool

    method virtual select_addressing :
      Cmm.memory_chunk ->
      Cmm.expression ->
      Arch.addressing_mode * Cmm.expression

    method virtual select_store :
      bool -> Arch.addressing_mode -> Cmm.expression -> 'op * Cmm.expression

    method select_condition : Cmm.expression -> Mach.test * Cmm.expression

    method regs_for : Cmm.machtype -> Reg.t array

    method virtual insert_debug :
      'env environment ->
      'instr ->
      Debuginfo.t ->
      Reg.t array ->
      Reg.t array ->
      unit

    method virtual insert :
      'env environment -> 'instr -> Reg.t array -> Reg.t array -> unit

    method virtual insert_move : 'env environment -> Reg.t -> Reg.t -> unit

    method insert_moves : 'env environment -> Reg.t array -> Reg.t array -> unit

    method insert_move_args :
      'env environment -> Reg.t array -> Reg.t array -> int -> unit

    method insert_move_results :
      'env environment -> Reg.t array -> Reg.t array -> int -> unit

    method insert_op_debug :
      'env environment ->
      'op ->
      Debuginfo.t ->
      Reg.t array ->
      Reg.t array ->
      Reg.t array

    method insert_op :
      'env environment -> 'op -> Reg.t array -> Reg.t array -> Reg.t array

    method emit_expr :
      'env environment ->
      Cmm.expression ->
      bound_name:Backend_var.With_provenance.t option ->
      Reg.t array option

    method private bind_let :
      'env environment ->
      Backend_var.With_provenance.t ->
      Reg.t array ->
      'env environment

    method private bind_let_mut :
      'env environment ->
      Backend_var.With_provenance.t ->
      Cmm.machtype ->
      Reg.t array ->
      'env environment

    method private emit_parts :
      'env environment ->
      effects_after:Effect_and_coeffect.t ->
      Cmm.expression ->
      (Cmm.expression * 'env environment) option

    method private emit_parts_list :
      'env environment ->
      Cmm.expression list ->
      (Cmm.expression list * 'env environment) option

    method private emit_tuple_not_flattened :
      'env environment -> Cmm.expression list -> Reg.t array list

    method private emit_tuple :
      'env environment -> Cmm.expression list -> Reg.t array

    method emit_extcall_args :
      'env environment ->
      Cmm.exttype list ->
      Cmm.expression list ->
      Reg.t array * int

    method insert_move_extcall_arg :
      'env environment -> Cmm.exttype -> Reg.t array -> Reg.t array -> unit

    method emit_stores :
      'env environment ->
      Debuginfo.t ->
      Cmm.expression list ->
      Reg.t array ->
      unit

    method emit_expr :
      'env environment ->
      Cmm.expression ->
      bound_name:Backend_var.With_provenance.t option ->
      Reg.t array option

    method emit_expr_aux :
      'env environment ->
      Cmm.expression ->
      bound_name:Backend_var.With_provenance.t option ->
      Reg.t array option

    method virtual emit_expr_aux_raise :
      'env environment ->
      Lambda.raise_kind ->
      Cmm.expression ->
      Debuginfo.t ->
      Reg.t array option

    method virtual emit_expr_aux_op :
      'env environment ->
      Backend_var.With_provenance.t option ->
      Cmm.operation ->
      Cmm.expression list ->
      Debuginfo.t ->
      Reg.t array option

    method virtual emit_expr_aux_ifthenelse :
      'env environment ->
      Backend_var.With_provenance.t option ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.kind_for_unboxing ->
      Reg.t array option

    method virtual emit_expr_aux_switch :
      'env environment ->
      Backend_var.With_provenance.t option ->
      Cmm.expression ->
      int array ->
      (Cmm.expression * Debuginfo.t) array ->
      Debuginfo.t ->
      Cmm.kind_for_unboxing ->
      Reg.t array option

    method virtual emit_expr_aux_catch :
      'env environment ->
      Backend_var.With_provenance.t option ->
      Cmm.rec_flag ->
      (Lambda.static_label
      * (Backend_var.With_provenance.t * Cmm.machtype) list
      * Cmm.expression
      * Debuginfo.t
      * bool)
      list ->
      Cmm.expression ->
      Cmm.kind_for_unboxing ->
      Reg.t array option

    method virtual emit_expr_aux_exit :
      'env environment ->
      Cmm.exit_label ->
      Cmm.expression list ->
      Cmm.trap_action list ->
      Reg.t array option

    method virtual emit_expr_aux_trywith :
      'env environment ->
      Backend_var.With_provenance.t option ->
      Cmm.expression ->
      Cmm.trywith_shared_label ->
      Backend_var.With_provenance.t ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.kind_for_unboxing ->
      Reg.t array option

    method emit_tail : 'env environment -> Cmm.expression -> unit

    method virtual emit_tail_apply :
      'env environment ->
      Cmm.machtype ->
      Cmm.operation ->
      Cmm.expression list ->
      Debuginfo.t ->
      unit

    method virtual emit_tail_ifthenelse :
      'env environment ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.kind_for_unboxing ->
      unit

    method virtual emit_tail_switch :
      'env environment ->
      Cmm.expression ->
      int array ->
      (Cmm.expression * Debuginfo.t) array ->
      Debuginfo.t ->
      Cmm.kind_for_unboxing ->
      unit

    method virtual emit_tail_catch :
      'env environment ->
      Cmm.rec_flag ->
      (Lambda.static_label
      * (Backend_var.With_provenance.t * Cmm.machtype) list
      * Cmm.expression
      * Debuginfo.t
      * bool)
      list ->
      Cmm.expression ->
      Cmm.kind_for_unboxing ->
      unit

    method virtual emit_tail_trywith :
      'env environment ->
      Cmm.expression ->
      Cmm.trywith_shared_label ->
      Backend_var.With_provenance.t ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.kind_for_unboxing ->
      unit

    method virtual emit_return :
      'env environment -> Cmm.expression -> Cmm.trap_action list -> unit
  end
