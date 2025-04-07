(* -------------------------------------------------------------------------- *
 *                               MIT License                                  *
 *                                                                            *
 * Copyright (c) 2025 Jane Street Group LLC                                   *
 * opensource-contacts@janestreet.com                                         *
 *                                                                            *
 * Permission is hereby granted, free of charge, to any person obtaining a    *
 * copy of this software and associated documentation files (the "Software"), *
 * to deal in the Software without restriction, including without limitation  *
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,   *
 * and/or sell copies of the Software, and to permit persons to whom the      *
 * Software is furnished to do so, subject to the following conditions:       *
 *                                                                            *
 * The above copyright notice and this permission notice shall be included    *
 * in all copies or substantial portions of the Software.                     *
 *                                                                            *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL    *
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING    *
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER        *
 * DEALINGS IN THE SOFTWARE.                                                  *
 ******************************************************************************)

module DLL = Flambda_backend_utils.Doubly_linked_list
module Int = Numbers.Int
module V = Backend_var
module VP = Backend_var.With_provenance

type trap_stack_info =
  | Unreachable
  | Reachable of Operation.trap_stack

type static_handler =
  { regs : Reg.t array list;
    traps_ref : trap_stack_info ref;
    label : Label.t
  }

type environment =
  { vars :
      (Reg.t array * V.Provenance.t option * Asttypes.mutable_flag) V.Map.t;
    static_exceptions : static_handler Int.Map.t;
    trap_stack : Operation.trap_stack;
    tailrec_label : Label.t
  }

val env_create : tailrec_label:Label.t -> environment

val env_add :
  ?mut:Asttypes.mutable_flag ->
  VP.t ->
  Reg.t array ->
  environment ->
  environment

val env_add_static_exception :
  Int.Map.key ->
  Reg.t array list ->
  environment ->
  Label.t ->
  environment * trap_stack_info ref

val env_find : V.Map.key -> environment -> Reg.t array

val env_find_mut :
  V.Map.key -> environment -> Reg.t array * Backend_var.Provenance.t option

val env_find_regs_for_exception_extra_args :
  Cmm.trywith_shared_label -> environment -> Reg.t array list

val env_find_static_exception : Int.Map.key -> environment -> static_handler

val env_set_trap_stack : environment -> Operation.trap_stack -> environment

val print_traps : Format.formatter -> Operation.trap_stack -> unit

val set_traps :
  Lambda.static_label ->
  trap_stack_info ref ->
  Operation.trap_stack ->
  Cmm.trap_action list ->
  unit

val set_traps_for_raise : environment -> unit

val trap_stack_is_empty : environment -> bool

val pop_all_traps : environment -> Cmm.trap_action list

val select_mutable_flag : Asttypes.mutable_flag -> Operation.mutable_flag

val oper_result_type : Cmm.operation -> Cmm.machtype

val size_component : Cmx_format.machtype_component -> int

val size_machtype : Cmx_format.machtype_component array -> int

val size_expr : environment -> Cmm.expression -> int

val swap_intcomp : Operation.integer_comparison -> Operation.integer_comparison

val name_regs : VP.t -> Reg.t array -> unit

val current_function_name : string ref

val current_function_is_check_enabled : bool ref

module Effect : sig
  type t =
    | None
    | Raise
    | Arbitrary

  val join : t -> t -> t

  val pure : t -> bool
end

module Coeffect : sig
  type t =
    | None
    | Read_mutable
    | Arbitrary

  val join : t -> t -> t

  val copure : t -> bool
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

module Or_never_returns : sig
  type 'a t =
    | Ok of 'a
    | Never_returns

  module Syntax : sig
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t

    val ( let** ) : 'a t -> ('a -> unit) -> unit
  end
end

val debug : bool

val float_test_of_float_comparison :
  Cmm.float_width ->
  Lambda.float_comparison ->
  label_false:Label.t ->
  label_true:Label.t ->
  Cfg.float_test

val int_test_of_integer_comparison :
  Lambda.integer_comparison ->
  signed:bool ->
  immediate:int option ->
  label_false:Label.t ->
  label_true:Label.t ->
  Cfg.int_test

val terminator_of_test :
  Operation.test -> label_false:Label.t -> label_true:Label.t -> Cfg.terminator

val invalid_stack_offset : int

module Stack_offset_and_exn : sig
  type handler_stack = Label.t list

  val compute_stack_offset : stack_offset:int -> traps:'a list -> int

  val check_and_set_stack_offset :
    'a Cfg.instruction -> stack_offset:int -> traps:handler_stack -> unit

  val process_terminator :
    stack_offset:int ->
    traps:handler_stack ->
    Cfg.terminator Cfg.instruction ->
    int * handler_stack

  val process_basic :
    Cfg.t ->
    stack_offset:int ->
    traps:handler_stack ->
    Cfg.basic Cfg.instruction ->
    int * handler_stack

  val update_block :
    Cfg.t -> Label.t -> stack_offset:int -> traps:handler_stack -> unit

  val update_cfg : Cfg.t -> unit
end

val make_stack_offset : int -> Cfg.basic

val make_name_for_debugger :
  ident:Ident.t ->
  which_parameter:int option ->
  provenance:Backend_var.Provenance.t option ->
  is_assignment:bool ->
  regs:Reg.t array ->
  Cfg.basic

val make_const_int : nativeint -> Operation.t

val make_const_float32 : int32 -> Operation.t

val make_const_float : int64 -> Operation.t

val make_const_vec128 : Cmm.vec128_bits -> Operation.t

val make_const_symbol : Cmm.symbol -> Operation.t

val make_opaque : unit -> Operation.t

val regs_for : Cmm.machtype -> Reg.t array

val insert_debug :
  environment ->
  Sub_cfg.t ->
  Cfg.basic ->
  Debuginfo.t ->
  Reg.t array ->
  Reg.t array ->
  unit

val insert_op_debug_returning_id :
  environment ->
  Sub_cfg.t ->
  Operation.t ->
  Debuginfo.t ->
  Reg.t array ->
  Reg.t array ->
  InstructionId.t

val insert :
  environment -> Sub_cfg.t -> Cfg.basic -> Reg.t array -> Reg.t array -> unit

val insert' :
  environment ->
  Sub_cfg.t ->
  Cfg.terminator ->
  Reg.t array ->
  Reg.t array ->
  unit

val insert_debug' :
  environment ->
  Sub_cfg.t ->
  Cfg.terminator ->
  Debuginfo.t ->
  Reg.t array ->
  Reg.t array ->
  unit

val insert_op_debug' :
  environment ->
  Sub_cfg.t ->
  Cfg.terminator ->
  Debuginfo.t ->
  Reg.t array ->
  Reg.t array ->
  Reg.t array

val insert_move : environment -> Sub_cfg.t -> Reg.t -> Reg.t -> unit

val insert_moves :
  environment -> Sub_cfg.t -> Reg.t array -> Reg.t array -> unit

val insert_move_args :
  environment -> Sub_cfg.t -> Reg.t array -> Reg.t array -> int -> unit

val insert_move_results :
  environment -> Sub_cfg.t -> Reg.t array -> Reg.t array -> int -> unit

val maybe_emit_naming_op :
  environment ->
  Sub_cfg.t ->
  bound_name:Backend_var.With_provenance.t option ->
  Reg.t array ->
  unit

val join :
  environment ->
  Reg.t array Or_never_returns.t ->
  Sub_cfg.t ->
  Reg.t array Or_never_returns.t ->
  Sub_cfg.t ->
  bound_name:Backend_var.With_provenance.t option ->
  Reg.t array Or_never_returns.t

val join_array :
  environment ->
  (Reg.t array Or_never_returns.t * Sub_cfg.t) array ->
  bound_name:Backend_var.With_provenance.t option ->
  Reg.t array Or_never_returns.t
