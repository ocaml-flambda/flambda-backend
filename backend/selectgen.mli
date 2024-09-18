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

type environment = unit Select_utils.environment

class virtual selector_generic :
  object
    method is_store : Mach.operation -> bool

    method lift_op : Mach.operation -> Mach.instruction_desc

    method make_store :
      Cmm.memory_chunk -> Arch.addressing_mode -> bool -> Mach.instruction_desc

    method make_stack_offset : int -> Mach.instruction_desc

    method make_name_for_debugger :
      ident:Backend_var.t ->
      which_parameter:int option ->
      provenance:Backend_var.Provenance.t option ->
      is_assignment:bool ->
      regs:Reg.t array ->
      Mach.instruction_desc

    method make_const_int : nativeint -> Mach.operation

    method make_const_float32 : int32 -> Mach.operation

    method make_const_float : int64 -> Mach.operation

    method make_const_vec128 : Cmm.vec128_bits -> Mach.operation

    method make_const_symbol : Cmm.symbol -> Mach.operation

    method make_opaque : unit -> Mach.operation

    (* The following methods must or can be overridden by the processor
       description *)
    method is_immediate : Mach.integer_operation -> int -> bool
    (* Must be overriden to indicate whether a constant is a suitable immediate
       operand to the given integer arithmetic instruction. The default
       implementation handles shifts by immediate amounts, but produces no
       immediate operations otherwise. *)

    method virtual is_immediate_test : Mach.integer_comparison -> int -> bool
    (* Must be defined to indicate whether a constant is a suitable immediate
       operand to the given integer test *)

    method virtual select_addressing :
      Cmm.memory_chunk ->
      Cmm.expression ->
      Arch.addressing_mode * Cmm.expression
    (* Must be defined to select addressing modes *)

    method is_simple_expr : Cmm.expression -> bool

    method effects_of : Cmm.expression -> Select_utils.Effect_and_coeffect.t
    (* Can be overridden to reflect special extcalls known to be pure *)

    method select_operation :
      Cmm.operation ->
      Cmm.expression list ->
      Debuginfo.t ->
      Mach.operation * Cmm.expression list
    (* Can be overridden to deal with special arithmetic instructions *)

    method select_condition : Cmm.expression -> Mach.test * Cmm.expression
    (* Can be overridden to deal with special test instructions *)

    method select_store :
      bool ->
      Arch.addressing_mode ->
      Cmm.expression ->
      Mach.operation * Cmm.expression
    (* Can be overridden to deal with special store constant instructions *)

    method regs_for : Cmm.machtype -> Reg.t array
    (* Return an array of fresh registers of the given type. Default
       implementation is like Reg.createv. Can be overridden if float values are
       stored as pairs of integer registers. *)

    method insert_op :
      environment -> Mach.operation -> Reg.t array -> Reg.t array -> Reg.t array
    (* Can be overridden to deal with 2-address instructions or instructions
       with hardwired input/output registers *)

    method insert_op_debug :
      environment ->
      Mach.operation ->
      Debuginfo.t ->
      Reg.t array ->
      Reg.t array ->
      Reg.t array
    (* Can be overridden to deal with 2-address instructions or instructions
       with hardwired input/output registers *)

    method insert_move_extcall_arg :
      environment -> Cmm.exttype -> Reg.t array -> Reg.t array -> unit
    (* Can be overridden to deal with unusual unboxed calling conventions, e.g.
       on a 64-bit platform, passing unboxed 32-bit arguments in 32-bit stack
       slots. *)

    method emit_extcall_args :
      environment ->
      Cmm.exttype list ->
      Cmm.expression list ->
      Reg.t array * int
    (* Can be overridden to deal with stack-based calling conventions *)

    method emit_stores :
      environment -> Debuginfo.t -> Cmm.expression list -> Reg.t array -> unit
    (* Fill a freshly allocated block. Can be overridden for architectures that
       do not provide Arch.offset_addressing. *)

    method mark_call : unit
    (* informs the code emitter that the current function is non-leaf: it may
       perform a (non-tail) call; by default, sets [contains_calls := true] *)

    method mark_tailcall : unit
    (* informs the code emitter that the current function may end with a
       tail-call; by default, does nothing *)

    method mark_c_tailcall : unit

    (* informs the code emitter that the current function may call a C function
       that never returns; by default, sets [contains_calls := true] in [-g]
       mode and does nothing otherwise.

       It is unnecessary to save the stack pointer in this situation, unless we
       need to produce exact stack backtraces, hence the default definition.

       Some architectures still need to ensure that the stack is properly
       aligned when the C function is called, regardless of [-g]. This is
       achieved by overloading this method to set [contains_calls := true]. *)
    method mark_instr : Mach.instruction_desc -> unit
    (* dispatches on instructions to call one of the marking function above;
       overloading this is useful if Ispecific instructions need marking *)

    (* The following method is the entry point and should not be overridden *)
    method emit_fundecl :
      future_funcnames:Misc.Stdlib.String.Set.t -> Cmm.fundecl -> Mach.fundecl

    (* The following methods should not be overridden. They cannot be declared
       "private" in the current implementation because they are not always
       applied to "self", but ideally they should be private. *)
    method extract_onto : Mach.instruction -> Mach.instruction

    method extract : Mach.instruction

    method insert :
      environment -> Mach.instruction_desc -> Reg.t array -> Reg.t array -> unit

    method insert_debug :
      environment ->
      Mach.instruction_desc ->
      Debuginfo.t ->
      Reg.t array ->
      Reg.t array ->
      unit

    method insert_move : environment -> Reg.t -> Reg.t -> unit

    method insert_move_args :
      environment -> Reg.t array -> Reg.t array -> int -> unit

    method insert_move_results :
      environment -> Reg.t array -> Reg.t array -> int -> unit

    method insert_moves : environment -> Reg.t array -> Reg.t array -> unit

    method emit_expr :
      environment ->
      Cmm.expression ->
      bound_name:Backend_var.With_provenance.t option ->
      Reg.t array option

    method emit_expr_aux :
      environment ->
      Cmm.expression ->
      bound_name:Backend_var.With_provenance.t option ->
      Reg.t array option

    method emit_expr_aux_raise :
      environment ->
      Lambda.raise_kind ->
      Cmm.expression ->
      Debuginfo.t ->
      Reg.t array option

    method emit_expr_aux_op :
      environment ->
      Backend_var.With_provenance.t option ->
      Cmm.operation ->
      Cmm.expression list ->
      Debuginfo.t ->
      Reg.t array option

    method emit_expr_aux_ifthenelse :
      environment ->
      Backend_var.With_provenance.t option ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.kind_for_unboxing ->
      Reg.t array option

    method emit_expr_aux_switch :
      environment ->
      Backend_var.With_provenance.t option ->
      Cmm.expression ->
      int array ->
      (Cmm.expression * Debuginfo.t) array ->
      Debuginfo.t ->
      Cmm.kind_for_unboxing ->
      Reg.t array option

    method emit_expr_aux_catch :
      environment ->
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

    method emit_expr_aux_exit :
      environment ->
      Cmm.exit_label ->
      Cmm.expression list ->
      Cmm.trap_action list ->
      Reg.t array option

    method emit_expr_aux_trywith :
      environment ->
      Backend_var.With_provenance.t option ->
      Cmm.expression ->
      Cmm.trywith_shared_label ->
      Backend_var.With_provenance.t ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.kind_for_unboxing ->
      Reg.t array option

    method emit_tail : environment -> Cmm.expression -> unit

    method emit_tail_apply :
      environment ->
      Cmm.machtype ->
      Cmm.operation ->
      Cmm.expression list ->
      Debuginfo.t ->
      unit

    method emit_tail_ifthenelse :
      environment ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.kind_for_unboxing ->
      unit

    method emit_tail_switch :
      environment ->
      Cmm.expression ->
      int array ->
      (Cmm.expression * Debuginfo.t) array ->
      Debuginfo.t ->
      Cmm.kind_for_unboxing ->
      unit

    method emit_tail_catch :
      environment ->
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

    method emit_tail_trywith :
      environment ->
      Cmm.expression ->
      Cmm.trywith_shared_label ->
      Backend_var.With_provenance.t ->
      Cmm.expression ->
      Debuginfo.t ->
      Cmm.kind_for_unboxing ->
      unit

    method emit_return :
      environment -> Cmm.expression -> Cmm.trap_action list -> unit

    (* [contains_calls] is declared as a reference instance variable, instead of
       a mutable boolean instance variable, because the traversal uses
       functional object copies. *)
    val contains_calls : bool ref
  end
