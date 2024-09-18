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

open Cmm
open Select_utils

type environment = unit Select_utils.environment

type basic_or_terminator =
  | Basic of Cfg.basic
  | Terminator of Cfg.terminator
  | With_next_label of (Label.t -> Cfg.terminator)

let basic_op x = Basic (Op x)

class virtual selector_generic =
  object (self : 'self)
    inherit [unit, Cfg.operation, Cfg.basic] Select_utils.common_selector

    method is_store op = match op with Store (_, _, _) -> true | _ -> false

    method lift_op op = Cfg.Op op

    method make_store mem_chunk addr_mode is_assignment =
      Cfg.Op (Cfg.Store (mem_chunk, addr_mode, is_assignment))

    method make_stack_offset stack_ofs = Cfg.Op (Stackoffset stack_ofs)

    method make_name_for_debugger ~ident ~which_parameter ~provenance
        ~is_assignment ~regs =
      Cfg.Op
        (Cfg.Name_for_debugger
           { ident; which_parameter; provenance; is_assignment; regs })

    method make_const_int x = Cfg.Const_int x

    method make_const_float32 x = Cfg.Const_float32 x

    method make_const_float x = Cfg.Const_float x

    method make_const_vec128 x = Cfg.Const_vec128 x

    method make_const_symbol x = Cfg.Const_symbol x

    method make_opaque () = Cfg.Opaque

    (* Default instruction selection for stores (of words) *)

    method select_store is_assign addr arg : Cfg.operation * Cmm.expression =
      Store (Word_val, addr, is_assign), arg

    (* Default instruction selection for operators *)

    method select_operation (op : Cmm.operation) (args : Cmm.expression list)
        (_dbg : Debuginfo.t) : basic_or_terminator * Cmm.expression list =
      match op, args with
      | Capply _, Cconst_symbol (func, _dbg) :: rem ->
        ( With_next_label
            (fun label_after -> Call { op = Direct func; label_after }),
          rem )
      | Capply _, _ ->
        ( With_next_label
            (fun label_after -> Call { op = Indirect; label_after }),
          args )
      | Cextcall { func; builtin = true }, _ ->
        Misc.fatal_errorf
          "Selection.select_operation: builtin not recognized %s" func ()
      | Cextcall { func; alloc; ty; ty_args; returns; builtin = false }, _ ->
        let external_call =
          { Cfg.func_symbol = func;
            alloc;
            ty_res = ty;
            ty_args;
            stack_ofs = -1
          }
        in
        if returns
        then
          ( With_next_label
              (fun label_after ->
                Prim { op = External external_call; label_after }),
            args )
        else Terminator (Call_no_return external_call), args
      | Cload { memory_chunk; mutability; is_atomic }, [arg] ->
        let addressing_mode, eloc = self#select_addressing memory_chunk arg in
        let mutability = select_mutable_flag mutability in
        ( basic_op
            (Load { memory_chunk; addressing_mode; mutability; is_atomic }),
          [eloc] )
      | Cstore (chunk, init), [arg1; arg2] ->
        let addr, eloc = self#select_addressing chunk arg1 in
        let is_assign =
          match init with Initialization -> false | Assignment -> true
        in
        if chunk = Word_int || chunk = Word_val
        then
          let op, newarg2 = self#select_store is_assign addr arg2 in
          basic_op op, [newarg2; eloc]
        else basic_op (Store (chunk, addr, is_assign)), [arg2; eloc]
        (* Inversion addr/datum in Istore *)
      | Cdls_get, _ -> basic_op Dls_get, args
      | Calloc mode, _ ->
        basic_op (Alloc { bytes = 0; dbginfo = []; mode }), args
      | Caddi, _ -> self#select_arith_comm Mach.Iadd args
      | Csubi, _ -> self#select_arith Mach.Isub args
      | Cmuli, _ -> self#select_arith_comm Mach.Imul args
      | Cmulhi { signed }, _ ->
        self#select_arith_comm (Mach.Imulh { signed }) args
      | Cdivi, _ -> basic_op (Intop Mach.Idiv), args
      | Cmodi, _ -> basic_op (Intop Mach.Imod), args
      | Cand, _ -> self#select_arith_comm Mach.Iand args
      | Cor, _ -> self#select_arith_comm Mach.Ior args
      | Cxor, _ -> self#select_arith_comm Mach.Ixor args
      | Clsl, _ -> self#select_arith Mach.Ilsl args
      | Clsr, _ -> self#select_arith Mach.Ilsr args
      | Casr, _ -> self#select_arith Mach.Iasr args
      | Cclz { arg_is_non_zero }, _ ->
        basic_op (Intop (Mach.Iclz { arg_is_non_zero })), args
      | Cctz { arg_is_non_zero }, _ ->
        basic_op (Intop (Mach.Ictz { arg_is_non_zero })), args
      | Cpopcnt, _ -> basic_op (Intop Mach.Ipopcnt), args
      | Ccmpi comp, _ -> self#select_arith_comp (Mach.Isigned comp) args
      | Caddv, _ -> self#select_arith_comm Mach.Iadd args
      | Cadda, _ -> self#select_arith_comm Mach.Iadd args
      | Ccmpa comp, _ -> self#select_arith_comp (Mach.Iunsigned comp) args
      | Ccmpf (w, comp), _ -> basic_op (Floatop (w, Mach.Icompf comp)), args
      | Ccsel _, [cond; ifso; ifnot] ->
        let cond, earg = self#select_condition cond in
        basic_op (Csel cond), [earg; ifso; ifnot]
      | Cnegf w, _ -> basic_op (Floatop (w, Mach.Inegf)), args
      | Cabsf w, _ -> basic_op (Floatop (w, Mach.Iabsf)), args
      | Caddf w, _ -> basic_op (Floatop (w, Mach.Iaddf)), args
      | Csubf w, _ -> basic_op (Floatop (w, Mach.Isubf)), args
      | Cmulf w, _ -> basic_op (Floatop (w, Mach.Imulf)), args
      | Cdivf w, _ -> basic_op (Floatop (w, Mach.Idivf)), args
      | Creinterpret_cast cast, _ -> basic_op (Reinterpret_cast cast), args
      | Cstatic_cast cast, _ -> basic_op (Static_cast cast), args
      | Catomic { op = Fetch_and_add; size }, [src; dst] ->
        let dst_size =
          match size with
          | Word | Sixtyfour -> Word_int
          | Thirtytwo -> Thirtytwo_signed
        in
        let addr, eloc = self#select_addressing dst_size dst in
        basic_op (Intop_atomic { op = Fetch_and_add; size; addr }), [src; eloc]
      | Catomic { op = Compare_and_swap; size }, [compare_with; set_to; dst] ->
        let dst_size =
          match size with
          | Word | Sixtyfour -> Word_int
          | Thirtytwo -> Thirtytwo_signed
        in
        let addr, eloc = self#select_addressing dst_size dst in
        ( basic_op (Intop_atomic { op = Compare_and_swap; size; addr }),
          [compare_with; set_to; eloc] )
      | Cprobe { name; handler_code_sym; enabled_at_init }, _ ->
        ( With_next_label
            (fun label_after ->
              Prim
                { op = Probe { name; handler_code_sym; enabled_at_init };
                  label_after
                }),
          args )
      | Cprobe_is_enabled { name }, _ ->
        basic_op (Probe_is_enabled { name }), []
      | Cbeginregion, _ -> basic_op Begin_region, []
      | Cendregion, _ -> basic_op End_region, args
      | _ -> Misc.fatal_error "Selection.select_oper"

    method private select_arith_comm (op : Mach.integer_operation)
        (args : Cmm.expression list) : basic_or_terminator * Cmm.expression list
        =
      match args with
      | [arg; Cconst_int (n, _)] when self#is_immediate op n ->
        basic_op (Intop_imm (op, n)), [arg]
      | [Cconst_int (n, _); arg] when self#is_immediate op n ->
        basic_op (Intop_imm (op, n)), [arg]
      | _ -> basic_op (Intop op), args

    method private select_arith (op : Mach.integer_operation)
        (args : Cmm.expression list) : basic_or_terminator * Cmm.expression list
        =
      match args with
      | [arg; Cconst_int (n, _)] when self#is_immediate op n ->
        basic_op (Intop_imm (op, n)), [arg]
      | _ -> basic_op (Intop op), args

    method private select_arith_comp (cmp : Mach.integer_comparison)
        (args : Cmm.expression list) : basic_or_terminator * Cmm.expression list
        =
      match args with
      | [arg; Cconst_int (n, _)] when self#is_immediate (Mach.Icomp cmp) n ->
        basic_op (Intop_imm (Icomp cmp, n)), [arg]
      | [Cconst_int (n, _); arg]
        when self#is_immediate (Mach.Icomp (Select_utils.swap_intcomp cmp)) n ->
        basic_op (Intop_imm (Icomp (Select_utils.swap_intcomp cmp), n)), [arg]
      | _ -> basic_op (Intop (Icomp cmp)), args

    (* Buffering of instruction sequences *)

    method insert
        : environment -> Cfg.basic -> Reg.t array -> Reg.t array -> unit =
      fun _ _ _ _ -> Misc.fatal_error "not implemented"

    method insert_debug
        : environment ->
          Cfg.basic ->
          Debuginfo.t ->
          Reg.t array ->
          Reg.t array ->
          unit =
      fun _ _ _ _ _ -> Misc.fatal_error "not implemented"

    method insert_move env src dst =
      if src.Reg.stamp <> dst.Reg.stamp
      then self#insert env Cfg.(Op Move) [| src |] [| dst |]

    method emit_expr_aux_raise
        : environment ->
          Lambda.raise_kind ->
          Cmm.expression ->
          Debuginfo.t ->
          Reg.t array option =
      fun _ _ _ _ -> Misc.fatal_error "not implemented"

    method emit_expr_aux_op
        : environment ->
          Backend_var.With_provenance.t option ->
          Cmm.operation ->
          Cmm.expression list ->
          Debuginfo.t ->
          Reg.t array option =
      fun _ _ _ _ _ -> Misc.fatal_error "not implemented"

    method emit_expr_aux_ifthenelse
        : environment ->
          Backend_var.With_provenance.t option ->
          Cmm.expression ->
          Debuginfo.t ->
          Cmm.expression ->
          Debuginfo.t ->
          Cmm.expression ->
          Debuginfo.t ->
          Cmm.kind_for_unboxing ->
          Reg.t array option =
      fun _ _ _ _ _ _ _ _ _ -> Misc.fatal_error "not implemented"

    method emit_expr_aux_switch
        : environment ->
          Backend_var.With_provenance.t option ->
          Cmm.expression ->
          int array ->
          (Cmm.expression * Debuginfo.t) array ->
          Debuginfo.t ->
          Cmm.kind_for_unboxing ->
          Reg.t array option =
      fun _ _ _ _ _ _ _ -> Misc.fatal_error "not implemented"

    method emit_expr_aux_catch
        : environment ->
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
          Reg.t array option =
      fun _ _ _ _ _ -> Misc.fatal_error "not implemented"

    method emit_expr_aux_exit
        : environment ->
          Cmm.exit_label ->
          Cmm.expression list ->
          Cmm.trap_action list ->
          Reg.t array option =
      fun _ _ _ _ -> Misc.fatal_error "not implemented"

    method emit_expr_aux_trywith
        : environment ->
          Backend_var.With_provenance.t option ->
          Cmm.expression ->
          Cmm.trywith_shared_label ->
          Backend_var.With_provenance.t ->
          Cmm.expression ->
          Debuginfo.t ->
          Cmm.kind_for_unboxing ->
          Reg.t array option =
      fun _ _ _ _ _ _ _ _ -> Misc.fatal_error "not implemented"

    method emit_return
        : environment -> Cmm.expression -> Cmm.trap_action list -> unit =
      fun _ _ _ -> Misc.fatal_error "not implemented"

    method emit_tail_apply
        : environment ->
          Cmm.machtype ->
          Cmm.operation ->
          Cmm.expression list ->
          Debuginfo.t ->
          unit =
      fun _ _ _ _ _ -> Misc.fatal_error "not implemented"

    method emit_tail_ifthenelse
        : environment ->
          Cmm.expression ->
          Debuginfo.t ->
          Cmm.expression ->
          Debuginfo.t ->
          Cmm.expression ->
          Debuginfo.t ->
          Cmm.kind_for_unboxing ->
          unit =
      fun _ _ _ _ _ _ _ _ -> Misc.fatal_error "not implemented"

    method emit_tail_switch
        : environment ->
          Cmm.expression ->
          int array ->
          (Cmm.expression * Debuginfo.t) array ->
          Debuginfo.t ->
          Cmm.kind_for_unboxing ->
          unit =
      fun _ _ _ _ _ _ -> Misc.fatal_error "not implemented"

    method emit_tail_catch
        : environment ->
          Cmm.rec_flag ->
          (Lambda.static_label
          * (Backend_var.With_provenance.t * Cmm.machtype) list
          * Cmm.expression
          * Debuginfo.t
          * bool)
          list ->
          Cmm.expression ->
          Cmm.kind_for_unboxing ->
          unit =
      fun _ _ _ _ _ -> Misc.fatal_error "not implemented"

    method emit_tail_trywith
        : environment ->
          Cmm.expression ->
          Cmm.trywith_shared_label ->
          Backend_var.With_provenance.t ->
          Cmm.expression ->
          Debuginfo.t ->
          Cmm.kind_for_unboxing ->
          unit =
      fun _ _ _ _ _ _ _ -> Misc.fatal_error "not implemented"
  end
