(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                 et al.                                 *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2024--2025 Jane Street Group LLC.                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-9-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list
module Int = Numbers.Int
module Or_never_returns = Select_utils.Or_never_returns
module SU = Select_utils
module V = Backend_var
module VP = Backend_var.With_provenance
open SU.Or_never_returns.Syntax

(* CR-soon gyorsh: This functor must not have state, because it is instantiated
   twice with the same [Target] (see [Asmgen] and [Peephole_utils] to avoid
   dependency cycles. *)
module Make (Target : Cfg_selectgen_target_intf.S) = struct
  (* A syntactic criterion used in addition to judgements about (co)effects as
     to whether the evaluation of a given expression may be deferred by
     [emit_parts]. This criterion is a property of the instruction selection
     algorithm in this file rather than a property of the Cmm language. *)
  let rec is_simple_expr0 (expr : Cmm.expression) =
    match expr with
    | Cconst_int _ -> true
    | Cconst_natint _ -> true
    | Cconst_float32 _ -> true
    | Cconst_float _ -> true
    | Cconst_symbol _ -> true
    | Cconst_vec128 _ -> true
    | Cvar _ -> true
    | Ctuple el -> List.for_all is_simple_expr el
    | Clet (_id, arg, body) -> is_simple_expr arg && is_simple_expr body
    | Cphantom_let (_var, _defining_expr, body) -> is_simple_expr body
    | Csequence (e1, e2) -> is_simple_expr e1 && is_simple_expr e2
    | Cop (op, args, _) -> (
      match op with
      (* Cextcall with neither effects nor coeffects is simple if its arguments
         are *)
      | Cextcall { effects = No_effects; coeffects = No_coeffects } ->
        List.for_all is_simple_expr args
        (* The following may have side effects *)
      | Capply _ | Cextcall _ | Calloc _ | Cstore _ | Craise _ | Catomic _
      | Cprobe _ | Cprobe_is_enabled _ | Copaque | Cpoll ->
        false
      | Cprefetch _ | Cbeginregion | Cendregion ->
        false
        (* avoid reordering *)
        (* The remaining operations are simple if their args are *)
      | Cload _ | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand | Cor
      | Cxor | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf _
      | Cclz _ | Cctz _ | Cpopcnt | Cbswap _ | Ccsel _ | Cabsf _ | Caddf _
      | Csubf _ | Cmulf _ | Cdivf _ | Cpackf32 | Creinterpret_cast _
      | Cstatic_cast _ | Ctuple_field _ | Ccmpf _ | Cdls_get ->
        List.for_all is_simple_expr args)
    | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _ -> false

  and is_simple_expr expr =
    match Target.is_simple_expr expr with
    | Simple_if_all_expressions_are exprs -> List.for_all is_simple_expr exprs
    | Use_default -> is_simple_expr0 expr

  (* Analyses the effects and coeffects of an expression. This is used across a
     whole list of expressions with a view to determining which expressions may
     have their evaluation deferred. The result of this function, modulo
     target-specific judgements in [effects_of], is a property of the Cmm
     language rather than anything particular about the instruction selection
     algorithm in this file.

     In the case of e.g. an OCaml function call, the arguments whose evaluation
     cannot be deferred (cf. [emit_parts], below) are computed in right-to-left
     order first with their results going into temporaries, then the block is
     allocated, then the remaining arguments are evaluated before being combined
     with the temporaries. *)
  let rec effects_of0 (exp : Cmm.expression) =
    let module EC = SU.Effect_and_coeffect in
    match exp with
    | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
    | Cconst_symbol _ | Cconst_vec128 _ | Cvar _ ->
      EC.none
    | Ctuple el -> EC.join_list_map el effects_of
    | Clet (_id, arg, body) -> EC.join (effects_of arg) (effects_of body)
    | Cphantom_let (_var, _defining_expr, body) -> effects_of body
    | Csequence (e1, e2) -> EC.join (effects_of e1) (effects_of e2)
    | Cifthenelse (cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg) ->
      EC.join (effects_of cond) (EC.join (effects_of ifso) (effects_of ifnot))
    | Cop (op, args, _) ->
      let from_op =
        match op with
        | Cextcall { effects = e; coeffects = ce } ->
          EC.create (SU.select_effects e) (SU.select_coeffects ce)
        | Capply _ | Cprobe _ | Copaque | Cpoll -> EC.arbitrary
        | Calloc (Heap, _) -> EC.none
        | Calloc (Local, _) -> EC.coeffect_only Arbitrary
        | Cstore _ -> EC.effect_only Arbitrary
        | Cbeginregion | Cendregion -> EC.arbitrary
        | Cprefetch _ -> EC.arbitrary
        | Catomic _ -> EC.arbitrary
        | Craise _ -> EC.effect_only Raise
        | Cload { mutability = Immutable } -> EC.none
        | Cload { mutability = Mutable } | Cdls_get ->
          EC.coeffect_only Read_mutable
        | Cprobe_is_enabled _ -> EC.coeffect_only Arbitrary
        | Ctuple_field _ | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi
        | Cand | Cor | Cxor | Cbswap _ | Ccsel _ | Cclz _ | Cctz _ | Cpopcnt
        | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf _
        | Cabsf _ | Caddf _ | Csubf _ | Cmulf _ | Cdivf _ | Cpackf32
        | Creinterpret_cast _ | Cstatic_cast _ | Ccmpf _ ->
          EC.none
      in
      EC.join from_op (EC.join_list_map args effects_of)
    | Cswitch _ | Ccatch _ | Cexit _ -> EC.arbitrary

  and effects_of (expr : Cmm.expression) =
    match Target.effects_of expr with
    | Effects_of_all_expressions exprs ->
      SU.Effect_and_coeffect.join_list_map exprs effects_of
    | Use_default -> effects_of0 expr

  (* Says whether an integer constant is a suitable immediate argument for the
     given integer operation *)

  let is_immediate (op : Operation.integer_operation) n =
    match Target.is_immediate op n with
    | Is_immediate result -> result
    | Use_default -> (
      match op with
      | Ilsl | Ilsr | Iasr -> n >= 0 && n < Arch.size_int * 8
      | _ -> false)

  let is_immediate_test cmp n =
    match Target.is_immediate_test cmp n with
    | Is_immediate result -> result
    | Use_default -> is_immediate (Icomp cmp) n

  (* Instruction selection for conditionals *)

  let select_condition (arg : Cmm.expression) : Operation.test * Cmm.expression
      =
    match arg with
    | Cop (Ccmpi cmp, [arg1; Cconst_int (n, _)], _)
      when is_immediate_test (Isigned cmp) n ->
      Iinttest_imm (Isigned cmp, n), arg1
    | Cop (Ccmpi cmp, [Cconst_int (n, _); arg2], _)
      when is_immediate_test (Isigned (Cmm.swap_integer_comparison cmp)) n ->
      Iinttest_imm (Isigned (Cmm.swap_integer_comparison cmp), n), arg2
    | Cop (Ccmpi cmp, args, _) -> Iinttest (Isigned cmp), Ctuple args
    | Cop (Ccmpa cmp, [arg1; Cconst_int (n, _)], _)
      when is_immediate_test (Iunsigned cmp) n ->
      Iinttest_imm (Iunsigned cmp, n), arg1
    | Cop (Ccmpa cmp, [Cconst_int (n, _); arg2], _)
      when is_immediate_test (Iunsigned (Cmm.swap_integer_comparison cmp)) n ->
      Iinttest_imm (Iunsigned (Cmm.swap_integer_comparison cmp), n), arg2
    | Cop (Ccmpa cmp, args, _) -> Iinttest (Iunsigned cmp), Ctuple args
    | Cop (Ccmpf (width, cmp), args, _) -> Ifloattest (width, cmp), Ctuple args
    | Cop (Cand, [arg1; Cconst_int (1, _)], _) -> Ioddtest, arg1
    | _ -> Itruetest, arg

  let is_store (op : Operation.t) =
    match op with Store (_, _, _) -> true | _ -> false

  let bind_let (env : SU.environment) sub_cfg v r1 =
    let env =
      let rv = Reg.createv_with_typs_and_id ~id:(VP.var v) r1 in
      SU.insert_moves env sub_cfg r1 rv;
      SU.env_add v rv env
    in
    let provenance = VP.provenance v in
    (if Option.is_some provenance
    then
      let naming_op =
        SU.make_name_for_debugger ~ident:(VP.var v) ~which_parameter:None
          ~provenance ~is_assignment:false ~regs:r1
      in
      SU.insert_debug env sub_cfg naming_op Debuginfo.none [||] [||]);
    env

  (* Add an Iop opcode. Can be augmented by the processor description to insert
     moves before and after the operation, e.g. for two-address instructions, or
     instructions using dedicated registers. *)

  let insert_op_debug env sub_cfg op dbg rs rd =
    match Target.insert_op_debug env sub_cfg op dbg rs rd with
    | Regs rd -> rd
    | Use_default ->
      SU.insert_debug env sub_cfg (Op op) dbg rs rd;
      rd

  let insert_op env sub_cfg op rs rd =
    insert_op_debug env sub_cfg op Debuginfo.none rs rd

  (* Default instruction selection for stores (of words) *)

  let insert_move_extcall_arg env sub_cfg ty_arg src dst dbg =
    (* The default implementation an ordinary move. It can be overridden to use
       special move instructions, for example a "32-bit move" instruction for
       int32 arguments. *)
    match Target.insert_move_extcall_arg ty_arg src dst with
    | Rewritten (basic, src, dst) ->
      SU.insert_debug env sub_cfg basic dbg src dst
    | Use_default -> SU.insert_moves env sub_cfg src dst

  (* Default instruction selection for integer operations *)

  let select_arith_comm (op : Operation.integer_operation)
      (args : Cmm.expression list) :
      Cfg.basic_or_terminator * Cmm.expression list =
    match args with
    | [arg; Cconst_int (n, _)] when is_immediate op n ->
      SU.basic_op (Intop_imm (op, n)), [arg]
    | [Cconst_int (n, _); arg] when is_immediate op n ->
      SU.basic_op (Intop_imm (op, n)), [arg]
    | _ -> SU.basic_op (Intop op), args

  let select_arith (op : Operation.integer_operation)
      (args : Cmm.expression list) :
      Cfg.basic_or_terminator * Cmm.expression list =
    match args with
    | [arg; Cconst_int (n, _)] when is_immediate op n ->
      SU.basic_op (Intop_imm (op, n)), [arg]
    | _ -> SU.basic_op (Intop op), args

  let select_arith_comp (cmp : Operation.integer_comparison)
      (args : Cmm.expression list) :
      Cfg.basic_or_terminator * Cmm.expression list =
    match args with
    | [arg; Cconst_int (n, _)] when is_immediate (Operation.Icomp cmp) n ->
      SU.basic_op (Intop_imm (Icomp cmp, n)), [arg]
    | [Cconst_int (n, _); arg]
      when is_immediate (Operation.Icomp (SU.swap_intcomp cmp)) n ->
      SU.basic_op (Intop_imm (Icomp (SU.swap_intcomp cmp), n)), [arg]
    | _ -> SU.basic_op (Intop (Icomp cmp)), args

  (* Default instruction selection for operators *)

  let select_operation0 (op : Cmm.operation) (args : Cmm.expression list)
      (dbg : Debuginfo.t) ~label_after :
      Cfg.basic_or_terminator * Cmm.expression list =
    let wrong_num_args n =
      Misc.fatal_errorf
        "Selection.select_operation: expected %d argument(s) for@ %s" n
        (Printcmm.operation dbg op)
    in
    let[@inline] single_arg () =
      match args with [arg] -> arg | [] | _ :: _ -> wrong_num_args 1
    in
    let[@inline] two_args () =
      match args with
      | [arg1; arg2] -> arg1, arg2
      | [] | _ :: _ -> wrong_num_args 2
    in
    let[@inline] three_args () =
      match args with
      | [arg1; arg2; arg3] -> arg1, arg2, arg3
      | [] | _ :: _ -> wrong_num_args 3
    in
    match[@ocaml.warning "+fragile-match"] op with
    | Capply _ -> (
      match[@ocaml.warning "-fragile-match"] args with
      | Cconst_symbol (func, _dbg) :: rem ->
        Terminator (Call { op = Direct func; label_after }), rem
      | _ -> Terminator (Call { op = Indirect; label_after }), args)
    | Cextcall { func; builtin = true } ->
      Misc.fatal_errorf "Selection.select_operation: builtin not recognized %s"
        func ()
    | Cextcall
        { func; alloc; ty; ty_args; returns; builtin = false; effects; _ } ->
      let external_call =
        { Cfg.func_symbol = func;
          alloc;
          effects;
          ty_res = ty;
          ty_args;
          stack_ofs = -1
        }
      in
      if returns
      then Terminator (Prim { op = External external_call; label_after }), args
      else Terminator (Call_no_return external_call), args
    | Cload { memory_chunk; mutability; is_atomic } ->
      let arg = single_arg () in
      let addressing_mode, eloc = Target.select_addressing memory_chunk arg in
      let mutability = SU.select_mutable_flag mutability in
      ( SU.basic_op
          (Load { memory_chunk; addressing_mode; mutability; is_atomic }),
        [eloc] )
    | Cstore (chunk, init) -> (
      let arg1, arg2 = two_args () in
      let addr, eloc = Target.select_addressing chunk arg1 in
      let is_assign =
        match init with Initialization -> false | Assignment -> true
      in
      match[@ocaml.warning "-fragile-match"] chunk with
      | Word_int | Word_val ->
        let op, newarg2 =
          match Target.is_store_out_of_range chunk ~byte_offset:0 with
          | Within_range -> (
            match Target.select_store ~is_assign addr arg2 with
            | Rewritten (op, arg) -> op, arg
            | Use_default | Maybe_out_of_range ->
              Operation.Store (chunk, addr, is_assign), arg2)
          | Out_of_range ->
            Misc.fatal_errorf
              "Cstore expression produces store instruction that is out of \
               range:@ %s"
              (Printcmm.operation dbg op)
        in
        SU.basic_op op, [newarg2; eloc]
      | _ -> SU.basic_op (Store (chunk, addr, is_assign)), [arg2; eloc]
      (* Inversion addr/datum in Istore *))
    | Cdls_get -> SU.basic_op Dls_get, args
    | Calloc (mode, alloc_block_kind) ->
      let placeholder_for_alloc_block_kind : Cmm.alloc_dbginfo_item =
        { alloc_words = 0; alloc_block_kind; alloc_dbg = Debuginfo.none }
      in
      ( SU.basic_op
          (Alloc
             { bytes = 0; dbginfo = [placeholder_for_alloc_block_kind]; mode }),
        args )
    | Cpoll -> SU.basic_op Poll, args
    | Caddi -> select_arith_comm Iadd args
    | Csubi -> select_arith Isub args
    | Cmuli -> select_arith_comm Imul args
    | Cmulhi { signed } -> select_arith_comm (Imulh { signed }) args
    | Cdivi -> SU.basic_op (Intop Idiv), args
    | Cmodi -> SU.basic_op (Intop Imod), args
    | Cand -> select_arith_comm Iand args
    | Cor -> select_arith_comm Ior args
    | Cxor -> select_arith_comm Ixor args
    | Clsl -> select_arith Ilsl args
    | Clsr -> select_arith Ilsr args
    | Casr -> select_arith Iasr args
    | Cclz { arg_is_non_zero } ->
      SU.basic_op (Intop (Iclz { arg_is_non_zero })), args
    | Cctz { arg_is_non_zero } ->
      SU.basic_op (Intop (Ictz { arg_is_non_zero })), args
    | Cpopcnt -> SU.basic_op (Intop Ipopcnt), args
    | Ccmpi comp -> select_arith_comp (Isigned comp) args
    | Caddv -> select_arith_comm Iadd args
    | Cadda -> select_arith_comm Iadd args
    | Ccmpa comp -> select_arith_comp (Iunsigned comp) args
    | Ccmpf (w, comp) -> SU.basic_op (Floatop (w, Icompf comp)), args
    | Ccsel _ ->
      let cond, ifso, ifnot = three_args () in
      let cond, earg = select_condition cond in
      SU.basic_op (Csel cond), [earg; ifso; ifnot]
    | Cnegf w -> SU.basic_op (Floatop (w, Inegf)), args
    | Cabsf w -> SU.basic_op (Floatop (w, Iabsf)), args
    | Caddf w -> SU.basic_op (Floatop (w, Iaddf)), args
    | Csubf w -> SU.basic_op (Floatop (w, Isubf)), args
    | Cmulf w -> SU.basic_op (Floatop (w, Imulf)), args
    | Cdivf w -> SU.basic_op (Floatop (w, Idivf)), args
    | Creinterpret_cast cast -> SU.basic_op (Reinterpret_cast cast), args
    | Cstatic_cast cast -> SU.basic_op (Static_cast cast), args
    | Catomic { op; size } -> (
      match op with
      | Exchange | Fetch_and_add | Add | Sub | Land | Lor | Lxor ->
        let src, dst = two_args () in
        let dst_size : Cmm.memory_chunk =
          match size with
          | Word | Sixtyfour -> Word_int
          | Thirtytwo -> Thirtytwo_signed
        in
        let addr, eloc = Target.select_addressing dst_size dst in
        SU.basic_op (Intop_atomic { op; size; addr }), [src; eloc]
      | Compare_set | Compare_exchange ->
        let compare_with, set_to, dst = three_args () in
        let dst_size : Cmm.memory_chunk =
          match size with
          | Word | Sixtyfour -> Word_int
          | Thirtytwo -> Thirtytwo_signed
        in
        let addr, eloc = Target.select_addressing dst_size dst in
        ( SU.basic_op (Intop_atomic { op; size; addr }),
          [compare_with; set_to; eloc] ))
    | Cprobe { name; handler_code_sym; enabled_at_init } ->
      ( Terminator
          (Prim
             { op = Probe { name; handler_code_sym; enabled_at_init };
               label_after
             }),
        args )
    | Cprobe_is_enabled { name } -> SU.basic_op (Probe_is_enabled { name }), []
    | Cbeginregion -> SU.basic_op Begin_region, []
    | Cendregion -> SU.basic_op End_region, args
    | Cpackf32 | Copaque | Cbswap _ | Cprefetch _ | Craise _
    | Ctuple_field (_, _) ->
      Misc.fatal_error "Selection.select_oper"

  let rec select_operation (op : Cmm.operation) (args : Cmm.expression list)
      (dbg : Debuginfo.t) ~label_after :
      Cfg.basic_or_terminator * Cmm.expression list =
    match
      Target.select_operation ~generic_select_condition:select_condition op args
        dbg ~label_after
    with
    | Rewritten (basic_or_terminator, args) -> basic_or_terminator, args
    | Select_operation_then_rewrite (op, args, dbg, rewriter) -> (
      let basic_or_terminator, args =
        select_operation op args dbg ~label_after
      in
      match rewriter basic_or_terminator ~args with
      | Rewritten (basic_or_terminator, args) -> basic_or_terminator, args
      | Use_default -> basic_or_terminator, args)
    | Use_default -> select_operation0 op args dbg ~label_after

  let insert_return env sub_cfg (r : _ Or_never_returns.t)
      (traps : Cmm.trap_action list) =
    match r with
    | Never_returns -> ()
    | Ok r ->
      List.iter
        (fun trap ->
          let instr_desc =
            match trap with
            | Cmm.Push _ -> Misc.fatal_error "unexpected push on trap actions"
            | Cmm.Pop handler_id ->
              let lbl_handler =
                (SU.env_find_static_exception handler_id env).label
              in
              Cfg.Poptrap { lbl_handler }
          in
          Sub_cfg.add_instruction sub_cfg instr_desc [||] [||] Debuginfo.none)
        traps;
      let loc = Proc.loc_results_return (Reg.typv r) in
      SU.insert_moves env sub_cfg r loc;
      SU.insert' env sub_cfg Cfg.Return loc [||]

  (* Buffering of instruction sequences *)

  let insert_debug _env sub_cfg basic dbg arg res =
    Sub_cfg.add_instruction sub_cfg basic arg res dbg

  let insert_op_debug_returning_id _env sub_cfg op dbg arg res =
    let instr = Cfg.make_instr (Cfg.Op op) arg res dbg in
    Sub_cfg.add_instruction' sub_cfg instr;
    instr.id

  let setup_catch_handler (flag : Cmm.ccatch_flag) rs sub_cfg =
    match flag with
    | Normal | Recursive -> ()
    | Exn_handler ->
      Sub_cfg.mark_as_trap_handler sub_cfg;
      let exn_bucket_in_handler =
        match rs with
        | [] -> Misc.fatal_error "Exception handler with no parameters"
        | r :: _ -> r
      in
      Sub_cfg.add_instruction_at_start sub_cfg (Cfg.Op Move)
        [| Proc.loc_exn_bucket |] exn_bucket_in_handler Debuginfo.none

  let unreachable_handler : (Operation.trap_stack * Cmm.expression) Lazy.t =
    lazy
      (let dummy_constant : Cmm.expression = Cconst_int (1, Debuginfo.none) in
       let segfault : Cmm.expression =
         Cop
           ( Cload
               { memory_chunk = Word_int;
                 mutability = Mutable;
                 is_atomic = false
               },
             [Cconst_int (0, Debuginfo.none)],
             Debuginfo.none )
       in
       let dummy_raise : Cmm.expression =
         Cop (Craise Raise_notrace, [dummy_constant], Debuginfo.none)
       in
       (* The use of a raise operation means that this handler is known not to
          return, making it compatible with any layout for the body or
          surrounding code. We also set the trap stack to [Uncaught] to ensure
          that we don't introduce spurious control-flow edges inside the
          function. *)
       Uncaught, Csequence (segfault, dummy_raise))

  (* The following two functions, [emit_parts] and [emit_parts_list], force
     right-to-left evaluation order as required by the Flambda [Un_anf] pass
     (and to be consistent with the bytecode compiler). *)

  let rec emit_parts env sub_cfg ~effects_after exp : _ Or_never_returns.t =
    let module EC = SU.Effect_and_coeffect in
    let may_defer_evaluation =
      let ec = effects_of exp in
      match EC.effect ec with
      | Arbitrary | Raise ->
        (* Preserve the ordering of effectful expressions by evaluating them
           early (in the correct order) and assigning their results to
           temporaries. We can avoid this in just one case: if we know that
           every [exp'] in the original expression list (cf. [emit_parts_list])
           to be evaluated after [exp] cannot possibly affect the result of
           [exp] or depend on the result of [exp], then [exp] may be deferred.
           (Checking purity here is not enough: we need to check copurity too to
           avoid e.g. moving mutable reads earlier than the raising of an
           exception.) *)
        EC.pure_and_copure effects_after
      | None -> (
        match EC.coeffect ec with
        | None ->
          (* Pure expressions may be moved. *)
          true
        | Read_mutable -> (
          (* Read-mutable expressions may only be deferred if evaluation of
             every [exp'] (for [exp'] as in the comment above) has no effects
             "worse" (in the sense of the ordering in [t]) than raising an
             exception. *)
          match EC.effect effects_after with
          | None | Raise -> true
          | Arbitrary -> false)
        | Arbitrary -> (
          (* Arbitrary expressions may only be deferred if evaluation of every
             [exp'] (for [exp'] as in the comment above) has no effects. *)
          match EC.effect effects_after with
          | None -> true
          | Arbitrary | Raise -> false))
    in
    (* Even though some expressions may look like they can be deferred from the
       (co)effect analysis, it may be forbidden to move them. *)
    if may_defer_evaluation && is_simple_expr exp
    then Ok (exp, env)
    else
      match emit_expr env sub_cfg exp ~bound_name:None with
      | Never_returns -> Never_returns
      | Ok r ->
        if Array.length r = 0
        then Ok (Cmm.Ctuple [], env)
        else
          (* The normal case *)
          let id = V.create_local "bind" in
          (* Introduce a fresh temp to hold the result *)
          let tmp = Reg.createv_with_typs_and_id ~id r in
          SU.insert_moves env sub_cfg r tmp;
          Ok (Cmm.Cvar id, SU.env_add (VP.create id) tmp env)

  and emit_parts_list env sub_cfg exp_list : _ Or_never_returns.t =
    let module EC = SU.Effect_and_coeffect in
    let exp_list_right_to_left, _effect =
      (* Annotate each expression with the (co)effects that happen after it when
         the original expression list is evaluated from right to left. The
         resulting expression list has the rightmost expression first. *)
      List.fold_left
        (fun (exp_list, effects_after) exp ->
          let exp_effect = effects_of exp in
          (exp, effects_after) :: exp_list, EC.join exp_effect effects_after)
        ([], EC.none) exp_list
    in
    List.fold_left
      (fun (results_and_env : _ Or_never_returns.t) (exp, effects_after) :
           _ Or_never_returns.t ->
        match results_and_env with
        | Never_returns -> Never_returns
        | Ok (result, env) -> (
          match emit_parts env sub_cfg exp ~effects_after with
          | Never_returns -> Never_returns
          | Ok (exp_result, env) -> Ok (exp_result :: result, env)))
      (Or_never_returns.Ok ([], env))
      exp_list_right_to_left

  and emit_tuple_not_flattened env sub_cfg exp_list =
    let rec emit_list = function
      | [] -> Or_never_returns.Ok []
      | exp :: rem -> (
        (* Again, force right-to-left evaluation *)
        let* loc_rem = emit_list rem in
        match emit_expr env sub_cfg exp ~bound_name:None with
        | Never_returns -> Never_returns
        | Ok loc_exp -> Ok (loc_exp :: loc_rem))
    in
    emit_list exp_list

  and emit_tuple env sub_cfg exp_list =
    let* l = emit_tuple_not_flattened env sub_cfg exp_list in
    Ok (Array.concat l)

  and emit_extcall_args env sub_cfg ty_args args dbg =
    let* args = emit_tuple_not_flattened env sub_cfg args in
    let ty_args =
      match ty_args with
      | [] -> List.map (fun _ -> Cmm.XInt) args
      | _ :: _ -> ty_args
    in
    let locs, stack_ofs = Proc.loc_external_arguments ty_args in
    let ty_args = Array.of_list ty_args in
    if stack_ofs <> 0
    then SU.insert env sub_cfg (SU.make_stack_offset stack_ofs) [||] [||];
    List.iteri
      (fun i arg ->
        insert_move_extcall_arg env sub_cfg ty_args.(i) arg locs.(i) dbg)
      args;
    Ok (Array.concat (Array.to_list locs), stack_ofs)

  and emit_stores env sub_cfg dbg (args : Cmm.expression list) regs_addr =
    let addressing_mode =
      ref (Arch.offset_addressing Arch.identity_addressing (-Arch.size_int))
    in
    let byte_offset = ref 0 in
    let base =
      assert (Array.length regs_addr = 1);
      ref regs_addr
    in
    let for_one_arg arg =
      let original_arg = arg in
      let select_store_result =
        Target.select_store ~is_assign:false !addressing_mode arg
      in
      let arg : Cmm.expression =
        match select_store_result with
        | Maybe_out_of_range | Use_default -> arg
        | Rewritten (_, arg) -> arg
      in
      match emit_expr env sub_cfg arg ~bound_name:None with
      | Ok regs -> (
        let operation_replacing_store =
          match select_store_result with
          | Maybe_out_of_range -> None
          | Rewritten (op, _) -> if is_store op then None else Some op
          | Use_default -> None (* see above *)
        in
        match operation_replacing_store with
        | None ->
          for i = 0 to Array.length regs - 1 do
            let r = regs.(i) in
            let chunk : Cmm.memory_chunk =
              match r.Reg.typ with
              | Float -> Double
              | Float32 -> Single { reg = Float32 }
              | Vec128 ->
                (* 128-bit memory operations are unaligned by default. Aligned
                   (big)array operations are handled separately via cmm. *)
                Onetwentyeight_unaligned
              | Val | Addr | Int -> Word_val
              | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"
            in
            let is_out_of_range :
                Cfg_selectgen_target_intf.is_store_out_of_range_result =
              match select_store_result with
              | Rewritten _ | Use_default -> Within_range
              | Maybe_out_of_range ->
                Target.is_store_out_of_range chunk ~byte_offset:!byte_offset
            in
            let new_addressing_mode =
              match is_out_of_range with
              | Within_range -> !addressing_mode
              | Out_of_range ->
                (* Use a temporary to store the address [!base + offset]. *)
                let tmp = Reg.createv Cmm.typ_int in
                (* CR-someday xclerc: Now that this code in the "generic" part,
                   it is maybe a bit unexpected to assume there is no better
                   sequence to emit x += k. That being said, it is a corner
                   case. *)
                insert_debug env sub_cfg
                  (Op (SU.make_const_int (Nativeint.of_int !byte_offset)))
                  dbg [||] tmp;
                insert_debug env sub_cfg (Op (Operation.Intop Iadd)) dbg
                  (Array.append !base tmp) tmp;
                (* Use the temporary as the new base address. *)
                base := tmp;
                Arch.identity_addressing
            in
            insert_debug env sub_cfg
              (Op (Store (chunk, new_addressing_mode, false)))
              dbg
              (Array.append [| r |] regs_addr)
              [||];
            let size = SU.size_component r.Reg.typ in
            addressing_mode := Arch.offset_addressing new_addressing_mode size;
            byte_offset := !byte_offset + size
          done
        | Some op ->
          insert_debug env sub_cfg (Op op) dbg (Array.append regs regs_addr) [||];
          let size = SU.size_expr env original_arg in
          addressing_mode := Arch.offset_addressing !addressing_mode size;
          byte_offset := !byte_offset + size)
      | Never_returns ->
        Misc.fatal_error
          "emit_expr did not return any registers in [emit_stores]"
    in
    List.iter for_one_arg args

  (* Emit an expression.

     [bound_name] is the name that will be bound to the result of evaluating the
     expression, if such exists. This is used for emitting debugging info.

     Returns:

     - [Never_returns] if the expression does not finish normally (e.g. raises)

     - [Ok rs] if the expression yields a result in registers [rs] *)
  and emit_expr env sub_cfg (exp : Cmm.expression) ~bound_name :
      Reg.t array Or_never_returns.t =
    match exp with
    | Cconst_int (n, _dbg) ->
      let r = Reg.createv Cmm.typ_int in
      Ok (insert_op env sub_cfg (SU.make_const_int (Nativeint.of_int n)) [||] r)
    | Cconst_natint (n, _dbg) ->
      let r = Reg.createv Cmm.typ_int in
      Ok (insert_op env sub_cfg (SU.make_const_int n) [||] r)
    | Cconst_float32 (n, _dbg) ->
      let r = Reg.createv Cmm.typ_float32 in
      Ok
        (insert_op env sub_cfg
           (SU.make_const_float32 (Int32.bits_of_float n))
           [||] r)
    | Cconst_float (n, _dbg) ->
      let r = Reg.createv Cmm.typ_float in
      Ok
        (insert_op env sub_cfg
           (SU.make_const_float (Int64.bits_of_float n))
           [||] r)
    | Cconst_vec128 (bits, _dbg) ->
      let r = Reg.createv Cmm.typ_vec128 in
      Ok (insert_op env sub_cfg (SU.make_const_vec128 bits) [||] r)
    | Cconst_symbol (n, _dbg) ->
      (* Cconst_symbol _ evaluates to a statically-allocated address, so its
         value fits in a typ_int register and is never changed by the GC.

         Some Cconst_symbols point to statically-allocated blocks, some of which
         may point to heap values. However, any such blocks will be registered
         in the compilation unit's global roots structure, so adding this
         register to the frame table would be redundant *)
      let r = Reg.createv Cmm.typ_int in
      Ok (insert_op env sub_cfg (SU.make_const_symbol n) [||] r)
    | Cvar v -> (
      try Ok (SU.env_find v env)
      with Not_found ->
        Misc.fatal_error ("Selection.emit_expr: unbound var " ^ V.unique_name v)
      )
    | Clet (v, e1, e2) -> (
      match emit_expr env sub_cfg e1 ~bound_name:(Some v) with
      | Never_returns -> Never_returns
      | Ok r1 -> emit_expr (bind_let env sub_cfg v r1) sub_cfg e2 ~bound_name)
    | Cphantom_let (_var, _defining_expr, body) ->
      emit_expr env sub_cfg body ~bound_name
    | Ctuple [] -> Ok [||]
    | Ctuple exp_list -> (
      match emit_parts_list env sub_cfg exp_list with
      | Never_returns -> Never_returns
      | Ok (simple_list, ext_env) -> emit_tuple ext_env sub_cfg simple_list)
    | Cop (Craise k, args, dbg) -> emit_expr_raise env sub_cfg k args dbg
    | Cop (Copaque, args, dbg) -> (
      match emit_parts_list env sub_cfg args with
      | Never_returns -> Never_returns
      | Ok (simple_args, env) ->
        let* rs = emit_tuple env sub_cfg simple_args in
        Ok (insert_op_debug env sub_cfg (SU.make_opaque ()) dbg rs rs))
    | Cop (Ctuple_field (field, fields_layout), [arg], _dbg) -> (
      match emit_expr env sub_cfg arg ~bound_name:None with
      | Never_returns -> Never_returns
      | Ok loc_exp ->
        let flat_size a =
          Array.fold_left (fun acc t -> acc + Array.length t) 0 a
        in
        assert (Array.length loc_exp = flat_size fields_layout);
        let before = Array.sub fields_layout 0 field in
        let size_before = flat_size before in
        let field_slice =
          Array.sub loc_exp size_before (Array.length fields_layout.(field))
        in
        Ok field_slice)
    | Cop (op, args, dbg) -> emit_expr_op env sub_cfg bound_name op args dbg
    | Csequence (e1, e2) -> (
      match emit_expr env sub_cfg e1 ~bound_name:None with
      | Never_returns -> Never_returns
      | Ok _ -> emit_expr env sub_cfg e2 ~bound_name)
    | Cifthenelse (econd, ifso_dbg, eif, ifnot_dbg, eelse, dbg) ->
      emit_expr_ifthenelse env sub_cfg bound_name econd ifso_dbg eif ifnot_dbg
        eelse dbg
    | Cswitch (esel, index, ecases, dbg) ->
      emit_expr_switch env sub_cfg bound_name esel index ecases dbg
    | Ccatch (_, [], e1) -> emit_expr env sub_cfg e1 ~bound_name
    | Ccatch (rec_flag, handlers, body) ->
      emit_expr_catch env sub_cfg bound_name rec_flag handlers body
    | Cexit (lbl, args, traps) -> emit_expr_exit env sub_cfg lbl args traps

  (* Emit an expression in tail position of a function. *)
  and emit_tail env sub_cfg (exp : Cmm.expression) =
    match exp with
    | Clet (v, e1, e2) -> (
      match emit_expr env sub_cfg e1 ~bound_name:None with
      | Never_returns -> ()
      | Ok r1 -> emit_tail (bind_let env sub_cfg v r1) sub_cfg e2)
    | Cphantom_let (_var, _defining_expr, body) -> emit_tail env sub_cfg body
    | Cop ((Capply (ty, Rc_normal) as op), args, dbg) ->
      emit_tail_apply env sub_cfg ty op args dbg
    | Csequence (e1, e2) -> (
      match emit_expr env sub_cfg e1 ~bound_name:None with
      | Never_returns -> ()
      | Ok _ -> emit_tail env sub_cfg e2)
    | Cifthenelse (econd, ifso_dbg, eif, ifnot_dbg, eelse, dbg) ->
      emit_tail_ifthenelse env sub_cfg econd ifso_dbg eif ifnot_dbg eelse dbg
    | Cswitch (esel, index, ecases, dbg) ->
      emit_tail_switch env sub_cfg esel index ecases dbg
    | Ccatch (_, [], e1) -> emit_tail env sub_cfg e1
    | Ccatch (rec_flag, handlers, e1) ->
      emit_tail_catch env sub_cfg rec_flag handlers e1
    | Cop _ | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
    | Cconst_symbol _ | Cconst_vec128 _ | Cvar _ | Ctuple _ | Cexit _ ->
      emit_return env sub_cfg exp (SU.pop_all_traps env)

  and emit_expr_raise (env : SU.environment) sub_cfg k
      (args : Cmm.expression list) dbg : _ Or_never_returns.t =
    let* r1 = emit_tuple env sub_cfg args in
    let extra_args_regs =
      match env.trap_stack with
      | Uncaught ->
        (* Function-level or toplevel exception continuations never have extra
           args. *)
        []
      | Specific_trap (cont, _trap_stack) ->
        SU.env_find_regs_for_exception_extra_args cont env
    in
    (* Populate the distinguished extra args registers, for the current
       exception handler, with the extra args for this particular raise. *)
    let rd = Array.concat ([| Proc.loc_exn_bucket |] :: extra_args_regs) in
    Array.iter2
      (fun r1 rd -> SU.insert env sub_cfg (Op Move) [| r1 |] [| rd |])
      r1 rd;
    SU.insert_debug' env sub_cfg (Cfg.Raise k) dbg rd [||];
    SU.set_traps_for_raise env;
    Never_returns

  and emit_expr_op env sub_cfg bound_name op args dbg : _ Or_never_returns.t =
    match emit_parts_list env sub_cfg args with
    | Never_returns -> Never_returns
    | Ok (simple_args, env) -> (
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let add_naming_op_for_bound_name sub_cfg regs =
        match bound_name with
        | None -> ()
        | Some bound_name ->
          let provenance = VP.provenance bound_name in
          if Option.is_some provenance
          then
            let bound_name = VP.var bound_name in
            let naming_op =
              Operation.Name_for_debugger
                { ident = bound_name;
                  provenance;
                  which_parameter = None;
                  is_assignment = false;
                  regs
                }
            in
            insert_debug env sub_cfg (Op naming_op) Debuginfo.none [||] [||]
      in
      let ty = SU.oper_result_type op in
      let label_after = Cmm.new_label () in
      let new_op, new_args = select_operation op simple_args dbg ~label_after in
      match new_op with
      | Terminator (Call { op = Indirect; label_after } as term) ->
        let* r1 = emit_tuple env sub_cfg new_args in
        let rarg = Array.sub r1 1 (Array.length r1 - 1) in
        let rd = Reg.createv ty in
        let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv rarg) in
        let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
        let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
        SU.insert_move_args env sub_cfg rarg loc_arg stack_ofs;
        SU.insert_debug' env sub_cfg term dbg
          (Array.append [| r1.(0) |] loc_arg)
          loc_res;
        Sub_cfg.add_never_block sub_cfg ~label:label_after;
        (* The destination registers (as per the procedure calling convention)
           need to be named right now, otherwise the result of the function call
           may be unavailable in the debugger immediately after the call. *)
        add_naming_op_for_bound_name sub_cfg loc_res;
        SU.insert_move_results env sub_cfg loc_res rd stack_ofs;
        SU.set_traps_for_raise env;
        Ok rd
      | Terminator (Call { op = Direct _; label_after } as term) ->
        let* r1 = emit_tuple env sub_cfg new_args in
        let rd = Reg.createv ty in
        let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv r1) in
        let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
        let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
        SU.insert_move_args env sub_cfg r1 loc_arg stack_ofs;
        SU.insert_debug' env sub_cfg term dbg loc_arg loc_res;
        add_naming_op_for_bound_name sub_cfg loc_res;
        Sub_cfg.add_never_block sub_cfg ~label:label_after;
        SU.insert_move_results env sub_cfg loc_res rd stack_ofs;
        SU.set_traps_for_raise env;
        Ok rd
      | Terminator
          (Prim { op = External ({ ty_args; ty_res; _ } as r); label_after }) ->
        let* loc_arg, stack_ofs =
          emit_extcall_args env sub_cfg ty_args new_args dbg
        in
        let rd = Reg.createv ty_res in
        let term =
          Cfg.Prim { op = External { r with stack_ofs }; label_after }
        in
        let loc_res =
          SU.insert_op_debug' env sub_cfg term dbg loc_arg
            (Proc.loc_external_results (Reg.typv rd))
        in
        Sub_cfg.add_never_block sub_cfg ~label:label_after;
        add_naming_op_for_bound_name sub_cfg loc_res;
        SU.insert_move_results env sub_cfg loc_res rd stack_ofs;
        SU.set_traps_for_raise env;
        Ok rd
      | Terminator (Prim { op = Probe _; label_after } as term) ->
        let* r1 = emit_tuple env sub_cfg new_args in
        let rd = Reg.createv ty in
        let rd = SU.insert_op_debug' env sub_cfg term dbg r1 rd in
        SU.set_traps_for_raise env;
        Sub_cfg.add_never_block sub_cfg ~label:label_after;
        Ok rd
      | Terminator (Call_no_return ({ func_symbol; ty_args; _ } as r)) ->
        let* loc_arg, stack_ofs =
          emit_extcall_args env sub_cfg ty_args new_args dbg
        in
        let keep_for_checking =
          !SU.current_function_is_check_enabled
          && String.equal func_symbol Cmm.caml_flambda2_invalid
        in
        let returns, ty =
          if keep_for_checking then true, Cmm.typ_int else false, ty
        in
        let rd = Reg.createv ty in
        let label = Cmm.new_label () in
        let r = { r with stack_ofs } in
        let term : Cfg.terminator =
          if keep_for_checking
          then Prim { op = External r; label_after = label }
          else Call_no_return r
        in
        let (_ : Reg.t array) =
          SU.insert_op_debug' env sub_cfg term dbg loc_arg
            (Proc.loc_external_results (Reg.typv rd))
        in
        SU.set_traps_for_raise env;
        if returns
        then (
          Sub_cfg.add_never_block sub_cfg ~label;
          Ok rd)
        else Never_returns
      | Basic (Op (Alloc { bytes = _; mode; dbginfo = [placeholder] })) ->
        let rd = Reg.createv Cmm.typ_val in
        let bytes = SU.size_expr env (Ctuple new_args) in
        let alloc_words = (bytes + Arch.size_addr - 1) / Arch.size_addr in
        let op =
          Operation.Alloc
            { bytes = alloc_words * Arch.size_addr;
              dbginfo = [{ placeholder with alloc_words; alloc_dbg = dbg }];
              mode
            }
        in
        insert_debug env sub_cfg (Op op) dbg [||] rd;
        add_naming_op_for_bound_name sub_cfg rd;
        emit_stores env sub_cfg dbg new_args rd;
        SU.set_traps_for_raise env;
        Ok rd
      | Basic (Op (Alloc { bytes = _; mode = _; dbginfo })) ->
        Misc.fatal_errorf
          "Selection Alloc: expected a single placehold in dbginfo, found %d"
          (List.length dbginfo)
      | Basic (Op op) ->
        let* r1 = emit_tuple env sub_cfg new_args in
        let rd = Reg.createv ty in
        add_naming_op_for_bound_name sub_cfg rd;
        Ok (insert_op_debug env sub_cfg op dbg r1 rd)
      | Basic basic ->
        Misc.fatal_errorf "unexpected basic (%a)" Cfg.dump_basic basic
      | Terminator term ->
        Misc.fatal_errorf "unexpected terminator (%a)"
          (Cfg.dump_terminator ~sep:"")
          term)

  and emit_expr_ifthenelse env sub_cfg bound_name econd _ifso_dbg eif
      (_ifnot_dbg : Debuginfo.t) eelse (_dbg : Debuginfo.t) :
      _ Or_never_returns.t =
    (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
    let cond, earg = select_condition econd in
    match emit_expr env sub_cfg earg ~bound_name:None with
    | Never_returns -> Never_returns
    | Ok rarg ->
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let rif, sub_if = emit_new_sub_cfg env eif ~bound_name in
      let relse, sub_else = emit_new_sub_cfg env eelse ~bound_name in
      let r = SU.join env rif sub_if relse sub_else ~bound_name in
      let term_desc =
        SU.terminator_of_test cond
          ~label_true:(Sub_cfg.start_label sub_if)
          ~label_false:(Sub_cfg.start_label sub_else)
      in
      Sub_cfg.update_exit_terminator sub_cfg term_desc ~arg:rarg;
      Sub_cfg.join ~from:[sub_if; sub_else] ~to_:sub_cfg;
      r

  and emit_expr_switch env sub_cfg bound_name esel index ecases
      (_dbg : Debuginfo.t) : _ Or_never_returns.t =
    (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
    match emit_expr env sub_cfg esel ~bound_name:None with
    | Never_returns -> Never_returns
    | Ok rsel ->
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let sub_cases : (Reg.t array Or_never_returns.t * Sub_cfg.t) array =
        Array.map
          (fun (case, _dbg) -> emit_new_sub_cfg env case ~bound_name)
          ecases
      in
      let r = SU.join_array env sub_cases ~bound_name in
      let subs = Array.map (fun (_, sub_cfg) -> sub_cfg) sub_cases in
      let term_desc : Cfg.terminator =
        Switch (Array.map (fun idx -> Sub_cfg.start_label subs.(idx)) index)
      in
      Sub_cfg.update_exit_terminator sub_cfg term_desc ~arg:rsel;
      Sub_cfg.join ~from:(Array.to_list subs) ~to_:sub_cfg;
      r

  and emit_expr_catch env sub_cfg bound_name (flag : Cmm.ccatch_flag) handlers
      body =
    let handlers =
      List.map
        (fun (nfail, ids, e2, dbg, is_cold) ->
          let rs =
            List.map
              (fun (id, typ) ->
                let r = Reg.createv_with_id ~id:(VP.var id) typ in
                r)
              ids
          in
          nfail, ids, rs, e2, dbg, is_cold)
        handlers
    in
    let env, handlers_map =
      (* Since the handlers may be recursive, and called from the body, the same
         environment is used for translating both the handlers and the body. *)
      List.fold_left
        (fun (env, map) (nfail, ids, rs, e2, dbg, is_cold) ->
          let label = Cmm.new_label () in
          let env, r = SU.env_add_static_exception nfail rs env label in
          env, Int.Map.add nfail (r, (ids, rs, e2, dbg, is_cold, label)) map)
        (env, Int.Map.empty) handlers
    in
    let r_body, sub_body = emit_new_sub_cfg env body ~bound_name in
    let translate_one_handler _nfail
        (trap_info, (ids, rs, e2, _dbg, _is_cold, label)) =
      assert (List.length ids = List.length rs);
      let trap_stack, e2 =
        match (!trap_info : SU.trap_stack_info) with
        | Unreachable ->
          assert (Cmm.is_exn_handler flag);
          Lazy.force unreachable_handler
        | Reachable t -> t, e2
      in
      let ids_and_rs = List.combine ids rs in
      let new_env =
        List.fold_left
          (fun env ((id, _typ), r) -> SU.env_add id r env)
          (SU.env_set_trap_stack env trap_stack)
          ids_and_rs
      in
      let r, sub =
        emit_new_sub_cfg new_env e2 ~bound_name:None ~at_start:(fun sub_cfg ->
            List.iter
              (fun ((var, _typ), r) ->
                let provenance = VP.provenance var in
                if Option.is_some provenance
                then
                  let var = VP.var var in
                  let naming_op =
                    Operation.Name_for_debugger
                      { ident = var;
                        provenance;
                        which_parameter = None;
                        is_assignment = false;
                        regs = r
                      }
                  in
                  insert_debug new_env sub_cfg (Op naming_op) Debuginfo.none
                    [||] [||])
              ids_and_rs)
      in
      (rs, label), (r, sub)
    in
    let rec build_all_reachable_handlers ~already_built ~not_built =
      let not_built, to_build =
        Int.Map.partition
          (fun _n (r, _) ->
            match !r with SU.Unreachable -> true | SU.Reachable _ -> false)
          not_built
      in
      if Int.Map.is_empty to_build
      then already_built
      else
        let already_built =
          Int.Map.fold
            (fun nfail handler already_built ->
              translate_one_handler nfail handler :: already_built)
            to_build already_built
        in
        build_all_reachable_handlers ~already_built ~not_built
    in
    let l =
      match flag with
      | Normal | Recursive ->
        build_all_reachable_handlers ~already_built:[] ~not_built:handlers_map
        (* Note: we're dropping unreachable handlers here *)
      | Exn_handler ->
        (* We cannot drop exception handlers as some trap instructions may refer
           to them even if they're unreachable. Instead, [translate_one_handler]
           will generate a dummy handler for the unreachable cases. *)
        Int.Map.fold
          (fun nfail handler all_handlers ->
            translate_one_handler nfail handler :: all_handlers)
          handlers_map []
    in
    let a = Array.of_list ((r_body, sub_body) :: List.map snd l) in
    let r = SU.join_array env a ~bound_name in
    assert (Sub_cfg.exit_has_never_terminator sub_cfg);
    let sub_handlers =
      List.map
        (fun ((rs, label), (_, sub_handler)) ->
          Sub_cfg.add_empty_block_at_start sub_handler ~label;
          setup_catch_handler flag rs sub_handler;
          sub_handler)
        l
    in
    let term_desc = Cfg.Always (Sub_cfg.start_label sub_body) in
    Sub_cfg.update_exit_terminator sub_cfg term_desc;
    Sub_cfg.join ~from:(sub_body :: sub_handlers) ~to_:sub_cfg;
    r

  and emit_expr_exit env sub_cfg (lbl : Cmm.exit_label) args traps :
      _ Or_never_returns.t =
    match emit_parts_list env sub_cfg args with
    | Never_returns -> Never_returns
    | Ok (simple_list, ext_env) -> (
      match lbl with
      | Lbl nfail ->
        let* src = emit_tuple ext_env sub_cfg simple_list in
        let handler =
          try SU.env_find_static_exception nfail env
          with Not_found ->
            Misc.fatal_error
              ("Selection.emit_expr: unbound label "
             ^ Stdlib.Int.to_string nfail)
        in
        (* Intermediate registers to handle cases where some registers from src
           are present in dest *)
        let tmp_regs = Reg.createv_with_typs src in
        (* Ccatch registers must not contain out of heap pointers *)
        Array.iter
          (fun reg ->
            match reg.Reg.typ with
            | Addr -> assert false
            | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"
            | Val | Int | Float | Vec128 | Float32 -> ())
          src;
        SU.insert_moves env sub_cfg src tmp_regs;
        SU.insert_moves env sub_cfg tmp_regs (Array.concat handler.regs);
        assert (Sub_cfg.exit_has_never_terminator sub_cfg);
        List.iter
          (fun trap ->
            let instr_desc =
              match trap with
              | Cmm.Push handler_id ->
                let lbl_handler =
                  (SU.env_find_static_exception handler_id env).label
                in
                Cfg.Pushtrap { lbl_handler }
              | Cmm.Pop handler_id ->
                let lbl_handler =
                  (SU.env_find_static_exception handler_id env).label
                in
                Cfg.Poptrap { lbl_handler }
            in
            Sub_cfg.add_instruction sub_cfg instr_desc [||] [||] Debuginfo.none)
          traps;
        Sub_cfg.update_exit_terminator sub_cfg (Always handler.label);
        SU.set_traps nfail handler.SU.traps_ref env.SU.trap_stack traps;
        Never_returns
      | Return_lbl -> (
        match simple_list with
        | [expr] ->
          emit_return ext_env sub_cfg expr traps;
          Never_returns
        | [] -> Misc.fatal_error "Selection.emit_expr: Return without arguments"
        | _ :: _ :: _ ->
          Misc.fatal_error "Selection.emit_expr: Return with too many arguments"
        ))

  and emit_new_sub_cfg ?at_start env exp ~bound_name : _ * Sub_cfg.t =
    let sub_cfg = Sub_cfg.make_empty () in
    (match at_start with None -> () | Some f -> f sub_cfg);
    let r = emit_expr env sub_cfg exp ~bound_name in
    r, sub_cfg

  (* Same, but in tail position *)

  and emit_return env sub_cfg exp traps =
    assert (Sub_cfg.exit_has_never_terminator sub_cfg);
    insert_return env sub_cfg (emit_expr env sub_cfg exp ~bound_name:None) traps

  and emit_tail_apply env sub_cfg ty op args dbg =
    match emit_parts_list env sub_cfg args with
    | Never_returns -> ()
    | Ok (simple_args, env) -> (
      let label_after = Cmm.new_label () in
      let new_op, new_args = select_operation op simple_args dbg ~label_after in
      match new_op with
      | Terminator (Call { op = Indirect; label_after } as term) ->
        let** r1 = emit_tuple env sub_cfg new_args in
        let rd = Reg.createv ty in
        let rarg = Array.sub r1 1 (Array.length r1 - 1) in
        let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv rarg) in
        let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
        let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
        if stack_ofs = 0 && SU.trap_stack_is_empty env
        then (
          let call = Cfg.Tailcall_func Indirect in
          SU.insert_moves env sub_cfg rarg loc_arg;
          SU.insert_debug' env sub_cfg call dbg
            (Array.append [| r1.(0) |] loc_arg)
            [||])
        else (
          SU.insert_move_args env sub_cfg rarg loc_arg stack_ofs;
          SU.insert_debug' env sub_cfg term dbg
            (Array.append [| r1.(0) |] loc_arg)
            loc_res;
          Sub_cfg.add_never_block sub_cfg ~label:label_after;
          SU.set_traps_for_raise env;
          SU.insert env sub_cfg (Op (Stackoffset (-stack_ofs))) [||] [||];
          insert_return env sub_cfg (Ok loc_res) (SU.pop_all_traps env))
      | Terminator (Call { op = Direct func; label_after } as term) ->
        let** r1 = emit_tuple env sub_cfg new_args in
        let rd = Reg.createv ty in
        let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv r1) in
        let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
        let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
        if String.equal func.sym_name !SU.current_function_name
           && SU.trap_stack_is_empty env
        then (
          let call = Cfg.Tailcall_self { destination = env.SU.tailrec_label } in
          let loc_arg' =
            assert (stack_ofs >= 0);
            if stack_ofs = 0 then loc_arg else Proc.loc_parameters (Reg.typv r1)
          in
          SU.insert_moves env sub_cfg r1 loc_arg';
          SU.insert_debug' env sub_cfg call dbg loc_arg' [||])
        else if stack_ofs = 0 && SU.trap_stack_is_empty env
        then (
          let call = Cfg.Tailcall_func (Direct func) in
          SU.insert_moves env sub_cfg r1 loc_arg;
          SU.insert_debug' env sub_cfg call dbg loc_arg [||])
        else (
          SU.insert_move_args env sub_cfg r1 loc_arg stack_ofs;
          SU.insert_debug' env sub_cfg term dbg loc_arg loc_res;
          Sub_cfg.add_never_block sub_cfg ~label:label_after;
          SU.set_traps_for_raise env;
          SU.insert env sub_cfg (Op (Stackoffset (-stack_ofs))) [||] [||];
          insert_return env sub_cfg (Ok loc_res) (SU.pop_all_traps env))
      | _ -> Misc.fatal_error "Cfg_selectgen.emit_tail")

  and emit_tail_ifthenelse env sub_cfg econd (_ifso_dbg : Debuginfo.t) eif
      (_ifnot_dbg : Debuginfo.t) eelse (_dbg : Debuginfo.t) =
    (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
    let cond, earg = select_condition econd in
    match emit_expr env sub_cfg earg ~bound_name:None with
    | Never_returns -> ()
    | Ok rarg ->
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let sub_if = emit_tail_new_sub_cfg env eif in
      let sub_else = emit_tail_new_sub_cfg env eelse in
      let term_desc =
        SU.terminator_of_test cond
          ~label_true:(Sub_cfg.start_label sub_if)
          ~label_false:(Sub_cfg.start_label sub_else)
      in
      Sub_cfg.update_exit_terminator sub_cfg term_desc ~arg:rarg;
      Sub_cfg.join_tail ~from:[sub_if; sub_else] ~to_:sub_cfg

  and emit_tail_switch env sub_cfg esel index ecases (_dbg : Debuginfo.t) =
    (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
    match emit_expr env sub_cfg esel ~bound_name:None with
    | Never_returns -> ()
    | Ok rsel ->
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let sub_cases =
        Array.map (fun (case, _dbg) -> emit_tail_new_sub_cfg env case) ecases
      in
      let term_desc : Cfg.terminator =
        Switch
          (Array.map (fun idx -> Sub_cfg.start_label sub_cases.(idx)) index)
      in
      Sub_cfg.update_exit_terminator sub_cfg term_desc ~arg:rsel;
      Sub_cfg.join_tail ~from:(Array.to_list sub_cases) ~to_:sub_cfg

  and emit_tail_catch env sub_cfg (flag : Cmm.ccatch_flag) handlers e1 =
    let handlers =
      List.map
        (fun (nfail, ids, e2, dbg, is_cold) ->
          let rs =
            List.map
              (fun (id, typ) ->
                let r = Reg.createv_with_id ~id:(VP.var id) typ in
                r)
              ids
          in
          nfail, ids, rs, e2, dbg, is_cold)
        handlers
    in
    let env, handlers_map =
      List.fold_left
        (fun (env, map) (nfail, ids, rs, e2, dbg, is_cold) ->
          let label = Cmm.new_label () in
          let env, r = SU.env_add_static_exception nfail rs env label in
          env, Int.Map.add nfail (r, (ids, rs, e2, dbg, is_cold, label)) map)
        (env, Int.Map.empty) handlers
    in
    assert (Sub_cfg.exit_has_never_terminator sub_cfg);
    let s_body = emit_tail_new_sub_cfg env e1 in
    let translate_one_handler _nfail
        (trap_info, (ids, rs, e2, _dbg, _is_cold, label)) =
      assert (List.length ids = List.length rs);
      let trap_stack, e2 =
        match (!trap_info : SU.trap_stack_info) with
        | Unreachable ->
          assert (Cmm.is_exn_handler flag);
          Lazy.force unreachable_handler
        | Reachable t -> t, e2
      in
      let ids_and_rs = List.combine ids rs in
      let new_env =
        List.fold_left
          (fun env ((id, _typ), r) -> SU.env_add id r env)
          (SU.env_set_trap_stack env trap_stack)
          ids_and_rs
      in
      let seq : Sub_cfg.t =
        emit_tail_new_sub_cfg new_env e2 ~at_start:(fun sub_cfg ->
            List.iter
              (fun ((var, _typ), r) ->
                let provenance = VP.provenance var in
                if Option.is_some provenance
                then
                  let var = VP.var var in
                  let naming_op =
                    Operation.Name_for_debugger
                      { ident = var;
                        provenance;
                        which_parameter = None;
                        is_assignment = false;
                        regs = r
                      }
                  in
                  insert_debug new_env sub_cfg (Op naming_op) Debuginfo.none
                    [||] [||])
              ids_and_rs)
      in
      Sub_cfg.add_empty_block_at_start seq ~label;
      rs, seq
    in
    let rec build_all_reachable_handlers ~already_built ~not_built =
      let not_built, to_build =
        Int.Map.partition
          (fun _n (r, _) ->
            match !r with SU.Unreachable -> true | SU.Reachable _ -> false)
          not_built
      in
      if Int.Map.is_empty to_build
      then already_built
      else
        let already_built =
          Int.Map.fold
            (fun nfail handler already_built ->
              translate_one_handler nfail handler :: already_built)
            to_build already_built
        in
        build_all_reachable_handlers ~already_built ~not_built
    in
    let new_handlers : (Reg.t array list * Sub_cfg.t) list =
      match flag with
      | Normal | Recursive ->
        build_all_reachable_handlers ~already_built:[] ~not_built:handlers_map
        (* Note: we're dropping unreachable handlers here *)
      | Exn_handler ->
        (* We cannot drop exception handlers as some trap instructions may refer
           to them even if they're unreachable. Instead, [translate_one_handler]
           will generate a dummy handler for the unreachable cases. *)
        Int.Map.fold
          (fun nfail handler all_handlers ->
            translate_one_handler nfail handler :: all_handlers)
          handlers_map []
    in
    assert (Sub_cfg.exit_has_never_terminator sub_cfg);
    let term_desc = Cfg.Always (Sub_cfg.start_label s_body) in
    Sub_cfg.update_exit_terminator sub_cfg term_desc;
    let s_handlers =
      List.map
        (fun (rs, s) ->
          setup_catch_handler flag rs s;
          s)
        new_handlers
    in
    Sub_cfg.join_tail ~from:(s_body :: s_handlers) ~to_:sub_cfg

  and emit_tail_new_sub_cfg ?at_start env exp : Sub_cfg.t =
    let sub_cfg = Sub_cfg.make_empty () in
    (match at_start with None -> () | Some f -> f sub_cfg);
    emit_tail env sub_cfg exp;
    sub_cfg

  (* Sequentialization of a function definition *)

  let emit_fundecl ~future_funcnames f =
    SU.current_function_name := f.Cmm.fun_name.sym_name;
    SU.current_function_is_check_enabled
      := Zero_alloc_checker.is_check_enabled f.Cmm.fun_codegen_options
           f.Cmm.fun_name.sym_name f.Cmm.fun_dbg;
    let num_regs_per_arg = Array.make (List.length f.Cmm.fun_args) 0 in
    let rargs =
      List.mapi
        (fun arg_index (var, ty) ->
          let r = Reg.createv_with_id ~id:(VP.var var) ty in
          num_regs_per_arg.(arg_index) <- Array.length r;
          r)
        f.Cmm.fun_args
    in
    let rarg = Array.concat rargs in
    let loc_arg = Proc.loc_parameters (Reg.typv rarg) in
    let tailrec_label = Cmm.new_label () in
    let env = SU.env_create ~tailrec_label in
    let env =
      List.fold_right2
        (fun (id, _ty) r env -> SU.env_add id r env)
        f.Cmm.fun_args rargs env
    in
    let body = Sub_cfg.make_empty () in
    let loc_arg_index = ref 0 in
    List.iteri
      (fun param_index (var, _ty) ->
        let provenance = VP.provenance var in
        let var = VP.var var in
        let num_regs_for_arg = num_regs_per_arg.(param_index) in
        let hard_regs_for_arg =
          Array.init num_regs_for_arg (fun index ->
              loc_arg.(!loc_arg_index + index))
        in
        loc_arg_index := !loc_arg_index + num_regs_for_arg;
        if Option.is_some provenance
        then
          let naming_op =
            Operation.Name_for_debugger
              { ident = var;
                provenance;
                which_parameter = Some param_index;
                is_assignment = false;
                regs = hard_regs_for_arg
              }
          in
          insert_debug env body (Op naming_op) Debuginfo.none hard_regs_for_arg
            [||])
      f.Cmm.fun_args;
    SU.insert_moves env body loc_arg rarg;
    let prologue_poll_instr_id =
      insert_op_debug_returning_id env body Operation.Poll Debuginfo.none [||]
        [||]
    in
    emit_tail env body f.Cmm.fun_body;
    let cfg =
      (* note: we set `fun_contains_calls` to `true` here, but will compute its
         proper value below, after possibly removing the prologue poll
         instruction. It is not very satisfactory, but as noted in the CR below,
         we should revisit the way we handle polling points. *)
      Cfg.create ~fun_name:f.Cmm.fun_name.sym_name ~fun_args:loc_arg
        ~fun_codegen_options:
          (Cfg.of_cmm_codegen_option f.Cmm.fun_codegen_options)
        ~fun_dbg:f.Cmm.fun_dbg ~fun_contains_calls:true
        ~fun_num_stack_slots:(Stack_class.Tbl.make 0) ~fun_poll:f.Cmm.fun_poll
        ~instruction_id:Cfg.instr_id
    in
    let layout = DLL.make_empty () in
    let entry_block =
      Cfg.make_empty_block ~label:(Cfg.entry_label cfg)
        (Cfg.make_instr (Cfg.Always tailrec_label) [||] [||] Debuginfo.none)
    in
    DLL.add_begin entry_block.body
      (Cfg.make_instr Cfg.Prologue [||] [||] Debuginfo.none);
    Cfg.add_block_exn cfg entry_block;
    DLL.add_end layout entry_block.start;
    let tailrec_block =
      Cfg.make_empty_block ~label:tailrec_label
        (Cfg.make_instr
           (Cfg.Always (Sub_cfg.start_label body))
           [||] [||] Debuginfo.none)
    in
    Cfg.add_block_exn cfg tailrec_block;
    DLL.add_end layout tailrec_block.start;
    let delete_prologue_poll =
      (* CR mshinwell/xclerc: find a neater way of doing this rather than making
         a special case for the [optimistic_prologue_poll_instr_id]. *)
      not
        (Cfg_polling.requires_prologue_poll ~future_funcnames
           ~fun_name:f.Cmm.fun_name.sym_name
           ~optimistic_prologue_poll_instr_id:prologue_poll_instr_id cfg)
    in
    let found_prologue_poll = ref false in
    Sub_cfg.iter_basic_blocks body ~f:(fun (block : Cfg.basic_block) ->
        if delete_prologue_poll && not !found_prologue_poll
        then
          DLL.filter_left block.body
            ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
              let is_prologue_poll =
                InstructionId.equal instr.id prologue_poll_instr_id
              in
              if is_prologue_poll then found_prologue_poll := true;
              not is_prologue_poll);
        if not (Cfg.is_never_terminator block.terminator.desc)
        then (
          block.can_raise <- Cfg.can_raise_terminator block.terminator.desc;
          if Cfg.is_return_terminator block.terminator.desc
          then
            DLL.add_end block.body
              (Cfg.make_instr Cfg.Reloadretaddr [||] [||] Debuginfo.none);
          Cfg.add_block_exn cfg block;
          DLL.add_end layout block.start)
        else assert (DLL.is_empty block.body));
    if delete_prologue_poll && not !found_prologue_poll
    then Misc.fatal_error "Did not find [Poll] instruction to delete";
    (* note: `Cfgize.Stack_offset_and_exn.update_cfg` may add edges to the
       graph, and should hence be executed before
       `Cfg.register_predecessors_for_all_blocks`. *)
    SU.Stack_offset_and_exn.update_cfg cfg;
    Cfg.register_predecessors_for_all_blocks cfg;
    let fun_contains_calls =
      Sub_cfg.exists_basic_blocks body ~f:Cfg.basic_block_contains_calls
    in
    let cfg = { cfg with fun_contains_calls } in
    let cfg_with_layout = Cfg_with_layout.create cfg ~layout in
    (* CR xclerc for xclerc: Regalloc_irc_utils.log_cfg_with_infos ~indent:1
       (Cfg_with_infos.make cfg_with_layout); *)
    Cfg_simplify.run cfg_with_layout
end
