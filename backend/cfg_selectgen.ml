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

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Cmm
open Select_utils
module DLL = Flambda_backend_utils.Doubly_linked_list
module Int = Numbers.Int
module VP = Backend_var.With_provenance

let debug = false

let float_test_of_float_comparison :
    Cmm.float_width ->
    Cmm.float_comparison ->
    label_false:Label.t ->
    label_true:Label.t ->
    Cfg.float_test =
 fun width comparison ~label_false ~label_true ->
  let lt, eq, gt, uo =
    match comparison with
    | CFeq -> label_false, label_true, label_false, label_false
    | CFneq -> label_true, label_false, label_true, label_true
    | CFlt -> label_true, label_false, label_false, label_false
    | CFnlt -> label_false, label_true, label_true, label_true
    | CFgt -> label_false, label_false, label_true, label_false
    | CFngt -> label_true, label_true, label_false, label_true
    | CFle -> label_true, label_true, label_false, label_false
    | CFnle -> label_false, label_false, label_true, label_true
    | CFge -> label_false, label_true, label_true, label_false
    | CFnge -> label_true, label_false, label_false, label_true
  in
  { width; lt; eq; gt; uo }

let int_test_of_integer_comparison :
    Cmm.integer_comparison ->
    signed:bool ->
    immediate:int option ->
    label_false:Label.t ->
    label_true:Label.t ->
    Cfg.int_test =
 fun comparison ~signed:is_signed ~immediate:imm ~label_false ~label_true ->
  let lt, eq, gt =
    match comparison with
    | Ceq -> label_false, label_true, label_false
    | Cne -> label_true, label_false, label_true
    | Clt -> label_true, label_false, label_false
    | Cgt -> label_false, label_false, label_true
    | Cle -> label_true, label_true, label_false
    | Cge -> label_false, label_true, label_true
  in
  { lt; eq; gt; is_signed; imm }

let terminator_of_test :
    Simple_operation.test ->
    label_false:Label.t ->
    label_true:Label.t ->
    Cfg.terminator =
 fun test ~label_false ~label_true ->
  let int_test comparison immediate =
    let signed, comparison =
      match comparison with
      | Simple_operation.Isigned comparison -> true, comparison
      | Simple_operation.Iunsigned comparison -> false, comparison
    in
    int_test_of_integer_comparison comparison ~signed ~immediate ~label_false
      ~label_true
  in
  match test with
  | Itruetest -> Truth_test { ifso = label_true; ifnot = label_false }
  | Ifalsetest -> Truth_test { ifso = label_false; ifnot = label_true }
  | Iinttest comparison -> Int_test (int_test comparison None)
  | Iinttest_imm (comparison, value) ->
    Int_test (int_test comparison (Some value))
  | Ifloattest (w, comparison) ->
    Float_test
      (float_test_of_float_comparison w comparison ~label_false ~label_true)
  | Ioddtest -> Parity_test { ifso = label_false; ifnot = label_true }
  | Ieventest -> Parity_test { ifso = label_true; ifnot = label_false }

let invalid_stack_offset = -1

module Stack_offset_and_exn = struct
  (* This module relies on the field `can_raise` of basic blocks but does not
     rely on this field of individual Cfg instructions. This may need to be
     revisited when we remove dead trap handlers and the associated
     pushtrap/poptrap operations. *)
  type handler_stack = Label.t list

  let compute_stack_offset ~stack_offset ~traps =
    stack_offset + (Proc.trap_size_in_bytes * List.length traps)

  let check_and_set_stack_offset :
      'a Cfg.instruction -> stack_offset:int -> traps:handler_stack -> unit =
   fun instr ~stack_offset ~traps ->
    assert (instr.stack_offset = invalid_stack_offset);
    Cfg.set_stack_offset instr (compute_stack_offset ~stack_offset ~traps)

  let process_terminator :
      stack_offset:int ->
      traps:handler_stack ->
      Cfg.terminator Cfg.instruction ->
      int * handler_stack =
   fun ~stack_offset ~traps term ->
    check_and_set_stack_offset term ~stack_offset ~traps;
    match term.desc with
    | Tailcall_self _
      when stack_offset <> 0 || List.compare_length_with traps 0 <> 0 ->
      Misc.fatal_errorf
        "Cfgize.Stack_offset_and_exn.process_terminator: unexpected handler on \
         self tailcall (id=%a)"
        InstructionId.format term.id
    | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
    | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
    | Tailcall_func _ | Call_no_return _ | Call _ | Prim _
    | Specific_can_raise _ ->
      stack_offset, traps

  let rec process_basic :
      Cfg.t ->
      stack_offset:int ->
      traps:handler_stack ->
      Cfg.basic Cfg.instruction ->
      int * handler_stack =
   fun cfg ~stack_offset ~traps instr ->
    check_and_set_stack_offset instr ~stack_offset ~traps;
    match instr.desc with
    | Pushtrap { lbl_handler } ->
      update_block cfg lbl_handler ~stack_offset ~traps;
      stack_offset, lbl_handler :: traps
    | Poptrap -> (
      match traps with
      | [] ->
        Misc.fatal_errorf
          "Cfgize.Stack_offset_and_exn.process_basic: trying to pop from an \
           empty stack (id=%a)"
          InstructionId.format instr.id
      | _ :: traps -> stack_offset, traps)
    | Op (Stackoffset n) -> stack_offset + n, traps
    | Op
        ( Move | Spill | Reload | Const_int _ | Const_float _ | Const_float32 _
        | Const_symbol _ | Const_vec128 _ | Load _ | Store _ | Intop _
        | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _ | Static_cast _
        | Reinterpret_cast _ | Probe_is_enabled _ | Opaque | Begin_region
        | End_region | Specific _ | Name_for_debugger _ | Dls_get | Poll
        | Alloc _ )
    | Reloadretaddr | Prologue ->
      stack_offset, traps
    | Stack_check _ ->
      Misc.fatal_error
        "Cfgize.Stack_offset_and_exn.process_basic: unexpected stack check"

  (* The argument [stack_offset] has a different meaning from the field
     [stack_offset] of Cfg's basic_blocks and instructions. The argument
     [stack_offset] refers to the offset derived from Istackoffset operations
     only, whereas the field also includes the trap stack depth in bytes. Both
     are non-negative. See [compute_stack_offset]. *)
  and update_block :
      Cfg.t -> Label.t -> stack_offset:int -> traps:handler_stack -> unit =
   fun cfg label ~stack_offset ~traps ->
    let block = Cfg.get_block_exn cfg label in
    let was_invalid =
      if block.stack_offset = invalid_stack_offset
      then true
      else (
        if debug
        then
          assert (block.stack_offset = compute_stack_offset ~stack_offset ~traps);
        false)
    in
    if was_invalid
    then (
      block.stack_offset <- compute_stack_offset ~stack_offset ~traps;
      let stack_offset, traps =
        DLL.fold_left block.body ~init:(stack_offset, traps)
          ~f:(fun (stack_offset, traps) instr ->
            process_basic cfg ~stack_offset ~traps instr)
      in
      let stack_offset, traps =
        process_terminator ~stack_offset ~traps block.terminator
      in
      (* non-exceptional successors *)
      Label.Set.iter
        (update_block cfg ~stack_offset ~traps)
        (Cfg.successor_labels ~normal:true ~exn:false block);
      (* exceptional successor *)
      if block.can_raise
      then (
        assert (Option.is_none block.exn);
        match traps with
        | [] -> ()
        | handler_label :: _ -> block.exn <- Some handler_label))

  let update_cfg : Cfg.t -> unit =
   fun cfg ->
    update_block cfg cfg.entry_label ~stack_offset:0 ~traps:[];
    Cfg.iter_blocks cfg ~f:(fun _ block ->
        if block.stack_offset = invalid_stack_offset then block.dead <- true;
        assert (not (block.is_trap_handler && block.dead)))
end

(* CR xclerc for xclerc: maybe_emit_naming_op, join, and join_array below should
   be shared with the ones in Select_utils. *)

let maybe_emit_naming_op env ~bound_name seq regs =
  match bound_name with
  | None -> ()
  | Some bound_name ->
    let provenance = Backend_var.With_provenance.provenance bound_name in
    if Option.is_some provenance
    then
      let bound_name = Backend_var.With_provenance.var bound_name in
      let naming_op =
        Operation.Name_for_debugger
          { ident = bound_name;
            provenance;
            which_parameter = None;
            is_assignment = false;
            regs
          }
      in
      seq#insert_debug env (Cfg.Op naming_op) Debuginfo.none [||] [||]

let join env opt_r1 seq1 opt_r2 seq2 ~bound_name =
  let maybe_emit_naming_op = maybe_emit_naming_op env ~bound_name in
  match opt_r1, opt_r2 with
  | None, _ -> opt_r2
  | _, None -> opt_r1
  | Some r1, Some r2 ->
    let l1 = Array.length r1 in
    assert (l1 = Array.length r2);
    let r = Array.make l1 Reg.dummy in
    for i = 0 to l1 - 1 do
      let typ = Cmm.lub_component r1.(i).Reg.typ r2.(i).Reg.typ in
      r.(i) <- Reg.create typ;
      seq1#insert_move env r1.(i) r.(i);
      maybe_emit_naming_op seq1 [| r.(i) |];
      seq2#insert_move env r2.(i) r.(i);
      maybe_emit_naming_op seq2 [| r.(i) |]
    done;
    Some r

let join_array env rs ~bound_name =
  let maybe_emit_naming_op = maybe_emit_naming_op env ~bound_name in
  let some_res = ref None in
  for i = 0 to Array.length rs - 1 do
    let r, _ = rs.(i) in
    match r with
    | None -> ()
    | Some r -> (
      match !some_res with
      | None -> some_res := Some (r, Array.map (fun r -> r.Reg.typ) r)
      | Some (r', types) ->
        let types =
          Array.map2 (fun r typ -> Cmm.lub_component r.Reg.typ typ) r types
        in
        some_res := Some (r', types))
  done;
  match !some_res with
  | None -> None
  | Some (template, types) ->
    let size_res = Array.length template in
    let res = Array.make size_res Reg.dummy in
    for i = 0 to size_res - 1 do
      res.(i) <- Reg.create types.(i)
    done;
    for i = 0 to Array.length rs - 1 do
      let r, s = rs.(i) in
      match r with
      | None -> ()
      | Some r ->
        s#insert_moves env r res;
        maybe_emit_naming_op s res
    done;
    Some res

type environment = Label.t Select_utils.environment

type basic_or_terminator =
  | Basic of Cfg.basic
  | Terminator of Cfg.terminator

let basic_op x = Basic (Op x)

class virtual selector_generic =
  object (self : 'self)
    inherit [Label.t, Operation.t, Cfg.basic] Select_utils.common_selector

    method is_store op = match op with Store (_, _, _) -> true | _ -> false

    method lift_op op = Cfg.Op op

    method make_store mem_chunk addr_mode is_assignment =
      Cfg.Op (Operation.Store (mem_chunk, addr_mode, is_assignment))

    method make_stack_offset stack_ofs = Cfg.Op (Stackoffset stack_ofs)

    method make_name_for_debugger ~ident ~which_parameter ~provenance
        ~is_assignment ~regs =
      Cfg.Op
        (Operation.Name_for_debugger
           { ident; which_parameter; provenance; is_assignment; regs })

    method make_const_int x = Operation.Const_int x

    method make_const_float32 x = Operation.Const_float32 x

    method make_const_float x = Operation.Const_float x

    method make_const_vec128 x = Operation.Const_vec128 x

    method make_const_symbol x = Operation.Const_symbol x

    method make_opaque () = Operation.Opaque

    (* Default instruction selection for stores (of words) *)

    method select_store is_assign addr arg : Operation.t * Cmm.expression =
      Store (Word_val, addr, is_assign), arg

    (* Default instruction selection for operators *)

    method select_operation (op : Cmm.operation) (args : Cmm.expression list)
        (dbg : Debuginfo.t) ~label_after
        : basic_or_terminator * Cmm.expression list =
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
        Misc.fatal_errorf
          "Selection.select_operation: builtin not recognized %s" func ()
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
        then
          Terminator (Prim { op = External external_call; label_after }), args
        else Terminator (Call_no_return external_call), args
      | Cload { memory_chunk; mutability; is_atomic } ->
        let arg = single_arg () in
        let addressing_mode, eloc = self#select_addressing memory_chunk arg in
        let mutability = select_mutable_flag mutability in
        ( basic_op
            (Load { memory_chunk; addressing_mode; mutability; is_atomic }),
          [eloc] )
      | Cstore (chunk, init) -> (
        let arg1, arg2 = two_args () in
        let addr, eloc = self#select_addressing chunk arg1 in
        let is_assign =
          match init with Initialization -> false | Assignment -> true
        in
        match[@ocaml.warning "-fragile-match"] chunk with
        | Word_int | Word_val ->
          let op, newarg2 = self#select_store is_assign addr arg2 in
          basic_op op, [newarg2; eloc]
        | _ -> basic_op (Store (chunk, addr, is_assign)), [arg2; eloc]
        (* Inversion addr/datum in Istore *))
      | Cdls_get -> basic_op Dls_get, args
      | Calloc (mode, alloc_block_kind) ->
        let placeholder_for_alloc_block_kind =
          { alloc_words = 0; alloc_block_kind; alloc_dbg = Debuginfo.none }
        in
        ( basic_op
            (Alloc
               { bytes = 0; dbginfo = [placeholder_for_alloc_block_kind]; mode }),
          args )
      | Cpoll -> basic_op Poll, args
      | Caddi -> self#select_arith_comm Simple_operation.Iadd args
      | Csubi -> self#select_arith Simple_operation.Isub args
      | Cmuli -> self#select_arith_comm Simple_operation.Imul args
      | Cmulhi { signed } ->
        self#select_arith_comm (Simple_operation.Imulh { signed }) args
      | Cdivi -> basic_op (Intop Idiv), args
      | Cmodi -> basic_op (Intop Imod), args
      | Cand -> self#select_arith_comm Simple_operation.Iand args
      | Cor -> self#select_arith_comm Simple_operation.Ior args
      | Cxor -> self#select_arith_comm Simple_operation.Ixor args
      | Clsl -> self#select_arith Simple_operation.Ilsl args
      | Clsr -> self#select_arith Simple_operation.Ilsr args
      | Casr -> self#select_arith Simple_operation.Iasr args
      | Cclz { arg_is_non_zero } ->
        basic_op (Intop (Iclz { arg_is_non_zero })), args
      | Cctz { arg_is_non_zero } ->
        basic_op (Intop (Ictz { arg_is_non_zero })), args
      | Cpopcnt -> basic_op (Intop Ipopcnt), args
      | Ccmpi comp ->
        self#select_arith_comp (Simple_operation.Isigned comp) args
      | Caddv -> self#select_arith_comm Simple_operation.Iadd args
      | Cadda -> self#select_arith_comm Simple_operation.Iadd args
      | Ccmpa comp ->
        self#select_arith_comp (Simple_operation.Iunsigned comp) args
      | Ccmpf (w, comp) -> basic_op (Floatop (w, Icompf comp)), args
      | Ccsel _ ->
        let cond, ifso, ifnot = three_args () in
        let cond, earg = self#select_condition cond in
        basic_op (Csel cond), [earg; ifso; ifnot]
      | Cnegf w -> basic_op (Floatop (w, Inegf)), args
      | Cabsf w -> basic_op (Floatop (w, Iabsf)), args
      | Caddf w -> basic_op (Floatop (w, Iaddf)), args
      | Csubf w -> basic_op (Floatop (w, Isubf)), args
      | Cmulf w -> basic_op (Floatop (w, Imulf)), args
      | Cdivf w -> basic_op (Floatop (w, Idivf)), args
      | Creinterpret_cast cast -> basic_op (Reinterpret_cast cast), args
      | Cstatic_cast cast -> basic_op (Static_cast cast), args
      | Catomic { op; size } -> (
        match op with
        | Exchange | Fetch_and_add | Add | Sub | Land | Lor | Lxor ->
          let src, dst = two_args () in
          let dst_size =
            match size with
            | Word | Sixtyfour -> Word_int
            | Thirtytwo -> Thirtytwo_signed
          in
          let addr, eloc = self#select_addressing dst_size dst in
          basic_op (Intop_atomic { op; size; addr }), [src; eloc]
        | Compare_set | Compare_exchange ->
          let compare_with, set_to, dst = three_args () in
          let dst_size =
            match size with
            | Word | Sixtyfour -> Word_int
            | Thirtytwo -> Thirtytwo_signed
          in
          let addr, eloc = self#select_addressing dst_size dst in
          ( basic_op (Intop_atomic { op; size; addr }),
            [compare_with; set_to; eloc] ))
      | Cprobe { name; handler_code_sym; enabled_at_init } ->
        ( Terminator
            (Prim
               { op = Probe { name; handler_code_sym; enabled_at_init };
                 label_after
               }),
          args )
      | Cprobe_is_enabled { name } -> basic_op (Probe_is_enabled { name }), []
      | Cbeginregion -> basic_op Begin_region, []
      | Cendregion -> basic_op End_region, args
      | Cpackf32 | Copaque | Cbswap _ | Cprefetch _ | Craise _
      | Ctuple_field (_, _) ->
        Misc.fatal_error "Selection.select_oper"

    method private select_arith_comm (op : Simple_operation.integer_operation)
        (args : Cmm.expression list) : basic_or_terminator * Cmm.expression list
        =
      match args with
      | [arg; Cconst_int (n, _)] when self#is_immediate op n ->
        basic_op (Intop_imm (op, n)), [arg]
      | [Cconst_int (n, _); arg] when self#is_immediate op n ->
        basic_op (Intop_imm (op, n)), [arg]
      | _ -> basic_op (Intop op), args

    method private select_arith (op : Simple_operation.integer_operation)
        (args : Cmm.expression list) : basic_or_terminator * Cmm.expression list
        =
      match args with
      | [arg; Cconst_int (n, _)] when self#is_immediate op n ->
        basic_op (Intop_imm (op, n)), [arg]
      | _ -> basic_op (Intop op), args

    method private select_arith_comp (cmp : Simple_operation.integer_comparison)
        (args : Cmm.expression list) : basic_or_terminator * Cmm.expression list
        =
      match args with
      | [arg; Cconst_int (n, _)]
        when self#is_immediate (Simple_operation.Icomp cmp) n ->
        basic_op (Intop_imm (Icomp cmp, n)), [arg]
      | [Cconst_int (n, _); arg]
        when self#is_immediate
               (Simple_operation.Icomp (Select_utils.swap_intcomp cmp))
               n ->
        basic_op (Intop_imm (Icomp (Select_utils.swap_intcomp cmp), n)), [arg]
      | _ -> basic_op (Intop (Icomp cmp)), args

    (* Buffering of instruction sequences *)

    val mutable sub_cfg = Sub_cfg.make_empty ()

    method insert_debug (_env : environment) basic dbg arg res =
      Sub_cfg.add_instruction sub_cfg basic arg res dbg

    method private insert_op_debug_returning_id (_env : environment) op dbg arg
        res =
      let instr = Cfg.make_instr (Cfg.Op op) arg res dbg in
      Sub_cfg.add_instruction' sub_cfg instr;
      instr.id

    method insert (_env : environment) basic arg res =
      (* CR mshinwell: fix debuginfo *)
      Sub_cfg.add_instruction sub_cfg basic arg res Debuginfo.none

    method insert' (_env : environment) term arg res =
      (* CR mshinwell: fix debuginfo *)
      Sub_cfg.set_terminator sub_cfg term arg res Debuginfo.none

    method insert_debug' (_env : environment) basic dbg arg res =
      Sub_cfg.set_terminator sub_cfg basic arg res dbg

    method private insert_op_debug' (_env : environment) op dbg rs rd =
      Sub_cfg.set_terminator sub_cfg op rs rd dbg;
      rd

    val mutable tailrec_label : Label.t = Label.none
    (* set in emit_fundecl *)

    method insert_move env src dst =
      if src.Reg.stamp <> dst.Reg.stamp
      then self#insert env (Op Move) [| src |] [| dst |]

    method emit_expr_aux_raise env k (args : expression list) dbg =
      let r1 = self#emit_tuple env args in
      let extra_args_regs =
        match env.trap_stack with
        | Uncaught ->
          (* Function-level or toplevel exception continuations never have extra
             args. *)
          [||]
        | Specific_trap (cont, _trap_stack) ->
          Select_utils.env_find_regs_for_exception_extra_args cont env
      in
      (* Populate the distinguished extra args registers, for the current
         exception handler, with the extra args for this particular raise. *)
      let rd = Array.append [| Proc.loc_exn_bucket |] extra_args_regs in
      Array.iter2
        (fun r1 rd -> self#insert env (Op Move) [| r1 |] [| rd |])
        r1 rd;
      self#insert_debug' env (Cfg.Raise k) dbg rd [||];
      set_traps_for_raise env;
      None

    method emit_expr_aux_op env bound_name op args dbg =
      let ret res = Some res in
      match self#emit_parts_list env args with
      | None -> None
      | Some (simple_args, env) -> (
        assert (Sub_cfg.exit_has_never_terminator sub_cfg);
        let add_naming_op_for_bound_name regs =
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
              self#insert_debug env (Op naming_op) Debuginfo.none [||] [||]
        in
        let ty = Select_utils.oper_result_type op in
        let label_after = Cmm.new_label () in
        let new_op, new_args =
          self#select_operation op simple_args dbg ~label_after
        in
        match new_op with
        | Terminator (Call { op = Indirect; label_after } as term) ->
          let r1 = self#emit_tuple env new_args in
          let rarg = Array.sub r1 1 (Array.length r1 - 1) in
          let rd = Reg.createv ty in
          let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv rarg) in
          let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
          let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
          self#insert_move_args env rarg loc_arg stack_ofs;
          self#insert_debug' env term dbg
            (Array.append [| r1.(0) |] loc_arg)
            loc_res;
          sub_cfg <- Sub_cfg.add_never_block sub_cfg ~label:label_after;
          (* The destination registers (as per the procedure calling convention)
             need to be named right now, otherwise the result of the function
             call may be unavailable in the debugger immediately after the
             call. *)
          add_naming_op_for_bound_name loc_res;
          self#insert_move_results env loc_res rd stack_ofs;
          Select_utils.set_traps_for_raise env;
          Some rd
        | Terminator (Call { op = Direct _; label_after } as term) ->
          let r1 = self#emit_tuple env new_args in
          let rd = Reg.createv ty in
          let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv r1) in
          let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
          let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
          self#insert_move_args env r1 loc_arg stack_ofs;
          self#insert_debug' env term dbg loc_arg loc_res;
          add_naming_op_for_bound_name loc_res;
          sub_cfg <- Sub_cfg.add_never_block sub_cfg ~label:label_after;
          self#insert_move_results env loc_res rd stack_ofs;
          Select_utils.set_traps_for_raise env;
          Some rd
        | Terminator
            (Prim { op = External ({ ty_args; ty_res; _ } as r); label_after })
          ->
          let loc_arg, stack_ofs =
            self#emit_extcall_args env ty_args new_args
          in
          let rd = Reg.createv ty_res in
          let term =
            Cfg.Prim { op = External { r with stack_ofs }; label_after }
          in
          let loc_res =
            self#insert_op_debug' env term dbg loc_arg
              (Proc.loc_external_results (Reg.typv rd))
          in
          sub_cfg <- Sub_cfg.add_never_block sub_cfg ~label:label_after;
          add_naming_op_for_bound_name loc_res;
          self#insert_move_results env loc_res rd stack_ofs;
          Select_utils.set_traps_for_raise env;
          ret rd
        | Terminator (Prim { op = Probe _; label_after } as term) ->
          let r1 = self#emit_tuple env new_args in
          let rd = Reg.createv ty in
          let rd = self#insert_op_debug' env term dbg r1 rd in
          Select_utils.set_traps_for_raise env;
          sub_cfg <- Sub_cfg.add_never_block sub_cfg ~label:label_after;
          ret rd
        | Terminator (Call_no_return ({ func_symbol; ty_args; _ } as r)) ->
          let loc_arg, stack_ofs =
            self#emit_extcall_args env ty_args new_args
          in
          let keep_for_checking =
            !Select_utils.current_function_is_check_enabled
            && String.equal func_symbol Cmm.caml_flambda2_invalid
          in
          let returns, ty =
            if keep_for_checking then true, typ_int else false, ty
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
            self#insert_op_debug' env term dbg loc_arg
              (Proc.loc_external_results (Reg.typv rd))
          in
          Select_utils.set_traps_for_raise env;
          if returns
          then (
            sub_cfg <- Sub_cfg.add_never_block sub_cfg ~label;
            ret rd)
          else None
        | Basic (Op (Alloc { bytes = _; mode; dbginfo = [placeholder] })) ->
          let rd = Reg.createv typ_val in
          let bytes = Select_utils.size_expr env (Ctuple new_args) in
          let alloc_words = (bytes + Arch.size_addr - 1) / Arch.size_addr in
          let op =
            Operation.Alloc
              { bytes = alloc_words * Arch.size_addr;
                dbginfo = [{ placeholder with alloc_words; alloc_dbg = dbg }];
                mode
              }
          in
          self#insert_debug env (Op op) dbg [||] rd;
          add_naming_op_for_bound_name rd;
          self#emit_stores env dbg new_args rd;
          Select_utils.set_traps_for_raise env;
          ret rd
        | Basic (Op (Alloc { bytes = _; mode = _; dbginfo })) ->
          Misc.fatal_errorf
            "Selection Alloc: expected a single placehold in dbginfo, found %d"
            (List.length dbginfo)
        | Basic (Op op) ->
          let r1 = self#emit_tuple env new_args in
          let rd = Reg.createv ty in
          add_naming_op_for_bound_name rd;
          ret (self#insert_op_debug env op dbg r1 rd)
        | Basic basic ->
          Misc.fatal_errorf "unexpected basic (%a)" Cfg.dump_basic basic
        | Terminator term ->
          Misc.fatal_errorf "unexpected terminator (%a)"
            (Cfg.dump_terminator ~sep:"")
            term)

    method emit_expr_aux_ifthenelse env bound_name econd _ifso_dbg eif
        (_ifnot_dbg : Debuginfo.t) eelse (_dbg : Debuginfo.t)
        (_value_kind : Cmm.kind_for_unboxing) =
      (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
      let cond, earg = self#select_condition econd in
      match self#emit_expr env earg ~bound_name:None with
      | None -> None
      | Some rarg ->
        assert (Sub_cfg.exit_has_never_terminator sub_cfg);
        let rif, (sif : 'self) = self#emit_sequence env eif ~bound_name in
        let relse, (selse : 'self) = self#emit_sequence env eelse ~bound_name in
        let r = join env rif sif relse selse ~bound_name in
        let sub_if = sif#extract in
        let sub_else = selse#extract in
        let term_desc =
          terminator_of_test cond
            ~label_true:(Sub_cfg.start_label sub_if)
            ~label_false:(Sub_cfg.start_label sub_else)
        in
        Sub_cfg.update_exit_terminator sub_cfg term_desc ~arg:rarg;
        sub_cfg <- Sub_cfg.join ~from:[sub_if; sub_else] ~to_:sub_cfg;
        r

    method emit_expr_aux_switch env bound_name esel index ecases
        (_dbg : Debuginfo.t) (_value_kind : Cmm.kind_for_unboxing) =
      (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
      match self#emit_expr env esel ~bound_name:None with
      | None -> None
      | Some rsel ->
        assert (Sub_cfg.exit_has_never_terminator sub_cfg);
        let sub_cases : (Reg.t array option * 'self) array =
          Array.map
            (fun (case, _dbg) -> self#emit_sequence env case ~bound_name)
            ecases
        in
        let r = join_array env sub_cases ~bound_name in
        let subs = Array.map (fun (_, s) -> s#extract) sub_cases in
        let term_desc : Cfg.terminator =
          Switch (Array.map (fun idx -> Sub_cfg.start_label subs.(idx)) index)
        in
        Sub_cfg.update_exit_terminator sub_cfg term_desc ~arg:rsel;
        sub_cfg <- Sub_cfg.join ~from:(Array.to_list subs) ~to_:sub_cfg;
        r

    method emit_expr_aux_catch env bound_name (_rec_flag : Cmm.rec_flag)
        handlers body (_value_kind : Cmm.kind_for_unboxing) =
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
        (* Since the handlers may be recursive, and called from the body, the
           same environment is used for translating both the handlers and the
           body. *)
        List.fold_left
          (fun (env, map) (nfail, ids, rs, e2, dbg, is_cold) ->
            let label = Cmm.new_label () in
            let env, r =
              Select_utils.env_add_static_exception nfail rs env label
            in
            env, Int.Map.add nfail (r, (ids, rs, e2, dbg, is_cold, label)) map)
          (env, Int.Map.empty) handlers
      in
      let r_body, s_body = self#emit_sequence env body ~bound_name in
      let translate_one_handler nfail
          (trap_info, (ids, rs, e2, _dbg, is_cold, label)) =
        assert (List.length ids = List.length rs);
        let trap_stack =
          match (!trap_info : Select_utils.trap_stack_info) with
          | Unreachable -> assert false
          | Reachable t -> t
        in
        let ids_and_rs = List.combine ids rs in
        let new_env =
          List.fold_left
            (fun env ((id, _typ), r) -> Select_utils.env_add id r env)
            (Select_utils.env_set_trap_stack env trap_stack)
            ids_and_rs
        in
        let r, s =
          self#emit_sequence new_env e2 ~bound_name:None ~at_start:(fun seq ->
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
                    seq#insert_debug new_env (Cfg.Op naming_op) Debuginfo.none
                      [||] [||])
                ids_and_rs)
        in
        (nfail, trap_stack, is_cold, label), (r, s)
      in
      let rec build_all_reachable_handlers ~already_built ~not_built =
        let not_built, to_build =
          Int.Map.partition
            (fun _n (r, _) ->
              match !r with
              | Select_utils.Unreachable -> true
              | Select_utils.Reachable _ -> false)
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
        build_all_reachable_handlers ~already_built:[] ~not_built:handlers_map
        (* Note: we're dropping unreachable handlers here *)
      in
      let a = Array.of_list ((r_body, s_body) :: List.map snd l) in
      let r = join_array env a ~bound_name in
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let s_body : Sub_cfg.t = s_body#extract in
      let s_handlers =
        List.map
          (fun ((_, _, _, label), (_, sub_handler)) ->
            let seq : Sub_cfg.t = sub_handler#extract in
            Sub_cfg.add_empty_block_at_start seq ~label)
          l
      in
      let term_desc = Cfg.Always (Sub_cfg.start_label s_body) in
      Sub_cfg.update_exit_terminator sub_cfg term_desc;
      sub_cfg <- Sub_cfg.join ~from:(s_body :: s_handlers) ~to_:sub_cfg;
      r

    method emit_expr_aux_exit env lbl args traps =
      match self#emit_parts_list env args with
      | None -> None
      | Some (simple_list, ext_env) -> (
        match lbl with
        | Lbl nfail ->
          let src = self#emit_tuple ext_env simple_list in
          let handler =
            try Select_utils.env_find_static_exception nfail env
            with Not_found ->
              Misc.fatal_error
                ("Selection.emit_expr: unbound label "
               ^ Stdlib.Int.to_string nfail)
          in
          (* Intermediate registers to handle cases where some registers from
             src are present in dest *)
          let tmp_regs = Reg.createv_with_typs src in
          (* Ccatch registers must not contain out of heap pointers *)
          Array.iter
            (fun reg ->
              match reg.Reg.typ with
              | Addr -> assert false
              | Valx2 -> Misc.fatal_error "Unexpected machtype_component Valx2"
              | Val | Int | Float | Vec128 | Float32 -> ())
            src;
          self#insert_moves env src tmp_regs;
          self#insert_moves env tmp_regs (Array.concat handler.regs);
          assert (Sub_cfg.exit_has_never_terminator sub_cfg);
          List.iter
            (fun trap ->
              let instr_desc =
                match trap with
                | Cmm.Push handler_id ->
                  let lbl_handler =
                    (Select_utils.env_find_static_exception handler_id env)
                      .extra
                  in
                  Cfg.Pushtrap { lbl_handler }
                | Cmm.Pop _ -> Cfg.Poptrap
              in
              Sub_cfg.add_instruction sub_cfg instr_desc [||] [||]
                Debuginfo.none)
            traps;
          Sub_cfg.update_exit_terminator sub_cfg (Always handler.extra);
          Select_utils.set_traps nfail handler.Select_utils.traps_ref
            env.Select_utils.trap_stack traps;
          None
        | Return_lbl -> (
          match simple_list with
          | [expr] ->
            self#emit_return ext_env expr traps;
            None
          | [] ->
            Misc.fatal_error "Selection.emit_expr: Return without arguments"
          | _ :: _ :: _ ->
            Misc.fatal_error
              "Selection.emit_expr: Return with too many arguments"))

    method emit_expr_aux_trywith env bound_name e1 exn_cont v ~extra_args e2
        (_dbg : Debuginfo.t) (_value_kind : Cmm.kind_for_unboxing) =
      (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let exn_label = Cmm.new_label () in
      (* For each exception handler having extra arguments, distinguished
         registers corresponding to such arguments are created. They are
         populated at each raise site, which is the only place Cmm allows extra
         arguments to be involved in a raise. Any function call that involves
         extra arguments on its exception continuation has to compiled using a
         wrapper; see [To_cmm_expr.translate_apply]. *)
      let extra_arg_regs_split =
        List.map (fun (_param, machtype) -> Reg.createv machtype) extra_args
      in
      let extra_arg_regs = Array.concat extra_arg_regs_split in
      let env_body = Select_utils.env_enter_trywith env exn_cont exn_label in
      let env_body =
        env_add_regs_for_exception_extra_args exn_cont extra_arg_regs env_body
      in
      let r1, s1 = self#emit_sequence env_body e1 ~bound_name in
      let exn_bucket_in_handler = Reg.createv typ_val in
      let rv_list = exn_bucket_in_handler :: extra_arg_regs_split in
      let with_handler env_handler e2 =
        let r2, s2 =
          self#emit_sequence env_handler e2 ~bound_name ~at_start:(fun seq ->
              List.iter2
                (fun v regs ->
                  let provenance = VP.provenance v in
                  if Option.is_some provenance
                  then
                    let var = VP.var v in
                    let naming_op =
                      Operation.Name_for_debugger
                        { ident = var;
                          provenance;
                          which_parameter = None;
                          is_assignment = false;
                          regs
                        }
                    in
                    seq#insert_debug env (Cfg.Op naming_op) Debuginfo.none [||]
                      [||])
                (v :: List.map fst extra_args)
                rv_list)
        in
        let r = join env r1 s1 r2 s2 ~bound_name in
        let s1 : Sub_cfg.t = s1#extract in
        let s2 : Sub_cfg.t = s2#extract in
        Sub_cfg.mark_as_trap_handler s2 ~exn_label;
        Sub_cfg.add_instruction_at_start s2 (Cfg.Op Move)
          [| Proc.loc_exn_bucket |] exn_bucket_in_handler Debuginfo.none;
        Sub_cfg.update_exit_terminator sub_cfg (Always (Sub_cfg.start_label s1));
        sub_cfg <- Sub_cfg.join ~from:[s1; s2] ~to_:sub_cfg;
        r
      in
      let env =
        List.fold_left2
          (fun env var regs -> Select_utils.env_add var regs env)
          env
          (v :: List.map fst extra_args)
          rv_list
      in
      match Select_utils.env_find_static_exception exn_cont env_body with
      | { traps_ref = { contents = Reachable ts }; _ } ->
        with_handler (Select_utils.env_set_trap_stack env ts) e2
      | { traps_ref = { contents = Unreachable }; _ } ->
        let dummy_constant = Cconst_int (1, Debuginfo.none) in
        let segfault =
          Cmm.(
            Cop
              ( Cload
                  { memory_chunk = Word_int;
                    mutability = Mutable;
                    is_atomic = false
                  },
                [Cconst_int (0, Debuginfo.none)],
                Debuginfo.none ))
        in
        let dummy_raise =
          Cop (Craise Raise_notrace, [dummy_constant], Debuginfo.none)
        in
        let unreachable =
          (* The use of a raise operation means that this handler is known not
             to return, making it compatible with any layout for the body or
             surrounding code. We also set the trap stack to [Uncaught] to
             ensure that we don't introduce spurious control-flow edges inside
             the function. *)
          Csequence (segfault, dummy_raise)
        in
        let env = Select_utils.env_set_trap_stack env Uncaught in
        with_handler env unreachable
        (* Misc.fatal_errorf "Selection.emit_expr: \ * Unreachable exception
           handler %d" lbl *)
      | exception Not_found ->
        Misc.fatal_errorf "Selection.emit_expr: Unbound handler %d" exn_cont

    method private emit_sequence ?at_start (env : environment) exp ~bound_name
        : _ * 'self =
      let s = {<sub_cfg = Sub_cfg.make_empty ()>} in
      (match at_start with None -> () | Some f -> f s);
      let r = s#emit_expr_aux env exp ~bound_name in
      r, s

    (* Same, but in tail position *)

    method private insert_return (env : environment) r
        (traps : trap_action list) =
      match r with
      | None -> ()
      | Some r ->
        List.iter
          (fun trap ->
            let instr_desc =
              match trap with
              | Cmm.Push _ -> Misc.fatal_error "unexpected push on trap actions"
              | Cmm.Pop _ -> Cfg.Poptrap
            in
            Sub_cfg.add_instruction sub_cfg instr_desc [||] [||] Debuginfo.none)
          traps;
        let loc = Proc.loc_results_return (Reg.typv r) in
        self#insert_moves env r loc;
        self#insert' env Cfg.Return loc [||]

    method emit_return (env : environment) exp traps =
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      self#insert_return env (self#emit_expr_aux env exp ~bound_name:None) traps

    method emit_tail_apply env ty op args dbg =
      match self#emit_parts_list env args with
      | None -> ()
      | Some (simple_args, env) -> (
        let label_after = Cmm.new_label () in
        let new_op, new_args =
          self#select_operation op simple_args dbg ~label_after
        in
        match new_op with
        | Terminator (Call { op = Indirect; label_after } as term) ->
          let r1 = self#emit_tuple env new_args in
          let rd = Reg.createv ty in
          let rarg = Array.sub r1 1 (Array.length r1 - 1) in
          let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv rarg) in
          let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
          let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
          if stack_ofs = 0 && Select_utils.trap_stack_is_empty env
          then (
            let call = Cfg.Tailcall_func Indirect in
            self#insert_moves env rarg loc_arg;
            self#insert_debug' env call dbg
              (Array.append [| r1.(0) |] loc_arg)
              [||])
          else (
            self#insert_move_args env rarg loc_arg stack_ofs;
            self#insert_debug' env term dbg
              (Array.append [| r1.(0) |] loc_arg)
              loc_res;
            sub_cfg <- Sub_cfg.add_never_block sub_cfg ~label:label_after;
            Select_utils.set_traps_for_raise env;
            self#insert env (Op (Stackoffset (-stack_ofs))) [||] [||];
            self#insert_return env (Some loc_res) (pop_all_traps env))
        | Terminator (Call { op = Direct func; label_after } as term) ->
          let r1 = self#emit_tuple env new_args in
          let rd = Reg.createv ty in
          let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv r1) in
          let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
          let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
          if String.equal func.sym_name !Select_utils.current_function_name
             && Select_utils.trap_stack_is_empty env
          then (
            let call = Cfg.Tailcall_self { destination = tailrec_label } in
            let loc_arg' =
              assert (stack_ofs >= 0);
              if stack_ofs = 0
              then loc_arg
              else Proc.loc_parameters (Reg.typv r1)
            in
            self#insert_moves env r1 loc_arg';
            self#insert_debug' env call dbg loc_arg' [||])
          else if stack_ofs = 0 && Select_utils.trap_stack_is_empty env
          then (
            let call = Cfg.Tailcall_func (Direct func) in
            self#insert_moves env r1 loc_arg;
            self#insert_debug' env call dbg loc_arg [||])
          else (
            self#insert_move_args env r1 loc_arg stack_ofs;
            self#insert_debug' env term dbg loc_arg loc_res;
            sub_cfg <- Sub_cfg.add_never_block sub_cfg ~label:label_after;
            Select_utils.set_traps_for_raise env;
            self#insert env (Op (Stackoffset (-stack_ofs))) [||] [||];
            self#insert_return env (Some loc_res) (pop_all_traps env))
        | _ -> Misc.fatal_error "Cfg_selectgen.emit_tail")

    method emit_tail_ifthenelse env econd (_ifso_dbg : Debuginfo.t) eif
        (_ifnot_dbg : Debuginfo.t) eelse (_dbg : Debuginfo.t)
        (_kind : Cmm.kind_for_unboxing) =
      (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
      let cond, earg = self#select_condition econd in
      match self#emit_expr env earg ~bound_name:None with
      | None -> ()
      | Some rarg ->
        assert (Sub_cfg.exit_has_never_terminator sub_cfg);
        let sub_if = self#emit_tail_sequence env eif in
        let sub_else = self#emit_tail_sequence env eelse in
        let term_desc =
          terminator_of_test cond
            ~label_true:(Sub_cfg.start_label sub_if)
            ~label_false:(Sub_cfg.start_label sub_else)
        in
        Sub_cfg.update_exit_terminator sub_cfg term_desc ~arg:rarg;
        sub_cfg <- Sub_cfg.join_tail ~from:[sub_if; sub_else] ~to_:sub_cfg

    method emit_tail_switch env esel index ecases (_dbg : Debuginfo.t)
        (_kind : Cmm.kind_for_unboxing) =
      (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
      match self#emit_expr env esel ~bound_name:None with
      | None -> ()
      | Some rsel ->
        assert (Sub_cfg.exit_has_never_terminator sub_cfg);
        let sub_cases =
          Array.map
            (fun (case, _dbg) -> self#emit_tail_sequence env case)
            ecases
        in
        let term_desc : Cfg.terminator =
          Switch
            (Array.map (fun idx -> Sub_cfg.start_label sub_cases.(idx)) index)
        in
        Sub_cfg.update_exit_terminator sub_cfg term_desc ~arg:rsel;
        sub_cfg
          <- Sub_cfg.join_tail ~from:(Array.to_list sub_cases) ~to_:sub_cfg

    method emit_tail_catch env (_rec_flag : Cmm.rec_flag) handlers e1
        (_value_kind : Cmm.kind_for_unboxing) =
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
            let env, r =
              Select_utils.env_add_static_exception nfail rs env label
            in
            env, Int.Map.add nfail (r, (ids, rs, e2, dbg, is_cold, label)) map)
          (env, Int.Map.empty) handlers
      in
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let s_body = self#emit_tail_sequence env e1 in
      let translate_one_handler nfail
          (trap_info, (ids, rs, e2, _dbg, is_cold, label)) =
        assert (List.length ids = List.length rs);
        let trap_stack =
          match (!trap_info : Select_utils.trap_stack_info) with
          | Unreachable -> assert false
          | Reachable t -> t
        in
        let ids_and_rs = List.combine ids rs in
        let new_env =
          List.fold_left
            (fun env ((id, _typ), r) -> Select_utils.env_add id r env)
            (Select_utils.env_set_trap_stack env trap_stack)
            ids_and_rs
        in
        let seq : Sub_cfg.t =
          self#emit_tail_sequence new_env e2 ~at_start:(fun seq ->
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
                    seq#insert_debug new_env (Cfg.Op naming_op) Debuginfo.none
                      [||] [||])
                ids_and_rs)
        in
        let seq = Sub_cfg.add_empty_block_at_start seq ~label in
        nfail, trap_stack, seq, is_cold
      in
      let rec build_all_reachable_handlers ~already_built ~not_built =
        let not_built, to_build =
          Int.Map.partition
            (fun _n (r, _) ->
              match !r with
              | Select_utils.Unreachable -> true
              | Select_utils.Reachable _ -> false)
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
      let new_handlers :
          (int * Simple_operation.trap_stack * Sub_cfg.t * bool) list =
        build_all_reachable_handlers ~already_built:[] ~not_built:handlers_map
        (* Note: we're dropping unreachable handlers here *)
      in
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let term_desc = Cfg.Always (Sub_cfg.start_label s_body) in
      Sub_cfg.update_exit_terminator sub_cfg term_desc;
      let s_handlers = List.map (fun (_, _, s, _) -> s) new_handlers in
      sub_cfg <- Sub_cfg.join_tail ~from:(s_body :: s_handlers) ~to_:sub_cfg

    method emit_tail_trywith env e1 exn_cont v ~extra_args e2
        (_dbg : Debuginfo.t) (_value_kind : Cmm.kind_for_unboxing) =
      (* CR-someday xclerc for xclerc: use the `_dbg` parameter *)
      assert (Sub_cfg.exit_has_never_terminator sub_cfg);
      let exn_label = Cmm.new_label () in
      (* See comment in emit_expr_aux_trywith about extra args *)
      let extra_arg_regs_split =
        List.map (fun (_param, machtype) -> Reg.createv machtype) extra_args
      in
      let extra_arg_regs = Array.concat extra_arg_regs_split in
      let env_body = Select_utils.env_enter_trywith env exn_cont exn_label in
      let env_body =
        env_add_regs_for_exception_extra_args exn_cont extra_arg_regs env_body
      in
      let s1 : Sub_cfg.t = self#emit_tail_sequence env_body e1 in
      let exn_bucket_in_handler = Reg.createv typ_val in
      let rv_list = exn_bucket_in_handler :: extra_arg_regs_split in
      let with_handler env_handler e2 =
        let s2 : Sub_cfg.t =
          self#emit_tail_sequence env_handler e2 ~at_start:(fun seq ->
              List.iter2
                (fun v regs ->
                  let provenance = VP.provenance v in
                  if Option.is_some provenance
                  then
                    let var = VP.var v in
                    let naming_op =
                      Operation.Name_for_debugger
                        { ident = var;
                          provenance;
                          which_parameter = None;
                          is_assignment = false;
                          regs
                        }
                    in
                    seq#insert_debug env (Cfg.Op naming_op) Debuginfo.none [||]
                      [||])
                (v :: List.map fst extra_args)
                rv_list)
        in
        Sub_cfg.mark_as_trap_handler s2 ~exn_label;
        Sub_cfg.add_instruction_at_start s2 (Cfg.Op Move)
          [| Proc.loc_exn_bucket |] exn_bucket_in_handler Debuginfo.none;
        Sub_cfg.update_exit_terminator sub_cfg (Always (Sub_cfg.start_label s1));
        sub_cfg <- Sub_cfg.join_tail ~from:[s1; s2] ~to_:sub_cfg
      in
      let env =
        List.fold_left2
          (fun env var regs -> Select_utils.env_add var regs env)
          env
          (v :: List.map fst extra_args)
          rv_list
      in
      match Select_utils.env_find_static_exception exn_cont env_body with
      | { traps_ref = { contents = Reachable ts }; _ } ->
        with_handler (Select_utils.env_set_trap_stack env ts) e2
      | { traps_ref = { contents = Unreachable }; _ } ->
        (* Note: The following [unreachable] expression has machtype [|Int|],
           but this might not be the correct machtype for this function's return
           value. It doesn't matter at runtime since the expression cannot
           return, but if we start checking (or joining) the machtypes of the
           different tails we will need to implement something like the
           [emit_expr_aux] version above, that hides the machtype. *)
        let unreachable =
          Cmm.(
            Cop
              ( Cload
                  { memory_chunk = Word_int;
                    mutability = Mutable;
                    is_atomic = false
                  },
                [Cconst_int (0, Debuginfo.none)],
                Debuginfo.none ))
        in
        with_handler env unreachable
      (* Misc.fatal_errorf "Selection.emit_expr: \ Unreachable exception handler
         %d" lbl *)
      | exception Not_found ->
        Misc.fatal_errorf "Selection.emit_expr: Unbound handler %d" exn_cont

    method private emit_tail_sequence ?at_start env exp =
      let s = {<sub_cfg = Sub_cfg.make_empty ()>} in
      (match at_start with None -> () | Some f -> f s);
      s#emit_tail env exp;
      s#extract

    method extract = sub_cfg

    (* Sequentialization of a function definition *)

    method emit_fundecl ~future_funcnames f =
      Select_utils.current_function_name := f.Cmm.fun_name.sym_name;
      Select_utils.current_function_is_check_enabled
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
      let env =
        List.fold_right2
          (fun (id, _ty) r env -> Select_utils.env_add id r env)
          f.Cmm.fun_args rargs Select_utils.env_empty
      in
      tailrec_label <- Cmm.new_label ();
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
            self#insert_debug env (Cfg.Op naming_op) Debuginfo.none
              hard_regs_for_arg [||])
        f.Cmm.fun_args;
      self#insert_moves env loc_arg rarg;
      let prologue_poll_instr_id =
        self#insert_op_debug_returning_id env Operation.Poll Debuginfo.none [||]
          [||]
      in
      self#emit_tail env f.Cmm.fun_body;
      let body = self#extract in
      let cfg =
        (* note: we set `fun_contains_calls` to `true` here, but will compute
           its proper value below, after possibly removing the prologue poll
           instruction. It is not very satisfactory, but as noted in the CR
           below, we should revisit the way we handle polling points. *)
        Cfg.create ~fun_name:f.Cmm.fun_name.sym_name ~fun_args:loc_arg
          ~fun_codegen_options:
            (Cfg.of_cmm_codegen_option f.Cmm.fun_codegen_options)
          ~fun_dbg:f.Cmm.fun_dbg ~fun_contains_calls:true
          ~fun_num_stack_slots:(Array.make Proc.num_stack_slot_classes 0)
          ~fun_poll:f.Cmm.fun_poll
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
        (* CR mshinwell/xclerc: find a neater way of doing this rather than
           making a special case for the [optimistic_prologue_poll_instr_id]. *)
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
      Stack_offset_and_exn.update_cfg cfg;
      Cfg.register_predecessors_for_all_blocks cfg;
      let fun_contains_calls =
        Sub_cfg.exists_basic_blocks body ~f:Cfg.basic_block_contains_calls
      in
      let cfg = { cfg with fun_contains_calls } in
      let cfg_with_layout =
        Cfg_with_layout.create cfg ~layout ~preserve_orig_labels:false
          ~new_labels:Label.Set.empty
      in
      (* CR xclerc for xclerc: Regalloc_irc_utils.log_cfg_with_infos ~indent:1
         (Cfg_with_infos.make cfg_with_layout); *)
      Merge_straightline_blocks.run cfg_with_layout;
      Simplify_terminator.run cfg;
      Eliminate_dead_code.run_dead_block cfg_with_layout;
      cfg_with_layout
  end
