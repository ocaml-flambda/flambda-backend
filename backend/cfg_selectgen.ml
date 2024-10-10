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
module Int = Numbers.Int
module VP = Backend_var.With_provenance

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
        Cfg.Name_for_debugger
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
      if Reg.anonymous r1.(i) && Cmm.ge_component r1.(i).Reg.typ r2.(i).Reg.typ
      then (
        r.(i) <- r1.(i);
        seq2#insert_move env r2.(i) r1.(i);
        maybe_emit_naming_op seq2 [| r1.(i) |])
      else if Reg.anonymous r2.(i)
              && Cmm.ge_component r2.(i).Reg.typ r1.(i).Reg.typ
      then (
        r.(i) <- r2.(i);
        seq1#insert_move env r1.(i) r2.(i);
        maybe_emit_naming_op seq1 [| r2.(i) |])
      else
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

module DLL = Flambda_backend_utils.Doubly_linked_list

type environment = Label.t Select_utils.environment

type basic_or_terminator =
  | Basic of Cfg.basic
  | Terminator of Cfg.terminator
  | With_next_label of (Label.t -> Cfg.terminator)

let basic_op x = Basic (Op x)

let next_instr_id = ref 0

let reset_next_instr_id () = next_instr_id := 0

let next_instr_id () : int =
  let res = !next_instr_id in
  incr next_instr_id;
  res

module Sub_cfg : sig
  type t =
    { entry : Cfg.basic_block;
      exit : Cfg.basic_block;
      layout : Cfg.basic_block DLL.t
    }

  val make_instr :
    'a -> Reg.t array -> Reg.t array -> Debuginfo.t -> 'a Cfg.instruction

  val make_empty_block :
    ?label:Label.t -> Cfg.terminator Cfg.instruction -> Cfg.basic_block

  val make_empty : unit -> t

  val add_instruction :
    t -> Cfg.basic -> Reg.t array -> Reg.t array -> Debuginfo.t -> unit

  val set_terminator :
    t -> Cfg.terminator -> Reg.t array -> Reg.t array -> Debuginfo.t -> unit

  val link_if_needed :
    from:Cfg.basic_block -> to_:Cfg.basic_block -> unit -> unit

  val dump : t -> unit
end = struct
  type t =
    { entry : Cfg.basic_block;
      exit : Cfg.basic_block;
      layout : Cfg.basic_block DLL.t
    }

  let make_instr desc arg res dbg =
    { Cfg.desc;
      arg;
      res;
      dbg;
      fdo = Fdo_info.none;
      live = Reg.Set.empty;
      stack_offset = -1;
      id = next_instr_id ();
      irc_work_list = Unknown_list;
      ls_order = 0;
      available_before =
        Some (Reg_availability_set.Ok Reg_with_debug_info.Set.empty);
      available_across = None
    }

  let make_empty_block ?label terminator : Cfg.basic_block =
    let start =
      match label with None -> Cmm.new_label () | Some label -> label
    in
    { start;
      body = DLL.make_empty ();
      terminator;
      predecessors = Label.Set.empty;
      stack_offset = -1;
      exn = None;
      can_raise = false;
      is_trap_handler = false;
      dead = false;
      cold = false
    }

  let make_empty () =
    let exit =
      make_empty_block (make_instr Cfg.Never [||] [||] Debuginfo.none)
    in
    let entry =
      make_empty_block
        (make_instr (Cfg.Always exit.start) [||] [||] Debuginfo.none)
    in
    let layout = DLL.make_empty () in
    DLL.add_end layout entry;
    DLL.add_end layout exit;
    { entry; exit; layout }

  let add_instruction sub_cfg desc arg res dbg =
    assert (sub_cfg.exit.terminator.desc = Cfg.Never);
    DLL.add_end sub_cfg.exit.body (make_instr desc arg res dbg)

  let set_terminator sub_cfg desc arg res dbg =
    assert (sub_cfg.exit.terminator.desc = Cfg.Never);
    sub_cfg.exit.terminator <- make_instr desc arg res dbg

  let link_if_needed ~(from : Cfg.basic_block) ~(to_ : Cfg.basic_block) () =
    if from.terminator.desc = Cfg.Never
    then
      from.terminator
        <- { from.terminator with
             desc = Always to_.start;
             id = next_instr_id ()
           }

  let dump sub_cfg =
    let liveness = Cfg_dataflow.Instr.Tbl.create 32 in
    DLL.iter sub_cfg.layout ~f:(fun (block : Cfg.basic_block) ->
        Regalloc_irc_utils.log_body_and_terminator ~indent:0 block.body
          block.terminator liveness)
end

class virtual selector_generic =
  object (self : 'self)
    inherit [Label.t, Cfg.operation, Cfg.basic] Select_utils.common_selector

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

    val mutable sub_cfg = Sub_cfg.make_empty ()

    val mutable tailrec_label : Label.t = -1 (* set in emit_fundecl *)

    method insert
        : environment -> Cfg.basic -> Reg.t array -> Reg.t array -> unit =
      fun _env basic arg res ->
        (* CR mshinwell: fix debuginfo *)
        Sub_cfg.add_instruction sub_cfg basic arg res Debuginfo.none

    method insert_debug
        : environment ->
          Cfg.basic ->
          Debuginfo.t ->
          Reg.t array ->
          Reg.t array ->
          unit =
      fun _env basic dbg arg res ->
        Sub_cfg.add_instruction sub_cfg basic arg res dbg

    method insert'
        : environment -> Cfg.terminator -> Reg.t array -> Reg.t array -> unit =
      fun _env term arg res ->
        (* CR mshinwell: fix debuginfo *)
        Sub_cfg.set_terminator sub_cfg term arg res Debuginfo.none

    method insert_debug'
        : environment ->
          Cfg.terminator ->
          Debuginfo.t ->
          Reg.t array ->
          Reg.t array ->
          unit =
      fun _env basic dbg arg res ->
        Sub_cfg.set_terminator sub_cfg basic arg res dbg

    method private insert_op_debug'
        : environment ->
          Cfg.terminator ->
          Debuginfo.t ->
          Reg.t array ->
          Reg.t array ->
          Reg.t array =
      fun _env op dbg rs rd ->
        Sub_cfg.set_terminator sub_cfg op rs rd dbg;
        rd

    method insert_move env src dst =
      if src.Reg.stamp <> dst.Reg.stamp
      then self#insert env Cfg.(Op Move) [| src |] [| dst |]

    method emit_expr_aux_raise
        : environment ->
          Lambda.raise_kind ->
          Cmm.expression ->
          Debuginfo.t ->
          Reg.t array option =
      fun env k arg dbg ->
        match self#emit_expr env arg ~bound_name:None with
        | None -> None
        | Some r1 ->
          let rd = [| Proc.loc_exn_bucket |] in
          self#insert env Cfg.(Op Move) r1 rd;
          self#insert_debug' env (Cfg.Raise k) dbg rd [||];
          set_traps_for_raise env;
          None

    method emit_expr_aux_op
        : environment ->
          Backend_var.With_provenance.t option ->
          Cmm.operation ->
          Cmm.expression list ->
          Debuginfo.t ->
          Reg.t array option =
      fun env bound_name op args dbg ->
        let ret res = Some res in
        match self#emit_parts_list env args with
        | None -> None
        | Some (simple_args, env) -> (
          assert (sub_cfg.exit.terminator.desc = Cfg.Never);
          let add_naming_op_for_bound_name regs =
            match bound_name with
            | None -> ()
            | Some bound_name ->
              let provenance = VP.provenance bound_name in
              if Option.is_some provenance
              then
                let bound_name = VP.var bound_name in
                let naming_op =
                  Cfg.Name_for_debugger
                    { ident = bound_name;
                      provenance;
                      which_parameter = None;
                      is_assignment = false;
                      regs
                    }
                in
                self#insert_debug env (Cfg.Op naming_op) Debuginfo.none [||] [||]
          in
          let ty = Select_utils.oper_result_type op in
          let new_op, new_args = self#select_operation op simple_args dbg in
          match new_op with
          | With_next_label (f : Label.t -> Cfg.terminator) -> (
            let label = Cmm.new_label () in
            let term = f label in
            match term with
            | Call { op = Indirect; label_after } ->
              let r1 = self#emit_tuple env new_args in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let rd = self#regs_for ty in
              let loc_arg, stack_ofs_args =
                Proc.loc_arguments (Reg.typv rarg)
              in
              let loc_res, stack_ofs_res =
                Proc.loc_results_call (Reg.typv rd)
              in
              let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
              self#insert_move_args env rarg loc_arg stack_ofs;
              self#insert_debug' env term dbg
                (Array.append [| r1.(0) |] loc_arg)
                loc_res;
              let next_block =
                Sub_cfg.make_empty_block ~label:label_after
                  (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
              in
              DLL.add_end sub_cfg.Sub_cfg.layout next_block;
              sub_cfg <- { sub_cfg with Sub_cfg.exit = next_block };
              (* The destination registers (as per the procedure calling
                 convention) need to be named right now, otherwise the result of
                 the function call may be unavailable in the debugger
                 immediately after the call. *)
              add_naming_op_for_bound_name loc_res;
              self#insert_move_results env loc_res rd stack_ofs;
              Select_utils.set_traps_for_raise env;
              Some rd
            | Call { op = Direct _; label_after } ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv r1) in
              let loc_res, stack_ofs_res =
                Proc.loc_results_call (Reg.typv rd)
              in
              let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
              self#insert_move_args env r1 loc_arg stack_ofs;
              self#insert_debug' env term dbg loc_arg loc_res;
              add_naming_op_for_bound_name loc_res;
              let next_block =
                Sub_cfg.make_empty_block ~label:label_after
                  (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
              in
              DLL.add_end sub_cfg.Sub_cfg.layout next_block;
              sub_cfg <- { sub_cfg with Sub_cfg.exit = next_block };
              self#insert_move_results env loc_res rd stack_ofs;
              Select_utils.set_traps_for_raise env;
              Some rd
            | Prim { op = External ({ ty_args; ty_res; _ } as r); label_after }
              ->
              let loc_arg, stack_ofs =
                self#emit_extcall_args env ty_args new_args
              in
              let rd = self#regs_for ty_res in
              let term =
                Cfg.Prim { op = External { r with stack_ofs }; label_after }
              in
              let loc_res =
                self#insert_op_debug' env term dbg loc_arg
                  (Proc.loc_external_results (Reg.typv rd))
              in
              let next_block =
                Sub_cfg.make_empty_block ~label:label_after
                  (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
              in
              DLL.add_end sub_cfg.Sub_cfg.layout next_block;
              sub_cfg <- { sub_cfg with Sub_cfg.exit = next_block };
              add_naming_op_for_bound_name loc_res;
              self#insert_move_results env loc_res rd stack_ofs;
              Select_utils.set_traps_for_raise env;
              ret rd
            | Prim { op = Probe _; label_after } ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              let rd = self#insert_op_debug' env term dbg r1 rd in
              Select_utils.set_traps_for_raise env;
              let next_block =
                Sub_cfg.make_empty_block ~label:label_after
                  (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
              in
              DLL.add_end sub_cfg.Sub_cfg.layout next_block;
              sub_cfg <- { sub_cfg with Sub_cfg.exit = next_block };
              ret rd
            | _ -> assert false)
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
            let rd = self#regs_for ty in
            let term = Cfg.Call_no_return { r with stack_ofs } in
            let _ =
              self#insert_op_debug' env term dbg loc_arg
                (Proc.loc_external_results (Reg.typv rd))
            in
            Select_utils.set_traps_for_raise env;
            if returns then ret rd else None
          | Basic (Op (Alloc { bytes = _; mode })) ->
            let rd = self#regs_for typ_val in
            let bytes = Select_utils.size_expr env (Ctuple new_args) in
            let alloc_words = (bytes + Arch.size_addr - 1) / Arch.size_addr in
            let op =
              Cfg.Alloc
                { bytes = alloc_words * Arch.size_addr;
                  dbginfo = [{ alloc_words; alloc_dbg = dbg }];
                  mode
                }
            in
            self#insert_debug env (Cfg.Op op) dbg [||] rd;
            add_naming_op_for_bound_name rd;
            self#emit_stores env dbg new_args rd;
            Select_utils.set_traps_for_raise env;
            ret rd
          | Basic (Op op) ->
            let r1 = self#emit_tuple env new_args in
            let rd = self#regs_for ty in
            add_naming_op_for_bound_name rd;
            ret (self#insert_op_debug env op dbg r1 rd)
          | Basic _ -> assert false
          | Terminator _ -> assert false)

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
      fun env bound_name econd _ifso_dbg eif _ifnotdbg eelse _XXXdbg _value_kind ->
        let cond, earg = self#select_condition econd in
        match self#emit_expr env earg ~bound_name:None with
        | None -> None
        | Some rarg ->
          assert (sub_cfg.exit.terminator.desc = Cfg.Never);
          let rif, (sif : 'self) = self#emit_sequence env eif ~bound_name in
          let relse, (selse : 'self) =
            self#emit_sequence env eelse ~bound_name
          in
          let r = join env rif sif relse selse ~bound_name in
          let sub_if = sif#extract in
          let sub_else = selse#extract in
          let term_desc =
            Cfgize.terminator_of_test cond
              ~label_false:sub_else.Sub_cfg.entry.start
              ~label_true:sub_if.Sub_cfg.entry.start
          in
          sub_cfg.exit.terminator
            <- { sub_cfg.exit.terminator with
                 desc = term_desc;
                 arg = rarg;
                 id = next_instr_id ()
               };
          DLL.transfer ~from:sub_if.Sub_cfg.layout ~to_:sub_cfg.layout ();
          DLL.transfer ~from:sub_else.Sub_cfg.layout ~to_:sub_cfg.layout ();
          let join_block =
            Sub_cfg.make_empty_block
              (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
          in
          Sub_cfg.link_if_needed ~from:sub_if.Sub_cfg.exit ~to_:join_block ();
          Sub_cfg.link_if_needed ~from:sub_else.Sub_cfg.exit ~to_:join_block ();
          DLL.add_end sub_cfg.Sub_cfg.layout join_block;
          sub_cfg <- { sub_cfg with Sub_cfg.exit = join_block };
          r

    method emit_expr_aux_switch
        : environment ->
          Backend_var.With_provenance.t option ->
          Cmm.expression ->
          int array ->
          (Cmm.expression * Debuginfo.t) array ->
          Debuginfo.t ->
          Cmm.kind_for_unboxing ->
          Reg.t array option =
      fun env bound_name esel index ecases _XXXdbg _value_kind ->
        match self#emit_expr env esel ~bound_name:None with
        | None -> None
        | Some rsel ->
          assert (sub_cfg.exit.terminator.desc = Cfg.Never);
          let sub_cases : (Reg.t array option * 'self) array =
            Array.map
              (fun (case, _dbg) -> self#emit_sequence env case ~bound_name)
              ecases
          in
          let r = join_array env sub_cases ~bound_name in
          let subs = Array.map (fun (_, s) -> s#extract) sub_cases in
          let term_desc : Cfg.terminator =
            Cfg.Switch
              (Array.map (fun idx -> subs.(idx).Sub_cfg.entry.start) index)
          in
          sub_cfg.exit.terminator
            <- { sub_cfg.exit.terminator with
                 desc = term_desc;
                 arg = rsel;
                 id = next_instr_id ()
               };
          Array.iter
            (fun sub_case ->
              DLL.transfer ~from:sub_case.Sub_cfg.layout ~to_:sub_cfg.layout ())
            subs;
          let join_block =
            Sub_cfg.make_empty_block
              (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
          in
          Array.iter
            (fun sub_case ->
              Sub_cfg.link_if_needed ~from:sub_case.Sub_cfg.exit ~to_:join_block
                ())
            subs;
          DLL.add_end sub_cfg.Sub_cfg.layout join_block;
          sub_cfg <- { sub_cfg with Sub_cfg.exit = join_block };
          r

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
      fun env bound_name _XXXrec_flag handlers body _value_kind ->
        let handlers =
          List.map
            (fun (nfail, ids, e2, dbg, is_cold) ->
              let rs =
                List.map
                  (fun (id, typ) ->
                    let r = self#regs_for typ in
                    Select_utils.name_regs id r;
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
                        Cfg.Name_for_debugger
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
              (fun _n (r, _) -> !r = Select_utils.Unreachable)
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
        assert (sub_cfg.exit.terminator.desc = Cfg.Never);
        let s_body : Sub_cfg.t = s_body#extract in
        let s_handlers =
          List.map
            (fun ((_, _, _, label), (_, sub_handler)) ->
              let seq : Sub_cfg.t = sub_handler#extract in
              let pre_entry : Cfg.basic_block =
                Sub_cfg.make_empty_block ~label
                  (Sub_cfg.make_instr (Cfg.Always seq.entry.start) [||] [||]
                     Debuginfo.none)
              in
              DLL.add_begin seq.layout pre_entry;
              let seq = { seq with entry = pre_entry } in
              seq)
            l
        in
        let term_desc = Cfg.Always s_body.Sub_cfg.entry.start in
        sub_cfg.exit.terminator
          <- { sub_cfg.exit.terminator with
               desc = term_desc;
               id = next_instr_id ()
             };
        DLL.transfer ~from:s_body.Sub_cfg.layout ~to_:sub_cfg.layout ();
        let join_block =
          Sub_cfg.make_empty_block
            (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
        in
        Sub_cfg.link_if_needed ~from:s_body.Sub_cfg.exit ~to_:join_block ();
        List.iter
          (fun sub_handler ->
            DLL.transfer ~from:sub_handler.Sub_cfg.layout ~to_:sub_cfg.layout ();
            Sub_cfg.link_if_needed ~from:sub_handler.Sub_cfg.exit
              ~to_:join_block ())
          s_handlers;
        DLL.add_end sub_cfg.Sub_cfg.layout join_block;
        sub_cfg <- { sub_cfg with Sub_cfg.exit = join_block };
        r

    method emit_expr_aux_exit
        : environment ->
          Cmm.exit_label ->
          Cmm.expression list ->
          Cmm.trap_action list ->
          Reg.t array option =
      fun env lbl args traps ->
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
            let tmp_regs = Reg.createv_like src in
            (* Ccatch registers must not contain out of heap pointers *)
            Array.iter (fun reg -> assert (reg.Reg.typ <> Addr)) src;
            self#insert_moves env src tmp_regs;
            self#insert_moves env tmp_regs (Array.concat handler.regs);
            assert (sub_cfg.exit.terminator.desc = Cfg.Never);
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
                DLL.add_end sub_cfg.exit.body
                  (Sub_cfg.make_instr instr_desc [||] [||] Debuginfo.none))
              traps;
            sub_cfg.exit.terminator
              <- { sub_cfg.exit.terminator with
                   desc = Cfg.Always handler.extra;
                   id = next_instr_id ()
                 };
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
      fun env bound_name e1 exn_cont v e2 _XXXdbg _value_kind ->
        assert (sub_cfg.exit.terminator.desc = Cfg.Never);
        let exn_label = Cmm.new_label () in
        let env_body = Select_utils.env_enter_trywith env exn_cont exn_label in
        let r1, s1 = self#emit_sequence env_body e1 ~bound_name in
        let rv = self#regs_for typ_val in
        let with_handler env_handler e2 =
          let r2, s2 =
            self#emit_sequence env_handler e2 ~bound_name ~at_start:(fun seq ->
                let provenance = VP.provenance v in
                if Option.is_some provenance
                then
                  let var = VP.var v in
                  let naming_op =
                    Cfg.Name_for_debugger
                      { ident = var;
                        provenance;
                        which_parameter = None;
                        is_assignment = false;
                        regs = rv
                      }
                  in
                  seq#insert_debug env (Cfg.Op naming_op) Debuginfo.none [||]
                    [||])
          in
          let move_exn_bucket =
            Sub_cfg.make_instr (Cfg.Op Move) [| Proc.loc_exn_bucket |] rv
              Debuginfo.none
          in
          let r = join env r1 s1 r2 s2 ~bound_name in
          let s1 : Sub_cfg.t = s1#extract in
          let s2 : Sub_cfg.t = s2#extract in
          s2.entry.start <- exn_label;
          DLL.add_begin s2.entry.body move_exn_bucket;
          s2.entry.is_trap_handler <- true;
          sub_cfg.exit.terminator
            <- { sub_cfg.exit.terminator with
                 desc = Always s1.entry.start;
                 id = next_instr_id ()
               };
          DLL.iter s1.layout ~f:(fun (block : Cfg.basic_block) ->
              if block.can_raise then block.exn <- Some s2.entry.start);
          DLL.transfer ~from:s1.Sub_cfg.layout ~to_:sub_cfg.layout ();
          DLL.transfer ~from:s2.Sub_cfg.layout ~to_:sub_cfg.layout ();
          let join_block =
            Sub_cfg.make_empty_block
              (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
          in
          DLL.add_end sub_cfg.Sub_cfg.layout join_block;
          Sub_cfg.link_if_needed ~from:s1.exit ~to_:join_block ();
          Sub_cfg.link_if_needed ~from:s2.exit ~to_:join_block ();
          sub_cfg <- { sub_cfg with Sub_cfg.exit = join_block };
          r
        in
        let env = Select_utils.env_add v rv env in
        match Select_utils.env_find_static_exception exn_cont env_body with
        | { traps_ref = { contents = Reachable ts }; _ } ->
          with_handler (Select_utils.env_set_trap_stack env ts) e2
        | { traps_ref = { contents = Unreachable }; _ } ->
          (* Note: The following [unreachable] expression has machtype [|Int|],
             but this might not be the correct machtype for this function's
             return value. It doesn't matter at runtime since the expression
             cannot return, but if we start checking (or joining) the machtypes
             of the different tails we will need to implement something like the
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
        (* Misc.fatal_errorf "Selection.emit_expr: \ Unreachable exception
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

    method private insert_return
        : environment -> Reg.t array option -> trap_action list -> unit =
      fun env r traps ->
        match r with
        | None -> ()
        | Some r ->
          List.iter
            (fun trap ->
              let instr_desc =
                match trap with
                | Cmm.Push _ -> assert false
                | Cmm.Pop _ -> Cfg.Poptrap
              in
              DLL.add_end sub_cfg.exit.body
                (Sub_cfg.make_instr instr_desc [||] [||] Debuginfo.none))
            traps;
          let loc = Proc.loc_results_return (Reg.typv r) in
          self#insert_moves env r loc;
          self#insert' env Cfg.Return loc [||]

    method emit_return
        : environment -> Cmm.expression -> Cmm.trap_action list -> unit =
      fun env exp traps ->
        assert (sub_cfg.exit.terminator.desc = Cfg.Never);
        let r = self#emit_expr_aux env exp ~bound_name:None in
        self#insert_return env r traps

    method emit_tail_apply
        : environment ->
          Cmm.machtype ->
          Cmm.operation ->
          Cmm.expression list ->
          Debuginfo.t ->
          unit =
      fun env ty op args dbg ->
        match self#emit_parts_list env args with
        | None -> ()
        | Some (simple_args, env) -> (
          let new_op, new_args = self#select_operation op simple_args dbg in
          let new_op =
            match new_op with
            | With_next_label f ->
              let label_after = Cmm.new_label () in
              f label_after
            | Basic _ | Terminator _ ->
              Misc.fatal_error "Cfg_selectgen.emit_tail"
          in
          match new_op with
          | Call { op = Indirect; label_after } ->
            let r1 = self#emit_tuple env new_args in
            let rd = self#regs_for ty in
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
              self#insert_debug' env new_op dbg
                (Array.append [| r1.(0) |] loc_arg)
                loc_res;
              let new_exit =
                Sub_cfg.make_empty_block ~label:label_after
                  (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
              in
              DLL.add_end sub_cfg.Sub_cfg.layout new_exit;
              sub_cfg <- { sub_cfg with exit = new_exit };
              Select_utils.set_traps_for_raise env;
              self#insert env Cfg.(Op (Stackoffset (-stack_ofs))) [||] [||];
              self#insert_return env (Some loc_res) (pop_all_traps env))
          | Call { op = Direct func; label_after } ->
            let r1 = self#emit_tuple env new_args in
            let rd = self#regs_for ty in
            let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv r1) in
            let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
            let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
            if stack_ofs = 0
               && func.sym_name = !Select_utils.current_function_name
               && Select_utils.trap_stack_is_empty env
            then (
              let call = Cfg.Tailcall_self { destination = tailrec_label } in
              let loc_arg' = Proc.loc_parameters (Reg.typv r1) in
              self#insert_moves env r1 loc_arg';
              self#insert_debug' env call dbg loc_arg' [||])
            else if stack_ofs = 0 && Select_utils.trap_stack_is_empty env
            then (
              let call = Cfg.Tailcall_func (Direct func) in
              self#insert_moves env r1 loc_arg;
              self#insert_debug' env call dbg loc_arg [||])
            else (
              self#insert_move_args env r1 loc_arg stack_ofs;
              self#insert_debug' env new_op dbg loc_arg loc_res;
              let new_exit =
                Sub_cfg.make_empty_block ~label:label_after
                  (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
              in
              DLL.add_end sub_cfg.Sub_cfg.layout new_exit;
              sub_cfg <- { sub_cfg with exit = new_exit };
              Select_utils.set_traps_for_raise env;
              self#insert env Cfg.(Op (Stackoffset (-stack_ofs))) [||] [||];
              self#insert_return env (Some loc_res) (pop_all_traps env))
          | _ -> Misc.fatal_error "Cfg_selectgen.emit_tail")

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
      fun env econd _ifso_dbg eif _ifnot_dbg eelse _XXXdbg _kind ->
        let cond, earg = self#select_condition econd in
        match self#emit_expr env earg ~bound_name:None with
        | None -> ()
        | Some rarg ->
          assert (sub_cfg.exit.terminator.desc = Cfg.Never);
          let sub_if = self#emit_tail_sequence env eif in
          let sub_else = self#emit_tail_sequence env eelse in
          let term_desc =
            Cfgize.terminator_of_test cond
              ~label_false:sub_else.Sub_cfg.entry.start
              ~label_true:sub_if.Sub_cfg.entry.start
          in
          sub_cfg.exit.terminator
            <- { sub_cfg.exit.terminator with
                 desc = term_desc;
                 id = next_instr_id ();
                 arg = rarg
               };
          DLL.transfer ~from:sub_if.Sub_cfg.layout ~to_:sub_cfg.layout ();
          DLL.transfer ~from:sub_else.Sub_cfg.layout ~to_:sub_cfg.layout ();
          let dummy_block =
            Sub_cfg.make_empty_block
              (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
          in
          DLL.add_end sub_cfg.Sub_cfg.layout dummy_block;
          sub_cfg <- { sub_cfg with Sub_cfg.exit = dummy_block }

    method emit_tail_switch
        : environment ->
          Cmm.expression ->
          int array ->
          (Cmm.expression * Debuginfo.t) array ->
          Debuginfo.t ->
          Cmm.kind_for_unboxing ->
          unit =
      fun env esel index ecases _XXXdbg _king ->
        match self#emit_expr env esel ~bound_name:None with
        | None -> ()
        | Some rsel ->
          assert (sub_cfg.exit.terminator.desc = Cfg.Never);
          let sub_cases =
            Array.map
              (fun (case, _dbg) -> self#emit_tail_sequence env case)
              ecases
          in
          let term_desc : Cfg.terminator =
            Cfg.Switch
              (Array.map (fun idx -> sub_cases.(idx).Sub_cfg.entry.start) index)
          in
          sub_cfg.exit.terminator
            <- { sub_cfg.exit.terminator with
                 desc = term_desc;
                 arg = rsel;
                 id = next_instr_id ()
               };
          Array.iter
            (fun sub_case ->
              DLL.transfer ~from:sub_case.Sub_cfg.layout ~to_:sub_cfg.layout ())
            sub_cases;
          let dummy_block =
            Sub_cfg.make_empty_block
              (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
          in
          DLL.add_end sub_cfg.Sub_cfg.layout dummy_block;
          sub_cfg <- { sub_cfg with Sub_cfg.exit = dummy_block }

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
      fun env _XXXrec_flag handlers e1 _value_kind ->
        let handlers =
          List.map
            (fun (nfail, ids, e2, dbg, is_cold) ->
              let rs =
                List.map
                  (fun (id, typ) ->
                    let r = self#regs_for typ in
                    Select_utils.name_regs id r;
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
        assert (sub_cfg.exit.terminator.desc = Cfg.Never);
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
                        Cfg.Name_for_debugger
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
          let pre_entry : Cfg.basic_block =
            Sub_cfg.make_empty_block ~label
              (Sub_cfg.make_instr (Cfg.Always seq.entry.start) [||] [||]
                 Debuginfo.none)
          in
          DLL.add_begin seq.layout pre_entry;
          let seq = { seq with entry = pre_entry } in
          nfail, trap_stack, seq, is_cold
        in
        let rec build_all_reachable_handlers ~already_built ~not_built =
          let not_built, to_build =
            Int.Map.partition
              (fun _n (r, _) -> !r = Select_utils.Unreachable)
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
        let new_handlers : (int * Mach.trap_stack * Sub_cfg.t * bool) list =
          build_all_reachable_handlers ~already_built:[] ~not_built:handlers_map
          (* Note: we're dropping unreachable handlers here *)
        in
        assert (sub_cfg.exit.terminator.desc = Cfg.Never);
        let term_desc = Cfg.Always s_body.Sub_cfg.entry.start in
        sub_cfg.exit.terminator
          <- { sub_cfg.exit.terminator with
               desc = term_desc;
               id = next_instr_id ()
             };
        DLL.transfer ~from:s_body.Sub_cfg.layout ~to_:sub_cfg.layout ();
        List.iter
          (fun (_, _, sub_handler, _) ->
            DLL.transfer ~from:sub_handler.Sub_cfg.layout ~to_:sub_cfg.layout ())
          new_handlers

    method emit_tail_trywith
        : environment ->
          Cmm.expression ->
          Cmm.trywith_shared_label ->
          Backend_var.With_provenance.t ->
          Cmm.expression ->
          Debuginfo.t ->
          Cmm.kind_for_unboxing ->
          unit =
      fun env e1 exn_cont v e2 _XXXdbg _value_kind ->
        assert (sub_cfg.exit.terminator.desc = Cfg.Never);
        let exn_label = Cmm.new_label () in
        let env_body = Select_utils.env_enter_trywith env exn_cont exn_label in
        let s1 : Sub_cfg.t = self#emit_tail_sequence env_body e1 in
        let rv = self#regs_for typ_val in
        let with_handler env_handler e2 =
          let s2 : Sub_cfg.t =
            self#emit_tail_sequence env_handler e2 ~at_start:(fun seq ->
                let provenance = VP.provenance v in
                if Option.is_some provenance
                then
                  let var = VP.var v in
                  let naming_op =
                    Cfg.Name_for_debugger
                      { ident = var;
                        provenance;
                        which_parameter = None;
                        is_assignment = false;
                        regs = rv
                      }
                  in
                  seq#insert_debug env_handler (Cfg.Op naming_op) Debuginfo.none
                    [||] [||])
          in
          s2.entry.start <- exn_label;
          let move_exn_bucket =
            Sub_cfg.make_instr (Cfg.Op Move) [| Proc.loc_exn_bucket |] rv
              Debuginfo.none
          in
          DLL.add_begin s2.entry.body move_exn_bucket;
          s2.entry.is_trap_handler <- true;
          sub_cfg.exit.terminator
            <- { sub_cfg.exit.terminator with
                 desc = Always s1.entry.start;
                 id = next_instr_id ()
               };
          DLL.iter s1.layout ~f:(fun (block : Cfg.basic_block) ->
              if block.can_raise then block.exn <- Some s2.entry.start);
          DLL.transfer ~from:s1.Sub_cfg.layout ~to_:sub_cfg.layout ();
          DLL.transfer ~from:s2.Sub_cfg.layout ~to_:sub_cfg.layout ();
          let dummy_block =
            Sub_cfg.make_empty_block
              (Sub_cfg.make_instr Cfg.Never [||] [||] Debuginfo.none)
          in
          DLL.add_end sub_cfg.Sub_cfg.layout dummy_block;
          sub_cfg <- { sub_cfg with Sub_cfg.exit = dummy_block }
        in
        let env = Select_utils.env_add v rv env in
        match Select_utils.env_find_static_exception exn_cont env_body with
        | { traps_ref = { contents = Reachable ts }; _ } ->
          with_handler (Select_utils.env_set_trap_stack env ts) e2
        | { traps_ref = { contents = Unreachable }; _ } ->
          (* Note: The following [unreachable] expression has machtype [|Int|],
             but this might not be the correct machtype for this function's
             return value. It doesn't matter at runtime since the expression
             cannot return, but if we start checking (or joining) the machtypes
             of the different tails we will need to implement something like the
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
        (* Misc.fatal_errorf "Selection.emit_expr: \ Unreachable exception
           handler %d" lbl *)
        | exception Not_found ->
          Misc.fatal_errorf "Selection.emit_expr: Unbound handler %d" exn_cont

    method private emit_tail_sequence ?at_start env exp =
      let s = {<sub_cfg = Sub_cfg.make_empty ()>} in
      (match at_start with None -> () | Some f -> f s);
      s#emit_tail env exp;
      s#extract

    method extract = sub_cfg

    method emit_fundecl
        : future_funcnames:Misc.Stdlib.String.Set.t ->
          Cmm.fundecl ->
          Cfg_with_layout.t =
      fun ~future_funcnames:_ f ->
        reset_next_instr_id ();
        Select_utils.current_function_name := f.Cmm.fun_name.sym_name;
        Select_utils.current_function_is_check_enabled
          := Zero_alloc_checker.is_check_enabled f.Cmm.fun_codegen_options
               f.Cmm.fun_name.sym_name f.Cmm.fun_dbg;
        let num_regs_per_arg = Array.make (List.length f.Cmm.fun_args) 0 in
        let rargs =
          List.mapi
            (fun arg_index (var, ty) ->
              let r = self#regs_for ty in
              Select_utils.name_regs var r;
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
                Cfg.Name_for_debugger
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
        self#emit_tail env f.Cmm.fun_body;
        let body = self#extract in
        if false then Sub_cfg.dump body;
        (* CR xclerc for xclerc: implement polling insertion. *)
        let fun_poll = Lambda.Default_poll in
        let fun_contains_calls =
          DLL.exists body.Sub_cfg.layout ~f:(fun (block : Cfg.basic_block) ->
              block.is_trap_handler
              || (match block.terminator.desc with
                 | Never | Always _ | Parity_test _ | Truth_test _
                 | Float_test _ | Int_test _ | Switch _ | Return ->
                   false
                 | Raise raise_kind -> (
                   match raise_kind with
                   | Lambda.Raise_notrace -> false
                   | Lambda.Raise_regular | Lambda.Raise_reraise ->
                     (* PR#6239 *)
                     (* caml_stash_backtrace; we #mark_call rather than
                        #mark_c_tailcall to get a good stack backtrace *)
                     true)
                 | Tailcall_self _ -> false
                 | Tailcall_func _ -> false
                 | Call_no_return _ -> true
                 | Call _ -> true
                 | Prim { op = Probe _ } -> true
                 | Prim _ -> false
                 | Specific_can_raise _ -> false)
              || DLL.exists block.body
                   ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
                     match instr.desc with
                     | Op (Alloc _ | Poll) -> true
                     | _ -> false))
        in
        let cfg : Cfg.t =
          Cfg.create ~fun_name:f.Cmm.fun_name.sym_name ~fun_args:loc_arg
            ~fun_codegen_options:
              (Cfg.of_cmm_codegen_option f.Cmm.fun_codegen_options)
            ~fun_dbg:f.Cmm.fun_dbg ~fun_contains_calls
            ~fun_num_stack_slots:(Array.make Proc.num_stack_slot_classes 0)
            ~fun_poll
        in
        let layout = DLL.make_empty () in
        let entry_block =
          Sub_cfg.make_empty_block ~label:(Cfg.entry_label cfg)
            (Sub_cfg.make_instr (Cfg.Always tailrec_label) [||] [||]
               Debuginfo.none)
        in
        DLL.add_begin entry_block.body
          (Sub_cfg.make_instr Cfg.Prologue [||] [||] Debuginfo.none);
        Cfg.add_block_exn cfg entry_block;
        DLL.add_end layout entry_block.start;
        let tailrec_block =
          Sub_cfg.make_empty_block ~label:tailrec_label
            (Sub_cfg.make_instr (Cfg.Always body.Sub_cfg.entry.start) [||] [||]
               Debuginfo.none)
        in
        Cfg.add_block_exn cfg tailrec_block;
        DLL.add_end layout tailrec_block.start;
        DLL.iter body.Sub_cfg.layout ~f:(fun (block : Cfg.basic_block) ->
            if block.terminator.desc <> Cfg.Never
            then (
              Cfg.add_block_exn cfg block;
              DLL.add_end layout block.start)
            else assert (DLL.is_empty block.body));
        Cfg.register_predecessors_for_all_blocks cfg;
        let cfg_with_layout =
          Cfg_with_layout.create cfg ~layout ~preserve_orig_labels:false
            ~new_labels:Label.Set.empty
        in
        (* CR xclerc for xclerc: Regalloc_irc_utils.log_cfg_with_infos ~indent:1
           (Cfg_with_infos.make cfg_with_layout); *)
        Cfgize.Stack_offset_and_exn.update_cfg cfg;
        Merge_straightline_blocks.run cfg_with_layout;
        Eliminate_dead_code.run_dead_block cfg_with_layout;
        Simplify_terminator.run cfg;
        cfg_with_layout
  end
