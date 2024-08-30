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
module Int = Numbers.Int
module V = Backend_var
module VP = Backend_var.With_provenance

(* The default instruction selection class *)

type environment = Select_utils.environment

class virtual selector_generic =
  object (self : 'self)
    (* A syntactic criterion used in addition to judgements about (co)effects as
       to whether the evaluation of a given expression may be deferred by
       [emit_parts]. This criterion is a property of the instruction selection
       algorithm in this file rather than a property of the Cmm language. *)
    method is_simple_expr =
      function
      | Cconst_int _ -> true
      | Cconst_natint _ -> true
      | Cconst_float32 _ -> true
      | Cconst_float _ -> true
      | Cconst_symbol _ -> true
      | Cconst_vec128 _ -> true
      | Cvar _ -> true
      | Ctuple el -> List.for_all self#is_simple_expr el
      | Clet (_id, arg, body) | Clet_mut (_id, _, arg, body) ->
        self#is_simple_expr arg && self#is_simple_expr body
      | Cphantom_let (_var, _defining_expr, body) -> self#is_simple_expr body
      | Csequence (e1, e2) -> self#is_simple_expr e1 && self#is_simple_expr e2
      | Cop (op, args, _) -> (
        match op with
        (* Cextcall with neither effects nor coeffects is simple if its
           arguments are *)
        | Cextcall { effects = No_effects; coeffects = No_coeffects } ->
          List.for_all self#is_simple_expr args
          (* The following may have side effects *)
        | Capply _ | Cextcall _ | Calloc _ | Cstore _ | Craise _ | Catomic _
        | Cprobe _ | Cprobe_is_enabled _ | Copaque ->
          false
        | Cprefetch _ | Cbeginregion | Cendregion ->
          false
          (* avoid reordering *)
          (* The remaining operations are simple if their args are *)
        | Cload _ | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand
        | Cor | Cxor | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _
        | Cnegf _ | Cclz _ | Cctz _ | Cpopcnt | Cbswap _ | Ccsel _ | Cabsf _
        | Caddf _ | Csubf _ | Cmulf _ | Cdivf _ | Cpackf32 | Creinterpret_cast _
        | Cstatic_cast _ | Ctuple_field _ | Ccmpf _ | Cdls_get ->
          List.for_all self#is_simple_expr args)
      | Cassign _ | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _
        ->
        false

    (* Analyses the effects and coeffects of an expression. This is used across
       a whole list of expressions with a view to determining which expressions
       may have their evaluation deferred. The result of this function, modulo
       target-specific judgements if the [effects_of] method is overridden, is a
       property of the Cmm language rather than anything particular about the
       instruction selection algorithm in this file.

       In the case of e.g. an OCaml function call, the arguments whose
       evaluation cannot be deferred (cf. [emit_parts], below) are computed in
       right-to-left order first with their results going into temporaries, then
       the block is allocated, then the remaining arguments are evaluated before
       being combined with the temporaries. *)
    method effects_of exp =
      let module EC = Select_utils.Effect_and_coeffect in
      match exp with
      | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
      | Cconst_symbol _ | Cconst_vec128 _ | Cvar _ ->
        EC.none
      | Ctuple el -> EC.join_list_map el self#effects_of
      | Clet (_id, arg, body) | Clet_mut (_id, _, arg, body) ->
        EC.join (self#effects_of arg) (self#effects_of body)
      | Cphantom_let (_var, _defining_expr, body) -> self#effects_of body
      | Csequence (e1, e2) -> EC.join (self#effects_of e1) (self#effects_of e2)
      | Cifthenelse (cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg, _kind) ->
        EC.join (self#effects_of cond)
          (EC.join (self#effects_of ifso) (self#effects_of ifnot))
      | Cop (op, args, _) ->
        let from_op =
          match op with
          | Cextcall { effects = e; coeffects = ce } ->
            EC.create
              (Select_utils.select_effects e)
              (Select_utils.select_coeffects ce)
          | Capply _ | Cprobe _ | Copaque -> EC.arbitrary
          | Calloc Alloc_heap -> EC.none
          | Calloc Alloc_local ->
            EC.coeffect_only Select_utils.Coeffect.Arbitrary
          | Cstore _ -> EC.effect_only Select_utils.Effect.Arbitrary
          | Cbeginregion | Cendregion -> EC.arbitrary
          | Cprefetch _ -> EC.arbitrary
          | Catomic _ -> EC.arbitrary
          | Craise _ -> EC.effect_only Select_utils.Effect.Raise
          | Cload { mutability = Asttypes.Immutable } -> EC.none
          | Cload { mutability = Asttypes.Mutable } | Cdls_get ->
            EC.coeffect_only Select_utils.Coeffect.Read_mutable
          | Cprobe_is_enabled _ ->
            EC.coeffect_only Select_utils.Coeffect.Arbitrary
          | Ctuple_field _ | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi
          | Cand | Cor | Cxor | Cbswap _ | Ccsel _ | Cclz _ | Cctz _ | Cpopcnt
          | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf _
          | Cabsf _ | Caddf _ | Csubf _ | Cmulf _ | Cdivf _ | Cpackf32
          | Creinterpret_cast _ | Cstatic_cast _ | Ccmpf _ ->
            EC.none
        in
        EC.join from_op (EC.join_list_map args self#effects_of)
      | Cassign _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _ -> EC.arbitrary

    (* Says whether an integer constant is a suitable immediate argument for the
       given integer operation *)

    method is_immediate (op : Mach.integer_operation) n =
      match op with
      | Ilsl | Ilsr | Iasr -> n >= 0 && n < Arch.size_int * 8
      | _ -> false

    (* Says whether an integer constant is a suitable immediate argument for the
       given integer test *)

    method virtual is_immediate_test : Mach.integer_comparison -> int -> bool

    (* Selection of addressing modes *)

    method virtual select_addressing
        : Cmm.memory_chunk ->
          Cmm.expression ->
          Arch.addressing_mode * Cmm.expression

    (* Default instruction selection for stores (of words) *)

    method select_store is_assign addr arg : Mach.operation * Cmm.expression =
      Istore (Word_val, addr, is_assign), arg

    (* call marking methods, documented in selectgen.mli *)
    val contains_calls = ref false

    method mark_call = contains_calls := true

    method mark_tailcall = ()

    method mark_c_tailcall = if !Clflags.debug then contains_calls := true

    method mark_instr : Mach.instruction_desc -> unit =
      function
      | Iop (Icall_ind | Icall_imm _ | Iextcall _ | Iprobe _) -> self#mark_call
      | Iop (Itailcall_ind | Itailcall_imm _) -> self#mark_tailcall
      | Iop (Ialloc _) | Iop (Ipoll _) ->
        self#mark_call (* caml_alloc*, caml_garbage_collection (incl. polls) *)
      | Iraise raise_kind -> (
        match raise_kind with
        | Lambda.Raise_notrace -> ()
        | Lambda.Raise_regular | Lambda.Raise_reraise ->
          (* PR#6239 *)
          (* caml_stash_backtrace; we #mark_call rather than #mark_c_tailcall to
             get a good stack backtrace *)
          self#mark_call)
      | Itrywith _ -> self#mark_call
      | _ -> ()

    (* Default instruction selection for operators *)

    method select_operation (op : Cmm.operation) (args : Cmm.expression list)
        _dbg : Mach.operation * Cmm.expression list =
      let open Mach in
      match op, args with
      | Capply _, Cconst_symbol (func, _dbg) :: rem -> Icall_imm { func }, rem
      | Capply _, _ -> Icall_ind, args
      | Cextcall { func; builtin = true }, _ ->
        Misc.fatal_errorf
          "Selection.select_operation: builtin not recognized %s" func ()
      | Cextcall { func; alloc; ty; ty_args; returns; builtin = false }, _ ->
        ( Iextcall { func; alloc; ty_res = ty; ty_args; returns; stack_ofs = -1 },
          args )
      | Cload { memory_chunk; mutability; is_atomic }, [arg] ->
        let addressing_mode, eloc = self#select_addressing memory_chunk arg in
        Iload { memory_chunk; addressing_mode; mutability; is_atomic }, [eloc]
      | Cstore (chunk, init), [arg1; arg2] ->
        let addr, eloc = self#select_addressing chunk arg1 in
        let is_assign =
          match init with Initialization -> false | Assignment -> true
        in
        if chunk = Word_int || chunk = Word_val
        then
          let op, newarg2 = self#select_store is_assign addr arg2 in
          op, [newarg2; eloc]
        else Istore (chunk, addr, is_assign), [arg2; eloc]
        (* Inversion addr/datum in Istore *)
      | Cdls_get, _ -> Idls_get, args
      | Calloc mode, _ -> Ialloc { bytes = 0; dbginfo = []; mode }, args
      | Caddi, _ -> self#select_arith_comm Iadd args
      | Csubi, _ -> self#select_arith Isub args
      | Cmuli, _ -> self#select_arith_comm Imul args
      | Cmulhi { signed }, _ -> self#select_arith_comm (Imulh { signed }) args
      | Cdivi, _ -> Iintop Idiv, args
      | Cmodi, _ -> Iintop Imod, args
      | Cand, _ -> self#select_arith_comm Iand args
      | Cor, _ -> self#select_arith_comm Ior args
      | Cxor, _ -> self#select_arith_comm Ixor args
      | Clsl, _ -> self#select_arith Ilsl args
      | Clsr, _ -> self#select_arith Ilsr args
      | Casr, _ -> self#select_arith Iasr args
      | Cclz { arg_is_non_zero }, _ -> Iintop (Iclz { arg_is_non_zero }), args
      | Cctz { arg_is_non_zero }, _ -> Iintop (Ictz { arg_is_non_zero }), args
      | Cpopcnt, _ -> Iintop Ipopcnt, args
      | Ccmpi comp, _ -> self#select_arith_comp (Isigned comp) args
      | Caddv, _ -> self#select_arith_comm Iadd args
      | Cadda, _ -> self#select_arith_comm Iadd args
      | Ccmpa comp, _ -> self#select_arith_comp (Iunsigned comp) args
      | Ccmpf (w, comp), _ -> Ifloatop (w, Icompf comp), args
      | Ccsel _, [cond; ifso; ifnot] ->
        let cond, earg = self#select_condition cond in
        Icsel cond, [earg; ifso; ifnot]
      | Cnegf w, _ -> Ifloatop (w, Inegf), args
      | Cabsf w, _ -> Ifloatop (w, Iabsf), args
      | Caddf w, _ -> Ifloatop (w, Iaddf), args
      | Csubf w, _ -> Ifloatop (w, Isubf), args
      | Cmulf w, _ -> Ifloatop (w, Imulf), args
      | Cdivf w, _ -> Ifloatop (w, Idivf), args
      | Creinterpret_cast cast, _ -> Ireinterpret_cast cast, args
      | Cstatic_cast cast, _ -> Istatic_cast cast, args
      | Catomic { op = Fetch_and_add; size }, [src; dst] ->
        let dst_size =
          match size with
          | Word | Sixtyfour -> Word_int
          | Thirtytwo -> Thirtytwo_signed
        in
        let addr, eloc = self#select_addressing dst_size dst in
        Iintop_atomic { op = Fetch_and_add; size; addr }, [src; eloc]
      | Catomic { op = Compare_and_swap; size }, [compare_with; set_to; dst] ->
        let dst_size =
          match size with
          | Word | Sixtyfour -> Word_int
          | Thirtytwo -> Thirtytwo_signed
        in
        let addr, eloc = self#select_addressing dst_size dst in
        ( Iintop_atomic { op = Compare_and_swap; size; addr },
          [compare_with; set_to; eloc] )
      | Cprobe { name; handler_code_sym; enabled_at_init }, _ ->
        Iprobe { name; handler_code_sym; enabled_at_init }, args
      | Cprobe_is_enabled { name }, _ -> Iprobe_is_enabled { name }, []
      | Cbeginregion, _ -> Ibeginregion, []
      | Cendregion, _ -> Iendregion, args
      | _ -> Misc.fatal_error "Selection.select_oper"

    method private select_arith_comm (op : Mach.integer_operation)
        (args : Cmm.expression list) : Mach.operation * Cmm.expression list =
      match args with
      | [arg; Cconst_int (n, _)] when self#is_immediate op n ->
        Iintop_imm (op, n), [arg]
      | [Cconst_int (n, _); arg] when self#is_immediate op n ->
        Iintop_imm (op, n), [arg]
      | _ -> Iintop op, args

    method private select_arith (op : Mach.integer_operation)
        (args : Cmm.expression list) : Mach.operation * Cmm.expression list =
      match args with
      | [arg; Cconst_int (n, _)] when self#is_immediate op n ->
        Iintop_imm (op, n), [arg]
      | _ -> Iintop op, args

    method private select_arith_comp (cmp : Mach.integer_comparison)
        (args : Cmm.expression list) : Mach.operation * Cmm.expression list =
      match args with
      | [arg; Cconst_int (n, _)] when self#is_immediate (Mach.Icomp cmp) n ->
        Iintop_imm (Icomp cmp, n), [arg]
      | [Cconst_int (n, _); arg]
        when self#is_immediate (Mach.Icomp (Select_utils.swap_intcomp cmp)) n ->
        Iintop_imm (Icomp (Select_utils.swap_intcomp cmp), n), [arg]
      | _ -> Iintop (Icomp cmp), args

    (* Instruction selection for conditionals *)

    method select_condition (arg : Cmm.expression) : Mach.test * Cmm.expression
        =
      match arg with
      | Cop (Ccmpi cmp, [arg1; Cconst_int (n, _)], _)
        when self#is_immediate_test (Isigned cmp) n ->
        Iinttest_imm (Isigned cmp, n), arg1
      | Cop (Ccmpi cmp, [Cconst_int (n, _); arg2], _)
        when self#is_immediate_test (Isigned (swap_integer_comparison cmp)) n ->
        Iinttest_imm (Isigned (swap_integer_comparison cmp), n), arg2
      | Cop (Ccmpi cmp, args, _) -> Iinttest (Isigned cmp), Ctuple args
      | Cop (Ccmpa cmp, [arg1; Cconst_int (n, _)], _)
        when self#is_immediate_test (Iunsigned cmp) n ->
        Iinttest_imm (Iunsigned cmp, n), arg1
      | Cop (Ccmpa cmp, [Cconst_int (n, _); arg2], _)
        when self#is_immediate_test (Iunsigned (swap_integer_comparison cmp)) n
        ->
        Iinttest_imm (Iunsigned (swap_integer_comparison cmp), n), arg2
      | Cop (Ccmpa cmp, args, _) -> Iinttest (Iunsigned cmp), Ctuple args
      | Cop (Ccmpf (width, cmp), args, _) ->
        Ifloattest (width, cmp), Ctuple args
      | Cop (Cand, [arg1; Cconst_int (1, _)], _) -> Ioddtest, arg1
      | _ -> Itruetest, arg

    (* Return an array of fresh registers of the given type. Normally
       implemented as Reg.createv, but some ports (e.g. Arm) can override this
       definition to store float values in pairs of integer registers. *)

    method regs_for tys = Reg.createv tys

    (* Buffering of instruction sequences *)

    val mutable instr_seq = Mach.dummy_instr

    method insert_debug _env desc dbg arg res =
      instr_seq <- Mach.instr_cons_debug desc arg res dbg instr_seq

    method insert _env desc arg res =
      (* CR mshinwell: fix debuginfo *)
      instr_seq <- Mach.instr_cons_debug desc arg res Debuginfo.none instr_seq

    method extract_onto (o : Mach.instruction) : Mach.instruction =
      let rec extract res i =
        if i == Mach.dummy_instr
        then res
        else extract { i with Mach.next = res } i.Mach.next
      in
      extract o instr_seq

    method extract = self#extract_onto (Mach.end_instr ())

    (* Insert a sequence of moves from one pseudoreg set to another. *)

    method insert_move env src dst =
      if src.Reg.stamp <> dst.Reg.stamp
      then self#insert env Mach.(Iop Imove) [| src |] [| dst |]

    method insert_moves env src dst =
      for i = 0 to min (Array.length src) (Array.length dst) - 1 do
        self#insert_move env src.(i) dst.(i)
      done

    (* Insert moves and stack offsets for function arguments and results *)

    method insert_move_args env arg loc stacksize =
      if stacksize <> 0
      then self#insert env Mach.(Iop (Istackoffset stacksize)) [||] [||];
      self#insert_moves env arg loc

    method insert_move_results env loc res stacksize =
      self#insert_moves env loc res;
      if stacksize <> 0
      then self#insert env Mach.(Iop (Istackoffset (-stacksize))) [||] [||]

    (* Add an Iop opcode. Can be overridden by processor description to insert
       moves before and after the operation, i.e. for two-address instructions,
       or instructions using dedicated registers. *)

    method insert_op_debug env op dbg rs rd =
      self#insert_debug env Mach.(Iop op) dbg rs rd;
      rd

    method insert_op env op rs rd =
      self#insert_op_debug env op Debuginfo.none rs rd

    (* Emit an expression.

       [bound_name] is the name that will be bound to the result of evaluating
       the expression, if such exists. This is used for emitting debugging info.

       Returns: - [None] if the expression does not finish normally (e.g.
       raises) - [Some rs] if the expression yields a result in registers
       [rs] *)
    method emit_expr (env : environment) exp ~bound_name =
      self#emit_expr_aux env exp ~bound_name

    (* Emit an expression which may end some regions early.

       Returns: - [None] if the expression does not finish normally (e.g.
       raises) - [Some (rs, unclosed)] if the expression yields a result in
       [rs], having left [unclosed] (a suffix of env.regions) regions open *)
    method emit_expr_aux (env : environment) exp ~bound_name
        : Reg.t array option =
      (* Normal case of returning a value: no regions are closed *)
      let ret res = Some res in
      match exp with
      | Cconst_int (n, _dbg) ->
        let r = self#regs_for typ_int in
        ret (self#insert_op env (Mach.Iconst_int (Nativeint.of_int n)) [||] r)
      | Cconst_natint (n, _dbg) ->
        let r = self#regs_for typ_int in
        ret (self#insert_op env (Mach.Iconst_int n) [||] r)
      | Cconst_float32 (n, _dbg) ->
        let r = self#regs_for typ_float32 in
        ret
          (self#insert_op env
             (Mach.Iconst_float32 (Int32.bits_of_float n))
             [||] r)
      | Cconst_float (n, _dbg) ->
        let r = self#regs_for typ_float in
        ret
          (self#insert_op env
             (Mach.Iconst_float (Int64.bits_of_float n))
             [||] r)
      | Cconst_vec128 (bits, _dbg) ->
        let r = self#regs_for typ_vec128 in
        ret (self#insert_op env (Mach.Iconst_vec128 bits) [||] r)
      | Cconst_symbol (n, _dbg) ->
        (* Cconst_symbol _ evaluates to a statically-allocated address, so its
           value fits in a typ_int register and is never changed by the GC.

           Some Cconst_symbols point to statically-allocated blocks, some of
           which may point to heap values. However, any such blocks will be
           registered in the compilation unit's global roots structure, so
           adding this register to the frame table would be redundant *)
        let r = self#regs_for typ_int in
        ret (self#insert_op env (Mach.Iconst_symbol n) [||] r)
      | Cvar v -> (
        try ret (Select_utils.env_find v env)
        with Not_found ->
          Misc.fatal_error
            ("Selection.emit_expr: unbound var " ^ V.unique_name v))
      | Clet (v, e1, e2) -> (
        match self#emit_expr env e1 ~bound_name:(Some v) with
        | None -> None
        | Some r1 -> self#emit_expr_aux (self#bind_let env v r1) e2 ~bound_name)
      | Clet_mut (v, k, e1, e2) -> (
        match self#emit_expr env e1 ~bound_name:(Some v) with
        | None -> None
        | Some r1 ->
          self#emit_expr_aux (self#bind_let_mut env v k r1) e2 ~bound_name)
      | Cphantom_let (_var, _defining_expr, body) ->
        self#emit_expr_aux env body ~bound_name
      | Cassign (v, e1) -> (
        let rv, provenance =
          try Select_utils.env_find_mut v env
          with Not_found ->
            Misc.fatal_error ("Selection.emit_expr: unbound var " ^ V.name v)
        in
        match self#emit_expr env e1 ~bound_name:None with
        | None -> None
        | Some r1 ->
          (if Option.is_some provenance
          then
            let naming_op =
              Mach.Iname_for_debugger
                { ident = v;
                  provenance;
                  which_parameter = None;
                  is_assignment = true;
                  regs = r1
                }
            in
            self#insert_debug env (Mach.Iop naming_op) Debuginfo.none [||] [||]);
          self#insert_moves env r1 rv;
          ret [||])
      | Ctuple [] -> ret [||]
      | Ctuple exp_list -> (
        match self#emit_parts_list env exp_list with
        | None -> None
        | Some (simple_list, ext_env) ->
          ret (self#emit_tuple ext_env simple_list))
      | Cop (Craise k, [arg], dbg) -> (
        match self#emit_expr env arg ~bound_name:None with
        | None -> None
        | Some r1 ->
          let rd = [| Proc.loc_exn_bucket |] in
          self#insert env (Mach.Iop Imove) r1 rd;
          self#insert_debug env (Mach.Iraise k) dbg rd [||];
          Select_utils.set_traps_for_raise env;
          None)
      | Cop (Copaque, args, dbg) -> (
        match self#emit_parts_list env args with
        | None -> None
        | Some (simple_args, env) ->
          let rs = self#emit_tuple env simple_args in
          ret (self#insert_op_debug env Mach.Iopaque dbg rs rs))
      | Cop (Ctuple_field (field, fields_layout), [arg], _dbg) -> (
        match self#emit_expr env arg ~bound_name:None with
        | None -> None
        | Some loc_exp ->
          let flat_size a =
            Array.fold_left (fun acc t -> acc + Array.length t) 0 a
          in
          assert (Array.length loc_exp = flat_size fields_layout);
          let before = Array.sub fields_layout 0 field in
          let size_before = flat_size before in
          let field_slice =
            Array.sub loc_exp size_before (Array.length fields_layout.(field))
          in
          ret field_slice)
      | Cop (op, args, dbg) -> (
        match self#emit_parts_list env args with
        | None -> None
        | Some (simple_args, env) -> (
          let add_naming_op_for_bound_name regs =
            match bound_name with
            | None -> ()
            | Some bound_name ->
              let provenance = VP.provenance bound_name in
              if Option.is_some provenance
              then
                let bound_name = VP.var bound_name in
                let naming_op =
                  Mach.Iname_for_debugger
                    { ident = bound_name;
                      provenance;
                      which_parameter = None;
                      is_assignment = false;
                      regs
                    }
                in
                self#insert_debug env (Mach.Iop naming_op) Debuginfo.none [||]
                  [||]
          in
          let ty = Select_utils.oper_result_type op in
          let new_op, new_args = self#select_operation op simple_args dbg in
          match new_op with
          | Icall_ind ->
            let r1 = self#emit_tuple env new_args in
            let rarg = Array.sub r1 1 (Array.length r1 - 1) in
            let rd = self#regs_for ty in
            let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv rarg) in
            let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
            let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
            self#insert_move_args env rarg loc_arg stack_ofs;
            self#insert_debug env (Mach.Iop new_op) dbg
              (Array.append [| r1.(0) |] loc_arg)
              loc_res;
            (* The destination registers (as per the procedure calling
               convention) need to be named right now, otherwise the result of
               the function call may be unavailable in the debugger immediately
               after the call. *)
            add_naming_op_for_bound_name loc_res;
            self#insert_move_results env loc_res rd stack_ofs;
            Select_utils.set_traps_for_raise env;
            Some rd
          | Icall_imm _ ->
            let r1 = self#emit_tuple env new_args in
            let rd = self#regs_for ty in
            let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv r1) in
            let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
            let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
            self#insert_move_args env r1 loc_arg stack_ofs;
            self#insert_debug env (Mach.Iop new_op) dbg loc_arg loc_res;
            add_naming_op_for_bound_name loc_res;
            self#insert_move_results env loc_res rd stack_ofs;
            Select_utils.set_traps_for_raise env;
            Some rd
          | Iextcall ({ func; ty_args; returns; _ } as r) ->
            let loc_arg, stack_ofs =
              self#emit_extcall_args env ty_args new_args
            in
            let keep_for_checking =
              (not returns)
              && !Select_utils.current_function_is_check_enabled
              && String.equal func Cmm.caml_flambda2_invalid
            in
            let returns, ty =
              if keep_for_checking then true, typ_int else returns, ty
            in
            let rd = self#regs_for ty in
            let loc_res =
              self#insert_op_debug env
                (Mach.Iextcall { r with stack_ofs; returns })
                dbg loc_arg
                (Proc.loc_external_results (Reg.typv rd))
            in
            add_naming_op_for_bound_name loc_res;
            self#insert_move_results env loc_res rd stack_ofs;
            Select_utils.set_traps_for_raise env;
            if returns then ret rd else None
          | Ialloc { bytes = _; mode } ->
            let rd = self#regs_for typ_val in
            let bytes = Select_utils.size_expr env (Ctuple new_args) in
            let alloc_words = (bytes + Arch.size_addr - 1) / Arch.size_addr in
            let op =
              Mach.Ialloc
                { bytes = alloc_words * Arch.size_addr;
                  dbginfo = [{ alloc_words; alloc_dbg = dbg }];
                  mode
                }
            in
            self#insert_debug env (Mach.Iop op) dbg [||] rd;
            add_naming_op_for_bound_name rd;
            self#emit_stores env dbg new_args rd;
            Select_utils.set_traps_for_raise env;
            ret rd
          | Iprobe _ ->
            let r1 = self#emit_tuple env new_args in
            let rd = self#regs_for ty in
            let rd = self#insert_op_debug env new_op dbg r1 rd in
            Select_utils.set_traps_for_raise env;
            ret rd
          | op ->
            let r1 = self#emit_tuple env new_args in
            let rd = self#regs_for ty in
            add_naming_op_for_bound_name rd;
            ret (self#insert_op_debug env op dbg r1 rd)))
      | Csequence (e1, e2) -> (
        match self#emit_expr env e1 ~bound_name:None with
        | None -> None
        | Some _ -> self#emit_expr_aux env e2 ~bound_name)
      | Cifthenelse (econd, _ifso_dbg, eif, _ifnot_dbg, eelse, dbg, _kind) -> (
        let cond, earg = self#select_condition econd in
        match self#emit_expr env earg ~bound_name:None with
        | None -> None
        | Some rarg ->
          let rif, (sif : 'self) = self#emit_sequence env eif ~bound_name in
          let relse, (selse : 'self) =
            self#emit_sequence env eelse ~bound_name
          in
          let r = Select_utils.join env rif sif relse selse ~bound_name in
          self#insert_debug env
            (Mach.Iifthenelse (cond, sif#extract, selse#extract))
            dbg rarg [||];
          r)
      | Cswitch (esel, index, ecases, dbg, _kind) -> (
        match self#emit_expr env esel ~bound_name:None with
        | None -> None
        | Some rsel ->
          let rscases =
            Array.map
              (fun (case, _dbg) -> self#emit_sequence env case ~bound_name)
              ecases
          in
          let r = Select_utils.join_array env rscases ~bound_name in
          self#insert_debug env
            (Mach.Iswitch (index, Array.map (fun (_, s) -> s#extract) rscases))
            dbg rsel [||];
          r)
      | Ccatch (_, [], e1, _) -> self#emit_expr_aux env e1 ~bound_name
      | Ccatch (rec_flag, handlers, body, _) ->
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
              let env, r = Select_utils.env_add_static_exception nfail rs env in
              env, Int.Map.add nfail (r, (ids, rs, e2, dbg, is_cold)) map)
            (env, Int.Map.empty) handlers
        in
        let r_body, s_body = self#emit_sequence env body ~bound_name in
        let translate_one_handler nfail
            (traps_info, (ids, rs, e2, _dbg, is_cold)) =
          assert (List.length ids = List.length rs);
          let trap_stack =
            match (!traps_info : Select_utils.trap_stack_info) with
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
                        Mach.Iname_for_debugger
                          { ident = var;
                            provenance;
                            which_parameter = None;
                            is_assignment = false;
                            regs = r
                          }
                      in
                      seq#insert_debug env (Mach.Iop naming_op) Debuginfo.none
                        [||] [||])
                  ids_and_rs)
          in
          (nfail, trap_stack, is_cold), (r, s)
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
        let r = Select_utils.join_array env a ~bound_name in
        let aux ((nfail, ts, is_cold), (_r, s)) =
          nfail, ts, s#extract, is_cold
        in
        let final_trap_stack =
          match r_body with
          | Some _ -> env.trap_stack
          | None -> (
            (* The body never returns, so the trap stack at the end of the catch
               is the one from the handlers *)
            match l with
            | [] ->
              (* This whole catch never returns *)
              env.trap_stack
            | ((_, ts, _), (_, _)) :: tl ->
              assert (List.for_all (fun ((_, ts', _), (_, _)) -> ts = ts') tl);
              ts)
        in
        self#insert env
          (Mach.Icatch
             (rec_flag, final_trap_stack, List.map aux l, s_body#extract))
          [||] [||];
        r
      | Cexit (lbl, args, traps) -> (
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
            self#insert env (Mach.Iexit (nfail, traps)) [||] [||];
            Select_utils.set_traps nfail handler.traps_ref env.trap_stack traps;
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
                "Selection.emit_expr: Return with too many arguments")))
      | Ctrywith (e1, exn_cont, v, e2, dbg, _value_kind) -> (
        let env_body = Select_utils.env_enter_trywith env exn_cont in
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
                    Mach.Iname_for_debugger
                      { ident = var;
                        provenance;
                        which_parameter = None;
                        is_assignment = false;
                        regs = rv
                      }
                  in
                  seq#insert_debug env (Mach.Iop naming_op) Debuginfo.none [||]
                    [||])
          in
          let r = Select_utils.join env r1 s1 r2 s2 ~bound_name in
          self#insert env
            (Mach.Itrywith
               ( s1#extract,
                 exn_cont,
                 ( env_handler.Select_utils.trap_stack,
                   Mach.instr_cons_debug (Mach.Iop Imove)
                     [| Proc.loc_exn_bucket |] rv dbg s2#extract ) ))
            [||] [||];
          r
        in
        let env = Select_utils.env_add v rv env in
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
          Misc.fatal_errorf "Selection.emit_expr: Unbound handler %d" exn_cont)

    method private emit_sequence ?at_start (env : environment) exp ~bound_name
        : _ * 'self =
      let s : 'self = {<instr_seq = Mach.dummy_instr>} in
      (match at_start with None -> () | Some f -> f s);
      let r = s#emit_expr_aux env exp ~bound_name in
      r, s

    method private bind_let (env : environment) v r1 =
      let env =
        if Select_utils.all_regs_anonymous r1
        then (
          Select_utils.name_regs v r1;
          Select_utils.env_add v r1 env)
        else
          let rv = Reg.createv_like r1 in
          Select_utils.name_regs v rv;
          self#insert_moves env r1 rv;
          Select_utils.env_add v rv env
      in
      let provenance = VP.provenance v in
      (if Option.is_some provenance
      then
        let naming_op =
          Mach.Iname_for_debugger
            { ident = VP.var v;
              which_parameter = None;
              provenance;
              is_assignment = false;
              regs = r1
            }
        in
        self#insert_debug env (Mach.Iop naming_op) Debuginfo.none [||] [||]);
      env

    method private bind_let_mut (env : environment) v k r1 =
      let rv = self#regs_for k in
      Select_utils.name_regs v rv;
      self#insert_moves env r1 rv;
      let provenance = VP.provenance v in
      (if Option.is_some provenance
      then
        let naming_op =
          Mach.Iname_for_debugger
            { ident = VP.var v;
              which_parameter = None;
              provenance = VP.provenance v;
              is_assignment = false;
              regs = r1
            }
        in
        self#insert_debug env (Mach.Iop naming_op) Debuginfo.none [||] [||]);
      Select_utils.env_add ~mut:Mutable v rv env

    (* The following two functions, [emit_parts] and [emit_parts_list], force
       right-to-left evaluation order as required by the Flambda [Un_anf] pass
       (and to be consistent with the bytecode compiler). *)

    method private emit_parts (env : environment) ~effects_after exp =
      let module EC = Select_utils.Effect_and_coeffect in
      let may_defer_evaluation =
        let ec = self#effects_of exp in
        match EC.effect ec with
        | Select_utils.Effect.Arbitrary | Select_utils.Effect.Raise ->
          (* Preserve the ordering of effectful expressions by evaluating them
             early (in the correct order) and assigning their results to
             temporaries. We can avoid this in just one case: if we know that
             every [exp'] in the original expression list (cf.
             [emit_parts_list]) to be evaluated after [exp] cannot possibly
             affect the result of [exp] or depend on the result of [exp], then
             [exp] may be deferred. (Checking purity here is not enough: we need
             to check copurity too to avoid e.g. moving mutable reads earlier
             than the raising of an exception.) *)
          EC.pure_and_copure effects_after
        | Select_utils.Effect.None -> (
          match EC.coeffect ec with
          | Select_utils.Coeffect.None ->
            (* Pure expressions may be moved. *)
            true
          | Select_utils.Coeffect.Read_mutable -> (
            (* Read-mutable expressions may only be deferred if evaluation of
               every [exp'] (for [exp'] as in the comment above) has no effects
               "worse" (in the sense of the ordering in [Effect.t]) than raising
               an exception. *)
            match EC.effect effects_after with
            | Select_utils.Effect.None | Select_utils.Effect.Raise -> true
            | Select_utils.Effect.Arbitrary -> false)
          | Select_utils.Coeffect.Arbitrary -> (
            (* Arbitrary expressions may only be deferred if evaluation of every
               [exp'] (for [exp'] as in the comment above) has no effects. *)
            match EC.effect effects_after with
            | Select_utils.Effect.None -> true
            | Select_utils.Effect.(Arbitrary | Raise) -> false))
      in
      (* Even though some expressions may look like they can be deferred from
         the (co)effect analysis, it may be forbidden to move them. *)
      if may_defer_evaluation && self#is_simple_expr exp
      then Some (exp, env)
      else
        match self#emit_expr env exp ~bound_name:None with
        | None -> None
        | Some r ->
          if Array.length r = 0
          then Some (Ctuple [], env)
          else
            (* The normal case *)
            let id = V.create_local "bind" in
            if Select_utils.all_regs_anonymous r
            then
              (* r is an anonymous, unshared register; use it directly *)
              Some (Cvar id, Select_utils.env_add (VP.create id) r env)
            else
              (* Introduce a fresh temp to hold the result *)
              let tmp = Reg.createv_like r in
              self#insert_moves env r tmp;
              Some (Cvar id, Select_utils.env_add (VP.create id) tmp env)

    method private emit_parts_list (env : environment) exp_list =
      let module EC = Select_utils.Effect_and_coeffect in
      let exp_list_right_to_left, _effect =
        (* Annotate each expression with the (co)effects that happen after it
           when the original expression list is evaluated from right to left.
           The resulting expression list has the rightmost expression first. *)
        List.fold_left
          (fun (exp_list, effects_after) exp ->
            let exp_effect = self#effects_of exp in
            (exp, effects_after) :: exp_list, EC.join exp_effect effects_after)
          ([], EC.none) exp_list
      in
      List.fold_left
        (fun results_and_env (exp, effects_after) ->
          match results_and_env with
          | None -> None
          | Some (result, env) -> (
            match self#emit_parts env exp ~effects_after with
            | None -> None
            | Some (exp_result, env) -> Some (exp_result :: result, env)))
        (Some ([], env))
        exp_list_right_to_left

    method private emit_tuple_not_flattened env exp_list =
      let rec emit_list = function
        | [] -> []
        | exp :: rem -> (
          (* Again, force right-to-left evaluation *)
          let loc_rem = emit_list rem in
          match self#emit_expr env exp ~bound_name:None with
          | None -> assert false (* should have been caught in emit_parts *)
          | Some loc_exp -> loc_exp :: loc_rem)
      in
      emit_list exp_list

    method private emit_tuple env exp_list =
      Array.concat (self#emit_tuple_not_flattened env exp_list)

    method emit_extcall_args env ty_args args =
      let args = self#emit_tuple_not_flattened env args in
      let ty_args =
        if ty_args = [] then List.map (fun _ -> XInt) args else ty_args
      in
      let locs, stack_ofs = Proc.loc_external_arguments ty_args in
      let ty_args = Array.of_list ty_args in
      if stack_ofs <> 0
      then self#insert env (Mach.Iop (Istackoffset stack_ofs)) [||] [||];
      List.iteri
        (fun i arg -> self#insert_move_extcall_arg env ty_args.(i) arg locs.(i))
        args;
      Array.concat (Array.to_list locs), stack_ofs

    method insert_move_extcall_arg env _ty_arg src dst =
      (* The default implementation is one or two ordinary moves. (Two in the
         case of an int64 argument on a 32-bit platform.) It can be overridden
         to use special move instructions, for example a "32-bit move"
         instruction for int32 arguments. *)
      self#insert_moves env src dst

    method emit_stores env dbg data regs_addr =
      let a =
        ref (Arch.offset_addressing Arch.identity_addressing (-Arch.size_int))
      in
      List.iter
        (fun e ->
          let op, arg = self#select_store false !a e in
          match self#emit_expr env arg ~bound_name:None with
          | None -> assert false
          | Some regs -> (
            match op with
            | Istore (_, _, _) ->
              for i = 0 to Array.length regs - 1 do
                let r = regs.(i) in
                let kind =
                  match r.Reg.typ with
                  | Float -> Double
                  | Float32 -> Single { reg = Float32 }
                  | Vec128 ->
                    (* 128-bit memory operations are default unaligned. Aligned
                       (big)array operations are handled separately via cmm. *)
                    Onetwentyeight_unaligned
                  | Val | Addr | Int -> Word_val
                in
                self#insert_debug env
                  (Mach.Iop (Istore (kind, !a, false)))
                  dbg
                  (Array.append [| r |] regs_addr)
                  [||];
                a
                  := Arch.offset_addressing !a
                       (Select_utils.size_component r.Reg.typ)
              done
            | _ ->
              self#insert_debug env (Mach.Iop op) dbg
                (Array.append regs regs_addr)
                [||];
              a := Arch.offset_addressing !a (Select_utils.size_expr env e)))
        data

    (* Same, but in tail position *)

    method private insert_return (env : environment) r
        (traps : trap_action list) =
      match r with
      | None -> ()
      | Some r ->
        let loc = Proc.loc_results_return (Reg.typv r) in
        self#insert_moves env r loc;
        self#insert env (Mach.Ireturn traps) loc [||]

    method private emit_return (env : environment) exp traps =
      self#insert_return env (self#emit_expr_aux env exp ~bound_name:None) traps

    (* Emit an expression in tail position of a function, closing all regions in
       [env.regions] *)
    method emit_tail (env : environment) exp =
      match exp with
      | Clet (v, e1, e2) -> (
        match self#emit_expr env e1 ~bound_name:None with
        | None -> ()
        | Some r1 -> self#emit_tail (self#bind_let env v r1) e2)
      | Clet_mut (v, k, e1, e2) -> (
        match self#emit_expr env e1 ~bound_name:None with
        | None -> ()
        | Some r1 -> self#emit_tail (self#bind_let_mut env v k r1) e2)
      | Cphantom_let (_var, _defining_expr, body) -> self#emit_tail env body
      | Cop ((Capply (ty, Rc_normal) as op), args, dbg) -> (
        match self#emit_parts_list env args with
        | None -> ()
        | Some (simple_args, env) -> (
          let new_op, new_args = self#select_operation op simple_args dbg in
          match new_op with
          | Icall_ind ->
            let r1 = self#emit_tuple env new_args in
            let rd = self#regs_for ty in
            let rarg = Array.sub r1 1 (Array.length r1 - 1) in
            let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv rarg) in
            let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
            let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
            if stack_ofs = 0 && Select_utils.trap_stack_is_empty env
            then (
              let call = Mach.Iop Itailcall_ind in
              self#insert_moves env rarg loc_arg;
              self#insert_debug env call dbg
                (Array.append [| r1.(0) |] loc_arg)
                [||])
            else (
              self#insert_move_args env rarg loc_arg stack_ofs;
              self#insert_debug env (Mach.Iop new_op) dbg
                (Array.append [| r1.(0) |] loc_arg)
                loc_res;
              Select_utils.set_traps_for_raise env;
              self#insert env (Mach.Iop (Istackoffset (-stack_ofs))) [||] [||];
              self#insert env
                (Mach.Ireturn (Select_utils.pop_all_traps env))
                loc_res [||])
          | Icall_imm { func } ->
            let r1 = self#emit_tuple env new_args in
            let rd = self#regs_for ty in
            let loc_arg, stack_ofs_args = Proc.loc_arguments (Reg.typv r1) in
            let loc_res, stack_ofs_res = Proc.loc_results_call (Reg.typv rd) in
            let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
            if stack_ofs = 0 && Select_utils.trap_stack_is_empty env
            then (
              let call = Mach.Iop (Itailcall_imm { func }) in
              self#insert_moves env r1 loc_arg;
              self#insert_debug env call dbg loc_arg [||])
            else if func.sym_name = !Select_utils.current_function_name
                    && Select_utils.trap_stack_is_empty env
            then (
              let call = Mach.Iop (Itailcall_imm { func }) in
              let loc_arg' = Proc.loc_parameters (Reg.typv r1) in
              self#insert_moves env r1 loc_arg';
              self#insert_debug env call dbg loc_arg' [||])
            else (
              self#insert_move_args env r1 loc_arg stack_ofs;
              self#insert_debug env (Mach.Iop new_op) dbg loc_arg loc_res;
              Select_utils.set_traps_for_raise env;
              self#insert env (Mach.Iop (Istackoffset (-stack_ofs))) [||] [||];
              self#insert env
                (Mach.Ireturn (Select_utils.pop_all_traps env))
                loc_res [||])
          | _ -> Misc.fatal_error "Selection.emit_tail"))
      | Csequence (e1, e2) -> (
        match self#emit_expr env e1 ~bound_name:None with
        | None -> ()
        | Some _ -> self#emit_tail env e2)
      | Cifthenelse (econd, _ifso_dbg, eif, _ifnot_dbg, eelse, dbg, _kind) -> (
        let cond, earg = self#select_condition econd in
        match self#emit_expr env earg ~bound_name:None with
        | None -> ()
        | Some rarg ->
          self#insert_debug env
            (Mach.Iifthenelse
               ( cond,
                 self#emit_tail_sequence env eif,
                 self#emit_tail_sequence env eelse ))
            dbg rarg [||])
      | Cswitch (esel, index, ecases, dbg, _kind) -> (
        match self#emit_expr env esel ~bound_name:None with
        | None -> ()
        | Some rsel ->
          let cases =
            Array.map
              (fun (case, _dbg) -> self#emit_tail_sequence env case)
              ecases
          in
          self#insert_debug env (Mach.Iswitch (index, cases)) dbg rsel [||])
      | Ccatch (_, [], e1, _) -> self#emit_tail env e1
      | Ccatch (rec_flag, handlers, e1, _) ->
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
              let env, r = Select_utils.env_add_static_exception nfail rs env in
              env, Int.Map.add nfail (r, (ids, rs, e2, dbg, is_cold)) map)
            (env, Int.Map.empty) handlers
        in
        let s_body = self#emit_tail_sequence env e1 in
        let translate_one_handler nfail (trap_info, (ids, rs, e2, _dbg, is_cold))
            =
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
          let seq =
            self#emit_tail_sequence new_env e2 ~at_start:(fun seq ->
                List.iter
                  (fun ((var, _typ), r) ->
                    let provenance = VP.provenance var in
                    if Option.is_some provenance
                    then
                      let var = VP.var var in
                      let naming_op =
                        Mach.Iname_for_debugger
                          { ident = var;
                            provenance;
                            which_parameter = None;
                            is_assignment = false;
                            regs = r
                          }
                      in
                      seq#insert_debug new_env (Mach.Iop naming_op)
                        Debuginfo.none [||] [||])
                  ids_and_rs)
          in
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
        let new_handlers =
          build_all_reachable_handlers ~already_built:[] ~not_built:handlers_map
          (* Note: we're dropping unreachable handlers here *)
        in
        (* The final trap stack doesn't matter, as it's not reachable. *)
        self#insert env
          (Mach.Icatch (rec_flag, env.trap_stack, new_handlers, s_body))
          [||] [||]
      | Ctrywith (e1, exn_cont, v, e2, dbg, _value_kind) -> (
        let env_body = Select_utils.env_enter_trywith env exn_cont in
        let s1 = self#emit_tail_sequence env_body e1 in
        let rv = self#regs_for typ_val in
        let with_handler env_handler e2 =
          let s2 =
            self#emit_tail_sequence env_handler e2 ~at_start:(fun seq ->
                let provenance = VP.provenance v in
                if Option.is_some provenance
                then
                  let var = VP.var v in
                  let naming_op =
                    Mach.Iname_for_debugger
                      { ident = var;
                        provenance;
                        which_parameter = None;
                        is_assignment = false;
                        regs = rv
                      }
                  in
                  seq#insert_debug env_handler (Mach.Iop naming_op)
                    Debuginfo.none [||] [||])
          in
          self#insert env
            (Mach.Itrywith
               ( s1,
                 exn_cont,
                 ( env_handler.Select_utils.trap_stack,
                   Mach.instr_cons_debug (Iop Imove) [| Proc.loc_exn_bucket |]
                     rv dbg s2 ) ))
            [||] [||]
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
          Misc.fatal_errorf "Selection.emit_expr: Unbound handler %d" exn_cont)
      | Cop _ | Cconst_int _ | Cconst_natint _ | Cconst_float32 _
      | Cconst_float _ | Cconst_symbol _ | Cconst_vec128 _ | Cvar _ | Cassign _
      | Ctuple _ | Cexit _ ->
        self#emit_return env exp (Select_utils.pop_all_traps env)

    method private emit_tail_sequence ?at_start env exp =
      let s = {<instr_seq = Mach.dummy_instr>} in
      (match at_start with None -> () | Some f -> f s);
      s#emit_tail env exp;
      s#extract

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
      self#emit_tail env f.Cmm.fun_body;
      let body = self#extract in
      instr_seq <- Mach.dummy_instr;
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
              Mach.Iname_for_debugger
                { ident = var;
                  provenance;
                  which_parameter = Some param_index;
                  is_assignment = false;
                  regs = hard_regs_for_arg
                }
            in
            self#insert_debug env (Mach.Iop naming_op) Debuginfo.none
              hard_regs_for_arg [||])
        f.Cmm.fun_args;
      self#insert_moves env loc_arg rarg;
      let polled_body =
        if Polling.requires_prologue_poll ~future_funcnames
             ~fun_name:f.Cmm.fun_name.sym_name body
        then
          Mach.instr_cons_debug
            (Iop (Ipoll { return_label = None }))
            [||] [||] f.Cmm.fun_dbg body
        else body
      in
      let body_with_prologue = self#extract_onto polled_body in
      Mach.instr_iter
        (fun instr -> self#mark_instr instr.Mach.desc)
        body_with_prologue;
      { Mach.fun_name = f.Cmm.fun_name.sym_name;
        fun_args = loc_arg;
        fun_body = body_with_prologue;
        fun_codegen_options = f.Cmm.fun_codegen_options;
        fun_dbg = f.Cmm.fun_dbg;
        fun_poll = f.Cmm.fun_poll;
        fun_num_stack_slots = Array.make Proc.num_stack_slot_classes 0;
        fun_contains_calls = !contains_calls
      }
  end
