# 2 "backend/amd64/cfg_stack_operands.ml"

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils

let debug = false

let may_use_stack_operand_for_second_argument
  : type a . num_args:int -> res_is_fst:bool ->
             spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun ~num_args ~res_is_fst map instr ->
  if debug then begin
    check_lengths instr ~of_arg:num_args ~of_res:1;
    if res_is_fst then begin
      check_same "res(0)" instr.res.(0) "arg(0)" instr.arg.(0);
    end;
  end;
  begin match is_spilled map instr.arg.(1) with
  | false -> ()
  | true ->
    use_stack_operand map instr.arg 1;
  end;
  May_still_have_spilled_registers

let may_use_stack_operand_for_only_argument
  : type a . has_result:bool -> spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun ~has_result map instr ->
  if debug then check_lengths instr ~of_arg:1 ~of_res:(if has_result then 1 else 0);
  begin match is_spilled map instr.arg.(0) with
  | false -> ()
  | true ->
    use_stack_operand map instr.arg 0
  end;
  if has_result then
    May_still_have_spilled_registers
  else
    All_spilled_registers_rewritten

let may_use_stack_operand_for_only_result
  : type a . spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun map instr ->
  if debug then check_lengths instr ~of_arg:0 ~of_res:1;
  begin match is_spilled map instr.res.(0) with
  | false ->
    All_spilled_registers_rewritten
  | true ->
    use_stack_operand map instr.res 0;
    All_spilled_registers_rewritten
  end

let may_use_stack_operand_for_result
  : type a . num_args:int -> spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun ~num_args map instr ->
  if debug then check_lengths instr ~of_arg:num_args ~of_res:1;
  begin match is_spilled map instr.res.(0) with
  | false -> ()
  | true ->
    if Reg.same instr.arg.(0) instr.res.(0) then begin
      use_stack_operand map instr.arg 0;
    end;
    use_stack_operand map instr.res 0;
  end;
  May_still_have_spilled_registers

type result =
  | No_result
  | Result_can_be_on_stack
  | Result_cannot_be_on_stack

let is_stack_operand : Reg.t -> bool =
  fun reg ->
    match reg.loc with
    | Stack _ -> true
    | Unknown | Reg _ -> false

let binary_operation
  : type a . spilled_map -> a Cfg.instruction -> result -> stack_operands_rewrite
  = fun map instr result ->
  if debug then begin
    match result with
    | No_result ->
      check_lengths instr ~of_arg:2 ~of_res:0
    | Result_can_be_on_stack ->
      check_lengths instr ~of_arg:2 ~of_res:1;
      check_same "res(0)" instr.res.(0) "arg(0)" instr.arg.(0)
    | Result_cannot_be_on_stack ->
      check_lengths instr ~of_arg:2 ~of_res:1
  end;
  let already_has_memory_operand =
    is_stack_operand instr.arg.(0)
    || is_stack_operand instr.arg.(1)
    || (match result with
      | No_result -> false
      | Result_cannot_be_on_stack ->
        assert (not (is_stack_operand instr.res.(0)));
        false
      | Result_can_be_on_stack ->
        (* note: actually unreachable since instr.res.(0) and
           instr.arg.(0) are the same. *)
        is_stack_operand instr.res.(0))
  in
  if already_has_memory_operand then
    May_still_have_spilled_registers
  else begin
    match is_spilled map instr.arg.(0), is_spilled map instr.arg.(1) with
    | false, false ->
      begin match result with
      | No_result | Result_can_be_on_stack ->
        All_spilled_registers_rewritten
      | Result_cannot_be_on_stack ->
        May_still_have_spilled_registers
      end
    | false, true ->
      use_stack_operand map instr.arg 1;
      begin match result with
      | No_result ->
        All_spilled_registers_rewritten
      | Result_can_be_on_stack | Result_cannot_be_on_stack ->
        May_still_have_spilled_registers
      end
    | true, false ->
      (* note: slightly different from the case above, because arg.(0) and res.(0) are the same. *)
      begin match result with
      | No_result ->
        use_stack_operand map instr.arg 0;
        All_spilled_registers_rewritten
      | Result_can_be_on_stack ->
        use_stack_operand map instr.res 0;
        use_stack_operand map instr.arg 0;
        All_spilled_registers_rewritten
      | Result_cannot_be_on_stack ->
        use_stack_operand map instr.arg 0;
        May_still_have_spilled_registers
      end;
    | true, true ->
      if Reg.same instr.arg.(0) instr.arg.(1) then
        May_still_have_spilled_registers
      else begin
        match result with
        | No_result ->
          (* CR xclerc for xclerc: try and find for a criterion to choose between
             the two. *)
          use_stack_operand map instr.arg 0;
          May_still_have_spilled_registers
        | Result_can_be_on_stack ->
          use_stack_operand map instr.arg 0;
          use_stack_operand map instr.res 0;
          May_still_have_spilled_registers
        | Result_cannot_be_on_stack ->
          use_stack_operand map instr.arg 0;
          May_still_have_spilled_registers
      end
  end

let unary_operation_argument_or_result_on_stack
  : type a . spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun map instr ->
  if debug then check_lengths instr ~of_arg:1 ~of_res:1;
  if is_stack_operand instr.arg.(0) || is_stack_operand instr.res.(0)
  then May_still_have_spilled_registers
  else match is_spilled map instr.arg.(0), is_spilled map instr.res.(0) with
  | false, false -> All_spilled_registers_rewritten
  | false, true ->
    use_stack_operand map instr.res 0;
    All_spilled_registers_rewritten
  | true, false ->
    use_stack_operand map instr.arg 0;
    All_spilled_registers_rewritten
  | true, true ->
    if Reg.same instr.arg.(0) instr.res.(0)
    then May_still_have_spilled_registers
    else begin
      use_stack_operand map instr.arg 0;
      May_still_have_spilled_registers
    end

let basic (map : spilled_map) (instr : Cfg.basic Cfg.instruction) =
  begin match instr.desc with
  | Op (Addf | Subf | Mulf | Divf) ->
    may_use_stack_operand_for_second_argument map instr ~num_args:2 ~res_is_fst:true
  | Op (Specific (Isimd op)) ->
    (match Simd_selection.register_behavior op with
    | R_to_fst | R_to_R | R_R_to_fst -> May_still_have_spilled_registers
    | R_RM_to_fst ->
      may_use_stack_operand_for_second_argument map instr ~num_args:2 ~res_is_fst:true
    | R_RM_to_rcx | R_RM_to_xmm0 | R_RM_to_R ->
      may_use_stack_operand_for_second_argument map instr ~num_args:2 ~res_is_fst:false
    | R_RM_rax_rdx_to_rcx | R_RM_rax_rdx_to_xmm0 ->
      may_use_stack_operand_for_second_argument map instr ~num_args:4 ~res_is_fst:false
    | R_RM_xmm0_to_fst ->
      may_use_stack_operand_for_second_argument map instr ~num_args:3 ~res_is_fst:true
    | R_to_RM -> may_use_stack_operand_for_result map instr ~num_args:1
    | RM_to_R -> may_use_stack_operand_for_only_argument map instr ~has_result:true)
  | Op (Scalarcast (V128_to_scalar (Float64x2) | V128_of_scalar (Float64x2))) ->
    unary_operation_argument_or_result_on_stack map instr
  | Op (Scalarcast (V128_to_scalar (Float32x4) | V128_of_scalar (Float32x4))) ->
    (* CR mslater: (SIMD) replace once we have unboxed float32 *)
    may_use_stack_operand_for_only_argument map instr ~has_result:true
  | Op (Scalarcast (V128_of_scalar (Int64x2 | Int32x4 | Int16x8 | Int8x16))) ->
    may_use_stack_operand_for_only_argument map instr ~has_result:true
  | Op (Scalarcast (V128_to_scalar (Int64x2 | Int32x4))) ->
    may_use_stack_operand_for_result map instr ~num_args:1
  | Op (Scalarcast (V128_to_scalar (Int16x8 | Int8x16))) ->
    (* CR mslater: (SIMD) replace once we have unboxed int16/int8 *)
    May_still_have_spilled_registers
  | Op (Floatofint | Intoffloat | Vectorcast _) ->
    may_use_stack_operand_for_only_argument map instr ~has_result:true
  | Op (Const_symbol _) ->
    if !Clflags.pic_code || !Clflags.dlcode || Arch.win64 then
      May_still_have_spilled_registers
    else
      may_use_stack_operand_for_only_result map instr
  | Op (Const_int n) ->
    if n <= 0x7FFFFFFFn && n >= -0x80000000n then begin
      may_use_stack_operand_for_only_result map instr
    end else begin
      May_still_have_spilled_registers
    end
  | Op (Intop (Iadd | Isub | Iand | Ior | Ixor)) ->
    binary_operation map instr Result_can_be_on_stack
  | Op (Intop (Icomp _)) ->
    binary_operation map instr Result_cannot_be_on_stack
  | Op (Intop_imm (Icomp _, _)) ->
    may_use_stack_operand_for_only_argument map instr ~has_result:true
  | Op (Intop_imm (Iadd, _)) ->
    (* Conservatively assume it will be turned into a `lea` instruction,
       and ask for everything to be in registers. *)
    May_still_have_spilled_registers
  | Op (Intop(Ilsl | Ilsr | Iasr)) ->
    may_use_stack_operand_for_result map instr ~num_args:2
  | Op(Intop_imm((Isub | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr), _)) ->
    may_use_stack_operand_for_result map instr ~num_args:1
  | Op (Csel _) (* CR gyorsh: optimize *)
  | Op (Specific (Ilfence | Isfence | Imfence))
  | Op (Intop(Imulh _ | Imul | Idiv | Imod))
  | Op (Intop_imm ((Imulh _ | Imul | Idiv | Imod), _))
  | Op (Specific (Irdtsc | Irdpmc))
  | Op (Intop (Ipopcnt | Iclz _| Ictz _))
  | Op (Intop_atomic _)
  | Op (Move | Spill | Reload | Negf | Absf | Const_float _  | Const_vec128 _ | Compf _
       | Stackoffset _ | Load _ | Store _ | Name_for_debugger _ | Probe_is_enabled _
       | Valueofint | Intofvalue | Opaque | Begin_region | End_region | Dls_get )
  | Op (Specific (Isextend32 | Izextend32 | Ilea _
                 | Istore_int (_, _, _)
                 | Ioffset_loc (_, _) | Ifloatarithmem (_, _)
                 | Ipause
                 | Iprefetch _
                 | Ibswap _ | Ifloatsqrtf _))
  | Reloadretaddr
  | Pushtrap _
  | Poptrap
  | Prologue ->
    (* no rewrite *)
    May_still_have_spilled_registers
  | Op (Intop (Icheckbound | Icheckalign _))
  | Op (Intop_imm ((Ipopcnt | Iclz _ | Ictz _ | Icheckbound | Icheckalign _), _)) ->
    (* should not happen *)
    fatal "unexpected instruction"
  end

let terminator (map : spilled_map) (term : Cfg.terminator Cfg.instruction) =
  ignore map;
  match (term : Cfg.terminator Cfg.instruction).desc with
  | Never -> fatal "unexpected terminator"
  | Int_test { lt = _; eq = _; gt =_; is_signed = _; imm = None; }
  | Prim  {op = Checkbound { immediate = None; }; _} ->
    binary_operation map term No_result
  | Prim  {op = Checkalign { immediate = None; _ }; _} ->
    may_use_stack_operand_for_only_argument ~has_result:false map term
  | Int_test { lt = _; eq = _; gt =_; is_signed = _; imm = Some _; }
  | Parity_test { ifso = _; ifnot = _; }
  | Truth_test { ifso = _; ifnot = _; }
  | Prim {op = Checkbound { immediate = Some _; }; _} ->
    may_use_stack_operand_for_only_argument ~has_result:false map term
  | Prim {op = Checkalign { immediate = Some _; _ }; _} ->
    if debug then check_lengths term ~of_arg:0 ~of_res:0;
    All_spilled_registers_rewritten
  | Float_test _ ->
    (* CR-someday xclerc for xclerc: this could be optimized, but the representation
       makes it more difficult than the cases above, because (i) multiple
       branching instructions may be emitted and (ii) the operand constraints
       depend on the exact kind of branch (because we sometimes swap the
       operands). *)
    May_still_have_spilled_registers
  | Always _
  | Return
  | Raise _
  | Switch _
  | Tailcall_self _
  | Tailcall_func _
  | Call_no_return _
  | Poll_and_jump _
  | Prim {op = External _ | Alloc _; _ } | Call {op = Indirect | Direct _; _} ->
    (* no rewrite *)
    May_still_have_spilled_registers
  | Prim {op = Probe _; _} ->
    may_use_stack_operands_everywhere map term
  | Specific_can_raise _ ->
    fatal "no instructions specific for this architecture can raise"
