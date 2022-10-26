# 2 "backend/amd64/cfg_stack_operands.ml"

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Cfg_regalloc_utils

let debug = true

let join_stack_operands_rewrite a b =
  match a,b with
  | All_spilled_registers_rewritten, All_spilled_registers_rewritten ->
    All_spilled_registers_rewritten
  | All_spilled_registers_rewritten, May_still_have_spilled_registers
  | May_still_have_spilled_registers, All_spilled_registers_rewritten
  | May_still_have_spilled_registers, May_still_have_spilled_registers ->
    May_still_have_spilled_registers

let use_stack_operand_if_spilled map regs index =
  match is_spilled regs.(index) with
  | false -> ()
  | true -> use_stack_operand map regs index

let need_rewrite reg =
  match is_spilled reg with
  | true -> May_still_have_spilled_registers
  | false -> All_spilled_registers_rewritten

let may_use_stack_operand_for_first_and_second_arguments
    : type a . spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun map instr ->
    if debug then check_lengths instr ~of_arg:3 ~of_res:1;
    check_same "res(0)" instr.res.(0) "arg(2)" instr.arg.(2);
    use_stack_operand_if_spilled map instr.arg 0;
    use_stack_operand_if_spilled map instr.arg 1;
    match is_spilled instr.res.(0) with
    | true -> May_still_have_spilled_registers
    | false -> All_spilled_registers_rewritten

let may_use_stack_for_first_or_second_and_third_arguments
  : type a . spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun map instr ->
    if debug then check_lengths instr ~of_arg:4 ~of_res:1;
    check_same "res(0)" instr.res.(0) "arg(3)" instr.arg.(3);
    let rewrite_first_or_second =
      match is_spilled instr.arg.(0), is_spilled instr.arg.(1) with
      | false, false ->
        All_spilled_registers_rewritten
      | true, false ->
        use_stack_operand map instr.arg 0;
        All_spilled_registers_rewritten
      | false, true ->
        use_stack_operand map instr.arg 1;
        All_spilled_registers_rewritten
      | true, true ->
        use_stack_operand map instr.arg 0;
        May_still_have_spilled_registers
    in
    use_stack_operand_if_spilled map instr.arg 2;
    join_stack_operands_rewrite
      (need_rewrite instr.res.(0))
      rewrite_first_or_second

let may_use_stack_operand_for_second_argument
  : type a . spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun map instr ->
  if debug then begin
    check_lengths instr ~of_arg:2 ~of_res:1;
    check_same "res(0)" instr.res.(0) "arg(0)" instr.arg.(0);
  end;
  use_stack_operand_if_spilled map instr.arg 1;
  need_rewrite instr.res.(0)

let may_use_stack_operand_for_only_argument
  : type a . has_result:bool -> spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun ~has_result map instr ->
  if debug then check_lengths instr ~of_arg:1 ~of_res:(if has_result then 1 else 0);
  use_stack_operand_if_spilled map instr.arg 0;
  if has_result then
    need_rewrite instr.res.(0)
  else
    All_spilled_registers_rewritten

let may_use_stack_operand_for_only_result
  : type a . spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun map instr ->
  if debug then check_lengths instr ~of_arg:0 ~of_res:1;
  use_stack_operand_if_spilled map instr.res 0;
  All_spilled_registers_rewritten

let may_use_stack_operand_for_result_one_arg
  : type a . spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun map instr ->
  if debug then begin
    check_lengths instr ~of_arg:1 ~of_res:1;
  end;
  (* CR gyorsh: this condition is stronger than the one we had before,
     but I think it always holds in the current version. *)
  check_same "res(0)" instr.res.(0) "arg(0)" instr.arg.(0);
  use_stack_operand_if_spilled map instr.res 0;
  All_spilled_registers_rewritten

let may_use_stack_operand_for_result_two_args
  : type a . spilled_map -> a Cfg.instruction -> stack_operands_rewrite
  = fun map instr ->
  if debug then begin
    check_lengths instr ~of_arg:1 ~of_res:1;
  end;
  check_same "res(0)" instr.res.(0) "arg(0)" instr.arg.(0);
  use_stack_operand_if_spilled map instr.res 0;
  need_rewrite instr.res.(0);

type result =
  | No_result
  | Result_can_be_on_stack
  | Result_cannot_be_on_stack

let is_stack_operand : Reg.t -> bool =
  fun reg ->
    match reg.loc with
    | Stack _ -> true
    | Unknown | Reg _ -> false

(* CR gyorsh: [join_stack_operands_rewrite] can simplify binary_operation *)
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
    match is_spilled instr.arg.(0), is_spilled instr.arg.(1) with
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

let basic (map : spilled_map) (instr : Cfg.basic Cfg.instruction) =
  begin match instr.desc with
  | Op (Addf | Subf | Mulf | Divf)
  | Op (Specific (Ifloat_min | Ifloat_max | Icrc32q)) ->
    may_use_stack_operand_for_second_argument map instr
  | Op (Floatofint | Intoffloat) ->
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
  | Op (Specific (Ifloat_iround | Ifloat_round _))
  | Op (Intop_imm (Icomp _, _)) ->
    may_use_stack_operand_for_only_argument map instr ~has_result:true
  | Op (Intop_imm (Iadd, _)) when (Reg.same_loc instr.arg.(0) instr.res.(0)) ->
    May_still_have_spilled_registers
  | Op (Intop(Ilsl | Ilsr | Iasr)) ->
    may_use_stack_operand_for_result_two_args map instr
  | Op(Intop_imm((Iadd | Isub | Iand | Ior | Ixor | Ilsl | Ilsr | Iasr), _)) ->
    may_use_stack_operand_for_result_one_arg map instr
  | Op(Csel tst) ->
    (match tst with
     | Iinttest _ ->
       may_use_stack_for_first_or_second_and_third_arguments map instr
     | Ifloattest _ ->
       (* CR gyorsh: this could be optimized *)
       May_still_have_spilled_registers
     | Iinttest_imm (_, _)
     | Itruetest
     | Ifalsetest
     | Ioddtest
     | Ieventest ->
       may_use_stack_operand_for_first_and_second_arguments map instr)
  | Op (Specific (Ilfence | Isfence | Imfence))
  | Op (Intop(Imulh _ | Imul | Idiv | Imod))
  | Op (Intop_imm ((Imulh _ | Imul | Idiv | Imod), _))
  | Op (Specific (Irdtsc | Irdpmc))
  | Op (Intop (Ipopcnt | Iclz _| Ictz _))
  | Op (Move | Spill | Reload | Negf | Absf | Const_float _ | Compf _ | Stackoffset _
       | Load _ | Store _ | Name_for_debugger _ | Probe_is_enabled _
       | Valueofint | Intofvalue | Opaque | Begin_region | End_region )
  | Op (Specific (Isqrtf | Isextend32 | Izextend32 | Ilea _
                 | Istore_int (_, _, _)
                 | Ioffset_loc (_, _) | Ifloatarithmem (_, _)
                 | Ipause
                 | Iprefetch _
                 | Ibswap _| Ifloatsqrtf _))
  | Reloadretaddr
  | Pushtrap _
  | Poptrap
  | Prologue ->
    (* no rewrite *)
    May_still_have_spilled_registers
  | Op (Intop Icheckbound)
  | Op (Intop_imm ((Ipopcnt | Iclz _ | Ictz _ | Icheckbound), _)) ->
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
  | Int_test { lt = _; eq = _; gt =_; is_signed = _; imm = Some _; }
  | Parity_test { ifso = _; ifnot = _; }
  | Truth_test { ifso = _; ifnot = _; }
  | Prim {op = Checkbound { immediate = Some _; }; _} ->
    may_use_stack_operand_for_only_argument ~has_result:false map term
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
