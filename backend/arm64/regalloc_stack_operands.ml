[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils

let debug = false

let basic (map : spilled_map) (instr : Cfg.basic Cfg.instruction) =
  match instr.desc with
  | Op (Specific Imove32) ->
    if debug then check_lengths instr ~of_arg:1 ~of_res:1;
    begin match is_spilled map instr.arg.(0), is_spilled map instr.res.(0) with
    | false, false ->
      All_spilled_registers_rewritten
    | false, true ->
      use_stack_operand map instr.res 0;
      All_spilled_registers_rewritten
    | true, false ->
      use_stack_operand map instr.arg 0;
      All_spilled_registers_rewritten
    | true, true ->
      if Reg.same instr.arg.(0) instr.res.(0) then begin
        All_spilled_registers_rewritten
      end else begin
        use_stack_operand map instr.res 0;
        May_still_have_spilled_registers
      end
    end
  | _ ->
    (* no rewrite *)
    May_still_have_spilled_registers

let terminator (map : spilled_map) (term : Cfg.terminator Cfg.instruction) =
  match term.desc with
  | Prim {op = Probe _; _} ->
    may_use_stack_operands_everywhere map term
  | _ ->
    (* no rewrite *)
    May_still_have_spilled_registers
