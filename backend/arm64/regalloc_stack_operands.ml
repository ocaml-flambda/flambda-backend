[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]
open! Regalloc_utils

let debug = false

let basic (map : spilled_map) (instr : Cfg.basic Cfg.instruction) =
  match instr.desc with
  | Op (Specific Imove32) -> (
    if debug then check_lengths instr ~of_arg:1 ~of_res:1;
    match is_spilled map instr.arg.(0), is_spilled map instr.res.(0) with
    | false, false -> All_spilled_registers_rewritten
    | false, true ->
      use_stack_operand map instr.res 0;
      All_spilled_registers_rewritten
    | true, false ->
      use_stack_operand map instr.arg 0;
      All_spilled_registers_rewritten
    | true, true ->
      if Reg.same instr.arg.(0) instr.res.(0)
      then All_spilled_registers_rewritten
      else (
        use_stack_operand map instr.res 0;
        May_still_have_spilled_registers))
  | Op
      (Specific
        ( Ifar_poll | Imuladd | Imulsub | Inegmulf | Imuladdf | Inegmuladdf
        | Imulsubf | Inegmulsubf | Isqrtf | Ifar_alloc _
        | Ishiftarith (_, _)
        | Ibswap _ | Isignext _ | Isimd _ ))
  | Op
      ( Move | Spill | Reload | Opaque | Begin_region | End_region | Dls_get
      | Poll | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
      | Const_vec128 _ | Stackoffset _ | Load _
      | Store (_, _, _)
      | Intop _
      | Intop_imm (_, _)
      | Intop_atomic _
      | Floatop (_, _)
      | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
      | Name_for_debugger _ | Alloc _ | Extcall _ )
  | Reloadretaddr | Prologue | Pushtrap _ | Poptrap _ | Stack_check _ ->
    (* no rewrite *)
    May_still_have_spilled_registers

let terminator (map : spilled_map) (term : Cfg.terminator Cfg.instruction) =
  match term.desc with
  | Call (Probe _) -> may_use_stack_operands_everywhere map term
  | Never | Return | Always _ | Parity_test _ | Truth_test _ | Float_test _
  | Int_test _ | Switch _ | Raise _ | Tailcall_self _ | Tailcall_func _
  | Call (OCaml _ | External _) ->
    (* no rewrite *)
    May_still_have_spilled_registers
