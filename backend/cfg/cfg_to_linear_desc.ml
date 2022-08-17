open Cfg_intf.S

let from_basic (basic : basic) : Linear.instruction_desc =
  match basic with
  | Prologue -> Lprologue
  | Reloadretaddr -> Lreloadretaddr
  | Pushtrap { lbl_handler } -> Lpushtrap { lbl_handler }
  | Poptrap -> Lpoptrap
  | Call (F Indirect) -> Lop Icall_ind
  | Call (F (Direct { func_symbol })) -> Lop (Icall_imm { func = func_symbol })
  | Call (P (External { func_symbol; alloc; ty_args; ty_res })) ->
    Lop
      (Iextcall { func = func_symbol; alloc; ty_args; ty_res; returns = true })
  | Call (P (Checkbound { immediate = None })) -> Lop (Iintop Icheckbound)
  | Call (P (Checkbound { immediate = Some i })) ->
    Lop (Iintop_imm (Icheckbound, i))
  | Call (P (Alloc { bytes; dbginfo; mode })) ->
    Lop (Ialloc { bytes; dbginfo; mode })
  | Op op ->
    let op : Mach.operation =
      match op with
      | Move -> Imove
      | Spill -> Ispill
      | Reload -> Ireload
      | Const_int n -> Iconst_int n
      | Const_float n -> Iconst_float n
      | Const_symbol n -> Iconst_symbol n
      | Stackoffset n -> Istackoffset n
      | Load (c, m, i) -> Iload (c, m, i)
      | Store (c, m, b) -> Istore (c, m, b)
      | Intop op -> Iintop op
      | Intop_imm (op, i) -> Iintop_imm (op, i)
      | Negf -> Inegf
      | Absf -> Iabsf
      | Addf -> Iaddf
      | Subf -> Isubf
      | Mulf -> Imulf
      | Divf -> Idivf
      | Compf c -> Icompf c
      | Floatofint -> Ifloatofint
      | Intoffloat -> Iintoffloat
      | Probe { name; handler_code_sym } -> Iprobe { name; handler_code_sym }
      | Probe_is_enabled { name } -> Iprobe_is_enabled { name }
      | Opaque -> Iopaque
      | Specific op -> Ispecific op
      | Begin_region -> Ibeginregion
      | End_region -> Iendregion
      | Name_for_debugger { ident; which_parameter; provenance; is_assignment }
        ->
        Iname_for_debugger { ident; which_parameter; provenance; is_assignment }
    in
    Lop op
