open Cfg_intf.S

let from_basic (basic : basic) : Linear.instruction_desc =
  match basic with
  | Prologue -> Lprologue
  | Reloadretaddr -> Lreloadretaddr
  | Pushtrap { lbl_handler } -> Lpushtrap { lbl_handler }
  | Poptrap -> Lpoptrap
  | Op op ->
    let op : Mach.operation =
      match op with
      | Move -> Imove
      | Spill -> Ispill
      | Reload -> Ireload
      | Const_int n -> Iconst_int n
      | Const_float n -> Iconst_float n
      | Const_symbol n -> Iconst_symbol n
      | Const_vec128 bits -> Iconst_vec128 bits
      | Stackoffset n -> Istackoffset n
      | Load (c, m, i) -> Iload (c, m, i)
      | Store (c, m, b) -> Istore (c, m, b)
      | Intop op -> Iintop op
      | Intop_imm (op, i) -> Iintop_imm (op, i)
      | Intop_atomic { op; size; addr } -> Iintop_atomic { op; size; addr }
      | Negf -> Inegf
      | Absf -> Iabsf
      | Addf -> Iaddf
      | Subf -> Isubf
      | Mulf -> Imulf
      | Divf -> Idivf
      | Compf c -> Icompf c
      | Csel c -> Icsel c
      | Floatofint -> Ifloatofint
      | Intoffloat -> Iintoffloat
      | Valueofint -> Ivalueofint
      | Intofvalue -> Iintofvalue
      | Vectorcast cast -> Ivectorcast cast
      | Scalarcast cast -> Iscalarcast cast
      | Probe_is_enabled { name } -> Iprobe_is_enabled { name }
      | Opaque -> Iopaque
      | Specific op -> Ispecific op
      | Begin_region -> Ibeginregion
      | End_region -> Iendregion
      | Name_for_debugger
          { ident; which_parameter; provenance; is_assignment; regs } ->
        Iname_for_debugger
          { ident; which_parameter; provenance; is_assignment; regs }
    in
    Lop op
