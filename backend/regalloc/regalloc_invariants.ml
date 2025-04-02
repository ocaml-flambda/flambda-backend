[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils

let precondition : Cfg_with_layout.t -> unit =
 fun cfg_with_layout ->
  (* note: the `live` field is set, because we want to call the `Deadcode` pass
     before `Cfgize`. *)
  let desc_is_neither_spill_or_reload (id : InstructionId.t) (desc : Cfg.basic)
      : unit =
    match desc with
    | Op op -> (
      match op with
      | Move -> ()
      | Spill -> fatal "instruction %a is a spill" InstructionId.format id
      | Reload -> fatal "instruction %a is a reload" InstructionId.format id
      | Const_int _ -> ()
      | Const_float32 _ -> ()
      | Const_float _ -> ()
      | Const_symbol _ -> ()
      | Const_vec128 _ -> ()
      | Stackoffset _ -> ()
      | Load _ -> ()
      | Store _ -> ()
      | Intop _ -> ()
      | Intop_imm _ -> ()
      | Intop_atomic _ -> ()
      | Floatop _ -> ()
      | Csel _ -> ()
      | Reinterpret_cast _ -> ()
      | Static_cast _ -> ()
      | Probe_is_enabled _ -> ()
      | Opaque -> ()
      | Begin_region -> ()
      | End_region -> ()
      | Specific op ->
        if Arch.operation_can_raise op
        then
          fatal
            "architecture specific instruction %a that can raise but isn't a \
             terminator"
            InstructionId.format id
      | Name_for_debugger _ -> ()
      | Dls_get -> ()
      | Poll -> ()
      | Alloc _ -> ())
    | Reloadretaddr | Pushtrap _ | Poptrap | Prologue | Stack_check _ -> ()
  in
  let register_must_not_be_on_stack (id : InstructionId.t) (reg : Reg.t) : unit
      =
    match reg.Reg.loc with
    | Unknown -> () (* most registers are not precolored *)
    | Reg _ ->
      () (* some register are precolored, e.g. to enforce constraints *)
    | Stack (Incoming _ | Outgoing _ | Domainstate _) ->
      (* incoming/outgoing/domainstate locations are for function parameters *)
      ()
    | Stack (Local _) ->
      (* local stack locations are for spilling, and will be introduced by the
         register allocator *)
      fatal "instruction %a has a register with a stack location"
        InstructionId.format id
  in
  let registers_must_not_be_on_stack (id : InstructionId.t) (regs : Reg.t array)
      : unit =
    ArrayLabels.iter regs ~f:(register_must_not_be_on_stack id)
  in
  (* CR xclerc for xclerc: the check below should not be in this function, since
     it is IRC-specific *)
  let register_must_be_on_unknown_list (id : InstructionId.t) (reg : Reg.t) :
      unit =
    match reg.Reg.irc_work_list with
    | Unknown_list -> ()
    | Precolored -> ()
    | Initial | Simplify | Freeze | Spill | Spilled | Coalesced | Colored
    | Select_stack ->
      fatal "instruction %a has a register (%a) already in a work list (%S)"
        InstructionId.format id Printreg.reg reg
        (Reg.string_of_irc_work_list reg.Reg.irc_work_list)
  in
  let register_must_be_on_unknown_list (id : InstructionId.t)
      (regs : Reg.t array) : unit =
    ArrayLabels.iter regs ~f:(register_must_be_on_unknown_list id)
  in
  Cfg_with_layout.iter_instructions cfg_with_layout
    ~instruction:(fun instr ->
      let id = instr.id in
      desc_is_neither_spill_or_reload id instr.desc;
      registers_must_not_be_on_stack id instr.arg;
      registers_must_not_be_on_stack id instr.res;
      register_must_be_on_unknown_list id instr.arg;
      register_must_be_on_unknown_list id instr.res)
    ~terminator:(fun term ->
      let id = term.id in
      registers_must_not_be_on_stack id term.arg;
      registers_must_not_be_on_stack id term.res;
      register_must_be_on_unknown_list id term.arg;
      register_must_be_on_unknown_list id term.res);
  let fun_num_stack_slots =
    (Cfg_with_layout.cfg cfg_with_layout).fun_num_stack_slots
  in
  Stack_class.Tbl.iter fun_num_stack_slots ~f:(fun stack_class num_slots ->
      if num_slots <> 0
      then
        fatal "stack slot class %a has %d slots(s)" Stack_class.print
          stack_class num_slots)

let postcondition_layout : Cfg_with_layout.t -> unit =
 fun cfg_with_layout ->
  let register_must_not_be_unknown (id : InstructionId.t) (reg : Reg.t) : unit =
    match reg.Reg.loc with
    | Reg _ -> ()
    | Stack (Local _ | Incoming _ | Outgoing _ | Domainstate _) -> ()
    | Unknown ->
      fatal "instruction %a has a register (%a) with an unknown location"
        InstructionId.format id Printreg.reg reg
  in
  let registers_must_not_be_unknown (id : InstructionId.t) (regs : Reg.t array)
      : unit =
    ArrayLabels.iter regs ~f:(register_must_not_be_unknown id)
  in
  let num_stack_locals (regs : Reg.t array) : int =
    Array.fold_left regs ~init:0 ~f:(fun acc reg ->
        match reg.Reg.loc with
        | Unknown | Reg _ | Stack (Incoming _ | Outgoing _ | Domainstate _) ->
          acc
        | Stack (Local _) -> succ acc)
  in
  let arch_constraints (id : InstructionId.t) (desc : Cfg.basic)
      (arg : Reg.t array) (res : Reg.t array) : unit =
    match Config.architecture with
    (* CR xclerc for xclerc: what about cross-compilation? *)
    | "amd64" | "arm64" -> (
      let num_locals = num_stack_locals arg + num_stack_locals res in
      match[@ocaml.warning "-4"] desc with
      | Op (Spill | Reload) ->
        (* CR xclerc for xclerc: should check arg/res according to spill/reload,
           rather than the total number. *)
        if num_locals > 1
        then
          fatal "instruction %a is a move and refers to %d spilling slots"
            InstructionId.format id num_locals
      | _ -> ())
    | arch -> fatal "unsupported architecture %S" arch
  in
  let register_classes_must_be_consistent (id : InstructionId.t) (reg : Reg.t) :
      unit =
    match reg.Reg.loc with
    | Reg phys_reg -> (
      try
        let (_ : Reg.t) = Proc.phys_reg reg.typ phys_reg in
        ()
      with Invalid_argument _ ->
        fatal
          "instruction %a assigned %a to register %i, which has an \
           incompatible class"
          InstructionId.format id Printreg.reg reg phys_reg)
    | Stack _ | Unknown -> ()
  in
  let register_classes_must_be_consistent (id : InstructionId.t)
      (regs : Reg.t array) : unit =
    ArrayLabels.iter regs ~f:(register_classes_must_be_consistent id)
  in
  let module Int = Numbers.Int in
  let used_stack_slots = Stack_class.Tbl.init ~f:(fun _ -> Int.Set.empty) in
  let record_stack_slot_use (reg : Reg.t) : unit =
    match reg.loc with
    | Unknown -> ()
    | Reg _ -> ()
    | Stack stack_loc -> (
      match stack_loc with
      | Local index ->
        let stack_class = Stack_class.of_machtype reg.typ in
        Stack_class.Tbl.update used_stack_slots stack_class ~f:(fun curr ->
            Int.Set.add index curr)
      | Incoming _ -> ()
      | Outgoing _ -> ()
      | Domainstate _ -> ())
  in
  let record_stack_slot_uses (regs : Reg.t array) : unit =
    Array.iter ~f:record_stack_slot_use regs
  in
  (* CR-someday xclerc for xclerc: for some properties, it may not be necessary
     to iterate over the instructions, iterating over the registers could be
     enough. *)
  Cfg_with_layout.iter_instructions cfg_with_layout
    ~instruction:(fun instr ->
      let id = instr.id in
      registers_must_not_be_unknown id instr.arg;
      registers_must_not_be_unknown id instr.res;
      arch_constraints id instr.desc instr.arg instr.res;
      register_classes_must_be_consistent id instr.arg;
      register_classes_must_be_consistent id instr.res;
      record_stack_slot_uses instr.arg;
      record_stack_slot_uses instr.res)
    ~terminator:(fun term ->
      let id = term.id in
      registers_must_not_be_unknown id term.arg;
      registers_must_not_be_unknown id term.res;
      register_classes_must_be_consistent id term.arg;
      register_classes_must_be_consistent id term.res;
      record_stack_slot_uses term.arg;
      record_stack_slot_uses term.res);
  let fun_num_stack_slots =
    (Cfg_with_layout.cfg cfg_with_layout).fun_num_stack_slots
  in
  Stack_class.Tbl.iter fun_num_stack_slots ~f:(fun stack_class num_slots ->
      let available_slots =
        Seq.ints 0 |> Seq.take num_slots |> Int.Set.of_seq
      in
      let string_of_set set =
        set |> Int.Set.elements |> List.map ~f:string_of_int
        |> String.concat ", "
      in
      let used_slots = Stack_class.Tbl.find used_stack_slots stack_class in
      let invalid = Int.Set.diff used_slots available_slots in
      if not (Int.Set.is_empty invalid)
      then
        fatal "stack slot class %a uses the following invalid slots: %s"
          Stack_class.print stack_class (string_of_set invalid);
      let unused = Int.Set.diff available_slots used_slots in
      if not (Int.Set.is_empty unused)
      then
        fatal "stack slot class %a has the following unused slots: %s"
          Stack_class.print stack_class (string_of_set unused))

let postcondition_liveness : Cfg_with_infos.t -> unit =
 fun cfg_with_infos ->
  postcondition_layout (Cfg_with_infos.cfg_with_layout cfg_with_infos);
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let entry_block = Cfg.get_block_exn cfg cfg.entry_label in
  let live_at_entry_point =
    Cfg_with_infos.liveness_find cfg_with_infos
      (Cfg.first_instruction_id entry_block)
  in
  Reg.Set.iter
    (fun reg ->
      match reg.Reg.loc with
      | Unknown -> assert false (* already tested in `postcondition_layout` *)
      | Reg _ -> ()
      | Stack (Local _) ->
        fatal "`Stack (Local _)`live at entry point: %a" Printreg.reg reg
      | Stack (Incoming _) -> ()
      | Stack (Outgoing _) ->
        fatal "`Stack (Outgoing _)` live at entry point: %a" Printreg.reg reg
      | Stack (Domainstate _) -> ())
    live_at_entry_point.before
