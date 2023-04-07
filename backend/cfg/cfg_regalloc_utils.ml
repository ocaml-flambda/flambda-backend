[@@@ocaml.warning "+a-4-30-40-41-42"]

module Array = ArrayLabels
module List = ListLabels
module DLL = Flambda_backend_utils.Doubly_linked_list

let bool_of_env env_var =
  match Sys.getenv_opt env_var |> Option.map String.lowercase_ascii with
  | Some ("1" | "true" | "on") -> true
  | Some ("0" | "false" | "off") | None -> false
  | Some var ->
    Misc.fatal_errorf
      "the %s variable is \"%s\" but should be one of: \"0\", \"1\", \"true\", \
       \"false\", \"on\", \"off\""
      env_var var

let validator_debug = bool_of_env "CFG_REGALLOC_VALIDATOR_DEBUG"

let fatal_callback = ref (fun () -> ())

let on_fatal ~f = fatal_callback := f

let fatal fmt =
  !fatal_callback ();
  Misc.fatal_errorf fmt

module Instruction = struct
  type id = int

  type t = Cfg.basic Cfg.instruction

  let compare (left : t) (right : t) : int = Int.compare left.id right.id

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module IdSet = MoreLabels.Set.Make (Int)
  module IdMap = MoreLabels.Map.Make (Int)
end

type cfg_infos =
  { arg : Reg.Set.t;
    res : Reg.Set.t;
    max_instruction_id : Instruction.id
  }

let collect_cfg_infos : Cfg_with_layout.t -> cfg_infos =
 fun cfg_with_layout ->
  let arg = ref Reg.Set.empty in
  let res = ref Reg.Set.empty in
  let max_id = ref Int.min_int in
  let add_registers (set : Reg.Set.t ref) (regs : Reg.t array) : unit =
    ArrayLabels.iter regs ~f:(fun reg ->
        match reg.Reg.loc with
        | Unknown -> set := Reg.Set.add reg !set
        | Reg _ | Stack _ -> ())
  in
  let update_max_id (instr : _ Cfg.instruction) : unit =
    max_id := Int.max !max_id instr.id
  in
  Cfg_with_layout.iter_instructions
    cfg_with_layout (* CR xclerc for xclerc: use fold *)
    ~instruction:(fun instr ->
      (instr : Instruction.t).irc_work_list <- Cfg.Unknown_list;
      add_registers arg instr.arg;
      add_registers res instr.res;
      instr.arg <- Array.copy instr.arg;
      instr.res <- Array.copy instr.res;
      update_max_id instr)
    ~terminator:(fun term ->
      term.irc_work_list <- Cfg.Unknown_list;
      add_registers arg term.arg;
      add_registers res term.res;
      term.arg <- Array.copy term.arg;
      term.res <- Array.copy term.res;
      update_max_id term);
  { arg = !arg; res = !res; max_instruction_id = !max_id }

type liveness = Cfg_liveness.Liveness.domain Cfg_dataflow.Instr.Tbl.t

let liveness_analysis : Cfg_with_layout.t -> liveness =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let init = { Cfg_liveness.before = Reg.Set.empty; across = Reg.Set.empty } in
  match
    Cfg_liveness.Liveness.run cfg ~init ~map:Cfg_liveness.Liveness.Instr ()
  with
  | Ok liveness -> liveness
  | Aborted _ -> .
  | Max_iterations_reached ->
    fatal "Unable to compute liveness from CFG for function %s@."
      cfg.Cfg.fun_name

module Move = struct
  type t =
    | Plain
    | Load
    | Store

  let op_of_move = function
    | Plain -> Cfg.Move
    | Load -> Cfg.Reload
    | Store -> Cfg.Spill

  let make_instr :
      t ->
      id:Instruction.id ->
      copy:_ Cfg.instruction ->
      from:Reg.t ->
      to_:Reg.t ->
      Instruction.t =
   fun move ~id ~copy:instr ~from ~to_ ->
    { desc = Op (op_of_move move);
      arg = [| from |];
      res = [| to_ |];
      dbg = instr.dbg;
      fdo = instr.fdo;
      live = instr.live;
      (* note: recomputed anyway *)
      stack_offset = instr.stack_offset;
      id;
      irc_work_list = Unknown_list
    }

  let to_string = function Plain -> "move" | Load -> "load" | Store -> "store"
end

let same_reg_class : Reg.t -> Reg.t -> bool =
 fun reg1 reg2 ->
  Int.equal (Proc.register_class reg1) (Proc.register_class reg2)

let make_temporary :
    same_class_and_base_name_as:Reg.t -> name_prefix:string -> Reg.t =
 fun ~same_class_and_base_name_as:reg ~name_prefix ->
  let new_temp = Reg.create reg.Reg.typ in
  (if not (Reg.anonymous reg)
  then
    let name =
      Reg.Raw_name.to_string reg.Reg.raw_name |> Option.value ~default:"anon"
    in
    let name = name_prefix ^ "-" ^ name in
    new_temp.Reg.raw_name
      <- Reg.Raw_name.create_from_var (Backend_var.create_local name));
  new_temp

let simplify_cfg : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  Profile.record ~accumulate:true "remove-noop-move"
    (fun () ->
      Cfg.iter_blocks cfg ~f:(fun _label block ->
          DLL.filter_left block.body ~f:(fun instr ->
              not (Cfg.is_noop_move instr))))
    ();
  Profile.record ~accumulate:true "eliminate" Eliminate_fallthrough_blocks.run
    cfg_with_layout;
  Profile.record ~accumulate:true "merge" Merge_straightline_blocks.run
    cfg_with_layout;
  Profile.record ~accumulate:true "dead_block"
    Eliminate_dead_code.run_dead_block cfg_with_layout;
  Profile.record ~accumulate:true "terminator" Simplify_terminator.run cfg;
  cfg_with_layout

let precondition : Cfg_with_layout.t -> unit =
 fun cfg_with_layout ->
  (* note: the `live` field is set, because we want to call the `Deadcode` pass
     before `Cfgize`. *)
  let desc_is_neither_spill_or_reload (id : Instruction.id) (desc : Cfg.basic) :
      unit =
    match desc with
    | Op op -> (
      match op with
      | Move -> ()
      | Spill -> fatal "instruction %d is a spill" id
      | Reload -> fatal "instruction %d is a reload" id
      | Const_int _ -> ()
      | Const_float _ -> ()
      | Const_symbol _ -> ()
      | Stackoffset _ -> ()
      | Load _ -> ()
      | Store _ -> ()
      | Intop _ -> ()
      | Intop_imm _ -> ()
      | Intop_atomic _ -> ()
      | Negf -> ()
      | Absf -> ()
      | Addf -> ()
      | Subf -> ()
      | Mulf -> ()
      | Divf -> ()
      | Compf _ -> ()
      | Csel _ -> ()
      | Floatofint -> ()
      | Intoffloat -> ()
      | Valueofint -> ()
      | Intofvalue -> ()
      | Probe_is_enabled _ -> ()
      | Opaque -> ()
      | Begin_region -> ()
      | End_region -> ()
      | Specific op ->
        if Arch.operation_can_raise op
        then
          fatal
            "architecture specific instruction %d that can raise but isn't a \
             terminator"
            id
      | Name_for_debugger _ -> ())
    | Reloadretaddr | Pushtrap _ | Poptrap | Prologue -> ()
  in
  let register_must_not_be_on_stack (id : Instruction.id) (reg : Reg.t) : unit =
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
      fatal "instruction %d has a register with a stack location" id
  in
  let registers_must_not_be_on_stack (id : Instruction.id) (regs : Reg.t array)
      : unit =
    ArrayLabels.iter regs ~f:(register_must_not_be_on_stack id)
  in
  (* CR xclerc for xclerc: the check below should not be in this function, since
     it is IRC-specific *)
  let register_must_be_on_unknown_list (id : Instruction.id) (reg : Reg.t) :
      unit =
    match reg.Reg.irc_work_list with
    | Unknown_list -> ()
    | Precolored -> ()
    | Initial | Simplify | Freeze | Spill | Spilled | Coalesced | Colored
    | Select_stack ->
      fatal "instruction %d has a register (%a) already in a work list (%S)" id
        Printmach.reg reg
        (Reg.string_of_irc_work_list reg.Reg.irc_work_list)
  in
  let register_must_be_on_unknown_list (id : Instruction.id)
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
  Array.iteri fun_num_stack_slots ~f:(fun reg_class num_slots ->
      if num_slots <> 0
      then fatal "register class %d has %d slots(s)" reg_class num_slots)

let postcondition : Cfg_with_layout.t -> allow_stack_operands:bool -> unit =
 fun cfg_with_layout ~allow_stack_operands ->
  let max_stack_slots = Array.init Proc.num_register_classes ~f:(fun _ -> -1) in
  let register_must_not_be_unknown (id : Instruction.id) (reg : Reg.t) : unit =
    match reg.Reg.loc with
    | Reg _ -> ()
    | Stack (Incoming _ | Outgoing _ | Domainstate _) -> ()
    | Stack (Local slot) ->
      let reg_class = Proc.register_class reg in
      max_stack_slots.(reg_class) <- max max_stack_slots.(reg_class) slot
    | Unknown ->
      fatal "instruction %d has a register with an unknown location" id
  in
  let registers_must_not_be_unknown (id : Instruction.id) (regs : Reg.t array) :
      unit =
    ArrayLabels.iter regs ~f:(register_must_not_be_unknown id)
  in
  let num_stack_locals (regs : Reg.t array) : int =
    Array.fold_left regs ~init:0 ~f:(fun acc reg ->
        match reg.Reg.loc with
        | Unknown | Reg _ | Stack (Incoming _ | Outgoing _ | Domainstate _) ->
          acc
        | Stack (Local _) -> succ acc)
  in
  let arch_constraints (id : Instruction.id) (desc : Cfg.basic)
      (arg : Reg.t array) (res : Reg.t array) : unit =
    match Config.architecture with
    (* CR xclerc for xclerc: what about cross-compilation? *)
    | "amd64" | "arm64" -> (
      let num_locals = num_stack_locals arg + num_stack_locals res in
      match desc with
      | Op (Spill | Reload) ->
        (* CR xclerc for xclerc: should check arg/res according to spill/reload,
           rather than the total number. *)
        if num_locals > 1
        then
          fatal "instruction %d is a move and refers to %d spilling slots" id
            num_locals
      | _ ->
        if (not allow_stack_operands) && num_locals > 0
        then
          fatal "instruction %d is not a move but refers to a spilling slot" id)
    | arch -> fatal "unsupported architecture %S" arch
  in
  let register_classes_must_be_consistent (id : Instruction.id) (reg : Reg.t) :
      unit =
    match reg.Reg.loc with
    | Reg phys_reg ->
      let phys_reg = Proc.phys_reg phys_reg in
      if not (same_reg_class reg phys_reg)
      then
        fatal
          "instruction %d assigned %a to %a but they are in different classes"
          id Printmach.reg reg Printmach.reg phys_reg
    | Stack _ | Unknown -> ()
  in
  let register_classes_must_be_consistent (id : Instruction.id)
      (regs : Reg.t array) : unit =
    ArrayLabels.iter regs ~f:(register_classes_must_be_consistent id)
  in
  Cfg_with_layout.iter_instructions cfg_with_layout
    ~instruction:(fun instr ->
      let id = instr.id in
      registers_must_not_be_unknown id instr.arg;
      registers_must_not_be_unknown id instr.res;
      arch_constraints id instr.desc instr.arg instr.res;
      register_classes_must_be_consistent id instr.arg;
      register_classes_must_be_consistent id instr.res)
    ~terminator:(fun term ->
      let id = term.id in
      registers_must_not_be_unknown id term.arg;
      registers_must_not_be_unknown id term.res;
      register_classes_must_be_consistent id term.arg;
      register_classes_must_be_consistent id term.res);
  let fun_num_stack_slots =
    (Cfg_with_layout.cfg cfg_with_layout).fun_num_stack_slots
  in
  let reg_class = ref 0 in
  Array.iter2 max_stack_slots fun_num_stack_slots ~f:(fun max_slot num_slots ->
      (* CR-soon xclerc for xclerc: make the condition stricter if max_slot >=
         num_slots *)
      if succ max_slot <> num_slots
      then
        fatal
          "register class %d has a max slot of %d, but the number of slots is \
           %d"
          !reg_class max_slot num_slots;
      incr reg_class)

let save_cfg : string -> Cfg_with_layout.t -> unit =
 fun str cfg_with_layout ->
  Cfg_with_layout.save_as_dot cfg_with_layout ~show_instr:true ~show_exn:true
    ~annotate_block:(fun label ->
      let block =
        Cfg.get_block_exn (Cfg_with_layout.cfg cfg_with_layout) label
      in
      Printf.sprintf "label:%d stack_offset:%d" label block.stack_offset)
    ~annotate_succ:(Printf.sprintf "%d->%d") str

let update_stack_slots : Cfg_with_layout.t -> num_stack_slots:int array -> unit
    =
 fun cfg_with_layout ~num_stack_slots ->
  let fun_num_stack_slots =
    (Cfg_with_layout.cfg cfg_with_layout).fun_num_stack_slots
  in
  for reg_class = 0 to pred Proc.num_register_classes do
    fun_num_stack_slots.(reg_class) <- num_stack_slots.(reg_class)
  done

let remove_prologue_if_not_required : Cfg_with_layout.t -> unit =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let prologue_required =
    Proc.prologue_required ~fun_contains_calls:cfg.fun_contains_calls
      ~fun_num_stack_slots:cfg.fun_num_stack_slots
  in
  if not prologue_required
  then
    (* note: `Cfgize` has put the prologue in the entry block *)
    let block = Cfg.get_block_exn cfg cfg.entry_label in
    DLL.filter_left block.body ~f:(fun instr ->
        match instr.Cfg.desc with Cfg.Prologue -> false | _ -> true)

let update_live_fields : Cfg_with_layout.t -> liveness -> unit =
 fun cfg_with_layout liveness ->
  (* CR xclerc for xclerc: partial duplicate of
     `Asmgen.recompute_liveness_on_cfg` *)
  let set_liveness (instr : _ Cfg.instruction) =
    match Cfg_dataflow.Instr.Tbl.find_opt liveness instr.id with
    | None -> fatal "Missing liveness information for instruction %d" instr.id
    | Some { Cfg_liveness.before = _; across } -> instr.live <- across
  in
  Cfg.iter_blocks (Cfg_with_layout.cfg cfg_with_layout) ~f:(fun _label block ->
      DLL.iter block.body ~f:set_liveness;
      set_liveness block.terminator)

(* CR-soon xclerc for xclerc: consider adding an overflow check. *)
let pow10 n =
  let res = ref 1 in
  for _ = 1 to n do
    res := !res * 10
  done;
  !res

let update_spill_cost : Cfg_with_layout.t -> flat:bool -> unit -> unit =
 fun cfg_with_layout ~flat () ->
  List.iter (Reg.all_registers ()) ~f:(fun reg -> reg.Reg.spill_cost <- 0);
  let update_reg (cost : int) (reg : Reg.t) : unit =
    (* CR-soon xclerc for xclerc: consider adding an overflow check. *)
    reg.Reg.spill_cost <- reg.Reg.spill_cost + cost
  in
  let update_array (cost : int) (regs : Reg.t array) : unit =
    Array.iter regs ~f:(fun reg -> update_reg cost reg)
  in
  let update_instr (cost : int) (instr : _ Cfg.instruction) : unit =
    update_array cost instr.arg;
    update_array cost instr.res
  in
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let loops_depths : Cfg_loop_infos.loop_depths =
    if flat then Label.Map.empty else (Cfg_loop_infos.build cfg).loop_depths
  in
  Cfg.iter_blocks cfg ~f:(fun label block ->
      let cost =
        match Label.Map.find_opt label loops_depths with
        | None ->
          assert flat;
          1
        | Some depth -> pow10 depth
      in
      DLL.iter ~f:(fun instr -> update_instr cost instr) block.body;
      (* Ignore probes *)
      match block.terminator.desc with
      | Prim { op = Probe _; _ } -> ()
      | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
      | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
      | Tailcall_func _ | Call_no_return _ | Call _ | Prim _
      | Specific_can_raise _ | Poll_and_jump _ ->
        update_instr cost block.terminator)

let is_spilled reg = reg.Reg.irc_work_list = Reg.Spilled

let check_length str arr expected =
  let actual = Array.length arr in
  if expected <> actual
  then
    fatal "the length of %s was expected to be %d but is actually %d" str
      expected actual

let check_lengths :
    type a. of_arg:int -> of_res:int -> a Cfg.instruction -> unit =
 fun ~of_arg ~of_res instr ->
  check_length "arg" instr.arg of_arg;
  check_length "res" instr.res of_res

let check_same str1 reg1 str2 reg2 =
  if not (Reg.same reg1 reg2)
  then
    fatal "%s and %s were expected to be the same but they differ (%a vs %a)"
      str1 str2 Printmach.reg reg1 Printmach.reg reg2

type stack_operands_rewrite =
  | All_spilled_registers_rewritten
  | May_still_have_spilled_registers

type spilled_map = Reg.t Reg.Tbl.t

let use_stack_operand (map : Reg.t Reg.Tbl.t) (regs : Reg.t array) (index : int)
    : unit =
  let reg = regs.(index) in
  match Reg.Tbl.find_opt map reg with
  | None -> fatal "register %a is missing from the map" Printmach.reg reg
  | Some spilled_reg -> regs.(index) <- spilled_reg

let may_use_stack_operands_array : Reg.t Reg.Tbl.t -> Reg.t array -> unit =
 fun map regs ->
  Array.iteri regs ~f:(fun i reg ->
      if is_spilled reg then use_stack_operand map regs i)

let may_use_stack_operands_everywhere :
    type a. Reg.t Reg.Tbl.t -> a Cfg.instruction -> stack_operands_rewrite =
 fun map instr ->
  may_use_stack_operands_array map instr.arg;
  may_use_stack_operands_array map instr.res;
  All_spilled_registers_rewritten

let insert_block :
    Cfg_with_layout.t ->
    Cfg.basic_instruction_list ->
    after:Cfg.basic_block ->
    next_instruction_id:(unit -> Instruction.id) ->
    unit =
 fun cfg_with_layout body ~after:predecessor_block ~next_instruction_id ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let successors =
    Cfg.successor_labels ~normal:true ~exn:false predecessor_block
  in
  if Label.Set.cardinal successors = 0
  then
    Misc.fatal_errorf
      "Cannot insert a block after block %a: it has no successors" Label.print
      predecessor_block.start;
  let last_insn =
    match DLL.last body with
    | None -> Misc.fatal_error "Inserting an empty block"
    | Some i -> i
  in
  let copy (i : Cfg.basic Cfg.instruction) : Cfg.basic Cfg.instruction =
    { i with id = next_instruction_id () }
  in
  (* copy body if there is more than one successor *)
  let first = ref true in
  let get_body () =
    if !first
    then (
      first := false;
      body)
    else
      let new_body = DLL.make_empty () in
      DLL.iter body ~f:(fun instr -> DLL.add_end new_body (copy instr));
      new_body
  in
  Label.Set.iter
    (fun successor_label ->
      let successor_block = Cfg.get_block_exn cfg successor_label in
      let start = Cmm.new_label () in
      let block : Cfg.basic_block =
        { start;
          body = get_body ();
          terminator =
            { (* The [successor_block] is the only successor. *)
              desc = Cfg.Always successor_label;
              arg = [||];
              res = [||];
              dbg = last_insn.dbg;
              fdo = last_insn.fdo;
              live = last_insn.live;
              stack_offset = last_insn.stack_offset;
              id = next_instruction_id ();
              irc_work_list = Unknown_list
            };
          (* The [predecessor_block] is the only predecessor. *)
          predecessors = Label.Set.singleton predecessor_block.start;
          stack_offset = predecessor_block.terminator.stack_offset;
          exn = None;
          can_raise = false;
          is_trap_handler = false;
          dead = predecessor_block.dead
        }
      in
      Cfg_with_layout.add_block cfg_with_layout block
        ~after:predecessor_block.start;
      (* Change the labels for the terminator in [predecessor_block]. *)
      Cfg.replace_successor_labels cfg ~normal:true ~exn:false predecessor_block
        ~f:(fun old_label ->
          if Label.equal old_label successor_label then start else old_label);
      (* Update predecessors for the [successor_block]. *)
      successor_block.predecessors
        <- successor_block.predecessors
           |> Label.Set.remove predecessor_block.start
           |> Label.Set.add start)
    successors
