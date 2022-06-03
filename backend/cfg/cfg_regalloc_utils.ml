[@@@ocaml.warning "+a-4-30-40-41-42"]

module Array = ArrayLabels
module List = ListLabels

let run_gc_for_benchmarks =
  match Sys.getenv_opt "REGALLOC_STATS" with None -> false | Some _ -> true

let gc_for_benchmarks () = if run_gc_for_benchmarks then Gc.full_major ()

external time_include_children : bool -> float
  = "caml_sys_time_include_children"

let cpu_time () = time_include_children true

let fatal_callback = ref (fun () -> ())

let on_fatal ~f = fatal_callback := f

let fatal fmt =
  !fatal_callback ();
  Misc.fatal_errorf fmt

module Stats = struct
  type line =
    { (* computed *)
      mutable is_entry : bool;
      (* from `Asmgen` *)
      mutable allocator : string;
      mutable total_time : float;
      (* from IRC *)
      mutable before_main : float;
      mutable main : float;
      mutable after_main : float;
      mutable build : float;
      mutable loop : float;
      mutable simplify : float;
      mutable coalesce : float;
      mutable freeze : float;
      mutable select_spill : float;
      mutable assign : float;
      mutable update_live : float;
      mutable num_rounds : int;
      mutable liveness : float;
      mutable num_regs : int;
      (* from `Emit` *)
      mutable num_instrs : int;
      mutable num_moves : int;
      mutable num_spills : int;
      mutable num_reloads : int
    }

  type 'a column =
    { name : string;
      default : 'a;
      update : line -> 'a -> unit;
      to_string : line -> string
    }

  let is_entry =
    { name = "is_entry";
      default = false;
      update = (fun _ _ -> assert false);
      to_string = (fun line -> if line.is_entry then "1" else "0")
    }

  let allocator =
    { name = "allocator";
      default = "";
      update = (fun line value -> line.allocator <- value);
      to_string = (fun line -> line.allocator)
    }

  let total_time =
    { name = "total_time";
      default = 0.;
      update = (fun line value -> line.total_time <- value);
      to_string = (fun line -> Float.to_string line.total_time)
    }

  let before_main =
    { name = "before_main";
      default = 0.;
      update = (fun line value -> line.before_main <- value);
      to_string = (fun line -> Float.to_string line.before_main)
    }

  let main =
    { name = "main";
      default = 0.;
      update = (fun line value -> line.main <- value);
      to_string = (fun line -> Float.to_string line.main)
    }

  let after_main =
    { name = "after_main";
      default = 0.;
      update = (fun line value -> line.after_main <- value);
      to_string = (fun line -> Float.to_string line.after_main)
    }

  let build =
    { name = "build";
      default = 0.;
      update = (fun line value -> line.build <- line.build +. value);
      to_string = (fun line -> Float.to_string line.build)
    }

  let loop =
    { name = "loop";
      default = 0.;
      update = (fun line value -> line.loop <- line.loop +. value);
      to_string = (fun line -> Float.to_string line.loop)
    }

  let simplify =
    { name = "simplify";
      default = 0.;
      update = (fun line value -> line.simplify <- line.simplify +. value);
      to_string = (fun line -> Float.to_string line.simplify)
    }

  let coalesce =
    { name = "coalesce";
      default = 0.;
      update = (fun line value -> line.coalesce <- line.coalesce +. value);
      to_string = (fun line -> Float.to_string line.coalesce)
    }

  let freeze =
    { name = "freeze";
      default = 0.;
      update = (fun line value -> line.freeze <- line.freeze +. value);
      to_string = (fun line -> Float.to_string line.freeze)
    }

  let select_spill =
    { name = "select_spill";
      default = 0.;
      update =
        (fun line value -> line.select_spill <- line.select_spill +. value);
      to_string = (fun line -> Float.to_string line.select_spill)
    }

  let assign =
    { name = "assign";
      default = 0.;
      update = (fun line value -> line.assign <- value);
      to_string = (fun line -> Float.to_string line.assign)
    }

  let update_live =
    { name = "update_live";
      default = 0.;
      update = (fun line value -> line.update_live <- value);
      to_string = (fun line -> Float.to_string line.update_live)
    }

  let num_rounds =
    { name = "num_rounds";
      default = 0;
      update = (fun line value -> line.num_rounds <- value);
      to_string = (fun line -> Int.to_string line.num_rounds)
    }

  let liveness =
    { name = "liveness";
      default = 0.;
      update = (fun line value -> line.liveness <- line.liveness +. value);
      to_string = (fun line -> Float.to_string line.liveness)
    }

  let num_regs =
    { name = "num_regs";
      default = 0;
      update = (fun line value -> line.num_regs <- value);
      to_string = (fun line -> Int.to_string line.num_regs)
    }

  let num_instrs =
    { name = "num_instrs";
      default = 0;
      update = (fun line value -> line.num_instrs <- value);
      to_string = (fun line -> Int.to_string line.num_instrs)
    }

  let num_moves =
    { name = "num_moves";
      default = 0;
      update = (fun line value -> line.num_moves <- value);
      to_string = (fun line -> Int.to_string line.num_moves)
    }

  let num_spills =
    { name = "num_spills";
      default = 0;
      update = (fun line value -> line.num_spills <- value);
      to_string = (fun line -> Int.to_string line.num_spills)
    }

  let num_reloads =
    { name = "num_reloads";
      default = 0;
      update = (fun line value -> line.num_reloads <- value);
      to_string = (fun line -> Int.to_string line.num_reloads)
    }

  type any_column = Column : 'a column -> any_column

  let columns =
    [ Column is_entry;
      Column allocator;
      Column total_time;
      Column before_main;
      Column main;
      Column after_main;
      Column build;
      Column loop;
      Column simplify;
      Column coalesce;
      Column freeze;
      Column select_spill;
      Column assign;
      Column update_live;
      Column num_rounds;
      Column liveness;
      Column num_regs;
      Column num_instrs;
      Column num_moves;
      Column num_spills;
      Column num_reloads ]

  let create name =
    let len = String.length name in
    let is_entry = len > 7 && String.sub name (len - 7) 7 = "__entry" in
    { is_entry;
      allocator = allocator.default;
      total_time = total_time.default;
      before_main = before_main.default;
      main = main.default;
      after_main = after_main.default;
      build = build.default;
      loop = loop.default;
      simplify = simplify.default;
      coalesce = coalesce.default;
      freeze = freeze.default;
      select_spill = select_spill.default;
      assign = assign.default;
      update_live = update_live.default;
      num_rounds = num_rounds.default;
      liveness = liveness.default;
      num_regs = num_regs.default;
      num_instrs = num_instrs.default;
      num_moves = num_moves.default;
      num_spills = num_spills.default;
      num_reloads = num_reloads.default
    }

  let lines : (string, line) Hashtbl.t = Hashtbl.create 128

  let find_or_create name =
    match Hashtbl.find_opt lines name with
    | Some line -> line
    | None ->
      let line = create name in
      Hashtbl.replace lines name line;
      line

  let update_fun_name name column value =
    let line = find_or_create name in
    column.update line value

  let update_cfg_with_layout cfg_with_layout column value =
    let name = Cfg.fun_name (Cfg_with_layout.cfg cfg_with_layout) in
    update_fun_name name column value

  let () =
    at_exit (fun () ->
        match Sys.getenv_opt "REGALLOC_STATS" with
        | None -> ()
        | Some directory ->
          let args = String.concat "|" (Array.to_list Sys.argv) in
          let digest = Digest.string args in
          let chan =
            open_out (Filename.concat directory (Digest.to_hex digest))
          in
          output_string chan args;
          output_string chan "\n";
          let col_names =
            List.map columns ~f:(function Column { name; _ } -> name)
          in
          output_string chan (String.concat "," ("function" :: col_names));
          output_string chan "\n";
          Hashtbl.iter
            (fun name line ->
              let values =
                List.map columns ~f:(function Column { to_string; _ } ->
                    to_string line)
              in
              output_string chan (String.concat "," (name :: values));
              output_string chan "\n")
            lines;
          close_out_noerr chan)
end

module Instruction = struct
  type id = int

  type t = Cfg.basic Cfg.instruction

  module Set = MoreLabels.Set.Make (struct
    type nonrec t = t

    let compare (left : t) (right : t) : int = Int.compare left.id right.id
  end)

  module IdSet = MoreLabels.Set.Make (Int)
  module IdMap = MoreLabels.Map.Make (Int)
end

(* CR xclerc for xclerc: in destroyed_at_xyz, lift the constants? *)

(* CR xclerc for xclerc: reimplement destroyed_at_xyz here, to avoid the call to
   Prod.destroyed_at_tuv? *)

let destroyed_at_basic : Cfg.basic -> Reg.t array =
 fun basic ->
  let at_oper = Proc.destroyed_at_oper in
  let default = at_oper (Iop Imove) in
  match basic with
  | Op op ->
    at_oper
      begin
        match op with
        | Move -> Iop Imove
        | Spill -> Iop Ispill
        | Reload -> Iop Ireload
        | Const_int x -> Iop (Iconst_int x)
        | Const_float x -> Iop (Iconst_float x)
        | Const_symbol x -> Iop (Iconst_symbol x)
        | Stackoffset x -> Iop (Istackoffset x)
        | Load (x, y, z) -> Iop (Iload (x, y, z))
        | Store (x, y, z) -> Iop (Istore (x, y, z))
        | Intop x -> Iop (Iintop x)
        | Intop_imm (x, y) -> Iop (Iintop_imm (x, y))
        | Negf -> Iop Inegf
        | Absf -> Iop Iabsf
        | Addf -> Iop Iaddf
        | Subf -> Iop Isubf
        | Mulf -> Iop Imulf
        | Divf -> Iop Idivf
        | Compf x -> Iop (Icompf x)
        | Floatofint -> Iop Ifloatofint
        | Intoffloat -> Iop Iintoffloat
        | Probe { name; handler_code_sym } ->
          Iop (Iprobe { name; handler_code_sym })
        | Probe_is_enabled { name } -> Iop (Iprobe_is_enabled { name })
        | Opaque -> Iop Iopaque
        | Begin_region -> Iop Ibeginregion
        | End_region -> Iop Iendregion
        | Specific x -> Iop (Ispecific x)
        | Name_for_debugger
            { ident; which_parameter; provenance; is_assignment } ->
          Iop
            (Iname_for_debugger
               { ident; which_parameter; provenance; is_assignment })
      end
  | Call c -> begin
    match c with
    | P (External { func_symbol; alloc; ty_res; ty_args }) ->
      let func = func_symbol in
      let returns = true in
      at_oper (Iop (Iextcall { func; ty_res; ty_args; alloc; returns }))
    | P (Alloc { bytes; dbginfo; mode }) ->
      at_oper (Iop (Ialloc { bytes; dbginfo; mode }))
    | P (Checkbound { immediate }) -> begin
      match immediate with
      | None -> at_oper (Iop (Iintop Icheckbound))
      | Some x -> at_oper (Iop (Iintop_imm (Icheckbound, x)))
    end
    | F Indirect -> at_oper (Iop Icall_ind)
    | F (Direct { func_symbol }) ->
      let func = func_symbol in
      at_oper (Iop (Icall_imm { func }))
  end
  | Reloadretaddr -> Proc.destroyed_at_reloadretaddr
  | Pushtrap _ ->
    [| Proc.phys_reg 11 |] (* CR xclerc for xclerc: amd64-specific *)
  | Poptrap -> default
  | Prologue -> default

let destroyed_at_terminator : Cfg.terminator -> Reg.t array =
 fun term ->
  let at_oper = Proc.destroyed_at_oper in
  let default = at_oper (Iop Imove) in
  match term with
  | Never -> assert false
  | Always _ -> default
  | Parity_test _ -> default
  | Truth_test _ -> default
  | Float_test _ -> default
  | Int_test _ -> default
  | Switch _ -> at_oper (Mach.Iswitch ([||], [||]))
  | Return -> at_oper (Mach.Ireturn [])
  | Raise x -> at_oper (Mach.Iraise x)
  | Tailcall x -> begin
    match x with
    | Self { destination } ->
      let func =
        ignore destination;
        "dummy"
      in
      at_oper (Mach.Iop (Itailcall_imm { func }))
    | Func Indirect -> at_oper (Mach.Iop Itailcall_ind)
    | Func (Direct { func_symbol }) ->
      let func = func_symbol in
      at_oper (Mach.Iop (Itailcall_imm { func }))
  end
  | Call_no_return { func_symbol; alloc; ty_res; ty_args } ->
    let func = func_symbol in
    let returns = false in
    at_oper (Mach.Iop (Iextcall { func; ty_res; ty_args; alloc; returns }))

let iter_instructions :
    Cfg_with_layout.t ->
    instruction:(Cfg.basic Cfg.instruction -> unit) ->
    terminator:(Cfg.terminator Cfg.instruction -> unit) ->
    unit =
 fun cfg_with_layout ~instruction ~terminator ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  Cfg.iter_blocks cfg ~f:(fun _label block ->
      List.iter ~f:instruction block.body;
      terminator block.terminator)

let fold_instructions :
    type a.
    Cfg_with_layout.t ->
    instruction:(a -> Cfg.basic Cfg.instruction -> a) ->
    terminator:(a -> Cfg.terminator Cfg.instruction -> a) ->
    init:a ->
    a =
 fun cfg_with_layout ~instruction ~terminator ~init ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  Cfg.fold_blocks cfg ~init ~f:(fun _label block acc ->
      let acc = List.fold_left ~init:acc ~f:instruction block.body in
      let acc = terminator acc block.terminator in
      acc)

let[@inline] int_max (left : int) (right : int) = Stdlib.max left right

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
    max_id := int_max !max_id instr.id
  in
  iter_instructions cfg_with_layout (* CR xclerc for xclerc: use fold *)
    ~instruction:(fun instr ->
      (instr : Instruction.t).irc_work_list <- Cfg.Unknown_list;
      add_registers arg instr.arg;
      add_registers res instr.res;
      update_max_id instr)
    ~terminator:(fun term ->
      term.irc_work_list <- Cfg.Unknown_list;
      add_registers arg term.arg;
      add_registers res term.res;
      update_max_id term);
  { arg = !arg; res = !res; max_instruction_id = !max_id }

type liveness = Cfg_liveness.Liveness.domain Cfg_dataflow.Instr.Tbl.t

let liveness_analysis : Cfg_with_layout.t -> liveness =
 fun cfg_with_layout ->
  let before = cpu_time () in
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let init = { Cfg_liveness.before = Reg.Set.empty; across = Reg.Set.empty } in
  match
    Cfg_liveness.Liveness.run cfg ~init ~map:Cfg_liveness.Liveness.Instr ()
  with
  | Result.Ok liveness ->
    let after = cpu_time () in
    Stats.update_cfg_with_layout cfg_with_layout Stats.liveness (after -. before);
    liveness
  | Result.Error _ ->
    fatal "Unable to compute liveness from CFG for function %s@."
      cfg.Cfg.fun_name

module Move = struct
  type t =
    | Plain
    | Fetch
    | Store

  let op_of_move = function
    | Plain -> Cfg.Move
    | Fetch -> Cfg.Reload
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

  let to_string = function
    | Plain -> "move"
    | Fetch -> "fetch"
    | Store -> "store"
end

let same_reg : Reg.t -> Reg.t -> bool =
 fun reg1 reg2 -> Int.equal reg1.stamp reg2.stamp

let same_reg_class : Reg.t -> Reg.t -> bool =
 fun reg1 reg2 -> Int.equal reg1.clas reg2.clas

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
  Cfg.iter_blocks cfg ~f:(fun _label block ->
      block.body
        <- List.filter block.body ~f:(fun instr -> not (Cfg.is_noop_move instr)));
  Eliminate_fallthrough_blocks.run cfg_with_layout;
  Merge_straightline_blocks.run cfg_with_layout;
  Eliminate_dead_code.run_dead_block cfg_with_layout;
  Simplify_terminator.run cfg;
  cfg_with_layout

let precondition : Cfg_with_layout.t -> unit =
 fun cfg_with_layout ->
  (* note: the `live` field is set, because we want to call the `Deadcode` pass
     before `Cfgize`. *)
  let desc_is_neither_spill_or_reload (id : Instruction.id) (desc : Cfg.basic) :
      unit =
    match desc with
    | Op op -> begin
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
      | Negf -> ()
      | Absf -> ()
      | Addf -> ()
      | Subf -> ()
      | Mulf -> ()
      | Divf -> ()
      | Compf _ -> ()
      | Floatofint -> ()
      | Intoffloat -> ()
      | Probe _ -> ()
      | Probe_is_enabled _ -> ()
      | Opaque -> ()
      | Begin_region -> ()
      | End_region -> ()
      | Specific _ -> ()
      | Name_for_debugger _ -> ()
    end
    | Call _ | Reloadretaddr | Pushtrap _ | Poptrap | Prologue -> ()
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
  iter_instructions cfg_with_layout
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

let postcondition : Cfg_with_layout.t -> unit =
 fun cfg_with_layout ->
  let max_stack_slots = Array.init Proc.num_register_classes ~f:(fun _ -> -1) in
  let register_must_not_be_unknown (id : Instruction.id) (reg : Reg.t) : unit =
    match reg.Reg.loc with
    | Reg _ -> ()
    | Stack (Incoming _ | Outgoing _ | Domainstate _) -> ()
    | Stack (Local slot) ->
      let reg_class = reg.Reg.clas in
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
        if num_locals > 0
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
  iter_instructions cfg_with_layout
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
    (* note: `Cfize` has put the prologue in the entry block *)
    let block = Cfg.get_block_exn cfg cfg.entry_label in
    block.body
      <- List.filter block.body ~f:(fun instr ->
             match instr.Cfg.desc with Cfg.Prologue -> false | _ -> true)

let update_live_fields : Cfg_with_layout.t -> liveness -> unit =
 fun cfg_with_layout liveness ->
  (* CR xclerc for xclerc: partial duplicate of
     `Asmgen.recompute_liveness_on_cfg` *)
  let before = cpu_time () in
  let with_liveness (instr : _ Cfg.instruction) =
    match Cfg_dataflow.Instr.Tbl.find_opt liveness instr.id with
    | None -> fatal "Missing liveness information for instruction %d" instr.id
    | Some { Cfg_liveness.before = _; across } -> Cfg.set_live instr across
  in
  Cfg.iter_blocks (Cfg_with_layout.cfg cfg_with_layout) ~f:(fun _label block ->
      block.body <- ListLabels.map block.body ~f:with_liveness;
      block.terminator <- with_liveness block.terminator);
  let after = cpu_time () in
  Stats.update_cfg_with_layout cfg_with_layout Stats.update_live
    (after -. before)

let update_spill_cost : Cfg_with_layout.t -> unit =
 fun cfg_with_layout ->
  List.iter (Reg.all_registers ()) ~f:(fun reg -> reg.Reg.spill_cost <- 0);
  let update_reg (reg : Reg.t) : unit =
    reg.Reg.spill_cost <- reg.Reg.spill_cost + 1
  in
  let update_array (regs : Reg.t array) : unit =
    Array.iter regs ~f:update_reg
  in
  let update_instr (instr : _ Cfg.instruction) : unit =
    update_array instr.arg;
    update_array instr.res
  in
  iter_instructions cfg_with_layout ~instruction:update_instr
    ~terminator:update_instr
