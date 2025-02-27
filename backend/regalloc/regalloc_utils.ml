[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
module Array = ArrayLabels
module List = ListLabels
module DLL = Flambda_backend_utils.Doubly_linked_list
module Substitution = Regalloc_substitution

let fatal_callback = ref (fun () -> ())

let on_fatal ~f = fatal_callback := f

let fatal fmt =
  !fatal_callback ();
  Misc.fatal_errorf fmt

let find_param_value param_name =
  !Flambda_backend_flags.regalloc_params
  |> List.rev
  |> List.find_map ~f:(fun param ->
         match String.split_on_char ':' param with
         | [] -> None
         | name :: rest ->
           if String.equal name param_name
           then Some (String.concat ":" rest)
           else None)

let bool_of_param ?guard ?(default = false) param_name =
  lazy
    (let res =
       match
         find_param_value param_name |> Option.map String.lowercase_ascii
       with
       | None -> default
       | Some ("1" | "true" | "on") -> true
       | Some ("0" | "false" | "off") -> false
       | Some value ->
         Misc.fatal_errorf
           "the %s variable is %S but should be one of: \"0\", \"1\", \
            \"true\", \"false\", \"on\", \"off\""
           param_name value
     in
     (if res
     then
       match guard with
       | None -> ()
       | Some (guard_value, guard_name) ->
         if not guard_value
         then fatal "%s is set but %s is not" param_name guard_name);
     res)

let debug = false

let invariants : bool Lazy.t =
  bool_of_param ~guard:(debug, "debug") "INVARIANTS"

let validator_debug = bool_of_param "VALIDATOR_DEBUG"

let block_temporaries = bool_of_param "BLOCK_TEMPORARIES"

let verbose : bool Lazy.t = bool_of_param "VERBOSE"

type liveness = Cfg_with_infos.liveness

let make_indent n = String.make (2 * n) ' '

(* CR-soon xclerc for xclerc: as noted by spiessimon it would be more robust to
   define a function like `log_with_increased_level : f:(unit -> 'a)` that would
   automatically call `indent` / `dedent`, and properly handle exceptions.
   However, there is a concern it could be costly (in particular note that
   currently all calls to log functions are expected to be under `if debug` to
   be sure no price is paid when debugging is disabled). *)
type log_function =
  { indent : unit -> unit;
    dedent : unit -> unit;
    reset_indentation : unit -> unit;
    log : 'a. ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a;
    enabled : bool
  }

let make_log_function : label:string -> log_function =
 fun ~label ->
  let enabled = Lazy.force verbose in
  let indent_level = ref 0 in
  let indent () = incr indent_level in
  let dedent () = decr indent_level in
  let reset_indentation () = indent_level := 0 in
  let log =
    if enabled
    then
      fun ?no_eol fmt ->
      Format.eprintf
        ("[%s] %s" ^^ fmt ^^ match no_eol with None -> "\n%!" | Some () -> "")
        label
        (make_indent !indent_level)
    else fun ?no_eol:_ fmt -> Format.(ifprintf err_formatter) fmt
  in
  { indent; dedent; reset_indentation; log; enabled }

  type id = InstructionId.t

  type t = Cfg.basic Cfg.instruction

  let dummy =
    { Cfg.desc = Cfg.Prologue;
      arg = [||];
      res = [||];
      dbg = Debuginfo.none;
      fdo = Fdo_info.none;
      live = Reg.Set.empty;
      stack_offset = -1;
      id = InstructionId.none;
      irc_work_list = Unknown_list;
      ls_order = -1;
      available_before = None;
      available_across = None
    }

  let compare (left : t) (right : t) : int =
    InstructionId.compare left.id right.id

  module Set = Set.Make (struct
    type nonrec t = t

    let compare = compare
  end)

  module IdSet = MoreLabels.Set.Make (InstructionId)
  module IdMap = MoreLabels.Map.Make (InstructionId)
end

let first_instruction_id (block : Cfg.basic_block) : InstructionId.t =
  match DLL.hd block.body with
  | None -> block.terminator.id
  | Some instr -> instr.id

type cfg_infos =
  { arg : Reg.Set.t;
    res : Reg.Set.t;
    max_instruction_id : InstructionId.t
  }

let collect_cfg_infos : Cfg_with_layout.t -> cfg_infos =
 fun cfg_with_layout ->
  let arg = ref Reg.Set.empty in
  let res = ref Reg.Set.empty in
  let max_id = ref InstructionId.none in
  let add_registers (set : Reg.Set.t ref) (regs : Reg.t array) : unit =
    ArrayLabels.iter regs ~f:(fun reg ->
        match reg.Reg.loc with
        | Unknown -> set := Reg.Set.add reg !set
        | Reg _ | Stack _ -> ())
  in
  let update_max_id (instr : _ Cfg.instruction) : unit =
    max_id := InstructionId.max !max_id instr.id
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

let log_instruction_suffix (instr : _ Cfg.instruction) (liveness : liveness) :
    unit =
  let live =
    match InstructionId.Tbl.find_opt liveness instr.id with
    | None -> Reg.Set.empty
    | Some { before = _; across } -> across
  in
  let live = live |> Reg.Set.elements |> Array.of_list in
  if Array.length instr.arg > 0
  then Format.eprintf " arg:%a" Printreg.regs instr.arg;
  if Array.length instr.res > 0
  then Format.eprintf " res:%a" Printreg.regs instr.res;
  if Array.length live > 0 then Format.eprintf " live:%a" Printreg.regs live;
  Format.eprintf "\n%!"

let make_log_body_and_terminator :
    log_function ->
    instr_prefix:(Cfg.basic Cfg.instruction -> string) ->
    term_prefix:(Cfg.terminator Cfg.instruction -> string) ->
    Cfg.basic_instruction_list ->
    Cfg.terminator Cfg.instruction ->
    liveness ->
    unit =
 fun { log; enabled; indent = _; dedent = _; reset_indentation = _ }
     ~instr_prefix ~term_prefix body term liveness ->
  DLL.iter body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
      log ~no_eol:() "%s " (instr_prefix instr);
      if enabled then Cfg.dump_basic Format.err_formatter instr.Cfg.desc;
      if enabled then log_instruction_suffix instr liveness);
  log ~no_eol:() "%s " (term_prefix term);
  if enabled
  then Cfg.dump_terminator ~sep:", " Format.err_formatter term.Cfg.desc;
  if enabled then log_instruction_suffix term liveness

let make_log_cfg_with_infos :
    log_function ->
    instr_prefix:(Cfg.basic Cfg.instruction -> string) ->
    term_prefix:(Cfg.terminator Cfg.instruction -> string) ->
    Cfg_with_infos.t ->
    unit =
 fun ({ indent; dedent; log; enabled; reset_indentation = _ } as log_function)
     ~instr_prefix ~term_prefix cfg_with_infos ->
  if enabled
  then
    let liveness = Cfg_with_infos.liveness cfg_with_infos in
    let cfg = Cfg_with_infos.cfg cfg_with_infos in
    let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
    let layout = Cfg_with_layout.layout cfg_with_layout in
    let log_body_and_terminator =
      make_log_body_and_terminator log_function ~instr_prefix ~term_prefix
    in
    DLL.iter layout ~f:(fun label ->
        let block = Cfg.get_block_exn cfg label in
        let exn =
          match block.exn with
          | None -> " [no exn]"
          | Some exn_label ->
            Printf.sprintf " [exn: %s]" (Label.to_string exn_label)
        in
        let handler =
          match block.is_trap_handler with false -> "" | true -> " [handler]"
        in
        log "(block %a)%s%s" Label.format block.start exn handler;
        indent ();
        log_body_and_terminator block.body block.terminator liveness;
        dedent ())

module Move = struct
  type t =
    | Plain
    | Load
    | Store

  let op_of_move = function
    | Plain -> Operation.Move
    | Load -> Operation.Reload
    | Store -> Operation.Spill

  let make_instr :
      t ->
      id:InstructionId.t ->
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
      irc_work_list = Unknown_list;
      ls_order = -1;
      available_before = instr.available_before;
      available_across = instr.available_across
    }

  let to_string = function Plain -> "move" | Load -> "load" | Store -> "store"
end

let same_reg_class : Reg.t -> Reg.t -> bool =
 fun reg1 reg2 ->
  Int.equal (Proc.register_class reg1) (Proc.register_class reg2)

let same_stack_class : Reg.t -> Reg.t -> bool =
 fun reg1 reg2 ->
  Stack_class.equal
    (Stack_class.of_machtype reg1.typ)
    (Stack_class.of_machtype reg2.typ)

let make_temporary :
    same_class_and_base_name_as:Reg.t -> name_prefix:string -> Reg.t =
 fun ~same_class_and_base_name_as:reg ~name_prefix ->
  let new_temp = Reg.create reg.Reg.typ in
  let name =
    Reg.Raw_name.to_string reg.Reg.raw_name |> Option.value ~default:"anon"
  in
  let name = name_prefix ^ "-" ^ name in
  new_temp.Reg.raw_name
    <- Reg.Raw_name.create_from_var (Backend_var.create_local name);
  new_temp

let simplify_cfg : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  Cfg.iter_blocks cfg ~f:(fun _label block ->
      DLL.filter_left block.body ~f:(fun instr -> not (Cfg.is_noop_move instr)));
  Eliminate_fallthrough_blocks.run cfg_with_layout;
  Merge_straightline_blocks.run cfg_with_layout;
  Eliminate_dead_code.run_dead_block cfg_with_layout;
  Simplify_terminator.run cfg;
  Eliminate_dead_code.run_dead_block cfg_with_layout;
  cfg_with_layout

let save_cfg : string -> Cfg_with_layout.t -> unit =
 fun str cfg_with_layout ->
  Cfg_with_layout.save_as_dot cfg_with_layout ~show_instr:true ~show_exn:true
    ~annotate_block:(fun label ->
      let block =
        Cfg.get_block_exn (Cfg_with_layout.cfg cfg_with_layout) label
      in
      Printf.sprintf "label:%s stack_offset:%d" (Label.to_string label)
        block.stack_offset)
    ~annotate_succ:(fun lbl1 lbl2 ->
      Printf.sprintf "%s->%s" (Label.to_string lbl1) (Label.to_string lbl2))
    str

let remove_prologue : Cfg.basic_block -> bool =
 fun block ->
  let removed = ref false in
  DLL.filter_left block.body ~f:(fun instr ->
      match[@ocaml.warning "-4"] instr.Cfg.desc with
      | Cfg.Prologue ->
        removed := true;
        false
      | _ -> true);
  !removed

let remove_prologue_if_not_required : Cfg_with_layout.t -> unit =
 fun cfg_with_layout ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let prologue_required =
    Proc.prologue_required ~fun_contains_calls:cfg.fun_contains_calls
      ~fun_num_stack_slots:cfg.fun_num_stack_slots
  in
  if not prologue_required
  then
    (* note: `Cfgize` has put the prologue in the entry block or its
       successor. *)
    let entry_block = Cfg.get_block_exn cfg cfg.entry_label in
    let removed = remove_prologue entry_block in
    if not removed
    then
      match entry_block.terminator.desc with
      | Always label ->
        let block = Cfg.get_block_exn cfg label in
        let removed = remove_prologue block in
        assert removed
      | Never | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
      | Switch _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _
      | Call_no_return _ | Call _ | Prim _ ->
        assert false

let update_live_fields : Cfg_with_layout.t -> liveness -> unit =
 fun cfg_with_layout liveness ->
  (* CR xclerc for xclerc: partial duplicate of
     `Asmgen.recompute_liveness_on_cfg` *)
  let set_liveness (instr : _ Cfg.instruction) =
    match InstructionId.Tbl.find_opt liveness instr.id with
    | None ->
      fatal "Missing liveness information for instruction %a"
        InstructionId.format instr.id
    | Some { Cfg_liveness.before = _; across } -> instr.live <- across
  in
  Cfg.iter_blocks (Cfg_with_layout.cfg cfg_with_layout) ~f:(fun _label block ->
      DLL.iter block.body ~f:set_liveness;
      set_liveness block.terminator)

(* CR-soon xclerc for xclerc: consider adding an overflow check. *)
let pow ~base n =
  let res = ref 1 in
  for _ = 1 to n do
    res := !res * base
  done;
  !res

let spill_normal_cost = lazy (find_param_value "SPILL_NORMAL_COST")

let spill_cold_cost = lazy (find_param_value "SPILL_COLD_COST")

let spill_loop_cost = lazy (find_param_value "SPILL_LOOP_COST")

let cost_for_block : Cfg.basic_block -> int =
 fun block ->
  let param =
    match block.cold with false -> spill_normal_cost | true -> spill_cold_cost
  in
  match Lazy.force param with None -> 1 | Some cost -> int_of_string cost

let update_spill_cost : Cfg_with_infos.t -> flat:bool -> unit -> unit =
 fun cfg_with_infos ~flat () ->
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
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let loops_depths : Cfg_loop_infos.loop_depths =
    if flat
    then Label.Map.empty
    else (Cfg_with_infos.loop_infos cfg_with_infos).loop_depths
  in
  Cfg.iter_blocks cfg ~f:(fun label block ->
      let base_cost = cost_for_block block in
      let cost_multiplier =
        match Label.Map.find_opt label loops_depths with
        | None ->
          assert flat;
          1
        | Some depth ->
          let base =
            match Lazy.force spill_loop_cost with
            | None -> 10
            | Some cost -> int_of_string cost
          in
          pow ~base depth
      in
      let cost = base_cost * cost_multiplier in
      DLL.iter ~f:(fun instr -> update_instr cost instr) block.body;
      (* Ignore probes *)
      match[@ocaml.warning "-4"] block.terminator.desc with
      | Prim { op = Probe _; _ } -> ()
      | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
      | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
      | Tailcall_func _ | Call_no_return _ | Call _ | Prim _ ->
        update_instr cost block.terminator)

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
      str1 str2 Printreg.reg reg1 Printreg.reg reg2

type stack_operands_rewrite =
  | All_spilled_registers_rewritten
  | May_still_have_spilled_registers

let equal_stack_operands_rewrite left right =
  match left, right with
  | All_spilled_registers_rewritten, All_spilled_registers_rewritten
  | May_still_have_spilled_registers, May_still_have_spilled_registers ->
    true
  | (All_spilled_registers_rewritten | May_still_have_spilled_registers), _ ->
    false

type spilled_map = Substitution.t

let is_spilled (map : spilled_map) (reg : Reg.t) : bool = Reg.Tbl.mem map reg

let use_stack_operand (map : spilled_map) (regs : Reg.t array) (index : int) :
    unit =
  let reg = regs.(index) in
  match Reg.Tbl.find_opt map reg with
  | None -> fatal "register %a is missing from the map" Printreg.reg reg
  | Some spilled_reg -> regs.(index) <- spilled_reg

let may_use_stack_operands_array : spilled_map -> Reg.t array -> unit =
 fun map regs ->
  Array.iteri regs ~f:(fun i reg ->
      if is_spilled map reg then use_stack_operand map regs i)

let may_use_stack_operands_everywhere :
    type a. spilled_map -> a Cfg.instruction -> stack_operands_rewrite =
 fun map instr ->
  may_use_stack_operands_array map instr.arg;
  may_use_stack_operands_array map instr.res;
  All_spilled_registers_rewritten

let insert_block :
    Cfg_with_layout.t ->
    Cfg.basic_instruction_list ->
    after:Cfg.basic_block ->
    before:Cfg.basic_block option ->
    next_instruction_id:(unit -> InstructionId.t) ->
    Cfg.basic_block list =
 fun cfg_with_layout body ~after:predecessor_block ~before:only_successor
     ~next_instruction_id ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  let successors =
    match only_successor with
    | None -> Cfg.successor_labels ~normal:true ~exn:false predecessor_block
    | Some only_successor -> Label.Set.singleton only_successor.start
  in
  if Label.Set.cardinal successors = 0
  then
    Misc.fatal_errorf
      "Cannot insert a block after block %a: it has no successors" Label.print
      predecessor_block.start;
  let dbg, fdo, live, stack_offset, available_before, available_across =
    match DLL.last body with
    | None ->
      ( Debuginfo.none,
        Fdo_info.none,
        Reg.Set.empty,
        predecessor_block.stack_offset,
        None,
        None )
    | Some
        { dbg; fdo; live; stack_offset; available_before; available_across; _ }
      ->
      dbg, fdo, live, stack_offset, available_before, available_across
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
  Label.Set.fold
    (fun successor_label new_labels ->
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
              dbg;
              fdo;
              live;
              stack_offset;
              id = next_instruction_id ();
              irc_work_list = Unknown_list;
              ls_order = -1;
              available_before;
              available_across
            };
          (* The [predecessor_block] is the only predecessor. *)
          predecessors = Label.Set.singleton predecessor_block.start;
          stack_offset = predecessor_block.terminator.stack_offset;
          exn = None;
          can_raise = false;
          is_trap_handler = false;
          dead = predecessor_block.dead;
          cold = predecessor_block.cold
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
           |> Label.Set.add start;
      block :: new_labels)
    successors []

let occurs_array : Reg.t array -> Reg.t -> bool =
 fun regs reg ->
  let i = ref 0 in
  let len = Array.length regs in
  while !i < len && not (Reg.same (Array.unsafe_get regs !i) reg) do
    incr i
  done;
  !i < len

let occurs_instruction : _ Cfg.instruction -> Reg.t -> bool =
 fun instr reg -> occurs_array instr.arg reg || occurs_array instr.res reg

let occurs_block_body : Cfg.basic_block -> Reg.t -> bool =
 fun block reg ->
  DLL.exists block.body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
      occurs_instruction instr reg)

let occurs_block : Cfg.basic_block -> Reg.t -> bool =
 fun block reg ->
  occurs_block_body block reg || occurs_instruction block.terminator reg
