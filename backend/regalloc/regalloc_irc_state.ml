[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils
open! Regalloc_irc_utils
module Doubly_linked_list = Oxcaml_utils.Doubly_linked_list

module RegWorkList = Arrayset.Make (struct
  type t = Reg.t

  let compare = Reg.compare

  let dummy = { Reg.dummy with stamp = -1 }
end)

let reg_set_of_reg_work_list (rwl : RegWorkList.t) : Reg.Set.t =
  RegWorkList.fold rwl ~init:Reg.Set.empty ~f:(fun acc elem ->
      Reg.Set.add elem acc)

module InstructionWorkList = Arrayset.Make (struct
  type t = Instruction.t

  let compare = Instruction.compare

  let dummy = Instruction.dummy
end)

let instruction_set_of_instruction_work_list (iwl : InstructionWorkList.t) :
    Instruction.Set.t =
  InstructionWorkList.fold iwl ~init:Instruction.Set.empty ~f:(fun acc elem ->
      Instruction.Set.add elem acc)

type t =
  { mutable initial : Reg.t Doubly_linked_list.t;
    simplify_work_list : RegWorkList.t;
    freeze_work_list : RegWorkList.t;
    spill_work_list : RegWorkList.t;
    spilled_nodes : RegWorkList.t;
    coalesced_nodes : RegWorkList.t;
    colored_nodes : Reg.t Doubly_linked_list.t;
    mutable select_stack : Reg.t list;
    coalesced_moves : InstructionWorkList.t;
    constrained_moves : InstructionWorkList.t;
    frozen_moves : InstructionWorkList.t;
    work_list_moves : InstructionWorkList.t;
    active_moves : InstructionWorkList.t;
    adj_set : RegisterStamp.PairSet.t;
    move_list : Instruction.Set.t Reg.Tbl.t;
    stack_slots : Regalloc_stack_slots.t;
    mutable inst_temporaries : Reg.Set.t;
    mutable block_temporaries : Reg.Set.t;
    reg_work_list : WorkList.t Reg.Tbl.t;
    reg_color : int option Reg.Tbl.t;
    reg_alias : Reg.t option Reg.Tbl.t;
    reg_interf : Reg.t list Reg.Tbl.t;
    reg_degree : int Reg.Tbl.t
  }

let[@inline] make ~initial ~stack_slots () =
  let num_registers = List.length (Reg.all_relocatable_regs ()) in
  let reg_work_list = Reg.Tbl.create num_registers in
  let reg_color = Reg.Tbl.create num_registers in
  let reg_alias = Reg.Tbl.create num_registers in
  let reg_interf = Reg.Tbl.create num_registers in
  let reg_degree = Reg.Tbl.create num_registers in
  List.iter (Reg.all_relocatable_regs ()) ~f:(fun reg ->
      Reg.Tbl.replace reg_work_list reg WorkList.Unknown_list;
      Reg.Tbl.replace reg_color reg None;
      Reg.Tbl.replace reg_alias reg None;
      Reg.Tbl.replace reg_interf reg [];
      Reg.Tbl.replace reg_degree reg 0);
  List.iter initial ~f:(fun reg ->
      Reg.Tbl.replace reg_work_list reg WorkList.Initial);
  Reg.Set.iter
    (fun reg ->
      Reg.Tbl.replace reg_work_list reg WorkList.Precolored;
      Reg.Tbl.replace reg_color reg
        (match reg.Reg.loc with
        | Reg color -> Some color
        | Unknown | Stack _ ->
          fatal "precolored register %a is not an hardware register"
            Printreg.reg reg);
      Reg.Tbl.replace reg_alias reg None;
      Reg.Tbl.replace reg_interf reg [];
      Reg.Tbl.replace reg_degree reg Degree.infinite)
    (all_precolored_regs ());
  let original_capacity = num_registers in
  let simplify_work_list = RegWorkList.make ~original_capacity in
  let freeze_work_list = RegWorkList.make ~original_capacity in
  let spill_work_list = RegWorkList.make ~original_capacity in
  let spilled_nodes = RegWorkList.make ~original_capacity in
  let coalesced_nodes = RegWorkList.make ~original_capacity in
  let colored_nodes = Doubly_linked_list.make_empty () in
  let select_stack = [] in
  let original_capacity = 128 in
  let coalesced_moves = InstructionWorkList.make ~original_capacity in
  let constrained_moves = InstructionWorkList.make ~original_capacity in
  let frozen_moves = InstructionWorkList.make ~original_capacity in
  let work_list_moves = InstructionWorkList.make ~original_capacity in
  let active_moves = InstructionWorkList.make ~original_capacity in
  let adj_set = RegisterStamp.PairSet.make ~num_registers in
  let move_list = Reg.Tbl.create 128 in
  let inst_temporaries = Reg.Set.empty in
  let block_temporaries = Reg.Set.empty in
  let initial = Doubly_linked_list.of_list initial in
  { initial;
    simplify_work_list;
    freeze_work_list;
    spill_work_list;
    spilled_nodes;
    coalesced_nodes;
    colored_nodes;
    select_stack;
    coalesced_moves;
    constrained_moves;
    frozen_moves;
    work_list_moves;
    active_moves;
    adj_set;
    move_list;
    stack_slots;
    inst_temporaries;
    block_temporaries;
    reg_work_list;
    reg_color;
    reg_alias;
    reg_interf;
    reg_degree
  }

let[@inline] add_initial_one state reg =
  Reg.Tbl.replace state.reg_work_list reg WorkList.Initial;
  Reg.Tbl.replace state.reg_color reg None;
  Reg.Tbl.replace state.reg_alias reg None;
  Reg.Tbl.replace state.reg_interf reg [];
  Reg.Tbl.replace state.reg_degree reg 0;
  Doubly_linked_list.add_begin state.initial reg

let[@inline] add_initial_list state regs =
  List.iter regs ~f:(fun reg ->
      Reg.Tbl.replace state.reg_work_list reg WorkList.Initial;
      Reg.Tbl.replace state.reg_color reg None;
      Reg.Tbl.replace state.reg_alias reg None;
      Reg.Tbl.replace state.reg_interf reg [];
      Reg.Tbl.replace state.reg_degree reg 0;
      Doubly_linked_list.add_begin state.initial reg)

let[@inline] reset state ~new_inst_temporaries ~new_block_temporaries =
  let unknown_reg_work_list (rwl : RegWorkList.t) : unit =
    RegWorkList.iter rwl ~f:(fun reg ->
        Reg.Tbl.replace state.reg_work_list reg WorkList.Unknown_list)
  in
  let unknown_instruction_work_list (iwl : InstructionWorkList.t) : unit =
    InstructionWorkList.iter iwl ~f:(fun instr ->
        instr.irc_work_list <- Unknown_list)
  in
  List.iter (Reg.all_relocatable_regs ()) ~f:(fun reg ->
      Reg.Tbl.replace state.reg_color reg None;
      Reg.Tbl.replace state.reg_alias reg None;
      Reg.Tbl.replace state.reg_interf reg [];
      Reg.Tbl.replace state.reg_degree reg 0);
  Reg.Set.iter
    (fun reg ->
      assert (
        WorkList.equal
          (Reg.Tbl.find state.reg_work_list reg)
          WorkList.Precolored);
      (match reg.Reg.loc, Reg.Tbl.find state.reg_color reg with
      | Reg color, Some color' -> assert (color = color')
      | Reg _, None -> assert false
      | (Unknown | Stack _), _ -> assert false);
      Reg.Tbl.replace state.reg_alias reg None;
      Reg.Tbl.replace state.reg_interf reg [];
      assert (Reg.Tbl.find state.reg_degree reg = Degree.infinite))
    (all_precolored_regs ());
  state.initial <- Doubly_linked_list.of_list new_inst_temporaries;
  Doubly_linked_list.add_list state.initial new_block_temporaries;
  Doubly_linked_list.transfer ~from:state.colored_nodes ~to_:state.initial ();
  RegWorkList.iter state.coalesced_nodes ~f:(fun reg ->
      Doubly_linked_list.add_end state.initial reg);
  Doubly_linked_list.iter state.initial ~f:(fun reg ->
      Reg.Tbl.replace state.reg_work_list reg WorkList.Initial);
  unknown_reg_work_list state.simplify_work_list;
  RegWorkList.clear state.simplify_work_list;
  unknown_reg_work_list state.freeze_work_list;
  RegWorkList.clear state.freeze_work_list;
  unknown_reg_work_list state.spill_work_list;
  RegWorkList.clear state.spill_work_list;
  unknown_reg_work_list state.spilled_nodes;
  RegWorkList.clear state.spilled_nodes;
  RegWorkList.clear state.coalesced_nodes;
  assert (Misc.Stdlib.List.is_empty state.select_stack);
  unknown_instruction_work_list state.coalesced_moves;
  InstructionWorkList.clear state.coalesced_moves;
  unknown_instruction_work_list state.constrained_moves;
  InstructionWorkList.clear state.constrained_moves;
  unknown_instruction_work_list state.frozen_moves;
  InstructionWorkList.clear state.frozen_moves;
  unknown_instruction_work_list state.work_list_moves;
  InstructionWorkList.clear state.work_list_moves;
  unknown_instruction_work_list state.active_moves;
  InstructionWorkList.clear state.active_moves;
  RegisterStamp.PairSet.clear state.adj_set;
  Reg.Tbl.clear state.move_list

let[@inline] work_list state reg =
  match Reg.Tbl.find_opt state.reg_work_list reg with
  | None -> fatal "%a is not in the work_list map" Printreg.reg reg
  | Some x -> x

let[@inline] work_list_opt state reg = Reg.Tbl.find_opt state.reg_work_list reg

let[@inline] color state reg =
  match Reg.Tbl.find_opt state.reg_color reg with
  | None -> fatal "%a is not in the color map" Printreg.reg reg
  | Some x -> x

let[@inline] set_color state reg color =
  Reg.Tbl.replace state.reg_color reg color

let[@inline] degree state reg =
  match Reg.Tbl.find_opt state.reg_degree reg with
  | None -> fatal "%a is not in the degree map" Printreg.reg reg
  | Some x -> x

let[@inline] set_degree state reg degree =
  Reg.Tbl.replace state.reg_degree reg degree

let[@inline] is_precolored state reg =
  WorkList.equal (work_list state reg) WorkList.Precolored

let[@inline] is_precolored_or_colored state reg =
  match work_list state reg with
  | Precolored | Colored -> true
  | Unknown_list | Initial | Simplify | Freeze | Spill | Spilled | Coalesced
  | Select_stack ->
    false

let[@inline] iter_and_clear_initial state ~f =
  Doubly_linked_list.iter state.initial ~f:(fun reg ->
      Reg.Tbl.replace state.reg_work_list reg WorkList.Unknown_list);
  Doubly_linked_list.iter state.initial ~f;
  Doubly_linked_list.clear state.initial

let[@inline] is_empty_simplify_work_list state =
  RegWorkList.is_empty state.simplify_work_list

let[@inline] add_simplify_work_list state reg =
  Reg.Tbl.replace state.reg_work_list reg WorkList.Simplify;
  RegWorkList.add state.simplify_work_list reg

let[@inline] choose_and_remove_simplify_work_list state =
  match RegWorkList.choose_and_remove state.simplify_work_list with
  | None -> fatal "simplify_work_list is empty"
  | Some res ->
    Reg.Tbl.replace state.reg_work_list res WorkList.Unknown_list;
    res

let[@inline] is_empty_freeze_work_list state =
  RegWorkList.is_empty state.freeze_work_list

let[@inline] mem_freeze_work_list state reg =
  WorkList.equal (work_list state reg) WorkList.Freeze

let[@inline] add_freeze_work_list state reg =
  Reg.Tbl.replace state.reg_work_list reg WorkList.Freeze;
  RegWorkList.add state.freeze_work_list reg

let[@inline] remove_freeze_work_list state reg =
  Reg.Tbl.replace state.reg_work_list reg WorkList.Unknown_list;
  RegWorkList.remove state.freeze_work_list reg

let[@inline] choose_and_remove_freeze_work_list state =
  match RegWorkList.choose_and_remove state.freeze_work_list with
  | None -> fatal "freeze_work_list is empty"
  | Some res ->
    Reg.Tbl.replace state.reg_work_list res WorkList.Unknown_list;
    res

let[@inline] is_empty_spill_work_list state =
  RegWorkList.is_empty state.spill_work_list

let[@inline] mem_spill_work_list state reg =
  WorkList.equal (work_list state reg) WorkList.Spill

let[@inline] add_spill_work_list state reg =
  Reg.Tbl.replace state.reg_work_list reg WorkList.Spill;
  RegWorkList.add state.spill_work_list reg

let[@inline] remove_spill_work_list state reg =
  Reg.Tbl.replace state.reg_work_list reg WorkList.Unknown_list;
  RegWorkList.remove state.spill_work_list reg

let[@inline] fold_spill_work_list state ~f ~init =
  RegWorkList.fold state.spill_work_list ~f ~init

let[@inline] spill_work_list state =
  reg_set_of_reg_work_list state.spill_work_list

let[@inline] is_empty_spilled_nodes state =
  RegWorkList.is_empty state.spilled_nodes

let[@inline] add_spilled_nodes state reg =
  Reg.Tbl.replace state.reg_work_list reg WorkList.Spilled;
  RegWorkList.add state.spilled_nodes reg

let[@inline] spilled_nodes state = RegWorkList.to_list state.spilled_nodes

let[@inline] clear_spilled_nodes state =
  RegWorkList.iter state.spilled_nodes ~f:(fun reg ->
      Reg.Tbl.replace state.reg_work_list reg WorkList.Unknown_list);
  RegWorkList.clear state.spilled_nodes

let[@inline] add_coalesced_nodes state reg =
  Reg.Tbl.replace state.reg_work_list reg WorkList.Coalesced;
  RegWorkList.add state.coalesced_nodes reg

let[@inline] iter_coalesced_nodes state ~f =
  RegWorkList.iter state.coalesced_nodes ~f

let[@inline] add_colored_nodes state reg =
  Reg.Tbl.replace state.reg_work_list reg WorkList.Colored;
  Doubly_linked_list.add_begin state.colored_nodes reg

let[@inline] is_empty_select_stack state =
  Misc.Stdlib.List.is_empty state.select_stack

let[@inline] push_select_stack state reg =
  Reg.Tbl.replace state.reg_work_list reg WorkList.Select_stack;
  state.select_stack <- reg :: state.select_stack

let[@inline] pop_select_stack state =
  match state.select_stack with
  | [] -> fatal "select_stack is empty"
  | hd :: tl ->
    state.select_stack <- tl;
    Reg.Tbl.replace state.reg_work_list hd WorkList.Unknown_list;
    hd

let[@inline] iter_and_clear_select_stack state ~f =
  List.iter state.select_stack ~f;
  state.select_stack <- []

let[@inline] add_coalesced_moves state instr =
  instr.Cfg.irc_work_list <- Coalesced;
  InstructionWorkList.add state.coalesced_moves instr

let[@inline] add_constrained_moves state instr =
  instr.Cfg.irc_work_list <- Constrained;
  InstructionWorkList.add state.constrained_moves instr

let[@inline] add_frozen_moves state instr =
  instr.Cfg.irc_work_list <- Frozen;
  InstructionWorkList.add state.frozen_moves instr

let[@inline] is_empty_work_list_moves state =
  InstructionWorkList.is_empty state.work_list_moves

let[@inline] add_work_list_moves state instr =
  instr.Cfg.irc_work_list <- Work_list;
  InstructionWorkList.add state.work_list_moves instr

let[@inline] choose_and_remove_work_list_moves state =
  match InstructionWorkList.choose_and_remove state.work_list_moves with
  | None -> fatal "work_list_moves is empty"
  | Some res ->
    res.Cfg.irc_work_list <- Unknown_list;
    res

let[@inline] mem_active_moves _state instr =
  Cfg.equal_irc_work_list instr.Cfg.irc_work_list Cfg.Active

let[@inline] add_active_moves state instr =
  instr.Cfg.irc_work_list <- Active;
  InstructionWorkList.add state.active_moves instr

let[@inline] remove_active_moves state instr =
  instr.Cfg.irc_work_list <- Unknown_list;
  InstructionWorkList.remove state.active_moves instr

let[@inline] mem_adj_set state reg1 reg2 =
  RegisterStamp.PairSet.mem state.adj_set
    (RegisterStamp.pair reg1.Reg.stamp reg2.Reg.stamp)

let[@inline] adj_list state reg = Reg.Tbl.find state.reg_interf reg

let[@inline] add_edge state u v =
  let is_interesting_reg reg =
    match reg.Reg.loc with
    | Reg _ -> true
    | Unknown -> true
    | Stack (Local _ | Incoming _ | Outgoing _ | Domainstate _) -> false
  in
  let pair = RegisterStamp.pair u.Reg.stamp v.Reg.stamp in
  if (not (Reg.same u v))
     && is_interesting_reg u && is_interesting_reg v && same_reg_class u v
     && not (RegisterStamp.PairSet.mem state.adj_set pair)
  then (
    RegisterStamp.PairSet.add state.adj_set pair;
    let add_adj_list x y =
      Reg.Tbl.replace state.reg_interf x (y :: Reg.Tbl.find state.reg_interf x)
    in
    let incr_degree x =
      let deg = degree state x in
      if debug && deg = Degree.infinite
      then fatal "trying to increment the degree of a precolored node";
      Reg.Tbl.replace state.reg_degree x (succ deg)
    in
    if not (is_precolored state u)
    then (
      add_adj_list u v;
      incr_degree u);
    if not (is_precolored state v)
    then (
      add_adj_list v u;
      incr_degree v))

let[@inline] iter_adjacent state reg ~f =
  List.iter (adj_list state reg) ~f:(fun reg ->
      match work_list state reg with
      | Select_stack | Coalesced -> ()
      | Unknown_list | Precolored | Initial | Simplify | Freeze | Spill
      | Spilled | Colored ->
        f reg)

let[@inline] for_all_adjacent state reg ~f =
  List.for_all (adj_list state reg) ~f:(fun reg ->
      match work_list state reg with
      | Select_stack | Coalesced -> true
      | Unknown_list | Precolored | Initial | Simplify | Freeze | Spill
      | Spilled | Colored ->
        f reg)

let[@inline] adj_set state = state.adj_set

let[@inline] is_empty_node_moves state reg =
  match Reg.Tbl.find_opt state.move_list reg with
  | None -> true
  | Some move_list ->
    not
      (Instruction.Set.exists
         (fun instr ->
           match instr.irc_work_list with
           | Active | Work_list -> true
           | Unknown_list | Coalesced | Constrained | Frozen -> false)
         move_list)

let[@inline] iter_node_moves state reg ~f =
  match Reg.Tbl.find_opt state.move_list reg with
  | None -> ()
  | Some move_list ->
    Instruction.Set.iter
      (fun instr ->
        match instr.irc_work_list with
        | Active | Work_list -> f instr
        | Unknown_list | Coalesced | Constrained | Frozen -> ())
      move_list

let[@inline] is_move_related state reg =
  match Reg.Tbl.find_opt state.move_list reg with
  | None -> false
  | Some move_list ->
    Instruction.Set.exists
      (fun instr ->
        match instr.irc_work_list with
        | Active | Work_list -> true
        | Unknown_list | Coalesced | Constrained | Frozen -> false)
      move_list

let[@inline] enable_moves_one state reg =
  let n = reg in
  iter_node_moves state n ~f:(fun m ->
      match m.irc_work_list with
      | Active ->
        m.irc_work_list <- Work_list;
        InstructionWorkList.remove state.active_moves m;
        InstructionWorkList.add state.work_list_moves m
      | Unknown_list | Coalesced | Constrained | Frozen | Work_list -> ())

let[@inline] decr_degree state reg =
  let d = degree state reg in
  if d = Degree.infinite
  then ()
  else (
    Reg.Tbl.replace state.reg_degree reg (pred d);
    if Int.equal d (k reg)
    then (
      enable_moves_one state reg;
      iter_adjacent state reg ~f:(fun r -> enable_moves_one state r);
      Reg.Tbl.replace state.reg_work_list reg WorkList.Unknown_list;
      RegWorkList.remove state.spill_work_list reg;
      if is_move_related state reg
      then (
        Reg.Tbl.replace state.reg_work_list reg WorkList.Freeze;
        RegWorkList.add state.freeze_work_list reg)
      else (
        Reg.Tbl.replace state.reg_work_list reg WorkList.Simplify;
        RegWorkList.add state.simplify_work_list reg)))

let[@inline] find_move_list state reg =
  match Reg.Tbl.find_opt state.move_list reg with
  | None -> Instruction.Set.empty
  | Some res -> res

let[@inline] add_move_list state reg instr =
  match Reg.Tbl.find_opt state.move_list reg with
  | None ->
    Reg.Tbl.replace state.move_list reg (Instruction.Set.singleton instr)
  | Some existing ->
    Reg.Tbl.replace state.move_list reg (Instruction.Set.add instr existing)

let[@inline] union_move_list state reg set =
  match Reg.Tbl.find_opt state.move_list reg with
  | None -> Reg.Tbl.replace state.move_list reg set
  | Some existing ->
    Reg.Tbl.replace state.move_list reg (Instruction.Set.union existing set)

let[@inline] rec find_alias state reg =
  if WorkList.equal (work_list state reg) WorkList.Coalesced
  then
    match Reg.Tbl.find state.reg_alias reg with
    | None -> fatal "register %a has no alias" Printreg.reg reg
    | Some reg' -> find_alias state reg'
  else reg

let[@inline] add_alias state v u =
  (* We should never generate moves between registers of different types.
     Bit-casting operations have specific instructions. *)
  if not (Proc.types_are_compatible v u)
  then
    fatal
      "trying to create an alias between %a and %a but they have incompatible \
       types"
      Printreg.reg v Printreg.reg u;
  Reg.Tbl.replace state.reg_alias v (Some u)

let[@inline] stack_slots state = state.stack_slots

let[@inline] add_inst_temporaries_list state regs =
  state.inst_temporaries
    <- Reg.Set.add_seq (List.to_seq regs) state.inst_temporaries

let[@inline] add_block_temporaries_list state regs =
  state.block_temporaries
    <- Reg.Set.add_seq (List.to_seq regs) state.block_temporaries

let[@inline] mem_inst_temporaries state reg =
  Reg.Set.mem reg state.inst_temporaries

let[@inline] mem_block_temporaries state reg =
  Reg.Set.mem reg state.block_temporaries

let[@inline] mem_all_introduced_temporaries state reg =
  mem_inst_temporaries state reg || mem_block_temporaries state reg

let[@inline] diff_all_introduced_temporaries state set =
  Reg.Set.diff (Reg.Set.diff set state.inst_temporaries) state.block_temporaries

let update_register_locations state =
  if debug then log "update_register_locations";
  List.iter (Reg.all_relocatable_regs ()) ~f:(fun reg ->
      match reg.Reg.loc with
      | Reg _ -> ()
      | Stack _ -> ()
      | Unknown -> (
        match Reg.Tbl.find state.reg_color reg with
        | None ->
          (* because of rewrites, the register may no longer be present *)
          ()
        | Some color ->
          if debug then log "updating %a to %d" Printreg.reg reg color;
          reg.Reg.loc <- Reg color))

let[@inline] check_disjoint sets ~is_disjoint =
  List.iter sets ~f:(fun (name1, set1) ->
      List.iter sets ~f:(fun (name2, set2) ->
          if String.compare name1 name2 < 0
          then
            if not (is_disjoint set1 set2)
            then fatal "sets %s and %s are not disjoint" name1 name2))

let[@inline] check_set_and_field_consistency_reg state
    (worklist, set, field_value) =
  Reg.Set.iter
    (fun reg ->
      if not (WorkList.equal (work_list state reg) field_value)
      then
        fatal "register %a is in %s but its field equals %S" Printreg.reg reg
          worklist
          (WorkList.to_string (work_list state reg)))
    set

let[@inline] check_set_and_field_consistency_instr (work_list, set, field_value)
    =
  Instruction.Set.iter
    (fun instr ->
      if not (Cfg.equal_irc_work_list instr.Cfg.irc_work_list field_value)
      then
        fatal "instruction %a is in %s but its field equals %S"
          InstructionId.format instr.Cfg.id work_list
          (Cfg.string_of_irc_work_list instr.Cfg.irc_work_list))
    set

let[@inline] check_inter_has_no_duplicates state (reg : Reg.t) : unit =
  let l = Reg.Tbl.find state.reg_interf reg in
  let s = Reg.Set.of_list l in
  if List.length l <> Reg.Set.cardinal s
  then fatal "interf list for %a is not a set" Printreg.reg reg

let reg_set_of_doubly_linked_list (l : Reg.t Doubly_linked_list.t) : Reg.Set.t =
  Doubly_linked_list.fold_right l ~init:Reg.Set.empty ~f:Reg.Set.add

let[@inline] invariant state =
  (* CR xclerc for xclerc: avoid multiple conversions to sets. *)
  if debug && Lazy.force invariants
  then (
    (* interf (list) is morally a set *)
    List.iter
      (Reg.all_relocatable_regs ())
      ~f:(check_inter_has_no_duplicates state);
    Reg.Set.iter (check_inter_has_no_duplicates state) (all_precolored_regs ());
    (* register sets are disjoint *)
    check_disjoint ~is_disjoint:Reg.Set.disjoint
      [ "precolored", all_precolored_regs ();
        "initial", reg_set_of_doubly_linked_list state.initial;
        "simplify_work_list", reg_set_of_reg_work_list state.simplify_work_list;
        "freeze_work_list", reg_set_of_reg_work_list state.freeze_work_list;
        "spill_work_list", reg_set_of_reg_work_list state.spill_work_list;
        "spilled_nodes", reg_set_of_reg_work_list state.spilled_nodes;
        "coalesced_nodes", reg_set_of_reg_work_list state.coalesced_nodes;
        "colored_nodes", reg_set_of_doubly_linked_list state.colored_nodes;
        "select_stack", Reg.Set.of_list state.select_stack ];
    List.iter
      ~f:(check_set_and_field_consistency_reg state)
      [ "precolored", all_precolored_regs (), WorkList.Precolored;
        "initial", reg_set_of_doubly_linked_list state.initial, WorkList.Initial;
        ( "simplify_work_list",
          reg_set_of_reg_work_list state.simplify_work_list,
          WorkList.Simplify );
        ( "freeze_work_list",
          reg_set_of_reg_work_list state.freeze_work_list,
          WorkList.Freeze );
        ( "spill_work_list",
          reg_set_of_reg_work_list state.spill_work_list,
          WorkList.Spill );
        ( "spilled_nodes",
          reg_set_of_reg_work_list state.spilled_nodes,
          WorkList.Spilled );
        ( "coalesced_nodes",
          reg_set_of_reg_work_list state.coalesced_nodes,
          WorkList.Coalesced );
        ( "colored_nodes",
          reg_set_of_doubly_linked_list state.colored_nodes,
          WorkList.Colored );
        ( "select_stack",
          Reg.Set.of_list state.select_stack,
          WorkList.Select_stack ) ];
    (* move sets are disjoint *)
    check_disjoint ~is_disjoint:Instruction.Set.disjoint
      [ ( "coalesced_moves",
          instruction_set_of_instruction_work_list state.coalesced_moves );
        ( "constrained_moves",
          instruction_set_of_instruction_work_list state.constrained_moves );
        ( "frozen_moves",
          instruction_set_of_instruction_work_list state.frozen_moves );
        ( "work_list_moves",
          instruction_set_of_instruction_work_list state.work_list_moves );
        ( "active_moves",
          instruction_set_of_instruction_work_list state.active_moves ) ];
    List.iter ~f:check_set_and_field_consistency_instr
      [ ( "coalesced_moves",
          instruction_set_of_instruction_work_list state.coalesced_moves,
          Cfg.Coalesced );
        ( "constrained_moves",
          instruction_set_of_instruction_work_list state.constrained_moves,
          Cfg.Constrained );
        ( "frozen_moves",
          instruction_set_of_instruction_work_list state.frozen_moves,
          Cfg.Frozen );
        ( "work_list_moves",
          instruction_set_of_instruction_work_list state.work_list_moves,
          Cfg.Work_list );
        ( "active_moves",
          instruction_set_of_instruction_work_list state.active_moves,
          Cfg.Active ) ];
    (* degree is consistent with adjacency lists/sets *)
    let work_lists =
      Reg.Set.union
        (reg_set_of_reg_work_list state.simplify_work_list)
        (Reg.Set.union
           (reg_set_of_reg_work_list state.freeze_work_list)
           (reg_set_of_reg_work_list state.spill_work_list))
    in
    let work_lists_or_precolored =
      Reg.Set.union (all_precolored_regs ()) work_lists
    in
    Reg.Set.iter
      (fun u ->
        let degree = degree state u in
        if degree = Degree.infinite
        then fatal "invariant: infinite degree for %a" Printreg.reg u
        else
          let adj_list = Reg.Set.of_list (adj_list state u) in
          let cardinal =
            Reg.Set.cardinal (Reg.Set.inter adj_list work_lists_or_precolored)
          in
          if not (Int.equal degree cardinal)
          then (
            List.iter (Reg.Tbl.find state.reg_interf u) ~f:(fun r ->
                log "%a <- interf[%a]" Printreg.reg r Printreg.reg u);
            Reg.Set.iter
              (fun r -> log "%a <- adj_list[%a]" Printreg.reg r Printreg.reg u)
              adj_list;
            Reg.Set.iter
              (fun r ->
                log "%a <- work_lists_or_precolored[%a]" Printreg.reg r
                  Printreg.reg u)
              (Reg.Set.inter adj_list work_lists_or_precolored);
            fatal
              "invariant expected degree for %a to be %d but got %d\n\
              \ (#adj_list=%d, #work_lists_or_precolored=%d)" Printreg.reg u
              cardinal degree
              (Reg.Set.cardinal adj_list)
              (Reg.Set.cardinal work_lists_or_precolored)))
      work_lists)
