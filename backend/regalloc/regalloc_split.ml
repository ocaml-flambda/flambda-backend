[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
open! Regalloc_split_utils
module DLL = Flambda_backend_utils.Doubly_linked_list
module State = Regalloc_split_state

(* [remove_from_bindings state label ~field ~extract bindings] removes registers
   in a set from [bindings]. The set is determined by (i) calling [field] on
   [state] in order to get a map, (ii) retrieving the data associated with
   [label] from this map, and (iii) calling [extract] on the data. *)
let[@inline] remove_from_bindings :
    State.t ->
    Label.t ->
    field:(State.t -> 'a Label.Map.t) ->
    extract:('a -> Reg.Set.t) ->
    Reg.t Reg.Map.t ->
    Reg.t Reg.Map.t =
 fun state label ~field ~extract bindings ->
  match Label.Map.find_opt label (field state) with
  | None -> bindings
  | Some data ->
    let regs = extract data in
    Reg.Map.filter
      (fun reg _ ->
        let remove = Reg.Set.mem reg regs in
        if split_debug
        then if remove then log ~indent:3 "removing %a" Printmach.reg reg;
        not remove)
      bindings

(* [compute_substitution_tree state substs bindings tree] populates [substs]
   with the substitutions computed for each block seen by iterating over the
   tree in a prefix manner. [bindings] is the part of the substitutions
   propagating from the parent node in the tree. In effect, the way names are
   distributed is quite similar to what happens in SSA. *)
let rec compute_substitution_tree :
    State.t ->
    Substitution.map ->
    Reg.t Reg.Map.t ->
    Cfg_dominators.dominator_tree ->
    unit =
 fun state substs bindings tree ->
  let label = tree.label in
  if split_debug then log ~indent:1 "compute_substitution_tree %d" label;
  (* First, remove the phi and definitions from the bindings. *)
  if split_debug then log ~indent:2 "removing from phis";
  let bindings =
    remove_from_bindings state label ~field:State.phi_at_beginning
      ~extract:Fun.id bindings
  in
  if split_debug then log ~indent:2 "removing from definitions";
  (* note: it is possible to have two definitions in a row (i.e. without any
     destruction or phi between them), since we delete the dominated
     destructions of constant values. *)
  let bindings =
    remove_from_bindings state label ~field:State.definitions_at_beginning
      ~extract:Fun.id bindings
  in
  let subst = Reg.Tbl.create 17 in
  Label.Tbl.replace substs label subst;
  (* Second, add the bindings to the substitution. *)
  if split_debug then log ~indent:2 "adding from bindings";
  Reg.Map.iter
    (fun old_reg new_reg ->
      if split_debug
      then log ~indent:3 "%a -> %a" Printmach.reg old_reg Printmach.reg new_reg;
      Reg.Tbl.replace subst old_reg new_reg)
    bindings;
  (* Third, add the definitions to the substitution and bindings. *)
  if split_debug then log ~indent:2 "adding from definitions";
  let bindings =
    match Label.Map.find_opt label (State.definitions_at_beginning state) with
    | None -> bindings
    | Some renames ->
      Reg.Set.fold
        (fun old_reg bindings ->
          let slots = State.stack_slots state in
          let (_ : int) = Regalloc_stack_slots.get_or_create slots old_reg in
          let new_reg = Reg.clone old_reg in
          Regalloc_stack_slots.use_same_slot_or_fatal slots new_reg
            ~existing:old_reg;
          if split_debug
          then
            log ~indent:3 "renaming %a to %a" Printmach.reg old_reg
              Printmach.reg new_reg;
          Reg.Tbl.replace subst old_reg new_reg;
          Reg.Map.add old_reg new_reg bindings)
        renames bindings
  in
  (* Fourth, remove the destroyed registers from the bindings. *)
  if split_debug then log ~indent:2 "removing from destructions";
  let bindings =
    remove_from_bindings state label ~field:State.destructions_at_end
      ~extract:snd bindings
  in
  (* Finally, propagate the bindings to the children. *)
  List.iter tree.children ~f:(fun child ->
      compute_substitution_tree state substs bindings child)

(* Computes the substitutions for all blocks, by introducing new registers at
   reload points and then propagating names until new ones take over. *)
let compute_substitutions : State.t -> Cfg_with_infos.t -> Substitution.map =
 fun state cfg_with_infos ->
  if split_debug then log ~indent:0 "compute_substitutions";
  let cfg = Cfg_with_infos.cfg cfg_with_infos in
  let dom_forest =
    Cfg_dominators.dominator_forest (Cfg_with_infos.dominators cfg_with_infos)
  in
  let substs = Label.Tbl.create (Label.Tbl.length cfg.blocks) in
  List.iter dom_forest ~f:(fun dom_tree ->
      compute_substitution_tree state substs Reg.Map.empty dom_tree);
  substs

let apply_substitutions : Cfg_with_infos.t -> Substitution.map -> unit =
 fun cfg_with_infos substs ->
  Substitution.apply_cfg_in_place substs (Cfg_with_infos.cfg cfg_with_infos)

type 'a make_operation =
  State.t ->
  stack_subst:Substitution.t ->
  old_reg:Reg.t ->
  new_reg:Reg.t ->
  copy:'a Cfg.instruction ->
  Instruction.t

(* Creates a spill instruction for the register named `old_reg` before
   substitution and `new_reg` after substitution, copying debug and FDO
   information from `copy`, and populating `stack_subst` with a mapping from
   `old_reg` to its stack slot. *)
let make_spill : type a. a make_operation =
 fun state ~stack_subst ~old_reg ~new_reg ~copy ->
  let stack_reg =
    match Reg.Tbl.find_opt stack_subst old_reg with
    | Some stack_reg -> stack_reg
    | None ->
      let slots = State.stack_slots state in
      let slot : int = Regalloc_stack_slots.get_or_fatal slots old_reg in
      let stack : Reg.t =
        make_temporary ~same_class_and_base_name_as:old_reg ~name_prefix:"stack"
      in
      Regalloc_stack_slots.use_same_slot_or_fatal slots stack ~existing:old_reg;
      stack.Reg.loc <- Reg.(Stack (Local slot));
      Reg.Tbl.replace stack_subst old_reg stack;
      stack
  in
  if split_debug
  then
    log ~indent:3 "spill %a -> %a" Printmach.reg new_reg Printmach.reg stack_reg;
  Move.make_instr Move.Store
    ~id:(State.get_and_incr_instruction_id state)
    ~copy ~from:new_reg ~to_:stack_reg

let dummy_instr_of_terminator : Cfg.terminator Cfg.instruction -> Instruction.t
    =
 fun terminator ->
  { Instruction.dummy with
    dbg = terminator.dbg;
    fdo = terminator.fdo;
    live = terminator.live;
    stack_offset = terminator.stack_offset
  }

let rec insert_spills_or_reloads_in_block :
    State.t ->
    make_spill_or_reload:'a make_operation ->
    occur_check:(Instruction.t -> Reg.t -> bool) ->
    insert:(Instruction.t DLL.cell -> Instruction.t -> unit) ->
    copy_default:Instruction.t ->
    add_default:(Instruction.t DLL.t -> Instruction.t -> unit) ->
    move_cell:(Instruction.t DLL.cell -> Instruction.t DLL.cell option) ->
    block_subst:Substitution.t ->
    stack_subst:Substitution.t ->
    Cfg.basic_block ->
    Instruction.t DLL.cell option ->
    Reg.Set.t ->
    unit =
 fun state ~make_spill_or_reload ~occur_check ~insert ~copy_default ~add_default
     ~move_cell ~block_subst ~stack_subst block cell live_at_interesting_point ->
  match Reg.Set.is_empty live_at_interesting_point with
  | true -> ()
  | false -> (
    match cell with
    | None ->
      Reg.Set.iter
        (fun old_reg ->
          let new_reg = Substitution.apply_reg block_subst old_reg in
          let spill_or_reload =
            make_spill_or_reload state ~stack_subst ~old_reg ~new_reg
              ~copy:copy_default
          in
          add_default block.body spill_or_reload)
        live_at_interesting_point
    | Some cell ->
      let live_at_interesting_point =
        Reg.Set.filter
          (fun old_reg ->
            let new_reg = Substitution.apply_reg block_subst old_reg in
            let instr = DLL.value cell in
            if occur_check instr new_reg
            then (
              let spill_or_reload =
                make_spill_or_reload state ~stack_subst ~old_reg ~new_reg
                  ~copy:instr
              in
              insert cell spill_or_reload;
              false)
            else true)
          live_at_interesting_point
      in
      let cell = move_cell cell in
      insert_spills_or_reloads_in_block state ~make_spill_or_reload ~occur_check
        ~insert ~copy_default ~add_default ~move_cell ~block_subst ~stack_subst
        block cell live_at_interesting_point)

(* Inserts the spills in a block, as early as possible (i.e. immediately after
   the register is last set), to reduce live ranges. *)
let insert_spills_in_block :
    State.t ->
    block_subst:Substitution.t ->
    stack_subst:Substitution.t ->
    Cfg.basic_block ->
    Instruction.t DLL.cell option ->
    Reg.Set.t ->
    unit =
 fun state ~block_subst ~stack_subst block cell live_at_destruction_point ->
  insert_spills_or_reloads_in_block state ~make_spill_or_reload:make_spill
    ~occur_check:(fun instr reg ->
      (* We assume `new_reg` has no location yet (we are before register
         allocation, but selection uses fixed registers in various places). If
         the assertion does not hold, we need to look at the registers destroyed
         by the instruction. *)
      assert (Reg.is_unknown reg);
      occurs_array instr.res reg)
    ~insert:DLL.insert_after
    ~copy_default:
      (match DLL.hd block.body with
      | None -> dummy_instr_of_terminator block.terminator
      | Some hd -> hd)
    ~add_default:DLL.add_begin ~move_cell:DLL.prev ~block_subst ~stack_subst
    block cell live_at_destruction_point

(* Inserts spills in all blocks. *)
(* CR mshinwell: Add special handling for [Iname_for_debugger] (see
   [Spill.add_spills]). *)
let insert_spills :
    State.t -> Cfg_with_infos.t -> Substitution.map -> Substitution.t =
 fun state cfg_with_infos substs ->
  if split_debug then log ~indent:0 "insert_spills";
  let destructions_at_end = State.destructions_at_end state in
  let stack_subst = Reg.Tbl.create (Label.Map.cardinal destructions_at_end) in
  Label.Map.iter
    (fun label (_, live_at_destruction_point) ->
      if split_debug then log ~indent:1 "block %d" label;
      let block = Cfg_with_infos.get_block_exn cfg_with_infos label in
      let block_subst = Substitution.for_label substs label in
      insert_spills_in_block state ~block_subst ~stack_subst block
        (DLL.last_cell block.body) live_at_destruction_point)
    destructions_at_end;
  stack_subst

(* Creates a reload instruction for the register named `old_reg` before
   substitution and `new_reg` after substitution, copying debug and FDO
   information from `copy`, and getting the stack slot from `stack_subst` if
   `old_reg` is mapped. *)
let make_reload : type a. a make_operation =
 fun state ~stack_subst ~old_reg ~new_reg ~copy ->
  let stack_reg : Reg.t =
    match Reg.Tbl.find_opt stack_subst old_reg with
    | Some stack_reg -> stack_reg
    | None ->
      let slots = State.stack_slots state in
      let slot = Regalloc_stack_slots.get_or_create slots old_reg in
      let stack =
        make_temporary ~same_class_and_base_name_as:old_reg ~name_prefix:"stack"
      in
      Regalloc_stack_slots.use_same_slot_or_fatal slots stack ~existing:old_reg;
      Regalloc_stack_slots.use_same_slot_or_fatal slots stack ~existing:new_reg;
      stack.Reg.loc <- Reg.(Stack (Local slot));
      stack
  in
  if split_debug
  then
    log ~indent:3 "reload %a -> %a" Printmach.reg stack_reg Printmach.reg
      new_reg;
  Move.make_instr Move.Load
    ~id:(State.get_and_incr_instruction_id state)
    ~copy ~from:stack_reg ~to_:new_reg

(* Inserts the relaods in a block, as late as possible (i.e. immediately before
   the register is first read), to reduce live ranges. *)
let insert_reloads_in_block :
    State.t ->
    block_subst:Substitution.t ->
    stack_subst:Substitution.t ->
    Cfg.basic_block ->
    Instruction.t DLL.cell option ->
    Reg.Set.t ->
    unit =
 fun state ~block_subst ~stack_subst block cell live_at_definition_point ->
  insert_spills_or_reloads_in_block state ~make_spill_or_reload:make_reload
    ~occur_check:(fun instr reg -> occurs_array instr.arg reg)
    ~insert:DLL.insert_before
    ~copy_default:(dummy_instr_of_terminator block.terminator)
    ~add_default:DLL.add_end ~move_cell:DLL.next ~block_subst ~stack_subst block
    cell live_at_definition_point

(* Inserts reloads in all blocks. *)
let insert_reloads :
    State.t -> Cfg_with_infos.t -> Substitution.map -> Substitution.t -> unit =
 fun state cfg_with_infos substs stack_subst ->
  if split_debug then log ~indent:0 "insert_reloads";
  Label.Map.iter
    (fun label live_at_definition_point ->
      if split_debug then log ~indent:1 "block %d" label;
      let block = Cfg_with_infos.get_block_exn cfg_with_infos label in
      let block_subst = Substitution.for_label substs label in
      insert_reloads_in_block state ~block_subst ~stack_subst block
        (DLL.hd_cell block.body) live_at_definition_point)
    (State.definitions_at_beginning state)

let add_phi_moves_to_instr_list :
    State.t ->
    before:Cfg.basic_block ->
    phi:Cfg.basic_block ->
    Substitution.map ->
    Reg.Set.t ->
    Cfg.basic_instruction_list ->
    unit =
 fun state ~before ~phi substs to_unify instrs ->
  let before_subst = Substitution.for_label substs before.start in
  let phi_subst = Substitution.for_label substs phi.start in
  Reg.Set.iter
    (fun reg ->
      let from = Substitution.apply_reg before_subst reg in
      let to_ = Substitution.apply_reg phi_subst reg in
      match Reg.same from to_ with
      | true ->
        if split_debug
        then
          log ~indent:3 "(no phi necessary because %a == %a)" Printmach.reg from
            Printmach.reg to_
      | false ->
        if split_debug
        then log ~indent:3 "phi %a -> %a" Printmach.reg from Printmach.reg to_;
        let phi_move =
          Move.make_instr Move.Plain
            ~id:(State.get_and_incr_instruction_id state)
            ~copy:before.terminator ~from ~to_
        in
        DLL.add_end instrs phi_move)
    to_unify

(* Insert phi moves: - to the predecessor block if the edge is an "always" one;
   - to a newly-inserted block otherwise. Returns `true` iff at least one block
   was inserted. *)
let insert_phi_moves : State.t -> Cfg_with_infos.t -> Substitution.map -> bool =
 fun state cfg_with_infos substs ->
  let block_inserted = ref false in
  Label.Map.iter
    (fun label to_unify ->
      let block = Cfg_with_infos.get_block_exn cfg_with_infos label in
      if split_debug
      then
        log ~indent:1 "insert_phi_moves for block %d: %a" label Printmach.regset
          to_unify;
      if block.is_trap_handler then fatal "phi block %d is a trap handler" label;
      Label.Set.iter
        (fun predecessor_label ->
          let predecessor_block =
            Cfg_with_infos.get_block_exn cfg_with_infos predecessor_label
          in
          match predecessor_block.terminator.desc with
          | Return | Raise _ | Tailcall_func _ | Call_no_return _ | Never ->
            assert false
          | Tailcall_self _ -> ()
          | Always _ ->
            add_phi_moves_to_instr_list state ~before:predecessor_block
              ~phi:block substs to_unify predecessor_block.body
          | Switch _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
          | Call _ | Prim _ | Specific_can_raise _ ->
            let instrs = DLL.make_empty () in
            add_phi_moves_to_instr_list state ~before:predecessor_block
              ~phi:block substs to_unify instrs;
            let inserted_blocks =
              insert_block (Cfg_with_infos.cfg_with_layout cfg_with_infos)
                instrs ~after:predecessor_block ~before:(Some block)
                ~next_instruction_id:(fun () ->
                  State.get_and_incr_instruction_id state)
            in
            block_inserted := true;
            if split_debug && Lazy.force split_invariants
            then (
              (match inserted_blocks with
              | [inserted_block] ->
                let inserted_label = inserted_block.start in
                if not (Label.Set.mem inserted_label block.predecessors)
                then fatal "inserted block is not a predecessor";
                if not
                     (Label.Set.mem inserted_label
                        (Cfg.successor_labels ~normal:true ~exn:false
                           inserted_block))
                then fatal "inserted block not a normal successor";
                if Label.Set.mem inserted_label
                     (Cfg.successor_labels ~normal:false ~exn:true
                        inserted_block)
                then fatal "inserted block an exceptional successor"
              | [] -> fatal "no block was inserted"
              | _ :: _ :: _ -> fatal "several blocks were inserted");
              ()))
        block.predecessors)
    (State.phi_at_beginning state);
  !block_inserted

let split_at_destruction_points :
    Cfg_with_infos.t -> cfg_infos -> (Regalloc_stack_slots.t * bool) option =
 fun cfg_with_infos cfg_infos ->
  if split_debug
  then (
    log ~indent:0 "split_at_destruction_points";
    Regalloc_irc_utils.log_cfg_with_infos ~indent:1 cfg_with_infos);
  let state =
    Profile.record ~accumulate:true "state"
      (fun () ->
        State.make cfg_with_infos
          ~next_instruction_id:(succ cfg_infos.max_instruction_id))
      ()
  in
  if split_debug
  then (
    let doms = Cfg_with_infos.dominators cfg_with_infos in
    log_dominance_frontier ~indent:1 (Cfg_with_infos.cfg cfg_with_infos) doms;
    log_dominator_forest ~indent:1 (Cfg_dominators.dominator_forest doms));
  match Label.Map.is_empty (State.definitions_at_beginning state) with
  | true ->
    log ~indent:1 "renaming_infos is empty (no new names introduced)";
    None
  | false ->
    if split_debug then State.log_renaming_info ~indent:1 state;
    let substs =
      Profile.record ~accumulate:true "compute_substitutions"
        (fun () -> compute_substitutions state cfg_with_infos)
        ()
    in
    if split_debug then log_substitutions ~indent:1 substs;
    Profile.record ~accumulate:true "apply_substitutions"
      (fun () -> apply_substitutions cfg_with_infos substs)
      ();
    let stack_subst =
      Profile.record ~accumulate:true "insert_spills"
        (fun () -> insert_spills state cfg_with_infos substs)
        ()
    in
    if split_debug then log_stack_subst ~indent:1 stack_subst;
    Profile.record ~accumulate:true "insert_reloads"
      (fun () -> insert_reloads state cfg_with_infos substs stack_subst)
      ();
    let block_inserted =
      Profile.record ~accumulate:true "insert_phi_moves"
        (fun () -> insert_phi_moves state cfg_with_infos substs)
        ()
    in
    if split_debug
    then Regalloc_irc_utils.log_cfg_with_infos ~indent:1 cfg_with_infos;
    Some (State.stack_slots state, block_inserted)

let split_live_ranges : Cfg_with_infos.t -> cfg_infos -> Regalloc_stack_slots.t
    =
 fun cfg_with_infos cfg_infos ->
  (* CR-soon xclerc for xclerc: support closure, flambda, and
     flambda2/classic *)
  (match Config.flambda, Config.flambda2 with
  | false, false -> fatal "Regalloc_split: closure is currently not supported"
  | true, false -> fatal "Regalloc_split: flambda is currently not supported"
  | false, true ->
    (* note: classic mode is not properly supported, but is used in the
       "tests/backend/frame-too-long" tests. We now implicitly disable split (in
       `Regalloc_rewrite.prelude`) if classic mode is enabled. *)
    (* if Flambda2_ui.Flambda_features.classic_mode () then fatal
       "Regalloc_split: classic mode is currently not supported" *)
    ()
  | true, true -> assert false);
  match split_at_destruction_points cfg_with_infos cfg_infos with
  | None -> Regalloc_stack_slots.make ()
  | Some (stack_slots, block_inserted) ->
    Cfg_with_infos.invalidate_liveness cfg_with_infos;
    if block_inserted
    then Cfg_with_infos.invalidate_dominators_and_loop_infos cfg_with_infos;
    let (_ : Cfg_with_infos.t) =
      Profile.record ~accumulate:true "cfg_deadcode" Cfg_deadcode.run
        cfg_with_infos
    in
    stack_slots
