[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
open! Regalloc_split_utils

type destructions_at_end = (destruction_kind * Reg.Set.t) Label.Map.t

type definitions_at_beginning = Reg.Set.t Label.Map.t

type phi_at_beginning = Reg.Set.t Label.Map.t

type t =
  { dominators : Cfg_dominators.t;
    destructions_at_end : destructions_at_end;
    definitions_at_beginning : definitions_at_beginning;
    phi_at_beginning : phi_at_beginning;
    stack_slots : StackSlots.t;
    mutable next_instruction_id : Instruction.id
  }

let log_renaming_info : indent:int -> t -> unit =
 fun ~indent state ->
  log ~indent "renaming_infos";
  log ~indent:(indent + 1) "destructions:";
  Label.Map.iter
    (fun label (kind, regset) ->
      log ~indent:(indent + 2) " - end of block %d %s (%a)" label
        (match kind with
        | Destruction_on_all_paths -> "[all-paths]"
        | Destruction_only_on_exceptional_path -> "[exn-only]")
        Printmach.regset regset)
    state.destructions_at_end;
  log ~indent:(indent + 1) "definitions:";
  Label.Map.iter
    (fun label regset ->
      log ~indent:(indent + 2) " - beginning of block %d (%a)" label
        Printmach.regset regset)
    state.definitions_at_beginning;
  log ~indent:1 "phi:";
  Label.Map.iter
    (fun label regset ->
      log ~indent:(indent + 2) " - beginning of block %d (%a)" label
        Printmach.regset regset)
    state.phi_at_beginning

let add_destruction_point_at_end :
    Cfg_with_liveness.t ->
    Cfg.basic_block ->
    destruction_kind ->
    destructions_at_end ->
    destructions_at_end =
 fun cfg_with_liveness block kind destructions_at_end ->
  let destroyed_regs : Reg.Set.t =
    match kind with
    | Destruction_on_all_paths -> (
      match
        Cfg_with_liveness.liveness_find_opt cfg_with_liveness
          block.terminator.id
      with
      | None ->
        fatal
          "Regalloc_split_state.add_destruction_point_at_end: no liveness \
           information for block %d"
          block.start
      | Some { Cfg_liveness.across; before = _ } -> across)
    | Destruction_only_on_exceptional_path -> (
      match block.exn with
      | None ->
        fatal
          "Regalloc_split_state.add_destruction_point_at_end: no exceptional \
           successor for block %d"
          block.start
      | Some exn_succ -> live_at_block_beginning cfg_with_liveness exn_succ)
  in
  Label.Map.add block.start (kind, destroyed_regs) destructions_at_end

let compute_destructions : Cfg_with_liveness.t -> destructions_at_end =
 fun cfg_with_liveness ->
  fold_blocks cfg_with_liveness ~init:Label.Map.empty
    ~f:(fun _label block destructions_at_end ->
      match destruction_point_at_end block with
      | None -> destructions_at_end
      | Some kind ->
        add_destruction_point_at_end cfg_with_liveness block kind
          destructions_at_end)

(* Definitions are added at the beginning of exception handlers, and at the
   beginning of successors of destruction points. *)
let compute_definitions :
    Cfg_with_liveness.t ->
    destructions_at_end:destructions_at_end ->
    definitions_at_beginning =
 fun cfg_with_liveness ~destructions_at_end ->
  let definitions_at_beginning = Label.Map.empty in
  let definitions_at_beginning =
    fold_blocks cfg_with_liveness ~init:definitions_at_beginning
      ~f:(fun label block definitions_at_beginning ->
        if block.is_trap_handler
        then
          Label.Map.add label
            (live_at_block_beginning cfg_with_liveness label)
            definitions_at_beginning
        else definitions_at_beginning)
  in
  let definitions_at_beginning =
    Label.Map.fold
      (fun destruction_label (kind, _) definitions_at_beginning ->
        match kind with
        | Destruction_only_on_exceptional_path -> definitions_at_beginning
        | Destruction_on_all_paths ->
          let destruction_block =
            get_block_exn cfg_with_liveness destruction_label
          in
          let normal_successor_labels =
            Cfg.successor_labels destruction_block ~normal:true ~exn:false
          in
          let definitions_at_beginning =
            Label.Set.fold
              (fun successor_label definitions_at_beginning ->
                (if split_debug && Lazy.force split_invariants
                then
                  let successor_block =
                    get_block_exn cfg_with_liveness successor_label
                  in
                  let predecessor_labels = successor_block.predecessors in
                  let all_predecessors_end_with_destruction_point : bool =
                    Label.Set.for_all
                      (fun predecessor_label ->
                        let predecessor_block =
                          get_block_exn cfg_with_liveness predecessor_label
                        in
                        destruction_point_at_end predecessor_block
                        = Some Destruction_on_all_paths)
                      predecessor_labels
                  in
                  (* Should not happen since destruction points currently only
                     have one normal successor. *)
                  if not all_predecessors_end_with_destruction_point
                  then fatal "not all predecessors end with a destruction point");
                Label.Map.add successor_label
                  (live_at_block_beginning cfg_with_liveness successor_label)
                  definitions_at_beginning)
              normal_successor_labels definitions_at_beginning
          in
          definitions_at_beginning)
      destructions_at_end definitions_at_beginning
  in
  definitions_at_beginning

let add_phis :
    Cfg_with_liveness.t ->
    Label.t ->
    Reg.Set.t ->
    phi_at_beginning ->
    Cfg_dominators.t ->
    phi_at_beginning =
 fun cfg_with_liveness destruction_label destroyed_regs phi_at_beginning doms ->
  let destruction_frontier =
    match
      Label.Map.find_opt destruction_label
        doms.Cfg_dominators.dominance_frontiers
    with
    | None ->
      fatal "Regalloc_split_state.add_phis: no dominance frontier for block %d"
        destruction_label
    | Some frontier -> frontier
  in
  Label.Set.fold
    (fun destruction_frontier_label phi_at_beginning ->
      let is_trap_handler =
        (get_block_exn cfg_with_liveness destruction_frontier_label)
          .is_trap_handler
      in
      if is_trap_handler
      then phi_at_beginning
      else
        Label.Map.update destruction_frontier_label
          (function
            | None -> Some destroyed_regs
            | Some set -> Some (Reg.Set.union set destroyed_regs))
          phi_at_beginning)
    destruction_frontier phi_at_beginning

let rec fix_point_phi :
    Cfg_with_liveness.t ->
    Cfg_dominators.t ->
    phi_at_beginning ->
    phi_at_beginning =
 fun cfg_with_liveness doms phi_at_beginning ->
  let changed = ref false in
  let res = ref phi_at_beginning in
  Label.Map.iter
    (fun label regset ->
      let frontier : Label.Set.t =
        match Label.Map.find_opt label doms.dominance_frontiers with
        | None ->
          fatal
            "Regalloc_split_state.fix_point_phi: no dominance frontier for \
             block %d"
            label
        | Some f -> f
      in
      Label.Set.iter
        (fun flab ->
          let is_trap_handler =
            (get_block_exn cfg_with_liveness flab).is_trap_handler
          in
          if is_trap_handler
          then ()
          else
            res
              := Label.Map.update flab
                   (function
                     | None ->
                       changed := true;
                       Some regset
                     | Some old_set ->
                       let new_set = Reg.Set.union old_set regset in
                       if Reg.Set.cardinal old_set <> Reg.Set.cardinal new_set
                       then changed := true;
                       Some new_set)
                   !res)
        frontier)
    phi_at_beginning;
  if !changed
  then fix_point_phi cfg_with_liveness doms !res
  else phi_at_beginning

(* Phis are first added at the dominance frontier of blocks appearing in either
   `destructions_at_end` or `definitions_at_beginning`. And then also at the
   dominance frontier of blocks having phis, until a fix point is reached. *)
let compute_phis :
    Cfg_with_liveness.t ->
    destructions_at_end:destructions_at_end ->
    definitions_at_beginning:definitions_at_beginning ->
    Cfg_dominators.t ->
    phi_at_beginning =
 fun cfg_with_liveness ~destructions_at_end ~definitions_at_beginning doms ->
  let phi_at_beginning = Label.Map.empty in
  let phi_at_beginning =
    Label.Map.fold
      (fun destruction_label (kind, destroyed_regs) phi_at_beginning ->
        match kind with
        | Destruction_only_on_exceptional_path -> phi_at_beginning
        | Destruction_on_all_paths ->
          add_phis cfg_with_liveness destruction_label destroyed_regs
            phi_at_beginning doms)
      destructions_at_end phi_at_beginning
  in
  let phi_at_beginning =
    Label.Map.fold
      (fun destruction_label destroyed_regs phi_at_beginning ->
        add_phis cfg_with_liveness destruction_label destroyed_regs
          phi_at_beginning doms)
      definitions_at_beginning phi_at_beginning
  in
  let phi_at_beginning =
    fix_point_phi cfg_with_liveness doms phi_at_beginning
  in
  phi_at_beginning

let make cfg_with_liveness ~next_instruction_id =
  let dominators =
    Cfg_dominators.build (Cfg_with_liveness.cfg cfg_with_liveness)
  in
  let destructions_at_end = compute_destructions cfg_with_liveness in
  let definitions_at_beginning =
    compute_definitions cfg_with_liveness ~destructions_at_end
  in
  let phi_at_beginning =
    compute_phis cfg_with_liveness ~destructions_at_end
      ~definitions_at_beginning dominators
  in
  let stack_slots = StackSlots.make () in
  { dominators;
    destructions_at_end;
    definitions_at_beginning;
    phi_at_beginning;
    stack_slots;
    next_instruction_id
  }

let dominators state = state.dominators

let destructions_at_end state = state.destructions_at_end

let definitions_at_beginning state = state.definitions_at_beginning

let phi_at_beginning state = state.phi_at_beginning

let stack_slots state = state.stack_slots

let get_and_incr_instruction_id state =
  let res = state.next_instruction_id in
  state.next_instruction_id <- succ res;
  res
