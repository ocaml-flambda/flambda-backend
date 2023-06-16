[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
open! Regalloc_split_utils
module DLL = Flambda_backend_utils.Doubly_linked_list

type destructions_at_end = (destruction_kind * Reg.Set.t) Label.Map.t

type definitions_at_beginning = Reg.Set.t Label.Map.t

type phi_at_beginning = Reg.Set.t Label.Map.t

type t =
  { dominators : Cfg_dominators.t;
    destructions_at_end : destructions_at_end;
    definitions_at_beginning : definitions_at_beginning;
    phi_at_beginning : phi_at_beginning;
    stack_slots : Regalloc_stack_slots.t;
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

(* Optimizes `destructions_at_end` and `definitions_at_beginning`, by deleting
   registers that appear in both for a given block if the registers are not used
   in said block. *)
module RemoveReloadSpillInSameBlock : sig
  val optimize :
    Cfg_with_liveness.t ->
    destructions_at_end:destructions_at_end ->
    definitions_at_beginning:definitions_at_beginning ->
    destructions_at_end * definitions_at_beginning
end = struct
  let optimize cfg_with_liveness ~destructions_at_end ~definitions_at_beginning
      =
    if split_debug then log ~indent:0 "RemoveReloadSpillInSameBlock.optimize";
    let destructions_at_end = ref destructions_at_end in
    let definitions_at_beginning =
      Label.Map.mapi
        (fun (label : Label.t) (definitions : Reg.Set.t) ->
          if split_debug then log ~indent:1 "block %d" label;
          match Label.Map.find_opt label !destructions_at_end with
          | None -> definitions
          | Some (Destruction_only_on_exceptional_path, _) -> definitions
          | Some (Destruction_on_all_paths, destructions) -> (
            if split_debug
            then (
              log ~indent:2 "definitions: %a" Printmach.regset definitions;
              log ~indent:2 "destructions: %a" Printmach.regset destructions);
            let can_be_removed : Reg.Set.t =
              Reg.Set.filter
                (fun (reg : Reg.t) ->
                  if split_debug
                  then log ~indent:3 "register %a" Printmach.reg reg;
                  match Reg.Set.mem reg destructions with
                  | false ->
                    if split_debug then log ~indent:3 "(not among destructions)";
                    false
                  | true ->
                    let block =
                      Cfg_with_liveness.get_block_exn cfg_with_liveness label
                    in
                    let occurs = occurs_block block reg in
                    if split_debug then log ~indent:3 "occurs? %B" occurs;
                    not occurs)
                definitions
            in
            if split_debug
            then
              log ~indent:2 "can be removed: %a" Printmach.regset can_be_removed;
            match Reg.Set.is_empty can_be_removed with
            | true -> definitions
            | false ->
              destructions_at_end
                := Label.Map.update label
                     (function
                       | None -> assert false
                       | Some (kind, set) ->
                         Some (kind, Reg.Set.diff set can_be_removed))
                     !destructions_at_end;
              Reg.Set.diff definitions can_be_removed))
        definitions_at_beginning
    in
    !destructions_at_end, definitions_at_beginning
end

(* Optimizes `destructions_at_end` by avoiding the repeated spill of a constant.
   If a register is set only once, then all spills of that register dominated by
   a spill of the same register can be deleted. *)
module RemoveDominatedSpillsForConstants : sig
  val optimize :
    Cfg_with_liveness.t ->
    Cfg_dominators.t ->
    destructions_at_end:destructions_at_end ->
    destructions_at_end
end = struct
  (* CR-someday xclerc for xclerc: move to Cfg_loop_infos? *)
  let is_in_loop : Cfg_loop_infos.t -> Label.t -> bool =
   fun loops label ->
    Cfg_loop_infos.EdgeMap.exists
      (fun _ (loop : Cfg_loop_infos.loop) -> Label.Set.mem label loop)
      loops.loops

  type set =
    | At_most_once
    | Maybe_more_than_once

  let string_of_set = function
    | At_most_once -> "at most once"
    | Maybe_more_than_once -> "maybe more than once"

  let rec remove_dominated_spills :
      Cfg_dominators.t ->
      Cfg_dominators.dominator_tree ->
      num_sets:set Reg.Tbl.t ->
      already_spilled:Label.t Reg.Map.t ->
      destructions_at_end:destructions_at_end ->
      destructions_at_end =
   fun doms tree ~num_sets ~already_spilled ~destructions_at_end ->
    if split_debug then log ~indent:1 "remove_dominated_spills %d" tree.label;
    let already_spilled = ref already_spilled in
    let destructions_at_end : (destruction_kind * Reg.Set.t) Label.Map.t =
      Label.Map.update tree.label
        (function
          | Some (Destruction_on_all_paths, destroyed) ->
            let destroyed : Reg.Set.t =
              Reg.Set.filter
                (fun (reg : Reg.t) ->
                  if split_debug
                  then log ~indent:2 "register %a" Printmach.reg reg;
                  let keep =
                    match Reg.Tbl.find_opt num_sets reg with
                    | None | Some Maybe_more_than_once -> true
                    | Some At_most_once -> (
                      match Reg.Map.find_opt reg !already_spilled with
                      | None ->
                        if split_debug then log ~indent:3 "case/2";
                        true
                      | Some spilled_at ->
                        if split_debug && Lazy.force split_invariants
                        then
                          if not
                               (Cfg_dominators.is_strictly_dominating
                                  doms.dominators spilled_at tree.label)
                          then fatal "inconsistent dominator tree";
                        if split_debug
                        then
                          log ~indent:3 "case/3 (already spilled at %d)"
                            spilled_at;
                        false)
                  in
                  if keep
                  then
                    already_spilled
                      := Reg.Map.add reg tree.label !already_spilled;
                  if split_debug then log ~indent:3 "keep? %B" keep;
                  keep)
                destroyed
            in
            if Reg.Set.is_empty destroyed
            then (
              if split_debug then log ~indent:3 "(the destroyed set is empty)";
              None)
            else Some (Destruction_on_all_paths, destroyed)
          | Some (Destruction_only_on_exceptional_path, _) as existing ->
            if split_debug
            then log ~indent:2 "(ignored as a half-destruction point)";
            existing
          | None ->
            if split_debug then log ~indent:2 "(not a destruction point)";
            None)
        destructions_at_end
    in
    List.fold_left tree.children ~init:destructions_at_end
      ~f:(fun destructions_at_end (child : Cfg_dominators.dominator_tree) ->
        if split_debug then log ~indent:2 "child %d" child.label;
        remove_dominated_spills doms child ~num_sets
          ~already_spilled:!already_spilled ~destructions_at_end)

  let optimize cfg_with_liveness doms ~destructions_at_end =
    if split_debug
    then log ~indent:0 "RemoveDominatedSpillsForConstants.optimize";
    let loops =
      (* CR-soon xclerc for xclerc: be sure to not duplicate this computation if
         for instance used to compute spilling costs. *)
      Cfg_loop_infos.build (Cfg_with_liveness.cfg cfg_with_liveness) doms
    in
    let incr_set (tbl : set Reg.Tbl.t) (arr : Reg.t array) ~(in_loop : bool) :
        unit =
      Array.iter arr ~f:(fun (reg : Reg.t) ->
          match Reg.Tbl.find_opt tbl reg with
          | None ->
            Reg.Tbl.replace tbl reg
              (if in_loop then Maybe_more_than_once else At_most_once)
          | Some At_most_once -> Reg.Tbl.replace tbl reg Maybe_more_than_once
          | Some Maybe_more_than_once -> ())
    in
    let num_sets =
      Cfg_with_liveness.fold_blocks cfg_with_liveness ~init:(Reg.Tbl.create 123)
        ~f:(fun label block acc ->
          let in_loop : bool = is_in_loop loops label in
          if split_debug then log ~indent:1 "block %d in_loop? %B" label in_loop;
          incr_set acc block.terminator.res ~in_loop;
          DLL.iter block.body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
              incr_set acc instr.res ~in_loop);
          acc)
    in
    if split_debug
    then (
      log ~indent:1 "num_sets:";
      Reg.Tbl.iter
        (fun reg num_set ->
          log ~indent:2 "%a ~> %s" Printmach.reg reg (string_of_set num_set))
        num_sets);
    remove_dominated_spills doms doms.Cfg_dominators.dominator_tree ~num_sets
      ~already_spilled:Reg.Map.empty ~destructions_at_end
end

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
  Cfg_with_liveness.fold_blocks cfg_with_liveness ~init:Label.Map.empty
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
    Cfg_with_liveness.fold_blocks cfg_with_liveness
      ~init:definitions_at_beginning
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
            Cfg_with_liveness.get_block_exn cfg_with_liveness destruction_label
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
                    Cfg_with_liveness.get_block_exn cfg_with_liveness
                      successor_label
                  in
                  let predecessor_labels = successor_block.predecessors in
                  let all_predecessors_end_with_destruction_point : bool =
                    Label.Set.for_all
                      (fun predecessor_label ->
                        let predecessor_block =
                          Cfg_with_liveness.get_block_exn cfg_with_liveness
                            predecessor_label
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
        (Cfg_with_liveness.get_block_exn cfg_with_liveness
           destruction_frontier_label)
          .is_trap_handler
      in
      (* If the block is a trap handler, it has definitions at beginning since
         we are introducing new names - hence no phi. *)
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
            (Cfg_with_liveness.get_block_exn cfg_with_liveness flab)
              .is_trap_handler
          in
          if not is_trap_handler
          then
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
  let destructions_at_end, definitions_at_beginning =
    RemoveReloadSpillInSameBlock.optimize cfg_with_liveness ~destructions_at_end
      ~definitions_at_beginning
  in
  let destructions_at_end =
    RemoveDominatedSpillsForConstants.optimize cfg_with_liveness dominators
      ~destructions_at_end
  in
  let stack_slots = Regalloc_stack_slots.make () in
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
