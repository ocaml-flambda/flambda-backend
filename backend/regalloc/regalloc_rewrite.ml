[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils
module DLL = Flambda_backend_utils.Doubly_linked_list
module Substitution = Regalloc_substitution

module type State = sig
  type t

  val stack_slots : t -> Regalloc_stack_slots.t

  val get_and_incr_instruction_id : t -> InstructionId.t
end

module type Utils = sig
  val log : ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a

  val indent : unit -> unit

  val dedent : unit -> unit

  val log_body_and_terminator :
    Cfg.basic_instruction_list ->
    Cfg.terminator Cfg.instruction ->
    liveness ->
    unit
end

type direction =
  | Load_before_cell of Cfg.basic Cfg.instruction DLL.cell
  | Store_after_cell of Cfg.basic Cfg.instruction DLL.cell
  | Load_after_list of Cfg.basic_instruction_list
  | Store_before_list of Cfg.basic_instruction_list

(* Applies an optimization on the CFG outputted by [rewrite_gen] having one
   temporary per variable per block rather than one per use of the variable,
   reducing the number of spills and reloads needed for variables used multiple
   times in a block. It iterates over each block and builds a substitution from
   the first used temporary for each variable to all the other temporaries used
   later for that variable, deleting now redundant reload/spill instructions
   along the way.

   This optimization is unsound when spilled nodes are used directly in
   instructions (if allowed by the ISA) without a new temporary being created
   (hence stack operands are not used in [rewrite_gen] if this optimization is
   enabled). Spills in blocks inserted by [rewrite_gen] (due to spills in block
   terminators) do not share temporaries with the body of the original block and
   are hence not considered in this optimization.

   No new temporaries are created by this optimization. Some temporaries are
   promoted to block temporaries (and so moved from the list of new instruction
   temporaries to the list of new block temporaries). Instruction temporaries
   that are now redundant (due to being replaced by block temporaries) are
   removed from the list of new instruction temporaries. *)
let coalesce_temp_spills_and_reloads (block : Cfg.basic_block)
    ~new_inst_temporaries ~new_block_temporaries =
  (* CR-soon mitom: Avoid cases where optimisation worsens spills and reloads
     due to assigning block temporaries for spilled registers that have live
     ranges interfering with things that have already been register allocated *)
  let removed_inst_temporaries = Reg.Tbl.create 128 in
  let var_to_block_temp = Reg.Tbl.create 8 in
  let replacements = Reg.Tbl.create 8 in
  let last_spill = Reg.Tbl.create 8 in
  let replace to_replace replace_with =
    if not (Reg.same to_replace replace_with)
    then Reg.Tbl.add replacements to_replace replace_with
  in
  let update_info_using_inst (inst_cell : Cfg.basic Cfg.instruction DLL.cell) =
    let inst = DLL.value inst_cell in
    match[@ocaml.warning "-4"] inst.desc with
    | Op Reload -> (
      let var = inst.arg.(0) in
      let temp = inst.res.(0) in
      match Reg.Tbl.find_opt var_to_block_temp var with
      | None -> Reg.Tbl.add var_to_block_temp var temp
      | Some block_temp ->
        DLL.delete_curr inst_cell;
        replace temp block_temp)
    | Op Spill -> (
      let var = inst.res.(0) in
      let temp = inst.arg.(0) in
      (match Reg.Tbl.find_opt last_spill var with
      | None -> ()
      | Some prev_inst_cell -> DLL.delete_curr prev_inst_cell);
      Reg.Tbl.replace last_spill var inst_cell;
      match Reg.Tbl.find_opt var_to_block_temp var with
      | None -> Reg.Tbl.add var_to_block_temp var temp
      | Some block_temp -> replace temp block_temp)
    | _ -> ()
  in
  DLL.iter_cell block.body ~f:update_info_using_inst;
  if Reg.Tbl.length replacements <> 0
  then (
    Substitution.apply_block_in_place replacements block;
    Reg.Tbl.iter
      (fun temp block_temp ->
        Reg.Tbl.replace removed_inst_temporaries temp ();
        Reg.Tbl.replace removed_inst_temporaries block_temp ();
        new_block_temporaries := block_temp :: !new_block_temporaries)
      replacements);
  new_inst_temporaries
    := List.filter
         ~f:(fun temp -> not (Reg.Tbl.mem removed_inst_temporaries temp))
         !new_inst_temporaries

type move_kind =
  | Load
  | Store

let equal_move_kind left right =
  match left, right with
  | Load, Load -> true
  | Store, Store -> true
  | (Load | Store), _ -> false

let rewrite_gen :
    type s.
    (module State with type t = s) ->
    (module Utils with type state = s) ->
    s ->
    Cfg_with_infos.t ->
    spilled_nodes:Reg.t list ->
    block_temporaries:bool ->
    Reg.t list * Reg.t list * bool =
 fun (module State : State with type t = s) (module Utils) state cfg_with_infos
     ~spilled_nodes ~block_temporaries ->
  let should_coalesce_temp_spills_and_reloads =
    Lazy.force Regalloc_utils.block_temporaries && block_temporaries
  in
  if debug
  then (
    Utils.log "rewrite";
    Utils.indent ());
  let block_insertion = ref false in
  let spilled_map : Reg.t Reg.Tbl.t =
    List.fold_left spilled_nodes ~init:(Reg.Tbl.create 17)
      ~f:(fun spilled_map reg ->
        let spilled = Reg.create reg.Reg.typ in
        (* for printing *)
        spilled.Reg.raw_name <- reg.Reg.raw_name;
        let slot =
          Regalloc_stack_slots.get_or_create (State.stack_slots state) reg
        in
        spilled.Reg.loc <- Reg.(Stack (Local slot));
        if debug
        then Utils.log "spilling %a to %a" Printreg.reg reg Printreg.reg spilled;
        Reg.Tbl.replace spilled_map reg spilled;
        spilled_map)
  in
  let is_spilled reg = Reg.Tbl.mem spilled_map reg in
  let new_inst_temporaries : Reg.t list ref = ref [] in
  let new_block_temporaries = ref [] in
  let make_new_temporary ~(move : Move.t) (reg : Reg.t) : Reg.t =
    let res =
      make_temporary ~same_class_and_base_name_as:reg ~name_prefix:"temp"
    in
    new_inst_temporaries := res :: !new_inst_temporaries;
    if debug
    then
      Utils.log "adding temporary %a (to %s %a)" Printreg.reg res
        (Move.to_string move) Printreg.reg reg;
    res
  in
  let[@inline] array_contains_spilled (arr : Reg.t array) : bool =
    let len = Array.length arr in
    let i = ref 0 in
    while !i < len && not (is_spilled (Array.unsafe_get arr !i)) do
      incr i
    done;
    !i < len
  in
  let[@inline] instruction_contains_spilled (instr : _ Cfg.instruction) : bool =
    array_contains_spilled instr.arg || array_contains_spilled instr.res
  in
  let rewrite_instruction ~(direction : direction)
      ~(sharing : (Reg.t * move_kind) Reg.Tbl.t) (instr : _ Cfg.instruction) :
      unit =
    let[@inline] rewrite_reg (reg : Reg.t) : Reg.t =
      if is_spilled reg
      then (
        let spilled =
          match Reg.Tbl.find_opt spilled_map reg with
          | None -> assert false
          | Some r -> r
        in
        let move, move_dir =
          match direction with
          | Load_before_cell _ | Load_after_list _ -> Move.Load, Load
          | Store_after_cell _ | Store_before_list _ -> Move.Store, Store
        in
        let add_instr, temp =
          match Reg.Tbl.find_opt sharing reg with
          | None ->
            let new_temp = make_new_temporary ~move reg in
            Reg.Tbl.add sharing reg (new_temp, move_dir);
            true, new_temp
          | Some (r, dir) -> not (equal_move_kind dir move_dir), r
        in
        (if add_instr
        then
          let from, to_ =
            match move_dir with Load -> spilled, temp | Store -> temp, spilled
          in
          let new_instr =
            Move.make_instr move
              ~id:(State.get_and_incr_instruction_id state)
              ~copy:instr ~from ~to_
          in
          match direction with
          | Load_before_cell cell -> DLL.insert_before cell new_instr
          | Store_after_cell cell -> DLL.insert_after cell new_instr
          | Load_after_list list -> DLL.add_end list new_instr
          | Store_before_list list -> DLL.add_begin list new_instr);
        temp)
      else reg
    in
    let rewrite_array (arr : Reg.t array) : unit =
      let len = Array.length arr in
      for i = 0 to pred len do
        let reg = Array.unsafe_get arr i in
        Array.unsafe_set arr i (rewrite_reg reg)
      done
    in
    match direction with
    | Load_before_cell _ | Load_after_list _ -> rewrite_array instr.arg
    | Store_after_cell _ | Store_before_list _ -> rewrite_array instr.res
  in
  let liveness = Cfg_with_infos.liveness cfg_with_infos in
  Cfg.iter_blocks (Cfg_with_infos.cfg cfg_with_infos) ~f:(fun label block ->
      if debug
      then (
        Utils.log "body of #%a, before:" Label.format label;
        Utils.indent ();
        Utils.log_body_and_terminator block.body block.terminator liveness;
        Utils.dedent ());
      let block_rewritten = ref false in
      DLL.iter_cell block.body ~f:(fun cell ->
          let instr = DLL.value cell in
          if instruction_contains_spilled instr
          then
            (* CR-soon mitom: Use stack operands regardless of whether
               coalescing temporaries when it allows using the memory address of
               a variable used exactly once in a block directly in an
               instruction. Currently, if the "block" temporary for this
               variable is register allocated, an extra spill/reload instruction
               is added compared to using it directly in the instruction (if
               possible).

               For variables used 2+ times in the block, short circuiting here
               is fine. If the block temporary we create gets register
               allocated, then that is better than using stack operands to use
               the memory address directly in the instruction. If the block
               temporary is spilled, stack operands will apply to it in the next
               round in the same way it would have done to the original
               variable. *)
            if should_coalesce_temp_spills_and_reloads
               || Regalloc_utils.equal_stack_operands_rewrite
                    (Regalloc_stack_operands.basic spilled_map instr)
                    May_still_have_spilled_registers
            then (
              block_rewritten := true;
              let sharing = Reg.Tbl.create 8 in
              rewrite_instruction ~direction:(Load_before_cell cell) ~sharing
                instr;
              rewrite_instruction ~direction:(Store_after_cell cell) ~sharing
                instr));
      if instruction_contains_spilled block.terminator
      then
        (* CR-soon mitom: Same issue as short circuiting in basic instruction
           rewriting *)
        if should_coalesce_temp_spills_and_reloads
           || Regalloc_utils.equal_stack_operands_rewrite
                (Regalloc_stack_operands.terminator spilled_map block.terminator)
                May_still_have_spilled_registers
        then (
          block_rewritten := true;
          let sharing = Reg.Tbl.create 8 in
          rewrite_instruction ~direction:(Load_after_list block.body)
            ~sharing:(Reg.Tbl.create 8) block.terminator;
          let new_instrs = DLL.make_empty () in
          rewrite_instruction ~direction:(Store_before_list new_instrs) ~sharing
            block.terminator;
          if not (DLL.is_empty new_instrs)
          then
            (* insert block *)
            let (_ : Cfg.basic_block list) =
              Regalloc_utils.insert_block
                (Cfg_with_infos.cfg_with_layout cfg_with_infos)
                new_instrs ~after:block ~before:None
                ~next_instruction_id:(fun () ->
                  State.get_and_incr_instruction_id state)
            in
            block_insertion := true);
      if !block_rewritten && should_coalesce_temp_spills_and_reloads
      then
        coalesce_temp_spills_and_reloads block ~new_inst_temporaries
          ~new_block_temporaries;
      if debug
      then (
        Utils.log "and after:";
        Utils.indent ();
        Utils.log_body_and_terminator block.body block.terminator liveness;
        Utils.dedent ();
        Utils.log "end"));
  if debug then Utils.dedent ();
  !new_inst_temporaries, !new_block_temporaries, !block_insertion

(* CR-soon xclerc for xclerc: investigate exactly why this threshold is
   necessary. *)
(* If the number of temporaries is above this value, do not split/rename.
   Experimentally, it seems to trigger a pathological behaviour of IRC when
   above. *)
let threshold_split_live_ranges = 1024

let prelude :
    (module Utils) ->
    on_fatal_callback:(unit -> unit) ->
    Cfg_with_infos.t ->
    cfg_infos * Regalloc_stack_slots.t =
 fun (module Utils) ~on_fatal_callback cfg_with_infos ->
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  on_fatal ~f:on_fatal_callback;
  if debug
  then Utils.log "run (%S)" (Cfg_with_layout.cfg cfg_with_layout).fun_name;
  Reg.reinit ();
  if debug && Lazy.force invariants
  then (
    Utils.log "precondition";
    Regalloc_invariants.precondition cfg_with_layout);
  let cfg_infos = collect_cfg_infos cfg_with_layout in
  let num_temporaries =
    (* note: this should probably be `Reg.Set.cardinal (Reg.Set.union
       cfg_infos.arg cfg_infos.res)` but the following experimentally produces
       the same results without computing the union. *)
    Reg.Set.cardinal cfg_infos.arg
  in
  if debug then Utils.log "#temporaries(before):%d" num_temporaries;
  if num_temporaries >= threshold_split_live_ranges
     || Flambda2_ui.Flambda_features.classic_mode ()
  then cfg_infos, Regalloc_stack_slots.make ()
  else if Lazy.force Regalloc_split_utils.split_live_ranges
  then
    let stack_slots =
      Profile.record ~accumulate:true "split"
        (fun () -> Regalloc_split.split_live_ranges cfg_with_infos cfg_infos)
        ()
    in
    let cfg_infos = collect_cfg_infos cfg_with_layout in
    cfg_infos, stack_slots
  else cfg_infos, Regalloc_stack_slots.make ()

let postlude :
    type s.
    (module State with type t = s) ->
    (module Utils) ->
    s ->
    f:(unit -> unit) ->
    Cfg_with_infos.t ->
    unit =
 fun (module State : State with type t = s) (module Utils) state ~f
     cfg_with_infos ->
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  (* note: slots need to be updated before prologue removal *)
  Profile.record ~accumulate:true "stack_slots_optimize"
    (fun () ->
      Regalloc_stack_slots.optimize (State.stack_slots state) cfg_with_infos)
    ();
  Regalloc_stack_slots.update_cfg_with_layout (State.stack_slots state)
    cfg_with_layout;
  if debug
  then (
    Utils.indent ();
    Stack_class.Tbl.iter
      (Cfg_with_layout.cfg cfg_with_layout).fun_num_stack_slots
      ~f:(fun stack_class num_stack_slots ->
        Utils.log "stack_slots[%a]=%d" Stack_class.print stack_class
          num_stack_slots);
    Utils.dedent ());
  remove_prologue_if_not_required cfg_with_layout;
  update_live_fields cfg_with_layout (Cfg_with_infos.liveness cfg_with_infos);
  f ();
  if debug && Lazy.force invariants
  then (
    Utils.log "postcondition";
    Regalloc_invariants.postcondition_liveness cfg_with_infos)
