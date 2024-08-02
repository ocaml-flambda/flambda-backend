[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
open! Cfg
module DLL = Flambda_backend_utils.Doubly_linked_list

module type State = sig
  type t

  val stack_slots : t -> Regalloc_stack_slots.t

  val get_and_incr_instruction_id : t -> Instruction.id

  val get_round_num : t -> int
end

module type Utils = sig
  val debug : bool

  val invariants : bool Lazy.t

  val log :
    indent:int -> ?no_eol:unit -> ('a, Format.formatter, unit) format -> 'a

  val log_body_and_terminator :
    indent:int ->
    Cfg.basic_instruction_list ->
    Cfg.terminator Cfg.instruction ->
    liveness ->
    unit

  val is_spilled : Reg.t -> bool

  val set_spilled : Reg.t -> unit
end

type direction =
  | Load_before_cell of Cfg.basic Cfg.instruction DLL.cell
  | Store_after_cell of Cfg.basic Cfg.instruction DLL.cell
  | Load_after_list of Cfg.basic_instruction_list
  | Store_before_list of Cfg.basic_instruction_list

let coalesce_temp_reloads cfg_with_infos new_temporaries =
  let coalesce_temp_reloads_per_block _ block =
    let var_to_temp = Reg.Tbl.create 8 in
    let subsitution = Reg.Tbl.create 8 in
    let redundant = ref [] in
    let update_info_using_inst inst_cell =
      let inst = DLL.value inst_cell in
      match inst.desc with
      | Op Reload -> (
        let var = inst.arg.(0) in
        let temp = inst.res.(0) in
        match Reg.Tbl.find_opt var_to_temp var with
        | None -> Reg.Tbl.add var_to_temp var temp
        | Some temp_to_use_for_all ->
          redundant := inst_cell :: !redundant;
          Reg.Tbl.add subsitution temp temp_to_use_for_all)
      | _ -> ()
    in
    DLL.iter_cell block.body ~f:update_info_using_inst;
    List.iter ~f:DLL.delete_curr !redundant;
    Substitution.apply_block_in_place subsitution block;
    new_temporaries
      := List.filter
           ~f:(fun reg -> not (Reg.Tbl.mem subsitution reg))
           !new_temporaries
  in
  Cfg_with_infos.cfg cfg_with_infos
  |> Cfg.iter_blocks ~f:coalesce_temp_reloads_per_block

let rewrite_gen :
    type s.
    (module State with type t = s) ->
    (module Utils) ->
    s ->
    Cfg_with_infos.t ->
    spilled_nodes:Reg.t list ->
    Reg.t list * bool =
 fun (module State : State with type t = s) (module Utils) state cfg_with_infos
     ~spilled_nodes ->
  if Utils.debug then Utils.log ~indent:1 "rewrite";
  let should_coalesce_temp_reloads = State.get_round_num state = 1 in
  let block_insertion = ref false in
  let spilled_map : Reg.t Reg.Tbl.t =
    List.fold_left spilled_nodes ~init:(Reg.Tbl.create 17)
      ~f:(fun spilled_map reg ->
        if Utils.debug then assert (Utils.is_spilled reg);
        let spilled = Reg.create reg.Reg.typ in
        if not should_coalesce_temp_reloads then Utils.set_spilled spilled;
        (* for printing *)
        if not (Reg.anonymous reg) then spilled.Reg.raw_name <- reg.Reg.raw_name;
        let slot =
          Regalloc_stack_slots.get_or_create (State.stack_slots state) reg
        in
        spilled.Reg.loc <- Reg.(Stack (Local slot));
        if Utils.debug
        then
          Utils.log ~indent:2 "spilling %a to %a" Printmach.reg reg
            Printmach.reg spilled;
        Reg.Tbl.replace spilled_map reg spilled;
        spilled_map)
  in
  let new_temporaries : Reg.t list ref = ref [] in
  let make_new_temporary ~(move : Move.t) (reg : Reg.t) : Reg.t =
    let res =
      make_temporary ~same_class_and_base_name_as:reg ~name_prefix:"temp"
    in
    new_temporaries := res :: !new_temporaries;
    if Utils.debug
    then
      Utils.log ~indent:2 "adding temporary %a (to %s %a)" Printmach.reg res
        (Move.to_string move) Printmach.reg reg;
    res
  in
  let[@inline] array_contains_spilled (arr : Reg.t array) : bool =
    let len = Array.length arr in
    let i = ref 0 in
    while !i < len && not (Utils.is_spilled (Array.unsafe_get arr !i)) do
      incr i
    done;
    !i < len
  in
  let[@inline] instruction_contains_spilled (instr : _ Cfg.instruction) : bool =
    array_contains_spilled instr.arg || array_contains_spilled instr.res
  in
  let rewrite_instruction ~(direction : direction)
      ~(sharing : (Reg.t * [`load | `store]) Reg.Tbl.t)
      (instr : _ Cfg.instruction) : unit =
    let[@inline] rewrite_reg (reg : Reg.t) : Reg.t =
      if Utils.is_spilled reg
      then (
        let spilled =
          match Reg.Tbl.find_opt spilled_map reg with
          | None -> assert false
          | Some r -> r
        in
        let move, move_dir =
          match direction with
          | Load_before_cell _ | Load_after_list _ -> Move.Load, `load
          | Store_after_cell _ | Store_before_list _ -> Move.Store, `store
        in
        let add_instr, temp =
          match Reg.Tbl.find_opt sharing reg with
          | None ->
            let new_temp = make_new_temporary ~move reg in
            Reg.Tbl.add sharing reg (new_temp, move_dir);
            true, new_temp
          | Some (r, dir) -> dir <> move_dir, r
        in
        (if add_instr
        then
          let from, to_ =
            match move_dir with
            | `load -> spilled, temp
            | `store -> temp, spilled
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
      if Utils.debug
      then (
        Utils.log ~indent:2 "body of #%d, before:" label;
        Utils.log_body_and_terminator ~indent:3 block.body block.terminator
          liveness);
      DLL.iter_cell block.body ~f:(fun cell ->
          let instr = DLL.value cell in
          if instruction_contains_spilled instr
          then
            match Regalloc_stack_operands.basic spilled_map instr with
            | All_spilled_registers_rewritten -> ()
            | May_still_have_spilled_registers ->
              let sharing = Reg.Tbl.create 8 in
              rewrite_instruction ~direction:(Load_before_cell cell) ~sharing
                instr;
              rewrite_instruction ~direction:(Store_after_cell cell) ~sharing
                instr);
      if instruction_contains_spilled block.terminator
      then
        match
          Regalloc_stack_operands.terminator spilled_map block.terminator
        with
        | All_spilled_registers_rewritten -> ()
        | May_still_have_spilled_registers ->
          (let sharing = Reg.Tbl.create 8 in
           rewrite_instruction ~direction:(Load_after_list block.body)
             ~sharing:(Reg.Tbl.create 8) block.terminator;
           let new_instrs = DLL.make_empty () in
           rewrite_instruction ~direction:(Store_before_list new_instrs)
             ~sharing block.terminator;
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
          if Utils.debug
          then (
            Utils.log ~indent:2 "and after:";
            Utils.log_body_and_terminator ~indent:3 block.body block.terminator
              liveness;
            Utils.log ~indent:2 "end"));
  if should_coalesce_temp_reloads
  then coalesce_temp_reloads cfg_with_infos new_temporaries;
  !new_temporaries, !block_insertion

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
  if Utils.debug
  then
    Utils.log ~indent:0 "run (%S)"
      (Cfg_with_layout.cfg cfg_with_layout).fun_name;
  Reg.reinit ();
  if Utils.debug && Lazy.force Utils.invariants
  then (
    Utils.log ~indent:0 "precondition";
    Regalloc_invariants.precondition cfg_with_layout);
  let cfg_infos = collect_cfg_infos cfg_with_layout in
  let num_temporaries =
    (* note: this should probably be `Reg.Set.cardinal (Reg.Set.union
       cfg_infos.arg cfg_infos.res)` but the following experimentally produces
       the same results without computing the union. *)
    Reg.Set.cardinal cfg_infos.arg
  in
  if Utils.debug
  then Utils.log ~indent:0 "#temporaries(before):%d" num_temporaries;
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
  if Utils.debug
  then
    Array.iteri (Cfg_with_layout.cfg cfg_with_layout).fun_num_stack_slots
      ~f:(fun stack_class num_stack_slots ->
        Utils.log ~indent:1 "stack_slots[%d]=%d" stack_class num_stack_slots);
  remove_prologue_if_not_required cfg_with_layout;
  update_live_fields cfg_with_layout (Cfg_with_infos.liveness cfg_with_infos);
  f ();
  if Utils.debug && Lazy.force Utils.invariants
  then (
    Utils.log ~indent:0 "postcondition";
    Regalloc_invariants.postcondition_liveness cfg_with_infos)
