[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
module DLL = Flambda_backend_utils.Doubly_linked_list

module type State = sig
  type t

  val stack_slots : t -> Regalloc_stack_slots.t

  val get_and_incr_instruction_id : t -> Instruction.id
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

module type Optimization_reg = sig
  type t

  val of_reg : Reg.t -> t

  val to_reg : t -> Reg.t

  val cl : t -> int

  module Set : Set.S with type elt = t

  module Tbl : Hashtbl.S with type key = t
end

module Optimization_reg : Optimization_reg = struct
  type t = { reg : Reg.t }

  type reg = t

  let[@inline] of_reg (reg : Reg.t) = { reg }

  let[@inline] to_reg { reg } = reg

  let cl { reg } = Proc.register_class reg

  module RegOrder = struct
    type t = reg

    let compare r1 r2 = (to_reg r1).stamp - (to_reg r2).stamp
  end

  module Set = Set.Make (RegOrder)

  module Tbl = Hashtbl.Make (struct
    type t = reg

    let equal r1 r2 = Reg.same (to_reg r1) (to_reg r2)

    let hash (r : t) = r.reg.stamp
  end)
end

module Inst_temporary : Optimization_reg = Optimization_reg

module Block_temporary : Optimization_reg = Optimization_reg

module Spilled_var : Optimization_reg = Optimization_reg

module Actual_var : Optimization_reg = Optimization_reg

module Unspilled_reg : Optimization_reg = Optimization_reg

let[@inline] convert_reg (type a b)
    (module A : Optimization_reg with type t = a)
    (module B : Optimization_reg with type t = b) x =
  x |> A.to_reg |> B.of_reg

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
    spilled_map_external cfg_with_infos ~new_inst_temporaries
    ~new_block_temporaries =
  let spilled_map = Spilled_var.Tbl.create 8 in
  Reg.Tbl.iter
    (fun spilled actual ->
      Spilled_var.Tbl.add spilled_map
        (Spilled_var.of_reg spilled)
        (Actual_var.of_reg actual))
    spilled_map_external;
  let actual_to_spilled =
    spilled_map |> Spilled_var.Tbl.to_seq
    |> Seq.map (fun (k, v) -> v, k)
    |> Actual_var.Tbl.of_seq
  in
  let block_temp_to_inst_temps = Block_temporary.Tbl.create 8 in
  let link_to_block_temp inst_temp block_temp =
    if not
         (Reg.same
            (Inst_temporary.to_reg inst_temp)
            (Block_temporary.to_reg block_temp))
    then
      Block_temporary.Tbl.replace block_temp_to_inst_temps block_temp
        (inst_temp
        :: (Block_temporary.Tbl.find_opt block_temp_to_inst_temps block_temp
           |> Option.value ~default:[]))
  in
  let instrs_to_remove_for_var = Actual_var.Tbl.create 8 in
  let remove_instr var instr_cell =
    let existing =
      Actual_var.Tbl.find_opt instrs_to_remove_for_var var
      |> Option.value ~default:[]
    in
    Actual_var.Tbl.replace instrs_to_remove_for_var var (instr_cell :: existing)
  in
  let var_to_block_temp = Actual_var.Tbl.create 8 in
  let last_spill = Actual_var.Tbl.create 8 in
  let update_info_using_inst (inst_cell : Cfg.basic Cfg.instruction DLL.cell) =
    let inst = DLL.value inst_cell in
    let promote_to_block =
      convert_reg (module Inst_temporary) (module Block_temporary)
    in
    let add_if_needed var =
      if not (Actual_var.Tbl.mem actual_to_spilled var)
      then (
        let var_as_spilled =
          convert_reg (module Actual_var) (module Spilled_var) var
        in
        Spilled_var.Tbl.replace spilled_map var_as_spilled var;
        Actual_var.Tbl.replace actual_to_spilled var var_as_spilled)
    in
    match inst.desc with
    | Op Reload -> (
      let var = Actual_var.of_reg inst.arg.(0) in
      let temp = Inst_temporary.of_reg inst.res.(0) in
      add_if_needed var;
      match Actual_var.Tbl.find_opt var_to_block_temp var with
      | None -> Actual_var.Tbl.add var_to_block_temp var (promote_to_block temp)
      | Some block_temp ->
        remove_instr var inst_cell;
        link_to_block_temp temp block_temp)
    | Op Spill -> (
      let var = Actual_var.of_reg inst.res.(0) in
      let temp = Inst_temporary.of_reg inst.arg.(0) in
      add_if_needed var;
      (match Actual_var.Tbl.find_opt last_spill var with
      | None -> ()
      | Some prev_inst_cell -> remove_instr var prev_inst_cell);
      Actual_var.Tbl.replace last_spill var inst_cell;
      match Actual_var.Tbl.find_opt var_to_block_temp var with
      | None -> Actual_var.Tbl.add var_to_block_temp var (promote_to_block temp)
      | Some block_temp -> link_to_block_temp temp block_temp)
    | _ -> ()
  in
  DLL.iter_cell block.body ~f:update_info_using_inst;
  let spilled_in_this_block =
    var_to_block_temp |> Actual_var.Tbl.to_seq_keys
    |> Seq.map (Actual_var.Tbl.find actual_to_spilled)
    |> Spilled_var.Set.of_seq
  in
  let (spilled_to_unspilled_things_crossed
        : Unspilled_reg.Set.t Spilled_var.Tbl.t) =
    Spilled_var.Tbl.create 8
  in
  let (spilled_to_spilled_things_crossed : Spilled_var.Set.t Spilled_var.Tbl.t)
      =
    Spilled_var.Tbl.create 8
  in
  Spilled_var.Set.iter
    (fun spilled ->
      Spilled_var.Tbl.replace spilled_to_unspilled_things_crossed spilled
        Unspilled_reg.Set.empty)
    spilled_in_this_block;
  Spilled_var.Set.iter
    (fun spilled ->
      Spilled_var.Tbl.replace spilled_to_spilled_things_crossed spilled
        Spilled_var.Set.empty)
    spilled_in_this_block;
  let update_live_info_using_inst (inst : Cfg.basic Cfg.instruction) =
    match inst.desc with
    | Op Reload | Op Spill -> ()
    | _ ->
      let liveness_domain =
        Cfg_with_infos.liveness_find cfg_with_infos inst.id
      in
      let live = Reg.Set.union liveness_domain.before liveness_domain.across in
      let spilled_things_live =
        Spilled_var.Set.filter
          (Spilled_var.Tbl.mem spilled_map)
          (Reg.Set.to_seq live |> Seq.map Spilled_var.of_reg
         |> Spilled_var.Set.of_seq)
      in
      let spilled_things_from_this_block_live =
        Spilled_var.Set.filter
          (fun (original_var : Spilled_var.t) ->
            let (actual : Actual_var.t) =
              match Spilled_var.Tbl.find_opt spilled_map original_var with
              | Some x -> x
              | None ->
                convert_reg
                  (module Spilled_var)
                  (module Actual_var)
                  original_var
            in
            Actual_var.Tbl.mem var_to_block_temp actual)
          spilled_things_live
      in
      let (unspilled_things_live : Unspilled_reg.Set.t) =
        Spilled_var.Set.to_seq spilled_things_live
        |> Seq.map Spilled_var.to_reg |> Reg.Set.of_seq |> Reg.Set.diff live
        |> Reg.Set.to_seq
        |> Seq.map Unspilled_reg.of_reg
        |> Unspilled_reg.Set.of_seq
      in
      let update_live (original_var : Spilled_var.t) =
        let update_spilled_to_spilled to_add =
          let remove_self = Spilled_var.Set.remove original_var in
          let remove_diff_class =
            Spilled_var.Set.filter (fun var ->
                Spilled_var.cl original_var = Spilled_var.cl var)
          in
          Spilled_var.Tbl.find_opt spilled_to_spilled_things_crossed
            original_var
          |> Option.value ~default:Spilled_var.Set.empty
          |> Spilled_var.Set.union (to_add |> remove_self |> remove_diff_class)
          |> Spilled_var.Tbl.replace spilled_to_spilled_things_crossed
               original_var
        in
        let update_spilled_to_unspilled to_add =
          let remove_diff_class =
            Unspilled_reg.Set.filter (fun unspilled ->
                Spilled_var.cl original_var = Unspilled_reg.cl unspilled)
          in
          Spilled_var.Tbl.find_opt spilled_to_unspilled_things_crossed
            original_var
          |> Option.value ~default:Unspilled_reg.Set.empty
          |> Unspilled_reg.Set.union (remove_diff_class to_add)
          |> Spilled_var.Tbl.replace spilled_to_unspilled_things_crossed
               original_var
        in
        if Spilled_var.Tbl.mem spilled_map original_var
        then (
          update_spilled_to_spilled spilled_things_from_this_block_live;
          update_spilled_to_unspilled unspilled_things_live)
      in
      Spilled_var.Set.iter update_live spilled_things_from_this_block_live
  in
  DLL.iter block.body ~f:update_live_info_using_inst;
  let convert_spilled_to_unspilled (spilled : Spilled_var.t) =
    let unspilled = spilled |> Spilled_var.to_reg |> Unspilled_reg.of_reg in
    Spilled_var.Tbl.remove spilled_to_unspilled_things_crossed spilled;
    Spilled_var.Tbl.remove spilled_to_spilled_things_crossed spilled;
    Spilled_var.Tbl.iter
      (fun other_spilled spilled_things_crossed ->
        if Spilled_var.Set.mem spilled spilled_things_crossed
        then (
          Spilled_var.Tbl.replace spilled_to_spilled_things_crossed
            other_spilled
            (Spilled_var.Set.remove spilled spilled_things_crossed);
          Spilled_var.Tbl.replace spilled_to_unspilled_things_crossed
            other_spilled
            (let existing =
               Spilled_var.Tbl.find_opt spilled_to_unspilled_things_crossed
                 other_spilled
               |> Option.value ~default:Unspilled_reg.Set.empty
             in
             Unspilled_reg.Set.add unspilled existing)))
      spilled_to_spilled_things_crossed
  in
  let substitution = Reg.Tbl.create 8 in
  let make_block_temp spilled =
    let var = Spilled_var.Tbl.find spilled_map spilled in
    let block_temp = Actual_var.Tbl.find var_to_block_temp var in
    Block_temporary.Tbl.find_opt block_temp_to_inst_temps block_temp
    |> Option.value ~default:[]
    |> List.iter ~f:(fun inst_temp ->
           Reg.Tbl.add substitution
             (Inst_temporary.to_reg inst_temp)
             (Block_temporary.to_reg block_temp));
    List.iter ~f:DLL.delete_curr
      (Actual_var.Tbl.find_opt instrs_to_remove_for_var var
      |> Option.value ~default:[])
  in
  let rec pick_block_temporaries () =
    let eligible spilled unspilled_things_crossed =
      Unspilled_reg.Set.cardinal unspilled_things_crossed
      < Proc.num_available_registers.(Spilled_var.cl spilled)
    in
    let score spilled_var =
      Actual_var.Tbl.find_opt instrs_to_remove_for_var
        (Spilled_var.Tbl.find spilled_map spilled_var)
      |> Option.value ~default:[] |> List.length
    in
    let best =
      Spilled_var.Tbl.fold
        (fun spilled_var unspilled_things_crossed acc ->
          if eligible spilled_var unspilled_things_crossed
          then
            let curr_score = score spilled_var in
            match acc with
            | Some (_, prev_score) when curr_score <= prev_score -> acc
            | _ -> Some (spilled_var, curr_score)
          else acc)
        spilled_to_unspilled_things_crossed None
    in
    match best with
    | Some (spilled, _) ->
      convert_spilled_to_unspilled spilled;
      make_block_temp spilled;
      pick_block_temporaries ()
    | None -> ()
  in
  pick_block_temporaries ();
  if Reg.Tbl.length substitution <> 0
  then (
    Substitution.apply_block_in_place substitution block;
    Reg.Tbl.iter
      (fun inst_temp block_temp ->
        let remove_inst_temporary temp =
          new_inst_temporaries := Reg.Set.remove temp !new_inst_temporaries
        in
        remove_inst_temporary inst_temp;
        remove_inst_temporary block_temp;
        new_block_temporaries := block_temp :: !new_block_temporaries)
      substitution)

let rewrite_gen :
    type s.
    (module State with type t = s) ->
    (module Utils) ->
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
  if Utils.debug then Utils.log ~indent:1 "rewrite";
  let block_insertion = ref false in
  let spilled_map : Reg.t Reg.Tbl.t =
    List.fold_left spilled_nodes ~init:(Reg.Tbl.create 17)
      ~f:(fun spilled_map reg ->
        if Utils.debug then assert (Utils.is_spilled reg);
        let spilled = Reg.create reg.Reg.typ in
        Utils.set_spilled spilled;
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
  let new_inst_temporaries : Reg.Set.t ref = ref Reg.Set.empty in
  let new_block_temporaries = ref [] in
  let make_new_temporary ~(move : Move.t) (reg : Reg.t) : Reg.t =
    let res =
      make_temporary ~same_class_and_base_name_as:reg ~name_prefix:"temp"
    in
    new_inst_temporaries := Reg.Set.add res !new_inst_temporaries;
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
               || Regalloc_stack_operands.basic spilled_map instr
                  = May_still_have_spilled_registers
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
           || Regalloc_stack_operands.terminator spilled_map block.terminator
              = May_still_have_spilled_registers
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
        coalesce_temp_spills_and_reloads block spilled_map cfg_with_infos
          ~new_inst_temporaries ~new_block_temporaries;
      if Utils.debug
      then (
        Utils.log ~indent:2 "and after:";
        Utils.log_body_and_terminator ~indent:3 block.body block.terminator
          liveness;
        Utils.log ~indent:2 "end"));
  ( !new_inst_temporaries |> Reg.Set.to_seq |> List.of_seq,
    !new_block_temporaries,
    !block_insertion )

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
