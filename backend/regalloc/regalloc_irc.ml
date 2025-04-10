[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils
open! Regalloc_irc_utils
module State = Regalloc_irc_state

let filter_unavailable : Reg.t array -> Reg.t array =
 fun regs ->
  let is_available (reg : Reg.t) : bool =
    match reg.loc with
    | Unknown -> true
    | Reg r ->
      let reg_class = Proc.register_class reg in
      r - Proc.first_available_register.(reg_class)
      < Proc.num_available_registers.(reg_class)
    | Stack _ -> true
  in
  let num_available =
    Array.fold_left regs ~init:0 ~f:(fun acc reg ->
        if is_available reg then succ acc else acc)
  in
  if num_available = Array.length regs
  then regs
  else
    let res = Array.make num_available Reg.dummy in
    let idx = ref 0 in
    Array.iter regs ~f:(fun reg ->
        if is_available reg
        then (
          res.(!idx) <- reg;
          incr idx));
    res

let build : State.t -> Cfg_with_infos.t -> unit =
 fun state cfg_with_infos ->
  if debug then log "build";
  let liveness = Cfg_with_infos.liveness cfg_with_infos in
  let add_edges_live (id : InstructionId.t) ~(def : Reg.t array)
      ~(move_src : Reg.t) ~(destroyed : Reg.t array) : unit =
    let destroyed = filter_unavailable destroyed in
    let live = InstructionId.Tbl.find liveness id in
    if debug && Reg.set_has_collisions live.across
    then fatal "live set has physical register collisions";
    if Array.length def > 0
    then
      Reg.Set.iter
        (fun reg1 ->
          if move_src == Reg.dummy || not (Reg.same reg1 move_src)
          then Array.iter def ~f:(fun reg2 -> State.add_edge state reg1 reg2))
        live.across;
    if Array.length destroyed > 0
    then
      Reg.Set.iter
        (fun reg1 ->
          Array.iter destroyed ~f:(fun reg2 -> State.add_edge state reg1 reg2))
        live.across
  in
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  Cfg_with_layout.iter_instructions cfg_with_layout
    ~instruction:(fun (instr : Instruction.t) ->
      if is_move_instruction instr
         && (not (Reg.is_stack instr.arg.(0)))
         && (not (Reg.is_stack instr.res.(0)))
         && same_reg_class instr.arg.(0) instr.res.(0)
      then (
        State.add_move_list state instr.arg.(0) instr;
        if not (Reg.same instr.arg.(0) instr.res.(0))
        then State.add_move_list state instr.res.(0) instr;
        State.add_work_list_moves state instr;
        let move_src =
          if Reg.same instr.arg.(0) instr.res.(0)
          then Reg.dummy
          else instr.arg.(0)
        in
        add_edges_live instr.id ~def:instr.res ~move_src ~destroyed:[||])
      else
        add_edges_live instr.id ~def:instr.res ~move_src:Reg.dummy
          ~destroyed:(Proc.destroyed_at_basic instr.desc))
    ~terminator:(fun term ->
      (* we assume that a terminator cannot be a move instruction *)
      add_edges_live term.id ~def:term.res ~move_src:Reg.dummy
        ~destroyed:(Proc.destroyed_at_terminator term.desc));
  Cfg.iter_blocks (Cfg_with_layout.cfg cfg_with_layout) ~f:(fun _label block ->
      if block.is_trap_handler
      then
        let first_id = Cfg.first_instruction_id block in
        let live = InstructionId.Tbl.find liveness first_id in
        Reg.Set.iter
          (fun reg1 ->
            Array.iter (filter_unavailable Proc.destroyed_at_raise)
              ~f:(fun reg2 -> State.add_edge state reg1 reg2))
          (Reg.Set.remove Proc.loc_exn_bucket live.before))

let make_work_list : State.t -> unit =
 fun state ->
  if debug
  then (
    log "make_work_list";
    indent ());
  State.iter_and_clear_initial state ~f:(fun reg ->
      let deg = State.degree state reg in
      if debug
      then (
        log "- %a has degree=%s (k=%d)" Printreg.reg reg (Degree.to_string deg)
          (k reg);
        indent ());
      if deg >= k reg
      then (
        if debug then log "~> spill_work_list";
        State.add_spill_work_list state reg)
      else if State.is_move_related state reg
      then (
        if debug then log "~> freeze_work_list";
        State.add_freeze_work_list state reg)
      else (
        if debug then log "~> simplify_work_list";
        State.add_simplify_work_list state reg);
      if debug then dedent ());
  if debug then dedent ()

let simplify : State.t -> unit =
 fun state ->
  let reg = State.choose_and_remove_simplify_work_list state in
  if debug then log "simplify %a" Printreg.reg reg;
  State.push_select_stack state reg;
  State.iter_adjacent state reg ~f:(fun adj -> State.decr_degree state adj)

let ok : State.t -> Reg.t -> Reg.t -> bool =
 fun state t r ->
  WorkList.equal (State.work_list state t) WorkList.Precolored
  || State.degree state t < k t
  || State.mem_adj_set state t r

let all_adjacent_are_ok : State.t -> Reg.t -> Reg.t -> bool =
 fun state u v -> State.for_all_adjacent state v ~f:(fun t -> ok state t u)

let conservative : State.t -> Reg.t -> Reg.t -> bool =
 fun state reg1 reg2 ->
  let exception False in
  let i = ref 0 in
  let seen = Reg.Tbl.create 32 in
  let f (reg : Reg.t) : unit =
    if not (Reg.Tbl.mem seen reg)
    then (
      Reg.Tbl.replace seen reg ();
      let k = k reg in
      if State.degree state reg >= k
      then (
        incr i;
        if !i >= k then raise_notrace False))
  in
  try
    State.iter_adjacent state reg1 ~f;
    State.iter_adjacent state reg2 ~f;
    true
  with False -> false

let combine : State.t -> Reg.t -> Reg.t -> unit =
 fun state u v ->
  if debug then log "combine u=%a v=%a" Printreg.reg u Printreg.reg v;
  if State.mem_freeze_work_list state v
  then State.remove_freeze_work_list state v
  else State.remove_spill_work_list state v;
  State.add_coalesced_nodes state v;
  State.add_alias state v u;
  (* note: See book errata
     (https://www.cs.princeton.edu/~appel/modern/ml/errata98.html) *)
  State.union_move_list state u (State.find_move_list state v);
  State.enable_moves_one state v;
  State.iter_adjacent state v ~f:(fun t ->
      State.add_edge state t u;
      State.decr_degree state t);
  if State.mem_freeze_work_list state u && State.degree state u >= k u
  then (
    State.remove_freeze_work_list state u;
    State.add_spill_work_list state u)

let add_work_list : State.t -> Reg.t -> unit =
 fun state reg ->
  (* note: the test that `reg` is not precolored is redundant since precolored
     registers have an infinite degree. *)
  if State.degree state reg < k reg && not (State.is_move_related state reg)
  then (
    State.remove_freeze_work_list state reg;
    State.add_simplify_work_list state reg)

let coalesce : State.t -> unit =
 fun state ->
  if debug
  then (
    log "coalesce";
    indent ());
  let m = State.choose_and_remove_work_list_moves state in
  let x = m.res.(0) in
  let y = m.arg.(0) in
  if debug then log "x=%a y=%a" Printreg.reg x Printreg.reg y;
  let x = State.find_alias state x in
  let y = State.find_alias state y in
  if debug then log "x=%a y=%a" Printreg.reg x Printreg.reg y;
  let u, v = if State.is_precolored state y then y, x else x, y in
  if debug then log "u=%a v=%a" Printreg.reg u Printreg.reg v;
  if Reg.same u v
  then (
    if debug then log "case #1/4";
    State.add_coalesced_moves state m;
    add_work_list state u)
  else if State.is_precolored state v
          || (* We must not alias v->u if u uses the same register as a neighbor
                of v. Simply checking whether u and v are adjacent is not
                sufficient because the interference graph treats machine
                registers aliased at multiple types (e.g. xmm0 at float32,
                float, and vec128) as disjoint. *)
          State.interferes_with_adj state v u
  then (
    if debug then log "case #2/4";
    State.add_constrained_moves state m;
    add_work_list state u;
    add_work_list state v)
  else if match State.is_precolored state u with
          | true -> all_adjacent_are_ok state u v
          | false -> conservative state u v
  then (
    if debug then log "case #3/4";
    State.add_coalesced_moves state m;
    combine state u v;
    add_work_list state u)
  else (
    if debug then log "case #4/4";
    State.add_active_moves state m);
  if debug then dedent ()

let freeze_moves : State.t -> Reg.t -> unit =
 fun state u ->
  if debug then log "freeze_moves %a" Printreg.reg u;
  State.iter_node_moves state u ~f:(fun m ->
      let x = m.res.(0) in
      let y = m.arg.(0) in
      let alias_y = State.find_alias state y in
      let v =
        if Reg.same alias_y (State.find_alias state u)
        then State.find_alias state x
        else alias_y
      in
      State.remove_active_moves state m;
      State.add_frozen_moves state m;
      if State.degree state v < k v && State.is_empty_node_moves state v
      then (
        State.remove_freeze_work_list state v;
        State.add_simplify_work_list state v))

let freeze : State.t -> unit =
 fun state ->
  let reg = State.choose_and_remove_freeze_work_list state in
  if debug then log "freeze %a" Printreg.reg reg;
  State.add_simplify_work_list state reg;
  freeze_moves state reg

let select_spilling_register_using_heuristics : State.t -> Reg.t =
 fun state ->
  match Lazy.force Spilling_heuristics.value with
  | Set_choose -> (
    (* This is the "heuristics" from the IRC paper: pick any candidate, just try
       to avoid any of the temporaries introduces for spilling. *)
    let spill_work_list = State.spill_work_list state in
    match
      Reg.Set.choose_opt
        (State.diff_all_introduced_temporaries state spill_work_list)
    with
    | Some reg -> reg
    | None -> (
      match Reg.Set.choose_opt spill_work_list with
      | Some reg -> reg
      | None -> fatal "spill_work_list is empty"))
  | Flat_uses | Hierarchical_uses -> (
    (* note: this assumes that `Reg.spill_cost` has been updated as needed (only
       when `rewrite` is called); the value computed here can however not be
       similarly cached because it depends on the degree (which does not need a
       call to `rewrite` to change). *)
    let weighted_cost (reg : Reg.t) =
      if debug
      then
        log "register %a has spill cost %d" Printreg.reg reg reg.Reg.spill_cost;
      (float reg.Reg.spill_cost /. float (State.degree state reg))
      (* note: while this magic constant is questionable, it is key to not favor
         the introduced temporaries which, by construct, have very few
         occurrences. *)
      +. if State.mem_inst_temporaries state reg then 10_000. else 0.
    in
    match State.is_empty_spill_work_list state with
    | true -> fatal "spill_work_list is empty"
    | false ->
      State.fold_spill_work_list state ~init:(Reg.dummy, Float.max_float)
        ~f:(fun ((_curr_reg, curr_min_cost) as acc) reg ->
          let reg_cost = weighted_cost reg in
          if debug
          then log "register %a has weighted cost %f" Printreg.reg reg reg_cost;
          if Float.compare reg_cost curr_min_cost < 0
          then reg, reg_cost
          else acc)
      |> fst)

let select_spill : State.t -> unit =
 fun state ->
  if debug
  then (
    log "select_spill";
    indent ());
  let reg = select_spilling_register_using_heuristics state in
  if debug
  then
    log "chose %a using heuristics %S" Printreg.reg reg
      Spilling_heuristics.(to_string @@ Lazy.force value);
  State.remove_spill_work_list state reg;
  State.add_simplify_work_list state reg;
  freeze_moves state reg;
  if debug then dedent ()

let assign_colors : State.t -> Cfg_with_layout.t -> unit =
 fun state _cfg_with_layout ->
  if debug
  then (
    log "assign_colors";
    indent ());
  State.iter_and_clear_select_stack state ~f:(fun n ->
      if debug
      then (
        log "%a" Printreg.reg n;
        indent ());
      let reg_class = Proc.register_class n in
      let reg_num_avail =
        Array.unsafe_get Proc.num_available_registers reg_class
      in
      let reg_first_avail =
        Array.unsafe_get Proc.first_available_register reg_class
      in
      let ok_colors = Array.make reg_num_avail true in
      let counter = ref reg_num_avail in
      let rec mark_adjacent_colors_and_get_first_available (adj : Reg.t list) :
          int =
        match adj with
        | [] ->
          let first_avail = ref 0 in
          while
            !first_avail < reg_num_avail
            && not (Array.unsafe_get ok_colors !first_avail)
          do
            incr first_avail
          done;
          !first_avail
        | hd :: tl ->
          let alias = State.find_alias state hd in
          if State.is_precolored_or_colored state alias
          then (
            match State.color state alias with
            | None -> assert false
            | Some color ->
              if debug then log "color %d is not available" color;
              if Array.unsafe_get ok_colors (color - reg_first_avail)
              then (
                Array.unsafe_set ok_colors (color - reg_first_avail) false;
                decr counter;
                if !counter > 0
                then mark_adjacent_colors_and_get_first_available tl
                else reg_num_avail)
              else mark_adjacent_colors_and_get_first_available tl)
          else mark_adjacent_colors_and_get_first_available tl
      in
      let first_avail =
        mark_adjacent_colors_and_get_first_available (State.adj_list state n)
      in
      if first_avail = reg_num_avail
      then (
        if debug then log "spilling";
        State.add_spilled_nodes state n)
      else (
        State.add_colored_nodes state n;
        let c = first_avail + reg_first_avail in
        if debug then log "coloring with %d" c;
        State.set_color state n (Some c));
      if debug then dedent ());
  State.iter_coalesced_nodes state ~f:(fun n ->
      let alias = State.find_alias state n in
      State.set_color state n (State.color state alias));
  if debug then dedent ()

module Utils = struct
  include Regalloc_irc_utils

  type state = State.t

  let debug = debug

  let invariants = invariants

  let is_spilled state reg =
    match State.work_list_opt state reg with
    | None ->
      (* Freshly-created may not have been added to the map yet; such registers
         would morally be in the "unknown" work list, hence returning
         `false`. *)
      false
    | Some work_list -> WorkList.equal work_list WorkList.Spilled

  let set_spilled _state _reg = ()
end

(* Returns `true` if new temporaries have been introduced. *)
let rewrite :
    State.t ->
    Cfg_with_infos.t ->
    spilled_nodes:Reg.t list ->
    reset:bool ->
    block_temporaries:bool ->
    bool =
 fun state cfg_with_infos ~spilled_nodes ~reset ~block_temporaries ->
  let new_inst_temporaries, new_block_temporaries, block_inserted =
    Regalloc_rewrite.rewrite_gen
      (module State)
      (module Utils)
      state cfg_with_infos ~spilled_nodes ~block_temporaries
  in
  if block_inserted
  then Cfg_with_infos.invalidate_dominators_and_loop_infos cfg_with_infos;
  match new_inst_temporaries, new_block_temporaries with
  | [], [] -> false
  | _ ->
    (Cfg_with_infos.invalidate_liveness cfg_with_infos;
     State.add_inst_temporaries_list state new_inst_temporaries;
     State.add_block_temporaries_list state new_block_temporaries;
     match reset with
     | true -> State.reset state ~new_inst_temporaries ~new_block_temporaries
     | false ->
       State.clear_spilled_nodes state;
       State.add_initial_list state new_block_temporaries;
       State.add_initial_list state new_inst_temporaries);
    true

(* CR xclerc for xclerc: could probably be lower; the compiler distribution
   seems to be fine with 4 *)
let max_rounds = 50

let rec main : round:int -> State.t -> Cfg_with_infos.t -> unit =
 fun ~round state cfg_with_infos ->
  if round > max_rounds
  then
    fatal "register allocation was not succesful after %d rounds (%s)"
      max_rounds (Cfg_with_infos.cfg cfg_with_infos).fun_name;
  if debug
  then (
    log "main, round #%d" round;
    indent ());
  let work_lists_desc state (name, f) =
    Printf.sprintf "%s:%s" name (if f state then "{}" else "...")
  in
  let work_lists_desc state =
    String.concat " "
      (List.map ~f:(work_lists_desc state)
         [ "simplify_wl", State.is_empty_simplify_work_list;
           "wl_moves", State.is_empty_work_list_moves;
           "freeze_wl", State.is_empty_freeze_work_list;
           "spill_wl", State.is_empty_spill_work_list ])
  in
  let log_work_list_desc prefix =
    if debug then log "%s -- %s" prefix (work_lists_desc state)
  in
  build state cfg_with_infos;
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  if debug
  then (
    let adj_set = State.adj_set state in
    log "(%d pairs in adj_set)" (RegisterStamp.PairSet.cardinal adj_set);
    (* CR-someday xclerc for xclerc: remove (kept for the moment for debugging,
       but does not deserve to be controlled by a variable) *)
    if false
    then
      (* may produce a *lot* of lines... *)
      RegisterStamp.PairSet.iter adj_set ~f:(fun p ->
          log "(%d, %d) <- adj_set" (RegisterStamp.fst p) (RegisterStamp.snd p)));
  make_work_list state;
  State.invariant state;
  if debug then log_work_list_desc "before loop";
  let spill_cost_is_up_to_date = ref false in
  let continue = ref true in
  while !continue do
    if not (State.is_empty_simplify_work_list state)
    then simplify state
    else if not (State.is_empty_work_list_moves state)
    then coalesce state
    else if not (State.is_empty_freeze_work_list state)
    then freeze state
    else if not (State.is_empty_spill_work_list state)
    then (
      if not !spill_cost_is_up_to_date
      then (
        (match Lazy.force Spilling_heuristics.value with
        | Set_choose ->
          (* note: `spill_cost` will not be used by the heuristics *) ()
        | Flat_uses -> update_spill_cost cfg_with_infos ~flat:true ()
        | Hierarchical_uses -> update_spill_cost cfg_with_infos ~flat:false ());
        spill_cost_is_up_to_date := true);
      select_spill state)
    else continue := false;
    if debug then log_work_list_desc "end of loop";
    State.invariant state
  done;
  if debug then log "(after loop)";
  assign_colors state cfg_with_layout;
  State.invariant state;
  if debug then dedent ();
  match State.spilled_nodes state with
  | [] -> if debug then log "(end of main)"
  | _ :: _ as spilled_nodes -> (
    if debug
    then
      List.iter spilled_nodes ~f:(fun reg ->
          log "/!\\ register %a needs to be spilled" Printreg.reg reg);
    match
      rewrite state cfg_with_infos ~spilled_nodes ~reset:true
        ~block_temporaries:(round = 1)
    with
    | false -> if debug then log "(end of main)"
    | true ->
      State.invariant state;
      main ~round:(succ round) state cfg_with_infos)

let run : Cfg_with_infos.t -> Cfg_with_infos.t =
 fun cfg_with_infos ->
  if debug then reset_indentation ();
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let cfg_infos, stack_slots =
    Regalloc_rewrite.prelude
      (module Utils)
      ~on_fatal_callback:(fun () -> save_cfg "irc" cfg_with_layout)
      cfg_with_infos
  in
  (* CR xclerc for xclerc: consider moving the computation of temporaries and
     the creation of the state to `prelude`. *)
  let all_temporaries = Reg.Set.union cfg_infos.arg cfg_infos.res in
  if debug then log "#temporaries=%d" (Reg.Set.cardinal all_temporaries);
  let state =
    State.make
      ~initial:(Reg.Set.elements all_temporaries)
      ~stack_slots ~last_used:cfg_infos.max_instruction_id ()
  in
  let spilling_because_unused = Reg.Set.diff cfg_infos.res cfg_infos.arg in
  (match Reg.Set.elements spilling_because_unused with
  | [] -> ()
  | _ :: _ as spilled_nodes ->
    List.iter spilled_nodes ~f:(fun reg -> State.add_spilled_nodes state reg);
    (* note: rewrite will remove the `spilling` registers from the "spilled"
       work list and set the field to unknown. *)
    let (_ : bool) =
      rewrite state cfg_with_infos ~spilled_nodes ~reset:false
        ~block_temporaries:false
    in
    ());
  main ~round:1 state cfg_with_infos;
  if debug then log_cfg_with_infos cfg_with_infos;
  Regalloc_rewrite.postlude
    (module State)
    (module Utils)
    state
    ~f:(fun () ->
      State.update_register_locations state;
      Reg.Set.iter
        (fun reg -> State.set_degree state reg 0)
        (all_precolored_regs ()))
    cfg_with_infos;
  cfg_with_infos
