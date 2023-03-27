[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Cfg_regalloc_utils
open! Cfg_irc_utils
open! Cfg_irc_split
module State = Cfg_irc_state
module DLL = Flambda_backend_utils.Doubly_linked_list

let build : State.t -> Cfg_with_liveness.t -> unit =
 fun state cfg_with_liveness ->
  if irc_debug then log ~indent:1 "build";
  let liveness = Cfg_with_liveness.liveness cfg_with_liveness in
  let add_edges_live (id : Instruction.id) ~(def : Reg.t array)
      ~(move_src : Reg.t) ~(destroyed : Reg.t array) : unit =
    let live = Cfg_dataflow.Instr.Tbl.find liveness id in
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
  let cfg_with_layout = Cfg_with_liveness.cfg_with_layout cfg_with_liveness in
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
        let live = Cfg_dataflow.Instr.Tbl.find liveness first_id in
        Reg.Set.iter
          (fun reg1 ->
            Array.iter Proc.destroyed_at_raise ~f:(fun reg2 ->
                State.add_edge state reg1 reg2))
          (Reg.Set.remove Proc.loc_exn_bucket live.before))

let make_work_list : State.t -> unit =
 fun state ->
  if irc_debug then log ~indent:1 "make_work_list";
  State.iter_and_clear_initial state ~f:(fun reg ->
      let deg = reg.Reg.degree in
      if irc_debug
      then
        log ~indent:2 "- %a has degree=%s (k=%d)" Printmach.reg reg
          (Degree.to_string deg) (k reg);
      if deg >= k reg
      then (
        if irc_debug then log ~indent:3 "~> spill_work_list";
        State.add_spill_work_list state reg)
      else if State.is_move_related state reg
      then (
        if irc_debug then log ~indent:3 "~> freeze_work_list";
        State.add_freeze_work_list state reg)
      else (
        if irc_debug then log ~indent:3 "~> simplify_work_list";
        State.add_simplify_work_list state reg))

let simplify : State.t -> unit =
 fun state ->
  let reg = State.choose_and_remove_simplify_work_list state in
  if irc_debug then log ~indent:1 "simplify %a" Printmach.reg reg;
  State.push_select_stack state reg;
  State.iter_adjacent state reg ~f:(fun adj -> State.decr_degree state adj)

let ok : State.t -> Reg.t -> Reg.t -> bool =
 fun state t r ->
  t.Reg.irc_work_list = Reg.Precolored
  || t.Reg.degree < k t
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
      if reg.Reg.degree >= k
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
  if irc_debug
  then log ~indent:2 "combine u=%a v=%a" Printmach.reg u Printmach.reg v;
  Profile.record ~accumulate:true "part1"
    (fun () ->
      if State.mem_freeze_work_list state v
      then State.remove_freeze_work_list state v
      else State.remove_spill_work_list state v;
      State.add_coalesced_nodes state v;
      State.add_alias state v u)
    ();
  (* note: See book errata
     (https://www.cs.princeton.edu/~appel/modern/ml/errata98.html) *)
  Profile.record ~accumulate:true "part2"
    (fun () ->
      State.union_move_list state u (State.find_move_list state v);
      State.enable_moves_one state v;
      State.iter_adjacent state v ~f:(fun t ->
          State.add_edge state t u;
          State.decr_degree state t))
    ();
  Profile.record ~accumulate:true "part3"
    (fun () ->
      if State.mem_freeze_work_list state u && u.Reg.degree >= k u
      then (
        State.remove_freeze_work_list state u;
        State.add_spill_work_list state u))
    ()

let add_work_list : State.t -> Reg.t -> unit =
 fun state reg ->
  (* note: the test that `reg` is not precolored is redundant since precolored
     registers have an infinite degree. *)
  if reg.Reg.degree < k reg && not (State.is_move_related state reg)
  then (
    State.remove_freeze_work_list state reg;
    State.add_simplify_work_list state reg)

let coalesce : State.t -> unit =
 fun state ->
  if irc_debug then log ~indent:1 "coalesce";
  let m = State.choose_and_remove_work_list_moves state in
  let x = m.res.(0) in
  let y = m.arg.(0) in
  if irc_debug then log ~indent:2 "x=%a y=%a" Printmach.reg x Printmach.reg y;
  let x = State.find_alias state x in
  let y = State.find_alias state y in
  if irc_debug then log ~indent:2 "x=%a y=%a" Printmach.reg x Printmach.reg y;
  let u, v = if State.is_precolored state y then y, x else x, y in
  if irc_debug then log ~indent:2 "u=%a v=%a" Printmach.reg u Printmach.reg v;
  if Reg.same u v
  then (
    if irc_debug then log ~indent:2 "case #1/4";
    Profile.record ~accumulate:true "case1"
      (fun () ->
        State.add_coalesced_moves state m;
        add_work_list state u)
      ())
  else if State.is_precolored state v || State.mem_adj_set state u v
  then (
    if irc_debug then log ~indent:2 "case #2/4";
    Profile.record ~accumulate:true "case2"
      (fun () ->
        State.add_constrained_moves state m;
        add_work_list state u;
        add_work_list state v)
      ())
  else if match State.is_precolored state u with
          | true ->
            Profile.record ~accumulate:true "all_adjacent_are_ok"
              (fun () -> all_adjacent_are_ok state u v)
              ()
          | false ->
            Profile.record ~accumulate:true "conservative"
              (fun () -> conservative state u v)
              ()
  then (
    if irc_debug then log ~indent:2 "case #3/4";
    Profile.record ~accumulate:true "case3"
      (fun () ->
        State.add_coalesced_moves state m;
        combine state u v;
        add_work_list state u)
      ())
  else (
    if irc_debug then log ~indent:2 "case #4/4";
    Profile.record ~accumulate:true "case4"
      (fun () -> State.add_active_moves state m)
      ())

let freeze_moves : State.t -> Reg.t -> unit =
 fun state u ->
  if irc_debug then log ~indent:2 "freeze_moves %a" Printmach.reg u;
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
      if v.Reg.degree < k v && State.is_empty_node_moves state v
      then (
        State.remove_freeze_work_list state v;
        State.add_simplify_work_list state v))

let freeze : State.t -> unit =
 fun state ->
  let reg = State.choose_and_remove_freeze_work_list state in
  if irc_debug then log ~indent:1 "freeze %a" Printmach.reg reg;
  State.add_simplify_work_list state reg;
  freeze_moves state reg

let select_spilling_register_using_heuristics : State.t -> Reg.t =
 fun state ->
  match Lazy.force Spilling_heuristics.env with
  | Set_choose -> (
    (* This is the "heuristics" from the IRC paper: pick any candidate, just try
       to avoid any of the temporaries introduces for spilling. *)
    let spill_work_list = State.spill_work_list state in
    let introduced_temporaries = State.introduced_temporaries state in
    match
      Reg.Set.choose_opt (Reg.Set.diff spill_work_list introduced_temporaries)
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
      if irc_debug
      then
        log ~indent:2 "register %a has spill cost %d" Printmach.reg reg
          reg.Reg.spill_cost;
      (float reg.Reg.spill_cost /. float reg.Reg.degree)
      (* note: while this magic constant is questionable, it is key to not favor
         the introduced temporaries which, by construct, have very few
         occurrences. *)
      +. if State.mem_introduced_temporaries state reg then 10_000. else 0.
    in
    match State.is_empty_spill_work_list state with
    | true -> fatal "spill_work_list is empty"
    | false ->
      State.fold_spill_work_list state ~init:(Reg.dummy, Float.max_float)
        ~f:(fun ((_curr_reg, curr_min_cost) as acc) reg ->
          let reg_cost = weighted_cost reg in
          if irc_debug
          then
            log ~indent:2 "register %a has weighted cost %f" Printmach.reg reg
              reg_cost;
          if reg_cost < curr_min_cost then reg, reg_cost else acc)
      |> fst)

let select_spill : State.t -> unit =
 fun state ->
  if irc_debug then log ~indent:1 "select_spill";
  let reg = select_spilling_register_using_heuristics state in
  if irc_debug
  then
    log ~indent:2 "chose %a using heuristics %S" Printmach.reg reg
      Spilling_heuristics.(to_string @@ Lazy.force env);
  State.remove_spill_work_list state reg;
  State.add_simplify_work_list state reg;
  freeze_moves state reg

let assign_colors : State.t -> Cfg_with_layout.t -> unit =
 fun state _cfg_with_layout ->
  if irc_debug then log ~indent:1 "assign_colors";
  State.iter_and_clear_select_stack state ~f:(fun n ->
      if irc_debug then log ~indent:2 "%a" Printmach.reg n;
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
            match alias.Reg.irc_color with
            | None -> assert false
            | Some color ->
              if irc_debug then log ~indent:3 "color %d is not available" color;
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
        if irc_debug then log ~indent:3 "spilling";
        State.add_spilled_nodes state n)
      else (
        State.add_colored_nodes state n;
        let c = first_avail + reg_first_avail in
        if irc_debug then log ~indent:3 "coloring with %d" c;
        n.Reg.irc_color <- Some c));
  State.iter_coalesced_nodes state ~f:(fun n ->
      let alias = State.find_alias state n in
      n.Reg.irc_color <- alias.Reg.irc_color)

type direction =
  | Load_before_cell of Cfg.basic Cfg.instruction DLL.cell
  | Store_after_cell of Cfg.basic Cfg.instruction DLL.cell
  | Load_after_list of Cfg.basic_instruction_list
  | Store_before_list of Cfg.basic_instruction_list

(* Returns `true` if new temporaries have been introduced. *)
let rewrite : State.t -> Cfg_with_liveness.t -> Reg.t list -> reset:bool -> bool
    =
 fun state cfg_with_liveness spilled_nodes ~reset ->
  if irc_debug then log ~indent:1 "rewrite";
  let spilled_map : Reg.t Reg.Tbl.t =
    List.fold_left spilled_nodes ~init:(Reg.Tbl.create 17)
      ~f:(fun spilled_map reg ->
        if irc_debug then assert (reg.Reg.irc_work_list = Reg.Spilled);
        let spilled = Reg.create reg.Reg.typ in
        spilled.spill <- true;
        (* for printing *)
        if not (Reg.anonymous reg) then spilled.Reg.raw_name <- reg.Reg.raw_name;
        let slot = State.get_num_stack_slot state reg in
        spilled.Reg.loc <- Reg.(Stack (Local slot));
        if irc_debug
        then
          log ~indent:2 "spilling %a to %a" Printmach.reg reg Printmach.reg
            spilled;
        Reg.Tbl.replace spilled_map reg spilled;
        spilled_map)
  in
  let new_temporaries : Reg.t list ref = ref [] in
  let make_new_temporary ~(move : Move.t) (reg : Reg.t) : Reg.t =
    let res =
      make_temporary ~same_class_and_base_name_as:reg ~name_prefix:"temp"
    in
    new_temporaries := res :: !new_temporaries;
    if irc_debug
    then
      log ~indent:2 "adding temporary %a (to %s %a)" Printmach.reg res
        (Move.to_string move) Printmach.reg reg;
    res
  in
  let[@inline] array_contains_spilled (arr : Reg.t array) : bool =
    let len = Array.length arr in
    let i = ref 0 in
    while
      !i < len && (Array.unsafe_get arr !i).Reg.irc_work_list <> Reg.Spilled
    do
      incr i
    done;
    !i < len
  in
  let rewrite_instruction ~(direction : direction)
      ~(sharing : (Reg.t * [`load | `store]) Reg.Tbl.t)
      (instr : _ Cfg.instruction) : unit =
    let f (reg : Reg.t) : Reg.t =
      if reg.Reg.irc_work_list = Reg.Spilled
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
    match direction with
    | Load_before_cell _ | Load_after_list _ ->
      if array_contains_spilled instr.arg
      then instr.arg <- Array.map instr.arg ~f
    | Store_after_cell _ | Store_before_list _ ->
      if array_contains_spilled instr.res
      then instr.res <- Array.map instr.res ~f
  in
  let liveness = Cfg_with_liveness.liveness cfg_with_liveness in
  Cfg.iter_blocks (Cfg_with_liveness.cfg cfg_with_liveness)
    ~f:(fun label block ->
      if irc_debug
      then (
        log ~indent:2 "body of #%d, before:" label;
        log_body_and_terminator ~indent:3 block.body block.terminator liveness);
      DLL.iter_cell block.body ~f:(fun cell ->
          let instr = DLL.value cell in
          match
            Profile.record ~accumulate:true "stack_operands"
              (fun () -> Cfg_stack_operands.basic spilled_map instr)
              ()
          with
          | All_spilled_registers_rewritten -> ()
          | May_still_have_spilled_registers ->
            let sharing = Reg.Tbl.create 8 in
            rewrite_instruction ~direction:(Load_before_cell cell) ~sharing
              instr;
            rewrite_instruction ~direction:(Store_after_cell cell) ~sharing
              instr);
      match
        Profile.record ~accumulate:true "stack_operands"
          (fun () -> Cfg_stack_operands.terminator spilled_map block.terminator)
          ()
      with
      | All_spilled_registers_rewritten -> ()
      | May_still_have_spilled_registers ->
        (let sharing = Reg.Tbl.create 8 in
         rewrite_instruction ~direction:(Load_after_list block.body)
           ~sharing:(Reg.Tbl.create 8) block.terminator;
         let new_instrs = DLL.make_empty () in
         rewrite_instruction ~direction:(Store_before_list new_instrs) ~sharing
           block.terminator;
         if not (DLL.is_empty new_instrs)
         then
           (* insert block *)
           Cfg_regalloc_utils.insert_block
             (Cfg_with_liveness.cfg_with_layout cfg_with_liveness)
             new_instrs ~after:block ~next_instruction_id:(fun () ->
               State.get_and_incr_instruction_id state));
        if irc_debug
        then (
          log ~indent:2 "and after:";
          log_body_and_terminator ~indent:3 block.body block.terminator liveness;
          log ~indent:2 "end"));
  match !new_temporaries, reset with
  | [], _ -> false
  | _ :: _, true ->
    State.reset state ~new_temporaries:!new_temporaries;
    true
  | _ :: _, false ->
    State.add_introduced_temporaries_list state !new_temporaries;
    State.clear_spilled_nodes state;
    State.add_initial_list state !new_temporaries;
    true

(* CR xclerc for xclerc: could probably be lower; the compiler distribution
   seems to be fine with 4 *)
let max_rounds = 50

let rec main : round:int -> State.t -> Cfg_with_liveness.t -> unit =
 fun ~round state cfg_with_liveness ->
  if round > max_rounds
  then
    fatal "register allocation was not succesful after %d rounds (%s)"
      max_rounds (Cfg_with_liveness.cfg cfg_with_liveness).fun_name;
  if irc_debug then log ~indent:0 "main, round #%d" round;
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
    if irc_debug then log ~indent:1 "%s -- %s" prefix (work_lists_desc state)
  in
  Profile.record ~accumulate:true "build"
    (fun () -> build state cfg_with_liveness)
    ();
  let cfg_with_layout = Cfg_with_liveness.cfg_with_layout cfg_with_liveness in
  if irc_debug
  then (
    let adj_set = State.adj_set state in
    log ~indent:1 "(%d pairs in adj_set)"
      (RegisterStamp.PairSet.cardinal adj_set);
    (* CR-someday xclerc for xclerc: remove (kept for the moment for debugging,
       but does not deserve to be controlled by a vaiable) *)
    if false
    then
      (* may produce a *lot* of lines... *)
      RegisterStamp.PairSet.iter adj_set ~f:(fun p ->
          log ~indent:1 "(%d, %d) <- adj_set" (RegisterStamp.fst p)
            (RegisterStamp.snd p)));
  Profile.record ~accumulate:true "make_work_list" make_work_list state;
  State.invariant state;
  if irc_debug then log_work_list_desc "before loop";
  let spill_cost_is_up_to_date = ref false in
  let continue = ref true in
  while !continue do
    if not (State.is_empty_simplify_work_list state)
    then Profile.record ~accumulate:true "simplify" simplify state
    else if not (State.is_empty_work_list_moves state)
    then Profile.record ~accumulate:true "coalesce" coalesce state
    else if not (State.is_empty_freeze_work_list state)
    then Profile.record ~accumulate:true "freeze" freeze state
    else if not (State.is_empty_spill_work_list state)
    then
      Profile.record ~accumulate:true "select_spill"
        (fun () ->
          if not !spill_cost_is_up_to_date
          then (
            (match Lazy.force Spilling_heuristics.env with
            | Set_choose ->
              (* note: `spill_cost` will not be used by the heuristics *) ()
            | Flat_uses -> update_spill_cost cfg_with_layout ~flat:true ()
            | Hierarchical_uses ->
              update_spill_cost cfg_with_layout ~flat:false ());
            spill_cost_is_up_to_date := true);
          select_spill state)
        ()
    else continue := false;
    if irc_debug then log_work_list_desc "end of loop";
    State.invariant state
  done;
  if irc_debug then log ~indent:1 "(after loop)";
  Profile.record ~accumulate:true "assign_colors"
    (fun () -> assign_colors state cfg_with_layout)
    ();
  State.invariant state;
  match State.spilled_nodes state with
  | [] -> if irc_debug then log ~indent:1 "(end of main)"
  | _ :: _ as spilled_nodes -> (
    if irc_debug
    then
      List.iter spilled_nodes ~f:(fun reg ->
          log ~indent:1 "/!\\ register %a needs to be spilled" Printmach.reg reg);
    match
      Profile.record ~accumulate:true "rewrite"
        (fun () -> rewrite state cfg_with_liveness spilled_nodes ~reset:true)
        ()
    with
    | false -> ()
    | true ->
      State.invariant state;
      Cfg_with_liveness.invalidate_liveness cfg_with_liveness;
      main ~round:(succ round) state cfg_with_liveness)

let run : Cfg_with_liveness.t -> Cfg_with_liveness.t =
 fun cfg_with_liveness ->
  let cfg_with_layout = Cfg_with_liveness.cfg_with_layout cfg_with_liveness in
  on_fatal ~f:(fun () -> save_cfg "irc" cfg_with_layout);
  if irc_debug
  then log ~indent:0 "run (%S)" (Cfg_with_layout.cfg cfg_with_layout).fun_name;
  Reg.reinit ();
  if irc_debug && irc_invariants
  then (
    log ~indent:0 "precondition";
    precondition cfg_with_layout);
  if irc_debug
  then
    Array.iteri all_precolored_regs ~f:(fun i reg ->
        log ~indent:0 "precolored[%d] = %a (class %d)" i Printmach.reg reg
          (Proc.register_class reg));
  let { arg; res; max_instruction_id } = collect_cfg_infos cfg_with_layout in
  let all_temporaries = Reg.Set.union arg res in
  if irc_debug
  then log ~indent:0 "#temporaries=%d" (Reg.Set.cardinal all_temporaries);
  let state =
    State.make
      ~initial:(Reg.Set.elements all_temporaries)
      ~next_instruction_id:(succ max_instruction_id) ()
  in
  let spilling_because_split =
    match Lazy.force Split_mode.env with
    | Off -> []
    | Naive -> naive_split_cfg state cfg_with_liveness
  in
  let spilling_because_split_or_unused : Reg.t list =
    Reg.Set.fold
      (fun reg acc -> if Reg.Set.mem reg arg then acc else reg :: acc)
      res spilling_because_split
  in
  if irc_debug
  then
    List.iter spilling_because_split_or_unused ~f:(fun r ->
        log ~indent:0 "%a <- spilling_because_split_or_unused" Printmach.reg r);
  (match spilling_because_split_or_unused with
  | [] -> ()
  | _ :: _ as spilling -> (
    List.iter spilling ~f:(fun reg -> State.add_spilled_nodes state reg);
    (* note: rewrite will remove the `spilling` registers from the "spilled"
       work list and set the field to unknown. *)
    match rewrite state cfg_with_liveness spilling ~reset:false with
    | false -> ()
    | true -> Cfg_with_liveness.invalidate_liveness cfg_with_liveness));
  Profile.record ~accumulate:true "main"
    (fun () -> main ~round:1 state cfg_with_liveness)
    ();
  (* note: slots need to be updated before prologue removal *)
  if irc_debug
  then
    Array.iteri (State.num_stack_slots state)
      ~f:(fun reg_class num_stack_slots ->
        log ~indent:1 "stack_slots[%d]=%d" reg_class num_stack_slots);
  update_stack_slots cfg_with_layout
    ~num_stack_slots:(State.num_stack_slots state);
  remove_prologue_if_not_required cfg_with_layout;
  update_register_locations ();
  update_live_fields cfg_with_layout
    (Cfg_with_liveness.liveness cfg_with_liveness);
  if irc_debug && irc_invariants
  then (
    log ~indent:0 "postcondition";
    postcondition cfg_with_layout ~allow_stack_operands:true);
  Array.iter all_precolored_regs ~f:(fun reg -> reg.Reg.degree <- 0);
  cfg_with_liveness
