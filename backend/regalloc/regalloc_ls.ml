[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils
open! Regalloc_ls_utils
module State = Regalloc_ls_state

let snapshot_for_fatal = ref None

module Utils = Regalloc_ls_utils

let rewrite :
    State.t ->
    Cfg_with_infos.t ->
    spilled_nodes:Reg.t list ->
    block_temporaries:bool ->
    unit =
 fun state cfg_with_infos ~spilled_nodes ~block_temporaries ->
  let _new_inst_temporaries, _new_block_temporaries, block_inserted =
    Regalloc_rewrite.rewrite_gen
      (module State)
      (module Utils)
      state cfg_with_infos ~spilled_nodes ~block_temporaries
  in
  Cfg_with_infos.invalidate_liveness cfg_with_infos;
  if block_inserted
  then Cfg_with_infos.invalidate_dominators_and_loop_infos cfg_with_infos

(* Equivalent to [build_intervals] in "backend/interval.ml". *)
let build_intervals : State.t -> Cfg_with_infos.t -> unit =
 fun state cfg_with_infos ->
  indent ();
  log "build_intervals";
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let liveness = Cfg_with_infos.liveness cfg_with_infos in
  let past_ranges : Interval.t Reg.Tbl.t = Reg.Tbl.create 123 in
  let current_ranges : Range.t Reg.Tbl.t = Reg.Tbl.create 123 in
  let add_range (reg : Reg.t) ({ begin_; end_ } as range : Range.t) : unit =
    match Reg.Tbl.find_opt past_ranges reg with
    | None ->
      Reg.Tbl.replace past_ranges reg
        { Interval.reg; begin_; end_; ranges = DLL.make_single range }
    | Some (interval : Interval.t) ->
      DLL.add_end interval.ranges range;
      interval.end_ <- end_
  in
  let update_range (reg : Reg.t) ~(begin_ : int) ~(end_ : int) : unit =
    match Reg.Tbl.find_opt current_ranges reg with
    | None -> Reg.Tbl.replace current_ranges reg { Range.begin_; end_ }
    | Some ({ begin_ = _; end_ = curr_end } as curr) ->
      if (begin_ asr 1) - (curr_end asr 1) <= 1
      then curr.end_ <- end_
      else (
        add_range reg curr;
        Reg.Tbl.replace current_ranges reg { Range.begin_; end_ })
  in
  (* Equivalent to [update_interval_position_by_instr] in
     "backend/interval.ml". *)
  let update_instr :
      type a.
      int ->
      a Cfg.instruction ->
      trap_handler:bool ->
      destroyed:Reg.t array ->
      unit =
   fun pos instr ~trap_handler ~destroyed ->
    let on = pos lsl 1 in
    let off = on + 1 in
    if trap_handler
    then
      Array.iter Proc.destroyed_at_raise ~f:(fun reg ->
          update_range reg ~begin_:on ~end_:on);
    instr.ls_order <- on;
    Array.iter instr.arg ~f:(fun reg -> update_range reg ~begin_:on ~end_:on);
    Array.iter instr.res ~f:(fun reg -> update_range reg ~begin_:off ~end_:off);
    let live = InstructionId.Tbl.find liveness instr.id in
    Reg.Set.iter (fun reg -> update_range reg ~begin_:on ~end_:off) live.across;
    Array.iter destroyed ~f:(fun reg -> update_range reg ~begin_:off ~end_:off)
  in
  let pos = ref 0 in
  (* Equivalent to [walk_instruction] in "backend/interval.ml".*)
  iter_instructions_dfs cfg_with_layout
    ~instruction:(fun ~trap_handler instr ->
      incr pos;
      update_instr !pos instr ~trap_handler
        ~destroyed:(Proc.destroyed_at_basic instr.desc))
    ~terminator:(fun ~trap_handler term ->
      incr pos;
      update_instr !pos term ~trap_handler
        ~destroyed:(Proc.destroyed_at_terminator term.desc);
      (* Increment a second time to be in line with upstream `Iend` instructions
         present at the end of every "block". *)
      incr pos);
  Reg.Tbl.iter (fun reg (range : Range.t) -> add_range reg range) current_ranges;
  if debug && Lazy.force verbose
  then
    Cfg.iter_blocks_dfs (Cfg_with_layout.cfg cfg_with_layout)
      ~f:(fun _label block ->
        indent ();
        log "(block %a)" Label.format block.start;
        log_body_and_terminator block.body block.terminator liveness;
        dedent ());
  State.update_intervals state past_ranges;
  dedent ()

type spilling_reg =
  | Spilling of Reg.t
  | Not_spilling

let allocate_stack_slot : Reg.t -> spilling_reg =
 fun reg ->
  indent ();
  log "spilling register %a" Printreg.reg reg;
  dedent ();
  Spilling reg

exception No_free_register

let allocate_free_register : State.t -> Interval.t -> spilling_reg =
 fun state interval ->
  let reg = interval.reg in
  match reg.loc with
  | Unknown -> (
    let reg_class = Reg_class.of_machtype reg.typ in
    let intervals = State.active state ~reg_class in
    let first_available = Reg_class.first_available_register reg_class in
    match Reg_class.num_available_registers reg_class with
    | 0 ->
      fatal "register class %a has no available registers" Reg_class.print
        reg_class
    | num_available_registers ->
      let available = Array.make num_available_registers true in
      let num_still_available = ref num_available_registers in
      let set_not_available (r : int) : unit =
        let idx = r - first_available in
        if available.(idx) then decr num_still_available;
        available.(idx) <- false;
        if !num_still_available = 0 then raise No_free_register
      in
      let set_not_available_if_valid_phys_reg (interval : Interval.t) : unit =
        match interval.reg.loc with
        | Reg r ->
          if r - first_available < num_available_registers
          then set_not_available r
        | Stack _ | Unknown -> ()
      in
      DLL.iter intervals.active_dll ~f:set_not_available_if_valid_phys_reg;
      Interval.DescEndList.iter intervals.active_sl
        ~f:set_not_available_if_valid_phys_reg;
      let remove_bound_overlapping (itv : Interval.t) : unit =
        match itv.reg.loc with
        | Reg r ->
          if r - first_available < num_available_registers
             && available.(r - first_available)
             && Interval.overlap itv interval
          then set_not_available r
        | Stack _ | Unknown -> ()
      in
      DLL.iter intervals.inactive_dll ~f:remove_bound_overlapping;
      DLL.iter intervals.fixed_dll ~f:remove_bound_overlapping;
      (* note: already removed, for transition *)
      Interval.DescEndList.iter intervals.inactive_sl
        ~f:remove_bound_overlapping;
      Interval.DescEndList.iter intervals.fixed_sl ~f:remove_bound_overlapping;
      let rec assign idx =
        if idx >= num_available_registers
        then Misc.fatal_error "No_free_register should have been raised earlier"
        else if available.(idx)
        then (
          reg.loc <- Reg (first_available + idx);
          Interval.DLL.insert_sorted intervals.active_dll interval;
          Interval.DescEndList.insert intervals.active_sl interval;
          if debug
          then (
            indent ();
            log "assigning %d to register %a" idx Printreg.reg reg;
            dedent ());
          Not_spilling)
        else assign (succ idx)
      in
      assign 0)
  | Reg _ | Stack _ -> Not_spilling

let allocate_blocked_register : State.t -> Interval.t -> spilling_reg =
 fun state interval ->
  let reg = interval.reg in
  let reg_class = Reg_class.of_machtype reg.typ in
  let intervals = State.active state ~reg_class in
  (* will change to `match Interval.DescEndList.hd intervals.active_sl with` *)
  match DLL.hd_cell intervals.active_dll with
  | Some hd_cell ->
    let hd_cell' = Interval.DescEndList.hd_cell intervals.active_sl in
    if Option.is_none hd_cell'
    then fatal "Regalloc_ls.allocate_blocked_register: missing skip list head";
    let hd_cell' = Option.get hd_cell' in
    let hd = DLL.value hd_cell in
    let hd' = Interval.DescEndList.value hd_cell' in
    if not (Interval.equal hd hd')
    then
      fatal
        "Regalloc_ls.allocate_blocked_register: inconsistent heads (dll=%a vs \
         sl=%a)"
        Interval.print hd Interval.print hd';
    let chk r =
      assert (same_reg_class r.Interval.reg hd.Interval.reg);
      Reg.same_loc r.Interval.reg hd.Interval.reg && Interval.overlap r interval
    in
    let dll_exists =
      DLL.exists ~f:chk intervals.fixed_dll
      || DLL.exists ~f:chk intervals.inactive_dll
    in
    let sl_exists =
      Interval.DescEndList.exists ~f:chk intervals.fixed_sl
      || Interval.DescEndList.exists ~f:chk intervals.inactive_sl
    in
    if not (Bool.equal dll_exists sl_exists)
    then
      fatal
        "Regalloc_ls.allocate_blocked_register: inconsistent lists \
         (dll_exists=%B sl_exists=%B)"
        dll_exists sl_exists;
    if hd.end_ > interval.end_ && not dll_exists
    then (
      (match hd.reg.loc with Reg _ -> () | Stack _ | Unknown -> assert false);
      interval.reg.loc <- hd.reg.loc;
      DLL.delete_curr hd_cell;
      Interval.DLL.insert_sorted intervals.active_dll interval;
      Interval.DescEndList.delete_curr hd_cell';
      Interval.DescEndList.insert intervals.active_sl interval;
      allocate_stack_slot hd.reg)
    else allocate_stack_slot reg
  | None ->
    if Option.is_some (Interval.DescEndList.hd_cell intervals.active_sl)
    then
      fatal "Regalloc_ls.allocate_blocked_register: unexpected skip list head";
    allocate_stack_slot reg

let reg_reinit () =
  List.iter (Reg.all_relocatable_regs ()) ~f:(fun (reg : Reg.t) ->
      match reg.loc with Reg _ -> reg.loc <- Unknown | Unknown | Stack _ -> ())

(* CR xclerc for xclerc: could probably be lower; the compiler distribution
   seems to be fine with 3 *)
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
  reg_reinit ();
  State.check_consistency state "main/start";
  build_intervals state cfg_with_infos;
  State.check_consistency state "main/after build_intervals";
  State.invariant_intervals state cfg_with_infos;
  State.invariant_active state;
  if debug then snapshot_for_fatal := Some (State.for_fatal state);
  if debug
  then (
    log "iterating over intervals";
    indent ());
  let spilled =
    State.fold_intervals state ~init:Reg.Set.empty
      ~f:(fun acc (interval : Interval.t) ->
        State.check_consistency state "main/before release";
        (* Equivalent to [walk_interval] in "backend/linscan.ml".*)
        let pos = interval.begin_ land lnot 1 in
        if debug
        then log_interval ~kind:(Printf.sprintf "<pos=%d>" pos) interval;
        State.release_expired_intervals state ~pos;
        State.check_consistency state "main/after release";
        let spilled =
          match allocate_free_register state interval with
          | spilled ->
            State.check_consistency state "main/after allocate_free_register";
            spilled
          | exception No_free_register ->
            allocate_blocked_register state interval
        in
        State.check_consistency state "main/after allocation";
        match spilled with
        | Not_spilling -> acc
        | Spilling reg -> Reg.Set.add reg acc)
  in
  if debug
  then (
    dedent ();
    dedent ());
  if not (Reg.Set.is_empty spilled)
  then (
    rewrite state cfg_with_infos ~spilled_nodes:(Reg.Set.elements spilled)
      ~block_temporaries:(round = 1);
    main ~round:(succ round) state cfg_with_infos)

let run : Cfg_with_infos.t -> Cfg_with_infos.t =
 fun cfg_with_infos ->
  if debug then reset_indentation ();
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let cfg_infos, stack_slots =
    Regalloc_rewrite.prelude
      (module Utils)
      ~on_fatal_callback:(fun () ->
        Option.iter
          (fun (intervals_dll, intervals_sl, active) ->
            Format.eprintf "Regalloc_ls.run (on_fatal):";
            Format.eprintf "\n\nactives:\n";
            Reg_class.Tbl.iter active ~f:(fun reg_class a ->
                Format.eprintf "class %a:\n %a\n" Reg_class.print reg_class
                  ClassIntervals.print a);
            Format.eprintf "\n\nintervals_dll:\n";
            DLL.iter intervals_dll ~f:(fun i ->
                Format.eprintf "- %a\n" Interval.print i);
            Format.eprintf "\n\nintervals_sl:\n";
            Interval.AscBeginList.iter intervals_sl ~f:(fun i ->
                Format.eprintf "- %a\n" Interval.print i);
            Format.eprintf "\n%!")
          !snapshot_for_fatal;
        save_cfg "ls" cfg_with_layout)
      cfg_with_infos
  in
  let spilling_because_unused = Reg.Set.diff cfg_infos.res cfg_infos.arg in
  let state = State.make ~stack_slots in
  (match Reg.Set.elements spilling_because_unused with
  | [] -> ()
  | _ :: _ as spilled_nodes ->
    rewrite state cfg_with_infos ~spilled_nodes ~block_temporaries:false;
    Cfg_with_infos.invalidate_liveness cfg_with_infos);
  main ~round:1 state cfg_with_infos;
  Regalloc_rewrite.postlude
    (module State)
    (module Utils)
    state
    ~f:(fun () ->
      if debug && Lazy.force verbose
      then (
        let liveness = Cfg_with_infos.liveness cfg_with_infos in
        indent ();
        Cfg.iter_blocks_dfs (Cfg_with_layout.cfg cfg_with_layout)
          ~f:(fun _label block ->
            log "(block %a)" Label.format block.start;
            log_body_and_terminator block.body block.terminator liveness);
        dedent ()))
    cfg_with_infos;
  cfg_with_infos
