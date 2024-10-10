[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
open! Regalloc_ls_utils
module State = Regalloc_ls_state

let snapshot_for_fatal = ref None

module Utils = struct
  include Regalloc_ls_utils

  let debug = ls_debug

  let invariants = ls_invariants

  let log = log

  let log_body_and_terminator = log_body_and_terminator

  let is_spilled reg = reg.Reg.spill

  let set_spilled _reg = ()
end

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
  log ~indent:1 "build_intervals";
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let liveness = Cfg_with_infos.liveness cfg_with_infos in
  let past_ranges : Interval.t Reg.Tbl.t = Reg.Tbl.create 123 in
  let current_ranges : Range.t Reg.Tbl.t = Reg.Tbl.create 123 in
  let add_range (reg : Reg.t) ({ begin_; end_ } as range : Range.t) : unit =
    match Reg.Tbl.find_opt past_ranges reg with
    | None ->
      Reg.Tbl.replace past_ranges reg
        { Interval.reg; begin_; end_; ranges = [range] }
    | Some (interval : Interval.t) ->
      interval.ranges <- range :: interval.ranges;
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
    let live = Cfg_dataflow.Instr.Tbl.find liveness instr.id in
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
  Reg.Tbl.iter
    (fun _reg (interval : Interval.t) ->
      interval.ranges <- List.rev interval.ranges)
    past_ranges;
  if ls_debug && Lazy.force ls_verbose
  then
    iter_cfg_dfs (Cfg_with_layout.cfg cfg_with_layout) ~f:(fun block ->
        log ~indent:2 "(block %d)" block.start;
        log_body_and_terminator ~indent:2 block.body block.terminator liveness);
  State.update_intervals state past_ranges

type spilling_reg =
  | Spilling of Reg.t
  | Not_spilling

let allocate_stack_slot : Reg.t -> spilling_reg =
 fun reg ->
  log ~indent:3 "spilling register %a" Printmach.reg reg;
  reg.spill <- true;
  Spilling reg

exception No_free_register

let allocate_free_register : State.t -> Interval.t -> spilling_reg =
 fun state interval ->
  let reg = interval.reg in
  match reg.loc, reg.spill with
  | Unknown, true -> allocate_stack_slot reg
  | Unknown, _ -> (
    let reg_class = Proc.register_class reg in
    let intervals = State.active state ~reg_class in
    let first_available = Proc.first_available_register.(reg_class) in
    match Proc.num_available_registers.(reg_class) with
    | 0 -> fatal "register class %d has no available registers" reg_class
    | num_available_registers ->
      let available = Array.make num_available_registers true in
      let num_still_available = ref num_available_registers in
      let set_not_available r =
        let idx = r - first_available in
        if idx < num_available_registers
        then (
          if available.(idx) then decr num_still_available;
          available.(idx) <- false;
          if !num_still_available = 0 then raise No_free_register)
      in
      List.iter intervals.active ~f:(fun (interval : Interval.t) ->
          match interval.reg.loc with
          | Reg r -> set_not_available r
          | Stack _ | Unknown -> ());
      let remove_bound_overlapping (itv : Interval.t) : unit =
        match itv.reg.loc with
        | Reg r ->
          if r - first_available < num_available_registers
             && available.(r - first_available)
             && Interval.overlap itv interval
          then set_not_available r
        | Stack _ | Unknown -> ()
      in
      List.iter intervals.inactive ~f:remove_bound_overlapping;
      List.iter intervals.fixed ~f:remove_bound_overlapping;
      let rec assign idx =
        if idx >= num_available_registers
        then raise No_free_register
        else if available.(idx)
        then (
          reg.loc <- Reg (first_available + idx);
          reg.spill <- false;
          intervals.active
            <- Interval.List.insert_sorted intervals.active interval;
          if ls_debug
          then log ~indent:3 "assigning %d to register %a" idx Printmach.reg reg;
          Not_spilling)
        else assign (succ idx)
      in
      assign 0)
  | (Reg _ | Stack _), _ -> Not_spilling

let allocate_blocked_register : State.t -> Interval.t -> spilling_reg =
 fun state interval ->
  let reg = interval.reg in
  let reg_class = Proc.register_class reg in
  let intervals = State.active state ~reg_class in
  match intervals.active with
  | hd :: tl ->
    let chk r =
      assert (same_reg_class r.Interval.reg hd.Interval.reg);
      Reg.same_loc r.Interval.reg hd.Interval.reg && Interval.overlap r interval
    in
    if hd.end_ > interval.end_
       && not
            (List.exists ~f:chk intervals.fixed
            || List.exists ~f:chk intervals.inactive)
    then (
      (match hd.reg.loc with Reg _ -> () | Stack _ | Unknown -> assert false);
      interval.reg.loc <- hd.reg.loc;
      intervals.active <- Interval.List.insert_sorted tl interval;
      allocate_stack_slot hd.reg)
    else allocate_stack_slot reg
  | [] -> allocate_stack_slot reg

let reg_reinit () =
  List.iter (Reg.all_registers ()) ~f:(fun (reg : Reg.t) ->
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
  if ls_debug then log ~indent:0 "main, round #%d" round;
  reg_reinit ();
  build_intervals state cfg_with_infos;
  State.invariant_intervals state cfg_with_infos;
  State.invariant_active state;
  if ls_debug then snapshot_for_fatal := Some (State.for_fatal state);
  if ls_debug then log ~indent:1 "iterating over intervals";
  let spilled =
    State.fold_intervals state ~init:Reg.Set.empty
      ~f:(fun acc (interval : Interval.t) ->
        (* Equivalent to [walk_interval] in "backend/linscan.ml".*)
        let pos = interval.begin_ land lnot 1 in
        if ls_debug
        then
          log_interval ~indent:2 ~kind:(Printf.sprintf "<pos=%d>" pos) interval;
        State.release_expired_intervals state ~pos;
        let spilled =
          match allocate_free_register state interval with
          | spilled -> spilled
          | exception No_free_register ->
            allocate_blocked_register state interval
        in
        State.invariant_active state;
        match spilled with
        | Not_spilling -> acc
        | Spilling reg -> Reg.Set.add reg acc)
  in
  if not (Reg.Set.is_empty spilled)
  then (
    rewrite state cfg_with_infos ~spilled_nodes:(Reg.Set.elements spilled)
      ~block_temporaries:(round = 1);
    main ~round:(succ round) state cfg_with_infos)

let run : Cfg_with_infos.t -> Cfg_with_infos.t =
 fun cfg_with_infos ->
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let cfg_infos, stack_slots =
    Regalloc_rewrite.prelude
      (module Utils)
      ~on_fatal_callback:(fun () ->
        Option.iter
          (fun (intervals, active) ->
            Format.eprintf "Regalloc_ls.run (on_fatal):";
            Format.eprintf "\n\nactives:\n";
            Array.iteri active ~f:(fun i a ->
                Format.eprintf "class %d:\n %a\n" i ClassIntervals.print a);
            Format.eprintf "\n\nintervals:\n";
            List.iter intervals ~f:(fun i ->
                Format.eprintf "- %a\n" Interval.print i);
            Format.eprintf "\n%!")
          !snapshot_for_fatal;
        save_cfg "ls" cfg_with_layout)
      cfg_with_infos
  in
  let spilling_because_unused = Reg.Set.diff cfg_infos.res cfg_infos.arg in
  let state =
    State.make ~stack_slots
      ~next_instruction_id:(succ cfg_infos.max_instruction_id)
  in
  (match Reg.Set.elements spilling_because_unused with
  | [] -> ()
  | _ :: _ as spilled_nodes ->
    List.iter spilled_nodes ~f:(fun reg -> reg.Reg.spill <- true);
    rewrite state cfg_with_infos ~spilled_nodes ~block_temporaries:false;
    Cfg_with_infos.invalidate_liveness cfg_with_infos);
  main ~round:1 state cfg_with_infos;
  Regalloc_rewrite.postlude
    (module State)
    (module Utils)
    state
    ~f:(fun () ->
      if ls_debug && Lazy.force ls_verbose
      then
        let liveness = Cfg_with_infos.liveness cfg_with_infos in
        iter_cfg_dfs (Cfg_with_layout.cfg cfg_with_layout) ~f:(fun block ->
            log ~indent:2 "(block %d)" block.start;
            log_body_and_terminator ~indent:2 block.body block.terminator
              liveness))
    cfg_with_infos;
  cfg_with_infos
