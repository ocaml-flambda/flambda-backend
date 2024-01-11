[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
open! Regalloc_gi_utils
module State = Regalloc_gi_state

module Utils = struct
  include Regalloc_gi_utils

  let debug = gi_debug

  let invariants = gi_invariants

  let log = log

  let log_body_and_terminator = log_body_and_terminator

  let is_spilled reg = reg.Reg.spill

  let set_spilled _reg = ()
end

let rewrite : State.t -> Cfg_with_infos.t -> spilled_nodes:Reg.t list -> bool =
 fun state cfg_with_infos ~spilled_nodes ->
  let new_temporaries, block_inserted =
    Regalloc_rewrite.rewrite_gen
      (module State)
      (module Utils)
      state cfg_with_infos ~spilled_nodes
  in
  if new_temporaries <> []
  then Cfg_with_infos.invalidate_liveness cfg_with_infos;
  if block_inserted
  then Cfg_with_infos.invalidate_dominators_and_loop_infos cfg_with_infos;
  match new_temporaries with
  | [] -> false
  | _ :: _ ->
    State.add_introduced_temporaries_list state new_temporaries;
    State.clear_assignments state;
    true

let update_register_locations : State.t -> unit =
 fun state ->
  if gi_debug then log ~indent:0 "update_register_locations";
  let update_register (reg : Reg.t) : unit =
    match reg.Reg.loc with
    | Reg _ -> ()
    | Stack _ -> ()
    | Unknown -> (
      match State.find_assignment state reg with
      | None ->
        (* a register may "disappear" because of split/rename *)
        ()
      | Some location ->
        if gi_debug
        then
          log ~indent:1 "updating %a to %a" Printmach.reg reg
            Hardware_register.print_location location;
        reg.Reg.loc <- Hardware_register.reg_location_of_location location)
  in
  List.iter (Reg.all_registers ()) ~f:update_register

module Prio_queue = Make_max_priority_queue (Int)

type prio_queue = (Reg.t * Interval.t) Prio_queue.t

let priority_heuristics : Reg.t -> Interval.t -> int =
 fun _reg itv ->
  match Lazy.force Priority_heuristics.value with
  | Priority_heuristics.Interval_length -> Interval.length itv

let make_hardware_registers_and_prio_queue (cfg_with_infos : Cfg_with_infos.t) :
    Hardware_registers.t * prio_queue =
  if gi_debug then log ~indent:0 "creating registers and queue";
  let intervals = build_intervals cfg_with_infos in
  let hardware_registers = Hardware_registers.make () in
  let prio_queue =
    (* CR-soon xclerc for xclerc: use the number of temporaries. *)
    Prio_queue.make ~initial_capacity:256
  in
  Reg.Tbl.iter
    (fun reg interval ->
      match reg.loc with
      | Reg _ ->
        if gi_debug
        then (
          log ~indent:1 "pre-assigned register %a" Printmach.reg reg;
          log ~indent:2 "%a" Interval.print interval);
        let hardware_reg = Hardware_registers.of_reg hardware_registers reg in
        Hardware_register.add_non_evictable hardware_reg reg interval
      | Unknown ->
        let priority = priority_heuristics reg interval in
        if gi_debug
        then (
          log ~indent:1 "register %a" Printmach.reg reg;
          log ~indent:2 "%a" Interval.print interval;
          log ~indent:2 "priority=%d" priority);
        Prio_queue.add prio_queue ~priority ~data:(reg, interval)
      | Stack _ ->
        if gi_debug
        then (
          log ~indent:1 "stack register %a" Printmach.reg reg;
          log ~indent:2 "%a" Interval.print interval);
        ())
    intervals;
  hardware_registers, prio_queue

(* CR xclerc for xclerc: try to find a reasonable threshold. *)
let max_rounds = 32

(* CR xclerc for xclerc: the `round` parameter is temporary; this is an hybrid
   version of "greedy" using the `rewrite` function from IRC when it needs to
   spill. *)
let rec main : round:int -> State.t -> Cfg_with_infos.t -> unit =
 fun ~round state cfg_with_infos ->
  if round > max_rounds
  then
    fatal "register allocation was not succesful after %d rounds (%s)"
      max_rounds (Cfg_with_infos.cfg cfg_with_infos).fun_name;
  if gi_debug
  then (
    log ~indent:0 "main, round #%d" round;
    log_cfg_with_infos ~indent:0 cfg_with_infos);
  if gi_debug then log ~indent:0 "updating spilling costs";
  let flat =
    match Lazy.force Spilling_heuristics.value with
    | Flat_uses -> true
    | Hierarchical_uses -> false
  in
  update_spill_cost cfg_with_infos ~flat ();
  State.iter_introduced_temporaries state ~f:(fun (reg : Reg.t) ->
      reg.Reg.spill_cost <- reg.Reg.spill_cost + 10_000);
  if gi_debug
  then (
    log ~indent:0 "spilling costs";
    List.iter (Reg.all_registers ()) ~f:(fun (reg : Reg.t) ->
        reg.Reg.spill <- false;
        log ~indent:1 "%a: %d" Printmach.reg reg reg.spill_cost));
  let hardware_registers, prio_queue =
    make_hardware_registers_and_prio_queue cfg_with_infos
  in
  let step = ref 0 in
  let spilling = ref ([] : (Reg.t * Interval.t) list) in
  while not (Prio_queue.is_empty prio_queue) do
    incr step;
    if gi_debug
    then log ~indent:1 "step #%d (size=%d)" !step (Prio_queue.size prio_queue);
    let { Prio_queue.priority; data = reg, interval } =
      Prio_queue.get_and_remove prio_queue
    in
    if gi_debug
    then log ~indent:2 "got register %a (prio=%d)" Printmach.reg reg priority;
    match Hardware_registers.find_available hardware_registers reg interval with
    | For_assignment { hardware_reg } ->
      if gi_debug
      then
        log ~indent:3 "assigning %a to %a" Printmach.reg reg
          Hardware_register.print_location hardware_reg.location;
      State.add_assignment state reg ~to_:hardware_reg.location;
      hardware_reg.assigned
        <- { Hardware_register.pseudo_reg = reg; interval; evictable = true }
           :: hardware_reg.assigned
    | For_eviction { hardware_reg; evicted_regs } ->
      if gi_debug
      then
        log ~indent:3 "evicting %a from %a" Printmach.regs
          (Array.of_list
             (List.map evicted_regs
                ~f:(fun { Hardware_register.pseudo_reg; _ } -> pseudo_reg)))
          Hardware_register.print_location hardware_reg.location;
      List.iter evicted_regs
        ~f:(fun
             { Hardware_register.pseudo_reg = evict_reg;
               interval = evict_interval;
               evictable
             }
           ->
          if not evictable
          then
            fatal
              "register %a has been picked up for eviction, but is not \
               evictable"
              Printmach.reg evict_reg;
          State.remove_assignment state evict_reg;
          Prio_queue.add prio_queue
            ~priority:(priority_heuristics evict_reg evict_interval)
            ~data:(evict_reg, evict_interval));
      State.add_assignment state reg ~to_:hardware_reg.location;
      (* CR xclerc for xclerc: very inefficient. *)
      hardware_reg.assigned
        <- { Hardware_register.pseudo_reg = reg; interval; evictable = true }
           :: List.filter hardware_reg.assigned
                ~f:(fun { Hardware_register.pseudo_reg = r; _ } ->
                  not
                    (List.exists evicted_regs
                       ~f:(fun { Hardware_register.pseudo_reg = r'; _ } ->
                         Reg.same r r')))
    | Split_or_spill ->
      (* CR xclerc for xclerc: we should actually try to split. *)
      if gi_debug then log ~indent:3 "spilling %a" Printmach.reg reg;
      reg.Reg.spill <- true;
      spilling := (reg, interval) :: !spilling
  done;
  match !spilling with
  | [] -> ()
  | _ :: _ as spilled_nodes -> (
    if gi_debug
    then (
      log_cfg_with_infos ~indent:0 cfg_with_infos;
      log ~indent:1 "stack slots";
      Regalloc_stack_slots.iter (State.stack_slots state)
        ~f:(fun (reg : Reg.t) (slot : int) ->
          log ~indent:2 "  - %a ~> %d" Printmach.reg reg slot);
      log ~indent:1 "needs to spill %d registers:" (List.length !spilling);
      List.iter !spilling ~f:(fun (_reg, interval) ->
          log ~indent:2 "  - %a" Interval.print interval);
      Cfg.iter_blocks (Cfg_with_infos.cfg cfg_with_infos)
        ~f:(fun (_ : Label.t) (block : Cfg.basic_block) ->
          let occurs =
            List.exists spilled_nodes ~f:(fun (reg, _) ->
                occurs_block block reg)
          in
          if occurs
          then (
            let dummy_liveness_for_log = Cfg_dataflow.Instr.Tbl.create 12 in
            log ~indent:0 "block %d has an occurrence of a spilling register"
              block.start;
            log_body_and_terminator ~indent:1 block.body block.terminator
              dummy_liveness_for_log)));
    match
      rewrite state cfg_with_infos
        ~spilled_nodes:(List.map spilled_nodes ~f:fst)
    with
    | false -> if gi_debug then log ~indent:1 "(end of main)"
    | true -> main ~round:(succ round) state cfg_with_infos)

let run : Cfg_with_infos.t -> Cfg_with_infos.t =
 fun cfg_with_infos ->
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let cfg_infos, stack_slots =
    Regalloc_rewrite.prelude
      (module Utils)
      ~on_fatal_callback:(fun () -> save_cfg "gi" cfg_with_layout)
      cfg_with_infos
  in
  (* CR xclerc for xclerc: consider moving the computation of temporaries and
     the creation of the state to `prelude`. *)
  let all_temporaries = Reg.Set.union cfg_infos.arg cfg_infos.res in
  if gi_debug
  then log ~indent:0 "#temporaries=%d" (Reg.Set.cardinal all_temporaries);
  let state =
    State.make ~stack_slots
      ~next_instruction_id:(succ cfg_infos.max_instruction_id)
  in
  let spilling_because_unused = Reg.Set.diff cfg_infos.res cfg_infos.arg in
  (match Reg.Set.elements spilling_because_unused with
  | [] -> ()
  | _ :: _ as spilled_nodes ->
    List.iter spilled_nodes ~f:(fun reg -> reg.Reg.spill <- true);
    (* note: rewrite will remove the `spilling` registers from the "spilled"
       work list and set the field to unknown. *)
    let (_ : bool) = rewrite state cfg_with_infos ~spilled_nodes in
    Cfg_with_infos.invalidate_liveness cfg_with_infos);
  main ~round:1 state cfg_with_infos;
  if gi_debug then log_cfg_with_infos ~indent:1 cfg_with_infos;
  Regalloc_rewrite.postlude
    (module State)
    (module Utils)
    state
    ~f:(fun () -> update_register_locations state)
    cfg_with_infos;
  cfg_with_infos
