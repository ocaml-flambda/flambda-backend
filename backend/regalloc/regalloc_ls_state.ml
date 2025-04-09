[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils
open! Regalloc_ls_utils
module DLL = Flambda_backend_utils.Doubly_linked_list

type t =
  { interval_dll : Interval.t DLL.t;
    active : ClassIntervals.t array;
    stack_slots : Regalloc_stack_slots.t;
    instruction_id : InstructionId.sequence
  }

let print_intervals ppf (t : t) : unit =
  Format.fprintf ppf "interval_list(%d): %a\n"
    (List.length t.interval_list)
    Interval.List.print t.interval_list;
  Format.fprintf ppf "interval_dll(%d): %a\n"
    (DLL.length t.interval_dll)
    Interval.DLL.print t.interval_dll

let check_consistency t msg : unit =
  Array.iteri t.active ~f:(fun i ci ->
      ClassIntervals.check_consistency ci (Printf.sprintf "%s (idx=%d)" msg i));
  let consistent =
    equal_list_dll Interval.equal t.interval_list t.interval_dll
  in
  if not consistent
  then (
    print_intervals Format.err_formatter t;
    Misc.fatal_errorf "Regalloc_ls_state.check_consistency")

let for_fatal t =
  ( DLL.map t.interval_dll ~f:Interval.copy,
    Array.map t.active ~f:ClassIntervals.copy )

let[@inline] make ~stack_slots ~last_used =
  let interval_dll = DLL.make_empty () in
  let active =
    Array.init Proc.num_register_classes ~f:(fun _ -> ClassIntervals.make ())
  in
  let instruction_id = InstructionId.make_sequence ~last_used () in
  { interval_dll; active; stack_slots; instruction_id }

type class_interval_array =
  { elements : Interval.t array;
    mutable length : int
  }

let dummy_interval =
  { Interval.reg = Reg.dummy;
    begin_ = -1;
    end_ = -1;
    (* this list can be safely duplicated below, because the empty list is
       stateless *)
    ranges = DLL.make_empty ()
  }

let make_class_interval_array () =
  (* CR-soon xclerc for xclerc: this will essentially use twice the required
     memory *)
  { elements = Array.make (Reg.num_registers ()) dummy_interval; length = 0 }

let add_class_interval_array ci x =
  assert (ci.length < Array.length ci.elements);
  ci.elements.(ci.length) <- x;
  ci.length <- succ ci.length

let extract_class_interval_array ci : Interval.t array =
  Array.sub ci.elements ~pos:0 ~len:ci.length

let compare_asc_begin (left : Interval.t) (right : Interval.t) =
  match Int.compare left.begin_ right.begin_ with
  | 0 ->
    (* note: not necessary, used to enforce a unique order *)
    Reg.compare left.reg right.reg
  | c -> c

let compare_desc_end (left : Interval.t) (right : Interval.t) =
  match Int.compare right.end_ left.end_ with
  | 0 ->
    (* note: not necessary, used to enforce a unique order *)
    Reg.compare right.reg left.reg
  | c -> c

let[@inline] update_intervals state map =
  let active : ClassIntervals.t array = state.active in
  Array.iter active ~f:ClassIntervals.clear;
  let active' : class_interval_array array =
    Array.init (Array.length active) ~f:(fun _ -> make_class_interval_array ())
  in
  let class_intervals = make_class_interval_array () in
  Reg.Tbl.iter
    (fun reg interval ->
      match reg.loc with
      | Reg _ ->
        let reg_class = Proc.register_class reg in
        add_class_interval_array active'.(reg_class) interval
      | Stack _ | Unknown -> add_class_interval_array class_intervals interval)
    map;
  let class_intervals = extract_class_interval_array class_intervals in
  Array.sort ~cmp:compare_asc_begin class_intervals;
  DLL.clear state.interval_dll;
  DLL.add_array state.interval_dll class_intervals;
  if debug then log_interval_dll ~kind:"regular" state.interval_dll;
  Array.iteri active' ~f:(fun i (intervals : class_interval_array) ->
      let extracted = extract_class_interval_array intervals in
      Array.sort extracted ~cmp:compare_desc_end;
      DLL.clear active.(i).fixed_dll;
      DLL.add_array active.(i).fixed_dll extracted)

let[@inline] iter_intervals state ~f = DLL.iter state.interval_dll ~f

let[@inline] fold_intervals state ~f ~init =
  DLL.fold_left state.interval_dll ~f ~init

let[@inline] release_expired_intervals state ~pos =
  Array.iter state.active ~f:(fun x ->
      ClassIntervals.release_expired_intervals x ~pos)

let[@inline] active state ~reg_class = state.active.(reg_class)

let[@inline] active_classes state = state.active

let[@inline] stack_slots state = state.stack_slots

let[@inline] get_and_incr_instruction_id state =
  InstructionId.get_next state.instruction_id

let rec check_ranges (prev : Range.t) (cell : Range.t DLL.cell option) : int =
  if prev.begin_ > prev.end_
  then fatal "Regalloc_ls_state.check_ranges: prev.begin_ > prev.end_";
  match cell with
  | None -> prev.end_
  | Some cell ->
    let value = DLL.value cell in
    if prev.end_ >= value.begin_
    then fatal "Regalloc_ls_state.check_ranges: prev.end_ >= hd.begin_";
    check_ranges value (DLL.next cell)

let rec check_intervals (prev : Interval.t) (rest : Interval.t DLL.cell option)
    : unit =
  if prev.begin_ > prev.end_
  then fatal "Regalloc_ls_state.check_intervals: prev.begin_ > prev.end_";
  (match DLL.hd_cell prev.ranges with
  | None -> fatal "Regalloc_ls_state.check_intervals: no ranges"
  | Some cell ->
    let value = DLL.value cell in
    if value.begin_ <> prev.begin_
    then fatal "Regalloc_ls_state.check_intervals: hd.begin_ <> prev.begin_";
    let end_ = check_ranges value (DLL.next cell) in
    if end_ <> prev.end_
    then fatal "Regalloc_ls_state.check_intervals: end_ <> prev.end_");
  match rest with
  | None -> ()
  | Some hd_rest ->
    let hd = DLL.value hd_rest in
    if prev.begin_ > hd.begin_
    then fatal "Regalloc_ls_state.check_intervals: prev.begin_ > hd.begin_";
    check_intervals hd (DLL.next hd_rest)

let rec is_in_a_range ls_order (cell : Range.t DLL.cell option) : bool =
  match cell with
  | None -> false
  | Some cell ->
    let value = DLL.value cell in
    (ls_order >= value.begin_ && ls_order <= value.end_)
    || is_in_a_range ls_order (DLL.next cell)

let[@inline] invariant_intervals state cfg_with_infos =
  if debug && Lazy.force invariants
  then (
    (match DLL.hd_cell state.interval_dll with
    | None -> ()
    | Some hd_cell -> check_intervals (DLL.value hd_cell) (DLL.next hd_cell));
    let interval_map : Interval.t Reg.Map.t =
      fold_intervals state ~init:Reg.Map.empty ~f:(fun acc interval ->
          Reg.Map.update interval.reg
            (function
              | None -> Some interval
              | Some _reg ->
                fatal
                  "Regalloc_ls_state.invariant_intervals: state.intervals \
                   duplicate register %a"
                  Printreg.reg interval.reg)
            acc)
    in
    let check_instr : type a. a Cfg.instruction -> unit =
     fun instr ->
      Reg.Set.iter
        (fun reg ->
          match Reg.Map.find_opt reg interval_map with
          | None ->
            fatal
              "Regalloc_ls_state.invariant_intervals: register %a is not in \
               interval_map"
              Printreg.reg reg
          | Some interval ->
            if instr.ls_order < interval.begin_
            then
              fatal
                "Regalloc_ls_state.invariant_intervals: instr.ls_order < \
                 interval.begin_";
            if instr.ls_order > interval.end_
            then
              fatal
                "Regalloc_ls_state.invariant_intervals: instr.ls_order > \
                 interval.end_";
            if not (is_in_a_range instr.ls_order (DLL.hd_cell interval.ranges))
            then
              fatal
                "Regalloc_ls_state.invariant_intervals: not (is_in_a_range \
                 instr.ls_order interval.ranges)")
        instr.live
    in
    Cfg_with_layout.iter_instructions
      (Cfg_with_infos.cfg_with_layout cfg_with_infos)
      ~instruction:check_instr ~terminator:check_instr)

let invariant_field_dll (reg_class : int) (field_name : string)
    (l : Interval.t DLL.t) =
  let rec is prev curr =
    match curr with
    | None -> ()
    | Some cell ->
      let value = DLL.value cell in
      if value.Interval.end_ > prev.Interval.end_
      then
        fatal
          "Regalloc_ls_state.invariant_field_dll: active.(%d).%s is not sorted"
          reg_class field_name
      else is value (DLL.next cell)
  in
  match DLL.hd_cell l with
  | None -> ()
  | Some cell -> is (DLL.value cell) (DLL.next cell)

let invariant_field_dll (reg_class : int) (field_name : string)
    (l : Interval.t DLL.t) =
  let rec is prev curr =
    match curr with
    | None -> ()
    | Some cell ->
      let value = DLL.value cell in
      if value.Interval.end_ > prev.Interval.end_
      then
        fatal
          "Regalloc_ls_state.invariant_field_dll: active.(%d).%s is not sorted"
          reg_class field_name
      else is value (DLL.next cell)
  in
  match DLL.hd_cell l with
  | None -> ()
  | Some cell -> is (DLL.value cell) (DLL.next cell)

let[@inline] invariant_active state =
  if debug && Lazy.force invariants
  then
    Array.iteri state.active ~f:(fun reg_class intervals ->
        invariant_field_dll reg_class "fixed "
          intervals.ClassIntervals.fixed_dll;
        invariant_field_dll reg_class "active "
          intervals.ClassIntervals.active_dll;
        invariant_field_dll reg_class "inactive "
          intervals.ClassIntervals.inactive_dll)
