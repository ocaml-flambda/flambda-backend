[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils
open! Regalloc_ls_utils
module DLL = Oxcaml_utils.Doubly_linked_list

type t =
  { interval_dll : Interval.t DLL.t;
    active : ClassIntervals.t Reg_class.Tbl.t;
    stack_slots : Regalloc_stack_slots.t
  }

let for_fatal t =
  ( DLL.map t.interval_dll ~f:Interval.copy,
    Reg_class.Tbl.map t.active ~f:ClassIntervals.copy )

let[@inline] make ~stack_slots =
  let interval_dll = DLL.make_empty () in
  let active =
    Reg_class.Tbl.init ~f:(fun _reg_class -> ClassIntervals.make ())
  in
  { interval_dll; active; stack_slots }

(* CR-someday xclerc: consider using Dynarray *)
type class_interval_array =
  { mutable elements : Interval.t array;
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
  { elements = Array.make 32 dummy_interval; length = 0 }

let add_class_interval_array ci x =
  if ci.length >= Array.length ci.elements
  then (
    let old = ci.elements in
    let len = Array.length old in
    ci.elements <- Array.make (Int.max ci.length (2 * len)) dummy_interval;
    Array.blit ~src:old ~dst:ci.elements ~src_pos:0 ~dst_pos:0 ~len);
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
  let active : ClassIntervals.t Reg_class.Tbl.t = state.active in
  Reg_class.Tbl.iter active ~f:(fun _regclass intervals ->
      ClassIntervals.clear intervals);
  let active' : class_interval_array Reg_class.Tbl.t =
    Reg_class.Tbl.init ~f:(fun _ -> make_class_interval_array ())
  in
  let class_intervals = make_class_interval_array () in
  Reg.Tbl.iter
    (fun reg interval ->
      match reg.loc with
      | Reg _ ->
        let reg_class = Reg_class.of_machtype reg.typ in
        add_class_interval_array (Reg_class.Tbl.find active' reg_class) interval
      | Stack _ | Unknown -> add_class_interval_array class_intervals interval)
    map;
  let class_intervals = extract_class_interval_array class_intervals in
  Array.sort ~cmp:compare_asc_begin class_intervals;
  DLL.clear state.interval_dll;
  DLL.add_array state.interval_dll class_intervals;
  if debug then log_interval_dll ~kind:"regular" state.interval_dll;
  Reg_class.Tbl.iter active'
    ~f:(fun reg_class (intervals : class_interval_array) ->
      let extracted = extract_class_interval_array intervals in
      Array.sort extracted ~cmp:compare_desc_end;
      DLL.clear (Reg_class.Tbl.find active reg_class).fixed_dll;
      DLL.add_array (Reg_class.Tbl.find active reg_class).fixed_dll extracted)

let[@inline] iter_intervals state ~f = DLL.iter state.interval_dll ~f

let[@inline] fold_intervals state ~f ~init =
  DLL.fold_left state.interval_dll ~f ~init

let[@inline] release_expired_intervals state ~pos =
  Reg_class.Tbl.iter state.active ~f:(fun _ x ->
      ClassIntervals.release_expired_intervals x ~pos)

let[@inline] active state ~reg_class = Reg_class.Tbl.find state.active reg_class

let[@inline] active_classes state = state.active

let[@inline] stack_slots state = state.stack_slots

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

let invariant_field_dll (reg_class : Reg_class.t) (field_name : string)
    (l : Interval.t DLL.t) =
  let rec is prev curr =
    match curr with
    | None -> ()
    | Some cell ->
      let value = DLL.value cell in
      if value.Interval.end_ > prev.Interval.end_
      then
        fatal
          "Regalloc_ls_state.invariant_field_dll: active.(%a).%s is not sorted"
          Reg_class.print reg_class field_name
      else is value (DLL.next cell)
  in
  match DLL.hd_cell l with
  | None -> ()
  | Some cell -> is (DLL.value cell) (DLL.next cell)

let[@inline] invariant_active state =
  if debug && Lazy.force invariants
  then
    Reg_class.Tbl.iter state.active ~f:(fun reg_class intervals ->
        invariant_field_dll reg_class "fixed "
          intervals.ClassIntervals.fixed_dll;
        invariant_field_dll reg_class "active "
          intervals.ClassIntervals.active_dll;
        invariant_field_dll reg_class "inactive "
          intervals.ClassIntervals.inactive_dll)
