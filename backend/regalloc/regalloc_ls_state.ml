[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils
open! Regalloc_ls_utils
module DLL = Flambda_backend_utils.Doubly_linked_list

type t =
  { interval_sl : Interval.AscBeginList.t;
    active : ClassIntervals.t Reg_class.Tbl.t;
    stack_slots : Regalloc_stack_slots.t
  }

let print_intervals ppf (t : t) : unit =
  Format.fprintf ppf "interval_dll(%d):\n" (DLL.length t.interval_dll);
  DLL.iter t.interval_dll ~f:(fun i ->
      Format.fprintf ppf " %a\n" Interval.print i);
  Format.fprintf ppf "interval_sl(%d):\n"
    (Interval.AscBeginList.length t.interval_sl);
  Interval.AscBeginList.iter t.interval_sl ~f:(fun i ->
      Format.fprintf ppf " %a\n" Interval.print i);
  Format.fprintf ppf "\n%!"

let check_consistency t msg : unit =
  Reg_class.Tbl.iter t.active ~f:(fun reg_class ci ->
      ClassIntervals.check_consistency ci
        (Printf.sprintf "%s (reg_glass=%s)" msg (Reg_class.to_string reg_class)));
  let consistent = equal_dll_asc_sl t.interval_dll t.interval_sl in
  if not consistent
  then (
    print_intervals Format.err_formatter t;
    Misc.fatal_errorf "Regalloc_ls_state.check_consistency")

let for_fatal t =
  ( Interval.AscBeginList.map t.interval_sl ~f:Interval.copy,
    Reg_class.Tbl.map t.active ~f:ClassIntervals.copy )

let[@inline] make ~stack_slots =
  (* CR xclerc for xclerc: review the parameters. *)
  let interval_sl =
    Interval.AscBeginList.make_empty ~max_skip_level:7 ~skip_factor:0.5 ()
  in
  let active =
    Reg_class.Tbl.init ~f:(fun _reg_class -> ClassIntervals.make ())
  in
  { interval_sl; active; stack_slots }

let[@inline] update_intervals state map =
  check_consistency state "State.update_intervals/begin";
  let active : ClassIntervals.t Reg_class.Tbl.t = state.active in
  Reg_class.Tbl.iter active ~f:(fun _regclass intervals ->
      ClassIntervals.clear intervals);
  Interval.AscBeginList.clear state.interval_sl;
  Reg.Tbl.iter
    (fun reg interval ->
      match reg.loc with
      | Reg _ ->
        let reg_class = Reg_class.of_machtype reg.typ in
        Interval.DescEndList.insert
          (Reg_class.Tbl.find state.active reg_class).fixed_sl interval
      | Stack _ | Unknown ->
        Interval.AscBeginList.insert state.interval_sl interval)
    map;
  if debug then log_interval_asc_sl ~kind:"regular" state.interval_sl

let[@inline] iter_intervals state ~f =
  Interval.AscBeginList.iter state.interval_sl ~f

let[@inline] fold_intervals state ~f ~init =
  Interval.AscBeginList.fold_left state.interval_sl ~f ~init

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

let rec _check_intervals (prev : Interval.t) (rest : Interval.t DLL.cell option)
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
    _check_intervals hd (DLL.next hd_rest)

let rec _is_in_a_range ls_order (cell : Range.t DLL.cell option) : bool =
  match cell with
  | None -> false
  | Some cell ->
    let value = DLL.value cell in
    (ls_order >= value.begin_ && ls_order <= value.end_)
    || _is_in_a_range ls_order (DLL.next cell)

let[@inline] invariant_intervals _state _cfg_with_infos =
  (* TODO *)
  (* if debug && Lazy.force invariants then ( (match DLL.hd_cell
     state.interval_dll with | None -> () | Some hd_cell -> check_intervals
     (DLL.value hd_cell) (DLL.next hd_cell)); let interval_map : Interval.t
     Reg.Map.t = fold_intervals state ~init:Reg.Map.empty ~f:(fun acc interval
     -> Reg.Map.update interval.reg (function | None -> Some interval | Some
     _reg -> fatal "Regalloc_ls_state.invariant_intervals: state.intervals \
     duplicate register %a" Printreg.reg interval.reg) acc) in let check_instr :
     type a. a Cfg.instruction -> unit = fun instr -> Reg.Set.iter (fun reg ->
     match Reg.Map.find_opt reg interval_map with | None -> fatal
     "Regalloc_ls_state.invariant_intervals: register %a is not in \
     interval_map" Printreg.reg reg | Some interval -> if instr.ls_order <
     interval.begin_ then fatal "Regalloc_ls_state.invariant_intervals:
     instr.ls_order < \ interval.begin_"; if instr.ls_order > interval.end_ then
     fatal "Regalloc_ls_state.invariant_intervals: instr.ls_order > \
     interval.end_"; if not (is_in_a_range instr.ls_order (DLL.hd_cell
     interval.ranges)) then fatal "Regalloc_ls_state.invariant_intervals: not
     (is_in_a_range \ instr.ls_order interval.ranges)") instr.live in
     Cfg_with_layout.iter_instructions (Cfg_with_infos.cfg_with_layout
     cfg_with_infos) ~instruction:check_instr ~terminator:check_instr) *)
  ()
