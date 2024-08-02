[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
open! Regalloc_ls_utils

type t =
  { mutable intervals : Interval.t list;
    active : ClassIntervals.t array;
    stack_slots : Regalloc_stack_slots.t;
    mutable next_instruction_id : Instruction.id;
    mutable round_num : int
  }

let for_fatal t =
  ( List.map t.intervals ~f:Interval.copy,
    Array.map t.active ~f:ClassIntervals.copy )

let[@inline] make ~stack_slots ~next_instruction_id =
  let intervals = [] in
  let active =
    Array.init Proc.num_register_classes ~f:(fun _ -> ClassIntervals.make ())
  in
  let round_num = 1 in
  { intervals; active; stack_slots; next_instruction_id; round_num }

let[@inline] update_intervals state map =
  let active = state.active in
  Array.iter active ~f:ClassIntervals.clear;
  state.intervals
    <- Reg.Tbl.fold
         (fun reg interval acc ->
           match reg.loc with
           | Reg _ ->
             let reg_class = Proc.register_class reg in
             active.(reg_class).fixed <- interval :: active.(reg_class).fixed;
             acc
           | Stack _ | Unknown -> interval :: acc)
         map []
       |> List.sort ~cmp:(fun (left : Interval.t) (right : Interval.t) ->
              Int.compare left.begin_ right.begin_);
  if ls_debug then log_intervals ~indent:1 ~kind:"regular" state.intervals;
  Array.iter active ~f:(fun (intervals : ClassIntervals.t) ->
      intervals.fixed
        <- List.sort
             ~cmp:(fun (left : Interval.t) (right : Interval.t) ->
               Int.compare right.end_ left.end_)
             intervals.fixed;
      if ls_debug then log_intervals ~indent:1 ~kind:"fixed" intervals.fixed)

let[@inline] iter_intervals state ~f = List.iter state.intervals ~f

let[@inline] fold_intervals state ~f ~init =
  List.fold_left state.intervals ~f ~init

let[@inline] release_expired_intervals state ~pos =
  Array.iter state.active ~f:(fun x ->
      ClassIntervals.release_expired_intervals x ~pos)

let[@inline] active state ~reg_class = state.active.(reg_class)

let[@inline] active_classes state = state.active

let[@inline] stack_slots state = state.stack_slots

let[@inline] get_and_incr_instruction_id state =
  let res = state.next_instruction_id in
  state.next_instruction_id <- succ res;
  res

let[@inline] get_round_num state = state.round_num

let[@inline] incr_round_num state = state.round_num <- succ state.round_num

let rec check_ranges (prev : Range.t) (l : Range.t list) : int =
  if prev.begin_ > prev.end_
  then fatal "Regalloc_ls_state.check_ranges: prev.begin_ > prev.end_";
  match l with
  | [] -> prev.end_
  | hd :: tl ->
    if prev.end_ >= hd.begin_
    then fatal "Regalloc_ls_state.check_ranges: prev.end_ >= hd.begin_";
    check_ranges hd tl

let rec check_intervals (prev : Interval.t) (l : Interval.t list) : unit =
  if prev.begin_ > prev.end_
  then fatal "Regalloc_ls_state.check_intervals: prev.begin_ > prev.end_";
  (match prev.ranges with
  | [] -> fatal "Regalloc_ls_state.check_intervals: no ranges"
  | hd :: tl ->
    if hd.begin_ <> prev.begin_
    then fatal "Regalloc_ls_state.check_intervals: hd.begin_ <> prev.begin_";
    let end_ = check_ranges hd tl in
    if end_ <> prev.end_
    then fatal "Regalloc_ls_state.check_intervals: end_ <> prev.end_");
  match l with
  | [] -> ()
  | hd :: tl ->
    if prev.begin_ > hd.begin_
    then fatal "Regalloc_ls_state.check_intervals: prev.begin_ > hd.begin_";
    check_intervals hd tl

let rec is_in_a_range ls_order (l : Range.t list) : bool =
  match l with
  | [] -> false
  | hd :: tl ->
    (ls_order >= hd.begin_ && ls_order <= hd.end_) || is_in_a_range ls_order tl

let[@inline] invariant_intervals state cfg_with_infos =
  if ls_debug && Lazy.force ls_invariants
  then (
    (match state.intervals with [] -> () | hd :: tl -> check_intervals hd tl);
    let interval_map : Interval.t Reg.Map.t =
      fold_intervals state ~init:Reg.Map.empty ~f:(fun acc interval ->
          Reg.Map.update interval.reg
            (function
              | None -> Some interval
              | Some _reg ->
                fatal
                  "Regalloc_ls_state.invariant_intervals: state.intervals \
                   duplicate register %a"
                  Printmach.reg interval.reg)
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
              Printmach.reg reg
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
            if not (is_in_a_range instr.ls_order interval.ranges)
            then
              fatal
                "Regalloc_ls_state.invariant_intervals: not (is_in_a_range \
                 instr.ls_order interval.ranges)")
        instr.live
    in
    Cfg_with_layout.iter_instructions
      (Cfg_with_infos.cfg_with_layout cfg_with_infos)
      ~instruction:check_instr ~terminator:check_instr)

let invariant_active_field (reg_class : int) (field_name : string)
    (l : Interval.t list) =
  let rec is prev l =
    match l with
    | [] -> ()
    | hd :: tl ->
      if hd.Interval.end_ > prev.Interval.end_
      then
        fatal
          "Regalloc_ls_state.invariant_active_field: active.(%d).%s is not \
           sorted"
          reg_class field_name
      else is hd tl
  in
  match l with [] -> () | hd :: tl -> is hd tl

let[@inline] invariant_active state =
  if ls_debug && Lazy.force ls_invariants
  then
    Array.iteri state.active ~f:(fun reg_class intervals ->
        invariant_active_field reg_class "fixed " intervals.ClassIntervals.fixed;
        invariant_active_field reg_class "active "
          intervals.ClassIntervals.active;
        invariant_active_field reg_class "inactive "
          intervals.ClassIntervals.inactive)
