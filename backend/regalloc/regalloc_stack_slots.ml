[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
module DLL = Flambda_backend_utils.Doubly_linked_list

let debug = false

type slot = int

type t =
  { stack_slots : int Reg.Tbl.t;
    num_stack_slots : int Stack_class.Tbl.t
  }

let[@inline] make () =
  let stack_slots = Reg.Tbl.create 128 in
  let num_stack_slots = Stack_class.Tbl.make 0 in
  { stack_slots; num_stack_slots }

let iter t ~f = Reg.Tbl.iter f t.stack_slots

let[@inline] total_size_in_slots t =
  Stack_class.Tbl.total_size_in_slots t.num_stack_slots

let[@inline] get_and_incr t ~stack_class =
  let res = Stack_class.Tbl.find t.num_stack_slots stack_class in
  Stack_class.Tbl.replace t.num_stack_slots stack_class (succ res);
  res

let[@inline] get_or_create t reg =
  match Reg.Tbl.find_opt t.stack_slots reg with
  | Some slot -> slot
  | None ->
    let res =
      get_and_incr t ~stack_class:(Stack_class.of_machtype reg.Reg.typ)
    in
    Reg.Tbl.replace t.stack_slots reg res;
    res

let[@inline] get_or_fatal t reg =
  match Reg.Tbl.find_opt t.stack_slots reg with
  | None -> fatal "register %a has no associated slot" Printmach.reg reg
  | Some slot -> slot

let[@inline] use_same_slot_or_fatal t reg ~existing =
  match Reg.Tbl.find_opt t.stack_slots existing with
  | None -> fatal "register %a has no associated slot" Printmach.reg existing
  | Some slot -> Reg.Tbl.replace t.stack_slots reg slot

let[@inline] update_cfg_with_layout t cfg_with_layout =
  let fun_num_stack_slots =
    (Cfg_with_layout.cfg cfg_with_layout).fun_num_stack_slots
  in
  Stack_class.Tbl.copy_values ~from:t.num_stack_slots ~to_:fun_num_stack_slots

(** The optimization below is conceptually fairly close to what linscan does:
   - for each stack slot class / stack slot couple, we compute the interval of
     uses;
   - we re-assign slots by putting in the same "bucket" slots whose
     intervals do not overlap.

   It is also considerably simpler than linscan:
   - we do not distinguish the different kinds of uses (arg/res/live);
   - we do not track "holes" in the intervals;
   - we know that, by definition, we have enough slots to store
     everything and hence have no need to "restart" the computation. *)

(* CR-someday xclerc for xclerc: see whether parts could actually be shared with
   linscan. *)

let apply_reg_stack_local (reg : Reg.t) ~(f : slot -> unit) : unit =
  match reg.loc with
  | Unknown -> ()
  | Reg _ -> ()
  | Stack stack_loc -> (
    match stack_loc with
    | Local slot_index -> f slot_index
    | Incoming _ -> ()
    | Outgoing _ -> ()
    | Domainstate _ -> ())

module Point : sig
  type t =
    { layout_index : int; (* index of block in layout *)
      instruction_index : int (* index of instruction in block *)
    }

  val dummy : t

  val compare : t -> t -> int

  val print : Format.formatter -> t -> unit
end = struct
  type t =
    { layout_index : int;
      instruction_index : int
    }

  let dummy = { layout_index = -1; instruction_index = -1 }

  let compare
      { layout_index = left_layout_index;
        instruction_index = left_instruction_index
      }
      { layout_index = right_layout_index;
        instruction_index = right_instruction_index
      } : int =
    match Int.compare left_layout_index right_layout_index with
    | 0 -> Int.compare left_instruction_index right_instruction_index
    | c -> c

  let print ppf t =
    Format.fprintf ppf "%d:%d" t.layout_index t.instruction_index
end

module Interval : sig
  type t =
    { mutable start : Point.t; (* inclusive *)
      mutable end_ : Point.t (* inclusive *)
    }

  val overlap : t -> t -> bool

  val print : Format.formatter -> t -> unit
end = struct
  type t =
    { mutable start : Point.t;
      mutable end_ : Point.t
    }

  let overlap left right =
    Point.compare left.start right.end_ <= 0
    && Point.compare left.end_ right.start >= 0

  let print ppf t =
    Format.fprintf ppf "[%a..%a]" Point.print t.start Point.print t.end_
end

module Intervals : sig
  type slots = t

  type t = Interval.t array Stack_class.Tbl.t
  (* table key is stack slot class, array index is slot index *)

  val build_from_cfg : slots -> Cfg_with_infos.t -> t

  val print : Format.formatter -> t -> unit
end
with type slots := t = struct
  type t = Interval.t array Stack_class.Tbl.t

  let make slots =
    Stack_class.Tbl.init ~f:(fun stack_class ->
        Array.init (Stack_class.Tbl.find slots.num_stack_slots stack_class)
          ~f:(fun _ -> { Interval.start = Point.dummy; end_ = Point.dummy }))

  let visit_reg (t : t) (point : Point.t) (reg : Reg.t) : unit =
    apply_reg_stack_local reg ~f:(fun slot_index ->
        let stack_class = Stack_class.of_machtype reg.typ in
        let interval = (Stack_class.Tbl.find t stack_class).(slot_index) in
        if interval.start == Point.dummy then interval.start <- point;
        interval.end_ <- point)

  let visit_array (t : t) (point : Point.t) (regs : Reg.t array) : unit =
    Array.iter regs ~f:(fun reg -> visit_reg t point reg)

  let visit_set (t : t) (point : Point.t) (regs : Reg.Set.t) : unit =
    Reg.Set.iter (fun reg -> visit_reg t point reg) regs

  (* CR-someday xclerc for xclerc: since we are only interested in the very
     first and the very last occurrences and are working on a doubly-linked
     list, the computation could start from both ends. *)
  let build_from_cfg slots cfg_with_infos =
    let intervals = make slots in
    let live_across (id : Instruction.id) : Reg.Set.t =
      match Cfg_with_infos.liveness_find_opt cfg_with_infos id with
      | None -> fatal "missing liveness information for instruction %d" id
      | Some { before = _; across } -> across
    in
    let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    let visit_instr (type a) (point : Point.t) (instr : a Cfg.instruction) :
        unit =
      visit_array intervals point instr.arg;
      visit_array intervals point instr.res;
      visit_set intervals point (live_across instr.id)
    in
    DLL.iteri (Cfg_with_layout.layout cfg_with_layout)
      ~f:(fun layout_index label ->
        let block = Cfg.get_block_exn cfg label in
        DLL.iteri block.body ~f:(fun instruction_index instr ->
            visit_instr { Point.layout_index; instruction_index } instr);
        visit_instr
          { Point.layout_index; instruction_index = DLL.length block.body }
          block.terminator);
    intervals

  let print ppf t =
    Stack_class.Tbl.iter t ~f:(fun stack_class intervals ->
        Array.iteri intervals ~f:(fun slot_index interval ->
            Format.fprintf ppf "stack_class=%a slot_index=%d -> %a\n"
              Stack_class.print stack_class slot_index Interval.print interval))
end

module Int = Numbers.Int

module Buckets : sig
  type slots = t

  type t

  val build_from_intervals : slots -> Intervals.t -> t

  val contains_empty : t -> bool

  val find_bucket :
    t -> stack_class:Stack_class.t -> slot_index:slot -> int option

  val print : Format.formatter -> t -> unit
end
with type slots := t = struct
  type t = Interval.t Int.Tbl.t array Stack_class.Tbl.t
  (* outer table key is stack slot class, array index is bucket index, inner
     table key is slot index *)

  let does_not_fit (bucket : Interval.t Int.Tbl.t) (interval : Interval.t) :
      bool =
    try
      Int.Tbl.iter
        (fun _ bucket_interval ->
          if Interval.overlap bucket_interval interval then raise Exit)
        bucket;
      false
    with Exit -> true

  let build_from_intervals slots intervals =
    let buckets =
      Stack_class.Tbl.init ~f:(fun stack_class ->
          let num_slots =
            Stack_class.Tbl.find slots.num_stack_slots stack_class
          in
          Array.init num_slots ~f:(fun _ -> Int.Tbl.create num_slots))
    in
    Stack_class.Tbl.iter intervals ~f:(fun stack_class intervals ->
        Array.iteri intervals ~f:(fun slot_index interval ->
            let buckets = Stack_class.Tbl.find buckets stack_class in
            let bucket_index = ref 0 in
            while
              !bucket_index < Array.length buckets
              && does_not_fit buckets.(!bucket_index) interval
            do
              incr bucket_index
            done;
            assert (!bucket_index < Array.length buckets);
            Int.Tbl.replace buckets.(!bucket_index) slot_index interval));
    buckets

  let contains_empty t =
    (* Given how we add slots to buckets, empty buckets are at the end *)
    Stack_class.Tbl.exists t
      ~f:(fun _stack_class (buckets : Interval.t Int.Tbl.t array) ->
        match Array.length buckets with
        | 0 -> false
        | len ->
          let last_bucket = buckets.(len - 1) in
          Int.Tbl.length last_bucket = 0)

  let find_bucket t ~stack_class ~slot_index =
    let buckets = Stack_class.Tbl.find t stack_class in
    let len = Array.length buckets in
    let bucket_index = ref 0 in
    while
      !bucket_index < len
      && not (Int.Tbl.mem buckets.(!bucket_index) slot_index)
    do
      incr bucket_index
    done;
    if !bucket_index < len then Some !bucket_index else None

  let print ppf t =
    Stack_class.Tbl.iter t ~f:(fun stack_class buckets ->
        Array.iteri buckets ~f:(fun bucket_index bucket ->
            Format.fprintf ppf "stack_class=%a bucket_index=%d\n"
              Stack_class.print stack_class bucket_index;
            Int.Tbl.iter
              (fun slot_index interval ->
                Format.fprintf ppf "  . slot_index=%d %a\n" slot_index
                  Interval.print interval)
              bucket))
end

let optimization_enabled (t : t) : bool =
  match total_size_in_slots t with
  | 0 -> false
  | total_num_slots -> (
    match find_param_value "STACK_SLOTS_THRESHOLD" with
    | None -> true
    | Some stack_slots_threshold ->
      total_num_slots < int_of_string stack_slots_threshold)

let optimize (t : t) (cfg_with_infos : Cfg_with_infos.t) : unit =
  if optimization_enabled t
  then (
    (* First, compute the intervals for all stack slots *)
    let intervals = Intervals.build_from_cfg t cfg_with_infos in
    if debug then Format.eprintf "intervals:\n%a%!" Intervals.print intervals;
    (* Second, put the intervals into buckets *)
    let buckets = Buckets.build_from_intervals t intervals in
    if debug then Format.eprintf "buckets:\n%a%!" Buckets.print buckets;
    (* Third, check whether we have less non-empty buckets than original
       slots *)
    let optimized = Buckets.contains_empty buckets in
    if debug then Format.eprintf "optimized=%B\n%!" optimized;
    (* Finally, if so, reassign the slot indices *)
    if optimized
    then (
      let max_bucket_indices = Stack_class.Tbl.make (-1) in
      List.iter (Reg.all_registers ()) ~f:(fun (reg : Reg.t) ->
          apply_reg_stack_local reg ~f:(fun slot_index ->
              let stack_class = Stack_class.of_machtype reg.typ in
              match Buckets.find_bucket buckets ~stack_class ~slot_index with
              | None ->
                fatal "slot %d (stack_class=%a) is not in any of the buckets"
                  slot_index Stack_class.print stack_class
              | Some bucket_index ->
                if debug
                then
                  Format.eprintf
                    "changing the slot index of %a (class %a): %d ~> %d\n%!"
                    Printmach.reg reg Stack_class.print stack_class slot_index
                    bucket_index;
                reg.loc <- Stack (Local bucket_index);
                Stack_class.Tbl.update max_bucket_indices stack_class
                  ~f:(fun curr -> Stdlib.Int.max curr bucket_index);
                if Reg.Tbl.mem t.stack_slots reg
                then Reg.Tbl.replace t.stack_slots reg bucket_index));
      List.iter Stack_class.all ~f:(fun stack_class ->
          let old_value = Stack_class.Tbl.find t.num_stack_slots stack_class in
          let new_value =
            succ (Stack_class.Tbl.find max_bucket_indices stack_class)
          in
          if new_value > old_value
          then
            fatal "more slots are now used for class %a (before: %d, after: %d)"
              Stack_class.print stack_class old_value new_value;
          if debug
          then
            Format.eprintf "stack_class %a has %d fewer slots (%d ~> %d)\n%!"
              Stack_class.print stack_class (old_value - new_value) old_value
              new_value;
          Stack_class.Tbl.replace t.num_stack_slots stack_class new_value);
      Cfg_with_infos.invalidate_liveness cfg_with_infos))
