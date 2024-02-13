[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
module DLL = Flambda_backend_utils.Doubly_linked_list

let gi_debug = true

let gi_rng = Random.State.make [| 4; 6; 2 |]

let bool_of_param param_name =
  bool_of_param ~guard:(gi_debug, "gi_debug") param_name

let gi_verbose : bool Lazy.t = bool_of_param "GI_VERBOSE"

let gi_invariants : bool Lazy.t = bool_of_param "GI_INVARIANTS"

let log_function =
  lazy (make_log_function ~verbose:(Lazy.force gi_verbose) ~label:"gi")

let log :
    type a.
    indent:int -> ?no_eol:unit -> (a, Format.formatter, unit) format -> a =
 fun ~indent ?no_eol fmt -> (Lazy.force log_function).log ~indent ?no_eol fmt

let instr_prefix (instr : Cfg.basic Cfg.instruction) =
  Printf.sprintf "#%04d" instr.ls_order

let term_prefix (term : Cfg.terminator Cfg.instruction) =
  Printf.sprintf "#%04d" term.ls_order

let log_body_and_terminator :
    indent:int ->
    Cfg.basic_instruction_list ->
    Cfg.terminator Cfg.instruction ->
    liveness ->
    unit =
 fun ~indent body terminator liveness ->
  make_log_body_and_terminator (Lazy.force log_function) ~instr_prefix
    ~term_prefix ~indent body terminator liveness

let log_cfg_with_infos : indent:int -> Cfg_with_infos.t -> unit =
 fun ~indent cfg_with_infos ->
  make_log_cfg_with_infos (Lazy.force log_function) ~instr_prefix ~term_prefix
    ~indent cfg_with_infos

(* CR xclerc for xclerc: add more heuristics *)
module Priority_heuristics = struct
  type t =
    | Interval_length
    | Random_for_testing

  let all = [Interval_length; Random_for_testing]

  let to_string = function
    | Interval_length -> "interval_length"
    | Random_for_testing -> "random"

  let random () = Random.State.int gi_rng 10_000

  let value = lazy Random_for_testing
end

(* CR xclerc for xclerc: add more heuristics *)
module Selection_heuristics = struct
  type t =
    | First_available
    | Best_fit
    | Worst_fit
    | Random_for_testing

  let all = [First_available; Best_fit; Worst_fit; Random_for_testing]

  let to_string = function
    | First_available -> "first_available"
    | Best_fit -> "best_fit"
    | Worst_fit -> "worst_fit"
    | Random_for_testing -> "random"

  let include_in_random = function
    | Random_for_testing | Worst_fit -> false
    | _ -> true

  let random =
    let all = List.filter all ~f:include_in_random in
    let len = List.length all in
    fun () -> List.nth all (Random.State.int gi_rng len)

  let value = lazy Random_for_testing
end

module Spilling_heuristics = struct
  type t =
    | Flat_uses
    | Hierarchical_uses
    | Random_for_testing

  let all = [Flat_uses; Hierarchical_uses; Random_for_testing]

  let to_string = function
    | Flat_uses -> "flat_uses"
    | Hierarchical_uses -> "hierarchical_uses"
    | Random_for_testing -> "random"

  let random () = Random.State.bool gi_rng

  let value = lazy Random_for_testing
end

(* CR xclerc for xclerc: reuse `{Map,Set}.OrderedType`? *)
module type Order = sig
  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module type Priority_queue = sig
  type priority

  type 'a t

  type 'a element =
    { priority : priority;
      data : 'a
    }

  val make : initial_capacity:int -> 'a t

  val is_empty : 'a t -> bool

  val size : 'a t -> int

  val add : 'a t -> priority:priority -> data:'a -> unit

  val get : 'a t -> 'a element

  val remove : 'a t -> unit

  val get_and_remove : 'a t -> 'a element

  val iter : 'a t -> f:('a element -> unit) -> unit
end

(* CR xclerc for xclerc: some issues we might want to address with the
   implementation below: - it uses `Obj.magic`; - `elements` can only grow. *)
module Make_max_priority_queue (Priority : Order) :
  Priority_queue with type priority = Priority.t = struct
  type priority = Priority.t

  type 'a element =
    { priority : priority;
      data : 'a
    }

  let dummy = { priority = Obj.magic 0; data = Obj.magic 0 }

  let element_compare : 'a element -> 'a element -> int =
   fun left right ->
    assert (left != dummy);
    assert (right != dummy);
    Priority.compare left.priority right.priority

  type 'a t =
    { mutable size : int;
      mutable elements : 'a element array
    }

  let make : initial_capacity:int -> 'a t =
   fun ~initial_capacity ->
    let size = 0 in
    let elements = Array.make initial_capacity dummy in
    { size; elements }

  let is_empty : 'a t -> bool = fun queue -> queue.size = 0

  let size : 'a t -> int = fun queue -> queue.size

  let resize : 'a t -> unit =
   fun queue ->
    let current_capacity = Array.length queue.elements in
    let new_capacity =
      if current_capacity <= 2048
      then 2 * current_capacity
      else current_capacity + 2048
    in
    let new_elements = Array.make new_capacity dummy in
    Array.blit ~src:queue.elements ~src_pos:0 ~dst:new_elements ~dst_pos:0
      ~len:queue.size;
    queue.elements <- new_elements

  let parent : int -> int = fun i -> (i - 1) / 2

  let left_child : int -> int = fun i -> (2 * i) + 1

  let right_child : int -> int = fun i -> (2 * i) + 2

  let swap : 'a element array -> int -> int -> unit =
   fun arr i j ->
    assert (arr.(i) != dummy);
    assert (arr.(j) != dummy);
    let tmp = arr.(i) in
    arr.(i) <- arr.(j);
    arr.(j) <- tmp

  let upify : 'a element array -> start:int -> unit =
   fun arr ~start ->
    let i = ref start in
    while !i > 0 && element_compare arr.(!i) arr.(parent !i) > 0 do
      swap arr !i (parent !i);
      i := parent !i
    done

  let rec downify : 'a element array -> idx:int -> len:int -> unit =
   fun arr ~idx ~len ->
    let left = left_child idx in
    let right = right_child idx in
    let largest = ref idx in
    if left < len && element_compare arr.(left) arr.(!largest) > 0
    then largest := left;
    if right < len && element_compare arr.(right) arr.(!largest) > 0
    then largest := right;
    if !largest <> idx
    then (
      swap arr idx !largest;
      downify arr ~idx:!largest ~len)

  let rec add : 'a t -> priority:priority -> data:'a -> unit =
   fun queue ~priority ~data ->
    if Array.length queue.elements = queue.size
    then (
      resize queue;
      add queue ~priority ~data)
    else
      let elem = { priority; data } in
      let old_size = queue.size in
      Array.unsafe_set queue.elements old_size elem;
      queue.size <- succ old_size;
      upify queue.elements ~start:old_size

  let get : 'a t -> 'a element =
   fun queue ->
    match queue.size with
    | 0 -> fatal "trying to get an element from an empty priority queue"
    | _ ->
      let res = Array.unsafe_get queue.elements 0 in
      assert (res != dummy);
      res

  let remove : 'a t -> unit =
   fun queue ->
    match queue.size with
    | 0 -> fatal "trying to remove an element from an empty priority queue"
    | _ ->
      let old_size = queue.size in
      let index = pred old_size in
      swap queue.elements 0 index;
      queue.elements.(index) <- dummy;
      queue.size <- pred old_size;
      downify queue.elements ~idx:0 ~len:queue.size

  let get_and_remove : 'a t -> 'a element =
   fun queue ->
    match queue.size with
    | 0 ->
      fatal "trying to get and remove an element from an empty priority queue"
    | _ ->
      let res = Array.unsafe_get queue.elements 0 in
      assert (res != dummy);
      remove queue;
      res

  let iter : 'a t -> f:('a element -> unit) -> unit =
   fun queue ~f ->
    for i = 0 to pred queue.size do
      let elem = Array.unsafe_get queue.elements i in
      assert (elem != dummy);
      f elem
    done
end

let iter_cfg_layout : Cfg_with_layout.t -> f:(Cfg.basic_block -> unit) -> unit =
 fun cfg_with_layout ~f ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  DLL.iter (Cfg_with_layout.layout cfg_with_layout) ~f:(fun label ->
      let block = Cfg.get_block_exn cfg label in
      f block)

let iter_instructions_layout :
    Cfg_with_layout.t ->
    instruction:(trap_handler:bool -> Cfg.basic Cfg.instruction -> unit) ->
    terminator:(trap_handler:bool -> Cfg.terminator Cfg.instruction -> unit) ->
    unit =
 fun cfg_with_layout ~instruction ~terminator ->
  let f (block : Cfg.basic_block) =
    let trap_handler_id =
      if block.is_trap_handler
      then Regalloc_utils.first_instruction_id block
      else min_int
    in
    DLL.iter block.body ~f:(fun instr ->
        instruction ~trap_handler:(Int.equal instr.Cfg.id trap_handler_id) instr);
    terminator
      ~trap_handler:(Int.equal block.terminator.Cfg.id trap_handler_id)
      block.terminator
  in
  iter_cfg_layout cfg_with_layout ~f

(* CR xclerc for xclerc: the code below is largely copied from the linscan
   allocator, because it is likely tweaks will be needed to implement the "full"
   greedy allocator. However, some elements should be factored out once we know
   what is actually needed. *)

module Range = struct
  type t =
    { begin_ : int;
      mutable end_ : int
    }

  let length t = t.end_ - t.begin_ + 1

  let copy r = { begin_ = r.begin_; end_ = r.end_ }

  let print ppf r = Format.fprintf ppf "[%d,%d]" r.begin_ r.end_

  let rec overlap : t list -> t list -> bool =
   fun left right ->
    match left, right with
    | left_hd :: left_tl, right_hd :: right_tl ->
      if left_hd.end_ >= right_hd.begin_ && right_hd.end_ >= left_hd.begin_
      then true
      else if left_hd.end_ < right_hd.end_
      then overlap left_tl right
      else if left_hd.end_ > right_hd.end_
      then overlap left right_tl
      else overlap left_tl right_tl
    | [], _ | _, [] -> false

  let rec is_live : t list -> pos:int -> bool =
   fun l ~pos ->
    match l with
    | [] -> false
    | hd :: tl ->
      if pos < hd.begin_
      then false
      else if pos <= hd.end_
      then true
      else is_live tl ~pos

  let rec remove_expired : t list -> pos:int -> t list =
   fun l ~pos ->
    match l with
    | [] -> []
    | hd :: tl -> if pos < hd.end_ then l else remove_expired tl ~pos

  (* CR xclerc for xclerc: assumes no overlap *)
  let rec merge : t list -> t list -> t list =
   fun left right ->
    match left, right with
    | [], [] -> []
    | [], _ :: _ -> right
    | _ :: _, [] -> left
    | ( ({ begin_ = left_begin; end_ = _ } as left_hd) :: left_tl,
        ({ begin_ = right_begin; end_ = _ } as right_hd) :: right_tl ) ->
      if left_begin < right_begin
      then left_hd :: merge left_tl right
      else right_hd :: merge left right_tl
end

module Interval = struct
  type t =
    { mutable begin_ : int;
      mutable end_ : int;
      mutable ranges : Range.t list
    }

  let make_empty () =
    (* CR xclerc for xclerc: avoid the non-sensical bounds. *)
    { begin_ = max_int; end_ = max_int; ranges = [] }

  let length t =
    List.fold_left t.ranges ~init:0 ~f:(fun acc range ->
        acc + Range.length range)

  let print ppf t =
    Format.fprintf ppf "[%d,%d]:" t.begin_ t.end_;
    List.iter t.ranges ~f:(fun r -> Format.fprintf ppf " %a" Range.print r)

  let overlap : t -> t -> bool =
   (* CR xclerc for xclerc: short-cut to avoid iterating over the lists using
      the Interval.{begin_in_,end_} fields *)
   fun left right -> Range.overlap left.ranges right.ranges

  (* CR xclerc for xclerc: assumes no overlap *)
  let add_ranges : t -> from:t -> unit =
   fun t ~from ->
    t.begin_ <- Int.min t.begin_ from.begin_;
    t.end_ <- Int.min t.end_ from.end_;
    t.ranges <- Range.merge t.ranges from.ranges
end

let build_intervals : Cfg_with_infos.t -> Interval.t Reg.Tbl.t =
 fun cfg_with_infos ->
  if gi_debug then log ~indent:1 "build_intervals";
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let liveness = Cfg_with_infos.liveness cfg_with_infos in
  let past_ranges : Interval.t Reg.Tbl.t = Reg.Tbl.create 123 in
  let current_ranges : Range.t Reg.Tbl.t = Reg.Tbl.create 123 in
  let add_range (reg : Reg.t) ({ begin_; end_ } as range : Range.t) : unit =
    match Reg.Tbl.find_opt past_ranges reg with
    | None ->
      Reg.Tbl.replace past_ranges reg
        { Interval.begin_; end_; ranges = [range] }
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
      Array.iter (Proc.destroyed_at_raise ()) ~f:(fun reg ->
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
  iter_instructions_layout cfg_with_layout
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
  if gi_debug && Lazy.force gi_verbose
  then
    iter_cfg_layout cfg_with_layout ~f:(fun block ->
        log ~indent:2 "(block %d)" block.start;
        log_body_and_terminator ~indent:2 block.body block.terminator liveness);
  past_ranges

module Hardware_register = struct
  type location =
    { reg_class : int;
      reg_index_in_class : int
    }

  let make_location ~reg_class ~reg_index_in_class =
    if reg_class < 0 || reg_class >= Proc.num_register_classes
    then fatal "invalid register class: %d" reg_class;
    if reg_index_in_class < 0
       || reg_index_in_class >= Proc.num_available_registers.(reg_class)
    then
      fatal "invalid register index: %d (class=%d)" reg_index_in_class reg_class;
    { reg_class; reg_index_in_class }

  let print_location ppf { reg_class; reg_index_in_class } =
    Format.fprintf ppf "{ cls=%d; idx=%d }" reg_class reg_index_in_class

  let reg_location_of_location { reg_class; reg_index_in_class } =
    let reg_index =
      Proc.first_available_register.(reg_class) + reg_index_in_class
    in
    Reg.Reg reg_index

  type assigned =
    { pseudo_reg : Reg.t;
      interval : Interval.t;
      evictable : bool
    }

  let print_assigned ppf { pseudo_reg; interval; evictable } =
    Format.fprintf ppf "%a %a (evitable=%B)" Printmach.reg pseudo_reg
      Interval.print interval evictable

  type t =
    { location : location;
      interval : Interval.t;
      mutable assigned : assigned list
    }

  let add_non_evictable t reg interval =
    Interval.add_ranges t.interval ~from:interval;
    t.assigned
      <- { pseudo_reg = reg; interval; evictable = false } :: t.assigned
end

type available =
  | For_assignment of { hardware_reg : Hardware_register.t }
  | For_eviction of
      { hardware_reg : Hardware_register.t;
        evicted_regs : Hardware_register.assigned list
      }
  | Split_or_spill

module Hardware_registers = struct
  type t = Hardware_register.t array array
  (* first array index is register class, second array index is register
     index *)

  let make () =
    Array.init Proc.num_register_classes ~f:(fun reg_class ->
        let num_available_registers =
          Proc.num_available_registers.(reg_class)
        in
        Array.init num_available_registers ~f:(fun reg_index_in_class ->
            let location =
              Hardware_register.make_location ~reg_class ~reg_index_in_class
            in
            { Hardware_register.location;
              interval = Interval.make_empty ();
              assigned = []
            }))

  let of_reg (t : t) (reg : Reg.t) : Hardware_register.t =
    match reg.loc with
    | Reg reg_index ->
      let reg_class : int = Proc.register_class reg in
      let reg_index_in_class : int =
        reg_index - Proc.first_available_register.(reg_class)
      in
      t.(reg_class).(reg_index_in_class)
    | Unknown -> fatal "`Unknown` location (expected `Reg _`)"
    | Stack _ -> fatal "`Stack _` location (expected `Reg _`)"

  (* Modified Stdlib.Array.find_opt *)
  let find_opt a ~f ~limit =
    let n = Int.min (Array.length a) limit in
    let rec loop i =
      if i = n
      then None
      else
        let x = Array.unsafe_get a i in
        if f x then Some x else loop (succ i)
    in
    loop 0

  (* Modified Stdlib.Array.fold_left *)
  let fold_left a ~f ~init ~limit =
    let n = Int.min (Array.length a) limit in
    let r = ref init in
    for i = 0 to n - 1 do
      r := f !r (Array.unsafe_get a i)
    done;
    !r

  let find_in_class (t : t) ~(of_reg : Reg.t) ~(f : Hardware_register.t -> bool)
      =
    find_opt t.(Proc.register_class of_reg) ~f ~limit:!Arch.limit_regalloc

  let fold_class :
      type a.
      t -> of_reg:Reg.t -> f:(a -> Hardware_register.t -> a) -> init:a -> a =
   fun t ~of_reg ~f ~init ->
    fold_left
      t.(Proc.register_class of_reg)
      ~f ~init ~limit:!Arch.limit_regalloc

  let actual_cost (reg : Reg.t) : int =
    (* CR xclerc for xclerc: it could make sense to give a lower cost to reg
       already spilled (e.g. by the split preprocessing) since they already have
       a stack slot *)
    reg.Reg.spill_cost

  let overlap (hardware_reg : Hardware_register.t) (interval : Interval.t) :
      bool =
    if gi_debug
    then
      log ~indent:4 "considering %a" Hardware_register.print_location
        hardware_reg.location;
    let overlap_hard : bool = Interval.overlap interval hardware_reg.interval in
    let overlap_assigned =
      List.exists hardware_reg.assigned
        ~f:(fun
             { Hardware_register.pseudo_reg = _; interval = itv; evictable = _ }
           -> Interval.overlap itv interval)
    in
    let overlap = overlap_hard || overlap_assigned in
    if gi_debug
    then
      log ~indent:5 "overlap=%B (hard=%B, assigned=%B)" overlap overlap_hard
        overlap_assigned;
    overlap

  let find_first (t : t) (reg : Reg.t) (interval : Interval.t) :
      Hardware_register.t option =
    find_in_class t ~of_reg:reg ~f:(fun hardware_reg ->
        not (overlap hardware_reg interval))

  let find_using_length (t : t) (reg : Reg.t) (interval : Interval.t)
      ~(better : int -> int -> bool) : Hardware_register.t option =
    fold_class t ~of_reg:reg ~init:None ~f:(fun acc hardware_reg ->
        if overlap hardware_reg interval
        then acc
        else
          let length = Interval.length hardware_reg.interval in
          match acc with
          | None -> Some (hardware_reg, length)
          | Some (_, acc_length) ->
            if better length acc_length
            then Some (hardware_reg, length)
            else acc)
    |> Option.map fst

  let find_evictable (t : t) (reg : Reg.t) (interval : Interval.t) : available =
    let eviction =
      fold_class t ~of_reg:reg ~init:None ~f:(fun acc hardware_reg ->
          if gi_debug
          then
            log ~indent:4 "considering %a (length=%d)"
              Hardware_register.print_location hardware_reg.location
              (List.length hardware_reg.assigned);
          let overlap_hard = Interval.overlap interval hardware_reg.interval in
          if overlap_hard
          then acc
          else
            let overlaping : Hardware_register.assigned list =
              List.filter hardware_reg.assigned
                ~f:(fun
                     { Hardware_register.pseudo_reg;
                       interval = itv;
                       evictable = _
                     }
                   ->
                  let overlap = Interval.overlap interval itv in
                  if gi_debug
                  then
                    log ~indent:5 "%a is assigned / overlap=%B" Printmach.reg
                      pseudo_reg overlap;
                  overlap)
            in
            (match overlaping with
            | [] -> fatal "overlaping list should not be empty"
            | _ :: _ -> ());
            let (cost, evictable) : int * bool =
              List.fold_left overlaping ~init:(0, true)
                ~f:(fun
                     (acc_cost, acc_evictable)
                     { Hardware_register.pseudo_reg; interval = _; evictable }
                   ->
                  acc_cost + actual_cost pseudo_reg, acc_evictable && evictable)
            in
            if not evictable
            then acc
            else
              let evict_cost =
                match acc with None -> max_int | Some (_, _, c) -> c
              in
              if cost < evict_cost && cost < actual_cost reg
              then (
                if gi_debug
                then
                  List.iter overlaping ~f:(fun assigned ->
                      log ~indent:5 "evicting %a"
                        Hardware_register.print_assigned assigned);
                Some (hardware_reg, overlaping, cost))
              else acc)
    in
    match eviction with
    | Some (hardware_reg, evicted_regs, _) ->
      For_eviction { hardware_reg; evicted_regs }
    | None -> Split_or_spill

  let find_available : t -> Reg.t -> Interval.t -> available =
   fun t reg interval ->
    let with_no_overlap =
      let heuristic =
        match Lazy.force Selection_heuristics.value with
        | Selection_heuristics.Random_for_testing ->
          Selection_heuristics.random ()
        | heuristic -> heuristic
      in
      match heuristic with
      | Selection_heuristics.Random_for_testing -> assert false
      | Selection_heuristics.First_available ->
        if gi_debug
        then
          log ~indent:3
            "trying to find an available register with 'first-available'";
        find_first t reg interval
      | Selection_heuristics.Best_fit ->
        if gi_debug
        then
          log ~indent:3 "trying to find an available register with 'best-fit'";
        find_using_length t reg interval ~better:( > )
      | Selection_heuristics.Worst_fit ->
        if gi_debug
        then
          log ~indent:3 "trying to find an available register with 'worst-fit'";
        find_using_length t reg interval ~better:( < )
    in
    match with_no_overlap with
    | Some hardware_reg -> For_assignment { hardware_reg }
    | None ->
      if gi_debug then log ~indent:3 "trying to find an evictable register";
      find_evictable t reg interval
end
