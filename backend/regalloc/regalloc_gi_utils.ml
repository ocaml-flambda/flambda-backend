[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils
module DLL = Oxcaml_utils.Doubly_linked_list

let gi_rng = Random.State.make [| 4; 6; 2 |]

let log_function = lazy (make_log_function ~label:"gi")

let indent () = (Lazy.force log_function).indent ()

let dedent () = (Lazy.force log_function).dedent ()

let reset_indentation () = (Lazy.force log_function).reset_indentation ()

let log : type a. ?no_eol:unit -> (a, Format.formatter, unit) format -> a =
 fun ?no_eol fmt -> (Lazy.force log_function).log ?no_eol fmt

let instr_prefix (instr : Cfg.basic Cfg.instruction) =
  Printf.sprintf "#%04d" instr.ls_order

let term_prefix (term : Cfg.terminator Cfg.instruction) =
  Printf.sprintf "#%04d" term.ls_order

let log_body_and_terminator :
    Cfg.basic_instruction_list ->
    Cfg.terminator Cfg.instruction ->
    liveness ->
    unit =
 fun body terminator liveness ->
  make_log_body_and_terminator (Lazy.force log_function) ~instr_prefix
    ~term_prefix body terminator liveness

let log_cfg_with_infos : Cfg_with_infos.t -> unit =
 fun cfg_with_infos ->
  make_log_cfg_with_infos (Lazy.force log_function) ~instr_prefix ~term_prefix
    cfg_with_infos

(* CR xclerc for xclerc: add more heuristics *)
module Priority_heuristics = struct
  type t =
    | Interval_length
    | Random_for_testing

  let default = Interval_length

  let all = [Interval_length; Random_for_testing]

  let to_string = function
    | Interval_length -> "interval_length"
    | Random_for_testing -> "random"

  let random () = Random.State.int gi_rng 10_000

  let value =
    let available_heuristics () =
      String.concat ", "
        (all |> List.map ~f:to_string |> List.map ~f:(Printf.sprintf "%S"))
    in
    lazy
      (match find_param_value "GI_PRIORITY_HEURISTICS" with
      | None -> default
      | Some id -> (
        match String.lowercase_ascii id with
        | "interval_length" | "interval-length" -> Interval_length
        | "random" -> Random_for_testing
        | _ ->
          fatal "unknown heuristics %S (possible values: %s)" id
            (available_heuristics ())))
end

(* CR xclerc for xclerc: add more heuristics *)
module Selection_heuristics = struct
  type t =
    | First_available
    | Best_fit
    | Worst_fit
    | Random_for_testing

  let default = First_available

  let all = [First_available; Best_fit; Worst_fit; Random_for_testing]

  let to_string = function
    | First_available -> "first_available"
    | Best_fit -> "best_fit"
    | Worst_fit -> "worst_fit"
    | Random_for_testing -> "random"

  let include_in_random = function
    | Random_for_testing | Worst_fit -> false
    | First_available | Best_fit -> true

  let random =
    let all = List.filter all ~f:include_in_random in
    let len = List.length all in
    fun () -> List.nth all (Random.State.int gi_rng len)

  let value =
    let available_heuristics () =
      String.concat ", "
        (all |> List.map ~f:to_string |> List.map ~f:(Printf.sprintf "%S"))
    in
    lazy
      (match find_param_value "GI_SELECTION_HEURISTICS" with
      | None -> default
      | Some id -> (
        match String.lowercase_ascii id with
        | "first_available" | "first-available" -> First_available
        | "best_fit" | "best-fit" -> Best_fit
        | "worst_fit" | "worst-fit" -> Worst_fit
        | "random" -> Random_for_testing
        | _ ->
          fatal "unknown heuristics %S (possible values: %s)" id
            (available_heuristics ())))
end

module Spilling_heuristics = struct
  type t =
    | Flat_uses
    | Hierarchical_uses
    | Random_for_testing

  let default = Flat_uses

  let all = [Flat_uses; Hierarchical_uses; Random_for_testing]

  let to_string = function
    | Flat_uses -> "flat_uses"
    | Hierarchical_uses -> "hierarchical_uses"
    | Random_for_testing -> "random"

  let random () = Random.State.bool gi_rng

  let value =
    let available_heuristics () =
      String.concat ", "
        (all |> List.map ~f:to_string |> List.map ~f:(Printf.sprintf "%S"))
    in
    lazy
      (match find_param_value "GI_SPILLING_HEURISTICS" with
      | None -> default
      | Some id -> (
        match String.lowercase_ascii id with
        | "flat_uses" | "flat-uses" -> Flat_uses
        | "hierarchical_uses" | "hierarchical-uses" -> Hierarchical_uses
        | "random" -> Random_for_testing
        | _ ->
          fatal "unknown heuristics %S (possible values: %s)" id
            (available_heuristics ())))
end

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
      else InstructionId.none
    in
    DLL.iter block.body ~f:(fun instr ->
        instruction
          ~trap_handler:(InstructionId.equal instr.Cfg.id trap_handler_id)
          instr);
    terminator
      ~trap_handler:
        (InstructionId.equal block.terminator.Cfg.id trap_handler_id)
      block.terminator
  in
  Cfg_with_layout.iter_blocks cfg_with_layout ~f

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
    (* CR-soon xclerc for xclerc: use the same version as linscan (cursors). *)
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
  (* CR-soon xclerc for xclerc: use a doubly-linked list for `ranges`, and do
     not store bounds. *)
  type t =
    { mutable begin_ : int option;
      mutable end_ : int option;
      (* The `begin_` and `end_` fields should always either both be `None`, or
         they should both be `Some`. `Option.is_none begin_` <=> `List.is_empty
         ranges`. *)
      mutable ranges : Range.t list
    }

  let make_empty () =
    (* CR xclerc for xclerc: avoid the non-sensical bounds. *)
    { begin_ = None; end_ = None; ranges = [] }

  let length t =
    List.fold_left t.ranges ~init:0 ~f:(fun acc range ->
        acc + Range.length range)

  let print_bound ppf print_bound =
    match print_bound with
    | None -> Format.fprintf ppf "-"
    | Some bound -> Format.fprintf ppf "%d" bound

  let print ppf t =
    Format.fprintf ppf "[%a,%a]:" print_bound t.begin_ print_bound t.end_;
    List.iter t.ranges ~f:(fun r -> Format.fprintf ppf " %a" Range.print r)

  let is_before_or_alone : int option -> int option -> bool =
   fun left right ->
    match left, right with
    | None, None | None, Some _ | Some _, None -> true
    | Some left, Some right -> left < right

  let overlap : t -> t -> bool =
   fun left right ->
    if is_before_or_alone left.end_ right.begin_
       || is_before_or_alone right.end_ left.begin_
    then false
    else Range.overlap left.ranges right.ranges

  let[@inline] lift_opt op left right =
    match left, right with
    | None, None -> None
    | None, (Some _ as value) | (Some _ as value), None -> value
    | Some left, Some right -> Some (op left right)

  (* CR xclerc for xclerc: assumes no overlap *)
  let add_ranges : t -> from:t -> unit =
   fun t ~from ->
    t.begin_ <- lift_opt Int.min t.begin_ from.begin_;
    t.end_ <- lift_opt Int.max t.end_ from.end_;
    t.ranges <- Range.merge t.ranges from.ranges
end

let build_intervals : Cfg_with_infos.t -> Interval.t Reg.Tbl.t =
 fun cfg_with_infos ->
  if debug
  then (
    log "build_intervals";
    indent ());
  let cfg_with_layout = Cfg_with_infos.cfg_with_layout cfg_with_infos in
  let liveness = Cfg_with_infos.liveness cfg_with_infos in
  let past_ranges : Interval.t Reg.Tbl.t = Reg.Tbl.create 123 in
  let current_ranges : Range.t Reg.Tbl.t = Reg.Tbl.create 123 in
  let add_range (reg : Reg.t) ({ begin_; end_ } as range : Range.t) : unit =
    match Reg.Tbl.find_opt past_ranges reg with
    | None ->
      Reg.Tbl.replace past_ranges reg
        { Interval.begin_ = Some begin_; end_ = Some end_; ranges = [range] }
    | Some (interval : Interval.t) ->
      interval.ranges <- range :: interval.ranges;
      interval.end_ <- Some end_
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
  if debug && Lazy.force verbose
  then
    Cfg_with_layout.iter_blocks cfg_with_layout ~f:(fun block ->
        log "(block %a)" Label.format block.start;
        log_body_and_terminator block.body block.terminator liveness);
  if debug then dedent ();
  past_ranges

module Hardware_register = struct
  type location =
    { reg_class : Reg_class.t;
      reg_index_in_class : int
    }

  let make_location ~reg_class ~reg_index_in_class =
    if reg_index_in_class < 0
       || reg_index_in_class >= Reg_class.num_available_registers reg_class
    then
      fatal "invalid register index: %d (class=%a)" reg_index_in_class
        Reg_class.print reg_class;
    { reg_class; reg_index_in_class }

  let print_location ppf { reg_class; reg_index_in_class } =
    Format.fprintf ppf "{ cls=%a; idx=%d }" Reg_class.print reg_class
      reg_index_in_class

  let reg_location_of_location { reg_class; reg_index_in_class } =
    let reg_index =
      Reg_class.first_available_register reg_class + reg_index_in_class
    in
    Reg.Reg reg_index

  type assigned =
    { pseudo_reg : Reg.t;
      interval : Interval.t;
      evictable : bool
    }

  let print_assigned ppf { pseudo_reg; interval; evictable } =
    Format.fprintf ppf "%a %a (evitable=%B)" Printreg.reg pseudo_reg
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
  type t = Hardware_register.t array Reg_class.Tbl.t
  (* array index is register index in class *)

  let make () =
    Reg_class.Tbl.init ~f:(fun reg_class ->
        let num_available_registers =
          Reg_class.num_available_registers reg_class
        in
        Array.init num_available_registers ~f:(fun reg_index_in_class ->
            let location =
              Hardware_register.make_location ~reg_class ~reg_index_in_class
            in
            { Hardware_register.location;
              interval = Interval.make_empty ();
              assigned = []
            }))

  let of_reg (t : t) (reg : Reg.t) : Hardware_register.t option =
    match reg.loc with
    | Reg reg_index ->
      let reg_class : Reg_class.t = Reg_class.of_machtype reg.typ in
      let reg_index_in_class : int =
        reg_index - Reg_class.first_available_register reg_class
      in
      let hw_regs = Reg_class.Tbl.find t reg_class in
      if reg_index_in_class < Array.length hw_regs
      then Some hw_regs.(reg_index_in_class)
      else None
    | Unknown -> fatal "`Unknown` location (expected `Reg _`)"
    | Stack _ -> fatal "`Stack _` location (expected `Reg _`)"

  let find_in_class (t : t) ~(of_reg : Reg.t) ~(f : Hardware_register.t -> bool)
      =
    let reg_class = Reg_class.of_machtype of_reg.typ in
    Array.find_opt (Reg_class.Tbl.find t reg_class) ~f

  let fold_class :
      type a.
      t -> of_reg:Reg.t -> f:(a -> Hardware_register.t -> a) -> init:a -> a =
   fun t ~of_reg ~f ~init ->
    let reg_class = Reg_class.of_machtype of_reg.typ in
    Array.fold_left (Reg_class.Tbl.find t reg_class) ~f ~init

  let actual_cost (costs : SpillCosts.t) (reg : Reg.t) : int =
    (* CR xclerc for xclerc: it could make sense to give a lower cost to reg
       already spilled (e.g. by the split preprocessing) since they already have
       a stack slot *)
    SpillCosts.for_reg costs reg

  let overlap (hardware_reg : Hardware_register.t) (interval : Interval.t) :
      bool =
    if debug
    then (
      log "considering %a" Hardware_register.print_location
        hardware_reg.location;
      indent ());
    let overlap_hard : bool = Interval.overlap interval hardware_reg.interval in
    let overlap_assigned =
      List.exists hardware_reg.assigned
        ~f:(fun
             { Hardware_register.pseudo_reg = _; interval = itv; evictable = _ }
           -> Interval.overlap itv interval)
    in
    let overlap = overlap_hard || overlap_assigned in
    if debug
    then (
      log "overlap=%B (hard=%B, assigned=%B)" overlap overlap_hard
        overlap_assigned;
      dedent ());
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

  let find_evictable (t : t) (costs : SpillCosts.t) (reg : Reg.t)
      (interval : Interval.t) : available =
    let eviction =
      fold_class t ~of_reg:reg ~init:None ~f:(fun acc hardware_reg ->
          if debug
          then
            log "considering %a (length=%d)" Hardware_register.print_location
              hardware_reg.location
              (List.length hardware_reg.assigned);
          let overlap_hard = Interval.overlap interval hardware_reg.interval in
          if overlap_hard
          then acc
          else (
            if debug then indent ();
            let overlaping : Hardware_register.assigned list =
              List.filter hardware_reg.assigned
                ~f:(fun
                     { Hardware_register.pseudo_reg;
                       interval = itv;
                       evictable = _
                     }
                   ->
                  let overlap = Interval.overlap interval itv in
                  if debug
                  then
                    log "%a is assigned / overlap=%B" Printreg.reg pseudo_reg
                      overlap;
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
                  ( acc_cost + actual_cost costs pseudo_reg,
                    acc_evictable && evictable ))
            in
            if debug then dedent ();
            if not evictable
            then acc
            else
              let evict_cost =
                match acc with None -> max_int | Some (_, _, c) -> c
              in
              if cost < evict_cost && cost < actual_cost costs reg
              then (
                if debug
                then
                  List.iter overlaping ~f:(fun assigned ->
                      log "evicting %a" Hardware_register.print_assigned
                        assigned);
                Some (hardware_reg, overlaping, cost))
              else acc))
    in
    match eviction with
    | Some (hardware_reg, evicted_regs, _) ->
      For_eviction { hardware_reg; evicted_regs }
    | None -> Split_or_spill

  let find_available : t -> SpillCosts.t -> Reg.t -> Interval.t -> available =
   fun t costs reg interval ->
    let with_no_overlap =
      let heuristic =
        match Lazy.force Selection_heuristics.value with
        | Selection_heuristics.Random_for_testing ->
          Selection_heuristics.random ()
        | (First_available | Best_fit | Worst_fit) as heuristic -> heuristic
      in
      match heuristic with
      | Selection_heuristics.Random_for_testing -> assert false
      | Selection_heuristics.First_available ->
        if debug
        then log "trying to find an available register with 'first-available'";
        find_first t reg interval
      | Selection_heuristics.Best_fit ->
        if debug then log "trying to find an available register with 'best-fit'";
        find_using_length t reg interval ~better:( > )
      | Selection_heuristics.Worst_fit ->
        if debug
        then log "trying to find an available register with 'worst-fit'";
        find_using_length t reg interval ~better:( < )
    in
    match with_no_overlap with
    | Some hardware_reg -> For_assignment { hardware_reg }
    | None ->
      if debug then log "trying to find an evictable register";
      find_evictable t costs reg interval
end
