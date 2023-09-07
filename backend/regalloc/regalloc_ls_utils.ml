[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Regalloc_utils
module DLL = Flambda_backend_utils.Doubly_linked_list

let ls_debug = false

let bool_of_param param_name =
  bool_of_param ~guard:(ls_debug, "ls_debug") param_name

let ls_verbose : bool Lazy.t = bool_of_param "LS_VERBOSE"

let ls_invariants : bool Lazy.t = bool_of_param "LS_INVARIANTS"

let log_function =
  lazy (make_log_function ~verbose:(Lazy.force ls_verbose) ~label:"ls")

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

let iter_cfg_dfs : Cfg.t -> f:(Cfg.basic_block -> unit) -> unit =
 fun cfg ~f ->
  let marked = ref Label.Set.empty in
  let rec iter (label : Label.t) : unit =
    if not (Label.Set.mem label !marked)
    then (
      marked := Label.Set.add label !marked;
      let block = Cfg.get_block_exn cfg label in
      f block;
      Label.Set.iter
        (fun succ_label -> iter succ_label)
        (Cfg.successor_labels ~normal:true ~exn:true block))
  in
  iter cfg.entry_label;
  (* note: some block may not have been seen since we currently cannot remove
     all non-reachable blocks. *)
  if Label.Set.cardinal !marked <> Label.Tbl.length cfg.blocks
  then
    Cfg.iter_blocks cfg ~f:(fun label block ->
        if not (Label.Set.mem label !marked) then f block)

let iter_instructions_dfs :
    Cfg_with_layout.t ->
    instruction:(trap_handler:bool -> Cfg.basic Cfg.instruction -> unit) ->
    terminator:(trap_handler:bool -> Cfg.terminator Cfg.instruction -> unit) ->
    unit =
 fun cfg_with_layout ~instruction ~terminator ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  iter_cfg_dfs cfg ~f:(fun block ->
      let trap_handler_id =
        if block.is_trap_handler
        then Regalloc_utils.first_instruction_id block
        else min_int
      in
      DLL.iter block.body ~f:(fun instr ->
          instruction
            ~trap_handler:(Int.equal instr.Cfg.id trap_handler_id)
            instr);
      terminator
        ~trap_handler:(Int.equal block.terminator.Cfg.id trap_handler_id)
        block.terminator)

module Range = struct
  type t =
    { mutable begin_ : int;
      mutable end_ : int
    }

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
end

module Interval = struct
  type t =
    { reg : Reg.t;
      mutable begin_ : int;
      mutable end_ : int;
      mutable ranges : Range.t list
    }

  let copy t =
    { reg = t.reg;
      begin_ = t.begin_;
      end_ = t.end_;
      ranges = List.map t.ranges ~f:Range.copy
    }

  let print ppf t =
    Format.fprintf ppf "%a[%d,%d]:" Printmach.reg t.reg t.begin_ t.end_;
    List.iter t.ranges ~f:(fun r -> Format.fprintf ppf " %a" Range.print r)

  let overlap : t -> t -> bool =
   fun left right -> Range.overlap left.ranges right.ranges

  let is_live : t -> pos:int -> bool = fun t ~pos -> Range.is_live t.ranges ~pos

  let remove_expired : t -> pos:int -> unit =
   fun t ~pos -> t.ranges <- Range.remove_expired t.ranges ~pos

  module List = struct
    let print ppf l =
      List.iter l ~f:(fun i -> Format.fprintf ppf "- %a\n" print i)

    let rec release_expired_fixed l ~pos =
      match l with
      | [] -> []
      | hd :: tl ->
        if hd.end_ >= pos
        then (
          remove_expired hd ~pos;
          hd :: release_expired_fixed tl ~pos)
        else []

    let rec insert_sorted l interval =
      match l with
      | [] -> [interval]
      | hd :: tl ->
        if hd.end_ <= interval.end_
        then interval :: l
        else hd :: insert_sorted tl interval
  end
end

module ClassIntervals = struct
  type t =
    { mutable fixed : Interval.t list;
      mutable active : Interval.t list;
      mutable inactive : Interval.t list
    }

  let make () = { fixed = []; active = []; inactive = [] }

  let copy t =
    { fixed = List.map t.fixed ~f:Interval.copy;
      active = List.map t.active ~f:Interval.copy;
      inactive = List.map t.inactive ~f:Interval.copy
    }

  let print ppf t =
    Format.fprintf ppf "fixed: %a\n" Interval.List.print t.fixed;
    Format.fprintf ppf "active: %a\n" Interval.List.print t.active;
    Format.fprintf ppf "inactive: %a\n" Interval.List.print t.inactive

  let clear t =
    t.fixed <- [];
    t.active <- [];
    t.inactive <- []

  let rec release_expired_active t ~pos l =
    match l with
    | [] -> []
    | hd :: tl ->
      if hd.Interval.end_ >= pos
      then (
        Interval.remove_expired hd ~pos;
        if Interval.is_live hd ~pos
        then hd :: release_expired_active t ~pos tl
        else (
          t.inactive <- Interval.List.insert_sorted t.inactive hd;
          release_expired_active t ~pos tl))
      else []

  let rec release_expired_inactive t ~pos l =
    match l with
    | [] -> []
    | hd :: tl ->
      if hd.Interval.end_ >= pos
      then (
        Interval.remove_expired hd ~pos;
        if not (Interval.is_live hd ~pos)
        then hd :: release_expired_inactive t ~pos tl
        else (
          t.active <- Interval.List.insert_sorted t.active hd;
          release_expired_inactive t ~pos tl))
      else []

  let release_expired_intervals t ~pos =
    t.fixed <- Interval.List.release_expired_fixed t.fixed ~pos;
    t.active <- release_expired_active t ~pos t.active;
    t.inactive <- release_expired_inactive t ~pos t.inactive
end

let log_interval ~indent ~kind (interval : Interval.t) =
  let reg_class = Proc.register_class interval.reg in
  log ~indent "%s %a (class %d) [%d..%d]" kind Printmach.reg interval.reg
    reg_class interval.begin_ interval.end_;
  let ranges =
    List.map interval.ranges ~f:(fun { Range.begin_; end_ } ->
        Printf.sprintf "[%d..%d]" begin_ end_)
  in
  log ~indent:(succ indent) "%s" (String.concat ", " ranges)

let log_intervals ~indent ~kind (intervals : Interval.t list) =
  List.iter intervals ~f:(fun (interval : Interval.t) ->
      log_interval ~indent ~kind interval)
