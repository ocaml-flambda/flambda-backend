[@@@ocaml.warning "+a-30-40-41-42"]

open! Int_replace_polymorphic_compare
open! Regalloc_utils
module DLL = Flambda_backend_utils.Doubly_linked_list

let log_function = lazy (make_log_function ~label:"ls")

let equal_list_dll eq list dll =
  let rec aux eq list cell =
    match list, cell with
    | [], None -> true
    | _ :: _, None | [], Some _ -> false
    | hd :: tl, Some cell ->
      let value = DLL.value cell in
      eq hd value && aux eq tl (DLL.next cell)
  in
  aux eq list (DLL.hd_cell dll)

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

let iter_instructions_dfs :
    Cfg_with_layout.t ->
    instruction:(trap_handler:bool -> Cfg.basic Cfg.instruction -> unit) ->
    terminator:(trap_handler:bool -> Cfg.terminator Cfg.instruction -> unit) ->
    unit =
 fun cfg_with_layout ~instruction ~terminator ->
  let cfg = Cfg_with_layout.cfg cfg_with_layout in
  Cfg.iter_blocks_dfs cfg ~f:(fun (_ : Label.t) (block : Cfg.basic_block) ->
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
        block.terminator)

module Range = struct
  type t =
    { mutable begin_ : int;
      mutable end_ : int
    }

  let equal left right =
    Int.equal left.begin_ right.begin_ && Int.equal left.end_ right.end_

  let copy r = { begin_ = r.begin_; end_ = r.end_ }

  let print ppf r = Format.fprintf ppf "[%d,%d]" r.begin_ r.end_

  let rec overlap_cursor : t DLL.Cursor.t -> t DLL.Cursor.t -> bool =
   fun left right ->
    let left_value = DLL.Cursor.value left in
    let right_value = DLL.Cursor.value right in
    if left_value.end_ >= right_value.begin_
       && right_value.end_ >= left_value.begin_
    then true
    else if left_value.end_ < right_value.end_
    then
      match DLL.Cursor.next left with
      | Error `End_of_list -> false
      | Ok () -> overlap_cursor left right
    else if left_value.end_ > right_value.end_
    then
      match DLL.Cursor.next right with
      | Error `End_of_list -> false
      | Ok () -> overlap_cursor left right
    else
      match DLL.Cursor.next left, DLL.Cursor.next right with
      | Error `End_of_list, _ | _, Error `End_of_list -> false
      | Ok (), Ok () -> overlap_cursor left right

  let overlap : t DLL.t -> t DLL.t -> bool =
   fun left right ->
    match DLL.create_hd_cursor left, DLL.create_hd_cursor right with
    | Error `Empty, _ | _, Error `Empty -> false
    | Ok left, Ok right -> overlap_cursor left right

  let rec is_live_cursor : t DLL.Cursor.t -> pos:int -> bool =
   fun cursor ~pos ->
    let value = DLL.Cursor.value cursor in
    if pos < value.begin_
    then false
    else if pos <= value.end_
    then true
    else
      match DLL.Cursor.next cursor with
      | Error `End_of_list -> false
      | Ok () -> is_live_cursor cursor ~pos

  let is_live : t DLL.t -> pos:int -> bool =
   fun l ~pos ->
    match DLL.create_hd_cursor l with
    | Error `Empty -> false
    | Ok cursor -> is_live_cursor cursor ~pos

  let rec remove_expired_cursor : t DLL.Cursor.t -> pos:int -> unit =
   fun cursor ~pos ->
    let value = DLL.Cursor.value cursor in
    if pos < value.end_
    then ()
    else
      match DLL.Cursor.delete_and_next cursor with
      | Error `End_of_list -> ()
      | Ok () -> remove_expired_cursor cursor ~pos

  let remove_expired : t DLL.t -> pos:int -> unit =
   fun l ~pos ->
    match DLL.create_hd_cursor l with
    | Error `Empty -> ()
    | Ok cursor -> remove_expired_cursor cursor ~pos
end

module Interval = struct
  type t =
    { reg : Reg.t;
      mutable begin_ : int;
      mutable end_ : int;
      ranges : Range.t DLL.t
    }

  let equal left right =
    Reg.same left.reg right.reg
    && Int.equal left.begin_ right.begin_
    && Int.equal left.end_ right.end_
    && DLL.equal Range.equal left.ranges right.ranges

  let copy t =
    { reg = t.reg;
      begin_ = t.begin_;
      end_ = t.end_;
      ranges = DLL.map t.ranges ~f:Range.copy
    }

  let print ppf t =
    Format.fprintf ppf "%a[%d,%d]:" Printreg.reg t.reg t.begin_ t.end_;
    DLL.iter t.ranges ~f:(fun r -> Format.fprintf ppf " %a" Range.print r)

  let overlap : t -> t -> bool =
   fun left right -> Range.overlap left.ranges right.ranges

  let is_live : t -> pos:int -> bool = fun t ~pos -> Range.is_live t.ranges ~pos

  let remove_expired : t -> pos:int -> unit =
   fun t ~pos -> Range.remove_expired t.ranges ~pos

  module DLL = struct
    let print ppf l =
      DLL.iter l ~f:(fun i -> Format.fprintf ppf "- %a\n" print i)

    let release_expired_fixed l ~pos =
      let rec aux curr ~pos =
        match curr with
        | None -> ()
        | Some cell ->
          let value = DLL.value cell in
          if value.end_ >= pos
          then (
            remove_expired value ~pos;
            aux (DLL.next cell) ~pos)
          else DLL.cut_from cell
      in
      aux (DLL.hd_cell l) ~pos

    let insert_sorted (l : t DLL.t) (interval : t) : unit =
      let rec aux l interval curr =
        match curr with
        | None -> DLL.add_end l interval
        | Some cell ->
          let value = DLL.value cell in
          if value.end_ <= interval.end_
          then DLL.insert_before cell interval
          else aux l interval (DLL.next cell)
      in
      aux l interval (DLL.hd_cell l)
  end
end

module ClassIntervals = struct
  type t =
    { fixed_dll : Interval.t DLL.t;
      active_dll : Interval.t DLL.t;
      inactive_dll : Interval.t DLL.t
    }

  let print ppf t =
    Format.fprintf ppf "fixed_dll(%d): %a\n" (DLL.length t.fixed_dll)
      Interval.DLL.print t.fixed_dll;
    Format.fprintf ppf "active_dll(%d): %a\n" (DLL.length t.active_dll)
      Interval.DLL.print t.active_dll;
    Format.fprintf ppf "inactive_dll(%d): %a\n"
      (DLL.length t.inactive_dll)
      Interval.DLL.print t.inactive_dll

  let make () =
    { fixed_dll = DLL.make_empty ();
      active_dll = DLL.make_empty ();
      inactive_dll = DLL.make_empty ()
    }

  let copy t =
    { fixed_dll = DLL.map t.fixed_dll ~f:Interval.copy;
      active_dll = DLL.map t.active_dll ~f:Interval.copy;
      inactive_dll = DLL.map t.inactive_dll ~f:Interval.copy
    }

  let clear t =
    DLL.clear t.fixed_dll;
    DLL.clear t.active_dll;
    DLL.clear t.inactive_dll

  module DLL = struct
    let release_expired_active (t : t) ~(pos : int) (l : Interval.t DLL.t) :
        unit =
      let rec aux t ~pos curr : unit =
        match curr with
        | None -> ()
        | Some cell ->
          let value = DLL.value cell in
          if value.Interval.end_ >= pos
          then (
            Interval.remove_expired value ~pos;
            if Interval.is_live value ~pos
            then aux t ~pos (DLL.next cell)
            else (
              Interval.DLL.insert_sorted t.inactive_dll value;
              let next = DLL.next cell in
              DLL.delete_curr cell;
              aux t ~pos next))
          else DLL.cut_from cell
      in
      aux t ~pos (DLL.hd_cell l)

    let release_expired_inactive (t : t) ~(pos : int) (l : Interval.t DLL.t) :
        unit =
      let rec aux t ~pos curr =
        match curr with
        | None -> ()
        | Some cell ->
          let value = DLL.value cell in
          if value.Interval.end_ >= pos
          then (
            Interval.remove_expired value ~pos;
            if not (Interval.is_live value ~pos)
            then aux t ~pos (DLL.next cell)
            else (
              Interval.DLL.insert_sorted t.active_dll value;
              let next = DLL.next cell in
              DLL.delete_curr cell;
              aux t ~pos next))
          else DLL.cut_from cell
      in
      aux t ~pos (DLL.hd_cell l)
  end

  let release_expired_intervals t ~pos =
    Interval.DLL.release_expired_fixed t.fixed_dll ~pos;
    DLL.release_expired_active t ~pos t.active_dll;
    DLL.release_expired_inactive t ~pos t.inactive_dll
end

let log_interval ~kind (interval : Interval.t) =
  let reg_class = Proc.register_class interval.reg in
  log "%s %a (class %d) [%d..%d]" kind Printreg.reg interval.reg reg_class
    interval.begin_ interval.end_;
  let ranges = Buffer.create 128 in
  let first = ref true in
  DLL.iter interval.ranges ~f:(fun { Range.begin_; end_ } ->
      if !first then first := false else Buffer.add_string ranges ", ";
      Buffer.add_string ranges (Printf.sprintf "[%d..%d]" begin_ end_));
  indent ();
  log "%s" (Buffer.contents ranges);
  dedent ()

let log_interval_dll ~kind (intervals : Interval.t DLL.t) =
  DLL.iter intervals ~f:(fun (interval : Interval.t) ->
      log_interval ~kind interval)
