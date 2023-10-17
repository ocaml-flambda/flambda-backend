[@@@ocaml.warning "+a-30-40-41-42"]

module List = ListLabels
module String = Misc.Stdlib.String

module DLL = Flambda_backend_utils.Doubly_linked_list

let function_is_assumed_to_never_poll func =
  Polling.function_is_assumed_to_never_poll func

type polling_point = Polling.polling_point = Alloc | Poll | Function_call | External_call

type error = Polling.error = Poll_error of (polling_point * Debuginfo.t) list

exception Error = Polling.Error

module Unsafe_or_safe_domain = struct
  type t = Polling.Unsafe_or_safe.t
  let bot = Polling.Unsafe_or_safe.bot
  let join = Polling.Unsafe_or_safe.join
  let less_equal = Polling.Unsafe_or_safe.lessequal
end

module Unsafe_or_safe_transfer = struct
  type domain = Unsafe_or_safe_domain.t
  type error = |
  let basic
    : domain -> Cfg.basic Cfg.instruction -> (domain, error) result
    = fun dom instr ->
      match[@ocaml.warning "-4"] instr.desc with
      | Op Poll -> Ok Safe
      | Op _
      | Reloadretaddr
      | Pushtrap _
      | Poptrap
      | Prologue ->
        Ok dom

  let terminator
    : domain -> exn:domain -> Cfg.terminator Cfg.instruction -> (domain, error) result
    = fun dom ~exn term ->
      match[@ocaml.warning "-4"] term.desc with
      | Never -> assert false
      | Always _
      | Parity_test _
      | Truth_test _
      | Float_test _
      | Int_test _
      | Switch _ -> Ok dom
      | Raise _ -> Ok exn
      | Tailcall_self _
      | Tailcall_func _
      | Prim { op = Alloc _; label_after = _ }
      | Return -> Ok Safe
      | Call_no_return _
      | Call _
      | Prim _
      | Specific_can_raise _ ->
        if Cfg.can_raise_terminator term.desc then
          Ok (Unsafe_or_safe_domain.join dom exn)
        else
          Ok dom
  
  let exception_
    : domain -> (domain, error) result
    = fun dom -> Ok dom
end

module PolledLoopsAnalysis = Cfg_dataflow.Backward (Unsafe_or_safe_domain) (Unsafe_or_safe_transfer)

let polled_loops_analysis
  : Cfg_with_layout.t -> PolledLoopsAnalysis.domain Label.Tbl.t
  = fun cfg_with_layout ->
    let init : Unsafe_or_safe_domain.t = Unsafe_or_safe_domain.bot in
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    match PolledLoopsAnalysis.run ~init ~map:PolledLoopsAnalysis.Block cfg () with
    | Ok res -> res
    | Aborted _ -> .
    | Max_iterations_reached ->
      Misc.fatal_error "Cfg_polling.polled_loops_analysis has been interrupted"

module Polls_before_prtc_domain = struct
  type t = Polling.Polls_before_prtc.t
  let bot = Polling.Polls_before_prtc.bot
  let join = Polling.Polls_before_prtc.join
  let less_equal = Polling.Polls_before_prtc.lessequal
end

module Polls_before_prtc_transfer (FF : sig val future_funcnames : String.Set.t end) = struct
  type domain = Polls_before_prtc_domain.t
  type error = |
  let basic
    : domain -> Cfg.basic Cfg.instruction -> (domain, error) result
    = fun dom instr ->
      match instr.desc with
      | Op Poll -> Ok Always_polls
      | Op _
      | Reloadretaddr
      | Pushtrap _
      | Poptrap
      | Prologue ->
        Ok dom
  [@@ocaml.warning "-4"]
  let terminator
    : domain -> exn:domain -> Cfg.terminator Cfg.instruction -> (domain, error) result
    = fun dom ~exn term ->
      match[@ocaml.warning "-4"] term.desc with
      | Never -> assert false
      | Always _
      | Parity_test _
      | Truth_test _
      | Float_test _
      | Int_test _
      | Switch _ -> Ok dom
      | Raise _ -> Ok exn
      | Tailcall_self _ | Tailcall_func Indirect -> Ok Might_not_poll
      | Tailcall_func (Direct func) ->
        if String.Set.mem func.sym_name FF.future_funcnames
        || function_is_assumed_to_never_poll func.sym_name
             then Ok Might_not_poll
             else Ok Always_polls
      | Prim { op = Alloc _; label_after = _ }
      | Return -> Ok Always_polls
      | Call_no_return _
      | Call _
      | Prim _
      | Specific_can_raise _ ->
        if Cfg.can_raise_terminator term.desc then
          Ok (Polls_before_prtc_domain.join dom exn)
        else
          Ok dom
  let exception_
    : domain -> (domain, error) result
    = fun dom -> Ok dom
end

let potentially_recursive_tailcall
  : future_funcnames:String.Set.t -> Cfg_with_layout.t -> Polls_before_prtc_domain.t
  = fun ~future_funcnames cfg_with_layout ->
    let module Polls_before_prtc_transfer_ff = Polls_before_prtc_transfer (struct
        let future_funcnames = future_funcnames
      end) in
    let module PTRCAnalysis =
      Cfg_dataflow.Backward
        (Polls_before_prtc_domain)
        (Polls_before_prtc_transfer_ff)
    in
    let init : Polls_before_prtc_domain.t = Polls_before_prtc_domain.bot in
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    match PTRCAnalysis.run ~init ~map:PTRCAnalysis.Block cfg () with
    | Ok res ->
      begin match Label.Tbl.find_opt res cfg.entry_label with
        | None -> assert false
        | Some res -> res
      end
    | Aborted _ -> .
    | Max_iterations_reached ->
      Misc.fatal_error "Cfg_polling.potentially_recursive_tailcall has been interrupted"

let instr_cfg_with_layout
  : Cfg_with_layout.t -> block_needs_poll:PolledLoopsAnalysis.domain Label.Tbl.t -> back_edges:Cfg_loop_infos.Edge.t list -> bool
  = fun cfg_with_layout ~block_needs_poll ~back_edges ->
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    List.fold_left back_edges ~init:false ~f:(fun added_poll { Cfg_loop_infos.Edge.src; dst; } ->
        let needs_poll =
          match Label.Tbl.find_opt block_needs_poll dst with
          | None -> assert false
          | Some Safe -> false
          | Some Unsafe -> true
        in
        if needs_poll then begin
          let after = Cfg.get_block_exn cfg src in
          let cfg_infos =
            Regalloc_utils.collect_cfg_infos cfg_with_layout
          in
          let next_instruction_id = ref (succ cfg_infos.max_instruction_id) in
          let next_instruction_id () =
            let res = !next_instruction_id in
            incr next_instruction_id;
            res
          in
          let poll =
            (* CR xclerc for xclerc: double check the various fields. *)
            { after.terminator with
              Cfg.id = next_instruction_id ();
              Cfg.desc = Cfg.Op Poll; }
          in
          begin match Label.Set.cardinal (Cfg.successor_labels after ~normal:true ~exn:false), after.exn with
          | 1, None ->
            DLL.add_end after.body poll
          | _ ->
            let before = Some (Cfg.get_block_exn cfg dst) in
            let instrs = DLL.of_list [ poll ] in
            (* CR-soon xclerc: that kind of indicates the `insert_block` function
               should be moved outside of "regalloc/" *)
            let _ : Cfg.basic_block list =
              Regalloc_utils.insert_block
                cfg_with_layout
                instrs
                ~after
                ~before
                ~next_instruction_id
            in
            ()
          end;
          true
        end else
          added_poll)

type polling_points = (polling_point * Debuginfo.t) list

let add_poll_alloc_or_calls_basic
  : Cfg.basic Cfg.instruction -> polling_points -> polling_points
  = fun instr points ->
    match instr.desc with
    | Op op ->
      begin match op with
        | Move
        | Spill
        | Reload
        | Const_int _
        | Const_float _
        | Const_symbol _
        | Const_vec128 _
        | Stackoffset _
        | Load _
        | Store _
        | Intop _
        | Intop_imm _
        | Intop_atomic _
        | Negf
        | Absf
        | Addf
        | Subf
        | Mulf
        | Divf
        | Compf _
        | Csel _
        | Floatofint
        | Intoffloat
        | Valueofint
        | Intofvalue
        | Vectorcast _
        | Scalarcast _
        | Probe_is_enabled _
        | Opaque
        | Begin_region
        | End_region
        | Specific _
        | Name_for_debugger _ ->
          (* CR xclerc for xclerc: double check the `Specific` case. *)
          points
        | Poll ->
          (Poll, instr.dbg) :: points
      end
    | Reloadretaddr
    | Pushtrap _
    | Poptrap
    | Prologue -> points

let add_poll_alloc_or_calls_terminator
  : Cfg.terminator Cfg.instruction -> polling_points -> polling_points
  = fun term points ->
    match term.desc with
    | Never -> assert false
    | Always _
    | Parity_test _
    | Truth_test _
    | Float_test _
    | Int_test _
    | Switch _
    | Return
    | Raise _
    | Specific_can_raise _ ->
      (* CR xclerc for xclerc: double check the `Specific_can_raise` case. *)
      points
    | Tailcall_self _
    | Tailcall_func _ -> (Function_call, term.dbg) :: points
    | Call _ -> (Function_call, term.dbg) :: points
    | Call_no_return { alloc = false; func_symbol = _; ty_res = _; ty_args = _;}
    | Prim { op = External { alloc = false; func_symbol = _; ty_res = _; ty_args = _;}; label_after = _ } -> points
    | Call_no_return { alloc = true; func_symbol = _; ty_res = _; ty_args = _;}
    | Prim { op = External { alloc = true; func_symbol = _; ty_res = _; ty_args = _;}; label_after = _ } ->
      (External_call, term.dbg) :: points
    | Prim { op = Alloc _; label_after = _ } -> (Alloc, term.dbg) :: points
    | Prim { op = Checkbound _; label_after = _ } -> points
    | Prim { op = Probe _; label_after = _ } -> points

let find_poll_alloc_or_calls
  : Cfg.t -> polling_points
  = fun cfg ->
    Cfg.fold_blocks cfg ~init:[] ~f:(fun _label block acc ->
        let acc = DLL.fold_right ~init:acc ~f:add_poll_alloc_or_calls_basic block.body in
        let acc = add_poll_alloc_or_calls_terminator block.terminator acc in
        acc)

let contains_polls
  : Cfg.t -> bool
  = fun cfg ->
    Cfg.fold_blocks cfg ~init:false ~f:(fun _label block acc ->
        acc || DLL.exists block.body ~f:(fun (instr : Cfg.basic Cfg.instruction) ->
            match[@ocaml.warning "-4"] instr.Cfg.desc with
            | Cfg.Op Poll -> true
            | _ -> false
          ))

let is_disabled fun_name =
  (not Config.poll_insertion) ||
  !Flambda_backend_flags.disable_poll_insertion ||
  function_is_assumed_to_never_poll fun_name

let instrument_fundecl
  : future_funcnames:Misc.Stdlib.String.Set.t -> Cfg_with_layout.t -> Cfg_with_layout.t
  = fun ~future_funcnames:_ cfg_with_layout ->
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    if is_disabled cfg.fun_name then cfg_with_layout
    else begin
      let block_needs_poll = polled_loops_analysis cfg_with_layout in
      (* CR xclerc for xclerc: use `Cfg_with_infos`, to cache the computations? *)
      let doms = Cfg_dominators.build cfg in
      let back_edges = Cfg_loop_infos.compute_back_edges cfg doms in
      let added_poll = instr_cfg_with_layout cfg_with_layout ~block_needs_poll ~back_edges in
      begin match cfg.fun_poll with
        | Error_poll -> begin
            match find_poll_alloc_or_calls cfg with
            | [] -> ()
            | poll_error_instrs -> raise (Error(Poll_error poll_error_instrs))
          end
        | Default_poll -> () end;
      let new_contains_calls =
        cfg.fun_contains_calls || added_poll || contains_polls cfg
      in
      cfg.fun_contains_calls <- new_contains_calls;
      cfg_with_layout
    end

let requires_prologue_poll
  : future_funcnames:Misc.Stdlib.String.Set.t -> fun_name:string -> Cfg_with_layout.t -> bool
  = fun ~future_funcnames ~fun_name cfg_with_layout ->
  if is_disabled fun_name then false
  else
    match potentially_recursive_tailcall ~future_funcnames cfg_with_layout with
    | Might_not_poll -> true
    | Always_polls -> false
