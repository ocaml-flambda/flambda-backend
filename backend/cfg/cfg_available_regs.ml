[@@@ocaml.warning "+a-30-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list
module R = Reg
module RAS = Reg_availability_set
module RD = Reg_with_debug_info
module V = Backend_var

(* If permitted to do so by the command line flags, this pass will extend live
   ranges for otherwise dead but available registers across allocations, polls
   and calls when it is safe to do so. This allows the values of more variables
   to be seen in the debugger, for example when the last use of some variable is
   just before a call, and the debugger is standing in the callee. It may
   however affect the semantics of e.g. finalizers. *)
let extend_live () = !Dwarf_flags.gdwarf_may_alter_codegen

(* CR xclerc for xclerc: consider passing this value through the domain. *)
let all_regs_that_might_be_named = ref Reg.Set.empty

let check_invariants :
    type a.
    a Cfg.instruction ->
    print_instr:(Format.formatter -> a Cfg.instruction -> unit) ->
    avail_before:RAS.t ->
    unit =
 fun instr ~print_instr ~avail_before ->
  match avail_before with
  | Unreachable -> ()
  | Ok avail_before ->
    (* Every register that is live across an instruction should also be
       available before the instruction. *)
    let live = R.Set.inter instr.live !all_regs_that_might_be_named in
    if not (R.Set.subset live (RD.Set.forget_debug_info avail_before))
    then
      Misc.fatal_errorf
        "Named live registers not a subset of available registers: live={%a}  \
         avail_before=%a missing={%a} insn=%a"
        Printmach.regset live
        (RAS.print ~print_reg:Printmach.reg)
        (RAS.Ok avail_before) Printmach.regset
        (R.Set.diff live (RD.Set.forget_debug_info avail_before))
        print_instr instr;
    (* Every register that is an input to an instruction should be available. *)
    let args = R.inter_set_array !all_regs_that_might_be_named instr.arg in
    let avail_before_fdi = RD.Set.forget_debug_info avail_before in
    if not (R.Set.subset args avail_before_fdi)
    then
      Misc.fatal_errorf
        "Instruction has unavailable input register(s): avail_before=%a \
         avail_before_fdi={%a} inputs={%a} insn=%a"
        (RAS.print ~print_reg:Printmach.reg)
        (RAS.Ok avail_before) Printmach.regset avail_before_fdi Printmach.regset
        args print_instr instr

(* CR xclerc for xclerc: double check the whole `Domain` module. *)
module Domain = struct
  type t = { avail_before : Reg_availability_set.t option } [@@unboxed]

  let bot = { avail_before = Some Unreachable }

  let join ({ avail_before = left_avail } as left)
      ({ avail_before = right_avail } as right) : t =
    match left_avail, right_avail with
    | None, None -> left
    | None, Some _ -> right
    | Some _, None -> left
    | Some left_ras, Some right_ras ->
      { avail_before = Some (RAS.inter left_ras right_ras) }

  let less_equal { avail_before = left_avail } { avail_before = right_avail } :
      bool =
    match left_avail, right_avail with
    | None, None -> true
    | None, Some _ -> true
    | Some _, None -> false
    | Some left_ras, Some right_ras -> RAS.subset right_ras left_ras
end

(* [Transfer] calculates, given the registers "available before" an instruction
   [instr], the registers that are available both "across" and immediately after
   [instr]. This is a forwards dataflow analysis.

   Registers not in [all_regs_that_might_be_named] are ignored, to improve
   performance.

   "available before" can be thought of, at the assembly level, as the set of
   registers available when the program counter is equal to the address of the
   particular instruction under consideration (that is to say, immediately prior
   to the instruction being executed). Inputs to that instruction are available
   at this point even if the instruction will clobber them. Results from the
   previous instruction are also available at this point.

   "available across" is the registers available during the execution of some
   particular instruction. These are the registers "available before" minus
   registers that may be clobbered or otherwise invalidated by the instruction.
   (The notion of "available across" is only useful for [Op] instructions.
   Recall that some of these may expand into multiple machine instructions
   including clobbers, e.g. for [Alloc].)

   The [available_before] and [available_across] fields of each instruction are
   updated by the transfer functions. *)
module Transfer = struct
  type domain = Domain.t

  type image =
    { normal : domain;
      exceptional : domain
    }

  let unreachable = RAS.Unreachable

  let ok set = RAS.Ok set

  let[@inline] common :
      type a.
      avail_before:RD.Set.t ->
      destroyed_at:(a -> Reg.t array) ->
      is_interesting_constructor:(a -> bool) ->
      is_end_region:(a -> bool) ->
      a Cfg.instruction ->
      RAS.t option * RAS.t =
   fun ~avail_before ~destroyed_at ~is_interesting_constructor ~is_end_region
       instr ->
    (* We split the calculation of registers that become unavailable after a
       call into two parts. First: anything that the target marks as destroyed
       by the operation, combined with any registers that will be clobbered by
       the operation writing out its results. *)
    let made_unavailable_1 =
      let regs_clobbered = Array.append (destroyed_at instr.desc) instr.res in
      RD.Set.made_unavailable_by_clobber avail_before ~regs_clobbered
        ~register_class:Proc.register_class ~stack_class:(fun r ->
          Proc.stack_slot_class r.typ)
    in
    (* Second: the cases of (a) allocations, (b) other polling points, (c) OCaml
       to OCaml function calls and (d) end-region operations. In these cases,
       since the GC may run, registers always become unavailable unless: (a)
       they are "live across" the instruction; and/or (b) they hold immediates
       and are assigned to the stack. For the moment we assume that [Ispecific]
       instructions do not run the GC. *)
    (* CR-someday mshinwell: Consider factoring this out from here and
       [Available_ranges.Make_ranges.end_pos_offset]. *)
    let made_unavailable_2 =
      match is_interesting_constructor instr.desc with
      | true ->
        RD.Set.filter
          (fun reg ->
            let holds_immediate = RD.holds_non_pointer reg in
            let on_stack = RD.assigned_to_stack reg in
            let live_across = Reg.Set.mem (RD.reg reg) instr.live in
            let remains_available =
              live_across || (holds_immediate && on_stack)
            in
            let reg_is_of_type_addr =
              match (RD.reg reg).typ with
              | Addr -> true
              | Val | Int | Float | Vec128 -> false
            in
            if remains_available
               || (not (extend_live ()))
               || is_end_region instr.desc
               || (not (RD.assigned_to_stack reg))
               || RD.Set.mem reg made_unavailable_1
               || reg_is_of_type_addr
            then not remains_available
            else (
              instr.live <- Reg.Set.add (RD.reg reg) instr.live;
              false))
          avail_before
      | false -> RD.Set.empty
    in
    let made_unavailable = RD.Set.union made_unavailable_1 made_unavailable_2 in
    let avail_across = RD.Set.diff avail_before made_unavailable in
    let avail_after =
      (* If a result register will never be named, we can forget about it for
         the purposes of this analysis. *)
      let res = Reg.inter_set_array !all_regs_that_might_be_named instr.res in
      RD.Set.union (RD.Set.without_debug_info res) avail_across
    in
    Some (ok avail_across), ok avail_after

  let basic ({ avail_before } : domain) (instr : Cfg.basic Cfg.instruction) :
      domain =
    assert (Option.is_some avail_before);
    instr.available_before <- avail_before;
    let avail_before = Option.get avail_before in
    if !Dwarf_flags.ddebug_invariants
    then check_invariants instr ~print_instr:Cfg.print_basic ~avail_before;
    let avail_across, avail_after =
      match avail_before with
      | Unreachable -> None, unreachable
      | Ok avail_before -> (
        match instr.desc with
        | Op
            (Name_for_debugger
              { ident; which_parameter; provenance; is_assignment; regs }) ->
          (* First forget about any existing debug info to do with [ident] if
             the naming corresponds to an assignment operation. *)
          let forgetting_ident : RD.Set.t =
            if not is_assignment
            then avail_before
            else
              RD.Set.map
                (fun reg ->
                  match RD.debug_info reg with
                  | None -> reg
                  | Some debug_info ->
                    if V.same (RD.Debug_info.holds_value_of debug_info) ident
                    then RD.clear_debug_info reg
                    else reg)
                avail_before
          in
          let avail_after = ref forgetting_ident in
          let num_parts_of_value = Array.length regs in
          (* Add debug info about [ident], but only for registers that are known
             to be available. *)
          for part_of_value = 0 to num_parts_of_value - 1 do
            let reg = regs.(part_of_value) in
            if RD.Set.mem_reg forgetting_ident reg
            then
              let regd =
                RD.create ~reg ~holds_value_of:ident ~part_of_value
                  ~num_parts_of_value ~which_parameter ~provenance
              in
              avail_after
                := RD.Set.add regd (RD.Set.filter_reg !avail_after reg)
          done;
          Some (ok avail_before), ok !avail_after
        | Op (Move | Reload | Spill) ->
          (* Moves are special: they enable us to propagate names. No-op moves
             need to be handled specially---in this case, we may learn that a
             given hard register holds the value of multiple pseudoregisters
             (all of which have the same value). This makes us match up properly
             with [Cfg_liveness]. *)
          let move_to_same_location =
            let move_to_same_location = ref true in
            for i = 0 to Array.length instr.arg - 1 do
              let arg = instr.arg.(i) in
              let res = instr.res.(i) in
              (* Note that the register classes must be the same, so we don't
                 need to check that. *)
              if arg.loc <> res.loc then move_to_same_location := false
            done;
            !move_to_same_location
          in
          let made_unavailable =
            if move_to_same_location
            then RD.Set.empty
            else
              RD.Set.made_unavailable_by_clobber avail_before
                ~regs_clobbered:instr.res ~register_class:Proc.register_class
                ~stack_class:(fun r -> Proc.stack_slot_class r.typ)
          in
          let results =
            Array.map2
              (fun arg_reg result_reg ->
                match RD.Set.find_reg_exn avail_before arg_reg with
                | exception Not_found ->
                  (* Note that [arg_reg] might not be in
                     [all_regs_that_might_be_named], meaning it wouldn't be
                     found in [avail_before]. In that case we shouldn't
                     propagate anything. *)
                  None
                | arg_reg ->
                  if Option.is_some (RD.debug_info arg_reg)
                  then
                    Some
                      (RD.create_copying_debug_info ~reg:result_reg
                         ~debug_info_from:arg_reg)
                  else None)
              instr.arg instr.res
          in
          let avail_across = RD.Set.diff avail_before made_unavailable in
          let avail_after =
            Array.fold_left
              (fun avail_after reg_opt ->
                match reg_opt with
                | None -> avail_after
                | Some reg -> RD.Set.add reg avail_after)
              avail_across results
          in
          Some (ok avail_across), ok avail_after
        | Op
            ( Const_int _ | Const_float _ | Const_symbol _ | Const_vec128 _
            | Stackoffset _ | Load _ | Store _ | Intop _ | Intop_imm _
            | Intop_atomic _ | Negf | Absf | Addf | Subf | Mulf | Divf | Compf _
            | Csel _ | Floatofint | Intoffloat | Valueofint | Intofvalue
            | Probe_is_enabled _ | Opaque | Begin_region | End_region
            | Specific _ )
        | Reloadretaddr | Pushtrap _ | Poptrap | Prologue ->
          let is_op_end_region = function[@ocaml.warning "-4"]
            | Cfg.(Op End_region) -> true
            | _ -> false
          in
          common ~avail_before ~destroyed_at:Proc.destroyed_at_basic
            ~is_interesting_constructor:is_op_end_region
            ~is_end_region:is_op_end_region instr)
    in
    instr.available_across <- avail_across;
    { avail_before = Some avail_after }

  let terminator ({ avail_before } : domain)
      (term : Cfg.terminator Cfg.instruction) : image =
    assert (Option.is_some avail_before);
    term.available_before <- avail_before;
    let avail_before = Option.get avail_before in
    if !Dwarf_flags.ddebug_invariants
    then check_invariants term ~print_instr:Cfg.print_terminator ~avail_before;
    let avail_across, avail_after =
      match avail_before with
      | Unreachable -> None, unreachable
      | Ok avail_before -> (
        match term.desc with
        | Never -> assert false
        | Tailcall_self _ ->
          (* CR xclerc for xclerc: TODO *)
          None, unreachable
        | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
        | Switch _ | Call _ | Prim _ | Specific_can_raise _ | Poll_and_jump _
        | Return | Raise _ | Tailcall_func _ | Call_no_return _ ->
          common ~avail_before ~destroyed_at:Proc.destroyed_at_terminator
            ~is_interesting_constructor:
              Cfg.(
                function
                | Never -> assert false
                | Call _
                | Prim { op = Alloc _ | Probe _; label_after = _ }
                | Poll_and_jump _ ->
                  true
                | Always _ | Parity_test _ | Truth_test _ | Float_test _
                | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
                | Tailcall_func _ | Call_no_return _ | Specific_can_raise _
                | Prim { op = External _ | Checkbound _; label_after = _ } ->
                  false)
            ~is_end_region:(fun _ -> false)
            term)
    in
    term.available_across <- avail_across;
    let avail_before_handler =
      match avail_after with
      | Unreachable -> unreachable
      | Ok avail_at_raise ->
        let without_exn_bucket =
          RD.Set.filter_reg avail_at_raise Proc.loc_exn_bucket
        in
        let with_anonymous_exn_bucket =
          RD.Set.add
            (RD.create_without_debug_info ~reg:Proc.loc_exn_bucket)
            without_exn_bucket
        in
        ok with_anonymous_exn_bucket
    in
    { normal = { avail_before = Some avail_after };
      exceptional = { avail_before = Some avail_before_handler }
    }
end

module Analysis = Cfg_dataflow.Forward (Domain) (Transfer)

let compute_all_regs_that_might_be_named : Cfg.t -> Reg.Set.t =
 fun cfg ->
  Cfg.fold_blocks cfg ~init:Reg.Set.empty ~f:(fun _label block acc ->
      DLL.fold_left block.body ~init:acc ~f:(fun acc instr ->
          match[@ocaml.warning "-4"] instr.Cfg.desc with
          | Cfg.(Op (Name_for_debugger { regs; _ })) ->
            Reg.add_set_array acc regs
          | _ -> acc))

let run : Cfg_with_layout.t -> Cfg_with_layout.t =
 fun cfg_with_layout ->
  if !Clflags.debug && not !Dwarf_flags.restrict_to_upstream_dwarf
  then (
    let cfg = Cfg_with_layout.cfg cfg_with_layout in
    let fun_args = R.set_of_array cfg.fun_args in
    let avail_before = RAS.Ok (RD.Set.without_debug_info fun_args) in
    all_regs_that_might_be_named := compute_all_regs_that_might_be_named cfg;
    let init : Domain.t = { Domain.avail_before = Some avail_before } in
    match Analysis.run cfg ~init ~handlers_are_entry_points:false () with
    | Error () ->
      Misc.fatal_errorf "Cfg_available_regs.run: dataflow analysis failed"
    | Ok (_ : Domain.t Label.Tbl.t) -> ());
  cfg_with_layout
