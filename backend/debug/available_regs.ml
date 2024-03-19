(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Mark Shinwell and Thomas Refis, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+4"]

(* CR mshinwell: We need a Cfg version of this pass. *)

module M = Mach
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

let disable_extend_live = ref false

(* This pass treats [avail_at_exit] like a "result" structure whereas the
   equivalent in [Liveness] is like an "environment". (Which means we need to be
   careful not to throw away information about further-out catch handlers
   collected in [avail_at_exit].) *)
let avail_at_exit = Hashtbl.create 42

let current_trap_stack = ref M.Uncaught

let augment_availability_at_exit nfail avail_before =
  let avail_at_top_of_handler =
    match Hashtbl.find avail_at_exit nfail with
    | exception Not_found ->
      Misc.fatal_errorf "Iexit %d not in scope of Icatch" nfail
    | avail_at_top_of_handler -> avail_at_top_of_handler
  in
  let avail_at_top_of_handler =
    RAS.inter avail_at_top_of_handler avail_before
  in
  Hashtbl.replace avail_at_exit nfail avail_at_top_of_handler

let augment_availability_at_raise avail =
  match !current_trap_stack with
  | Uncaught -> ()
  | Specific_trap (label, _) -> augment_availability_at_exit label avail

let check_invariants (instr : M.instruction) ~all_regs_that_might_be_named
    ~(avail_before : RAS.t) =
  match avail_before with
  | Unreachable -> ()
  | Ok avail_before ->
    (* Every register that is live and named across an instruction should also
       be available before the instruction. *)
    let live = R.Set.inter instr.live all_regs_that_might_be_named in
    if not (R.Set.subset live (RD.Set.forget_debug_info avail_before))
    then
      Misc.fatal_errorf
        "Named live registers not a subset of available registers: live={%a}  \
         avail_before=%a missing={%a} insn=%a"
        Printmach.regset live
        (RAS.print ~print_reg:Printmach.reg)
        (RAS.Ok avail_before) Printmach.regset
        (R.Set.diff live (RD.Set.forget_debug_info avail_before))
        Printmach.instr
        { instr with M.next = M.end_instr () };
    (* Every named register that is an input to an instruction should be
       available. *)
    let args =
      R.Set.inter (R.set_of_array instr.arg) all_regs_that_might_be_named
    in
    let avail_before_fdi = RD.Set.forget_debug_info avail_before in
    if not (R.Set.subset args avail_before_fdi)
    then
      Misc.fatal_errorf
        "Instruction has unavailable input register(s): avail_before=%a \
         avail_before_fdi={%a} inputs={%a} insn=%a"
        (RAS.print ~print_reg:Printmach.reg)
        (RAS.Ok avail_before) Printmach.regset avail_before_fdi Printmach.regset
        args Printmach.instr
        { instr with M.next = M.end_instr () }

(* [available_regs ~instr ~all_regs_that_might_be_named ~avail_before]
   calculates, given the registers "available before" an instruction [instr],
   the registers that are available both "across" and immediately after [instr].
   This is a forwards dataflow analysis.

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
   (The notion of "available across" is only useful for [Iop] instructions.
   Recall that some of these may expand into multiple machine instructions
   including clobbers, e.g. for [Ialloc].)

   The [available_before] and [available_across] fields of each instruction are
   updated by this function. *)
let rec available_regs (instr : M.instruction) ~all_regs_that_might_be_named
    ~(avail_before : RAS.t) : RAS.t =
  if !Dwarf_flags.ddebug_invariants
  then check_invariants instr ~all_regs_that_might_be_named ~avail_before;
  instr.available_before <- avail_before;
  let avail_across, avail_after =
    let ok set = RAS.Ok set in
    let unreachable = RAS.Unreachable in
    match avail_before with
    | Unreachable -> None, unreachable
    | Ok avail_before -> (
      match instr.desc with
      | Iend -> None, ok avail_before
      | Ireturn _ -> None, unreachable
      | Iop Itailcall_ind | Iop (Itailcall_imm _) ->
        Some (ok Reg_with_debug_info.Set.empty), unreachable
      | Iop
          (Iname_for_debugger
            { ident; which_parameter; provenance; is_assignment; regs }) ->
        (* First forget about any existing debug info to do with [ident] if the
           naming corresponds to an assignment operation. *)
        let forgetting_ident =
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
            avail_after := RD.Set.add regd (RD.Set.filter_reg !avail_after reg)
        done;
        Some (ok avail_before), ok !avail_after
      | Iop (Imove | Ireload | Ispill) ->
        (* Moves are special: they enable us to propagate names. No-op moves
           need to be handled specially---in this case, we may learn that a
           given hard register holds the value of multiple pseudoregisters (all
           of which have the same value). This makes us match up properly with
           [Liveness]. *)
        let move_to_same_location =
          let move_to_same_location = ref true in
          for i = 0 to Array.length instr.arg - 1 do
            let arg = instr.arg.(i) in
            let res = instr.res.(i) in
            (* Note that the register classes must be the same, so we don't need
               to check that. *)
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
                   [all_regs_that_might_be_named], meaning it wouldn't be found
                   in [avail_before]. In that case we shouldn't propagate
                   anything. *)
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
      | Iop
          (( Icall_ind | Icall_imm _ | Ialloc _ | Ipoll _ | Iprobe _
           | Iconst_int _ | Iconst_float32 _ | Iconst_float _
           | Iconst_vec128 _ | Iconst_symbol _
           | Iextcall _ | Istackoffset _ | Iload _ | Istore _ | Iintop _
           | Iintop_imm _ | Iintop_atomic _ | Icompf _ | Inegf | Iabsf | Iaddf
           | Isubf | Imulf | Idivf | Icsel _ | Ivalueofint | Iintofvalue
           | Iopaque | Ispecific _ | Iscalarcast _ | Ivectorcast _
           | Iprobe_is_enabled _ | Ibeginregion | Iendregion | Idls_get ) as op)
        ->
        (* We split the calculation of registers that become unavailable after a
           call into two parts. First: anything that the target marks as
           destroyed by the operation, combined with any registers that will be
           clobbered by the operation writing out its results. *)
        let made_unavailable_1 =
          let regs_clobbered =
            Array.append (Proc.destroyed_at_oper instr.desc) instr.res
          in
          RD.Set.made_unavailable_by_clobber avail_before ~regs_clobbered
            ~register_class:Proc.register_class ~stack_class:(fun r ->
              Proc.stack_slot_class r.typ)
        in
        (* Second: the cases of (a) allocations, (b) other polling points, (c)
           OCaml to OCaml function calls and (d) end-region operations. In these
           cases, since the GC may run, registers always become unavailable
           unless: (a) they are "live across" the instruction; and/or (b) they
           hold immediates and are assigned to the stack. For the moment we
           assume that [Ispecific] instructions do not run the GC. *)
        (* CR-someday mshinwell: Consider factoring this out from here and
           [Available_ranges.Make_ranges.end_pos_offset]. *)
        let made_unavailable_2 =
          match op with
          | Icall_ind | Icall_imm _ | Ialloc _ | Ipoll _ | Iprobe _ | Iendregion
            ->
            RD.Set.filter
              (fun reg ->
                let holds_immediate = RD.holds_non_pointer reg in
                let on_stack = RD.assigned_to_stack reg in
                let live_across = Reg.Set.mem (RD.reg reg) instr.live in
                let remains_available =
                  live_across || (holds_immediate && on_stack)
                in
                let is_end_region =
                  (* The live range extension can't be done for end-region, as
                     the relevant range of the stack could be reused. *)
                  match[@ocaml.warning "-4"] op with
                  | Iendregion -> true
                  | _ -> false
                in
                let reg_is_of_type_addr =
                  match (RD.reg reg).typ with
                  | Addr -> true
                  | Val | Int | Float | Vec128 -> false
                in
                if remains_available
                   || (not (extend_live ()))
                   || is_end_region
                   || (not (RD.assigned_to_stack reg))
                   || RD.Set.mem reg made_unavailable_1
                   || reg_is_of_type_addr || !disable_extend_live
                then not remains_available
                else (
                  instr.live <- Reg.Set.add (RD.reg reg) instr.live;
                  false))
              avail_before
          | Imove | Ispill | Ireload
          | Iconst_int _ | Iconst_float32 _ | Iconst_float _
          | Iconst_vec128 _ | Iconst_symbol _ | Itailcall_ind | Itailcall_imm _
          | Iextcall _ | Istackoffset _ | Iload _ | Istore _ | Iintop _
          | Iintop_imm _ | Iintop_atomic _ | Icompf _ | Inegf | Iabsf | Iaddf
          | Isubf | Imulf | Idivf | Icsel _ | Ivalueofint | Iintofvalue
          | Iopaque | Ispecific _ | Iscalarcast _ | Ivectorcast _
          | Iname_for_debugger _ | Iprobe_is_enabled _ | Ibeginregion | Idls_get
            ->
            RD.Set.empty
        in
        let made_unavailable =
          RD.Set.union made_unavailable_1 made_unavailable_2
        in
        let avail_across = RD.Set.diff avail_before made_unavailable in
        if M.operation_can_raise op
        then augment_availability_at_raise (ok avail_across);
        let avail_after =
          (* If a result register will never be named, we can forget about it
             for the purposes of this analysis. *)
          let res =
            Reg.inter_set_array all_regs_that_might_be_named instr.res
          in
          RD.Set.union (RD.Set.without_debug_info res) avail_across
        in
        Some (ok avail_across), ok avail_after
      | Iifthenelse (_, ifso, ifnot) ->
        join [ifso; ifnot] ~all_regs_that_might_be_named ~avail_before
      | Iswitch (_, cases) ->
        (* CR mshinwell: Proc.destroyed_at_oper actually applies to all
           instructions, so we need to call it in more cases. We're only going
           to implement this on the Cfg version of this pass though as these are
           probably mostly corner cases, it's easier to implement on Cfg, and
           we'll be switching to that soon. *)
        join (Array.to_list cases) ~all_regs_that_might_be_named ~avail_before
      | Icatch (recursive, ts, handlers, body) ->
        let old_disable_extend_live = !disable_extend_live in
        (match recursive with
        | Nonrecursive -> ()
        | Recursive ->
          (* In extend-live mode, we disable extension of any live ranges until
             the fixed point has been reached. *)
          disable_extend_live := true);
        List.iter
          (fun (nfail, _ts, _handler, _) ->
            (* In case there are nested [Icatch] expressions with the same
               handler numbers, we rely on the [Hashtbl] shadowing semantics. *)
            Hashtbl.add avail_at_exit nfail unreachable)
          handlers;
        let avail_after_body =
          available_regs body ~all_regs_that_might_be_named
            ~avail_before:(ok avail_before)
        in
        (* CR-someday mshinwell: Consider potential efficiency speedups (see
           suggestions from @chambart on GPR#856). *)
        let aux (nfail, ts, handler, _) (nfail', avail_at_top_of_handler) =
          assert (nfail = nfail');
          current_trap_stack := ts;
          available_regs handler ~all_regs_that_might_be_named
            ~avail_before:avail_at_top_of_handler
        in
        let aux_equal (nfail, avail_before_handler)
            (nfail', avail_before_handler') =
          assert (nfail = nfail');
          current_trap_stack := ts;
          RAS.equal avail_before_handler avail_before_handler'
        in
        let rec fixpoint avail_at_top_of_handlers =
          let avail_after_handlers =
            List.map2 aux handlers avail_at_top_of_handlers
          in
          let avail_at_top_of_handlers' =
            List.map
              (fun (nfail, _ts, _handler, _) ->
                match Hashtbl.find avail_at_exit nfail with
                | exception Not_found -> assert false (* see above *)
                | avail_at_top_of_handler -> nfail, avail_at_top_of_handler)
              handlers
          in
          match recursive with
          | Nonrecursive -> avail_after_handlers
          | Recursive ->
            if List.for_all2 aux_equal avail_at_top_of_handlers
                 avail_at_top_of_handlers'
            then
              (* In extend-live mode, do one more round, during which the
                 availability sets on the instructions will be updated. We can
                 skip this if we're in a nested loop and live range extension is
                 currently disabled; when it becomes enabled again at the outer
                 loop the live sets will be updated. *)
              if (not (extend_live ())) || old_disable_extend_live
              then avail_after_handlers
              else (
                disable_extend_live := old_disable_extend_live;
                List.map2 aux handlers avail_at_top_of_handlers)
            else fixpoint avail_at_top_of_handlers'
        in
        let init_avail_at_top_of_handlers =
          List.map
            (fun (nfail, _ts, _handler, _) ->
              match Hashtbl.find avail_at_exit nfail with
              | exception Not_found -> assert false (* see above *)
              | avail_at_top_of_handler -> nfail, avail_at_top_of_handler)
            handlers
        in
        let avail_after_handlers = fixpoint init_avail_at_top_of_handlers in
        List.iter
          (fun (nfail, _ts, _handler, _) -> Hashtbl.remove avail_at_exit nfail)
          handlers;
        current_trap_stack := ts;
        let avail_after =
          List.fold_left
            (fun avail_at_join avail_after_handler ->
              RAS.inter avail_at_join avail_after_handler)
            avail_after_body avail_after_handlers
        in
        None, avail_after
      | Iexit (nfail, _traps) ->
        let avail_before = ok avail_before in
        augment_availability_at_exit nfail avail_before;
        None, unreachable
      | Itrywith (body, nfail, (ts, handler)) ->
        Hashtbl.add avail_at_exit nfail unreachable;
        let avail_before = ok avail_before in
        let after_body =
          available_regs body ~all_regs_that_might_be_named ~avail_before
        in
        let avail_before_handler =
          let with_exn_bucket = Hashtbl.find avail_at_exit nfail in
          match (with_exn_bucket : RAS.t) with
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
        let saved_trap_stack = !current_trap_stack in
        current_trap_stack := ts;
        let avail_after =
          RAS.inter after_body
            (available_regs handler ~all_regs_that_might_be_named
               ~avail_before:avail_before_handler)
        in
        current_trap_stack := saved_trap_stack;
        Hashtbl.remove avail_at_exit nfail;
        None, avail_after
      | Iraise _ ->
        let avail_before = ok avail_before in
        augment_availability_at_raise avail_before;
        None, unreachable)
  in
  instr.available_across <- avail_across;
  match[@ocaml.warning "-4"] instr.desc with
  | Iend -> avail_after
  | _ ->
    available_regs instr.next ~all_regs_that_might_be_named
      ~avail_before:avail_after

and join branches ~all_regs_that_might_be_named ~avail_before =
  let avail_before = RAS.Ok avail_before in
  let avails =
    List.map
      (available_regs ~all_regs_that_might_be_named ~avail_before)
      branches
  in
  let avail_after =
    match avails with
    | [] -> avail_before
    | avail :: avails -> List.fold_left RAS.inter avail avails
  in
  None, avail_after

let all_regs_that_might_be_named instr =
  let all_regs = ref Reg.Set.empty in
  Mach.instr_iter
    (fun (instr : Mach.instruction) ->
      match[@ocaml.warning "-4"] instr.desc with
      | Iop (Iname_for_debugger { regs; _ }) ->
        all_regs := Reg.Set.union (Reg.set_of_array regs) !all_regs
      | _ -> ())
    instr;
  !all_regs

let fundecl (f : M.fundecl) =
  if !Clflags.debug && not !Dwarf_flags.restrict_to_upstream_dwarf
  then (
    assert (Hashtbl.length avail_at_exit = 0);
    current_trap_stack := M.Uncaught;
    disable_extend_live := false;
    let fun_args = R.set_of_array f.fun_args in
    let avail_before = RAS.Ok (RD.Set.without_debug_info fun_args) in
    let all_regs_that_might_be_named =
      all_regs_that_might_be_named f.fun_body
    in
    ignore
      (available_regs f.fun_body ~all_regs_that_might_be_named ~avail_before
        : RAS.t));
  f
