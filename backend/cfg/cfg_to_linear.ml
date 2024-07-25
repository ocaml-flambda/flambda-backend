(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2021 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)
[@@@ocaml.warning "+a-30-40-41-42"]

module CL = Cfg_with_layout
module L = Linear
module DLL = Flambda_backend_utils.Doubly_linked_list

let to_linear_instr ?(like : _ Cfg.instruction option) desc ~next :
    L.instruction =
  let arg, res, dbg, live, fdo, available_before, available_across =
    match like with
    | None ->
      [||], [||], Debuginfo.none, Reg.Set.empty, Fdo_info.none, None, None
    | Some like ->
      ( like.arg,
        like.res,
        like.dbg,
        like.live,
        like.fdo,
        like.available_before,
        like.available_across )
  in
  { desc; next; arg; res; dbg; live; fdo; available_before; available_across }

let basic_to_linear (i : _ Cfg.instruction) ~next =
  let desc = Cfg_to_linear_desc.from_basic i.desc in
  to_linear_instr ~like:i desc ~next

let mk_int_test ~lt ~eq ~gt : Cmm.integer_comparison =
  match eq, lt, gt with
  | true, false, false -> Ceq
  | false, true, false -> Clt
  | false, false, true -> Cgt
  | false, true, true -> Cne
  | true, true, false -> Cle
  | true, false, true -> Cge
  | true, true, true -> assert false
  | false, false, false -> assert false

(* Certain "unordered" outcomes of float comparisons are not expressible as a
   single Cmm.float_comparison operator, or a disjunction of disjoint
   Cmm.float_comparison operators. For example, for float_test { lt = L0; eq =
   L0; gt = L0; uo = L1 } there is no Mach comparison for the branch to L1.

   We haven't seen a program that leads to it yet, but it is possible that
   future transformations will. So, for now these cases are fatal error. If we
   need to handle them, if needed, we can emit an unconditional jump that
   appears last, after all other conditional jumps. *)
type float_cond =
  | Must_be_last
  | Any of Cmm.float_comparison

let mk_float_cond ~lt ~eq ~gt ~uo =
  match eq, lt, gt, uo with
  | true, false, false, false -> Any CFeq
  | false, true, false, false -> Any CFlt
  | false, false, true, false -> Any CFgt
  | true, true, false, false -> Any CFle
  | true, false, true, false -> Any CFge
  | false, true, true, true -> Any CFneq
  | true, false, true, true -> Any CFnlt
  | true, true, false, true -> Any CFngt
  | false, false, true, true -> Any CFnle
  | false, true, false, true -> Any CFnge
  | true, true, true, true -> assert false (* unconditional jump *)
  | false, false, false, false -> assert false (* no successors *)
  | true, true, true, false ->
    Misc.fatal_error "Encountered disjunction of conditions: [CFle; CFgt]"
  (* Any [CFle; CFgt] *)
  (* CR-someday gyorsh: if this case is reachable, how to choose between
     equivalent representations: [CFle;CFgt] [CFlt;CFge] [CFlt;CFeq;CFgt] *)
  | false, true, true, false ->
    Misc.fatal_error "Encountered disjunction of conditions [CFlt; CFgt]"
  (* Any [CFlt; CFgt] *)
  | false, false, false, true -> Must_be_last
  | true, false, false, true -> Must_be_last

let cross_section cfg_with_layout src dst =
  if !Flambda_backend_flags.basic_block_sections
     && not (Label.equal dst Linear_utils.labelled_insn_end.label)
  then
    let src_section = CL.get_section cfg_with_layout src in
    let dst_section = CL.get_section cfg_with_layout dst in
    match src_section, dst_section with
    | None, None -> false
    | Some src_name, Some dst_name -> not (String.equal src_name dst_name)
    | Some _, None -> Misc.fatal_errorf "Missing section for %d" dst
    | None, Some _ -> Misc.fatal_errorf "Missing section for %d" src
  else false

let linearize_terminator cfg_with_layout (func : string) start
    (terminator : Cfg.terminator Cfg.instruction)
    ~(next : Linear_utils.labelled_insn) : L.instruction * Label.t option =
  (* CR-someday gyorsh: refactor, a lot of redundant code for different cases *)
  (* CR-someday gyorsh: for successor labels that are not fallthrough, order of
     branch instructions should depend on perf data and possibly the relative
     position of the target labels and the current block: whether the jumps are
     forward or back. This information can be obtained from the layout. For now,
     we are making an arbitrary choice. *)
  (* If one of the successors is a fallthrough label, do not emit a jump for it.
     Otherwise, the last jump is unconditional. *)
  let branch_or_fallthrough d lbl =
    if (not (Label.equal next.label lbl))
       || cross_section cfg_with_layout start lbl
    then d @ [L.Lbranch lbl]
    else d
  in
  let single d = [d], None in
  let emit_bool (c1, l1) (c2, l2) =
    let branch_or_fallthrough_next =
      if cross_section cfg_with_layout start next.label
      then [L.Lbranch next.label]
      else []
    in
    (* c1 must be the inverse of c2 *)
    match Label.equal l1 next.label, Label.equal l2 next.label with
    | true, true -> branch_or_fallthrough_next
    | false, true -> L.Lcondbranch (c1, l1) :: branch_or_fallthrough_next
    | true, false -> L.Lcondbranch (c2, l2) :: branch_or_fallthrough_next
    | false, false ->
      if Label.equal l1 l2
      then [L.Lbranch l1]
      else [L.Lcondbranch (c1, l1); L.Lbranch l2]
  in
  let desc_list, tailrec_label =
    match terminator.desc with
    | Return -> [L.Lreturn], None
    | Raise kind -> [L.Lraise kind], None
    | Tailcall_func { op = Indirect; tail } ->
      [L.Lop (Itailcall_ind { tail })], None
    | Tailcall_func { op = Direct func_symbol; tail } ->
      [L.Lop (Itailcall_imm { func = func_symbol; tail })], None
    | Tailcall_self { op = { destination }; tail } ->
      ( [ L.Lop
            (Itailcall_imm
               { func = { sym_name = func; sym_global = Local }; tail }) ],
        Some destination )
    | Call_no_return { func_symbol; alloc; ty_args; ty_res; stack_ofs } ->
      single
        (L.Lop
           (Iextcall
              { func = func_symbol;
                alloc;
                ty_args;
                ty_res;
                returns = false;
                stack_ofs
              }))
    | Call { op; label_after; tail } ->
      let op : Mach.operation =
        match op with
        | Indirect -> Icall_ind { tail }
        | Direct func_symbol -> Icall_imm { func = func_symbol; tail }
      in
      branch_or_fallthrough [L.Lop op] label_after, None
    | Prim { op; label_after } ->
      let op : Mach.operation =
        match op with
        | External { func_symbol; alloc; ty_args; ty_res; stack_ofs } ->
          Iextcall
            { func = func_symbol;
              alloc;
              ty_args;
              ty_res;
              returns = true;
              stack_ofs
            }
        | Probe { name; handler_code_sym; enabled_at_init } ->
          Iprobe { name; handler_code_sym; enabled_at_init }
      in
      branch_or_fallthrough [L.Lop op] label_after, None
    | Specific_can_raise { op; label_after } ->
      branch_or_fallthrough [L.Lop (Ispecific op)] label_after, None
    | Switch labels -> single (L.Lswitch labels)
    | Never -> Misc.fatal_error "Cannot linearize terminator: Never"
    | Always label -> branch_or_fallthrough [] label, None
    | Parity_test { ifso; ifnot } ->
      emit_bool (Ieventest, ifso) (Ioddtest, ifnot), None
    | Truth_test { ifso; ifnot } ->
      emit_bool (Itruetest, ifso) (Ifalsetest, ifnot), None
    | Float_test { width; lt; eq; gt; uo } -> (
      let successor_labels =
        Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq
        |> Label.Set.add uo
      in
      match Label.Set.cardinal successor_labels with
      | 0 -> assert false
      | 1 -> branch_or_fallthrough [] (Label.Set.min_elt successor_labels), None
      | 2 | 3 | 4 ->
        let must_be_last, any =
          Label.Set.fold
            (fun lbl (must_be_last, any) ->
              let cond =
                mk_float_cond ~lt:(Label.equal lt lbl) ~eq:(Label.equal eq lbl)
                  ~gt:(Label.equal gt lbl) ~uo:(Label.equal uo lbl)
              in
              match cond with
              | Any c -> must_be_last, (c, lbl) :: any
              | Must_be_last -> lbl :: must_be_last, any)
            successor_labels ([], [])
        in
        let last =
          match must_be_last with
          | [] ->
            if Label.Set.mem next.label successor_labels
            then next.label
            else
              (* arbitrary choice (also see CR above) *)
              Label.Set.min_elt successor_labels
          | [lbl] ->
            Printf.eprintf "One success label must be last: %d\n" lbl;
            (* CR-someday gyorsh: fail for safety, until we see a case that
               exhibits this behavior.. This behavior should not be possible
               with the current cfg construction. *)
            Misc.fatal_errorf
              "Illegal branch: one successor label must be last %d" lbl ()
          | _ ->
            Misc.fatal_error
              "Illegal branch: more than one successor label that must be last"
        in
        let branches =
          List.filter_map
            (fun (c, lbl) ->
              if Label.equal lbl last
              then None
              else Some (L.Lcondbranch (Ifloattest (width, c), lbl)))
            any
        in
        branches @ branch_or_fallthrough [] last, None
      | _ -> assert false)
    | Int_test { lt; eq; gt; imm; is_signed } -> (
      let successor_labels =
        Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq
      in
      match Label.Set.cardinal successor_labels with
      | 0 -> assert false
      | 1 -> branch_or_fallthrough [] (Label.Set.min_elt successor_labels), None
      | 2 | 3 ->
        (* If fallthrough label is a successor, do not emit a jump for it.
           Otherwise, the last jump could be unconditional. *)
        let last =
          if Label.Set.mem next.label successor_labels
          then next.label
          else
            (* arbitrary choice (see also CR above) *)
            Label.Set.min_elt successor_labels
        in
        let cond_successor_labels = Label.Set.remove last successor_labels in
        (* Lcondbranch3 is emitted as an unsigned comparison, see ocaml PR
           #8677 *)
        let can_emit_Lcondbranch3 =
          match is_signed, imm with
          | false, Some 1 -> true
          | false, Some _ | false, None | true, _ -> false
        in
        if Label.Set.cardinal cond_successor_labels = 2 && can_emit_Lcondbranch3
        then
          (* generates one cmp instruction for all conditional jumps here *)
          let find l =
            if (not (cross_section cfg_with_layout start l))
               && Label.equal next.label l
            then None
            else Some l
          in
          [L.Lcondbranch3 (find lt, find eq, find gt)], None
        else
          let init = branch_or_fallthrough [] last in
          ( Label.Set.fold
              (fun lbl acc ->
                let cond =
                  mk_int_test ~lt:(Label.equal lt lbl) ~eq:(Label.equal eq lbl)
                    ~gt:(Label.equal gt lbl)
                in
                let comp =
                  match is_signed with
                  | true -> Mach.Isigned cond
                  | false -> Mach.Iunsigned cond
                in
                let test =
                  match imm with
                  | None -> Mach.Iinttest comp
                  | Some n -> Mach.Iinttest_imm (comp, n)
                in
                L.Lcondbranch (test, lbl) :: acc)
              cond_successor_labels init,
            None )
      | _ -> assert false)
  in
  ( List.fold_left
      (fun next desc -> to_linear_instr ~like:terminator desc ~next)
      next.insn (List.rev desc_list),
    tailrec_label )

let need_starting_label (cfg_with_layout : CL.t) (block : Cfg.basic_block)
    ~(prev_block : Cfg.basic_block) =
  if block.is_trap_handler
  then true
  else if cross_section cfg_with_layout prev_block.start block.start
  then true
  else
    match Label.Set.elements block.predecessors with
    | [] | _ :: _ :: _ -> true
    | [pred] when not (Label.equal pred prev_block.start) -> true
    | [_] -> (
      (* This block has a single predecessor which appears in the layout
         immediately prior to this block. *)
      (* No need for the label, unless the predecessor's terminator is [Switch]
         when the label is needed for the jump table. *)
      match prev_block.terminator.desc with
      | Switch _ -> true
      | Never -> Misc.fatal_error "Cannot linearize terminator: Never"
      | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
      | Call _ | Prim _ | Specific_can_raise _ ->
        (* If the label came from the original [Linear] code, preserve it for
           checking that the conversion from [Linear] to [Cfg] and back is the
           identity; and for various assertions in reorder. *)
        let new_labels = CL.new_labels cfg_with_layout in
        CL.preserve_orig_labels cfg_with_layout
        && not (Label.Set.mem block.start new_labels)
      | Return | Raise _ | Tailcall_func _ | Tailcall_self _ | Call_no_return _
        ->
        assert false)

let adjust_stack_offset body (block : Cfg.basic_block)
    ~(prev_block : Cfg.basic_block) =
  let block_stack_offset = block.stack_offset in
  let prev_stack_offset = prev_block.terminator.stack_offset in
  if block_stack_offset = prev_stack_offset
  then body
  else
    let delta_bytes = block_stack_offset - prev_stack_offset in
    to_linear_instr (Ladjust_stack_offset { delta_bytes }) ~next:body

let make_Llabel cfg_with_layout label =
  Linear.Llabel
    { label;
      section_name =
        (if !Flambda_backend_flags.basic_block_sections
        then CL.get_section cfg_with_layout label
        else None)
    }

(* CR-someday gyorsh: handle duplicate labels in new layout: print the same
   block more than once. *)
let run cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  let layout = CL.layout cfg_with_layout in
  let next = ref Linear_utils.labelled_insn_end in
  let tailrec_label = ref None in
  DLL.iter_right_cell layout ~f:(fun cell ->
      let label = DLL.value cell in
      if not (Label.Tbl.mem cfg.blocks label)
      then Misc.fatal_errorf "Unknown block labelled %d\n" label;
      let block = Label.Tbl.find cfg.blocks label in
      assert (Label.equal label block.start);
      let body =
        let terminator, terminator_tailrec_label =
          linearize_terminator cfg_with_layout cfg.fun_name block.start
            block.terminator ~next:!next
        in
        (match !tailrec_label, terminator_tailrec_label with
        | (Some _ | None), None -> ()
        | None, Some _ -> tailrec_label := terminator_tailrec_label
        | Some old_trl, Some new_trl -> assert (Label.equal old_trl new_trl));
        DLL.fold_right
          ~f:(fun i next -> basic_to_linear i ~next)
          ~init:terminator block.body
      in
      let insn =
        match DLL.prev cell with
        | None -> body (* Entry block of the function. Don't add label. *)
        | Some prev_cell ->
          let body =
            if block.is_trap_handler
            then to_linear_instr Lentertrap ~next:body
            else body
          in
          let prev = DLL.value prev_cell in
          let prev_block = Label.Tbl.find cfg.blocks prev in
          let body =
            if need_starting_label cfg_with_layout block ~prev_block
            then
              to_linear_instr
                (make_Llabel cfg_with_layout block.start)
                ~next:body
            else body
          in
          adjust_stack_offset body block ~prev_block
      in
      next := { Linear_utils.label; insn });
  let fun_contains_calls = cfg.fun_contains_calls in
  let fun_num_stack_slots = cfg.fun_num_stack_slots in
  let fun_frame_required =
    Proc.frame_required ~fun_contains_calls ~fun_num_stack_slots
  in
  let fun_prologue_required =
    Proc.prologue_required ~fun_contains_calls ~fun_num_stack_slots
  in
  let fun_section_name =
    if !Flambda_backend_flags.basic_block_sections
    then CL.get_section cfg_with_layout cfg.entry_label
    else None
  in
  { Linear.fun_name = cfg.fun_name;
    fun_args = Reg.set_of_array cfg.fun_args;
    fun_body = !next.insn;
    fun_tailrec_entry_point_label = !tailrec_label;
    fun_fast = not (List.mem Cfg.Reduce_code_size cfg.fun_codegen_options);
    fun_dbg = cfg.fun_dbg;
    fun_contains_calls;
    fun_num_stack_slots;
    fun_frame_required;
    fun_prologue_required;
    fun_section_name
  }

let layout_of_block_list : Cfg.basic_block list -> Cfg_with_layout.layout =
 fun blocks ->
  let res = DLL.make_empty () in
  List.iter (fun block -> DLL.add_end res block.Cfg.start) blocks;
  res

(** debug print block as assembly *)
let print_assembly (blocks : Cfg.basic_block list) =
  (* create a fake cfg just for printing these blocks *)
  let layout = layout_of_block_list blocks in
  let fun_name = "_fun_start_" in
  let cfg =
    Cfg.create ~fun_name ~fun_args:[||] ~fun_codegen_options:[]
      ~fun_dbg:Debuginfo.none ~fun_contains_calls:true ~fun_num_stack_slots:[||]
      ~fun_poll:Default_poll
  in
  List.iter
    (fun (block : Cfg.basic_block) ->
      Label.Tbl.add cfg.blocks block.start block)
    blocks;
  let cl =
    Cfg_with_layout.create cfg ~layout ~new_labels:Label.Set.empty
      ~preserve_orig_labels:true
  in
  let fundecl = run cl in
  X86_proc.reset_asm_code ();
  Emit.fundecl fundecl;
  X86_proc.generate_code (Some (X86_gas.generate_asm !Emitaux.output_channel))
