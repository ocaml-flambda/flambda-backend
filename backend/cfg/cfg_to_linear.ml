[@@@ocaml.warning "+a-30-40-41-42"]

module CL = Cfg_with_layout
module L = Linear

let to_linear_instr ?(like : _ Cfg.instruction option) desc ~next :
    L.instruction =
  let arg, res, dbg, live =
    match like with
    | None -> ([||], [||], Debuginfo.none, Reg.Set.empty)
    | Some like -> (like.arg, like.res, like.dbg, like.live)
  in
  { desc; next; arg; res; dbg; live }

let from_basic (basic : Cfg.basic) : L.instruction_desc =
  match basic with
  | Prologue -> Lprologue
  | Reloadretaddr -> Lreloadretaddr
  | Pushtrap { lbl_handler } -> Lpushtrap { lbl_handler }
  | Poptrap -> Lpoptrap
  | Call (F (Indirect { label_after })) -> Lop (Icall_ind { label_after })
  | Call (F (Direct { func_symbol; label_after })) ->
      Lop (Icall_imm { func = func_symbol; label_after })
  | Call (P (External { func_symbol; alloc; label_after })) ->
      Lop (Iextcall { func = func_symbol; alloc; label_after })
  | Call
      (P
        (Checkbound { immediate = None; label_after_error; spacetime_index }))
    ->
      Lop (Iintop (Icheckbound { label_after_error; spacetime_index }))
  | Call
      (P
        (Checkbound
          { immediate = Some i; label_after_error; spacetime_index })) ->
      Lop
        (Iintop_imm (Icheckbound { label_after_error; spacetime_index }, i))
  | Call (P (Alloc { bytes; label_after_call_gc; dbginfo; spacetime_index }))
    ->
      Lop (Ialloc { bytes; label_after_call_gc; dbginfo; spacetime_index })
  | Op op ->
      let op : Mach.operation =
        match op with
        | Move -> Imove
        | Spill -> Ispill
        | Reload -> Ireload
        | Const_int n -> Iconst_int n
        | Const_float n -> Iconst_float n
        | Const_symbol n -> Iconst_symbol n
        | Stackoffset n -> Istackoffset n
        | Load (c, m) -> Iload (c, m)
        | Store (c, m, b) -> Istore (c, m, b)
        | Intop op -> Iintop op
        | Intop_imm (op, i) -> Iintop_imm (op, i)
        | Negf -> Inegf
        | Absf -> Iabsf
        | Addf -> Iaddf
        | Subf -> Isubf
        | Mulf -> Imulf
        | Divf -> Idivf
        | Floatofint -> Ifloatofint
        | Intoffloat -> Iintoffloat
        | Probe { name; handler_code_sym } ->
            Iprobe { name; handler_code_sym }
        | Probe_is_enabled { name } -> Iprobe_is_enabled { name }
        | Specific op -> Ispecific op
        | Name_for_debugger
            { ident; which_parameter; provenance; is_assignment } ->
            Iname_for_debugger
              { ident; which_parameter; provenance; is_assignment }
      in
      Lop op

let basic_to_linear (i : _ Cfg.instruction) ~next =
  let desc = from_basic i.desc in
  to_linear_instr ~like:i desc ~next

let mk_int_test ~lt ~eq ~gt : Cmm.integer_comparison =
  match (eq, lt, gt) with
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
   Cmm.float_comparison operators. For example, for float_test { lt = L0; eq
   = L0; gt = L0; uo = L1 } there is no Mach comparison for the branch to L1.

   We haven't seen a program that leads to it yet, but it is possible that
   future transformations will. So, for now these cases are fatal error. If
   we need to handle them, if needed, we can emit an unconditional jump that
   appears last, after all other conditional jumps. *)
type float_cond =
  | Must_be_last
  | Any of Cmm.float_comparison list

let mk_float_cond ~lt ~eq ~gt ~uo =
  match (eq, lt, gt, uo) with
  | true, false, false, false -> Any [CFeq]
  | false, true, false, false -> Any [CFlt]
  | false, false, true, false -> Any [CFgt]
  | true, true, false, false -> Any [CFle]
  | true, false, true, false -> Any [CFge]
  | false, true, true, true -> Any [CFneq]
  | true, false, true, true -> Any [CFnlt]
  | true, true, false, true -> Any [CFngt]
  | false, false, true, true -> Any [CFnle]
  | false, true, false, true -> Any [CFnge]
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

let linearize_terminator cfg (terminator : Cfg.terminator Cfg.instruction)
    ~(next : Linear_utils.labelled_insn) =
  (* CR-someday gyorsh: refactor, a lot of redundant code for different cases *)
  (* CR-someday gyorsh: for successor labels that are not fallthrough, order
     of branch instructions should depend on perf data and possibly the
     relative position of the target labels and the current block: whether
     the jumps are forward or back. This information can be obtained from the
     layout. For now, we are making an arbitrary choice. *)
  (* If one of the successors is a fallthrough label, do not emit a jump for
     it. Otherwise, the last jump is unconditional. *)
  let branch_or_fallthrough lbl =
    if Label.equal next.label lbl then [] else [L.Lbranch lbl]
  in
  let emit_bool (c1, l1) (c2, l2) =
    (* c1 must be the inverse of c2 *)
    match (Label.equal l1 next.label, Label.equal l2 next.label) with
    | true, true -> []
    | false, true -> [L.Lcondbranch (c1, l1)]
    | true, false -> [L.Lcondbranch (c2, l2)]
    | false, false ->
        if Label.equal l1 l2 then [L.Lbranch l1]
        else [L.Lcondbranch (c1, l1); L.Lbranch l2]
  in
  let desc_list =
    match terminator.desc with
    | Return -> [L.Lreturn]
    | Raise kind -> [L.Lraise kind]
    | Tailcall (Func (Indirect { label_after })) ->
        [L.Lop (Itailcall_ind { label_after })]
    | Tailcall (Func (Direct { func_symbol; label_after })) ->
        [L.Lop (Itailcall_imm { func = func_symbol; label_after })]
    | Tailcall (Self { label_after }) ->
        [L.Lop (Itailcall_imm { func = Cfg.fun_name cfg; label_after })]
    | Switch labels -> [L.Lswitch labels]
    | Never -> Misc.fatal_error "Cannot linearize terminator: Never"
    | Always label -> branch_or_fallthrough label
    | Parity_test { ifso; ifnot } ->
        emit_bool (Ieventest, ifso) (Ioddtest, ifnot)
    | Truth_test { ifso; ifnot } ->
        emit_bool (Itruetest, ifso) (Ifalsetest, ifnot)
    | Float_test { lt; eq; gt; uo } -> (
        let successor_labels =
          Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq
          |> Label.Set.add uo
        in
        match Label.Set.cardinal successor_labels with
        | 0 -> assert false
        | 1 -> branch_or_fallthrough (Label.Set.min_elt successor_labels)
        | 2 | 3 | 4 ->
            let must_be_last, any =
              Label.Set.fold
                (fun lbl (must_be_last, any) ->
                  let cond =
                    mk_float_cond ~lt:(Label.equal lt lbl)
                      ~eq:(Label.equal eq lbl) ~gt:(Label.equal gt lbl)
                      ~uo:(Label.equal uo lbl)
                  in
                  match cond with
                  | Any cl ->
                      let l = List.map (fun c -> (c, lbl)) cl in
                      (must_be_last, l @ any)
                  | Must_be_last -> (lbl :: must_be_last, any))
                successor_labels ([], [])
            in
            let last =
              match must_be_last with
              | [] ->
                  if Label.Set.mem next.label successor_labels then
                    next.label
                  else
                    (* arbitrary choice (also see CR above) *)
                    Label.Set.min_elt successor_labels
              | [lbl] ->
                  Printf.eprintf "One success label must be last: %d\n" lbl;
                  (* CR-someday gyorsh: fail for safety, until we see a case
                     that exhibits this behavior.. This behavior should not
                     be possible with the current cfg construction. *)
                  Misc.fatal_errorf
                    "Illegal branch: one successor label must be last %d" lbl
                    ()
              | _ ->
                  Misc.fatal_error
                    "Illegal branch: more than one successor label that \
                     must be last"
            in
            let branches =
              List.filter_map
                (fun (c, lbl) ->
                  if Label.equal lbl last then None
                  else Some (L.Lcondbranch (Ifloattest c, lbl)))
                any
            in
            branches @ branch_or_fallthrough last
        | _ -> assert false )
    | Int_test { lt; eq; gt; imm; is_signed } -> (
        let successor_labels =
          Label.Set.singleton lt |> Label.Set.add gt |> Label.Set.add eq
        in
        match Label.Set.cardinal successor_labels with
        | 0 -> assert false
        | 1 -> branch_or_fallthrough (Label.Set.min_elt successor_labels)
        | 2 | 3 ->
            (* If fallthrough label is a successor, do not emit a jump for
               it. Otherwise, the last jump could be unconditional. *)
            let last =
              if Label.Set.mem next.label successor_labels then next.label
              else
                (* arbitrary choice (see also CR above) *)
                Label.Set.min_elt successor_labels
            in
            let cond_successor_labels =
              Label.Set.remove last successor_labels
            in
            (* Lcondbranch3 is emitted as an unsigned comparison, see ocaml
               PR #8677 *)
            let can_emit_Lcondbranch3 =
              match (is_signed, imm) with
              | false, Some 1 -> true
              | false, Some _ | false, None | true, _ -> false
            in
            if
              Label.Set.cardinal cond_successor_labels = 2
              && can_emit_Lcondbranch3
            then
              (* generates one cmp instruction for all conditional jumps here *)
              let find l =
                if Label.equal next.label l then None else Some l
              in
              [L.Lcondbranch3 (find lt, find eq, find gt)]
            else
              let init = branch_or_fallthrough last in
              Label.Set.fold
                (fun lbl acc ->
                  let cond =
                    mk_int_test ~lt:(Label.equal lt lbl)
                      ~eq:(Label.equal eq lbl) ~gt:(Label.equal gt lbl)
                  in
                  let comp =
                    match is_signed with
                    | true -> Mach.Isigned cond
                    | false -> Mach.Iunsigned cond
                  in
                  let test =
                    match imm with
                    | None -> Mach.Iinttest comp
                    | Some n -> Iinttest_imm (comp, n)
                  in
                  L.Lcondbranch (test, lbl) :: acc)
                cond_successor_labels init
        | _ -> assert false )
  in
  List.fold_left
    (fun next desc -> to_linear_instr ~like:terminator desc ~next)
    next.insn (List.rev desc_list)

let need_starting_label (cfg_with_layout : CL.t) (block : Cfg.basic_block)
    ~(prev_block : Cfg.basic_block) =
  if block.is_trap_handler then true
  else
    match Label.Set.elements block.predecessors with
    | [] | _ :: _ :: _ -> true
    | [pred] when not (Label.equal pred prev_block.start) -> true
    | [_] -> (
        (* This block has a single predecessor which appears in the layout
           immediately prior to this block. *)
        (* No need for the label, unless the predecessor's terminator is
           [Switch] when the label is needed for the jump table. *)
        (* CR-someday gyorsh: is this correct with label_after for calls? *)
        match prev_block.terminator.desc with
        | Switch _ -> true
        | Never -> Misc.fatal_error "Cannot linearize terminator: Never"
        | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
          ->
            (* If the label came from the original [Linear] code, preserve it
               for checking that the conversion from [Linear] to [Cfg] and
               back is the identity; and for various assertions in reorder. *)
            let new_labels = CL.new_labels cfg_with_layout in
            CL.preserve_orig_labels cfg_with_layout
            && not (Label.Set.mem block.start new_labels)
        | Return | Raise _ | Tailcall _ -> assert false )

let adjust_trap_depth body (block : Cfg.basic_block)
    ~(prev_block : Cfg.basic_block) =
  let block_trap_depth = block.trap_depth in
  let prev_trap_depth = prev_block.terminator.trap_depth in
  if block_trap_depth = prev_trap_depth then body
  else
    let delta_traps = block_trap_depth - prev_trap_depth in
    to_linear_instr (Ladjust_trap_depth { delta_traps }) ~next:body

(* CR-someday gyorsh: handle duplicate labels in new layout: print the same
   block more than once. *)
let run cfg_with_layout =
  let cfg = CL.cfg cfg_with_layout in
  let layout = Array.of_list (CL.layout cfg_with_layout) in
  let len = Array.length layout in
  let next = ref Linear_utils.labelled_insn_end in
  for i = len - 1 downto 0 do
    let label = layout.(i) in
    if not (Label.Tbl.mem cfg.blocks label) then
      Misc.fatal_errorf "Unknown block labelled %d\n" label;
    let block = Label.Tbl.find cfg.blocks label in
    assert (Label.equal label block.start);
    let body =
      let terminator =
        linearize_terminator cfg block.terminator ~next:!next
      in
      List.fold_left
        (fun next i -> basic_to_linear i ~next)
        terminator (List.rev block.body)
    in
    let insn =
      if i = 0 then body (* Entry block of the function. Don't add label. *)
      else
        let body =
          if block.is_trap_handler then to_linear_instr Lentertrap ~next:body
          else body
        in
        let prev = layout.(i - 1) in
        let prev_block = Label.Tbl.find cfg.blocks prev in
        let body =
          if not (need_starting_label cfg_with_layout block ~prev_block) then
            body
          else to_linear_instr (Llabel block.start) ~next:body
        in
        adjust_trap_depth body block ~prev_block
    in
    next := { label; insn }
  done;
  !next.insn

(** debug print block as assembly *)
let print_assembly (blocks : Cfg.basic_block list) =
  (* create a fake cfg just for printing these blocks *)
  let layout = List.map (fun (b : Cfg.basic_block) -> b.start) blocks in
  let fun_name = "_fun_start_" in
  let fun_tailrec_entry_point_label = 0 in
  let cfg = Cfg.create ~fun_name ~fun_tailrec_entry_point_label in
  List.iter
    (fun (block : Cfg.basic_block) ->
      Label.Tbl.add cfg.blocks block.start block)
    blocks;
  let cl =
    Cfg_with_layout.create cfg ~layout ~new_labels:Label.Set.empty
      ~preserve_orig_labels:true
  in
  let fun_body = run cl in
  let fundecl =
    { Linear.fun_name;
      fun_body;
      fun_fast = false;
      fun_dbg = Debuginfo.none;
      fun_spacetime_shape = None;
      fun_num_stack_slots = Array.make Proc.num_register_classes 0;
      fun_frame_required = false;
      fun_prologue_required = false;
      fun_contains_calls = false;
      fun_tailrec_entry_point_label
    }
  in
  X86_proc.reset_asm_code ();
  Emit.fundecl fundecl;
  X86_proc.generate_code
    (Some (X86_gas.generate_asm !Emitaux.output_channel))
