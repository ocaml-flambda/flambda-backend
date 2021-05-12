[@@@ocaml.warning "+a-30-40-41-42"]

module C = Cfg
module L = Linear
module T = Trap_stack.Make (Label)

type t =
  { cfg : Cfg.t;
    mutable layout : Label.t list;
    mutable new_labels : Label.Set.t;
        (** Labels added by cfg construction, except entry. Used for testing
            of the mapping back to Linear IR. *)
    mutable trap_handlers : Label.Set.t;
        (** All traps pushed in this function. Used to check that all trap
            handler blocks (i.e., blocks that start with [Lentertrap] in
            Linear), have is_trap_handler set, which is needed to convert
            them back to Linear (and restore [Lentertrap]). *)
    trap_stacks : T.t Label.Tbl.t;
        (** Maps labels of blocks to trap handler stacks at the beginning of
            the block. [cfg.block.trap_depth] can be derived from the
            corresponding trap_stack, except when the block is dead and the
            trap stack is not known, represented by None. *)
    (* CR-someday gyorsh: The need for this is because we need to record
       trap_stack for a block that hasn't been created yet. If we change
       create_empty_block to get_or_create, then we don't need this hashtbl
       and will be able to store the trap_stacks directly within their nodes,
       but it is a major restructuring of create_blocks, which won't be
       needed when we split blocks to have only one stack each. *)
    exns : T.t list Label.Tbl.t;
        (** Maps labels to trap stacks at each raise point in the
            corresponding block. *)
    (* CR-someday gyorsh: this won't be needed after block splitting, as it
       will be uniquely determined by the top of the trap stack. *)
    interproc_handler : Label.t;
        (** A fake label that represents the top of the trap stack on entry
            to this function. *)
    mutable unresolved_traps_to_pop : T.t list
        (** Collect trap stacks from [exns] that need to be propagated to the
            exns successors of a block, but cannot be propagated yet, because
            the top of these trap stacks is unresolved (i.e., the exns
            successor block is not known). These trap stacks will become
            resolved unless they are unreachable. *)
  }

let create cfg =
  { cfg;
    layout = [];
    new_labels = Label.Set.empty;
    trap_handlers = Label.Set.empty;
    trap_stacks = Label.Tbl.create 31;
    exns = Label.Tbl.create 31;
    interproc_handler = -1;
    unresolved_traps_to_pop = []
  }

(* CR-someday gyorsh: implement CFG traversal *)

(* CR-someday gyorsh: abstraction of cfg updates that transparently and
   efficiently keeps predecessors and successors in sync. For example, change
   successors relation should automatically updates the predecessors relation
   without recomputing them from scratch. *)

(* CR-someday gyorsh: Optimize terminators. Implement switch as jump table or
   as a sequence of branches depending on perf counters and layout. It should
   be implemented as a separate transformation on the cfg, that needs to be
   informated by the layout, but it should not be done while emitting linear.
   This way we can keep to_linear as simple as possible to ensure basic
   invariants are preserved, while other optimizations can be turned on and
   off. *)

let entry_id = 1

let last_linear_id = ref entry_id

let get_new_linear_id () =
  let id = !last_linear_id in
  last_linear_id := id + 1;
  id

let create_instruction desc ~trap_depth (i : Linear.instruction) :
    _ C.instruction =
  { desc;
    arg = i.arg;
    res = i.res;
    dbg = i.dbg;
    live = i.live;
    trap_depth;
    id = get_new_linear_id ()
  }

let record_traps t label traps =
  match Label.Tbl.find_opt t.trap_stacks label with
  | None -> Label.Tbl.add t.trap_stacks label traps
  | Some existing_traps ->
      if !C.verbose then
        T.print_pair
          (Printf.sprintf "Unify at %d" label)
          traps existing_traps;
      T.unify traps existing_traps;
      if !C.verbose then (
        Printf.printf "after: ";
        T.print existing_traps )

let resolve_traps_to_pop t ls =
  let rec loop prev =
    let progress = ref false in
    let curr =
      List.fold_left
        (fun acc traps ->
          match T.top_exn traps with
          | None ->
              Misc.fatal_errorf "resolve_traps_to_pop: Illegal trap stack"
          | Some top ->
              progress := true;
              record_traps t top (T.pop traps);
              acc
          | exception T.Unresolved -> traps :: acc)
        [] prev
    in
    if !progress then loop curr else curr
  in
  loop ls

let record_exn t (block : C.basic_block) traps =
  block.can_raise <- true;
  match Label.Tbl.find_opt t.exns block.start with
  | None -> Label.Tbl.add t.exns block.start [traps]
  | Some ls -> Label.Tbl.replace t.exns block.start (traps :: ls)

let create_empty_block t start ~trap_depth ~traps =
  let terminator : C.terminator C.instruction =
    { desc = C.Never (* placeholder for terminator, to be replaced *);
      arg = [||];
      res = [||];
      dbg = Debuginfo.none;
      live = Reg.Set.empty;
      trap_depth;
      id = get_new_linear_id ()
    }
  in
  let block : C.basic_block =
    { start;
      body = [];
      terminator;
      exns = Label.Set.empty;
      predecessors = Label.Set.empty;
      trap_depth;
      is_trap_handler = false;
      can_raise = false;
      can_raise_interproc = false;
      dead = false
    }
  in
  record_traps t start traps;
  if C.mem_block t.cfg start then
    Misc.fatal_errorf "A block with starting label %d is already registered"
      start;
  t.layout <- start :: t.layout;
  block

let register_block t (block : C.basic_block) traps =
  if Label.Tbl.mem t.cfg.blocks block.start then
    Misc.fatal_errorf "A block with starting label %d is already registered"
      block.start;
  if !C.verbose then Printf.printf "registering block %d\n" block.start;
  (* Body is constructed in reverse, fix it now: *)
  block.body <- List.rev block.body;
  (* Update trap stacks of normal successor blocks. *)
  Label.Set.iter
    (fun label -> record_traps t label traps)
    (C.successor_labels t.cfg ~normal:true ~exn:false block);
  (* Update trap stacks of exns successors. For each trap stack [s] in
     [t.exns]: The handler is the block identified by the label on the top of
     [s]. The trap stack of the handler must be the same as (pop [s]). The
     difficulty is that at this point the top of [s] might be unknown. Such
     trap stacks are recorded and resolved at the end of the pass. *)
  ( match Label.Tbl.find_opt t.exns block.start with
  | None -> ()
  | Some ls ->
      t.unresolved_traps_to_pop <-
        resolve_traps_to_pop t ls @ t.unresolved_traps_to_pop );
  Label.Tbl.add t.cfg.blocks block.start block

let can_raise_terminator (i : C.terminator) =
  match i with
  | Raise _ | Tailcall (Func _) -> true
  | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
  | Int_test _ | Switch _ | Return
  | Tailcall (Self _) ->
      false

let check_traps t label (block : C.basic_block) =
  match Label.Tbl.find_opt t.trap_stacks label with
  | None -> ()
  | Some trap_stack_at_start -> (
      if !C.verbose then (
        Printf.printf "%s: check_trap at %d: " t.cfg.fun_name label;
        T.print trap_stack_at_start );
      match T.to_list_exn trap_stack_at_start with
      | trap_labels ->
          if List.compare_length_with trap_labels block.trap_depth <> 0 then
            Misc.fatal_errorf
              "Malformed linear IR: mismatch trap_depth=%d, but \
               trap_stack_at_start length=%d"
              block.trap_depth (List.length trap_labels)
      | exception T.Unresolved ->
          (* At the end of the cfg construction, some blocks may have
             unresolved trap stacks. All these blocks must be dead, i.e.,
             unreachable from entry of the function. This check can be done
             as a separate pass in dead block elimination. Here we need to
             keep track of blocks with unresolved trap stacks, because
             [t.trap_stacks] is not available after cfg construction. This is
             done by marking them [dead] and checking that flag in
             [eliminate_dead_blocks]. *)
          block.dead <- true;
          if !C.verbose then
            Printf.printf
              "unknown trap stack at label %d, the block must be dead, or \
               there is a bug in trap stacks."
              label )

let register_exns t label (block : C.basic_block) =
  (* All exns in this block must be based off the trap_stack_at_start, which
     must have been successfully resolved by T.unify, i.e., does not contain
     any Unknown frames or labels. *)
  match Label.Tbl.find_opt t.exns label with
  | None -> ()
  | Some exns ->
      let f acc trap_stack =
        match T.top_exn trap_stack with
        | None ->
            Misc.fatal_errorf "register_exns: empty trap stack for %d" label
        | Some l ->
            if Label.equal l t.interproc_handler then (
              block.can_raise_interproc <- true;
              acc )
            else Label.Set.add l acc
        | exception T.Unresolved ->
            (* must be dead block or flow from exception handler only *)
            assert block.dead;
            if !C.verbose then
              Printf.printf
                "unknown trap stack in exns of block %d: the block must be \
                 dead, or there is a bug in trap stacks."
                label;
            acc
      in
      block.exns <- List.fold_left f Label.Set.empty exns;
      if !C.verbose then (
        Printf.printf "%s: %d exn stacks at %d: " t.cfg.fun_name
          (List.length exns) label;
        List.iter T.print exns;
        Printf.printf "%s: %d exns at %d: " t.cfg.fun_name
          (Label.Set.cardinal block.exns)
          label;
        Label.Set.iter (Printf.printf "%d ") block.exns;
        Printf.printf "\n" )

let check_and_register_traps t =
  (* check that all blocks referred to in pushtraps are marked as
     trap_handlers *)
  Label.Set.iter
    (fun label ->
      let trap_block = C.get_block_exn t.cfg label in
      if not trap_block.is_trap_handler then
        Misc.fatal_errorf
          "Label %d used as a handler in Lpushtrap, but the block at %d \
           does not start\n\
          \                           with an Lentertrap instruction." label
          label)
    t.trap_handlers;

  (* propagate remaining trap stacks to handlers *)
  t.unresolved_traps_to_pop <-
    resolve_traps_to_pop t t.unresolved_traps_to_pop;
  if List.compare_length_with t.unresolved_traps_to_pop 0 > 0 then (
    if !C.verbose then
      (* not a fatal error because of dead blocks *)
      Printf.printf "%d" (List.length t.unresolved_traps_to_pop);
    Misc.fatal_error "Unresolved traps at the end of cfg construction" );

  (* check that trap stacks at the start of all blocks are resolved *)
  C.iter_blocks t.cfg ~f:(check_traps t);

  (* compute block.exns successors using t.exns. *)
  C.iter_blocks t.cfg ~f:(register_exns t);

  (* after all exn successors are computed, check that if a block can_raise,
     then it has a registered exn successor or interproc exn. *)
  let f _ (block : C.basic_block) =
    let n = Label.Set.cardinal block.exns in
    assert ((not block.can_raise_interproc) || block.can_raise);
    assert ((not block.can_raise) || n > 0 || block.can_raise_interproc)
  in
  C.iter_blocks t.cfg ~f

let register_predecessors_for_all_blocks t =
  Label.Tbl.iter
    (fun label block ->
      let targets = C.successor_labels ~normal:true ~exn:true t.cfg block in
      Label.Set.iter
        (fun target ->
          let target_block = Label.Tbl.find t.cfg.blocks target in
          target_block.predecessors <-
            Label.Set.add label target_block.predecessors)
        targets)
    t.cfg.blocks

let get_or_make_label t (insn : Linear.instruction) :
    Linear_utils.labelled_insn =
  match insn.desc with
  | Llabel label -> { label; insn }
  | Lend -> Misc.fatal_error "Unexpected end of function instead of label"
  | Lprologue | Lop _ | Lreloadretaddr | Lreturn | Lbranch _ | Lcondbranch _
  | Lcondbranch3 _ | Lswitch _ | Lentertrap | Ladjust_trap_depth _
  | Lpushtrap _ | Lpoptrap | Lraise _ ->
      let label = Cmm.new_label () in
      t.new_labels <- Label.Set.add label t.new_labels;
      let insn = Linear.instr_cons (Llabel label) [||] [||] insn in
      { label; insn }

let of_cmm_float_test ~lbl ~inv (cmp : Cmm.float_comparison) : C.float_test =
  match cmp with
  | CFeq -> { eq = lbl; lt = inv; gt = inv; uo = inv }
  | CFlt -> { eq = inv; lt = lbl; gt = inv; uo = inv }
  | CFgt -> { eq = inv; lt = inv; gt = lbl; uo = inv }
  | CFle -> { eq = lbl; lt = lbl; gt = inv; uo = inv }
  | CFge -> { eq = lbl; lt = inv; gt = lbl; uo = inv }
  | CFneq -> { eq = inv; lt = lbl; gt = lbl; uo = lbl }
  | CFnlt -> { eq = lbl; lt = inv; gt = lbl; uo = lbl }
  | CFngt -> { eq = lbl; lt = lbl; gt = inv; uo = lbl }
  | CFnle -> { eq = inv; lt = inv; gt = lbl; uo = lbl }
  | CFnge -> { eq = inv; lt = lbl; gt = inv; uo = lbl }

let of_cmm_int_test ~lbl ~inv ~is_signed ~imm (cmp : Cmm.integer_comparison)
    : C.int_test =
  match cmp with
  | Ceq -> { eq = lbl; lt = inv; gt = inv; is_signed; imm }
  | Clt -> { eq = inv; lt = lbl; gt = inv; is_signed; imm }
  | Cgt -> { eq = inv; lt = inv; gt = lbl; is_signed; imm }
  | Cne -> { eq = inv; lt = lbl; gt = lbl; is_signed; imm }
  | Cle -> { eq = lbl; lt = lbl; gt = inv; is_signed; imm }
  | Cge -> { eq = lbl; lt = inv; gt = lbl; is_signed; imm }

let mk_int_test ~lbl ~inv ~imm (cmp : Mach.integer_comparison) : C.int_test =
  match cmp with
  | Isigned cmp -> of_cmm_int_test ~lbl ~inv cmp ~is_signed:true ~imm
  | Iunsigned cmp -> of_cmm_int_test ~lbl ~inv cmp ~is_signed:false ~imm

let block_is_registered t (block : C.basic_block) =
  Label.Tbl.mem t.cfg.blocks block.start

let add_terminator t (block : C.basic_block) (i : L.instruction)
    (desc : C.terminator) ~trap_depth ~traps =
  (* All terminators are followed by a label, except branches we created for
     fallthroughs in Linear. *)
  ( match desc with
  | Never -> Misc.fatal_error "Cannot add terminator: Never"
  | Always _ | Parity_test _ | Truth_test _ | Float_test _ | Int_test _ -> ()
  | Switch _ | Return | Raise _ | Tailcall _ ->
      if not (Linear_utils.defines_label i.next) then
        Misc.fatal_errorf "Linear instruction not followed by label:@ %a"
          Printlinear.instr
          ({ i with next = Linear.end_instr } : L.instruction));
  block.terminator <- create_instruction desc ~trap_depth i;
  if can_raise_terminator desc then record_exn t block traps;
  register_block t block traps

let to_basic (mop : Mach.operation) : C.basic =
  match mop with
  | Icall_ind { label_after } -> Call (F (Indirect { label_after }))
  | Icall_imm { func; label_after } ->
      Call (F (Direct { func_symbol = func; label_after }))
  | Iextcall { func; alloc; label_after } ->
      Call (P (External { func_symbol = func; alloc; label_after }))
  | Iintop (Icheckbound { label_after_error; spacetime_index }) ->
      Call
        (P
           (Checkbound
              { immediate = None; label_after_error; spacetime_index }))
  | Iintop
      ( ( Iadd | Isub | Imul | Imulh | Idiv | Imod | Iand | Ior | Ixor | Ilsl
        | Ilsr | Iasr | Icomp _ ) as op ) ->
      Op (Intop op)
  | Iintop_imm (Icheckbound { label_after_error; spacetime_index }, i) ->
      Call
        (P
           (Checkbound
              { immediate = Some i; label_after_error; spacetime_index }))
  | Iintop_imm
      ( ( ( Iadd | Isub | Imul | Imulh | Idiv | Imod | Iand | Ior | Ixor
          | Ilsl | Ilsr | Iasr | Icomp _ ) as op ),
        i ) ->
      Op (Intop_imm (op, i))
  | Ialloc { bytes; label_after_call_gc; dbginfo; spacetime_index } ->
      Call
        (P (Alloc { bytes; label_after_call_gc; dbginfo; spacetime_index }))
  | Iprobe { name; handler_code_sym } ->
      Op (Probe { name; handler_code_sym })
  | Iprobe_is_enabled { name } -> Op (Probe_is_enabled { name })
  | Istackoffset i -> Op (Stackoffset i)
  | Iload (c, a) -> Op (Load (c, a))
  | Istore (c, a, b) -> Op (Store (c, a, b))
  | Imove -> Op Move
  | Ispill -> Op Spill
  | Ireload -> Op Reload
  | Iconst_int n -> Op (Const_int n)
  | Iconst_float n -> Op (Const_float n)
  | Iconst_symbol n -> Op (Const_symbol n)
  | Inegf -> Op Negf
  | Iabsf -> Op Absf
  | Iaddf -> Op Addf
  | Isubf -> Op Subf
  | Imulf -> Op Mulf
  | Idivf -> Op Divf
  | Ifloatofint -> Op Floatofint
  | Iintoffloat -> Op Intoffloat
  | Ispecific op -> Op (Specific op)
  | Iname_for_debugger { ident; which_parameter; provenance; is_assignment }
    ->
      Op
        (Name_for_debugger
           { ident; which_parameter; provenance; is_assignment })
  | Itailcall_ind _ | Itailcall_imm _ -> assert false

(** [traps] represents the trap stack, with head being the top. [trap_depths]
    is the depth of the trap stack. *)
let rec create_blocks t (i : L.instruction) (block : C.basic_block)
    ~trap_depth ~traps =
  (* [traps] is constructed incrementally, because Ladjust_trap does not give
     enough information to compute it upfront, but trap_depth is directly
     computed. *)
  match i.desc with
  | Lend ->
      if not (block_is_registered t block) then
        Misc.fatal_errorf "End of function without terminator for block %d"
          block.start
  | Llabel start ->
      (* Not all labels need to start a new block. Keep the translation from
         linear to cfg simple, and optimize (remove/merge blocks) in a
         separate pass, because: - correctness of linear to cfg and back is
         easier to reason about. - the optimizations are easier to implement
         and reason about on the CFG. - some optimization opportunities
         cannot be identified in a single pass on the Linear IR, during cfg
         construction. - optimizations can be enabled by the client of the
         library whenever needed *)
      if !C.verbose then
        Printf.printf "Llabel start=%d, block.start=%d\n" start block.start;
      (* Labels always cause a new block to be created. If the block prior to
         the label falls through, an explicit successor edge is added. *)
      if not (block_is_registered t block) then (
        let fallthrough : C.terminator = Always start in
        block.terminator <- create_instruction fallthrough i ~trap_depth;
        register_block t block traps );
      (* CR-someday gyorsh: check for multiple consecutive labels *)
      let new_block = create_empty_block t start ~trap_depth ~traps in
      create_blocks t i.next new_block ~trap_depth ~traps
  | Lreturn ->
      if trap_depth <> 1 then
        Misc.fatal_errorf "Trap depth is %d, but it must be 1 at Lreturn"
          trap_depth;
      add_terminator t block i Return ~trap_depth ~traps;
      create_blocks t i.next block ~trap_depth ~traps
  | Lraise kind ->
      (* CR-someday gyorsh: Why does the compiler not generate adjust after
         raise? raise pops the trap handler stack and then the next block may
         have a different try depth. Also, why do we not need to update
         trap_depths and traps here like for pop?

         mshinwell: I don't think there's anything special about a raise for
         Linearize; it may still add a trap adjustment. Think about this some
         more...

         gyorsh: it is now handled in register_block called from
         add_terminator. *)
      add_terminator t block i (Raise kind) ~trap_depth ~traps;
      create_blocks t i.next block ~trap_depth ~traps
  | Lbranch lbl ->
      if !C.verbose then Printf.printf "Lbranch %d\n" lbl;
      add_terminator t block i (Always lbl) ~trap_depth ~traps;
      create_blocks t i.next block ~trap_depth ~traps
  | Lcondbranch (cond, lbl) ->
      (* This representation does not preserve the order of successors. *)
      let fallthrough = get_or_make_label t i.next in
      let inv = fallthrough.label in
      let desc : C.terminator =
        match (cond : Mach.test) with
        | Ieventest -> Parity_test { ifso = lbl; ifnot = inv }
        | Ioddtest -> Parity_test { ifso = inv; ifnot = lbl }
        | Itruetest -> Truth_test { ifso = lbl; ifnot = inv }
        | Ifalsetest -> Truth_test { ifso = inv; ifnot = lbl }
        | Ifloattest cmp -> Float_test (of_cmm_float_test cmp ~lbl ~inv)
        | Iinttest cmp -> Int_test (mk_int_test cmp ~lbl ~inv ~imm:None)
        | Iinttest_imm (cmp, n) ->
            Int_test (mk_int_test cmp ~lbl ~inv ~imm:(Some n))
      in
      add_terminator t block i desc ~trap_depth ~traps;
      create_blocks t fallthrough.insn block ~trap_depth ~traps
  | Lcondbranch3 (lbl0, lbl1, lbl2) ->
      let fallthrough = get_or_make_label t i.next in
      let get_dest lbl = Option.value lbl ~default:fallthrough.label in
      let it : C.int_test =
        { imm = Some 1;
          is_signed = false;
          lt = get_dest lbl0;
          eq = get_dest lbl1;
          gt = get_dest lbl2
        }
      in
      add_terminator t block i (Int_test it) ~trap_depth ~traps;
      create_blocks t fallthrough.insn block ~trap_depth ~traps
  | Lswitch labels ->
      (* CR-someday gyorsh: get rid of switches entirely and re-generate them
         based on optimization and perf data? *)
      add_terminator t block i (Switch labels) ~trap_depth ~traps;
      create_blocks t i.next block ~trap_depth ~traps
  | Ladjust_trap_depth { delta_traps } ->
      (* We do not emit any executable code for this insn; it only moves the
         virtual stack pointer in the emitter. We do not have a corresponding
         insn in [Cfg] because the required adjustment can change when blocks
         are reordered. Instead we regenerate the instructions when
         converting back to linear. We use [delta_traps] only to compute
         [trap_depth]s of other instructions. *)
      let trap_depth = trap_depth + delta_traps in
      if trap_depth < 0 then
        Misc.fatal_errorf
          "Ladjust_trap_depth %d moves the trap depth below zero: %d"
          delta_traps trap_depth;
      let traps = T.unknown () in
      create_blocks t i.next block ~trap_depth ~traps
  | Lpushtrap { lbl_handler } ->
      t.trap_handlers <- Label.Set.add lbl_handler t.trap_handlers;
      record_traps t lbl_handler traps;
      let desc = C.Pushtrap { lbl_handler } in
      block.body <- create_instruction desc ~trap_depth i :: block.body;
      let trap_depth = trap_depth + 1 in
      let traps = T.push traps lbl_handler in
      create_blocks t i.next block ~trap_depth ~traps
  | Lpoptrap ->
      let desc = C.Poptrap in
      block.body <- create_instruction desc ~trap_depth i :: block.body;
      let trap_depth = trap_depth - 1 in
      if trap_depth < 0 then
        Misc.fatal_error "Lpoptrap moves the trap depth below zero";
      if !C.verbose then (
        Printf.printf "before pop: ";
        T.print traps );
      let traps = T.pop traps in
      if !C.verbose then (
        Printf.printf "after pop: ";
        T.print traps );
      create_blocks t i.next block ~trap_depth ~traps
  | Lentertrap ->
      (* Must be the first instruction in the block. *)
      assert (List.compare_length_with block.body 0 = 0);
      block.is_trap_handler <- true;
      create_blocks t i.next block ~trap_depth ~traps
  | Lprologue ->
      let desc = C.Prologue in
      block.body <- create_instruction desc i ~trap_depth :: block.body;
      create_blocks t i.next block ~trap_depth ~traps
  | Lreloadretaddr ->
      let desc = C.Reloadretaddr in
      block.body <- create_instruction desc i ~trap_depth :: block.body;
      create_blocks t i.next block ~trap_depth ~traps
  | Lop mop -> (
      match mop with
      | Itailcall_ind { label_after } ->
          let desc = C.Tailcall (Func (Indirect { label_after })) in
          add_terminator t block i desc ~trap_depth ~traps;
          create_blocks t i.next block ~trap_depth ~traps
      | Itailcall_imm { func = func_symbol; label_after } ->
          let desc =
            if String.equal func_symbol (C.fun_name t.cfg) then
              C.Tailcall (Self { label_after })
            else C.Tailcall (Func (Direct { func_symbol; label_after }))
          in
          add_terminator t block i desc ~trap_depth ~traps;
          create_blocks t i.next block ~trap_depth ~traps
      | Imove | Ispill | Ireload | Inegf | Iabsf | Iaddf | Isubf | Imulf
      | Idivf | Ifloatofint | Iintoffloat | Iconst_int _ | Iconst_float _
      | Iconst_symbol _ | Icall_ind _ | Icall_imm _ | Iextcall _
      | Istackoffset _
      | Iload (_, _)
      | Istore (_, _, _)
      | Ialloc _ | Iintop _
      | Iintop_imm (_, _)
      | Iprobe _ | Iprobe_is_enabled _ | Ispecific _ | Iname_for_debugger _
        ->
          let desc = to_basic mop in
          block.body <- create_instruction desc i ~trap_depth :: block.body;
          if Mach.operation_can_raise mop then record_exn t block traps;
          create_blocks t i.next block ~trap_depth ~traps )

let run (f : Linear.fundecl) ~preserve_orig_labels =
  let t =
    let cfg =
      Cfg.create ~fun_name:f.fun_name
        ~fun_tailrec_entry_point_label:f.fun_tailrec_entry_point_label
    in
    create cfg
  in
  (* CR-someday gyorsh: label of the function entry must not conflict with
     existing labels. Relies on the invariant: Cmm.new_label() is int > 99.
     An alternative is to create a new type for label here, but it is less
     efficient because label is used as a key to Label.Tbl. mshinwell: I
     don't think a new type needs to cause any change in efficiency. A custom
     hashtable can be made with the appropriate hashing and equality
     functions. *)
  let traps = T.push (T.empty ()) t.interproc_handler in
  let trap_depth = 1 in
  let entry_block =
    create_empty_block t t.cfg.entry_label ~trap_depth ~traps
  in
  last_linear_id := entry_id;
  create_blocks t f.fun_body entry_block ~trap_depth ~traps;
  (* Register predecessors now rather than during cfg construction, because
     of forward jumps: the blocks do not exist when the jump that reference
     them is processed. *)
  check_and_register_traps t;
  register_predecessors_for_all_blocks t;
  (* Layout was constructed in reverse, fix it now: *)
  Cfg_with_layout.create t.cfg ~layout:(List.rev t.layout)
    ~preserve_orig_labels ~new_labels:t.new_labels
