(* CR-soon xclerc for xclerc: use the same warning set as flambda2. *)
[@@@ocaml.warning "+a-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list
open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

let debug = false

module State : sig
  type t

  val make :
    fun_name:string ->
    tailrec_label:Label.t ->
    contains_calls:bool ->
    Cfg.basic_block Label.Tbl.t ->
    t

  val get_fun_name : t -> string

  val get_tailrec_label : t -> Label.t

  val get_contains_calls : t -> bool

  val add_block : t -> label:Label.t -> block:Cfg.basic_block -> unit

  val get_layout : t -> Cfg_with_layout.layout

  val add_catch_handler : t -> handler_id:int -> Label.t

  val get_catch_handler : t -> handler_id:int -> Label.t

  val get_next_instruction_id : t -> int

  val add_exception_handler : t -> Label.t -> unit

  val get_exception_handlers : t -> Label.t list
end = struct
  type t =
    { fun_name : string;
      tailrec_label : Label.t;
      contains_calls : bool;
      blocks : Cfg.basic_block Label.Tbl.t;
      layout : Cfg_with_layout.layout;
      catch_handlers : Label.t Numbers.Int.Tbl.t;
      mutable next_instruction_id : int;
      mutable exception_handlers : Label.t list
    }

  let make ~fun_name ~tailrec_label ~contains_calls blocks =
    let layout = DLL.make_empty () in
    let catch_handlers = Numbers.Int.Tbl.create 31 in
    let next_instruction_id = 0 in
    let exception_handlers = [] in
    { fun_name;
      tailrec_label;
      contains_calls;
      blocks;
      layout;
      catch_handlers;
      next_instruction_id;
      exception_handlers
    }

  let get_fun_name t = t.fun_name

  let get_tailrec_label t = t.tailrec_label

  let get_contains_calls t = t.contains_calls

  let add_block t ~label ~block =
    if Label.Tbl.mem t.blocks label
    then
      Misc.fatal_errorf "Cfgize.State.add_block: duplicate block for label %d"
        label
    else (
      DLL.add_end t.layout label;
      Label.Tbl.replace t.blocks label block)

  let get_layout t = t.layout

  let add_catch_handler t ~handler_id =
    if Numbers.Int.Tbl.mem t.catch_handlers handler_id
    then
      Misc.fatal_errorf "Cfgize.State.add_catch_handler: duplicate handler %d"
        handler_id
    else
      let label = Cmm.new_label () in
      Numbers.Int.Tbl.replace t.catch_handlers handler_id label;
      label

  let get_catch_handler t ~handler_id =
    match Numbers.Int.Tbl.find_opt t.catch_handlers handler_id with
    | Some res -> res
    | None ->
      Misc.fatal_errorf "Cfgize.State.get_handler_label: unknown handler_id %d"
        handler_id

  let get_next_instruction_id t =
    let res = t.next_instruction_id in
    t.next_instruction_id <- succ res;
    res

  let add_exception_handler t lbl =
    t.exception_handlers <- lbl :: t.exception_handlers

  let get_exception_handlers t = t.exception_handlers
end

type basic_or_terminator =
  | Basic of Cfg.basic
  | Terminator of Cfg.terminator
  | With_next_label of (Label.t -> Cfg.terminator)

let basic_or_terminator_of_operation :
    State.t -> Mach.operation -> basic_or_terminator =
 fun state op ->
  match op with
  | Imove -> Basic (Op Move)
  | Ispill -> Basic (Op Spill)
  | Ireload -> Basic (Op Reload)
  | Iconst_int i -> Basic (Op (Const_int i))
  | Iconst_float32 f -> Basic (Op (Const_float32 f))
  | Iconst_float f -> Basic (Op (Const_float f))
  | Iconst_symbol s -> Basic (Op (Const_symbol s))
  | Iconst_vec128 bits -> Basic (Op (Const_vec128 bits))
  | Icall_ind ->
    With_next_label (fun label_after -> Call { op = Indirect; label_after })
  | Icall_imm { func } ->
    With_next_label (fun label_after -> Call { op = Direct func; label_after })
  | Itailcall_ind -> Terminator (Tailcall_func Indirect)
  | Itailcall_imm { func } ->
    Terminator
      (if String.equal (State.get_fun_name state) func.sym_name
      then Tailcall_self { destination = State.get_tailrec_label state }
      else Tailcall_func (Direct func))
  | Iextcall { func; ty_res; ty_args; alloc; returns; stack_ofs } ->
    let external_call =
      { Cfg.func_symbol = func; alloc; ty_res; ty_args; stack_ofs }
    in
    if returns
    then
      With_next_label
        (fun label_after -> Prim { op = External external_call; label_after })
    else Terminator (Call_no_return external_call)
  | Istackoffset ofs -> Basic (Op (Stackoffset ofs))
  | Iload { memory_chunk; addressing_mode; mutability; is_atomic } ->
    Basic
      (Op
         (Load
            { memory_chunk;
              addressing_mode;
              mutability = Mach.of_ast_mutable_flag mutability;
              is_atomic
            }))
  | Istore (mem, mode, assignment) -> Basic (Op (Store (mem, mode, assignment)))
  | Ialloc { bytes; dbginfo; mode } ->
    Basic (Op (Alloc { bytes; dbginfo; mode }))
  | Ipoll { return_label = None } -> Basic (Op Poll)
  | Ipoll { return_label = Some return_label } ->
    Misc.fatal_errorf "Cfgize.basic_or_terminator: unexpected Ipoll %d"
      return_label
  | Iintop
      (( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
       | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) as op) ->
    Basic (Op (Intop op))
  | Iintop_imm
      ( (( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
         | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) as op),
        imm ) ->
    Basic (Op (Intop_imm (op, imm)))
  | Iintop_atomic { op; size; addr } ->
    Basic (Op (Intop_atomic { op; size; addr }))
  | Icsel tst -> Basic (Op (Csel tst))
  | Ifloatop (w, Icompf comp) -> Basic (Op (Floatop (w, Icompf comp)))
  | Ifloatop (w, ((Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf) as op)) ->
    Basic (Op (Floatop (w, op)))
  | Ireinterpret_cast cast -> Basic (Op (Reinterpret_cast cast))
  | Istatic_cast cast -> Basic (Op (Static_cast cast))
  | Ispecific op ->
    if Arch.operation_can_raise op
    then
      With_next_label
        (fun label_after -> Specific_can_raise { op; label_after })
    else Basic (Op (Specific op))
  | Iopaque -> Basic (Op Opaque)
  | Iname_for_debugger
      { ident; which_parameter; provenance; is_assignment; regs } ->
    Basic
      (Op
         (Name_for_debugger
            { ident; which_parameter; provenance; is_assignment; regs }))
  | Iprobe { name; handler_code_sym; enabled_at_init } ->
    With_next_label
      (fun label_after ->
        Prim
          { op = Probe { name; handler_code_sym; enabled_at_init };
            label_after
          })
  | Iprobe_is_enabled { name } -> Basic (Op (Probe_is_enabled { name }))
  | Ibeginregion -> Basic (Op Begin_region)
  | Iendregion -> Basic (Op End_region)
  | Idls_get -> Basic (Op Dls_get)

let float_test_of_float_comparison :
    Cmm.float_width ->
    Cmm.float_comparison ->
    label_false:Label.t ->
    label_true:Label.t ->
    Cfg.float_test =
 fun width comparison ~label_false ~label_true ->
  let lt, eq, gt, uo =
    match comparison with
    | CFeq -> label_false, label_true, label_false, label_false
    | CFneq -> label_true, label_false, label_true, label_true
    | CFlt -> label_true, label_false, label_false, label_false
    | CFnlt -> label_false, label_true, label_true, label_true
    | CFgt -> label_false, label_false, label_true, label_false
    | CFngt -> label_true, label_true, label_false, label_true
    | CFle -> label_true, label_true, label_false, label_false
    | CFnle -> label_false, label_false, label_true, label_true
    | CFge -> label_false, label_true, label_true, label_false
    | CFnge -> label_true, label_false, label_false, label_true
  in
  { width; lt; eq; gt; uo }

let int_test_of_integer_comparison :
    Cmm.integer_comparison ->
    signed:bool ->
    immediate:int option ->
    label_false:Label.t ->
    label_true:Label.t ->
    Cfg.int_test =
 fun comparison ~signed:is_signed ~immediate:imm ~label_false ~label_true ->
  let lt, eq, gt =
    match comparison with
    | Ceq -> label_false, label_true, label_false
    | Cne -> label_true, label_false, label_true
    | Clt -> label_true, label_false, label_false
    | Cgt -> label_false, label_false, label_true
    | Cle -> label_true, label_true, label_false
    | Cge -> label_false, label_true, label_true
  in
  { lt; eq; gt; is_signed; imm }

let terminator_of_test :
    Mach.test -> label_false:Label.t -> label_true:Label.t -> Cfg.terminator =
 fun test ~label_false ~label_true ->
  let int_test comparison immediate =
    let signed, comparison =
      match comparison with
      | Mach.Isigned comparison -> true, comparison
      | Mach.Iunsigned comparison -> false, comparison
    in
    int_test_of_integer_comparison comparison ~signed ~immediate ~label_false
      ~label_true
  in
  match test with
  | Itruetest -> Truth_test { ifso = label_true; ifnot = label_false }
  | Ifalsetest -> Truth_test { ifso = label_false; ifnot = label_true }
  | Iinttest comparison -> Int_test (int_test comparison None)
  | Iinttest_imm (comparison, value) ->
    Int_test (int_test comparison (Some value))
  | Ifloattest (w, comparison) ->
    Float_test
      (float_test_of_float_comparison w comparison ~label_false ~label_true)
  | Ioddtest -> Parity_test { ifso = label_false; ifnot = label_true }
  | Ieventest -> Parity_test { ifso = label_true; ifnot = label_false }

let invalid_stack_offset = -1

let make_instruction : type a. State.t -> desc:a -> a Cfg.instruction =
 fun state ~desc ->
  let arg = [||] in
  let res = [||] in
  let dbg = Debuginfo.none in
  let live = Reg.Set.empty in
  let stack_offset = invalid_stack_offset in
  let id = State.get_next_instruction_id state in
  let fdo = Fdo_info.none in
  { desc;
    arg;
    res;
    dbg;
    live;
    stack_offset;
    id;
    fdo;
    irc_work_list = Unknown_list;
    ls_order = -1;
    available_before = None;
    available_across = None
  }

let copy_instruction :
    type a. State.t -> Mach.instruction -> desc:a -> a Cfg.instruction =
 fun state instr ~desc ->
  let { Mach.arg;
        res;
        dbg;
        live;
        desc = _;
        next = _;
        available_before;
        available_across
      } =
    instr
  in
  let stack_offset = invalid_stack_offset in
  let id = State.get_next_instruction_id state in
  let fdo = Fdo_info.none in
  { desc;
    arg;
    res;
    dbg;
    live;
    stack_offset;
    id;
    fdo;
    irc_work_list = Unknown_list;
    ls_order = -1;
    available_before = Some available_before;
    available_across
  }

let copy_instruction_no_reg :
    type a. State.t -> Mach.instruction -> desc:a -> a Cfg.instruction =
 fun state instr ~desc ->
  let { Mach.arg = _;
        res = _;
        dbg;
        live;
        desc = _;
        next = _;
        available_before;
        available_across
      } =
    instr
  in
  let arg = [||] in
  let res = [||] in
  let stack_offset = invalid_stack_offset in
  let id = State.get_next_instruction_id state in
  let fdo = Fdo_info.none in
  { desc;
    arg;
    res;
    dbg;
    live;
    stack_offset;
    id;
    fdo;
    irc_work_list = Unknown_list;
    ls_order = -1;
    available_before = Some available_before;
    available_across
  }

type terminator_info =
  | Terminator of Cfg.terminator Cfg.instruction
  | With_next_label of (Label.t -> Cfg.terminator Cfg.instruction)
  | Complex_terminator

type block_info =
  { instrs : Cfg.basic_instruction_list;
    last : Mach.instruction;
    terminator : terminator_info
  }

(* [extract_block_info state first] returns a [block_info] containing all the
   instructions starting from [first] until some kind of control flow is
   encountered or the end of the block is reached (i.e. [Iend]). If the returned
   terminator is [None], it is guaranteed that the [last] instruction is not an
   [Iop] (as it would either be part of [instrs] or be a terminator). *)
let extract_block_info : State.t -> Mach.instruction -> block_info =
 fun state first ->
  let rec loop (instr : Mach.instruction) acc =
    let return terminator instrs = { instrs; last = instr; terminator } in
    match instr.desc with
    | Iop op -> (
      match basic_or_terminator_of_operation state op with
      | Basic desc ->
        let instr' = copy_instruction state instr ~desc in
        (* note: useless moves (See `Cfg.is_noop_move`) are no longer removed
           because we want to compute liveness information on CFG values, and
           (i) such moves are necessary to compute the live sets and (ii) they
           can only be identified as useless after register allocation. *)
        DLL.add_end acc instr';
        loop instr.next acc
      | Terminator terminator ->
        return (Terminator (copy_instruction state instr ~desc:terminator)) acc
      | With_next_label terminator ->
        return
          (With_next_label
             (fun label_after ->
               copy_instruction state instr ~desc:(terminator label_after)))
          acc)
    | Iend | Ireturn _ | Iifthenelse _ | Iswitch _ | Icatch _ | Iexit _
    | Itrywith _ | Iraise _ ->
      return Complex_terminator acc
  in
  loop first (DLL.make_empty ())

(* Represents the control flow exiting the function without encountering a
   return. *)
let fallthrough_label : Label.t = -1

(* [add_blocks instr state ~start ~next ~is_cold] adds the block beginning at
   [instr] with label [start], and all recursively-reachable blocks to [state].
   [next] is the label of the block to be executed after the one beginning at
   [instr]. [is_cold] indicates whether to put the blocks in the cold
   section. *)
let rec add_blocks :
    Mach.instruction ->
    State.t ->
    start:Label.t ->
    next:Label.t ->
    is_cold:bool ->
    unit =
 fun instr state ~start ~next ~is_cold ->
  let { instrs; last; terminator } = extract_block_info state instr in
  let terminate_block ~trap_actions terminator =
    let body = instrs in
    List.iter
      (fun trap_action ->
        let instr =
          match trap_action with
          | Cmm.Push handler_id ->
            let lbl_handler = State.get_catch_handler state ~handler_id in
            make_instruction state ~desc:(Cfg.Pushtrap { lbl_handler })
          | Cmm.Pop _ -> make_instruction state ~desc:Cfg.Poptrap
        in
        DLL.add_end body instr)
      trap_actions;
    (match terminator.Cfg.desc with
    | Cfg.Return ->
      if State.get_contains_calls state
      then DLL.add_end body (make_instruction state ~desc:Cfg.Reloadretaddr)
      else ()
    | Cfg.Never | Cfg.Always _ | Cfg.Parity_test _ | Cfg.Truth_test _
    | Cfg.Float_test _ | Cfg.Int_test _ | Cfg.Switch _ | Cfg.Raise _
    | Cfg.Call_no_return _ | Cfg.Tailcall_self _ | Cfg.Tailcall_func _
    | Cfg.Call _ | Cfg.Prim _ | Cfg.Specific_can_raise _ ->
      ());
    let can_raise =
      (* Recompute [can_raise]. Only terminator can actually raise. *)
      Cfg.can_raise_terminator terminator.Cfg.desc
    in
    State.add_block state ~label:start
      ~block:
        { start;
          body;
          terminator;
          (* See [Cfg.register_predecessors_for_all_blocks] *)
          predecessors = Label.Set.empty;
          (* See [Stack_offset_and_exn.update_cfg] *)
          stack_offset = invalid_stack_offset;
          exn = None;
          can_raise;
          (* See [update_trap_handler_blocks] *)
          is_trap_handler = false;
          dead = false;
          cold = is_cold
        }
  in
  let prepare_next_block () =
    match last.next.desc with
    | Iend | Iop _ | Ireturn _ | Iifthenelse _ | Iswitch _ | Icatch _ | Iexit _
    | Itrywith _ | Iraise _ ->
      let start = Cmm.new_label () in
      let add_next_block () =
        add_blocks last.next state ~start ~next ~is_cold
      in
      start, add_next_block
  in
  match terminator with
  | Terminator terminator -> terminate_block ~trap_actions:[] terminator
  | With_next_label f ->
    let next, add_next_block = prepare_next_block () in
    terminate_block ~trap_actions:[] (f next);
    add_next_block ()
  | Complex_terminator -> (
    match last.desc with
    | Iop _ ->
      Misc.fatal_error
        "Cfgize.extract_block_info: unexpected Iop as Complex_terminator"
    | Iend ->
      if Label.equal next fallthrough_label
      then
        terminate_block ~trap_actions:[]
          (copy_instruction_no_reg state last ~desc:Cfg.Never)
      else
        terminate_block ~trap_actions:[]
          (copy_instruction_no_reg state last ~desc:(Cfg.Always next))
    | Ireturn trap_actions ->
      terminate_block ~trap_actions
        (copy_instruction state last ~desc:Cfg.Return)
    | Iifthenelse (test, ifso, ifnot) ->
      let label_true = Cmm.new_label () in
      let label_false = Cmm.new_label () in
      terminate_block ~trap_actions:[]
        (copy_instruction state last
           ~desc:(terminator_of_test test ~label_false ~label_true));
      let next, add_next_block = prepare_next_block () in
      add_blocks ifso state ~start:label_true ~next ~is_cold;
      add_blocks ifnot state ~start:label_false ~next ~is_cold;
      add_next_block ()
    | Iswitch (indexes, cases) ->
      let case_labels = Array.map (fun _ -> Cmm.new_label ()) cases in
      terminate_block ~trap_actions:[]
        (copy_instruction state last
           ~desc:(Cfg.Switch (Array.map (fun idx -> case_labels.(idx)) indexes)));
      let next, add_next_block = prepare_next_block () in
      Array.iteri
        (fun idx case ->
          add_blocks case state ~start:case_labels.(idx) ~next ~is_cold)
        cases;
      add_next_block ()
    | Icatch (_rec, _trap_stack, handlers, body) ->
      let handlers =
        List.map
          (fun (handler_id, _trap_stack, handler, is_cold) ->
            let handler_label = State.add_catch_handler state ~handler_id in
            handler_label, handler, is_cold)
          handlers
      in
      let body_label = Cmm.new_label () in
      terminate_block ~trap_actions:[]
        (copy_instruction_no_reg state last ~desc:(Cfg.Always body_label));
      let next, add_next_block = prepare_next_block () in
      add_blocks body state ~start:body_label ~next ~is_cold;
      List.iter
        (fun (handler_label, handler, is_handler_cold) ->
          add_blocks handler state ~start:handler_label ~next
            ~is_cold:(is_cold || is_handler_cold))
        handlers;
      add_next_block ()
    | Iexit (handler_id, trap_actions) ->
      let handler_label = State.get_catch_handler state ~handler_id in
      terminate_block ~trap_actions
        (copy_instruction_no_reg state last ~desc:(Cfg.Always handler_label))
    | Itrywith (body, handler_id, (_trap_stack, handler)) ->
      let label_body = Cmm.new_label () in
      let label_handler = State.add_catch_handler state ~handler_id in
      terminate_block ~trap_actions:[]
        (copy_instruction_no_reg state last ~desc:(Cfg.Always label_body));
      let next, add_next_block = prepare_next_block () in
      State.add_exception_handler state label_handler;
      add_blocks body state ~start:label_body ~next ~is_cold;
      add_blocks handler state ~start:label_handler ~next ~is_cold;
      add_next_block ()
    | Iraise raise_kind ->
      terminate_block ~trap_actions:[]
        (copy_instruction state last ~desc:(Cfg.Raise raise_kind)))

let update_trap_handler_blocks : State.t -> Cfg.t -> unit =
 fun state cfg ->
  List.iter
    (fun label ->
      match Label.Tbl.find_opt cfg.blocks label with
      | None ->
        Misc.fatal_errorf
          "Cfgize.update_trap_handler_blocks: inconsistent state (no block \
           labelled %d)"
          label
      | Some block -> block.is_trap_handler <- true)
    (State.get_exception_handlers state)

module Stack_offset_and_exn = struct
  (* This module relies on the field `can_raise` of basic blocks but does not
     rely on this field of individual Cfg instructions. This may need to be
     revisited when we remove dead trap handlers and the associated
     pushtrap/poptrap operations. *)
  type handler_stack = Label.t list

  let compute_stack_offset ~stack_offset ~traps =
    stack_offset + (Proc.trap_size_in_bytes * List.length traps)

  let check_and_set_stack_offset :
      'a Cfg.instruction -> stack_offset:int -> traps:handler_stack -> unit =
   fun instr ~stack_offset ~traps ->
    assert (instr.stack_offset = invalid_stack_offset);
    Cfg.set_stack_offset instr (compute_stack_offset ~stack_offset ~traps)

  let process_terminator :
      stack_offset:int ->
      traps:handler_stack ->
      Cfg.terminator Cfg.instruction ->
      int * handler_stack =
   fun ~stack_offset ~traps term ->
    check_and_set_stack_offset term ~stack_offset ~traps;
    match term.desc with
    | Tailcall_self _
      when stack_offset <> 0 || List.compare_length_with traps 0 <> 0 ->
      Misc.fatal_error
        "Cfgize.Stack_offset_and_exn.process_terminator: unexpected handler on \
         self tailcall"
    | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
    | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
    | Tailcall_func _ | Call_no_return _ | Call _ | Prim _
    | Specific_can_raise _ ->
      stack_offset, traps

  let rec process_basic :
      Cfg.t ->
      stack_offset:int ->
      traps:handler_stack ->
      Cfg.basic Cfg.instruction ->
      int * handler_stack =
   fun cfg ~stack_offset ~traps instr ->
    check_and_set_stack_offset instr ~stack_offset ~traps;
    match instr.desc with
    | Pushtrap { lbl_handler } ->
      update_block cfg lbl_handler ~stack_offset ~traps;
      stack_offset, lbl_handler :: traps
    | Poptrap -> (
      match traps with
      | [] ->
        Misc.fatal_error
          "Cfgize.Stack_offset_and_exn.process_basic: trying to pop from an \
           empty stack"
      | _ :: traps -> stack_offset, traps)
    | Op (Stackoffset n) -> stack_offset + n, traps
    | Op
        ( Move | Spill | Reload | Const_int _ | Const_float _ | Const_float32 _
        | Const_symbol _ | Const_vec128 _ | Load _ | Store _ | Intop _
        | Intop_imm _ | Intop_atomic _ | Floatop _ | Csel _ | Static_cast _
        | Reinterpret_cast _ | Probe_is_enabled _ | Opaque | Begin_region
        | End_region | Specific _ | Name_for_debugger _ | Dls_get | Poll
        | Alloc _ )
    | Reloadretaddr | Prologue ->
      stack_offset, traps
    | Stack_check _ ->
      Misc.fatal_error
        "Cfgize.Stack_offset_and_exn.process_basic: unexpected stack check"

  (* The argument [stack_offset] has a different meaning from the field
     [stack_offset] of Cfg's basic_blocks and instructions. The argument
     [stack_offset] refers to the offset derived from Istackoffset operations
     only, whereas the field also includes the trap stack depth in bytes. Both
     are non-negative. See [compute_stack_offset]. *)
  and update_block :
      Cfg.t -> Label.t -> stack_offset:int -> traps:handler_stack -> unit =
   fun cfg label ~stack_offset ~traps ->
    let block = Cfg.get_block_exn cfg label in
    let was_invalid =
      if block.stack_offset = invalid_stack_offset
      then true
      else (
        if debug
        then
          assert (block.stack_offset = compute_stack_offset ~stack_offset ~traps);
        false)
    in
    if was_invalid
    then (
      block.stack_offset <- compute_stack_offset ~stack_offset ~traps;
      let stack_offset, traps =
        DLL.fold_left block.body ~init:(stack_offset, traps)
          ~f:(fun (stack_offset, traps) instr ->
            process_basic cfg ~stack_offset ~traps instr)
      in
      let stack_offset, traps =
        process_terminator ~stack_offset ~traps block.terminator
      in
      (* non-exceptional successors *)
      Label.Set.iter
        (update_block cfg ~stack_offset ~traps)
        (Cfg.successor_labels ~normal:true ~exn:false block);
      (* exceptional successor *)
      if block.can_raise
      then (
        assert (Option.is_none block.exn);
        match traps with
        | [] -> ()
        | handler_label :: _ -> block.exn <- Some handler_label))

  let update_cfg : Cfg.t -> unit =
   fun cfg ->
    update_block cfg cfg.entry_label ~stack_offset:0 ~traps:[];
    Cfg.iter_blocks cfg ~f:(fun _ block ->
        if block.stack_offset = invalid_stack_offset then block.dead <- true;
        assert (not (block.is_trap_handler && block.dead)))
end

let fundecl :
    Mach.fundecl ->
    before_register_allocation:bool ->
    preserve_orig_labels:bool ->
    simplify_terminators:bool ->
    Cfg_with_layout.t =
 fun fundecl ~before_register_allocation ~preserve_orig_labels
     ~simplify_terminators ->
  let { Mach.fun_name;
        fun_args;
        fun_body;
        fun_codegen_options;
        fun_dbg;
        fun_num_stack_slots;
        fun_contains_calls;
        (* CR-someday mshinwell: [fun_poll] will need to be propagated in the
           future, e.g. when writing a [Polling] equivalent on [Cfg]. We don't
           do this at present since there is no need. *)
        fun_poll = _
      } =
    fundecl
  in
  let start_label = Cmm.new_label () in
  let tailrec_label = Cmm.new_label () in
  (* CR xclerc for xclerc: with the new pipeline, stacks slots are always 0 when
     Cfgize is called; we (temporarily?) always add a prologue and remove it
     after register allocation if it is not required. *)
  let prologue_required =
    if before_register_allocation
    then true
    else Proc.prologue_required ~fun_contains_calls ~fun_num_stack_slots
  in
  let cfg =
    Cfg.create ~fun_name ~fun_args
      ~fun_codegen_options:(Cfg.of_cmm_codegen_option fun_codegen_options)
      ~fun_dbg ~fun_contains_calls ~fun_num_stack_slots
  in
  let state =
    State.make ~fun_name ~tailrec_label ~contains_calls:fun_contains_calls
      cfg.blocks
  in
  (* Ensure any name-for-debugger operations come at the very start, even before
     the prologue. Otherwise, when the debugger is standing at the start of a
     function, parameters may be unavailable. *)
  let rev_name_for_debugger_ops_before_prologue, fun_body =
    let rec collect (instr : Mach.instruction) ops =
      match[@ocaml.warning "-4"] instr.desc with
      | Iop
          (Iname_for_debugger
            { ident; which_parameter; provenance; is_assignment; regs }) ->
        let cfg_op : Cfg.operation =
          Name_for_debugger
            { ident; which_parameter; provenance; is_assignment; regs }
        in
        (* We use the ordering of the collected instructions to identify the
           last name-for-debugger operation, whose availability set we will use
           for the inserted Cfg block, and crucially also to copy over to the
           prologue. The last step is important so that the parameters' ranges
           start at the very beginning of the function's code. *)
        collect instr.next ((instr, cfg_op) :: ops)
      | _ -> ops, instr
    in
    if !Clflags.debug && not !Dwarf_flags.restrict_to_upstream_dwarf
    then collect fun_body []
    else [], fun_body
  in
  let next_label, instr_to_copy =
    match rev_name_for_debugger_ops_before_prologue with
    | [] -> Cfg.entry_label cfg, fun_body
    | _ :: _ ->
      let instrs =
        List.map
          (fun ((naming_op_instr : Mach.instruction), naming_cfg_op) ->
            let instr = make_instruction state ~desc:(Cfg.Op naming_cfg_op) in
            instr.dbg <- naming_op_instr.dbg;
            instr.fdo <- Fdo_info.none;
            instr)
          (List.rev rev_name_for_debugger_ops_before_prologue)
      in
      let next_label = Cmm.new_label () in
      let last_naming_op_instr =
        fst (List.hd rev_name_for_debugger_ops_before_prologue)
      in
      State.add_block state ~label:(Cfg.entry_label cfg)
        ~block:
          { start = Cfg.entry_label cfg;
            body = DLL.of_list instrs;
            terminator =
              copy_instruction_no_reg state last_naming_op_instr
                ~desc:(Cfg.Always next_label);
            (* See [Cfg.register_predecessors_for_all_blocks] *)
            predecessors = Label.Set.empty;
            stack_offset = invalid_stack_offset;
            exn = None;
            can_raise = false;
            is_trap_handler = false;
            dead = false;
            cold = false
          };
      next_label, last_naming_op_instr
  in
  State.add_block state ~label:next_label
    ~block:
      { start = next_label;
        body =
          (match prologue_required with
          | false -> DLL.make_empty ()
          | true ->
            let instr =
              copy_instruction_no_reg state instr_to_copy ~desc:Cfg.Prologue
            in
            instr.dbg <- instr_to_copy.dbg;
            instr.fdo <- Fdo_info.none;
            DLL.make_single instr);
        terminator =
          copy_instruction_no_reg state instr_to_copy
            ~desc:(Cfg.Always tailrec_label);
        (* See [Cfg.register_predecessors_for_all_blocks] *)
        predecessors = Label.Set.empty;
        stack_offset = invalid_stack_offset;
        exn = None;
        can_raise = false;
        is_trap_handler = false;
        dead = false;
        cold = false
      };
  State.add_block state ~label:tailrec_label
    ~block:
      { start = tailrec_label;
        body = DLL.make_empty ();
        terminator =
          copy_instruction_no_reg state instr_to_copy
            ~desc:(Cfg.Always start_label);
        (* See [Cfg.register_predecessors_for_all_blocks] *)
        predecessors = Label.Set.empty;
        stack_offset = invalid_stack_offset;
        exn = None;
        can_raise = false;
        is_trap_handler = false;
        dead = false;
        cold = false
      };
  add_blocks fun_body state ~start:start_label ~next:fallthrough_label
    ~is_cold:false;
  update_trap_handler_blocks state cfg;
  (* note: `Stack_offset_and_exn.update_cfg` may add edges to the graph, and
     should hence be executed before
     `Cfg.register_predecessors_for_all_blocks`. *)
  Stack_offset_and_exn.update_cfg cfg;
  Profile.record ~accumulate:true "register_preds"
    Cfg.register_predecessors_for_all_blocks cfg;
  let cfg_with_layout =
    Cfg_with_layout.create cfg ~layout:(State.get_layout state)
      ~preserve_orig_labels ~new_labels:Label.Set.empty
  in
  (* note: the simplification of terminators is necessary for the equality. The
     other code path simplifies e.g. a switch with three branches into an
     integer test. This simplification should happen *after* the one about
     straightline blocks because merging blocks creates more opportunities for
     terminator simplification. *)
  Profile.record ~accumulate:true "optimizations"
    (fun () ->
      if simplify_terminators
      then Eliminate_fallthrough_blocks.run cfg_with_layout;
      if simplify_terminators then Merge_straightline_blocks.run cfg_with_layout;
      Eliminate_dead_code.run_dead_block cfg_with_layout;
      if simplify_terminators then Simplify_terminator.run cfg)
    ();
  Cfg_with_layout.reorder_blocks
    ~comparator:(fun label1 label2 ->
      let block1 = Cfg.get_block_exn cfg label1 in
      let block2 = Cfg.get_block_exn cfg label2 in
      Bool.compare block1.cold block2.cold)
    cfg_with_layout;
  cfg_with_layout
