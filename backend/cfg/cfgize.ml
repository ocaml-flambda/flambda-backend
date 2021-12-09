(* CR-soon xclerc for xclerc: use the same warning set as flambda2. *)
[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

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

  val get_layout : t -> Label.t list

  val add_catch_handler : t -> handler_id:int -> Label.t

  val get_catch_handler : t -> handler_id:int -> Label.t

  val get_next_instruction_id : t -> int

  val add_iend_with_poptrap : t -> Mach.instruction -> unit

  val is_iend_with_poptrap : t -> Mach.instruction -> bool

  val add_exception_handler : t -> Label.t -> unit

  val get_exception_handlers : t -> Label.t list
end = struct
  type t =
    { fun_name : string;
      tailrec_label : Label.t;
      contains_calls : bool;
      blocks : Cfg.basic_block Label.Tbl.t;
      mutable layout : Label.t list;
      catch_handlers : Label.t Numbers.Int.Tbl.t;
      mutable next_instruction_id : int;
      mutable iends_with_poptrap : Mach.instruction list;
      mutable exception_handlers : Label.t list
    }

  let make ~fun_name ~tailrec_label ~contains_calls blocks =
    let layout = [] in
    let catch_handlers = Numbers.Int.Tbl.create 31 in
    let next_instruction_id = 0 in
    let iends_with_poptrap = [] in
    let exception_handlers = [] in
    { fun_name;
      tailrec_label;
      contains_calls;
      blocks;
      layout;
      catch_handlers;
      next_instruction_id;
      iends_with_poptrap;
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
    else begin
      t.layout <- label :: t.layout;
      Label.Tbl.replace t.blocks label block
    end

  let get_layout t = List.rev t.layout

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

  let is_iend instr =
    match instr.Mach.desc with
    | Iend -> true
    | Iop _ | Ireturn _ | Iifthenelse _ | Iswitch _ | Icatch _ | Iexit _
    | Itrywith _ | Iraise _ ->
      false

  let add_iend_with_poptrap t iend =
    assert (is_iend iend);
    t.iends_with_poptrap <- iend :: t.iends_with_poptrap

  let is_iend_with_poptrap t iend =
    assert (is_iend iend);
    List.memq iend t.iends_with_poptrap

  let add_exception_handler t lbl =
    t.exception_handlers <- lbl :: t.exception_handlers

  let get_exception_handlers t = t.exception_handlers
end

type basic_or_terminator =
  | Basic of Cfg.basic
  | Terminator of Cfg.terminator

let basic_or_terminator_of_operation :
    State.t -> Mach.operation -> basic_or_terminator =
 fun state op ->
  match op with
  | Imove -> Basic (Op Move)
  | Ispill -> Basic (Op Spill)
  | Ireload -> Basic (Op Reload)
  | Iconst_int i -> Basic (Op (Const_int i))
  | Iconst_float f -> Basic (Op (Const_float f))
  | Iconst_symbol s -> Basic (Op (Const_symbol s))
  | Icall_ind -> Basic (Call (F Indirect))
  | Icall_imm { func } -> Basic (Call (F (Direct { func_symbol = func })))
  | Itailcall_ind -> Terminator (Tailcall (Func Indirect))
  | Itailcall_imm { func } ->
    Terminator
      (Tailcall
         (if String.equal (State.get_fun_name state) func
         then Self { destination = State.get_tailrec_label state }
         else Func (Direct { func_symbol = func })))
  | Iextcall { func; ty_res; ty_args; alloc; returns } ->
    let external_call = { Cfg.func_symbol = func; alloc; ty_res; ty_args } in
    if returns
    then Basic (Call (P (External external_call)))
    else Terminator (Call_no_return external_call)
  | Istackoffset ofs -> Basic (Op (Stackoffset ofs))
  | Iload (mem, mode) -> Basic (Op (Load (mem, mode)))
  | Istore (mem, mode, assignment) -> Basic (Op (Store (mem, mode, assignment)))
  | Ialloc { bytes; dbginfo } -> Basic (Call (P (Alloc { bytes; dbginfo })))
  | Iintop Icheckbound -> Basic (Call (P (Checkbound { immediate = None })))
  | Iintop_imm (Icheckbound, i) ->
    Basic (Call (P (Checkbound { immediate = Some i })))
  | Iintop
      (( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
       | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) as op) ->
    Basic (Op (Intop op))
  | Iintop_imm
      ( (( Iadd | Isub | Imul | Imulh _ | Idiv | Imod | Iand | Ior | Ixor | Ilsl
         | Ilsr | Iasr | Iclz _ | Ictz _ | Ipopcnt | Icomp _ ) as op),
        imm ) ->
    Basic (Op (Intop_imm (op, imm)))
  | Icompf comp -> Basic (Op (Compf comp))
  | Inegf -> Basic (Op Negf)
  | Iabsf -> Basic (Op Absf)
  | Iaddf -> Basic (Op Addf)
  | Isubf -> Basic (Op Subf)
  | Imulf -> Basic (Op Mulf)
  | Idivf -> Basic (Op Divf)
  | Ifloatofint -> Basic (Op Floatofint)
  | Iintoffloat -> Basic (Op Intoffloat)
  | Ispecific op -> Basic (Op (Specific op))
  | Iopaque -> Basic (Op Opaque)
  | Iname_for_debugger _ ->
    Misc.fatal_error
      "Cfgize.basic_or_terminator_of_operation: \"the Iname_for_debugger\" \
       instruction is currently not supported "
  | Iprobe { name; handler_code_sym } ->
    Basic (Op (Probe { name; handler_code_sym }))
  | Iprobe_is_enabled { name } -> Basic (Op (Probe_is_enabled { name }))

let float_test_of_float_comparison :
    Cmm.float_comparison ->
    label_false:Label.t ->
    label_true:Label.t ->
    Cfg.float_test =
 fun comparison ~label_false ~label_true ->
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
  { lt; eq; gt; uo }

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
  | Ifloattest comparison ->
    Float_test
      (float_test_of_float_comparison comparison ~label_false ~label_true)
  | Ioddtest -> Parity_test { ifso = label_false; ifnot = label_true }
  | Ieventest -> Parity_test { ifso = label_true; ifnot = label_false }

let invalid_trap_depth = -1

let make_instruction : type a. State.t -> desc:a -> a Cfg.instruction =
 fun state ~desc ->
  let arg = [||] in
  let res = [||] in
  let dbg = Debuginfo.none in
  let live = Reg.Set.empty in
  let trap_depth = invalid_trap_depth in
  let id = State.get_next_instruction_id state in
  let fdo = Fdo_info.none in
  { desc; arg; res; dbg; live; trap_depth; id; fdo }

let copy_instruction :
    type a. State.t -> Mach.instruction -> desc:a -> a Cfg.instruction =
 fun state instr ~desc ->
  let { Mach.arg;
        res;
        dbg;
        live;
        desc = _;
        next = _;
        available_before = _;
        available_across = _
      } =
    instr
  in
  let trap_depth = invalid_trap_depth in
  let id = State.get_next_instruction_id state in
  let fdo = Fdo_info.none in
  { desc; arg; res; dbg; live; trap_depth; id; fdo }

let copy_instruction_no_reg :
    type a. State.t -> Mach.instruction -> desc:a -> a Cfg.instruction =
 fun state instr ~desc ->
  let { Mach.arg = _;
        res = _;
        dbg;
        live;
        desc = _;
        next = _;
        available_before = _;
        available_across = _
      } =
    instr
  in
  let arg = [||] in
  let res = [||] in
  let trap_depth = invalid_trap_depth in
  let id = State.get_next_instruction_id state in
  let fdo = Fdo_info.none in
  { desc; arg; res; dbg; live; trap_depth; id; fdo }

let can_raise_operation : Cfg.operation -> bool =
 fun op ->
  match op with
  | Move -> false
  | Spill -> false
  | Reload -> false
  | Const_int _ -> false
  | Const_float _ -> false
  | Const_symbol _ -> false
  | Stackoffset _ -> false
  | Load _ -> false
  | Store _ -> false
  | Intop _ -> false
  | Intop_imm _ -> false
  | Negf -> false
  | Absf -> false
  | Addf -> false
  | Subf -> false
  | Mulf -> false
  | Divf -> false
  | Compf _ -> false
  | Floatofint -> false
  | Intoffloat -> false
  | Probe _ -> true
  | Probe_is_enabled _ -> false (* CR xclerc for xclerc: double check *)
  | Specific _ -> false (* CR xclerc for xclerc: double check *)
  | Opaque -> false
  | Name_for_debugger _ -> false

let can_raise_instr : Cfg.basic Cfg.instruction -> bool =
 fun instr ->
  match instr.desc with
  | Op op -> can_raise_operation op
  | Call _ -> true
  | Reloadretaddr -> false
  | Pushtrap _ -> false
  | Poptrap -> false
  | Prologue -> false

let can_raise_instrs : Cfg.basic Cfg.instruction list -> bool =
 fun instrs -> List.exists can_raise_instr instrs

let is_noop_move (instr : Cfg.basic Cfg.instruction) : bool =
  match instr.Cfg.desc with
  | Op (Move | Spill | Reload) ->
    (* CR xclerc for xclerc: is testing the location enough? *)
    Reg.same_loc instr.Cfg.arg.(0) instr.Cfg.res.(0)
  | Op
      ( Const_int _ | Const_float _ | Const_symbol _ | Stackoffset _ | Load _
      | Store _ | Intop _ | Intop_imm _ | Negf | Absf | Addf | Subf | Mulf
      | Divf | Compf _ | Floatofint | Intoffloat | Probe _ | Opaque
      | Probe_is_enabled _ | Specific _ | Name_for_debugger _ )
  | Call _ | Reloadretaddr | Pushtrap _ | Poptrap | Prologue ->
    false

let rec get_end : Mach.instruction -> Mach.instruction =
 fun instr ->
  match instr.Mach.desc with
  | Iend -> instr
  | Iop _ | Ireturn _ | Iifthenelse _ | Iswitch _ | Icatch _ | Iexit _
  | Itrywith _ | Iraise _ ->
    get_end instr.Mach.next

type block_info =
  { instrs : Cfg.basic Cfg.instruction list;
    last : Mach.instruction;
    terminator : Cfg.terminator Cfg.instruction option;
    can_raise : bool
  }

(* [extract_block_info state first] returns a [block_info] containing all the
   instructions starting from [first] until some kind of control flow is
   encountered or the end of the block is reached (i.e. [Iend]). If the returned
   terminator is [None], it is guaranteed that the [last] instruction is not an
   [Iop] (as it would either be part of [instrs] or be a terminator). *)
let extract_block_info : State.t -> Mach.instruction -> block_info =
 fun state first ->
  let rec loop (instr : Mach.instruction) acc =
    let return terminator can_raise =
      let instrs = List.rev acc in
      let can_raise = can_raise || can_raise_instrs instrs in
      { instrs; last = instr; terminator; can_raise }
    in
    match instr.desc with
    | Iop op -> begin
      match basic_or_terminator_of_operation state op with
      | Basic desc ->
        let instr' = copy_instruction state instr ~desc in
        if is_noop_move instr'
        then loop instr.next acc
        else loop instr.next (instr' :: acc)
      | Terminator terminator ->
        return
          (Some (copy_instruction state instr ~desc:terminator))
          (Cfg.can_raise_terminator terminator)
    end
    | Iend | Ireturn _ | Iifthenelse _ | Iswitch _ | Icatch _ | Iexit _
    | Itrywith _ ->
      return None false
    | Iraise _ -> return None true
  in
  loop first []

(* Represents the control flow exiting the function without encountering a
   return. *)
let fallthrough_label : Label.t = -1

(* [add_blocks instr state ~starts_with_pushtrap ~start ~next] adds the block
   beginning at [instr] with label [start], and all recursively-reachable blocks
   to [state]. [next] is the label of the block to be executed after the one
   beginning at [instr]. [starts_with_pushtrap] indicates whether the block
   should be prefixed with a pushtrap instruction (to the passed label). *)
let rec add_blocks :
    Mach.instruction ->
    State.t ->
    starts_with_pushtrap:Label.t option ->
    start:Label.t ->
    next:Label.t ->
    unit =
 fun instr state ~starts_with_pushtrap ~start ~next ->
  let { instrs; last; terminator; can_raise } =
    extract_block_info state instr
  in
  let terminate_block ~trap_actions terminator =
    let body = instrs in
    let body =
      match starts_with_pushtrap with
      | None -> body
      | Some lbl_handler ->
        make_instruction state ~desc:(Cfg.Pushtrap { lbl_handler }) :: body
    in
    let body =
      body
      @ List.map
          (function
            | Cmm.Push handler_id ->
              let lbl_handler = State.get_catch_handler state ~handler_id in
              make_instruction state ~desc:(Cfg.Pushtrap { lbl_handler })
            | Cmm.Pop -> make_instruction state ~desc:Cfg.Poptrap)
          trap_actions
    in
    let body =
      match terminator.Cfg.desc with
      | Cfg.Return ->
        if State.get_contains_calls state
        then body @ [make_instruction state ~desc:Cfg.Reloadretaddr]
        else body
      | Cfg.Never | Cfg.Always _ | Cfg.Parity_test _ | Cfg.Truth_test _
      | Cfg.Float_test _ | Cfg.Int_test _ | Cfg.Switch _ | Cfg.Raise _
      | Cfg.Tailcall _ | Cfg.Call_no_return _ ->
        body
    in
    State.add_block state ~label:start
      ~block:
        { start;
          body;
          terminator;
          (* See [Cfg.register_predecessors_for_all_blocks] *)
          predecessors = Label.Set.empty;
          (* See [Trap_depth_and_exns.update_cfg] *)
          trap_depth = invalid_trap_depth;
          exns = Label.Set.empty;
          can_raise;
          (* See [update_trap_handler_blocks] *)
          is_trap_handler = false;
          dead = false
        }
  in
  let prepare_next_block () =
    match last.next.desc with
    | Iend when not (State.is_iend_with_poptrap state last.next) ->
      next, fun () -> ()
    | Iend | Iop _ | Ireturn _ | Iifthenelse _ | Iswitch _ | Icatch _ | Iexit _
    | Itrywith _ | Iraise _ ->
      let start = Cmm.new_label () in
      let add_next_block () =
        add_blocks last.next state ~starts_with_pushtrap:None ~start ~next
      in
      start, add_next_block
  in
  match terminator with
  | Some terminator -> terminate_block ~trap_actions:[] terminator
  | None -> (
    match last.desc with
    | Iop _ ->
      Misc.fatal_error "Cfgize.extract_block_info: Iop with no terminator"
    | Iend ->
      if Label.equal next fallthrough_label
      then
        terminate_block ~trap_actions:[]
          (copy_instruction_no_reg state last ~desc:Cfg.Never)
      else
        terminate_block
          ~trap_actions:
            (if State.is_iend_with_poptrap state last then [Cmm.Pop] else [])
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
      add_blocks ifso state ~starts_with_pushtrap:None ~start:label_true ~next;
      add_blocks ifnot state ~starts_with_pushtrap:None ~start:label_false ~next;
      add_next_block ()
    | Iswitch (indexes, cases) ->
      let case_labels = Array.map (fun _ -> Cmm.new_label ()) cases in
      terminate_block ~trap_actions:[]
        (copy_instruction state last
           ~desc:(Cfg.Switch (Array.map (fun idx -> case_labels.(idx)) indexes)));
      let next, add_next_block = prepare_next_block () in
      Array.iteri
        (fun idx case ->
          add_blocks case state ~starts_with_pushtrap:None
            ~start:case_labels.(idx) ~next)
        cases;
      add_next_block ()
    | Icatch (_rec, _trap_stack, handlers, body) ->
      let handlers =
        List.map
          (fun (handler_id, _trap_stack, handler) ->
            let handler_label = State.add_catch_handler state ~handler_id in
            handler_label, handler)
          handlers
      in
      let body_label = Cmm.new_label () in
      terminate_block ~trap_actions:[]
        (copy_instruction_no_reg state last ~desc:(Cfg.Always body_label));
      let next, add_next_block = prepare_next_block () in
      add_blocks body state ~starts_with_pushtrap:None ~start:body_label ~next;
      List.iter
        (fun (handler_label, handler) ->
          add_blocks handler state ~starts_with_pushtrap:None
            ~start:handler_label ~next)
        handlers;
      add_next_block ()
    | Iexit (handler_id, trap_actions) ->
      let handler_label = State.get_catch_handler state ~handler_id in
      terminate_block ~trap_actions
        (copy_instruction_no_reg state last ~desc:(Cfg.Always handler_label))
    | Itrywith (body, kind, (_trap_stack, handler)) ->
      let label_body = Cmm.new_label () in
      let label_handler, starts_with_pushtrap =
        match kind with
        | Regular ->
          let label = Cmm.new_label () in
          label, Some label
        | Delayed handler_id ->
          let label = State.add_catch_handler state ~handler_id in
          label, None
      in
      terminate_block ~trap_actions:[]
        (copy_instruction_no_reg state last ~desc:(Cfg.Always label_body));
      let next, add_next_block = prepare_next_block () in
      State.add_iend_with_poptrap state (get_end body);
      State.add_exception_handler state label_handler;
      add_blocks body state ~starts_with_pushtrap ~start:label_body ~next;
      add_blocks handler state ~starts_with_pushtrap:None ~start:label_handler
        ~next;
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

module Trap_depth_and_exns = struct
  type handler_stack = Label.t list

  let equal_handler_stack : handler_stack -> handler_stack -> bool =
   fun left right -> List.equal Label.equal left right

  type handler_table = handler_stack Label.Tbl.t

  let record_handler :
      type a.
      handler_stack -> handler_table -> can_raise:(a -> bool) -> a -> unit =
   fun stack table ~can_raise instr ->
    if can_raise instr
    then
      match stack with
      | [] -> ()
      | handler_label :: handler_stack ->
        Option.iter
          (fun existing_stack ->
            assert (equal_handler_stack existing_stack handler_stack))
          (Label.Tbl.find_opt table handler_label);
        Label.Tbl.replace table handler_label handler_stack

  let terminator :
      handler_table ->
      handler_stack ->
      Cfg.terminator Cfg.instruction ->
      handler_stack =
   fun table stack term ->
    match term.desc with
    | Never | Return
    | Tailcall (Func _)
    | Call_no_return _ | Raise _ | Always _ | Parity_test _ | Truth_test _
    | Float_test _ | Int_test _ | Switch _ ->
      record_handler stack table ~can_raise:Cfg.can_raise_terminator term.desc;
      stack
    | Tailcall (Self _) ->
      if List.length stack <> 0
      then
        Misc.fatal_error
          "Cfgize.Trap_depth_and_exns.basic: unexpected handler on self \
           tailcall";
      stack

  let basic :
      handler_table ->
      handler_stack ->
      Cfg.basic Cfg.instruction ->
      handler_stack =
   fun table stack instr ->
    match instr.desc with
    | Pushtrap { lbl_handler } -> lbl_handler :: stack
    | Poptrap -> begin
      match stack with
      | [] ->
        Misc.fatal_error
          "Cfgize.Trap_depth_and_exns.basic: trying to pop from an empty stack"
      | _ :: stack -> stack
    end
    | Op _ | Call _ | Reloadretaddr | Prologue ->
      record_handler stack table ~can_raise:can_raise_instr instr;
      stack

  let rec update_block : Cfg.t -> Label.t -> handler_stack -> unit =
   fun cfg label stack ->
    let block = Cfg.get_block_exn cfg label in
    if block.trap_depth = invalid_trap_depth
    then begin
      block.trap_depth <- succ (List.length stack);
      (* map from handlers reachable from the block to stacks at the start of
         such blocks; used to both know which other blocks should be visited (as
         exceptional successors) and to populate the `exns` field. *)
      let table = Label.Tbl.create 17 in
      let stack =
        terminator table
          (ListLabels.fold_left block.body ~init:stack ~f:(basic table))
          block.terminator
      in
      (* non-exceptional successors *)
      Label.Set.iter
        (fun successor_label -> update_block cfg successor_label stack)
        (Cfg.successor_labels ~normal:true ~exn:false block);
      (* exceptional successors *)
      Label.Tbl.iter
        (fun handler_label handler_stack ->
          block.exns <- Label.Set.add handler_label block.exns;
          update_block cfg handler_label handler_stack)
        table
    end
    else assert (block.trap_depth = succ (List.length stack))

  let update_cfg : Cfg.t -> unit =
   fun cfg -> update_block cfg cfg.entry_label []
end

let fundecl :
    Mach.fundecl ->
    preserve_orig_labels:bool ->
    simplify_terminators:bool ->
    Cfg_with_layout.t =
 fun fundecl ~preserve_orig_labels ~simplify_terminators ->
  let { Mach.fun_name;
        fun_args = _;
        fun_body;
        fun_codegen_options;
        fun_dbg;
        fun_num_stack_slots;
        fun_contains_calls
      } =
    fundecl
  in
  let start_label = Cmm.new_label () in
  let tailrec_label = Cmm.new_label () in
  let fun_fast = not (List.mem Cmm.Reduce_code_size fun_codegen_options) in
  let prologue_required =
    Proc.prologue_required ~fun_contains_calls ~fun_num_stack_slots
  in
  let cfg =
    Cfg.create ~fun_name ~fun_dbg ~fun_fast ~fun_contains_calls
      ~fun_num_stack_slots
  in
  let state =
    State.make ~fun_name ~tailrec_label ~contains_calls:fun_contains_calls
      cfg.blocks
  in
  State.add_block state ~label:(Cfg.entry_label cfg)
    ~block:
      { start = Cfg.entry_label cfg;
        body =
          begin
            match prologue_required with
            | false -> []
            | true ->
              let dbg = fun_body.dbg in
              let fdo = Fdo_info.none in
              (* Note: the prologue must come after all `Iname_for_debugger`
                 instructions (this is currently not a concern because we do not
                 support such instructions). *)
              [{ (make_instruction state ~desc:Cfg.Prologue) with dbg; fdo }]
          end;
        terminator =
          copy_instruction_no_reg state fun_body
            ~desc:(Cfg.Always tailrec_label);
        (* See [Cfg.register_predecessors_for_all_blocks] *)
        predecessors = Label.Set.empty;
        trap_depth = invalid_trap_depth;
        exns = Label.Set.empty;
        can_raise = false;
        is_trap_handler = false;
        dead = false
      };
  State.add_block state ~label:tailrec_label
    ~block:
      { start = tailrec_label;
        body = [];
        terminator =
          copy_instruction_no_reg state fun_body ~desc:(Cfg.Always start_label);
        (* See [Cfg.register_predecessors_for_all_blocks] *)
        predecessors = Label.Set.empty;
        trap_depth = invalid_trap_depth;
        exns = Label.Set.empty;
        can_raise = false;
        is_trap_handler = false;
        dead = false
      };
  add_blocks fun_body state ~starts_with_pushtrap:None ~start:start_label
    ~next:fallthrough_label;
  update_trap_handler_blocks state cfg;
  (* note: `Trap_depth_and_exns.update_cfg` may add edges to the graph, and
     should hence be executed before
     `Cfg.register_predecessors_for_all_blocks`. *)
  Trap_depth_and_exns.update_cfg cfg;
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
  Eliminate_fallthrough_blocks.run cfg_with_layout;
  Merge_straightline_blocks.run cfg_with_layout;
  Eliminate_dead_code.run_dead_block cfg_with_layout;
  if simplify_terminators then Simplify_terminator.run cfg;
  cfg_with_layout
