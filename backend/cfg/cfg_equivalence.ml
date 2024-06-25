[@@@ocaml.warning "+a-30-40-41-42"]

module DLL = Flambda_backend_utils.Doubly_linked_list

type location = string

exception
  Different of
    { location : location;
      message : string
    }

let different : location -> string -> _ =
 fun location message -> raise (Different { location; message })

module type State = sig
  type t

  val make : unit -> t

  val add_to_explore : t -> Label.t -> Label.t -> unit

  val to_explore : t -> (Label.t * Label.t) option

  val add_seen : t -> Label.t -> unit

  val has_seen : t -> Label.t -> bool

  val num_seen : t -> int

  val add_labels_to_check : t -> location -> Label.t -> Label.t -> unit

  val _add_label_sets_to_check :
    t -> location -> Label.Set.t -> Label.Set.t -> unit

  val check : t -> Cfg.t -> unit
end

module type Container = sig
  type 'a t

  val make : unit -> 'a t

  val add : 'a -> 'a t -> unit

  val get : 'a t -> 'a option
end

module StackContainer : Container = struct
  type 'a t = 'a Stack.t

  let make = Stack.create

  let add = Stack.push

  let get = Stack.pop_opt
end

(*
 * module QueueContainer : Container = struct
 *   type 'a t = 'a Queue.t
 *   let make = Queue.create
 *   let add = Queue.add
 *   let get = Queue.take_opt
 * end
 *)

module Make_state (C : Container) : State = struct
  type t =
    { subst : Label.t Label.Tbl.t;
      to_explore : (Label.t * Label.t) C.t;
      mutable seen : Label.Set.t;
      mutable labels_to_check : (location * Label.t * Label.t) list;
      mutable label_sets_to_check : (location * Label.Set.t * Label.Set.t) list
    }

  let make () =
    let subst = Label.Tbl.create 123 in
    let to_explore = C.make () in
    let seen = Label.Set.empty in
    let labels_to_check = [] in
    let label_sets_to_check = [] in
    { to_explore; subst; seen; labels_to_check; label_sets_to_check }

  let add_subst t from to_ =
    Label.Tbl.iter
      (fun key data ->
        match Label.equal key from, Label.equal data to_ with
        | true, true | false, false -> ()
        | true, false | false, true ->
          Misc.fatal_errorf
            "Cfg_equivalence: inconsistent substitution (trying to add %a %a)"
            Label.print from Label.print to_)
      t.subst;
    Label.Tbl.replace t.subst from to_

  let add_to_explore t lbl1 lbl2 =
    add_subst t lbl1 lbl2;
    C.add (lbl1, lbl2) t.to_explore

  let to_explore t = C.get t.to_explore

  let add_seen t lbl = t.seen <- Label.Set.add lbl t.seen

  let has_seen t lbl = Label.Set.mem lbl t.seen

  let num_seen t = Label.Set.cardinal t.seen

  let add_labels_to_check t location lbl1 lbl2 =
    t.labels_to_check <- (location, lbl1, lbl2) :: t.labels_to_check

  let _add_label_sets_to_check t location set1 set2 =
    t.label_sets_to_check <- (location, set1, set2) :: t.label_sets_to_check

  let check_label t cfg location lbl1 lbl2 =
    if Label.Tbl.mem cfg.Cfg.blocks lbl1
    then (
      match Label.Tbl.find_opt t.subst lbl1 with
      | None ->
        different location
          (Printf.sprintf "label %s is not mapped" (Label.to_string lbl1))
      | Some lbl2' ->
        if not (Label.equal lbl2 lbl2')
        then
          different location
            (Printf.sprintf "label %s is mapped to %s (but %s was expected)"
               (Label.to_string lbl1) (Label.to_string lbl2')
               (Label.to_string lbl2)))
    else ()
  (* the label is not present in the "original" CFG, just ignore it *)

  let check t cfg =
    List.iter
      (fun (location, lbl1, lbl2) -> check_label t cfg location lbl1 lbl2)
      t.labels_to_check;
    List.iter
      (fun (location, set1, set2) ->
        let set1' =
          Label.Set.fold
            (fun lbl acc ->
              match Label.Tbl.find_opt t.subst lbl with
              | Some lbl' -> Label.Set.add lbl' acc
              | None ->
                different location
                  (Printf.sprintf "label %s is not mapped" (Label.to_string lbl)))
            set1 Label.Set.empty
        in
        if not (Label.Set.equal set1' set2)
        then
          let string_of_label_set set =
            set |> Label.Set.elements
            |> ListLabels.map ~f:Label.to_string
            |> StringLabels.concat ~sep:","
          in
          different location
            (Printf.sprintf "{%s}/{%s} <> {%s}" (string_of_label_set set1)
               (string_of_label_set set1')
               (string_of_label_set set2)))
      t.label_sets_to_check
end

module State = Make_state (StackContainer)

(* note: defined here because we do not want to touch anything under ocaml/ *)
let equal_raise_kind : Lambda.raise_kind -> Lambda.raise_kind -> bool =
 fun left right ->
  match left, right with
  | Raise_regular, Raise_regular -> true
  | Raise_reraise, Raise_reraise -> true
  | Raise_notrace, Raise_notrace -> true
  | Raise_regular, (Raise_reraise | Raise_notrace)
  | Raise_reraise, (Raise_regular | Raise_notrace)
  | Raise_notrace, (Raise_regular | Raise_reraise) ->
    false

let array_equal eq left right =
  Array.length left = Array.length right && Array.for_all2 eq left right

let is_valid_stack_offset : int -> bool = fun stack_offset -> stack_offset >= 0

let check_external_call_operation :
    location ->
    Cfg.external_call_operation ->
    Cfg.external_call_operation ->
    unit =
 fun location expected result ->
  if not (String.equal expected.func_symbol result.func_symbol)
  then different location "function symbol";
  if not (Bool.equal expected.alloc result.alloc)
  then different location "allocating";
  if not
       (array_equal Cmm.equal_machtype_component expected.ty_res result.ty_res)
  then different location "result type";
  if not (List.equal Cmm.equal_exttype expected.ty_args result.ty_args)
  then different location "argument types"

let check_operation : location -> Cfg.operation -> Cfg.operation -> unit =
 fun location expected result ->
  match expected, result with
  | Move, Move -> ()
  | Spill, Spill -> ()
  | Reload, Reload -> ()
  | Const_int expected, Const_int result when Nativeint.equal expected result ->
    ()
  | Const_float expected, Const_float result when Int64.equal expected result ->
    ()
  | Const_float32 expected, Const_float32 result
    when Int32.equal expected result ->
    ()
  | Const_symbol expected, Const_symbol result
    when String.equal expected.sym_name result.sym_name ->
    ()
  | Stackoffset expected, Stackoffset result when Int.equal expected result ->
    ()
  | ( Load
        { memory_chunk = expected_mem;
          addressing_mode = expected_arch_mode;
          mutability = expected_mut;
          is_atomic = expected_atomic
        },
      Load
        { memory_chunk = result_mem;
          addressing_mode = result_arch_mode;
          mutability = result_mut;
          is_atomic = result_atomic
        } )
    when Cmm.equal_memory_chunk expected_mem result_mem
         && expected_mut = result_mut
         && Arch.equal_addressing_mode expected_arch_mode result_arch_mode
         && Bool.equal expected_atomic result_atomic ->
    ()
  | ( Store (expected_mem, expected_arch_mode, expected_bool),
      Store (result_mem, result_arch_mode, result_bool) )
    when Cmm.equal_memory_chunk expected_mem result_mem
         && Arch.equal_addressing_mode expected_arch_mode result_arch_mode
         && Bool.equal expected_bool result_bool ->
    ()
  | Intop left_op, Intop right_op
    when Mach.equal_integer_operation left_op right_op ->
    ()
  | Intop_imm (left_op, left_imm), Intop_imm (right_op, right_imm)
    when Mach.equal_integer_operation left_op right_op
         && Int.equal left_imm right_imm ->
    ()
  | Floatop (left_w, left_op), Floatop (right_w, right_op)
    when Mach.equal_float_width left_w right_w
         && Mach.equal_float_operation left_op right_op ->
    ()
  | Reinterpret_cast left, Reinterpret_cast right
    when Cmm.equal_reinterpret_cast left right ->
    ()
  | Static_cast left, Static_cast right when Cmm.equal_static_cast left right ->
    ()
  | ( Probe_is_enabled { name = expected_name },
      Probe_is_enabled { name = result_name } )
    when String.equal expected_name result_name ->
    ()
  | Specific expected_spec, Specific result_spec
    when Arch.equal_specific_operation expected_spec result_spec ->
    ()
  | Opaque, Opaque -> ()
  | Begin_region, Begin_region -> ()
  | End_region, End_region -> ()
  | ( Name_for_debugger
        { ident = left_ident;
          which_parameter = left_which_parameter;
          provenance = left_provenance;
          is_assignment = left_is_assignment;
          regs = left_regs
        },
      Name_for_debugger
        { ident = right_ident;
          which_parameter = right_which_parameter;
          provenance = right_provenance;
          is_assignment = right_is_assignment;
          regs = right_regs
        } )
    when Ident.same left_ident right_ident
         && Option.equal Int.equal left_which_parameter right_which_parameter
         && Option.equal Backend_var.Provenance.equal left_provenance
              right_provenance
         && Bool.equal left_is_assignment right_is_assignment
         && List.equal Reg.same (Array.to_list left_regs)
              (Array.to_list right_regs) ->
    ()
  | Dls_get, Dls_get -> ()
  | Poll, Poll -> ()
  | ( Alloc
        { bytes = expected_bytes;
          dbginfo = _expected_dbginfo;
          mode = expected_mode
        },
      Alloc
        { bytes = result_bytes; dbginfo = _result_dbginfo; mode = result_mode }
    )
    when Int.equal expected_bytes result_bytes
         && Lambda.eq_mode expected_mode result_mode ->
    (* CR xclerc for xclerc: also check debug info *)
    ()
  | _ -> different location "primitive call operation"
 [@@ocaml.warning "-4"]

let check_prim_call_operation :
    location -> Cfg.prim_call_operation -> Cfg.prim_call_operation -> unit =
 fun location expected result ->
  match expected, result with
  | External expected, External result ->
    check_external_call_operation location expected result
  | ( Probe
        { name = expected_name;
          handler_code_sym = expected_handler_code_sym;
          enabled_at_init = expected_enabled_at_init
        },
      Probe
        { name = result_name;
          handler_code_sym = result_handler_code_sym;
          enabled_at_init = result_enabled_at_init
        } )
    when String.equal expected_name result_name
         && String.equal expected_handler_code_sym result_handler_code_sym
         && Bool.equal expected_enabled_at_init result_enabled_at_init ->
    ()
  | _ -> different location "primitive call operation"
 [@@ocaml.warning "-4"]

let check_func_call_operation :
    location -> Cfg.func_call_operation -> Cfg.func_call_operation -> unit =
 fun location expected result ->
  match expected, result with
  | Indirect, Indirect -> ()
  | Direct expected_func_symbol, Direct result_func_symbol
    when String.equal expected_func_symbol.sym_name result_func_symbol.sym_name
    ->
    ()
  | _ -> different location "function call operation"
 [@@ocaml.warning "-4"]

let check_basic : State.t -> location -> Cfg.basic -> Cfg.basic -> unit =
 fun state location expected result ->
  match expected, result with
  | Op expected, Op result -> check_operation location expected result
  | Reloadretaddr, Reloadretaddr -> ()
  | ( Pushtrap { lbl_handler = expected_lbl_handler },
      Pushtrap { lbl_handler = result_lbl_handler } ) ->
    State.add_to_explore state expected_lbl_handler result_lbl_handler
  | Poptrap, Poptrap -> ()
  | Prologue, Prologue -> ()
  | ( Stack_check { max_frame_size_bytes = expected_max_frame_size_bytes },
      Stack_check { max_frame_size_bytes = result_max_frame_size_bytes } ) ->
    if expected_max_frame_size_bytes <> result_max_frame_size_bytes
    then different location "stack check"
  | _ -> different location "basic"
 [@@ocaml.warning "-4"]

let check_instruction :
    type a.
    check_live:bool ->
    check_dbg:bool ->
    check_arg:bool ->
    int ->
    location ->
    a Cfg.instruction ->
    a Cfg.instruction ->
    unit =
 fun ~check_live ~check_dbg ~check_arg idx location expected result ->
  let location = Printf.sprintf "%s (index %d)" location idx in
  (* CR xclerc for xclerc: double check whether `Reg.same_loc` is enough. (note:
     `Reg.Set.equal` uses the `stamp` fields) *)
  if check_arg && not (array_equal Reg.same_loc expected.arg result.arg)
  then different location "input registers";
  if not (array_equal Reg.same_loc expected.res result.res)
  then different location "output registers";
  if check_dbg && not (Debuginfo.compare expected.dbg result.dbg = 0)
  then different location "debug info";
  if not (Fdo_info.equal expected.fdo result.fdo)
  then different location "FDO info";
  if check_live && not (Reg.Set.equal expected.live result.live)
  then different location "live register set";
  if is_valid_stack_offset expected.stack_offset
     && is_valid_stack_offset result.stack_offset
     && not (Int.equal expected.stack_offset result.stack_offset)
  then different location "stack offset";
  (* note: not comparing `id` fields on purpose *)
  ()

let check_basic_instruction :
    State.t ->
    location ->
    int ->
    Cfg.basic Cfg.instruction ->
    Cfg.basic Cfg.instruction ->
    unit =
 fun state location idx expected result ->
  check_basic state location expected.desc result.desc;
  let check_dbg =
    match expected.desc with Prologue -> false | _ -> true
    [@@ocaml.warning "-4"]
  in
  let check_live =
    match result.desc with
    | Op _ -> true
    | Reloadretaddr -> true
    | Pushtrap _ -> false
    | Poptrap -> false
    | Prologue -> false
    | Stack_check _ -> false
  in
  check_instruction ~check_live ~check_dbg ~check_arg:true idx location expected
    result

let check_basic_instruction_list :
    State.t ->
    location ->
    Cfg.basic_instruction_list ->
    Cfg.basic_instruction_list ->
    unit =
 fun state location expected result ->
  let expected_len = DLL.length expected in
  let result_len = DLL.length result in
  if expected_len = result_len
  then
    let i = ref 0 in
    DLL.iter2 expected result ~f:(fun expected result ->
        check_basic_instruction state location !i expected result;
        incr i)
  else if expected_len > result_len
  then different location "bodies with different sizes (expected is longer)"
  else different location "bodies with different sizes (result is longer)"

let check_terminator_instruction :
    State.t ->
    location ->
    Cfg.terminator Cfg.instruction ->
    Cfg.terminator Cfg.instruction ->
    unit =
 fun state location expected result ->
  (match expected.desc, result.desc with
  | Never, Never -> ()
  | Always lbl1, Always lbl2 -> State.add_to_explore state lbl1 lbl2
  | ( Parity_test { ifso = ifso1; ifnot = ifnot1 },
      Parity_test { ifso = ifso2; ifnot = ifnot2 } ) ->
    State.add_to_explore state ifso1 ifso2;
    State.add_to_explore state ifnot1 ifnot2
  | ( Truth_test { ifso = ifso1; ifnot = ifnot1 },
      Truth_test { ifso = ifso2; ifnot = ifnot2 } ) ->
    State.add_to_explore state ifso1 ifso2;
    State.add_to_explore state ifnot1 ifnot2
  | ( Float_test { width = w1; lt = lt1; eq = eq1; gt = gt1; uo = uo1 },
      Float_test { width = w2; lt = lt2; eq = eq2; gt = gt2; uo = uo2 } )
    when Cmm.equal_float_width w1 w2 ->
    State.add_to_explore state lt1 lt2;
    State.add_to_explore state eq1 eq2;
    State.add_to_explore state gt1 gt2;
    State.add_to_explore state uo1 uo2
  | ( Int_test
        { lt = lt1; eq = eq1; gt = gt1; is_signed = is_signed1; imm = imm1 },
      Int_test
        { lt = lt2; eq = eq2; gt = gt2; is_signed = is_signed2; imm = imm2 } )
    when Bool.equal is_signed1 is_signed2 && Option.equal Int.equal imm1 imm2 ->
    State.add_to_explore state lt1 lt2;
    State.add_to_explore state eq1 eq2;
    State.add_to_explore state gt1 gt2
  (* The following case is morally the same as the previous one, with a
     immediate which is off by one. *)
  | ( Int_test
        { lt = lt1;
          eq = eq1;
          gt = gt1;
          is_signed = is_signed1;
          imm = Some imm1
        },
      Int_test
        { lt = lt2;
          eq = eq2;
          gt = gt2;
          is_signed = is_signed2;
          imm = Some imm2
        } )
    when Bool.equal is_signed1 is_signed2
         && Int.equal imm1 (Int.pred imm2)
         && Label.equal lt1 eq1 && Label.equal eq2 gt2 ->
    State.add_to_explore state lt1 lt2;
    State.add_to_explore state gt1 gt2
  | Switch a1, Switch a2 when Array.length a1 = Array.length a2 ->
    Array.iter2 (fun l1 l2 -> State.add_to_explore state l1 l2) a1 a2
  | Return, Return -> ()
  | Raise rk1, Raise rk2 when equal_raise_kind rk1 rk2 -> ()
  | ( Tailcall_self { destination = expected_destination },
      Tailcall_self { destination = result_destination } ) ->
    let location = location ^ " (terminator)" in
    State.add_labels_to_check state location expected_destination
      result_destination
  | Tailcall_func tc1, Tailcall_func tc2 ->
    let location = location ^ " (terminator)" in
    check_func_call_operation location tc1 tc2
  | Call_no_return cn1, Call_no_return cn2 ->
    check_external_call_operation location cn1 cn2
  | Call { op = cn1; label_after = lbl1 }, Call { op = cn2; label_after = lbl2 }
    ->
    check_func_call_operation location cn1 cn2;
    State.add_to_explore state lbl1 lbl2
  | Prim { op = cn1; label_after = lbl1 }, Prim { op = cn2; label_after = lbl2 }
    ->
    check_prim_call_operation location cn1 cn2;
    State.add_to_explore state lbl1 lbl2
  | ( Specific_can_raise { op = op1; label_after = lbl1 },
      Specific_can_raise { op = op2; label_after = lbl2 } )
    when Arch.equal_specific_operation op1 op2 ->
    State.add_to_explore state lbl1 lbl2
  | _ -> different location "terminator");
  (* CR xclerc for xclerc: temporary, for testing *)
  let check_arg =
    match expected.desc with
    | Always _ -> false
    | Never | Parity_test _ | Truth_test _ | Float_test _ | Int_test _
    | Switch _ | Return | Raise _ | Tailcall_self _ | Tailcall_func _
    | Call_no_return _ | Call _ | Prim _ | Specific_can_raise _ ->
      true
  in
  check_instruction ~check_live:false ~check_dbg:false ~check_arg (-1) location
    expected result
 [@@ocaml.warning "-4"]

let check_basic_block : State.t -> Cfg.basic_block -> Cfg.basic_block -> unit =
 fun state expected result ->
  let location =
    Printf.sprintf "block %s/%s"
      (Label.to_string expected.start)
      (Label.to_string result.start)
  in
  check_basic_instruction_list state location expected.body result.body;
  check_terminator_instruction state location expected.terminator
    result.terminator;
  (* State.add_label_sets_to_check state (location ^ " (predecessors)")
     expected.predecessors result.predecessors; *)
  if is_valid_stack_offset expected.stack_offset
     && is_valid_stack_offset result.stack_offset
  then (
    if not (Int.equal expected.stack_offset result.stack_offset)
    then
      different location
        (Printf.sprintf "stack offset: expected=%d result=%d"
           expected.stack_offset result.stack_offset);
    let location = location ^ " (exceptional successors)" in
    match expected.exn, result.exn with
    | None, None -> ()
    | None, Some _ -> different location "unexpected successor"
    | Some _, None -> different location "missing successor"
    | Some expected, Some result ->
      State.add_labels_to_check state location expected result);
  if not (Bool.equal expected.can_raise result.can_raise)
  then different location "can_raise";
  if not (Bool.equal expected.is_trap_handler result.is_trap_handler)
  then different location "is_trap_handler";
  if not (Bool.equal expected.dead result.dead) then different location "dead"

let rec explore_cfg : State.t -> Cfg.t -> Cfg.t -> unit =
 fun state expected result ->
  match State.to_explore state with
  | None -> ()
  | Some (lbl1, lbl2) ->
    (if not (State.has_seen state lbl1)
    then
      let expected_block = Label.Tbl.find_opt expected.blocks lbl1 in
      let result_block = Label.Tbl.find_opt result.blocks lbl2 in
      match expected_block, result_block with
      | None, None -> assert false
      | None, Some _ -> different "graph" "extra block"
      | Some _, None -> different "graph" "missing block"
      | Some expected_block, Some result_block ->
        State.add_seen state lbl1;
        check_basic_block state expected_block result_block);
    explore_cfg state expected result

let check_cfg : State.t -> Cfg.t -> Cfg.t -> unit =
 fun state expected result ->
  let expected_num_blocks = Label.Tbl.length expected.blocks in
  let result_num_blocks = Label.Tbl.length result.blocks in
  if not (Int.equal expected_num_blocks result_num_blocks)
  then different "CFG" "number of blocks";
  if not (String.equal expected.fun_name result.fun_name)
  then different "CFG" "fun_name";
  if not (Debuginfo.compare expected.fun_dbg result.fun_dbg = 0)
  then different "CFG" "fun_dbg";
  State.add_to_explore state expected.entry_label result.entry_label;
  explore_cfg state expected result

let _check_layout : State.t -> Label.t list -> Label.t list -> unit =
 fun state expected result ->
  let expected_length = List.length expected in
  let result_length = List.length result in
  if expected_length <> result_length
  then
    different "layout sizes"
      (Printf.sprintf "expected %d, got %d" expected_length result_length);
  List.iter2
    (fun expected_label result_label ->
      State.add_labels_to_check state "layout" expected_label result_label)
    expected result

let save_cfg_as_dot : Cfg_with_layout.t -> string -> unit =
 fun cfg_with_layout msg ->
  Cfg_with_layout.save_as_dot cfg_with_layout ~show_instr:true ~show_exn:true
    ~annotate_succ:(Printf.sprintf "%d->%d") msg

let check_cfg_with_layout :
    ?mach:Mach.fundecl ->
    ?linear:Linear.fundecl ->
    Cfg_with_layout.t ->
    Cfg_with_layout.t ->
    unit =
 fun ?mach ?linear expected result ->
  try
    let state = State.make () in
    check_cfg state (Cfg_with_layout.cfg expected) (Cfg_with_layout.cfg result);
    (* CR xclerc for xclerc: re-enable check_layout state
       (Cfg_with_layout.layout expected) (Cfg_with_layout.layout result); *)
    (* CR gyorsh: consider enabling check_sections expected.sections
       result.sections *)
    State.check state (Cfg_with_layout.cfg expected);
    let num_seen = State.num_seen state in
    if not
         (Int.equal num_seen
            (Label.Tbl.length (Cfg_with_layout.cfg expected).blocks))
    then different "exploration" "partial"
  with Different { location; message } ->
    save_cfg_as_dot expected "expected";
    save_cfg_as_dot result "result";
    Option.iter (fun f -> Format.eprintf "%a\n%!" Printmach.fundecl f) mach;
    Option.iter (fun f -> Format.eprintf "%a\n%!" Printlinear.fundecl f) linear;
    Misc.fatal_errorf "Cfg_equivalence: error in %s\n  %s: %s\n"
      (Cfg.fun_name (Cfg_with_layout.cfg expected))
      location message
