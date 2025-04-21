(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Selection of pseudo-instructions, assignment of pseudo-registers,
   sequentialization. *)

open! Int_replace_polymorphic_compare

[@@@ocaml.warning "+a-4-40-41-42"]

open Cmm
module DLL = Flambda_backend_utils.Doubly_linked_list
module Int = Numbers.Int
module V = Backend_var
module VP = Backend_var.With_provenance

module Or_never_returns = struct
  type 'a t =
    | Ok of 'a
    | Never_returns

  module Syntax = struct
    let ( let* ) x f =
      match x with Never_returns -> Never_returns | Ok x -> f x

    let ( let** ) x f = match x with Never_returns -> () | Ok x -> f x
  end
end

type trap_stack_info =
  | Unreachable
  | Reachable of Operation.trap_stack

type static_handler =
  { regs : Reg.t array list;
    traps_ref : trap_stack_info ref;
    label : Label.t
  }

type environment =
  { vars :
      (Reg.t array * Backend_var.Provenance.t option * Asttypes.mutable_flag)
      V.Map.t;
    static_exceptions : static_handler Int.Map.t;
        (** Which registers must be populated when jumping to the given
        handler. *)
    trap_stack : Operation.trap_stack;
    tailrec_label : Label.t
  }

let env_add ?(mut = Asttypes.Immutable) var regs env =
  let provenance = VP.provenance var in
  let var = VP.var var in
  { env with vars = V.Map.add var (regs, provenance, mut) env.vars }

let env_add_static_exception id v env label =
  let r = ref Unreachable in
  let s : static_handler = { regs = v; traps_ref = r; label } in
  { env with static_exceptions = Int.Map.add id s env.static_exceptions }, r

let env_find id env =
  let regs, _provenance, _mut = V.Map.find id env.vars in
  regs

let env_find_mut id env =
  let regs, provenance, mut = V.Map.find id env.vars in
  (match mut with
  | Asttypes.Mutable -> ()
  | Asttypes.Immutable ->
    Misc.fatal_errorf "Selectgen.env_find_mut: %a is not mutable" V.print id);
  regs, provenance

let env_find_regs_for_exception_extra_args id env =
  match Int.Map.find id env.static_exceptions with
  | { regs = _exn :: extra_args; _ } -> extra_args
  | { regs = []; _ } ->
    Misc.fatal_errorf "Exception handler for continuation %d has no parameters"
      id
  | exception Not_found ->
    Misc.fatal_errorf
      "Could not find exception extra args registers for continuation %d" id

let env_find_static_exception id env =
  try Int.Map.find id env.static_exceptions
  with Not_found -> Misc.fatal_errorf "Not found static exception id=%d" id

let env_set_trap_stack env trap_stack = { env with trap_stack }

let rec combine_traps trap_stack = function
  | [] -> trap_stack
  | Push t :: l -> combine_traps (Operation.Specific_trap (t, trap_stack)) l
  | Pop _ :: l -> (
    match (trap_stack : Operation.trap_stack) with
    | Uncaught -> Misc.fatal_error "Trying to pop a trap from an empty stack"
    | Specific_trap (_, ts) -> combine_traps ts l)

let print_traps ppf traps =
  let rec print_traps ppf = function
    | Operation.Uncaught -> Format.fprintf ppf "T"
    | Operation.Specific_trap (lbl, ts) ->
      Format.fprintf ppf "%d::%a" lbl print_traps ts
  in
  Format.fprintf ppf "(%a)" print_traps traps

let set_traps nfail traps_ref base_traps exit_traps =
  let traps = combine_traps base_traps exit_traps in
  match !traps_ref with
  | Unreachable ->
    (* Format.eprintf "Traps for %d set to %a@." nfail print_traps traps; *)
    traps_ref := Reachable traps
  | Reachable prev_traps ->
    if not (Operation.equal_trap_stack prev_traps traps)
    then
      Misc.fatal_errorf
        "Mismatching trap stacks for continuation %d@.Previous traps: %a@.New \
         traps: %a"
        nfail print_traps prev_traps print_traps traps
    else ()

let set_traps_for_raise env =
  let ts = env.trap_stack in
  match ts with
  | Uncaught -> ()
  | Specific_trap (lbl, _) -> (
    match env_find_static_exception lbl env with
    | s -> set_traps lbl s.traps_ref ts [Pop lbl]
    | exception Not_found ->
      Misc.fatal_errorf "Trap %d not registered in env" lbl)

let trap_stack_is_empty env =
  match env.trap_stack with Uncaught -> true | Specific_trap _ -> false

let pop_all_traps env =
  let rec pop_all acc = function
    | Operation.Uncaught -> acc
    | Operation.Specific_trap (lbl, t) -> pop_all (Pop lbl :: acc) t
  in
  pop_all [] env.trap_stack

let env_create ~tailrec_label =
  { vars = V.Map.empty;
    static_exceptions = Int.Map.empty;
    trap_stack = Uncaught;
    tailrec_label
  }

let select_mutable_flag : Asttypes.mutable_flag -> Operation.mutable_flag =
  function
  | Immutable -> Immutable
  | Mutable -> Mutable

(* Infer the type of the result of an operation *)

let oper_result_type = function
  | Capply (ty, _) -> ty
  | Cextcall { ty; ty_args = _; alloc = _; func = _; _ } -> ty
  | Cload { memory_chunk; _ } -> (
    match memory_chunk with
    | Word_val -> typ_val
    | Single { reg = Float64 } | Double -> typ_float
    | Single { reg = Float32 } -> typ_float32
    | Onetwentyeight_aligned | Onetwentyeight_unaligned -> typ_vec128
    | _ -> typ_int)
  | Calloc _ -> typ_val
  | Cstore (_c, _) -> typ_void
  | Cdls_get -> typ_val
  | Cprefetch _ -> typ_void
  | Catomic
      { op = Fetch_and_add | Compare_set | Exchange | Compare_exchange; _ } ->
    typ_int
  | Catomic { op = Add | Sub | Land | Lor | Lxor; _ } -> typ_void
  | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand | Cor | Cxor | Clsl
  | Clsr | Casr | Cclz _ | Cctz _ | Cpopcnt | Cbswap _ | Ccmpi _ | Ccmpa _
  | Ccmpf _ ->
    typ_int
  | Caddv -> typ_val
  | Cadda -> typ_addr
  | Cnegf Float64
  | Cabsf Float64
  | Caddf Float64
  | Csubf Float64
  | Cmulf Float64
  | Cdivf Float64 ->
    typ_float
  | Cnegf Float32
  | Cabsf Float32
  | Caddf Float32
  | Csubf Float32
  | Cmulf Float32
  | Cdivf Float32 ->
    typ_float32
  | Cpackf32 -> typ_float
  | Ccsel ty -> ty
  | Creinterpret_cast Value_of_int -> typ_val
  | Creinterpret_cast V128_of_v128 -> typ_vec128
  | Creinterpret_cast (Float_of_int64 | Float_of_float32) -> typ_float
  | Creinterpret_cast (Float32_of_int32 | Float32_of_float) -> typ_float32
  | Creinterpret_cast (Int_of_value | Int64_of_float | Int32_of_float32) ->
    typ_int
  | Cstatic_cast (Float_of_float32 | Float_of_int Float64) -> typ_float
  | Cstatic_cast (Float32_of_float | Float_of_int Float32) -> typ_float32
  | Cstatic_cast (Int_of_float (Float64 | Float32)) -> typ_int
  | Cstatic_cast (V128_of_scalar _) -> typ_vec128
  | Cstatic_cast (Scalar_of_v128 Float64x2) -> typ_float
  | Cstatic_cast (Scalar_of_v128 Float32x4) -> typ_float32
  | Cstatic_cast (Scalar_of_v128 (Int8x16 | Int16x8 | Int32x4 | Int64x2)) ->
    typ_int
  | Craise _ -> typ_void
  | Cprobe _ -> typ_void
  | Cprobe_is_enabled _ -> typ_int
  | Copaque -> typ_val
  | Cpoll -> typ_void
  | Cbeginregion ->
    (* This must not be typ_val; the begin-region operation returns a naked
       pointer into the local allocation stack. *)
    typ_int
  | Cendregion -> typ_void
  | Ctuple_field (field, fields_ty) -> fields_ty.(field)

(* Infer the size in bytes of the result of an expression whose evaluation may
   be deferred (cf. [emit_parts]). *)

(* [size_component] is placed here and not in [Cmm] to avoid cyclic
   dependencies, because it uses [Arch]. *)
let size_component : machtype_component -> int = function
  | Val | Addr -> Arch.size_addr
  | Int ->
    assert (Int.equal Arch.size_int Arch.size_addr);
    Arch.size_int
  | Float -> Arch.size_float
  | Float32 ->
    (* CR layouts v5.1: reconsider when float32 fields are efficiently packed.
       Note that packed float32# arrays are handled via a separate path. *)
    Arch.size_float
  | Vec128 -> Arch.size_vec128
  | Valx2 ->
    assert (Int.equal (Arch.size_addr * 2) Arch.size_vec128);
    Arch.size_vec128

let size_machtype mty =
  let size = ref 0 in
  for i = 0 to Array.length mty - 1 do
    size := !size + size_component mty.(i)
  done;
  !size

let size_expr env exp =
  let rec size localenv = function
    | Cconst_int _ | Cconst_natint _ -> Arch.size_int
    | Cconst_symbol _ -> Arch.size_addr
    | Cconst_float _ -> Arch.size_float
    | Cconst_float32 _ ->
      (* CR layouts v5.1: reconsider when float32 fields are efficiently packed.
         Note that packed float32# arrays are handled via a separate path. *)
      Arch.size_float
    | Cconst_vec128 _ -> Arch.size_vec128
    | Cvar id -> (
      try V.Map.find id localenv
      with Not_found -> (
        try
          let regs = env_find id env in
          size_machtype (Array.map (fun r -> r.Reg.typ) regs)
        with Not_found ->
          Misc.fatal_error
            ("Selection.size_expr: unbound var " ^ V.unique_name id)))
    | Ctuple el -> List.fold_right (fun e sz -> size localenv e + sz) el 0
    | Cop (op, _, _) -> size_machtype (oper_result_type op)
    | Clet (id, arg, body) ->
      size (V.Map.add (VP.var id) (size localenv arg) localenv) body
    | Csequence (_e1, e2) -> size localenv e2
    | _ -> Misc.fatal_error "Selection.size_expr"
  in
  size V.Map.empty exp

(* Swap the two arguments of an integer comparison *)

let swap_intcomp = function
  | Operation.Isigned cmp -> Operation.Isigned (swap_integer_comparison cmp)
  | Operation.Iunsigned cmp -> Operation.Iunsigned (swap_integer_comparison cmp)

(* Name of function being compiled *)
let current_function_name = ref ""

let current_function_is_check_enabled = ref false

module Effect = struct
  type t =
    | None
    | Raise
    | Arbitrary

  let join t1 t2 =
    match t1, t2 with
    | None, t2 -> t2
    | t1, None -> t1
    | Raise, Raise -> Raise
    | Arbitrary, _ | _, Arbitrary -> Arbitrary

  let pure = function None -> true | Raise | Arbitrary -> false
end

module Coeffect = struct
  type t =
    | None
    | Read_mutable
    | Arbitrary

  let join t1 t2 =
    match t1, t2 with
    | None, t2 -> t2
    | t1, None -> t1
    | Read_mutable, Read_mutable -> Read_mutable
    | Arbitrary, _ | _, Arbitrary -> Arbitrary

  let copure = function None -> true | Read_mutable | Arbitrary -> false
end

module Effect_and_coeffect : sig
  type t

  val none : t

  val arbitrary : t

  val effect : t -> Effect.t

  val coeffect : t -> Coeffect.t

  val pure_and_copure : t -> bool

  val effect_only : Effect.t -> t

  val coeffect_only : Coeffect.t -> t

  val create : Effect.t -> Coeffect.t -> t

  val join : t -> t -> t

  val join_list_map : 'a list -> ('a -> t) -> t
end = struct
  type t = Effect.t * Coeffect.t

  let none = Effect.None, Coeffect.None

  let arbitrary = Effect.Arbitrary, Coeffect.Arbitrary

  let effect (e, _ce) = e

  let coeffect (_e, ce) = ce

  let pure_and_copure (e, ce) = Effect.pure e && Coeffect.copure ce

  let effect_only e = e, Coeffect.None

  let coeffect_only ce = Effect.None, ce

  let create e ce = e, ce

  let join (e1, ce1) (e2, ce2) = Effect.join e1 e2, Coeffect.join ce1 ce2

  let join_list_map xs f =
    match xs with
    | [] -> none
    | x :: xs -> List.fold_left (fun acc x -> join acc (f x)) (f x) xs
end

let select_effects (e : Cmm.effects) : Effect.t =
  match e with No_effects -> None | Arbitrary_effects -> Arbitrary

let select_coeffects (e : Cmm.coeffects) : Coeffect.t =
  match e with No_coeffects -> None | Has_coeffects -> Arbitrary

let debug = false

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
    Operation.test ->
    label_false:Label.t ->
    label_true:Label.t ->
    Cfg.terminator =
 fun test ~label_false ~label_true ->
  let int_test comparison immediate =
    let signed, comparison =
      match comparison with
      | Operation.Isigned comparison -> true, comparison
      | Operation.Iunsigned comparison -> false, comparison
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
    assert (instr.stack_offset = Cfg.invalid_stack_offset);
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
      Misc.fatal_errorf
        "Cfgize.Stack_offset_and_exn.process_terminator: unexpected handler on \
         self tailcall (id=%a)"
        InstructionId.format term.id
    | Never | Always _ | Parity_test _ | Truth_test _ | Float_test _
    | Int_test _ | Switch _ | Return | Raise _ | Tailcall_self _
    | Tailcall_func _ | Call_no_return _ | Call _ | Prim _ ->
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
    | Poptrap _ -> (
      match traps with
      | [] ->
        Misc.fatal_errorf
          "Cfgize.Stack_offset_and_exn.process_basic: trying to pop from an \
           empty stack (id=%a)"
          InstructionId.format instr.id
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
      if block.stack_offset = Cfg.invalid_stack_offset
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
   fun cfg -> update_block cfg cfg.entry_label ~stack_offset:0 ~traps:[]
end

let make_stack_offset stack_ofs = Cfg.Op (Stackoffset stack_ofs)

let make_name_for_debugger ~ident ~which_parameter ~provenance ~is_assignment
    ~regs =
  Cfg.Op
    (Operation.Name_for_debugger
       { ident; which_parameter; provenance; is_assignment; regs })

let make_const_int x = Operation.Const_int x

let make_const_float32 x = Operation.Const_float32 x

let make_const_float x = Operation.Const_float x

let make_const_vec128 x = Operation.Const_vec128 x

let make_const_symbol x = Operation.Const_symbol x

let make_opaque () = Operation.Opaque

let insert_debug (_env : environment) sub_cfg basic dbg arg res =
  Sub_cfg.add_instruction sub_cfg basic arg res dbg

let insert_op_debug_returning_id (_env : environment) sub_cfg op dbg arg res =
  let instr = Cfg.make_instr (Cfg.Op op) arg res dbg in
  Sub_cfg.add_instruction' sub_cfg instr;
  instr.id

let insert (_env : environment) sub_cfg basic arg res =
  (* CR mshinwell: fix debuginfo *)
  Sub_cfg.add_instruction sub_cfg basic arg res Debuginfo.none

let insert' (_env : environment) sub_cfg term arg res =
  (* CR mshinwell: fix debuginfo *)
  Sub_cfg.set_terminator sub_cfg term arg res Debuginfo.none

let insert_debug' (_env : environment) sub_cfg basic dbg arg res =
  Sub_cfg.set_terminator sub_cfg basic arg res dbg

let insert_op_debug' (_env : environment) sub_cfg op dbg rs rd =
  Sub_cfg.set_terminator sub_cfg op rs rd dbg;
  rd

let insert_move env sub_cfg src dst =
  (* This should never be called on regs with incompatible [typ]s, but the
     zero-alloc checker may unconditionally assume the result of
     caml_flambda2_invalid is typ_int. *)
  if not (Reg.same src dst)
  then insert env sub_cfg (Op Move) [| src |] [| dst |]

let insert_moves env sub_cfg src dst =
  for i = 0 to min (Array.length src) (Array.length dst) - 1 do
    insert_move env sub_cfg src.(i) dst.(i)
  done

(* Insert moves and stack offsets for function arguments and results *)

let insert_move_args env sub_cfg arg loc stacksize =
  if stacksize <> 0
  then insert env sub_cfg (make_stack_offset stacksize) [||] [||];
  insert_moves env sub_cfg arg loc

let insert_move_results env sub_cfg loc res stacksize =
  insert_moves env sub_cfg loc res;
  if stacksize <> 0
  then insert env sub_cfg (make_stack_offset (-stacksize)) [||] [||]

let maybe_emit_naming_op env sub_cfg ~bound_name regs =
  match bound_name with
  | None -> ()
  | Some bound_name ->
    let provenance = Backend_var.With_provenance.provenance bound_name in
    if Option.is_some provenance
    then
      let bound_name = Backend_var.With_provenance.var bound_name in
      let naming_op =
        Operation.Name_for_debugger
          { ident = bound_name;
            provenance;
            which_parameter = None;
            is_assignment = false;
            regs
          }
      in
      insert_debug env sub_cfg (Cfg.Op naming_op) Debuginfo.none [||] [||]

let join env (opt_r1 : _ Or_never_returns.t) sub_cfg1
    (opt_r2 : _ Or_never_returns.t) sub_cfg2 ~bound_name : _ Or_never_returns.t
    =
  let maybe_emit_naming_op sub_cfg =
    maybe_emit_naming_op env sub_cfg ~bound_name
  in
  match opt_r1, opt_r2 with
  | Never_returns, _ -> opt_r2
  | _, Never_returns -> opt_r1
  | Ok r1, Ok r2 ->
    let l1 = Array.length r1 in
    assert (l1 = Array.length r2);
    let r = Array.make l1 Reg.dummy in
    for i = 0 to l1 - 1 do
      let typ = Cmm.lub_component r1.(i).Reg.typ r2.(i).Reg.typ in
      r.(i) <- Reg.create typ;
      insert_move env sub_cfg1 r1.(i) r.(i);
      maybe_emit_naming_op sub_cfg1 [| r.(i) |];
      insert_move env sub_cfg2 r2.(i) r.(i);
      maybe_emit_naming_op sub_cfg2 [| r.(i) |]
    done;
    Ok r

let join_array env rs ~bound_name : _ Or_never_returns.t =
  let maybe_emit_naming_op sub_cfg =
    maybe_emit_naming_op env sub_cfg ~bound_name
  in
  let some_res = ref Or_never_returns.Never_returns in
  for i = 0 to Array.length rs - 1 do
    let r, _ = rs.(i) in
    match (r : _ Or_never_returns.t) with
    | Never_returns -> ()
    | Ok r -> (
      match !some_res with
      | Never_returns ->
        some_res := Or_never_returns.Ok (r, Array.map (fun r -> r.Reg.typ) r)
      | Ok (r', types) ->
        let types =
          Array.map2 (fun r typ -> Cmm.lub_component r.Reg.typ typ) r types
        in
        some_res := Or_never_returns.Ok (r', types))
  done;
  match !some_res with
  | Never_returns -> Never_returns
  | Ok (template, types) ->
    let size_res = Array.length template in
    let res = Array.make size_res Reg.dummy in
    for i = 0 to size_res - 1 do
      res.(i) <- Reg.create types.(i)
    done;
    for i = 0 to Array.length rs - 1 do
      let r, sub_cfg = rs.(i) in
      match (r : _ Or_never_returns.t) with
      | Never_returns -> ()
      | Ok r ->
        insert_moves env sub_cfg r res;
        maybe_emit_naming_op sub_cfg res
    done;
    Ok res

let basic_op op : Cfg.basic_or_terminator = Basic (Op op)
