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

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Cmm
module Int = Numbers.Int
module V = Backend_var
module VP = Backend_var.With_provenance

type trap_stack_info =
  | Unreachable
  | Reachable of Mach.trap_stack

type 'a static_handler =
  { regs : Reg.t array list;
    traps_ref : trap_stack_info ref;
    extra : 'a
  }

type 'a environment =
  { vars :
      (Reg.t array * Backend_var.Provenance.t option * Asttypes.mutable_flag)
      V.Map.t;
    static_exceptions : 'a static_handler Int.Map.t;
        (** Which registers must be populated when jumping to the given
        handler. *)
    trap_stack : Mach.trap_stack
  }

let env_add ?(mut = Asttypes.Immutable) var regs env =
  let provenance = VP.provenance var in
  let var = VP.var var in
  { env with vars = V.Map.add var (regs, provenance, mut) env.vars }

let env_add_static_exception id v env extra =
  let r = ref Unreachable in
  let s : _ static_handler = { regs = v; traps_ref = r; extra } in
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

let _env_find_with_provenance id env = V.Map.find id env.vars

let env_find_static_exception id env = Int.Map.find id env.static_exceptions

let env_enter_trywith env id extra =
  let env, _ = env_add_static_exception id [] env extra in
  env

let env_set_trap_stack env trap_stack = { env with trap_stack }

let rec combine_traps trap_stack = function
  | [] -> trap_stack
  | Push t :: l -> combine_traps (Mach.Specific_trap (t, trap_stack)) l
  | Pop _ :: l -> (
    match (trap_stack : Mach.trap_stack) with
    | Uncaught -> Misc.fatal_error "Trying to pop a trap from an empty stack"
    | Specific_trap (_, ts) -> combine_traps ts l)

let print_traps ppf traps =
  let rec print_traps ppf = function
    | Mach.Uncaught -> Format.fprintf ppf "T"
    | Mach.Specific_trap (lbl, ts) ->
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
    if prev_traps <> traps
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
    | Mach.Uncaught -> acc
    | Mach.Specific_trap (lbl, t) -> pop_all (Pop lbl :: acc) t
  in
  pop_all [] env.trap_stack

let env_empty =
  { vars = V.Map.empty;
    static_exceptions = Int.Map.empty;
    trap_stack = Uncaught
  }

let select_mutable_flag : Asttypes.mutable_flag -> Mach.mutable_flag = function
  | Immutable -> Immutable
  | Mutable -> Mutable

(* Infer the type of the result of an operation *)

let oper_result_type = function
  | Capply (ty, _, _) -> ty
  | Cextcall { ty; ty_args = _; alloc = _; func = _ } -> ty
  | Cload { memory_chunk } -> (
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
  | Catomic _ -> typ_int
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
  | Cbeginregion ->
    (* This must not be typ_val; the begin-region operation returns a naked
       pointer into the local allocation stack. *)
    typ_int
  | Cendregion -> typ_void
  | Ctuple_field (field, fields_ty) -> fields_ty.(field)

(* Infer the size in bytes of the result of an expression whose evaluation may
   be deferred (cf. [emit_parts]). *)

let size_component : machtype_component -> int = function
  | Val | Addr -> Arch.size_addr
  | Int -> Arch.size_int
  | Float -> Arch.size_float
  | Float32 ->
    (* CR layouts v5.1: reconsider when float32 fields are efficiently packed.
       Note that packed float32# arrays are handled via a separate path. *)
    Arch.size_float
  | Vec128 -> Arch.size_vec128

let size_machtype mty =
  let size = ref 0 in
  for i = 0 to Array.length mty - 1 do
    size := !size + size_component mty.(i)
  done;
  !size

let size_expr (env : _ environment) exp =
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
  | Mach.Isigned cmp -> Mach.Isigned (swap_integer_comparison cmp)
  | Mach.Iunsigned cmp -> Mach.Iunsigned (swap_integer_comparison cmp)

(* Naming of registers *)

let all_regs_anonymous rv =
  try
    for i = 0 to Array.length rv - 1 do
      if not (Reg.anonymous rv.(i)) then raise Exit
    done;
    true
  with Exit -> false

let name_regs id rv =
  let id = VP.var id in
  if Array.length rv = 1
  then rv.(0).Reg.raw_name <- Reg.Raw_name.create_from_var id
  else
    for i = 0 to Array.length rv - 1 do
      rv.(i).Reg.raw_name <- Reg.Raw_name.create_from_var id;
      rv.(i).Reg.part <- Some i
    done

let maybe_emit_naming_op env ~bound_name seq regs =
  match bound_name with
  | None -> ()
  | Some bound_name ->
    let provenance = VP.provenance bound_name in
    if Option.is_some provenance
    then
      let bound_name = VP.var bound_name in
      let naming_op =
        Mach.Iname_for_debugger
          { ident = bound_name;
            provenance;
            which_parameter = None;
            is_assignment = false;
            regs
          }
      in
      seq#insert_debug env (Mach.Iop naming_op) Debuginfo.none [||] [||]

(* "Join" two instruction sequences, making sure they return their results in
   the same registers. *)

let join env opt_r1 seq1 opt_r2 seq2 ~bound_name =
  let maybe_emit_naming_op = maybe_emit_naming_op env ~bound_name in
  match opt_r1, opt_r2 with
  | None, _ -> opt_r2
  | _, None -> opt_r1
  | Some r1, Some r2 ->
    let l1 = Array.length r1 in
    assert (l1 = Array.length r2);
    let r = Array.make l1 Reg.dummy in
    for i = 0 to l1 - 1 do
      if Reg.anonymous r1.(i) && Cmm.ge_component r1.(i).Reg.typ r2.(i).Reg.typ
      then (
        r.(i) <- r1.(i);
        seq2#insert_move env r2.(i) r1.(i);
        maybe_emit_naming_op seq2 [| r1.(i) |])
      else if Reg.anonymous r2.(i)
              && Cmm.ge_component r2.(i).Reg.typ r1.(i).Reg.typ
      then (
        r.(i) <- r2.(i);
        seq1#insert_move env r1.(i) r2.(i);
        maybe_emit_naming_op seq1 [| r2.(i) |])
      else
        let typ = Cmm.lub_component r1.(i).Reg.typ r2.(i).Reg.typ in
        r.(i) <- Reg.create typ;
        seq1#insert_move env r1.(i) r.(i);
        maybe_emit_naming_op seq1 [| r.(i) |];
        seq2#insert_move env r2.(i) r.(i);
        maybe_emit_naming_op seq2 [| r.(i) |]
    done;
    Some r

(* Same, for N branches *)

let join_array env rs ~bound_name =
  let maybe_emit_naming_op = maybe_emit_naming_op env ~bound_name in
  let some_res = ref None in
  for i = 0 to Array.length rs - 1 do
    let r, _ = rs.(i) in
    match r with
    | None -> ()
    | Some r -> (
      match !some_res with
      | None -> some_res := Some (r, Array.map (fun r -> r.Reg.typ) r)
      | Some (r', types) ->
        let types =
          Array.map2 (fun r typ -> Cmm.lub_component r.Reg.typ typ) r types
        in
        some_res := Some (r', types))
  done;
  match !some_res with
  | None -> None
  | Some (template, types) ->
    let size_res = Array.length template in
    let res = Array.make size_res Reg.dummy in
    for i = 0 to size_res - 1 do
      res.(i) <- Reg.create types.(i)
    done;
    for i = 0 to Array.length rs - 1 do
      let r, s = rs.(i) in
      match r with
      | None -> ()
      | Some r ->
        s#insert_moves env r res;
        maybe_emit_naming_op s res
    done;
    Some res

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

class virtual ['env, 'op, 'instr] common_selector =
  object (self : 'self)
    method virtual is_store : 'op -> bool

    method virtual lift_op : 'op -> 'instr

    method virtual make_store
        : Cmm.memory_chunk -> Arch.addressing_mode -> bool -> 'instr

    method virtual make_stack_offset : int -> 'instr

    method virtual make_name_for_debugger
        : ident:V.t ->
          which_parameter:int option ->
          provenance:V.Provenance.t option ->
          is_assignment:bool ->
          regs:Reg.t array ->
          'instr

    (* A syntactic criterion used in addition to judgements about (co)effects as
       to whether the evaluation of a given expression may be deferred by
       [emit_parts]. This criterion is a property of the instruction selection
       algorithm in this file rather than a property of the Cmm language. *)
    method is_simple_expr =
      function
      | Cconst_int _ -> true
      | Cconst_natint _ -> true
      | Cconst_float32 _ -> true
      | Cconst_float _ -> true
      | Cconst_symbol _ -> true
      | Cconst_vec128 _ -> true
      | Cvar _ -> true
      | Ctuple el -> List.for_all self#is_simple_expr el
      | Clet (_id, arg, body) | Clet_mut (_id, _, arg, body) ->
        self#is_simple_expr arg && self#is_simple_expr body
      | Cphantom_let (_var, _defining_expr, body) -> self#is_simple_expr body
      | Csequence (e1, e2) -> self#is_simple_expr e1 && self#is_simple_expr e2
      | Cop (op, args, _) -> (
        match op with
        (* Cextcall with neither effects nor coeffects is simple if its
           arguments are *)
        | Cextcall { effects = No_effects; coeffects = No_coeffects } ->
          List.for_all self#is_simple_expr args
          (* The following may have side effects *)
        | Capply _ | Cextcall _ | Calloc _ | Cstore _ | Craise _ | Catomic _
        | Cprobe _ | Cprobe_is_enabled _ | Copaque ->
          false
        | Cprefetch _ | Cbeginregion | Cendregion ->
          false
          (* avoid reordering *)
          (* The remaining operations are simple if their args are *)
        | Cload _ | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand
        | Cor | Cxor | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _
        | Cnegf _ | Cclz _ | Cctz _ | Cpopcnt | Cbswap _ | Ccsel _ | Cabsf _
        | Caddf _ | Csubf _ | Cmulf _ | Cdivf _ | Cpackf32 | Creinterpret_cast _
        | Cstatic_cast _ | Ctuple_field _ | Ccmpf _ | Cdls_get ->
          List.for_all self#is_simple_expr args)
      | Cassign _ | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _
        ->
        false

    (* Analyses the effects and coeffects of an expression. This is used across
       a whole list of expressions with a view to determining which expressions
       may have their evaluation deferred. The result of this function, modulo
       target-specific judgements if the [effects_of] method is overridden, is a
       property of the Cmm language rather than anything particular about the
       instruction selection algorithm in this file.

       In the case of e.g. an OCaml function call, the arguments whose
       evaluation cannot be deferred (cf. [emit_parts], below) are computed in
       right-to-left order first with their results going into temporaries, then
       the block is allocated, then the remaining arguments are evaluated before
       being combined with the temporaries. *)
    method effects_of exp =
      let module EC = Effect_and_coeffect in
      match exp with
      | Cconst_int _ | Cconst_natint _ | Cconst_float32 _ | Cconst_float _
      | Cconst_symbol _ | Cconst_vec128 _ | Cvar _ ->
        EC.none
      | Ctuple el -> EC.join_list_map el self#effects_of
      | Clet (_id, arg, body) | Clet_mut (_id, _, arg, body) ->
        EC.join (self#effects_of arg) (self#effects_of body)
      | Cphantom_let (_var, _defining_expr, body) -> self#effects_of body
      | Csequence (e1, e2) -> EC.join (self#effects_of e1) (self#effects_of e2)
      | Cifthenelse (cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg, _kind) ->
        EC.join (self#effects_of cond)
          (EC.join (self#effects_of ifso) (self#effects_of ifnot))
      | Cop (op, args, _) ->
        let from_op =
          match op with
          | Cextcall { effects = e; coeffects = ce } ->
            EC.create (select_effects e) (select_coeffects ce)
          | Capply _ | Cprobe _ | Copaque -> EC.arbitrary
          | Calloc Alloc_heap -> EC.none
          | Calloc Alloc_local -> EC.coeffect_only Coeffect.Arbitrary
          | Cstore _ -> EC.effect_only Effect.Arbitrary
          | Cbeginregion | Cendregion -> EC.arbitrary
          | Cprefetch _ -> EC.arbitrary
          | Catomic _ -> EC.arbitrary
          | Craise _ -> EC.effect_only Effect.Raise
          | Cload { mutability = Asttypes.Immutable } -> EC.none
          | Cload { mutability = Asttypes.Mutable } | Cdls_get ->
            EC.coeffect_only Coeffect.Read_mutable
          | Cprobe_is_enabled _ -> EC.coeffect_only Coeffect.Arbitrary
          | Ctuple_field _ | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi
          | Cand | Cor | Cxor | Cbswap _ | Ccsel _ | Cclz _ | Cctz _ | Cpopcnt
          | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf _
          | Cabsf _ | Caddf _ | Csubf _ | Cmulf _ | Cdivf _ | Cpackf32
          | Creinterpret_cast _ | Cstatic_cast _ | Ccmpf _ ->
            EC.none
        in
        EC.join from_op (EC.join_list_map args self#effects_of)
      | Cassign _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _ -> EC.arbitrary

    (* Says whether an integer constant is a suitable immediate argument for the
       given integer operation *)

    method is_immediate (op : Mach.integer_operation) n =
      match op with
      | Ilsl | Ilsr | Iasr -> n >= 0 && n < Arch.size_int * 8
      | _ -> false

    (* Says whether an integer constant is a suitable immediate argument for the
       given integer test *)

    method virtual is_immediate_test : Mach.integer_comparison -> int -> bool

    (* Selection of addressing modes *)

    method virtual select_addressing
        : Cmm.memory_chunk ->
          Cmm.expression ->
          Arch.addressing_mode * Cmm.expression

    method virtual select_store
        : bool -> Arch.addressing_mode -> Cmm.expression -> 'op * Cmm.expression

    (* Instruction selection for conditionals *)

    method select_condition (arg : Cmm.expression) : Mach.test * Cmm.expression
        =
      match arg with
      | Cop (Ccmpi cmp, [arg1; Cconst_int (n, _)], _)
        when self#is_immediate_test (Isigned cmp) n ->
        Iinttest_imm (Isigned cmp, n), arg1
      | Cop (Ccmpi cmp, [Cconst_int (n, _); arg2], _)
        when self#is_immediate_test (Isigned (swap_integer_comparison cmp)) n ->
        Iinttest_imm (Isigned (swap_integer_comparison cmp), n), arg2
      | Cop (Ccmpi cmp, args, _) -> Iinttest (Isigned cmp), Ctuple args
      | Cop (Ccmpa cmp, [arg1; Cconst_int (n, _)], _)
        when self#is_immediate_test (Iunsigned cmp) n ->
        Iinttest_imm (Iunsigned cmp, n), arg1
      | Cop (Ccmpa cmp, [Cconst_int (n, _); arg2], _)
        when self#is_immediate_test (Iunsigned (swap_integer_comparison cmp)) n
        ->
        Iinttest_imm (Iunsigned (swap_integer_comparison cmp), n), arg2
      | Cop (Ccmpa cmp, args, _) -> Iinttest (Iunsigned cmp), Ctuple args
      | Cop (Ccmpf (width, cmp), args, _) ->
        Ifloattest (width, cmp), Ctuple args
      | Cop (Cand, [arg1; Cconst_int (1, _)], _) -> Ioddtest, arg1
      | _ -> Itruetest, arg

    (* Return an array of fresh registers of the given type. Normally
       implemented as Reg.createv, but some ports (e.g. Arm) can override this
       definition to store float values in pairs of integer registers. *)

    method regs_for tys = Reg.createv tys

    method virtual insert_debug
        : 'env environment ->
          'instr ->
          Debuginfo.t ->
          Reg.t array ->
          Reg.t array ->
          unit

    method virtual insert
        : 'env environment -> 'instr -> Reg.t array -> Reg.t array -> unit

    method virtual insert_move : 'env environment -> Reg.t -> Reg.t -> unit

    method insert_moves env src dst =
      for i = 0 to min (Array.length src) (Array.length dst) - 1 do
        self#insert_move env src.(i) dst.(i)
      done

    (* Insert moves and stack offsets for function arguments and results *)

    method insert_move_args env arg loc stacksize =
      if stacksize <> 0
      then self#insert env (self#make_stack_offset stacksize) [||] [||];
      self#insert_moves env arg loc

    method insert_move_results env loc res stacksize =
      self#insert_moves env loc res;
      if stacksize <> 0
      then self#insert env (self#make_stack_offset (-stacksize)) [||] [||]

    (* Add an Iop opcode. Can be overridden by processor description to insert
       moves before and after the operation, i.e. for two-address instructions,
       or instructions using dedicated registers. *)

    method insert_op_debug env op dbg rs rd =
      self#insert_debug env (self#lift_op op) dbg rs rd;
      rd

    method insert_op env op rs rd =
      self#insert_op_debug env op Debuginfo.none rs rd

    method virtual emit_expr
        : 'env environment ->
          Cmm.expression ->
          bound_name:VP.t option ->
          Reg.t array option

    method private bind_let (env : 'env environment) v r1 =
      let env =
        if all_regs_anonymous r1
        then (
          name_regs v r1;
          env_add v r1 env)
        else
          let rv = Reg.createv_like r1 in
          name_regs v rv;
          self#insert_moves env r1 rv;
          env_add v rv env
      in
      let provenance = VP.provenance v in
      (if Option.is_some provenance
      then
        let naming_op =
          self#make_name_for_debugger ~ident:(VP.var v) ~which_parameter:None
            ~provenance ~is_assignment:false ~regs:r1
        in
        self#insert_debug env naming_op Debuginfo.none [||] [||]);
      env

    method private bind_let_mut (env : 'env environment) v k r1 =
      let rv = self#regs_for k in
      name_regs v rv;
      self#insert_moves env r1 rv;
      let provenance = VP.provenance v in
      (if Option.is_some provenance
      then
        let naming_op =
          self#make_name_for_debugger ~ident:(VP.var v) ~which_parameter:None
            ~provenance:(VP.provenance v) ~is_assignment:false ~regs:r1
        in
        self#insert_debug env naming_op Debuginfo.none [||] [||]);
      env_add ~mut:Mutable v rv env

    (* The following two functions, [emit_parts] and [emit_parts_list], force
       right-to-left evaluation order as required by the Flambda [Un_anf] pass
       (and to be consistent with the bytecode compiler). *)

    method private emit_parts (env : 'env environment) ~effects_after exp =
      let module EC = Effect_and_coeffect in
      let may_defer_evaluation =
        let ec = self#effects_of exp in
        match EC.effect ec with
        | Effect.Arbitrary | Effect.Raise ->
          (* Preserve the ordering of effectful expressions by evaluating them
             early (in the correct order) and assigning their results to
             temporaries. We can avoid this in just one case: if we know that
             every [exp'] in the original expression list (cf.
             [emit_parts_list]) to be evaluated after [exp] cannot possibly
             affect the result of [exp] or depend on the result of [exp], then
             [exp] may be deferred. (Checking purity here is not enough: we need
             to check copurity too to avoid e.g. moving mutable reads earlier
             than the raising of an exception.) *)
          EC.pure_and_copure effects_after
        | Effect.None -> (
          match EC.coeffect ec with
          | Coeffect.None ->
            (* Pure expressions may be moved. *)
            true
          | Coeffect.Read_mutable -> (
            (* Read-mutable expressions may only be deferred if evaluation of
               every [exp'] (for [exp'] as in the comment above) has no effects
               "worse" (in the sense of the ordering in [Effect.t]) than raising
               an exception. *)
            match EC.effect effects_after with
            | Effect.None | Effect.Raise -> true
            | Effect.Arbitrary -> false)
          | Coeffect.Arbitrary -> (
            (* Arbitrary expressions may only be deferred if evaluation of every
               [exp'] (for [exp'] as in the comment above) has no effects. *)
            match EC.effect effects_after with
            | Effect.None -> true
            | Effect.(Arbitrary | Raise) -> false))
      in
      (* Even though some expressions may look like they can be deferred from
         the (co)effect analysis, it may be forbidden to move them. *)
      if may_defer_evaluation && self#is_simple_expr exp
      then Some (exp, env)
      else
        match self#emit_expr env exp ~bound_name:None with
        | None -> None
        | Some r ->
          if Array.length r = 0
          then Some (Ctuple [], env)
          else
            (* The normal case *)
            let id = V.create_local "bind" in
            if all_regs_anonymous r
            then
              (* r is an anonymous, unshared register; use it directly *)
              Some (Cvar id, env_add (VP.create id) r env)
            else
              (* Introduce a fresh temp to hold the result *)
              let tmp = Reg.createv_like r in
              self#insert_moves env r tmp;
              Some (Cvar id, env_add (VP.create id) tmp env)

    method private emit_parts_list (env : 'env environment) exp_list =
      let module EC = Effect_and_coeffect in
      let exp_list_right_to_left, _effect =
        (* Annotate each expression with the (co)effects that happen after it
           when the original expression list is evaluated from right to left.
           The resulting expression list has the rightmost expression first. *)
        List.fold_left
          (fun (exp_list, effects_after) exp ->
            let exp_effect = self#effects_of exp in
            (exp, effects_after) :: exp_list, EC.join exp_effect effects_after)
          ([], EC.none) exp_list
      in
      List.fold_left
        (fun results_and_env (exp, effects_after) ->
          match results_and_env with
          | None -> None
          | Some (result, env) -> (
            match self#emit_parts env exp ~effects_after with
            | None -> None
            | Some (exp_result, env) -> Some (exp_result :: result, env)))
        (Some ([], env))
        exp_list_right_to_left

    method private emit_tuple_not_flattened env exp_list =
      let rec emit_list = function
        | [] -> []
        | exp :: rem -> (
          (* Again, force right-to-left evaluation *)
          let loc_rem = emit_list rem in
          match self#emit_expr env exp ~bound_name:None with
          | None -> assert false (* should have been caught in emit_parts *)
          | Some loc_exp -> loc_exp :: loc_rem)
      in
      emit_list exp_list

    method private emit_tuple env exp_list =
      Array.concat (self#emit_tuple_not_flattened env exp_list)

    method emit_extcall_args env ty_args args =
      let args = self#emit_tuple_not_flattened env args in
      let ty_args =
        if ty_args = [] then List.map (fun _ -> XInt) args else ty_args
      in
      let locs, stack_ofs = Proc.loc_external_arguments ty_args in
      let ty_args = Array.of_list ty_args in
      if stack_ofs <> 0
      then self#insert env (self#make_stack_offset stack_ofs) [||] [||];
      List.iteri
        (fun i arg -> self#insert_move_extcall_arg env ty_args.(i) arg locs.(i))
        args;
      Array.concat (Array.to_list locs), stack_ofs

    method insert_move_extcall_arg env _ty_arg src dst =
      (* The default implementation is one or two ordinary moves. (Two in the
         case of an int64 argument on a 32-bit platform.) It can be overridden
         to use special move instructions, for example a "32-bit move"
         instruction for int32 arguments. *)
      self#insert_moves env src dst

    method emit_stores env dbg data regs_addr =
      let a =
        ref (Arch.offset_addressing Arch.identity_addressing (-Arch.size_int))
      in
      List.iter
        (fun e ->
          let op, arg = self#select_store false !a e in
          match self#emit_expr env arg ~bound_name:None with
          | None -> assert false
          | Some regs -> (
            match self#is_store op with
            | true ->
              for i = 0 to Array.length regs - 1 do
                let r = regs.(i) in
                let kind =
                  match r.Reg.typ with
                  | Float -> Double
                  | Float32 -> Single { reg = Float32 }
                  | Vec128 ->
                    (* 128-bit memory operations are default unaligned. Aligned
                       (big)array operations are handled separately via cmm. *)
                    Onetwentyeight_unaligned
                  | Val | Addr | Int -> Word_val
                in
                self#insert_debug env
                  (self#make_store kind !a false)
                  dbg
                  (Array.append [| r |] regs_addr)
                  [||];
                a := Arch.offset_addressing !a (size_component r.Reg.typ)
              done
            | false ->
              self#insert_debug env (self#lift_op op) dbg
                (Array.append regs regs_addr)
                [||];
              a := Arch.offset_addressing !a (size_expr env e)))
        data
  end
