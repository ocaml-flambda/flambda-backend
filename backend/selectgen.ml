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

open Cmm
open Reg
open Mach

module Int = Numbers.Int
module V = Backend_var
module VP = Backend_var.With_provenance

type trap_stack_info =
  | Unreachable
  | Reachable of trap_stack

type static_handler =
  { regs: Reg.t array list;
    traps_ref : trap_stack_info ref }

type environment =
  { vars : (Reg.t array
            * Backend_var.Provenance.t option
            * Asttypes.mutable_flag) V.Map.t;
    static_exceptions : static_handler Int.Map.t;
    (** Which registers must be populated when jumping to the given
        handler. *)
    trap_stack : trap_stack;
  }

let env_add ?(mut=Asttypes.Immutable) var regs env =
  let provenance = VP.provenance var in
  let var = VP.var var in
  { env with vars = V.Map.add var (regs, provenance, mut) env.vars }

let env_add_static_exception id v env =
  let r = ref Unreachable in
  let s : static_handler =
    { regs = v;
      traps_ref = r }
  in
  { env with static_exceptions = Int.Map.add id s env.static_exceptions }, r

let env_find id env =
  let regs, _provenance, _mut = V.Map.find id env.vars in
  regs

let env_find_mut id env =
  let regs, provenance, mut = V.Map.find id env.vars in
  begin match mut with
  | Asttypes.Mutable -> ()
  | Asttypes.Immutable ->
    Misc.fatal_errorf
      "Selectgen.env_find_mut: %a is not mutable"
      V.print id
  end;
  regs, provenance

let _env_find_with_provenance id env =
  V.Map.find id env.vars

let env_find_static_exception id env =
  Int.Map.find id env.static_exceptions

let env_enter_trywith env kind =
  match kind with
  | Delayed id -> let env, _ = env_add_static_exception id [] env in env

let env_set_trap_stack env trap_stack =
  { env with trap_stack; }

let rec combine_traps trap_stack = function
  | [] -> trap_stack
  | (Push t) :: l -> combine_traps (Specific_trap (t, trap_stack)) l
  | Pop _ :: l ->
      begin match trap_stack with
      | Uncaught -> Misc.fatal_error "Trying to pop a trap from an empty stack"
      | Specific_trap (_, ts) -> combine_traps ts l
      end

let print_traps ppf traps =
  let rec print_traps ppf = function
    | Uncaught -> Format.fprintf ppf "T"
    | Specific_trap (lbl, ts) -> Format.fprintf ppf "%d::%a" lbl print_traps ts
  in
  Format.fprintf ppf "(%a)" print_traps traps

let set_traps nfail traps_ref base_traps exit_traps =
  let traps = combine_traps base_traps exit_traps in
  match !traps_ref with
  | Unreachable ->
    (* Format.eprintf "Traps for %d set to %a@." nfail print_traps traps; *)
    traps_ref := Reachable traps
  | Reachable prev_traps ->
      if prev_traps <> traps then
        Misc.fatal_errorf "Mismatching trap stacks for continuation %d@.\
                           Previous traps: %a@.\
                           New traps: %a"
          nfail
          print_traps prev_traps
          print_traps traps
      else ()

let set_traps_for_raise env =
  let ts = env.trap_stack in
  match ts with
  | Uncaught -> ()
  | Specific_trap (lbl, _) ->
    begin match env_find_static_exception lbl env with
    | s -> set_traps lbl s.traps_ref ts [Pop (Pop_specific lbl)]
    | exception Not_found -> Misc.fatal_errorf "Trap %d not registered in env" lbl
    end

let trap_stack_is_empty env =
  match env.trap_stack with
  | Uncaught -> true
  | Specific_trap _ -> false

let pop_all_traps env =
  let rec pop_all acc = function
    | Uncaught -> acc
    | Specific_trap (lbl, t) -> pop_all (Cmm.Pop (Pop_specific lbl) :: acc) t
  in
  pop_all [] env.trap_stack

let env_empty = {
  vars = V.Map.empty;
  static_exceptions = Int.Map.empty;
  trap_stack = Uncaught;
}

let select_mutable_flag : Asttypes.mutable_flag -> Mach.mutable_flag = function
  | Immutable -> Immutable
  | Mutable -> Mutable

(* Infer the type of the result of an operation *)

let oper_result_type = function
  | Capply(ty, _) -> ty
  | Cextcall { ty; ty_args = _; alloc = _; func = _; } -> ty
  | Cload {memory_chunk} ->
      begin match memory_chunk with
      | Word_val -> typ_val
      | Single | Double -> typ_float
      | Onetwentyeight_aligned | Onetwentyeight_unaligned -> typ_vec128
      | _ -> typ_int
      end
  | Calloc _ -> typ_val
  | Cstore (_c, _) -> typ_void
  | Cdls_get -> typ_val
  | Cprefetch _ -> typ_void
  | Catomic _ -> typ_int
  | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi |
    Cand | Cor | Cxor | Clsl | Clsr | Casr |
    Cclz _ | Cctz _ | Cpopcnt |
    Cbswap _ |
    Ccmpi _ | Ccmpa _ | Ccmpf _ -> typ_int
  | Caddv -> typ_val
  | Cadda -> typ_addr
  | Cnegf | Cabsf | Caddf | Csubf | Cmulf | Cdivf -> typ_float
  | Ccsel ty -> ty
  | Cfloatofint -> typ_float
  | Cintoffloat -> typ_int
  | Cvalueofint -> typ_val
  | Cintofvalue -> typ_int
  | Cvectorcast Bits128 -> typ_vec128
  | Cscalarcast (V128_of_scalar _) -> typ_vec128
  | Cscalarcast (V128_to_scalar (Float64x2 | Float32x4)) -> typ_float
  | Cscalarcast (V128_to_scalar (Int8x16 | Int16x8 | Int32x4 | Int64x2)) -> typ_int
  | Craise _ -> typ_void
  | Cprobe _ -> typ_void
  | Cprobe_is_enabled _ -> typ_int
  | Copaque -> typ_val
  | Cbeginregion ->
    (* This must not be typ_val; the begin-region operation returns a
       naked pointer into the local allocation stack. *)
    typ_int
  | Cendregion -> typ_void
  | Ctuple_field (field, fields_ty) -> fields_ty.(field)

(* Infer the size in bytes of the result of an expression whose evaluation
   may be deferred (cf. [emit_parts]). *)

let size_component = function
  | Val | Addr -> Arch.size_addr
  | Int -> Arch.size_int
  | Float -> Arch.size_float
  | Vec128 -> Arch.size_vec128

let size_machtype mty =
  let size = ref 0 in
  for i = 0 to Array.length mty - 1 do
    size := !size + size_component mty.(i)
  done;
  !size

let size_expr (env:environment) exp =
  let rec size localenv = function
      Cconst_int _ | Cconst_natint _ -> Arch.size_int
    | Cconst_symbol _ ->
        Arch.size_addr
    | Cconst_float _ -> Arch.size_float
    | Cconst_vec128 _ -> Arch.size_vec128
    | Cvar id ->
        begin try
          V.Map.find id localenv
        with Not_found ->
        try
          let regs = env_find id env in
          size_machtype (Array.map (fun r -> r.typ) regs)
        with Not_found ->
          Misc.fatal_error("Selection.size_expr: unbound var " ^
                           V.unique_name id)
        end
    | Ctuple el ->
        List.fold_right (fun e sz -> size localenv e + sz) el 0
    | Cop(op, _, _) ->
        size_machtype(oper_result_type op)
    | Clet(id, arg, body) ->
        size (V.Map.add (VP.var id) (size localenv arg) localenv) body
    | Csequence(_e1, e2) ->
        size localenv e2
    | _ ->
        Misc.fatal_error "Selection.size_expr"
  in size V.Map.empty exp

(* Swap the two arguments of an integer comparison *)

let swap_intcomp = function
    Isigned cmp -> Isigned(swap_integer_comparison cmp)
  | Iunsigned cmp -> Iunsigned(swap_integer_comparison cmp)

(* Naming of registers *)

let all_regs_anonymous rv =
  try
    for i = 0 to Array.length rv - 1 do
      if not (Reg.anonymous rv.(i)) then raise Exit
    done;
    true
  with Exit ->
    false

let name_regs id rv =
  let id = VP.var id in
  if Array.length rv = 1 then
    rv.(0).raw_name <- Raw_name.create_from_var id
  else
    for i = 0 to Array.length rv - 1 do
      rv.(i).raw_name <- Raw_name.create_from_var id;
      rv.(i).part <- Some i
    done

let maybe_emit_naming_op env ~bound_name seq regs =
  match bound_name with
  | None -> ()
  | Some bound_name ->
    let provenance = VP.provenance bound_name in
    if Option.is_some provenance then (
      let bound_name = VP.var bound_name in
      let naming_op =
        Iname_for_debugger {
          ident = bound_name;
          provenance;
          which_parameter = None;
          is_assignment = false;
          regs = regs;
        }
      in
      seq#insert_debug env (Iop naming_op) Debuginfo.none [| |] [| |]
    )

(* "Join" two instruction sequences, making sure they return their results
   in the same registers. *)

let join env opt_r1 seq1 opt_r2 seq2 ~bound_name =
  let maybe_emit_naming_op = maybe_emit_naming_op env ~bound_name in
  match (opt_r1, opt_r2) with
    (None, _) -> opt_r2
  | (_, None) -> opt_r1
  | (Some r1, Some r2) ->
      let l1 = Array.length r1 in
      assert (l1 = Array.length r2);
      let r = Array.make l1 Reg.dummy in
      for i = 0 to l1-1 do
        if Reg.anonymous r1.(i)
          && Cmm.ge_component r1.(i).typ r2.(i).typ
        then begin
          r.(i) <- r1.(i);
          seq2#insert_move env r2.(i) r1.(i);
          maybe_emit_naming_op seq2 [| r1.(i) |]
        end else if Reg.anonymous r2.(i)
          && Cmm.ge_component r2.(i).typ r1.(i).typ
        then begin
          r.(i) <- r2.(i);
          seq1#insert_move env r1.(i) r2.(i);
          maybe_emit_naming_op seq1 [| r2.(i) |]
        end else begin
          let typ = Cmm.lub_component r1.(i).typ r2.(i).typ in
          r.(i) <- Reg.create typ;
          seq1#insert_move env r1.(i) r.(i);
          maybe_emit_naming_op seq1 [| r.(i) |];
          seq2#insert_move env r2.(i) r.(i);
          maybe_emit_naming_op seq2 [| r.(i) |]
        end
      done;
      Some r

(* Same, for N branches *)

let join_array env rs ~bound_name =
  let maybe_emit_naming_op = maybe_emit_naming_op env ~bound_name in
  let some_res = ref None in
  for i = 0 to Array.length rs - 1 do
    let (r, _) = rs.(i) in
    match r with
    | None -> ()
    | Some r ->
      match !some_res with
      | None ->
        some_res := Some (r, Array.map (fun r -> r.typ) r)
      | Some (r', types) ->
        let types =
          Array.map2 (fun r typ -> Cmm.lub_component r.typ typ) r types
        in
        some_res := Some (r', types)
  done;
  match !some_res with
    None -> None
  | Some (template, types) ->
      let size_res = Array.length template in
      let res = Array.make size_res Reg.dummy in
      for i = 0 to size_res - 1 do
        res.(i) <- Reg.create types.(i)
      done;
      for i = 0 to Array.length rs - 1 do
        let (r, s) = rs.(i) in
        match r with
          None -> ()
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

  let pure = function
    | None -> true
    | Raise | Arbitrary -> false
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

  let copure = function
    | None -> true
    | Read_mutable | Arbitrary -> false
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

  let join (e1, ce1) (e2, ce2) =
    Effect.join e1 e2, Coeffect.join ce1 ce2

  let join_list_map xs f =
    match xs with
    | [] -> none
    | x::xs -> List.fold_left (fun acc x -> join acc (f x)) (f x) xs
end

let select_effects (e : Cmm.effects) : Effect.t =
  match e with
  | No_effects -> None
  | Arbitrary_effects -> Arbitrary

let select_coeffects (e : Cmm.coeffects) : Coeffect.t =
  match e with
  | No_coeffects -> None
  | Has_coeffects -> Arbitrary

(* The default instruction selection class *)

class virtual selector_generic = object (self : 'self)

(* A syntactic criterion used in addition to judgements about (co)effects as
   to whether the evaluation of a given expression may be deferred by
   [emit_parts].  This criterion is a property of the instruction selection
   algorithm in this file rather than a property of the Cmm language.
*)
method is_simple_expr = function
    Cconst_int _ -> true
  | Cconst_natint _ -> true
  | Cconst_float _ -> true
  | Cconst_symbol _ -> true
  | Cconst_vec128 _ -> true
  | Cvar _ -> true
  | Ctuple el -> List.for_all self#is_simple_expr el
  | Clet(_id, arg, body) | Clet_mut(_id, _, arg, body) ->
    self#is_simple_expr arg && self#is_simple_expr body
  | Cphantom_let(_var, _defining_expr, body) -> self#is_simple_expr body
  | Csequence(e1, e2) -> self#is_simple_expr e1 && self#is_simple_expr e2
  | Cop(op, args, _) ->
      begin match op with
        (* Cextcall with neither effects nor coeffects is simple
           if its arguments are *)
      | Cextcall { effects = No_effects; coeffects = No_coeffects; } ->
        List.for_all self#is_simple_expr args
        (* The following may have side effects *)
      | Capply _ | Cextcall _ | Calloc _ | Cstore _
      | Craise _ | Catomic _
      | Cprobe _ | Cprobe_is_enabled _ | Copaque -> false
      | Cprefetch _ | Cbeginregion | Cendregion -> false (* avoid reordering *)
        (* The remaining operations are simple if their args are *)
      | Cload _ | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand | Cor
      | Cxor | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf
      | Cclz _ | Cctz _ | Cpopcnt
      | Cbswap _
      | Ccsel _
      | Cabsf | Caddf | Csubf | Cmulf | Cdivf | Cfloatofint | Cintoffloat
      | Cvectorcast _ | Cscalarcast _
      | Cvalueofint | Cintofvalue
      | Ctuple_field _
      | Ccmpf _ | Cdls_get -> List.for_all self#is_simple_expr args
      end
  | Cassign _ | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _
  | Ctrywith _ -> false

(* Analyses the effects and coeffects of an expression.  This is used across
   a whole list of expressions with a view to determining which expressions
   may have their evaluation deferred.  The result of this function, modulo
   target-specific judgements if the [effects_of] method is overridden, is a
   property of the Cmm language rather than anything particular about the
   instruction selection algorithm in this file.

   In the case of e.g. an OCaml function call, the arguments whose evaluation
   cannot be deferred (cf. [emit_parts], below) are computed in right-to-left
   order first with their results going into temporaries, then the block is
   allocated, then the remaining arguments are evaluated before being
   combined with the temporaries. *)
method effects_of exp =
  let module EC = Effect_and_coeffect in
  match exp with
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _ | Cconst_vec128 _
  | Cvar _ -> EC.none
  | Ctuple el -> EC.join_list_map el self#effects_of
  | Clet (_id, arg, body) | Clet_mut (_id, _, arg, body) ->
    EC.join (self#effects_of arg) (self#effects_of body)
  | Cphantom_let (_var, _defining_expr, body) -> self#effects_of body
  | Csequence (e1, e2) ->
    EC.join (self#effects_of e1) (self#effects_of e2)
  | Cifthenelse (cond, _ifso_dbg, ifso, _ifnot_dbg, ifnot, _dbg, _kind) ->
    EC.join (self#effects_of cond)
      (EC.join (self#effects_of ifso) (self#effects_of ifnot))
  | Cop (op, args, _) ->
    let from_op =
      match op with
      | Cextcall { effects = e; coeffects = ce; } ->
        EC.create (select_effects e) (select_coeffects ce)
      | Capply _ | Cprobe _ | Copaque -> EC.arbitrary
      | Calloc Alloc_heap -> EC.none
      | Calloc Alloc_local -> EC.coeffect_only Coeffect.Arbitrary
      | Cstore _ -> EC.effect_only Effect.Arbitrary
      | Cbeginregion | Cendregion -> EC.arbitrary
      | Cprefetch _ -> EC.arbitrary
      | Catomic _ -> EC.arbitrary
      | Craise _ -> EC.effect_only Effect.Raise
      | Cload {mutability = Asttypes.Immutable} -> EC.none
      | Cload {mutability = Asttypes.Mutable} | Cdls_get ->
        EC.coeffect_only Coeffect.Read_mutable
      | Cprobe_is_enabled _ -> EC.coeffect_only Coeffect.Arbitrary
      | Ctuple_field _
      | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand | Cor | Cxor
      | Cbswap _
      | Ccsel _
      | Cclz _ | Cctz _ | Cpopcnt
      | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf | Cabsf
      | Caddf | Csubf | Cmulf | Cdivf | Cfloatofint | Cintoffloat
      | Cvectorcast _ | Cscalarcast _
      | Cvalueofint | Cintofvalue | Ccmpf _ ->
        EC.none
    in
    EC.join from_op (EC.join_list_map args self#effects_of)
  | Cassign _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _ ->
    EC.arbitrary

(* Says whether an integer constant is a suitable immediate argument for
   the given integer operation *)

method is_immediate op n =
  match op with
  | Ilsl | Ilsr | Iasr -> n >= 0 && n < Arch.size_int * 8
  | _ -> false

(* Says whether an integer constant is a suitable immediate argument for
   the given integer test *)

method virtual is_immediate_test : integer_comparison -> int -> bool

(* Selection of addressing modes *)

method virtual select_addressing :
  Cmm.memory_chunk -> Cmm.expression -> Arch.addressing_mode * Cmm.expression

(* Default instruction selection for stores (of words) *)

method select_store is_assign addr arg =
  (Istore(Word_val, addr, is_assign), arg)

(* call marking methods, documented in selectgen.mli *)
val contains_calls = ref false

method mark_call =
  contains_calls := true

method mark_tailcall = ()

method mark_c_tailcall =
  if !Clflags.debug then contains_calls := true

method mark_instr = function
  | Iop (Icall_ind | Icall_imm _ | Iextcall _ | Iprobe _) ->
      self#mark_call
  | Iop (Itailcall_ind | Itailcall_imm _) ->
      self#mark_tailcall
  | Iop (Ialloc _) | Iop (Ipoll _) ->
      self#mark_call (* caml_alloc*, caml_garbage_collection (incl. polls) *)
  | Iraise raise_kind ->
    begin match raise_kind with
      | Lambda.Raise_notrace -> ()
      | Lambda.Raise_regular
      | Lambda.Raise_reraise ->
          (* PR#6239 *)
          (* caml_stash_backtrace; we #mark_call rather than
             #mark_c_tailcall to get a good stack backtrace *)
          self#mark_call
    end
  | Itrywith _ ->
    self#mark_call
  | _ -> ()

(* Default instruction selection for operators *)

method select_operation op args _dbg =
  match (op, args) with
  | (Capply _, Cconst_symbol (func, _dbg) :: rem) ->
    (Icall_imm { func; }, rem)
  | (Capply _, _) ->
    (Icall_ind, args)
  | (Cextcall { func; builtin = true }, _) ->
     Misc.fatal_errorf "Selection.select_operation: builtin not recognized %s"
       func ();
  | (Cextcall { func; alloc; ty; ty_args; returns; builtin = false }, _) ->
    Iextcall { func; alloc; ty_res = ty; ty_args; returns; stack_ofs = -1 }, args
  | (Cload {memory_chunk; mutability; is_atomic}, [arg]) ->
    let (addressing_mode, eloc) = self#select_addressing memory_chunk arg in
    (Iload {memory_chunk; addressing_mode; mutability; is_atomic}, [eloc])
  | (Cstore (chunk, init), [arg1; arg2]) ->
      let (addr, eloc) = self#select_addressing chunk arg1 in
      let is_assign =
        match init with
        | Initialization -> false
        | Assignment -> true
      in
      if chunk = Word_int || chunk = Word_val then begin
        let (op, newarg2) = self#select_store is_assign addr arg2 in
        (op, [newarg2; eloc])
      end else begin
        (Istore(chunk, addr, is_assign), [arg2; eloc])
        (* Inversion addr/datum in Istore *)
      end
  | (Cdls_get, _) -> Idls_get, args
  | (Calloc mode, _) -> (Ialloc {bytes = 0; dbginfo = []; mode}), args
  | (Caddi, _) -> self#select_arith_comm Iadd args
  | (Csubi, _) -> self#select_arith Isub args
  | (Cmuli, _) -> self#select_arith_comm Imul args
  | (Cmulhi { signed }, _) -> self#select_arith_comm (Imulh {signed}) args
  | (Cdivi, _) -> (Iintop Idiv, args)
  | (Cmodi, _) -> (Iintop Imod, args)
  | (Cand, _) -> self#select_arith_comm Iand args
  | (Cor, _) -> self#select_arith_comm Ior args
  | (Cxor, _) -> self#select_arith_comm Ixor args
  | (Clsl, _) -> self#select_arith Ilsl args
  | (Clsr, _) -> self#select_arith Ilsr args
  | (Casr, _) -> self#select_arith Iasr args
  | (Cclz {arg_is_non_zero}, _) -> (Iintop (Iclz{arg_is_non_zero}), args)
  | (Cctz {arg_is_non_zero}, _) -> (Iintop (Ictz{arg_is_non_zero}), args)
  | (Cpopcnt, _) -> (Iintop Ipopcnt, args)
  | (Ccmpi comp, _) -> self#select_arith_comp (Isigned comp) args
  | (Caddv, _) -> self#select_arith_comm Iadd args
  | (Cadda, _) -> self#select_arith_comm Iadd args
  | (Ccmpa comp, _) -> self#select_arith_comp (Iunsigned comp) args
  | (Ccmpf comp, _) -> (Icompf comp, args)
  | (Ccsel _, [cond; ifso; ifnot]) ->
     let (cond, earg) = self#select_condition cond in
     (Icsel cond, [ earg; ifso; ifnot ])
  | (Cnegf, _) -> (Inegf, args)
  | (Cabsf, _) -> (Iabsf, args)
  | (Caddf, _) -> (Iaddf, args)
  | (Csubf, _) -> (Isubf, args)
  | (Cmulf, _) -> (Imulf, args)
  | (Cdivf, _) -> (Idivf, args)
  | (Cfloatofint, _) -> (Ifloatofint, args)
  | (Cintoffloat, _) -> (Iintoffloat, args)
  | (Cvalueofint, _) -> (Ivalueofint, args)
  | (Cintofvalue, _) -> (Iintofvalue, args)
  | (Cvectorcast cast, _) -> (Ivectorcast cast, args)
  | (Cscalarcast cast, _) -> (Iscalarcast cast, args)
  | (Catomic {op = Fetch_and_add; size}, [src; dst]) ->
    let dst_size = match size with Word | Sixtyfour -> Word_int | Thirtytwo -> Thirtytwo_signed in
    let (addr, eloc) = self#select_addressing dst_size dst in
    (Iintop_atomic { op = Fetch_and_add; size; addr }, [src; eloc])
  | (Catomic {op = Compare_and_swap; size}, [compare_with; set_to; dst]) ->
    let dst_size = match size with Word | Sixtyfour -> Word_int | Thirtytwo -> Thirtytwo_signed in
    let (addr, eloc) = self#select_addressing dst_size dst in
    (Iintop_atomic { op = Compare_and_swap; size; addr }, [compare_with; set_to; eloc])
  | (Cprobe { name; handler_code_sym; enabled_at_init; }, _) ->
    Iprobe { name; handler_code_sym; enabled_at_init; }, args
  | (Cprobe_is_enabled {name}, _) -> Iprobe_is_enabled {name}, []
  | (Cbeginregion, _) -> Ibeginregion, []
  | (Cendregion, _) -> Iendregion, args
  | _ -> Misc.fatal_error "Selection.select_oper"

method private select_arith_comm op = function
  | [arg; Cconst_int (n, _)] when self#is_immediate op n ->
      (Iintop_imm(op, n), [arg])
  | [Cconst_int (n, _); arg] when self#is_immediate op n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method private select_arith op = function
  | [arg; Cconst_int (n, _)] when self#is_immediate op n ->
      (Iintop_imm(op, n), [arg])
  | args ->
      (Iintop op, args)

method private select_arith_comp cmp = function
  | [arg; Cconst_int (n, _)] when self#is_immediate (Icomp cmp) n ->
      (Iintop_imm(Icomp cmp, n), [arg])
  | [Cconst_int (n, _); arg]
    when self#is_immediate (Icomp(swap_intcomp cmp)) n ->
      (Iintop_imm(Icomp(swap_intcomp cmp), n), [arg])
  | args ->
      (Iintop(Icomp cmp), args)

(* Instruction selection for conditionals *)

method select_condition = function
  | Cop(Ccmpi cmp, [arg1; Cconst_int (n, _)], _)
    when self#is_immediate_test (Isigned cmp) n ->
      (Iinttest_imm(Isigned cmp, n), arg1)
  | Cop(Ccmpi cmp, [Cconst_int (n, _); arg2], _)
    when self#is_immediate_test (Isigned (swap_integer_comparison cmp)) n ->
      (Iinttest_imm(Isigned(swap_integer_comparison cmp), n), arg2)
  | Cop(Ccmpi cmp, args, _) ->
      (Iinttest(Isigned cmp), Ctuple args)
  | Cop(Ccmpa cmp, [arg1; Cconst_int (n, _)], _)
    when self#is_immediate_test (Iunsigned cmp) n ->
      (Iinttest_imm(Iunsigned cmp, n), arg1)
  | Cop(Ccmpa cmp, [Cconst_int (n, _); arg2], _)
    when self#is_immediate_test (Iunsigned (swap_integer_comparison cmp)) n ->
      (Iinttest_imm(Iunsigned(swap_integer_comparison cmp), n), arg2)
  | Cop(Ccmpa cmp, args, _) ->
      (Iinttest(Iunsigned cmp), Ctuple args)
  | Cop(Ccmpf cmp, args, _) ->
      (Ifloattest cmp, Ctuple args)
  | Cop(Cand, [arg; Cconst_int (1, _)], _) ->
      (Ioddtest, arg)
  | arg ->
      (Itruetest, arg)

(* Return an array of fresh registers of the given type.
   Normally implemented as Reg.createv, but some
   ports (e.g. Arm) can override this definition to store float values
   in pairs of integer registers. *)

method regs_for tys = Reg.createv tys

(* Buffering of instruction sequences *)

val mutable instr_seq = dummy_instr

method insert_debug _env desc dbg arg res =
  instr_seq <- instr_cons_debug desc arg res dbg instr_seq

method insert _env desc arg res =
  (* CR mshinwell: fix debuginfo *)
  instr_seq <- instr_cons_debug desc arg res Debuginfo.none instr_seq

method extract_onto o =
  let rec extract res i =
    if i == dummy_instr
      then res
      else extract {i with next = res} i.next in
    extract o instr_seq

method extract =
  self#extract_onto (end_instr ())

(* Insert a sequence of moves from one pseudoreg set to another. *)

method insert_move env src dst =
  if src.stamp <> dst.stamp then
    self#insert env (Iop Imove) [|src|] [|dst|]

method insert_moves env src dst =
  for i = 0 to min (Array.length src) (Array.length dst) - 1 do
    self#insert_move env src.(i) dst.(i)
  done

(* Insert moves and stack offsets for function arguments and results *)

method insert_move_args env arg loc stacksize =
  if stacksize <> 0 then begin
    self#insert env (Iop(Istackoffset stacksize)) [||] [||]
  end;
  self#insert_moves env arg loc

method insert_move_results env loc res stacksize =
  self#insert_moves env loc res;
  if stacksize <> 0 then begin
    self#insert env (Iop(Istackoffset(-stacksize))) [||] [||]
  end

(* Add an Iop opcode. Can be overridden by processor description
   to insert moves before and after the operation, i.e. for two-address
   instructions, or instructions using dedicated registers. *)

method insert_op_debug env op dbg rs rd =
  self#insert_debug env (Iop op) dbg rs rd;
  rd

method insert_op env op rs rd =
  self#insert_op_debug env op Debuginfo.none rs rd

(* Emit an expression.

   [bound_name] is the name that will be bound to the result of evaluating
   the expression, if such exists.  This is used for emitting debugging
   info.

   Returns:
     - [None] if the expression does not finish normally (e.g. raises)
     - [Some rs] if the expression yields a result in registers [rs] *)
method emit_expr (env:environment) exp ~bound_name =
  self#emit_expr_aux env exp ~bound_name

(* Emit an expression which may end some regions early.

   Returns:
    - [None] if the expression does not finish normally (e.g. raises)
    - [Some (rs, unclosed)] if the expression yields a result in [rs],
      having left [unclosed] (a suffix of env.regions) regions open *)
method emit_expr_aux (env:environment) exp ~bound_name : Reg.t array option =
  (* Normal case of returning a value: no regions are closed *)
  let ret res = Some res in
  match exp with
    Cconst_int (n, _dbg) ->
      let r = self#regs_for typ_int in
      ret (self#insert_op env (Iconst_int(Nativeint.of_int n)) [||] r)
  | Cconst_natint (n, _dbg) ->
      let r = self#regs_for typ_int in
      ret (self#insert_op env (Iconst_int n) [||] r)
  | Cconst_float (n, _dbg) ->
      let r = self#regs_for typ_float in
      ret (self#insert_op env (Iconst_float (Int64.bits_of_float n)) [||] r)
  | Cconst_vec128 (bits, _dbg) ->
    let r = self#regs_for typ_vec128 in
    ret (self#insert_op env (Iconst_vec128 bits) [||] r)
  | Cconst_symbol (n, _dbg) ->
      (* Cconst_symbol _ evaluates to a statically-allocated address, so its
         value fits in a typ_int register and is never changed by the GC.

         Some Cconst_symbols point to statically-allocated blocks, some of
         which may point to heap values. However, any such blocks will be
         registered in the compilation unit's global roots structure, so
         adding this register to the frame table would be redundant *)
      let r = self#regs_for typ_int in
      ret (self#insert_op env (Iconst_symbol n) [||] r)
  | Cvar v ->
      begin try
        ret (env_find v env)
      with Not_found ->
        Misc.fatal_error("Selection.emit_expr: unbound var " ^ V.unique_name v)
      end
  | Clet(v, e1, e2) ->
      begin match self#emit_expr env e1 ~bound_name:(Some v) with
        None -> None
      | Some r1 -> self#emit_expr_aux (self#bind_let env v r1) e2 ~bound_name
      end
  | Clet_mut(v, k, e1, e2) ->
      begin match self#emit_expr env e1 ~bound_name:(Some v) with
        None -> None
      | Some r1 ->
          self#emit_expr_aux (self#bind_let_mut env v k r1) e2 ~bound_name
      end
  | Cphantom_let (_var, _defining_expr, body) ->
      self#emit_expr_aux env body ~bound_name
  | Cassign(v, e1) ->
      let rv, provenance =
        try
          env_find_mut v env
        with Not_found ->
          Misc.fatal_error ("Selection.emit_expr: unbound var " ^ V.name v) in
      begin match self#emit_expr env e1 ~bound_name:None with
        None -> None
      | Some r1 -> (
          if Option.is_some provenance then (
            let naming_op =
              Iname_for_debugger {
                ident = v;
                provenance;
                which_parameter = None;
                is_assignment = true;
                regs = r1;
              }
            in
            self#insert_debug env (Iop naming_op) Debuginfo.none [| |] [| |]
          );
          self#insert_moves env r1 rv; ret [||]
        )
      end
  | Ctuple [] ->
      ret [||]
  | Ctuple exp_list ->
      begin match self#emit_parts_list env exp_list with
        None -> None
      | Some(simple_list, ext_env) ->
          ret (self#emit_tuple ext_env simple_list)
      end
  | Cop(Craise k, [arg], dbg) ->
      begin match self#emit_expr env arg ~bound_name:None with
        None -> None
      | Some r1 ->
          let rd = [|Proc.loc_exn_bucket|] in
          self#insert env (Iop Imove) r1 rd;
          self#insert_debug env  (Iraise k) dbg rd [||];
          set_traps_for_raise env;
          None
      end
  | Cop(Copaque, args, dbg) ->
      begin match self#emit_parts_list env args with
        None -> None
      | Some (simple_args, env) ->
         let rs = self#emit_tuple env simple_args in
         ret (self#insert_op_debug env Iopaque dbg rs rs)
      end
  | Cop(Ctuple_field(field, fields_layout), [arg], _dbg) ->
      begin match self#emit_expr env arg ~bound_name:None with
        None -> None
      | Some loc_exp ->
        let flat_size a =
          Array.fold_left (fun acc t -> acc + Array.length t) 0 a
        in
        assert(Array.length loc_exp = flat_size fields_layout);
        let before = Array.sub fields_layout 0 field in
        let size_before = flat_size before in
        let field_slice =
          Array.sub loc_exp size_before (Array.length fields_layout.(field))
        in
        ret field_slice
      end
  | Cop(op, args, dbg) ->
      begin match self#emit_parts_list env args with
        None -> None
      | Some(simple_args, env) ->
          let add_naming_op_for_bound_name regs =
            match bound_name with
            | None -> ()
            | Some bound_name ->
              let provenance = VP.provenance bound_name in
              if Option.is_some provenance then (
                let bound_name = VP.var bound_name in
                let naming_op =
                  Iname_for_debugger {
                    ident = bound_name;
                    provenance;
                    which_parameter = None;
                    is_assignment = false;
                    regs = regs;
                  }
                in
                self#insert_debug env (Iop naming_op) Debuginfo.none [| |] [| |]
              )
          in
          let ty = oper_result_type op in
          let (new_op, new_args) = self#select_operation op simple_args dbg in
          match new_op with
            Icall_ind ->
              let r1 = self#emit_tuple env new_args in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let rd = self#regs_for ty in
              let (loc_arg, stack_ofs_args) = Proc.loc_arguments (Reg.typv rarg) in
              let (loc_res, stack_ofs_res) = Proc.loc_results_call (Reg.typv rd) in
              let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
              self#insert_move_args env rarg loc_arg stack_ofs;
              self#insert_debug env (Iop new_op) dbg
                          (Array.append [|r1.(0)|] loc_arg) loc_res;
              (* The destination registers (as per the procedure calling
                 convention) need to be named right now, otherwise the result
                 of the function call may be unavailable in the debugger
                 immediately after the call.  *)
              add_naming_op_for_bound_name loc_res;
              self#insert_move_results env loc_res rd stack_ofs;
              set_traps_for_raise env;
              Some rd
          | Icall_imm _ ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              let (loc_arg, stack_ofs_args) = Proc.loc_arguments (Reg.typv r1) in
              let (loc_res, stack_ofs_res) = Proc.loc_results_call (Reg.typv rd) in
              let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
              self#insert_move_args env r1 loc_arg stack_ofs;
              self#insert_debug env (Iop new_op) dbg loc_arg loc_res;
              add_naming_op_for_bound_name loc_res;
              self#insert_move_results env loc_res rd stack_ofs;
              set_traps_for_raise env;
              Some rd
          | Iextcall ({ func; ty_args; returns; _} as r) ->
              let (loc_arg, stack_ofs) =
                self#emit_extcall_args env ty_args new_args in
              let keep_for_checking =
                not returns &&
                !current_function_is_check_enabled &&
                String.equal func Cmm.caml_flambda2_invalid
              in
              let returns, ty =
                if keep_for_checking then true, typ_int
                else returns, ty
              in
              let rd = self#regs_for ty in
              let loc_res =
                self#insert_op_debug env
                  (Iextcall {r with stack_ofs = stack_ofs; returns }) dbg
                  loc_arg (Proc.loc_external_results (Reg.typv rd)) in
              add_naming_op_for_bound_name loc_res;
              self#insert_move_results env loc_res rd stack_ofs;
              set_traps_for_raise env;
              if returns then ret rd else None
          | Ialloc { bytes = _; mode } ->
              let rd = self#regs_for typ_val in
              let bytes = size_expr env (Ctuple new_args) in
              assert (bytes mod Arch.size_addr = 0);
              let alloc_words = bytes / Arch.size_addr in
              let op =
                Ialloc { bytes;
                         dbginfo = [{alloc_words; alloc_dbg = dbg}];
                         mode }
              in
              self#insert_debug env (Iop op) dbg [||] rd;
              add_naming_op_for_bound_name rd;
              self#emit_stores env new_args rd;
              set_traps_for_raise env;
              ret rd
          | Iprobe _ ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              let rd = self#insert_op_debug env new_op dbg r1 rd in
              set_traps_for_raise env;
              ret rd
          | op ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              add_naming_op_for_bound_name rd;
              ret (self#insert_op_debug env op dbg r1 rd)
      end
  | Csequence(e1, e2) ->
      begin match self#emit_expr env e1 ~bound_name:None with
        None -> None
      | Some _ -> self#emit_expr_aux env e2 ~bound_name
      end
  | Cifthenelse(econd, _ifso_dbg, eif, _ifnot_dbg, eelse, dbg, _kind) ->
      let (cond, earg) = self#select_condition econd in
      begin match self#emit_expr env earg ~bound_name:None with
        None -> None
      | Some rarg ->
          let (rif, (sif : 'self)) =
            self#emit_sequence env eif ~bound_name
          in
          let (relse, (selse : 'self)) =
            self#emit_sequence env eelse ~bound_name
          in
          let r = join env rif sif relse selse ~bound_name in
          self#insert_debug env (Iifthenelse(cond, sif#extract, selse#extract))
            dbg rarg [||];
          r
      end
  | Cswitch(esel, index, ecases, dbg, _kind) ->
      begin match self#emit_expr env esel ~bound_name:None with
        None -> None
      | Some rsel ->
          let rscases =
            Array.map (fun (case, _dbg) ->
                self#emit_sequence env case ~bound_name)
              ecases
          in
          let r = join_array env rscases ~bound_name in
          self#insert_debug env
            (Iswitch(index, Array.map (fun (_, s) -> s#extract) rscases))
            dbg rsel [||];
          r
      end
  | Ccatch(_, [], e1, _) ->
      self#emit_expr_aux env e1 ~bound_name
  | Ccatch(rec_flag, handlers, body, _) ->
      let handlers =
        List.map (fun (nfail, ids, e2, dbg, is_cold) ->
            let rs =
              List.map
                (fun (id, typ) ->
                  let r = self#regs_for typ in name_regs id r; r)
                ids in
            (nfail, ids, rs, e2, dbg, is_cold))
          handlers
      in
      let env, handlers_map =
        (* Since the handlers may be recursive, and called from the body,
           the same environment is used for translating both the handlers and
           the body. *)
        List.fold_left (fun (env, map) (nfail, ids, rs, e2, dbg, is_cold) ->
            let env, r = env_add_static_exception nfail rs env in
            env, Int.Map.add nfail (r, (ids, rs, e2, dbg, is_cold)) map)
          (env, Int.Map.empty) handlers
      in
      let (r_body, s_body) = self#emit_sequence env body ~bound_name in
      let translate_one_handler nfail (traps_info, (ids, rs, e2, _dbg, is_cold)) =
        assert(List.length ids = List.length rs);
        let trap_stack =
          match !traps_info with
          | Unreachable -> assert false
          | Reachable t -> t
        in
      let ids_and_rs = List.combine ids rs in
        let new_env =
          List.fold_left (fun env ((id, _typ), r) -> env_add id r env)
            (env_set_trap_stack env trap_stack) ids_and_rs
        in
        let (r, s) =
          self#emit_sequence new_env e2 ~bound_name:None ~at_start:(fun seq ->
            List.iter (fun ((var, _typ), r) ->
                let provenance = VP.provenance var in
                if Option.is_some provenance then (
                  let var = VP.var var in
                  let naming_op =
                    Iname_for_debugger {
                      ident = var;
                      provenance;
                      which_parameter = None;
                      is_assignment = false;
                      regs = r;
                    }
                  in
                  seq#insert_debug env (Iop naming_op) Debuginfo.none [| |] [| |]
                ))
              ids_and_rs)
        in
        ((nfail, trap_stack, is_cold), (r, s))
      in
      let rec build_all_reachable_handlers ~already_built ~not_built =
        let not_built, to_build =
          Int.Map.partition (fun _n (r, _) -> !r = Unreachable) not_built
        in
        if Int.Map.is_empty to_build then already_built
        else begin
          let already_built =
            Int.Map.fold
              (fun nfail handler already_built ->
                 (translate_one_handler nfail handler) :: already_built)
              to_build already_built
          in
          build_all_reachable_handlers ~already_built ~not_built
        end
      in
      let l =
        build_all_reachable_handlers ~already_built:[] ~not_built:handlers_map
        (* Note: we're dropping unreachable handlers here *)
      in
      let a = Array.of_list ((r_body, s_body) :: List.map snd l) in
      let r = join_array env a ~bound_name in
      let aux ((nfail, ts, is_cold), (_r, s)) = (nfail, ts, s#extract, is_cold) in
      let final_trap_stack =
        match r_body with
        | Some _ -> env.trap_stack
        | None ->
          (* The body never returns, so the trap stack at the end of the catch
             is the one from the handlers *)
          begin match l with
          | [] -> (* This whole catch never returns *)
            env.trap_stack
          | ((_, ts, _), (_, _)) :: tl ->
            assert (List.for_all (fun ((_, ts', _), (_, _)) -> ts = ts') tl);
            ts
          end
      in
      self#insert env
        (Icatch (rec_flag, final_trap_stack, List.map aux l, s_body#extract))
        [||] [||];
      r
  | Cexit (lbl,args,traps) ->
      begin match self#emit_parts_list env args with
        None -> None
      | Some (simple_list, ext_env) ->
          begin match lbl with
          | Lbl nfail ->
              let src = self#emit_tuple ext_env simple_list in
              let handler =
                try env_find_static_exception nfail env
                with Not_found ->
                  Misc.fatal_error ("Selection.emit_expr: unbound label "^
                                    Stdlib.Int.to_string nfail)
              in
              (* Intermediate registers to handle cases where some
                 registers from src are present in dest *)
              let tmp_regs = Reg.createv_like src in
              (* Ccatch registers must not contain out of heap pointers *)
              Array.iter (fun reg -> assert(reg.typ <> Addr)) src;
              self#insert_moves env src tmp_regs ;
              self#insert_moves env tmp_regs (Array.concat handler.regs) ;
              self#insert env (Iexit (nfail, traps)) [||] [||];
              set_traps nfail handler.traps_ref env.trap_stack traps;
              None
          | Return_lbl ->
              begin match simple_list with
              | [expr] -> self#emit_return ext_env expr traps; None
              | [] ->
                  Misc.fatal_error
                    "Selection.emit_expr: Return without arguments"
              | _ :: _ :: _ ->
                   Misc.fatal_error
                     "Selection.emit_expr: Return with too many arguments"
              end
          end
      end
  | Ctrywith(e1, kind, v, e2, dbg, _value_kind) ->
      let env_body = env_enter_trywith env kind in
      let (r1, s1) = self#emit_sequence env_body e1 ~bound_name in
      let rv = self#regs_for typ_val in
      let with_handler env_handler e2 =
        let (r2, s2) =
          self#emit_sequence env_handler e2 ~bound_name
            ~at_start:(fun seq ->
              let provenance = VP.provenance v in
              if Option.is_some provenance then (
                let var = VP.var v in
                let naming_op =
                  Iname_for_debugger {
                    ident = var;
                    provenance;
                    which_parameter = None;
                    is_assignment = false;
                    regs = rv;
                  }
                in
                seq#insert_debug env (Iop naming_op) Debuginfo.none [| |] [| |]
              ))
        in
        let r = join env r1 s1 r2 s2 ~bound_name in
        self#insert env
          (Itrywith(s1#extract, kind,
                    (env_handler.trap_stack,
                     instr_cons_debug (Iop Imove) [|Proc.loc_exn_bucket|] rv
                       dbg
                       s2#extract)))
          [||] [||];
        r
      in
      let env = env_add v rv env in
      begin match kind with
      | Delayed lbl ->
        begin match env_find_static_exception lbl env_body with
        | { traps_ref = { contents = Reachable ts; }; _} ->
          with_handler (env_set_trap_stack env ts) e2
        | { traps_ref = { contents = Unreachable; }; _ } ->
          let unreachable =
            Cmm.(Cop ((Cload { memory_chunk = Word_int; mutability = Mutable; is_atomic = false; }),
                      [Cconst_int (0, Debuginfo.none)],
                      Debuginfo.none))
          in
          with_handler env unreachable
          (* Misc.fatal_errorf "Selection.emit_expr: \
           *                    Unreachable exception handler %d" lbl *)
        | exception Not_found ->
          Misc.fatal_errorf "Selection.emit_expr: Unbound handler %d" lbl
        end
      end

method private emit_sequence ?at_start (env:environment) exp ~bound_name
    : _ * 'self =
  let s : 'self = {< instr_seq = dummy_instr >} in
  begin match at_start with
  | None -> ()
  | Some f -> f s
  end;
  let r = s#emit_expr_aux env exp ~bound_name in
  (r, s)

method private bind_let (env:environment) v r1 =
  let env =
    if all_regs_anonymous r1 then begin
      name_regs v r1;
      env_add v r1 env
    end else begin
      let rv = Reg.createv_like r1 in
      name_regs v rv;
      self#insert_moves env r1 rv;
      env_add v rv env
    end
  in
  let provenance = VP.provenance v in
  (if Option.is_some provenance then (
    let naming_op =
      Iname_for_debugger {
        ident = VP.var v;
        which_parameter = None;
        provenance;
        is_assignment = false;
        regs = r1;
      }
    in
    self#insert_debug env (Iop naming_op) Debuginfo.none [| |] [| |]
  ));
  env

method private bind_let_mut (env:environment) v k r1 =
  let rv = self#regs_for k in
  name_regs v rv;
  self#insert_moves env r1 rv;
  let provenance = VP.provenance v in
  (if Option.is_some provenance then (
    let naming_op =
      Iname_for_debugger {
        ident = VP.var v;
        which_parameter = None;
        provenance = VP.provenance v;
        is_assignment = false;
        regs = r1;
      }
    in
    self#insert_debug env (Iop naming_op) Debuginfo.none [| |] [| |]
  ));
  env_add ~mut:Mutable v rv env

(* The following two functions, [emit_parts] and [emit_parts_list], force
   right-to-left evaluation order as required by the Flambda [Un_anf] pass
   (and to be consistent with the bytecode compiler). *)

method private emit_parts (env:environment) ~effects_after exp =
  let module EC = Effect_and_coeffect in
  let may_defer_evaluation =
    let ec = self#effects_of exp in
    match EC.effect ec with
    | Effect.Arbitrary | Effect.Raise ->
      (* Preserve the ordering of effectful expressions by evaluating them
         early (in the correct order) and assigning their results to
         temporaries.  We can avoid this in just one case: if we know that
         every [exp'] in the original expression list (cf. [emit_parts_list])
         to be evaluated after [exp] cannot possibly affect the result of
         [exp] or depend on the result of [exp], then [exp] may be deferred.
         (Checking purity here is not enough: we need to check copurity too
         to avoid e.g. moving mutable reads earlier than the raising of
         an exception.) *)
      EC.pure_and_copure effects_after
    | Effect.None ->
      match EC.coeffect ec with
      | Coeffect.None ->
        (* Pure expressions may be moved. *)
        true
      | Coeffect.Read_mutable -> begin
        (* Read-mutable expressions may only be deferred if evaluation of
           every [exp'] (for [exp'] as in the comment above) has no effects
           "worse" (in the sense of the ordering in [Effect.t]) than raising
           an exception. *)
        match EC.effect effects_after with
        | Effect.None | Effect.Raise -> true
        | Effect.Arbitrary -> false
      end
      | Coeffect.Arbitrary -> begin
        (* Arbitrary expressions may only be deferred if evaluation of
           every [exp'] (for [exp'] as in the comment above) has no effects. *)
        match EC.effect effects_after with
        | Effect.None -> true
        | Effect.Arbitrary | Effect.Raise -> false
      end
  in
  (* Even though some expressions may look like they can be deferred from
     the (co)effect analysis, it may be forbidden to move them. *)
  if may_defer_evaluation && self#is_simple_expr exp then
    Some (exp, env)
  else begin
    match self#emit_expr env exp ~bound_name:None with
      None -> None
    | Some r ->
        if Array.length r = 0 then
          Some (Ctuple [], env)
        else begin
          (* The normal case *)
          let id = V.create_local "bind" in
          if all_regs_anonymous r then
            (* r is an anonymous, unshared register; use it directly *)
            Some (Cvar id, env_add (VP.create id) r env)
          else begin
            (* Introduce a fresh temp to hold the result *)
            let tmp = Reg.createv_like r in
            self#insert_moves env r tmp;
            Some (Cvar id, env_add (VP.create id) tmp env)
          end
        end
  end

method private emit_parts_list (env:environment) exp_list =
  let module EC = Effect_and_coeffect in
  let exp_list_right_to_left, _effect =
    (* Annotate each expression with the (co)effects that happen after it
       when the original expression list is evaluated from right to left.
       The resulting expression list has the rightmost expression first. *)
    List.fold_left (fun (exp_list, effects_after) exp ->
        let exp_effect = self#effects_of exp in
        (exp, effects_after)::exp_list, EC.join exp_effect effects_after)
      ([], EC.none)
      exp_list
  in
  List.fold_left (fun results_and_env (exp, effects_after) ->
      match results_and_env with
      | None -> None
      | Some (result, env) ->
          match self#emit_parts env exp ~effects_after with
          | None -> None
          | Some (exp_result, env) -> Some (exp_result :: result, env))
    (Some ([], env))
    exp_list_right_to_left

method private emit_tuple_not_flattened env exp_list =
  let rec emit_list = function
    [] -> []
  | exp :: rem ->
      (* Again, force right-to-left evaluation *)
      let loc_rem = emit_list rem in
      match self#emit_expr env exp ~bound_name:None with
        None -> assert false  (* should have been caught in emit_parts *)
      | Some loc_exp -> loc_exp :: loc_rem
  in
  emit_list exp_list

method private emit_tuple env exp_list =
  Array.concat (self#emit_tuple_not_flattened env exp_list)

method emit_extcall_args env ty_args args =
  let args = self#emit_tuple_not_flattened env args in
  let ty_args =
    if ty_args = [] then List.map (fun _ -> XInt) args else ty_args in
  let locs, stack_ofs = Proc.loc_external_arguments ty_args in
  let ty_args = Array.of_list ty_args in
  if stack_ofs <> 0 then
    self#insert env (Iop(Istackoffset stack_ofs)) [||] [||];
  List.iteri
    (fun i arg ->
      self#insert_move_extcall_arg env ty_args.(i) arg locs.(i))
    args;
  Array.concat (Array.to_list locs), stack_ofs

method insert_move_extcall_arg env _ty_arg src dst =
  (* The default implementation is one or two ordinary moves.
     (Two in the case of an int64 argument on a 32-bit platform.)
     It can be overridden to use special move instructions,
     for example a "32-bit move" instruction for int32 arguments. *)
  self#insert_moves env src dst

method emit_stores env data regs_addr =
  let a =
    ref (Arch.offset_addressing Arch.identity_addressing (-Arch.size_int)) in
  List.iter
    (fun e ->
      let (op, arg) = self#select_store false !a e in
      match self#emit_expr env arg ~bound_name:None with
        None -> assert false
      | Some regs ->
          match op with
            Istore(_, _, _) ->
              for i = 0 to Array.length regs - 1 do
                let r = regs.(i) in
                let kind = match r.typ with
                  | Float -> Double
                  | Vec128 ->
                    (* 128-bit memory operations are default unaligned. Aligned (big)array
                       operations are handled separately via cmm. *)
                    Onetwentyeight_unaligned
                  | Val | Addr | Int ->  Word_val
                in
                self#insert env
                            (Iop(Istore(kind, !a, false)))
                            (Array.append [|r|] regs_addr) [||];
                a := Arch.offset_addressing !a (size_component r.typ)
              done
          | _ ->
              self#insert env (Iop op) (Array.append regs regs_addr) [||];
              a := Arch.offset_addressing !a (size_expr env e))
    data

(* Same, but in tail position *)


method private insert_return (env:environment) r (traps:trap_action list) =
  match r with
    None -> ()
  | Some r ->
      let loc = Proc.loc_results_return (Reg.typv r) in
      self#insert_moves env r loc;
      self#insert env (Ireturn traps) loc [||]

method private emit_return (env:environment) exp traps =
  self#insert_return env (self#emit_expr_aux env exp ~bound_name:None) traps

(* Emit an expression in tail position of a function,
   closing all regions in [env.regions] *)
method emit_tail (env:environment) exp =
  match exp with
    Clet(v, e1, e2) ->
      begin match self#emit_expr env e1 ~bound_name:None with
        None -> ()
      | Some r1 -> self#emit_tail (self#bind_let env v r1) e2
      end
  | Clet_mut (v, k, e1, e2) ->
     begin match self#emit_expr env e1 ~bound_name:None with
       None -> ()
     | Some r1 -> self#emit_tail (self#bind_let_mut env v k r1) e2
     end
  | Cphantom_let (_var, _defining_expr, body) ->
      self#emit_tail env body
  | Cop((Capply(ty, _)) as op, args, dbg) ->
      begin match self#emit_parts_list env args with
        None -> ()
      | Some(simple_args, env) ->
          let (new_op, new_args) = self#select_operation op simple_args dbg in
          match new_op with
            Icall_ind ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let (loc_arg, stack_ofs_args) = Proc.loc_arguments (Reg.typv rarg) in
              let (loc_res, stack_ofs_res) = Proc.loc_results_call (Reg.typv rd) in
              let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
              if stack_ofs = 0 && trap_stack_is_empty env then begin
                let call = Iop (Itailcall_ind) in
                self#insert_moves env rarg loc_arg;
                self#insert_debug env call dbg
                            (Array.append [|r1.(0)|] loc_arg) [||];
              end else begin
                self#insert_move_args env rarg loc_arg stack_ofs;
                self#insert_debug env (Iop new_op) dbg
                            (Array.append [|r1.(0)|] loc_arg) loc_res;
                set_traps_for_raise env;
                self#insert env (Iop(Istackoffset(-stack_ofs))) [||] [||];
                self#insert env (Ireturn (pop_all_traps env)) loc_res [||]
              end
          | Icall_imm { func; } ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              let (loc_arg, stack_ofs_args) = Proc.loc_arguments (Reg.typv r1) in
              let (loc_res, stack_ofs_res) = Proc.loc_results_call (Reg.typv rd) in
              let stack_ofs = Stdlib.Int.max stack_ofs_args stack_ofs_res in
              if stack_ofs = 0 && trap_stack_is_empty env then begin
                let call = Iop (Itailcall_imm { func; }) in
                self#insert_moves env r1 loc_arg;
                self#insert_debug env call dbg loc_arg [||];
              end else if func.sym_name = !current_function_name && trap_stack_is_empty env then begin
                let call = Iop (Itailcall_imm { func; }) in
                let loc_arg' = Proc.loc_parameters (Reg.typv r1) in
                self#insert_moves env r1 loc_arg';
                self#insert_debug env call dbg loc_arg' [||];
              end else begin
                self#insert_move_args env r1 loc_arg stack_ofs;
                self#insert_debug env (Iop new_op) dbg loc_arg loc_res;
                set_traps_for_raise env;
                self#insert env (Iop(Istackoffset(-stack_ofs))) [||] [||];
                self#insert env (Ireturn (pop_all_traps env)) loc_res [||]
              end
          | _ -> Misc.fatal_error "Selection.emit_tail"
      end
  | Csequence(e1, e2) ->
      begin match self#emit_expr env e1 ~bound_name:None with
        None -> ()
      | Some _ -> self#emit_tail env e2
      end
  | Cifthenelse(econd, _ifso_dbg, eif, _ifnot_dbg, eelse, dbg, _kind) ->
      let (cond, earg) = self#select_condition econd in
      begin match self#emit_expr env earg ~bound_name:None with
        None -> ()
      | Some rarg ->
          self#insert_debug env
                      (Iifthenelse(cond, self#emit_tail_sequence env eif,
                                         self#emit_tail_sequence env eelse))
                      dbg rarg [||]
      end
  | Cswitch(esel, index, ecases, dbg, _kind) ->
      begin match self#emit_expr env esel ~bound_name:None with
        None -> ()
      | Some rsel ->
          let cases =
            Array.map (fun (case, _dbg) -> self#emit_tail_sequence env case)
              ecases
          in
          self#insert_debug env (Iswitch (index, cases)) dbg rsel [||]
      end
  | Ccatch(_, [], e1, _) ->
      self#emit_tail env e1
  | Ccatch(rec_flag, handlers, e1, _) ->
      let handlers =
        List.map (fun (nfail, ids, e2, dbg, is_cold) ->
            let rs =
              List.map
                (fun (id, typ) ->
                  let r = self#regs_for typ in name_regs id r; r)
                ids in
            (nfail, ids, rs, e2, dbg, is_cold))
          handlers in
      let env, handlers_map =
        List.fold_left (fun (env, map) (nfail, ids, rs, e2, dbg, is_cold) ->
            let env, r = env_add_static_exception nfail rs env in
            env, Int.Map.add nfail (r, (ids, rs, e2, dbg, is_cold)) map)
          (env, Int.Map.empty) handlers in
      let s_body = self#emit_tail_sequence env e1 in
      let translate_one_handler nfail (trap_info, (ids, rs, e2, _dbg, is_cold)) =
        assert(List.length ids = List.length rs);
        let trap_stack =
          match !trap_info with
          | Unreachable -> assert false
          | Reachable t -> t
        in
        let ids_and_rs = List.combine ids rs in
        let new_env =
          List.fold_left
            (fun env ((id, _typ),r) -> env_add id r env)
            (env_set_trap_stack env trap_stack) ids_and_rs
        in
        let seq =
          self#emit_tail_sequence new_env e2 ~at_start:(fun seq ->
            List.iter (fun ((var, _typ), r) ->
                let provenance = VP.provenance var in
                if Option.is_some provenance then (
                  let var = VP.var var in
                  let naming_op =
                    Iname_for_debugger {
                      ident = var;
                      provenance;
                      which_parameter = None;
                      is_assignment = false;
                      regs = r;
                    }
                  in
                  seq#insert_debug new_env (Iop naming_op) Debuginfo.none
                    [| |] [| |]
                ))
                ids_and_rs)
        in
        nfail, trap_stack, seq, is_cold
      in
      let rec build_all_reachable_handlers ~already_built ~not_built =
        let not_built, to_build =
          Int.Map.partition (fun _n (r, _) -> !r = Unreachable) not_built
        in
        if Int.Map.is_empty to_build then already_built
        else begin
          let already_built =
            Int.Map.fold
              (fun nfail handler already_built ->
                 (translate_one_handler nfail handler) :: already_built)
              to_build already_built
          in
          build_all_reachable_handlers ~already_built ~not_built
        end
      in
      let new_handlers =
        build_all_reachable_handlers ~already_built:[] ~not_built:handlers_map
        (* Note: we're dropping unreachable handlers here *)
      in
      (* The final trap stack doesn't matter, as it's not reachable. *)
      self#insert env (Icatch(rec_flag, env.trap_stack, new_handlers, s_body))
        [||] [||]
  | Ctrywith(e1, kind, v, e2, dbg, _value_kind) ->
      let env_body = env_enter_trywith env kind in
      let s1 = self#emit_tail_sequence env_body e1 in
      let rv = self#regs_for typ_val in
      let with_handler env_handler e2 =
        let s2 =
          self#emit_tail_sequence env_handler e2
            ~at_start:(fun seq ->
              let provenance = VP.provenance v in
              if Option.is_some provenance then (
                let var = VP.var v in
                let naming_op =
                  Iname_for_debugger {
                    ident = var;
                    provenance;
                    which_parameter = None;
                    is_assignment = false;
                    regs = rv;
                  }
                in
                seq#insert_debug env_handler (Iop naming_op)
                  Debuginfo.none [| |] [| |]
              ))
        in
        self#insert env
          (Itrywith(s1, kind,
                    (env_handler.trap_stack,
                     instr_cons_debug (Iop Imove) [|Proc.loc_exn_bucket|] rv dbg
                       s2)))
          [||] [||]
      in
      let env = env_add v rv env in
      begin match kind with
      | Delayed lbl ->
        begin match env_find_static_exception lbl env_body with
        | { traps_ref = { contents = Reachable ts; }; _} ->
          with_handler (env_set_trap_stack env ts) e2
        | { traps_ref = { contents = Unreachable; }; _ } ->
          let unreachable =
            Cmm.(Cop ((Cload { memory_chunk = Word_int; mutability = Mutable; is_atomic = false; }),
                      [Cconst_int (0, Debuginfo.none)],
                      Debuginfo.none))
          in
          with_handler env unreachable
        (* Misc.fatal_errorf "Selection.emit_expr: \
           Unreachable exception handler %d" lbl *)
        | exception Not_found ->
          Misc.fatal_errorf "Selection.emit_expr: Unbound handler %d" lbl
        end
      end
  | Cop _
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _ | Cconst_vec128 _
  | Cvar _
  | Cassign _
  | Ctuple _
  | Cexit _ ->
    self#emit_return env exp (pop_all_traps env)

method private emit_tail_sequence ?at_start env exp =
  let s = {< instr_seq = dummy_instr >} in
  begin match at_start with
  | None -> ()
  | Some f -> f s
  end;
  s#emit_tail env exp;
  s#extract

(* Sequentialization of a function definition *)

method emit_fundecl ~future_funcnames f =
  current_function_name := f.Cmm.fun_name.sym_name;
  current_function_is_check_enabled :=
    Checkmach.is_check_enabled f.Cmm.fun_codegen_options f.Cmm.fun_name.sym_name f.Cmm.fun_dbg;
  let num_regs_per_arg = Array.make (List.length f.Cmm.fun_args) 0 in
  let rargs =
    List.mapi
      (fun arg_index (var, ty) ->
        let r = self#regs_for ty in
        name_regs var r;
        num_regs_per_arg.(arg_index) <- Array.length r;
        r)
      f.Cmm.fun_args in
  let rarg = Array.concat rargs in
  let loc_arg = Proc.loc_parameters (Reg.typv rarg) in
  let env =
    List.fold_right2
      (fun (id, _ty) r env -> env_add id r env)
      f.Cmm.fun_args rargs env_empty in
  self#emit_tail env f.Cmm.fun_body;
  let body = self#extract in
  instr_seq <- dummy_instr;
  let loc_arg_index = ref 0 in
  List.iteri (fun param_index (var, _ty) ->
      let provenance = VP.provenance var in
      let var = VP.var var in
      let num_regs_for_arg = num_regs_per_arg.(param_index) in
      let hard_regs_for_arg =
        Array.init num_regs_for_arg (fun index ->
          loc_arg.(!loc_arg_index + index))
      in
      loc_arg_index := !loc_arg_index + num_regs_for_arg;
      if Option.is_some provenance then (
        let naming_op =
          Iname_for_debugger {
            ident = var;
            provenance;
            which_parameter = Some param_index;
            is_assignment = false;
            regs = hard_regs_for_arg;
          }
        in
        self#insert_debug env (Iop naming_op) Debuginfo.none
          hard_regs_for_arg [| |]
      ))
    f.Cmm.fun_args;
  self#insert_moves env loc_arg rarg;
  let polled_body =
    if Polling.requires_prologue_poll ~future_funcnames
         ~fun_name:f.Cmm.fun_name.sym_name body
      then
        instr_cons_debug
          (Iop(Ipoll { return_label = None })) [||] [||] f.Cmm.fun_dbg body
    else
      body
    in
  let body_with_prologue = self#extract_onto polled_body in
  instr_iter (fun instr -> self#mark_instr instr.Mach.desc) body_with_prologue;
  { fun_name = f.Cmm.fun_name.sym_name;
    fun_args = loc_arg;
    fun_body = body_with_prologue;
    fun_codegen_options = f.Cmm.fun_codegen_options;
    fun_dbg  = f.Cmm.fun_dbg;
    fun_poll = f.Cmm.fun_poll;
    fun_num_stack_slots = Array.make Proc.num_stack_slot_classes 0;
    fun_contains_calls = !contains_calls;
  }

end
