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

type region_stack = Reg.t array list

type environment =
  { vars : (Reg.t array
            * Backend_var.Provenance.t option
            * Asttypes.mutable_flag) V.Map.t;
    static_exceptions : (Reg.t array list * region_stack * trap_stack_info ref) Int.Map.t;
    (** Which registers must be populated when jumping to the given
        handler. *)
    trap_stack : trap_stack;
    regions : region_stack;
    region_tail : bool;
  }

let env_add ?(mut=Asttypes.Immutable) var regs env =
  let provenance = VP.provenance var in
  let var = VP.var var in
  { env with vars = V.Map.add var (regs, provenance, mut) env.vars }

let env_add_static_exception id v env =
  let r = ref Unreachable in
  { env with static_exceptions = Int.Map.add id (v, env.regions, r) env.static_exceptions }, r

let env_find id env =
  let regs, _provenance, _mut = V.Map.find id env.vars in
  regs

let env_find_mut id env =
  let regs, _provenance, mut = V.Map.find id env.vars in
  begin match mut with
  | Asttypes.Mutable -> ()
  | Asttypes.Immutable ->
    Misc.fatal_errorf
      "Selectgen.env_find_mut: %a is not mutable"
      V.print id
  end;
  regs

let env_find_static_exception id env =
  Int.Map.find id env.static_exceptions

let env_enter_trywith env kind =
  match kind with
  | Regular -> { env with trap_stack = Generic_trap env.trap_stack; }
  | Delayed id -> let env, _ = env_add_static_exception id [] env in env

let env_set_trap_stack env trap_stack =
  { env with trap_stack; }

let rec combine_traps trap_stack = function
  | [] -> trap_stack
  | (Push t) :: l -> combine_traps (Specific_trap (t, trap_stack)) l
  | Pop :: l ->
      begin match trap_stack with
      | Uncaught -> Misc.fatal_error "Trying to pop a trap from an empty stack"
      | Generic_trap ts | Specific_trap (_, ts) -> combine_traps ts l
      end

let print_traps ppf traps =
  let rec print_traps ppf = function
    | Uncaught -> Format.fprintf ppf "T"
    | Generic_trap ts -> Format.fprintf ppf "_::%a" print_traps ts
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
  | Uncaught
  | Generic_trap _ -> ()
  | Specific_trap (lbl, _) ->
    begin match env_find_static_exception lbl env with
    | (_, _, traps_ref) -> set_traps lbl traps_ref ts [Pop]
    | exception Not_found -> Misc.fatal_errorf "Trap %d not registered in env" lbl
    end

let trap_stack_is_empty env =
  match env.trap_stack with
  | Uncaught -> true
  | Generic_trap _ | Specific_trap _ -> false

let pop_all_traps env =
  let rec pop_all acc = function
    | Uncaught -> acc
    | Generic_trap t
    | Specific_trap (_, t) -> pop_all (Cmm.Pop :: acc) t
  in
  pop_all [] env.trap_stack

let env_empty = {
  vars = V.Map.empty;
  static_exceptions = Int.Map.empty;
  trap_stack = Uncaught;
  regions = [];
  region_tail = false;
}

(* Assuming [rs] is equal to or a suffix of [env.regions],
   return the last region in [env.regions] but not [rs]
   (or None if they are equal) *)
let env_close_regions env rs =
  let rec aux v es rs =
    match es, rs with
    | [], [] -> v
    | (r :: _), (r' :: _) when r == r' -> v
    | [], _::_ ->
       Misc.fatal_error "Selectgen.env_close_regions: not a suffix"
    | r :: es, rs -> aux (Some r) es rs
  in
  aux None env.regions rs

let select_mutable_flag : Asttypes.mutable_flag -> Mach.mutable_flag = function
  | Immutable -> Immutable
  | Mutable -> Mutable

(* Infer the type of the result of an operation *)

let oper_result_type = function
  | Capply(ty, _) -> ty
  | Cextcall { ty; ty_args = _; alloc = _; func = _; } -> ty
  | Cload (c, _) ->
      begin match c with
      | Word_val -> typ_val
      | Single | Double -> typ_float
      | _ -> typ_int
      end
  | Calloc _ -> typ_val
  | Cstore (_c, _) -> typ_void
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
  | Craise _ -> typ_void
  | Ccheckbound -> typ_void
  | Cprobe _ -> typ_void
  | Cprobe_is_enabled _ -> typ_int
  | Copaque -> typ_val
  | Cbeginregion ->
    (* This must not be typ_val; the begin-region operation returns a
       naked pointer into the local allocation stack. *)
    typ_int
  | Cendregion -> typ_void

(* Infer the size in bytes of the result of an expression whose evaluation
   may be deferred (cf. [emit_parts]). *)

let size_component = function
  | Val | Addr -> Arch.size_addr
  | Int -> Arch.size_int
  | Float -> Arch.size_float

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

(* "Join" two instruction sequences, making sure they return their results
   in the same registers. *)

let join env opt_r1 seq1 opt_r2 seq2 =
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
          seq2#insert_move env r2.(i) r1.(i)
        end else if Reg.anonymous r2.(i)
          && Cmm.ge_component r2.(i).typ r1.(i).typ
        then begin
          r.(i) <- r2.(i);
          seq1#insert_move env r1.(i) r2.(i)
        end else begin
          let typ = Cmm.lub_component r1.(i).typ r2.(i).typ in
          r.(i) <- Reg.create typ;
          seq1#insert_move env r1.(i) r.(i);
          seq2#insert_move env r2.(i) r.(i)
        end
      done;
      Some r

(* Same, for N branches *)

let join_array env rs =
  let some_res = ref None in
  for i = 0 to Array.length rs - 1 do
    let (r, _) = rs.(i) in
    match r with
    | None -> ()
    | Some r ->
      match !some_res with
      | None -> some_res := Some (r, Array.map (fun r -> r.typ) r)
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
        | Some r -> s#insert_moves env r res
      done;
      Some res

(* Name of function being compiled *)
let current_function_name = ref ""

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

class virtual selector_generic = object (self)

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
      | Craise _ | Ccheckbound | Catomic _
      | Cprobe _ | Cprobe_is_enabled _ | Copaque -> false
      | Cprefetch _ | Cbeginregion | Cendregion -> false (* avoid reordering *)
        (* The remaining operations are simple if their args are *)
      | Cload _ | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand | Cor
      | Cxor | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf
      | Cclz _ | Cctz _ | Cpopcnt
      | Cbswap _
      | Ccsel _
      | Cabsf | Caddf | Csubf | Cmulf | Cdivf | Cfloatofint | Cintoffloat
      | Cvalueofint | Cintofvalue
      | Ccmpf _ -> List.for_all self#is_simple_expr args
      end
  | Cassign _ | Cifthenelse _ | Cswitch _ | Ccatch _ | Cexit _
  | Ctrywith _ | Cregion _ | Ctail _ -> false

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
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
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
      | Craise _ | Ccheckbound -> EC.effect_only Effect.Raise
      | Cload (_, Asttypes.Immutable) -> EC.none
      | Cload (_, Asttypes.Mutable) -> EC.coeffect_only Coeffect.Read_mutable
      | Cprobe_is_enabled _ -> EC.coeffect_only Coeffect.Arbitrary
      | Caddi | Csubi | Cmuli | Cmulhi _ | Cdivi | Cmodi | Cand | Cor | Cxor
      | Cbswap _
      | Ccsel _
      | Cclz _ | Cctz _ | Cpopcnt
      | Clsl | Clsr | Casr | Ccmpi _ | Caddv | Cadda | Ccmpa _ | Cnegf | Cabsf
      | Caddf | Csubf | Cmulf | Cdivf | Cfloatofint | Cintoffloat
      | Cvalueofint | Cintofvalue | Ccmpf _ ->
        EC.none
    in
    EC.join from_op (EC.join_list_map args self#effects_of)
  | Cassign _ | Cswitch _ | Ccatch _ | Cexit _ | Ctrywith _
  | Cregion _ | Ctail _ ->
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

method mark_c_tailcall = ()

method mark_instr = function
  | Iop (Icall_ind | Icall_imm _ | Iextcall _ | Iprobe _) ->
      self#mark_call
  | Iop (Itailcall_ind | Itailcall_imm _) ->
      self#mark_tailcall
  | Iop (Ialloc _) | Iop (Ipoll _) ->
      self#mark_call (* caml_alloc*, caml_garbage_collection (incl. polls) *)
  | Iop (Iintop (Icheckbound) | Iintop_imm(Icheckbound, _)) ->
      self#mark_c_tailcall (* caml_ml_array_bound_error *)
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
    Iextcall { func; alloc; ty_res = ty; ty_args; returns }, args
  | (Cload (chunk, mut), [arg]) ->
      let (addr, eloc) = self#select_addressing chunk arg in
      (Iload(chunk, addr, select_mutable_flag mut), [eloc])
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
  | (Catomic {op = Fetch_and_add; size}, [src; dst]) ->
    let dst_size = match size with Word | Sixtyfour -> Word_int | Thirtytwo -> Thirtytwo_signed in
    let (addr, eloc) = self#select_addressing dst_size dst in
    (Iintop_atomic { op = Fetch_and_add; size; addr }, [src; eloc])
  | (Catomic {op = Compare_and_swap; size}, [compare_with; set_to; dst]) ->
    let dst_size = match size with Word | Sixtyfour -> Word_int | Thirtytwo -> Thirtytwo_signed in
    let (addr, eloc) = self#select_addressing dst_size dst in
    (Iintop_atomic { op = Compare_and_swap; size; addr }, [compare_with; set_to; eloc])
  | (Ccheckbound, _) ->
    self#select_arith Icheckbound args
  | (Cprobe { name; handler_code_sym; }, _) ->
    Iprobe { name; handler_code_sym; }, args
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
  instr_seq <- instr_cons desc arg res instr_seq

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
  if stacksize <> 0 then begin
    self#insert env (Iop(Istackoffset(-stacksize))) [||] [||]
  end;
  self#insert_moves env loc res

(* Add an Iop opcode. Can be overridden by processor description
   to insert moves before and after the operation, i.e. for two-address
   instructions, or instructions using dedicated registers. *)

method insert_op_debug env op dbg rs rd =
  self#insert_debug env (Iop op) dbg rs rd;
  rd

method insert_op env op rs rd =
  self#insert_op_debug env op Debuginfo.none rs rd

(* Add the instructions for the given expression
   at the end of the self sequence *)

method emit_expr (env:environment) exp =
  (* Environment used in recursive calls not in tail position *)
  let env' =
    if env.region_tail then {env with region_tail=false} else env in
  match exp with
    Cconst_int (n, _dbg) ->
      let r = self#regs_for typ_int in
      Some(self#insert_op env (Iconst_int(Nativeint.of_int n)) [||] r)
  | Cconst_natint (n, _dbg) ->
      let r = self#regs_for typ_int in
      Some(self#insert_op env (Iconst_int n) [||] r)
  | Cconst_float (n, _dbg) ->
      let r = self#regs_for typ_float in
      Some(self#insert_op env (Iconst_float (Int64.bits_of_float n)) [||] r)
  | Cconst_symbol (n, _dbg) ->
      (* Cconst_symbol _ evaluates to a statically-allocated address, so its
         value fits in a typ_int register and is never changed by the GC.

         Some Cconst_symbols point to statically-allocated blocks, some of
         which may point to heap values. However, any such blocks will be
         registered in the compilation unit's global roots structure, so
         adding this register to the frame table would be redundant *)
      let r = self#regs_for typ_int in
      Some(self#insert_op env (Iconst_symbol n) [||] r)
  | Cvar v ->
      begin try
        Some(env_find v env)
      with Not_found ->
        Misc.fatal_error("Selection.emit_expr: unbound var " ^ V.unique_name v)
      end
  | Clet(v, e1, e2) ->
      begin match self#emit_expr env' e1 with
        None -> None
      | Some r1 -> self#emit_expr (self#bind_let env v r1) e2
      end
  | Clet_mut(v, k, e1, e2) ->
      begin match self#emit_expr env' e1 with
        None -> None
      | Some r1 -> self#emit_expr (self#bind_let_mut env v k r1) e2
      end
  | Cphantom_let (_var, _defining_expr, body) ->
      self#emit_expr env body
  | Cassign(v, e1) ->
      let rv =
        try
          env_find_mut v env
        with Not_found ->
          Misc.fatal_error ("Selection.emit_expr: unbound var " ^ V.name v) in
      begin match self#emit_expr env' e1 with
        None -> None
      | Some r1 ->
          self#insert_moves env r1 rv; Some [||]
      end
  | Ctuple [] ->
      Some [||]
  | Ctuple exp_list ->
      begin match self#emit_parts_list env' exp_list with
        None -> None
      | Some(simple_list, ext_env) ->
          Some(self#emit_tuple ext_env simple_list)
      end
  | Cop(Craise k, [arg], dbg) ->
      begin match self#emit_expr env' arg with
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
         Some (self#insert_op_debug env Iopaque dbg rs rs)
      end
  | Cop(op, args, dbg) ->
      begin match self#emit_parts_list env' args with
        None -> None
      | Some(simple_args, env) ->
          let ty = oper_result_type op in
          let (new_op, new_args) = self#select_operation op simple_args dbg in
          match new_op with
            Icall_ind ->
              let r1 = self#emit_tuple env new_args in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let rd = self#regs_for ty in
              let (loc_arg, stack_ofs) = Proc.loc_arguments (Reg.typv rarg) in
              let loc_res = Proc.loc_results (Reg.typv rd) in
              self#insert_move_args env rarg loc_arg stack_ofs;
              self#insert_debug env (Iop new_op) dbg
                          (Array.append [|r1.(0)|] loc_arg) loc_res;
              self#insert_move_results env loc_res rd stack_ofs;
              set_traps_for_raise env;
              Some rd
          | Icall_imm _ ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              let (loc_arg, stack_ofs) = Proc.loc_arguments (Reg.typv r1) in
              let loc_res = Proc.loc_results (Reg.typv rd) in
              self#insert_move_args env r1 loc_arg stack_ofs;
              self#insert_debug env (Iop new_op) dbg loc_arg loc_res;
              self#insert_move_results env loc_res rd stack_ofs;
              set_traps_for_raise env;
              Some rd
          | Iextcall { ty_args; returns; _} ->
              let (loc_arg, stack_ofs) =
                self#emit_extcall_args env ty_args new_args in
              let rd = self#regs_for ty in
              let loc_res =
                self#insert_op_debug env new_op dbg
                  loc_arg (Proc.loc_external_results (Reg.typv rd)) in
              self#insert_move_results env loc_res rd stack_ofs;
              set_traps_for_raise env;
              if returns then Some rd else None
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
              self#emit_stores env new_args rd;
              set_traps_for_raise env;
              Some rd
          | Iprobe _ ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              let rd = self#insert_op_debug env new_op dbg r1 rd in
              set_traps_for_raise env;
              Some rd
          | op ->
              let r1 = self#emit_tuple env new_args in
              let rd = self#regs_for ty in
              Some (self#insert_op_debug env op dbg r1 rd)
      end
  | Csequence(e1, e2) ->
      begin match self#emit_expr env' e1 with
        None -> None
      | Some _ -> self#emit_expr env e2
      end
  | Cifthenelse(econd, _ifso_dbg, eif, _ifnot_dbg, eelse, _dbg, _kind) ->
      let (cond, earg) = self#select_condition econd in
      begin match self#emit_expr env' earg with
        None -> None
      | Some rarg ->
          let (rif, sif) = self#emit_sequence env eif in
          let (relse, selse) = self#emit_sequence env eelse in
          let r = join env rif sif relse selse in
          self#insert env (Iifthenelse(cond, sif#extract, selse#extract))
                      rarg [||];
          r
      end
  | Cswitch(esel, index, ecases, _dbg, _kind) ->
      begin match self#emit_expr env' esel with
        None -> None
      | Some rsel ->
          let rscases =
            Array.map (fun (case, _dbg) -> self#emit_sequence env case) ecases
          in
          let r = join_array env rscases in
          self#insert env (Iswitch(index,
                                   Array.map (fun (_, s) -> s#extract) rscases))
                      rsel [||];
          r
      end
  | Ccatch(_, [], e1, _) ->
      self#emit_expr env e1
  | Ccatch(rec_flag, handlers, body, _) ->
      let handlers =
        List.map (fun (nfail, ids, e2, dbg) ->
            let rs =
              List.map
                (fun (id, typ) ->
                  let r = self#regs_for typ in name_regs id r; r)
                ids in
            (nfail, ids, rs, e2, dbg))
          handlers
      in
      let env =
        (* Disable region-fusion on loops *)
        match rec_flag with Recursive -> env' | Nonrecursive -> env in
      let env, handlers_map =
        (* Since the handlers may be recursive, and called from the body,
           the same environment is used for translating both the handlers and
           the body. *)
        List.fold_left (fun (env, map) (nfail, ids, rs, e2, dbg) ->
            let env, r = env_add_static_exception nfail rs env in
            env, Int.Map.add nfail (r, (ids, rs, e2, dbg)) map)
          (env, Int.Map.empty) handlers
      in
      let (r_body, s_body) = self#emit_sequence env body in
      let translate_one_handler nfail (traps_info, (ids, rs, e2, _dbg)) =
        assert(List.length ids = List.length rs);
        let trap_stack =
          match !traps_info with
          | Unreachable -> assert false
          | Reachable t -> t
        in
        let new_env =
          List.fold_left (fun env ((id, _typ), r) -> env_add id r env)
            (env_set_trap_stack env trap_stack) (List.combine ids rs)
        in
        let (r, s) = self#emit_sequence new_env e2 in
        ((nfail, trap_stack), (r, s))
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
      let r = join_array env a in
      let aux ((nfail, ts), (_r, s)) = (nfail, ts, s#extract) in
      let final_trap_stack =
        match r_body with
        | Some _ -> env.trap_stack
        | None ->
          (* The body never returns, so the trap stack at the end of the catch
             is the one from the handlers *)
          begin match l with
          | [] -> (* This whole catch never returns *)
            env.trap_stack
          | ((_, ts), (_, _)) :: tl ->
            assert (List.for_all (fun ((_, ts'), (_, _)) -> ts = ts') tl);
            ts
          end
      in
      self#insert env
        (Icatch (rec_flag, final_trap_stack, List.map aux l, s_body#extract))
        [||] [||];
      r
  | Cexit (lbl,args,traps) ->
      begin match self#emit_parts_list env' args with
        None -> None
      | Some (simple_list, ext_env) ->
          begin match lbl with
          | Lbl nfail ->
              let src = self#emit_tuple ext_env simple_list in
              let dest_args, dest_regions, trap_stack =
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
              self#insert_moves env tmp_regs (Array.concat dest_args) ;
              begin match env_close_regions env dest_regions with
              | None -> ()
              | Some regs -> self#insert env (Iop Iendregion) regs [||]
              end;
              self#insert env (Iexit (nfail, traps)) [||] [||];
              set_traps nfail trap_stack env.trap_stack traps;
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
  | Ctrywith(e1, kind, v, e2, _dbg, _value_kind) ->
      (* This region is used only to clean up local allocations in the
         exceptional path. It must not be ended in the non-exception case
         as local allocations may be returned from the body of the "try". *)
      let end_region =
        if Config.stack_allocation
          && match kind with Regular -> true | Delayed _ -> false
        then begin
          let reg = self#regs_for typ_int in
          self#insert env (Iop Ibeginregion) [| |] reg;
          fun handler_instruction -> instr_cons (Iop Iendregion) reg [| |] handler_instruction
        end
        else
          fun handler_instruction -> handler_instruction
      in
      let env_body = env_enter_trywith env kind in
      let (r1, s1) = self#emit_sequence env_body e1 in
      let rv = self#regs_for typ_val in
      let with_handler env_handler e2 =
        let (r2, s2) = self#emit_sequence env_handler e2 in
        let r = join env r1 s1 r2 s2 in
        self#insert env
          (Itrywith(s1#extract, kind,
                    (env_handler.trap_stack,
                     instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv
                       (end_region s2#extract))))
          [||] [||];
        r
      in
      let env = env_add v rv env in
      begin match kind with
      | Regular -> with_handler env e2
      | Delayed lbl ->
        begin match env_find_static_exception lbl env_body with
        | (_, _, { contents = Reachable ts; }) ->
          with_handler (env_set_trap_stack env ts) e2
        | (_, _, { contents = Unreachable; }) ->
          let unreachable =
            Cmm.(Cop ((Cload (Word_int, Mutable)),
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
  | Cregion e ->
     assert(Config.stack_allocation);
     let reg = self#regs_for typ_int in
     self#insert env (Iop Ibeginregion) [| |] reg;
     let env = { env with regions = reg::env.regions; region_tail = true } in
     begin match self#emit_expr env e with
       None -> None
     | Some _ as res ->
        self#insert env (Iop Iendregion) reg [| |];
        res
     end
  | Ctail e ->
      assert env.region_tail;
      self#emit_expr env e

method private emit_sequence (env:environment) exp =
  let s = {< instr_seq = dummy_instr >} in
  let r = s#emit_expr env exp in
  (r, s)

method private bind_let (env:environment) v r1 =
  if all_regs_anonymous r1 then begin
    name_regs v r1;
    env_add v r1 env
  end else begin
    let rv = Reg.createv_like r1 in
    name_regs v rv;
    self#insert_moves env r1 rv;
    env_add v rv env
  end

method private bind_let_mut (env:environment) v k r1 =
  let rv = self#regs_for k in
  name_regs v rv;
  self#insert_moves env r1 rv;
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
    match self#emit_expr env exp with
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
      match self#emit_expr env exp with
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
      match self#emit_expr env arg with
        None -> assert false
      | Some regs ->
          match op with
            Istore(_, _, _) ->
              for i = 0 to Array.length regs - 1 do
                let r = regs.(i) in
                let kind = if r.typ = Float then Double else Word_val in
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
      let loc = Proc.loc_results (Reg.typv r) in
      if env.region_tail then
        self#insert env (Iop Iendregion) (List.hd env.regions) [||];
      self#insert_moves env r loc;
      self#insert env (Ireturn traps) loc [||]

method private emit_return (env:environment) exp traps =
  self#insert_return env (self#emit_expr env exp) traps

method emit_tail (env:environment) exp =
  let env' =
    if env.region_tail then {env with region_tail=false} else env in
  match exp with
    Clet(v, e1, e2) ->
      begin match self#emit_expr env' e1 with
        None -> ()
      | Some r1 -> self#emit_tail (self#bind_let env v r1) e2
      end
  | Clet_mut (v, k, e1, e2) ->
     begin match self#emit_expr env' e1 with
       None -> ()
     | Some r1 -> self#emit_tail (self#bind_let_mut env v k r1) e2
     end
  | Cphantom_let (_var, _defining_expr, body) ->
      self#emit_tail env body
  | Cop((Capply(ty, ((Rc_close_at_apply | Rc_normal) as pos))) as op,
        args, dbg) ->
      let tail = (pos = Lambda.Rc_close_at_apply) in
      let endregion = env.region_tail in
      begin match self#emit_parts_list env' args with
        None -> ()
      | Some(simple_args, env) ->
          let (new_op, new_args) = self#select_operation op simple_args dbg in
          match new_op with
            Icall_ind ->
              let r1 = self#emit_tuple env new_args in
              if endregion && tail then
                self#insert env (Iop Iendregion) (List.hd env.regions) [||];
              let endregion = endregion && not tail in
              let rarg = Array.sub r1 1 (Array.length r1 - 1) in
              let (loc_arg, stack_ofs) = Proc.loc_arguments (Reg.typv rarg) in
              if stack_ofs = 0 && trap_stack_is_empty env && not endregion then begin
                let call = Iop (Itailcall_ind) in
                self#insert_moves env rarg loc_arg;
                self#insert_debug env call dbg
                            (Array.append [|r1.(0)|] loc_arg) [||];
              end else begin
                let rd = self#regs_for ty in
                let loc_res = Proc.loc_results (Reg.typv rd) in
                self#insert_move_args env rarg loc_arg stack_ofs;
                self#insert_debug env (Iop new_op) dbg
                            (Array.append [|r1.(0)|] loc_arg) loc_res;
                set_traps_for_raise env;
                if not endregion then begin
                  self#insert env (Iop(Istackoffset(-stack_ofs))) [||] [||]
                end else begin
                  self#insert_move_results env loc_res rd stack_ofs;
                  self#insert env (Iop Iendregion) (List.hd env.regions) [||];
                  self#insert_moves env rd loc_res
                end;
                self#insert env (Ireturn (pop_all_traps env)) loc_res [||]
              end
          | Icall_imm { func; } ->
              let r1 = self#emit_tuple env new_args in
              if endregion && tail then
                self#insert env (Iop Iendregion) (List.hd env.regions) [||];
              let endregion = endregion && not tail in
              let (loc_arg, stack_ofs) = Proc.loc_arguments (Reg.typv r1) in
              if stack_ofs = 0 && trap_stack_is_empty env && not endregion then begin
                let call = Iop (Itailcall_imm { func; }) in
                self#insert_moves env r1 loc_arg;
                self#insert_debug env call dbg loc_arg [||];
              end else if func = !current_function_name && trap_stack_is_empty env && not endregion then begin
                let call = Iop (Itailcall_imm { func; }) in
                let loc_arg' = Proc.loc_parameters (Reg.typv r1) in
                self#insert_moves env r1 loc_arg';
                self#insert_debug env call dbg loc_arg' [||];
              end else begin
                let rd = self#regs_for ty in
                let loc_res = Proc.loc_results (Reg.typv rd) in
                self#insert_move_args env r1 loc_arg stack_ofs;
                self#insert_debug env (Iop new_op) dbg loc_arg loc_res;
                set_traps_for_raise env;
                if not endregion then begin
                  self#insert env (Iop(Istackoffset(-stack_ofs))) [||] [||]
                end else begin
                  self#insert_move_results env loc_res rd stack_ofs;
                  self#insert env (Iop Iendregion) (List.hd env.regions) [||];
                  self#insert_moves env rd loc_res
                end;
                self#insert env (Ireturn (pop_all_traps env)) loc_res [||]
              end
          | _ -> Misc.fatal_error "Selection.emit_tail"
      end
  | Csequence(e1, e2) ->
      begin match self#emit_expr env' e1 with
        None -> ()
      | Some _ -> self#emit_tail env e2
      end
  | Cifthenelse(econd, _ifso_dbg, eif, _ifnot_dbg, eelse, _dbg, _kind) ->
      let (cond, earg) = self#select_condition econd in
      begin match self#emit_expr env' earg with
        None -> ()
      | Some rarg ->
          self#insert env
                      (Iifthenelse(cond, self#emit_tail_sequence env eif,
                                         self#emit_tail_sequence env eelse))
                      rarg [||]
      end
  | Cswitch(esel, index, ecases, _dbg, _kind) ->
      begin match self#emit_expr env' esel with
        None -> ()
      | Some rsel ->
          let cases =
            Array.map (fun (case, _dbg) -> self#emit_tail_sequence env case)
              ecases
          in
          self#insert env (Iswitch (index, cases)) rsel [||]
      end
  | Ccatch(_, [], e1, _) ->
      self#emit_tail env e1
  | Ccatch(rec_flag, handlers, e1, _) ->
      let handlers =
        List.map (fun (nfail, ids, e2, dbg) ->
            let rs =
              List.map
                (fun (id, typ) ->
                  let r = self#regs_for typ in name_regs id r; r)
                ids in
            (nfail, ids, rs, e2, dbg))
          handlers in
      let env =
        (* Disable region-fusion on loops *)
        match rec_flag with Recursive -> env' | Nonrecursive -> env in
      let env, handlers_map =
        List.fold_left (fun (env, map) (nfail, ids, rs, e2, dbg) ->
            let env, r = env_add_static_exception nfail rs env in
            env, Int.Map.add nfail (r, (ids, rs, e2, dbg)) map)
          (env, Int.Map.empty) handlers in
      let s_body = self#emit_tail_sequence env e1 in
      let translate_one_handler nfail (trap_info, (ids, rs, e2, _dbg)) =
        assert(List.length ids = List.length rs);
        let trap_stack =
          match !trap_info with
          | Unreachable -> assert false
          | Reachable t -> t
        in
        let new_env =
          List.fold_left
            (fun env ((id, _typ),r) -> env_add id r env)
            (env_set_trap_stack env trap_stack) (List.combine ids rs)
        in
        nfail, trap_stack, self#emit_tail_sequence new_env e2
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
  | Ctrywith(e1, kind, v, e2, _dbg, _value_kind) ->
      (* This region is used only to clean up local allocations in the
         exceptional path. It need not be ended in the non-exception case. *)
      let end_region =
        if Config.stack_allocation then begin
          let reg = self#regs_for typ_int in
          self#insert env (Iop Ibeginregion) [| |] reg;
          fun handler_instruction -> instr_cons (Iop Iendregion) reg [| |] handler_instruction
        end
        else
          fun handler_instruction -> handler_instruction
      in
      let env_body = env_enter_trywith env kind in
      let s1 = self#emit_tail_sequence env_body e1 in
      let rv = self#regs_for typ_val in
      let with_handler env_handler e2 =
        let s2 = self#emit_tail_sequence env_handler e2 in
        self#insert env
          (Itrywith(s1, kind,
                    (env_handler.trap_stack,
                     instr_cons (Iop Imove) [|Proc.loc_exn_bucket|] rv
                       (end_region s2))))
          [||] [||]
      in
      let env = env_add v rv env in
      begin match kind with
      | Regular -> with_handler env e2
      | Delayed lbl ->
        begin match env_find_static_exception lbl env_body with
        | (_, _, { contents = Reachable ts; }) ->
          with_handler (env_set_trap_stack env ts) e2
        | (_, _, { contents = Unreachable; }) ->
          let unreachable =
            Cmm.(Cop ((Cload (Word_int, Mutable)),
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
  | Cregion e ->
      assert(Config.stack_allocation);
      if env.region_tail then
        self#emit_return env exp (pop_all_traps env)
      else begin
        let reg = self#regs_for typ_int in
        self#insert env (Iop Ibeginregion) [| |] reg;
        let env' = { env with regions = reg::env.regions; region_tail = true } in
        self#emit_tail env' e
      end
  | Ctail e ->
      assert env.region_tail;
      self#insert env' (Iop Iendregion) (List.hd env.regions) [| |];
      self#emit_tail { env with regions = List.tl env.regions;
                                region_tail = false } e
  | Cop _
  | Cconst_int _ | Cconst_natint _ | Cconst_float _ | Cconst_symbol _
  | Cvar _
  | Cassign _
  | Ctuple _
  | Cexit _ ->
    self#emit_return env exp (pop_all_traps env)

method private emit_tail_sequence env exp =
  let s = {< instr_seq = dummy_instr >} in
  s#emit_tail env exp;
  s#extract

(* Sequentialization of a function definition *)

method emit_fundecl ~future_funcnames f =
  current_function_name := f.Cmm.fun_name;
  let rargs =
    List.map
      (fun (id, ty) -> let r = self#regs_for ty in name_regs id r; r)
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
  self#insert_moves env loc_arg rarg;
  let polled_body =
    if Polling.requires_prologue_poll ~future_funcnames
         ~fun_name:f.Cmm.fun_name body
      then
        instr_cons_debug
          (Iop(Ipoll { return_label = None })) [||] [||] f.Cmm.fun_dbg body
    else
      body
    in
  let body_with_prologue = self#extract_onto polled_body in
  instr_iter (fun instr -> self#mark_instr instr.Mach.desc) body_with_prologue;
  { fun_name = f.Cmm.fun_name;
    fun_args = loc_arg;
    fun_body = body_with_prologue;
    fun_codegen_options = f.Cmm.fun_codegen_options;
    fun_dbg  = f.Cmm.fun_dbg;
    fun_poll = f.Cmm.fun_poll;
    fun_num_stack_slots = Array.make Proc.num_register_classes 0;
    fun_contains_calls = !contains_calls;
  }

end

let reset () =
  current_function_name := ""
