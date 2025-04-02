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

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Cmm
module Int = Numbers.Int
module V = Backend_var
module VP = Backend_var.With_provenance

type trap_stack_info =
  | Unreachable
  | Reachable of Simple_operation.trap_stack

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
    trap_stack : Simple_operation.trap_stack;
    regs_for_exception_extra_args : Reg.t array Int.Map.t
        (** For each exception handler, any registers that are to be used to hold
            extra arguments. *)
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

let env_add_regs_for_exception_extra_args id extra_args env =
  { env with
    regs_for_exception_extra_args =
      Int.Map.add id extra_args env.regs_for_exception_extra_args
  }

let env_find_regs_for_exception_extra_args id env =
  try Int.Map.find id env.regs_for_exception_extra_args
  with Not_found ->
    Misc.fatal_errorf
      "Could not find exception extra args registers for continuation %d" id

let _env_find_with_provenance id env = V.Map.find id env.vars

let env_find_static_exception id env = Int.Map.find id env.static_exceptions

let env_enter_trywith env id extra =
  let env, _ = env_add_static_exception id [] env extra in
  env

let env_set_trap_stack env trap_stack = { env with trap_stack }

let rec combine_traps trap_stack = function
  | [] -> trap_stack
  | Push t :: l ->
    combine_traps (Simple_operation.Specific_trap (t, trap_stack)) l
  | Pop _ :: l -> (
    match (trap_stack : Simple_operation.trap_stack) with
    | Uncaught -> Misc.fatal_error "Trying to pop a trap from an empty stack"
    | Specific_trap (_, ts) -> combine_traps ts l)

let print_traps ppf traps =
  let rec print_traps ppf = function
    | Simple_operation.Uncaught -> Format.fprintf ppf "T"
    | Simple_operation.Specific_trap (lbl, ts) ->
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
    if Stdlib.( <> ) prev_traps traps
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
    | Simple_operation.Uncaught -> acc
    | Simple_operation.Specific_trap (lbl, t) -> pop_all (Pop lbl :: acc) t
  in
  pop_all [] env.trap_stack

let env_empty =
  { vars = V.Map.empty;
    static_exceptions = Int.Map.empty;
    trap_stack = Uncaught;
    regs_for_exception_extra_args = Int.Map.empty
  }

let select_mutable_flag : Asttypes.mutable_flag -> Simple_operation.mutable_flag
    = function
  | Immutable -> Immutable
  | Mutable -> Mutable

(* Infer the type of the result of an operation *)

let oper_result_type = function
  | Capply (ty, _) -> ty
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
  | Simple_operation.Isigned cmp ->
    Simple_operation.Isigned (swap_integer_comparison cmp)
  | Simple_operation.Iunsigned cmp ->
    Simple_operation.Iunsigned (swap_integer_comparison cmp)

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
      rv.(i).Reg.raw_name <- Reg.Raw_name.create_from_var id
    done

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
