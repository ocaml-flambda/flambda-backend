(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Cmm_helpers
module Ece = Effects_and_coeffects

let unsupported_32_bits () =
  Misc.fatal_errorf "32 bits is currently unsupported in Flambda."

(* Are we compiling on/for a 32-bit architecture ? *)
let arch32 = Arch.size_int = 4

let arch64 = Arch.size_int = 8

let () = assert (arch32 || arch64)

let exttype_of_kind k =
  match (k : Flambda_kind.t) with
  | Value -> Cmm.XInt
  | Naked_number Naked_float -> Cmm.XFloat
  | Naked_number Naked_int64 -> Cmm.XInt64
  | Naked_number Naked_int32 -> Cmm.XInt32
  | Naked_number (Naked_immediate | Naked_nativeint) -> begin
    match Targetint_32_64.num_bits with
    | Thirty_two -> Cmm.XInt32
    | Sixty_four -> Cmm.XInt64
  end
  | Region -> Misc.fatal_error "[Region] kind not expected here"
  | Rec_info -> Misc.fatal_error "[Rec_info] kind not expected here"

(* Kinds and types *)

let check_arity arity args =
  Flambda_arity.With_subkinds.cardinal arity = List.length args

let machtype_of_kind k =
  match (k : Flambda_kind.t) with
  | Value -> Cmm.typ_val
  | Naked_number Naked_float -> Cmm.typ_float
  | Naked_number Naked_int64 -> typ_int64
  | Naked_number (Naked_immediate | Naked_int32 | Naked_nativeint) ->
    Cmm.typ_int
  | Region | Rec_info -> assert false

let machtype_of_kinded_parameter p =
  Bound_parameter.kind p |> Flambda_kind.With_subkind.kind |> machtype_of_kind

(* Constructors for constants *)

let symbol_from_linkage_name ?dbg ln =
  symbol_from_string ?dbg (Linkage_name.to_string ln)

let symbol ?dbg sym = symbol_from_linkage_name ?dbg (Symbol.linkage_name sym)

let name env name =
  Name.pattern_match name
    ~var:(fun v -> To_cmm_env.inline_variable env v)
    ~symbol:(fun s ->
      let env =
        To_cmm_env.check_scope ~allow_deleted:false env
          (Code_id_or_symbol.create_symbol s)
      in
      ( Cmm.Cconst_symbol
          (Linkage_name.to_string (Symbol.linkage_name s), Debuginfo.none),
        env,
        Ece.pure ))

let targetint ?(dbg = Debuginfo.none) t =
  match Targetint_32_64.repr t with
  | Int32 i -> int32 ~dbg i
  | Int64 i -> int64 ~dbg i

(* CR mshinwell: The following functions should be in [Targetint], etc. *)

let tag_targetint t = Targetint_32_64.(add (shift_left t 1) one)

let targetint_of_imm i =
  Targetint_31_63.Imm.to_targetint i.Targetint_31_63.value

let nativeint_of_targetint t =
  match Targetint_32_64.repr t with
  | Int32 i -> Nativeint.of_int32 i
  | Int64 i -> Int64.to_nativeint i

let const _env cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i -> targetint (targetint_of_imm i)
  | Tagged_immediate i -> targetint (tag_targetint (targetint_of_imm i))
  | Naked_float f -> float (Numeric_types.Float_by_bit_pattern.to_float f)
  | Naked_int32 i -> int32 i
  | Naked_int64 i -> int64 i
  | Naked_nativeint t -> targetint t

(* [Simple]s and lists thereof *)

let simple env s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:_ -> name env n)
    ~const:(fun c -> const env c, env, Ece.pure)

let arg_list env l =
  let aux (list, env, effs) x =
    let y, env, eff = simple env x in
    y :: list, env, Ece.join eff effs
  in
  let args, env, effs = List.fold_left aux ([], env, Ece.pure) l in
  List.rev args, env, effs

let param_list env l =
  let flambda_vars = Bound_parameters.vars l in
  let env, cmm_vars = To_cmm_env.create_variables env flambda_vars in
  let vars =
    List.map2
      (fun v v' -> v, machtype_of_kinded_parameter v')
      cmm_vars
      (Bound_parameters.to_list l)
  in
  env, vars

(* Block creation *)

(* Blocks of size 0 (i.e. with an empty list of fields) must be statically
   allocated, else the GC will bug (cf `make_alloc_generic` in cmm_helpers.ml).
   More precisely, blocks of size 0 must have a black header, which means they
   must either be statically allocated, or be pointers to one of the cell of the
   atom_table (see `startup_aux.c`).

   Both `make_alloc` and `make_float_alloc` from `cmm_helpers.ml` already check
   for that, but with an assertion, which do not produce helpful error
   messages. *)
let check_alloc_fields = function
  | [] ->
    Misc.fatal_error
      "Blocks dynamically allocated cannot have size 0 (empty arrays have to \
       be lifted so they can be statically allocated)"
  | _ -> ()

let make_array ?(dbg = Debuginfo.none) kind alloc_mode args =
  check_alloc_fields args;
  match (kind : Flambda_primitive.Array_kind.t) with
  | Immediates | Values ->
    make_alloc ~mode:(Alloc_mode.to_lambda alloc_mode) dbg 0 args
  | Naked_floats ->
    make_float_alloc
      ~mode:(Alloc_mode.to_lambda alloc_mode)
      dbg
      (Tag.to_int Tag.double_array_tag)
      args

let make_block ?(dbg = Debuginfo.none) kind alloc_mode args =
  check_alloc_fields args;
  match (kind : Flambda_primitive.Block_kind.t) with
  | Values (tag, _) ->
    make_alloc
      ~mode:(Alloc_mode.to_lambda alloc_mode)
      dbg (Tag.Scannable.to_int tag) args
  | Naked_floats ->
    make_float_alloc
      ~mode:(Alloc_mode.to_lambda alloc_mode)
      dbg
      (Tag.to_int Tag.double_array_tag)
      args

let make_closure_block ?(dbg = Debuginfo.none) alloc_mode l =
  assert (List.compare_length_with l 0 > 0);
  let tag = Tag.(to_int closure_tag) in
  make_alloc ~mode:(Alloc_mode.to_lambda alloc_mode) dbg tag l

(* try-with blocks *)

let raise_kind (kind : Trap_action.raise_kind option) : Lambda.raise_kind =
  match kind with
  | Some Regular -> Raise_regular
  | Some Reraise -> Raise_reraise
  | Some No_trace -> Raise_notrace
  | None -> Raise_notrace

(* Call into `caml_flambda2_invalid` for invalid/unreachable code, instead of
   simply generating code that segfaults *)
let invalid res ~message =
  let message_sym =
    Symbol.create
      (Compilation_unit.get_current_exn ())
      (Linkage_name.create (Variable.unique_name (Variable.create "invalid")))
  in
  let data_items =
    Cmm_helpers.emit_string_constant
      (Symbol.linkage_name_as_string message_sym, Cmmgen_state.Global)
      message []
    |> To_cmm_result.add_data_items res
  in
  let call_expr =
    extcall ~dbg:Debuginfo.none ~alloc:false ~is_c_builtin:false ~returns:false
      ~ty_args:[XInt] "caml_flambda2_invalid" Cmm.typ_void [symbol message_sym]
  in
  call_expr, data_items

let make_update env kind ~symbol var ~index ~prev_updates =
  let e, env, _ece = To_cmm_env.inline_variable env var in
  let addr = field_address symbol index Debuginfo.none in
  let update = store kind Lambda.Root_initialization ~addr ~new_value:e in
  match prev_updates with
  | None -> env, Some update
  | Some prev_updates -> env, Some (sequence prev_updates update)

let name_static env name =
  Name.pattern_match name
    ~var:(fun v -> env, `Var v)
    ~symbol:(fun s ->
      ( To_cmm_env.check_scope ~allow_deleted:false env
          (Code_id_or_symbol.create_symbol s),
        `Data [symbol_address (Symbol.linkage_name_as_string s)] ))

let const_static cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i -> [cint (nativeint_of_targetint (targetint_of_imm i))]
  | Tagged_immediate i ->
    [cint (nativeint_of_targetint (tag_targetint (targetint_of_imm i)))]
  | Naked_float f -> [cfloat (Numeric_types.Float_by_bit_pattern.to_float f)]
  | Naked_int32 i -> [cint (Nativeint.of_int32 i)]
  | Naked_int64 i ->
    if arch32
    then
      Misc.fatal_error "Not implemented for 32-bit platforms"
      (* split int64 on 32-bit archs *)
    else [cint (Int64.to_nativeint i)]
  | Naked_nativeint t -> [cint (nativeint_of_targetint t)]

let simple_static env s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:_ -> name_static env n)
    ~const:(fun c -> env, `Data (const_static c))
