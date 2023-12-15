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

open! Cmm_helpers
open! Cmm_builtins
module Ece = Effects_and_coeffects

let remove_var_with_provenance free_vars var =
  let v = Backend_var.With_provenance.var var in
  Backend_var.Set.remove v free_vars

let remove_vars_with_machtype free_vars vars =
  List.fold_left
    (fun free_vars (cmm_var, _machtype) ->
      remove_var_with_provenance free_vars cmm_var)
    free_vars vars

let exttype_of_kind (k : Flambda_kind.t) : Cmm.exttype =
  match k with
  | Value -> XInt
  | Naked_number Naked_float -> XFloat
  | Naked_number Naked_int64 -> XInt64
  | Naked_number Naked_int32 -> XInt32
  | Naked_number (Naked_immediate | Naked_nativeint) -> (
    match Targetint_32_64.num_bits with
    | Thirty_two -> XInt32
    | Sixty_four -> XInt64)
  | Naked_number Naked_vec128 -> XVec128
  | Region -> Misc.fatal_error "[Region] kind not expected here"
  | Rec_info -> Misc.fatal_error "[Rec_info] kind not expected here"

let machtype_of_kind (kind : Flambda_kind.With_subkind.t) =
  match Flambda_kind.With_subkind.kind kind with
  | Value -> (
    match Flambda_kind.With_subkind.subkind kind with
    | Tagged_immediate -> Cmm.typ_int
    | Anything | Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
    | Boxed_vec128 | Variant _ | Float_block _ | Float_array | Immediate_array
    | Value_array | Generic_array ->
      Cmm.typ_val)
  | Naked_number Naked_float -> Cmm.typ_float
  | Naked_number Naked_vec128 -> Cmm.typ_vec128
  | Naked_number (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint)
    ->
    Cmm.typ_int
  | Region | Rec_info -> assert false

let extended_machtype_of_kind (kind : Flambda_kind.With_subkind.t) =
  match Flambda_kind.With_subkind.kind kind with
  | Value -> (
    match Flambda_kind.With_subkind.subkind kind with
    | Tagged_immediate -> Extended_machtype.typ_tagged_int
    | Anything | Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
    | Boxed_vec128 | Variant _ | Float_block _ | Float_array | Immediate_array
    | Value_array | Generic_array ->
      Extended_machtype.typ_val)
  | Naked_number Naked_float -> Extended_machtype.typ_float
  | Naked_number Naked_vec128 -> Extended_machtype.typ_vec128
  | Naked_number (Naked_immediate | Naked_int32 | Naked_int64 | Naked_nativeint)
    ->
    Extended_machtype.typ_any_int
  | Region | Rec_info -> assert false

let memory_chunk_of_kind (kind : Flambda_kind.With_subkind.t) : Cmm.memory_chunk
    =
  match Flambda_kind.With_subkind.kind kind with
  | Value -> (
    match Flambda_kind.With_subkind.subkind kind with
    | Tagged_immediate -> Word_int
    | Anything | Boxed_float | Boxed_int32 | Boxed_int64 | Boxed_nativeint
    | Boxed_vec128 | Variant _ | Float_block _ | Float_array | Immediate_array
    | Value_array | Generic_array ->
      Word_val)
  | Naked_number (Naked_int32 | Naked_int64 | Naked_nativeint | Naked_immediate)
    ->
    Word_int
  | Naked_number Naked_float -> Double
  | Naked_number Naked_vec128 ->
    (* 128-bit memory operations are default unaligned. Aligned (big)array
       operations are handled separately via cmm. *)
    Onetwentyeight_unaligned
  | Region | Rec_info ->
    Misc.fatal_errorf "Bad kind %a for [memory_chunk_of_kind]"
      Flambda_kind.With_subkind.print kind

let machtype_of_kinded_parameter p = Bound_parameter.kind p |> machtype_of_kind

let targetint ~dbg t =
  match Targetint_32_64.repr t with
  | Int32 i -> int32 ~dbg i
  | Int64 i -> int64 ~dbg i

let tag_targetint t = Targetint_32_64.(add (shift_left t 1) one)

(* We shouldn't really be converting to [nativeint] but the definition of the
   Cmm term language currently requires this. *)
let nativeint_of_targetint t =
  match Targetint_32_64.repr t with
  | Int32 i -> Nativeint.of_int32 i
  | Int64 i -> Int64.to_nativeint i

let name0 ?consider_inlining_effectful_expressions env res name =
  Name.pattern_match name
    ~var:(fun v ->
      To_cmm_env.inline_variable ?consider_inlining_effectful_expressions env
        res v)
    ~symbol:(fun s ->
      let sym = To_cmm_result.symbol res s in
      (* CR mshinwell: fix debuginfo? *)
      To_cmm_env.
        { env;
          res;
          expr =
            { cmm = symbol ~dbg:Debuginfo.none sym;
              free_vars = Backend_var.Set.empty;
              effs = Ece.pure_can_be_duplicated
            }
        })

let name env name = name0 env name

let const ~dbg cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i -> targetint ~dbg (Targetint_31_63.to_targetint i)
  | Tagged_immediate i ->
    targetint ~dbg (tag_targetint (Targetint_31_63.to_targetint i))
  | Naked_float f -> float ~dbg (Numeric_types.Float_by_bit_pattern.to_float f)
  | Naked_int32 i -> int32 ~dbg i
  | Naked_int64 i -> int64 ~dbg i
  | Naked_vec128 i ->
    let { Vector_types.Vec128.Bit_pattern.high; low } =
      Vector_types.Vec128.Bit_pattern.to_bits i
    in
    vec128 ~dbg { high; low }
  | Naked_nativeint t -> targetint ~dbg t

let simple ?consider_inlining_effectful_expressions ~dbg env res s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:_ ->
      name0 ?consider_inlining_effectful_expressions env res n)
    ~const:(fun c ->
      To_cmm_env.
        { env;
          res;
          expr =
            { cmm = const ~dbg c;
              free_vars = Backend_var.Set.empty;
              effs = Ece.pure_can_be_duplicated
            }
        })

let name_static res name =
  Name.pattern_match name
    ~var:(fun v -> `Var v)
    ~symbol:(fun s -> `Data [symbol_address (To_cmm_result.symbol res s)])

let const_static cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i ->
    [cint (nativeint_of_targetint (Targetint_31_63.to_targetint i))]
  | Tagged_immediate i ->
    [ cint
        (nativeint_of_targetint
           (tag_targetint (Targetint_31_63.to_targetint i))) ]
  | Naked_float f -> [cfloat (Numeric_types.Float_by_bit_pattern.to_float f)]
  | Naked_int32 i -> [cint (Nativeint.of_int32 i)]
  (* We don't compile flambda-backend in 32-bit mode, so nativeint is 64
     bits. *)
  | Naked_int64 i -> [cint (Int64.to_nativeint i)]
  | Naked_vec128 v ->
    let { Vector_types.Vec128.Bit_pattern.high; low } =
      Vector_types.Vec128.Bit_pattern.to_bits v
    in
    [cvec128 { high; low }]
  | Naked_nativeint t -> [cint (nativeint_of_targetint t)]

let simple_static res s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:_ -> name_static res n)
    ~const:(fun c -> `Data (const_static c))

let simple_list ?consider_inlining_effectful_expressions ~dbg env res l =
  (* Note that [To_cmm_primitive] relies on this function translating the
     [Simple] at the head of the list first. *)
  let aux (list, acc_free_vars, env, res, acc_effs) x =
    let To_cmm_env.{ env; res; expr = { cmm; free_vars; effs } } =
      simple ?consider_inlining_effectful_expressions ~dbg env res x
    in
    let free_vars = Backend_var.Set.union acc_free_vars free_vars in
    cmm :: list, free_vars, env, res, Ece.join acc_effs effs
  in
  let args, free_vars, env, res, effs =
    List.fold_left aux
      ([], Backend_var.Set.empty, env, res, Ece.pure_can_be_duplicated)
      l
  in
  List.rev args, free_vars, env, res, effs

let bound_parameters env l =
  let flambda_vars = Bound_parameters.vars l in
  let env, cmm_vars = To_cmm_env.create_bound_parameters env flambda_vars in
  let vars =
    List.map2
      (fun v v' -> v, machtype_of_kinded_parameter v')
      cmm_vars
      (Bound_parameters.to_list l)
  in
  env, vars

let invalid res ~message =
  let dbg = Debuginfo.none in
  let message_sym, res =
    match To_cmm_result.invalid_message_symbol res ~message with
    | None ->
      let message_sym =
        Symbol.create
          (Compilation_unit.get_current_exn ())
          (Linkage_name.of_string
             (Variable.unique_name (Variable.create "invalid")))
      in
      let res =
        Cmm_helpers.emit_string_constant
          (To_cmm_result.symbol res message_sym)
          message []
        |> To_cmm_result.add_archive_data_items res
      in
      let res =
        To_cmm_result.add_invalid_message_symbol res message_sym ~message
      in
      message_sym, res
    | Some message_sym -> message_sym, res
  in
  let call_expr =
    extcall ~dbg ~alloc:false ~is_c_builtin:false ~returns:false ~ty_args:[XInt]
      Cmm.caml_flambda2_invalid Cmm.typ_void
      [symbol ~dbg (To_cmm_result.symbol res message_sym)]
  in
  call_expr, res

let make_update env res dbg (kind : Cmm.memory_chunk) ~symbol var ~index
    ~prev_updates =
  let To_cmm_env.{ env; res; expr = { cmm; free_vars; effs } } =
    To_cmm_env.inline_variable env res var
  in
  let cmm =
    if Config.runtime5
    then
      let imm_or_ptr : Lambda.immediate_or_pointer =
        match kind with
        | Word_val -> Pointer
        | Byte_unsigned | Byte_signed | Sixteen_unsigned | Sixteen_signed
        | Thirtytwo_unsigned | Thirtytwo_signed | Word_int | Single | Double
        | Onetwentyeight_unaligned | Onetwentyeight_aligned ->
          Immediate
      in
      Cmm_helpers.setfield index imm_or_ptr Root_initialization symbol cmm dbg
    else
      let addr = field_address symbol index dbg in
      store ~dbg kind Initialization ~addr ~new_value:cmm
  in
  let update =
    match prev_updates with
    | None -> To_cmm_env.{ cmm; free_vars; effs }
    | Some (prev : To_cmm_env.expr_with_info) ->
      let cmm = sequence prev.cmm cmm in
      let free_vars = Backend_var.Set.union prev.free_vars free_vars in
      let effs = Ece.join prev.effs effs in
      To_cmm_env.{ cmm; free_vars; effs }
  in
  env, res, Some update

let check_arity arity args =
  Flambda_arity.cardinal_unarized arity = List.length args

let extended_machtype_of_return_arity arity =
  match Flambda_arity.unarized_components arity with
  | [] ->
    (* Functions that never return have arity 0. In that case, we use the most
       restrictive machtype to ensure that the return value of the function is
       not used. *)
    Extended_machtype.typ_void
  | [k] ->
    (* Regular functions with a single return value *)
    extended_machtype_of_kind k
  | arity ->
    (* Functions returning multiple values *)
    List.map extended_machtype_of_kind arity |> Array.concat
