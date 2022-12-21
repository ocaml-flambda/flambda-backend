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
  | Region -> Misc.fatal_error "[Region] kind not expected here"
  | Rec_info -> Misc.fatal_error "[Rec_info] kind not expected here"

let machtype_of_kind (k : Flambda_kind.t) =
  match k with
  | Value -> Cmm.typ_val
  | Naked_number Naked_float -> Cmm.typ_float
  | Naked_number Naked_int64 -> typ_int64
  | Naked_number (Naked_immediate | Naked_int32 | Naked_nativeint) ->
    Cmm.typ_int
  | Region | Rec_info -> assert false

let machtype_of_kinded_parameter p =
  Bound_parameter.kind p |> Flambda_kind.With_subkind.kind |> machtype_of_kind

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

let symbol_from_linkage_name ~dbg ln =
  symbol_from_string ~dbg (Linkage_name.to_string ln)

let symbol ~dbg sym = symbol_from_linkage_name ~dbg (Symbol.linkage_name sym)

let name0 ?consider_inlining_effectful_expressions env res name =
  Name.pattern_match name
    ~var:(fun v ->
      To_cmm_env.inline_variable ?consider_inlining_effectful_expressions env
        res v)
    ~symbol:(fun s ->
      (* CR mshinwell: fix debuginfo? *)
      symbol ~dbg:Debuginfo.none s, env, res, Ece.pure_can_be_duplicated)

let name env name = name0 env name

let const ~dbg cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i -> targetint ~dbg (Targetint_31_63.to_targetint i)
  | Tagged_immediate i ->
    targetint ~dbg (tag_targetint (Targetint_31_63.to_targetint i))
  | Naked_float f -> float ~dbg (Numeric_types.Float_by_bit_pattern.to_float f)
  | Naked_int32 i -> int32 ~dbg i
  | Naked_int64 i -> int64 ~dbg i
  | Naked_nativeint t -> targetint ~dbg t

let simple ?consider_inlining_effectful_expressions ~dbg env res s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:_ ->
      name0 ?consider_inlining_effectful_expressions env res n)
    ~const:(fun c -> const ~dbg c, env, res, Ece.pure_can_be_duplicated)

let name_static name =
  Name.pattern_match name
    ~var:(fun v -> `Var v)
    ~symbol:(fun s -> `Data [symbol_address (Symbol.linkage_name_as_string s)])

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
  | Naked_int64 i -> [cint (Int64.to_nativeint i)]
  | Naked_nativeint t -> [cint (nativeint_of_targetint t)]

let simple_static s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:_ -> name_static n)
    ~const:(fun c -> `Data (const_static c))

let simple_list ?consider_inlining_effectful_expressions ~dbg env res l =
  (* Note that [To_cmm_primitive] relies on this function translating the
     [Simple] at the head of the list first. *)
  let aux (list, env, res, effs) x =
    let y, env, res, eff =
      simple ?consider_inlining_effectful_expressions ~dbg env res x
    in
    y :: list, env, res, Ece.join eff effs
  in
  let args, env, res, effs =
    List.fold_left aux ([], env, res, Ece.pure_can_be_duplicated) l
  in
  List.rev args, env, res, effs

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
          (Symbol.linkage_name_as_string message_sym, Global)
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
      "caml_flambda2_invalid" Cmm.typ_void
      [symbol ~dbg message_sym]
  in
  call_expr, res

let make_update env res dbg kind ~symbol var ~index ~prev_updates =
  let e, env, res, _ece = To_cmm_env.inline_variable env res var in
  let addr = field_address symbol index dbg in
  let update = store ~dbg kind Initialization ~addr ~new_value:e in
  match prev_updates with
  | None -> env, res, Some update
  | Some prev_updates -> env, res, Some (sequence prev_updates update)

let check_arity arity args =
  Flambda_arity.With_subkinds.cardinal arity = List.length args

let machtype_of_return_arity arity =
  (* Functions that never return have arity 0. In that case, we use the most
     restrictive machtype to ensure that the return value of the function is not
     used. *)
  match Flambda_arity.to_list arity with
  | [] -> Cmm.typ_void
  (* Regular functions with a single return value *)
  | [k] -> machtype_of_kind k
  | _ ->
    (* CR gbury: update when unboxed tuples are used *)
    Misc.fatal_errorf "Functions are currently limited to a single return value"
