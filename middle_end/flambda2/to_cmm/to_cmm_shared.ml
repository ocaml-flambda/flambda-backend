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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Cmm_helpers
module Ece = Effects_and_coeffects

let unsupported_32_bit () =
  Misc.fatal_errorf
    "Flambda 2 does not currently support compilation to 32-bit architectures"

let exttype_of_kind (k : Flambda_kind.t) : Cmm.exttype =
  match k with
  | Value -> XInt
  | Naked_number Naked_float -> XFloat
  | Naked_number Naked_int64 -> XInt64
  | Naked_number Naked_int32 -> XInt32
  | Naked_number (Naked_immediate | Naked_nativeint) -> begin
    match Targetint_32_64.num_bits with
    | Thirty_two -> XInt32
    | Sixty_four -> XInt64
  end
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

let name env name =
  Name.pattern_match name
    ~var:(fun v -> To_cmm_env.inline_variable env v)
    ~symbol:(fun s ->
      (* CR mshinwell: fix debuginfo? *)
      symbol ~dbg:Debuginfo.none s, env, Ece.pure)

let const ~dbg cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i -> targetint ~dbg (Targetint_31_63.to_targetint' i)
  | Tagged_immediate i ->
    targetint ~dbg (tag_targetint (Targetint_31_63.to_targetint' i))
  | Naked_float f -> float ~dbg (Numeric_types.Float_by_bit_pattern.to_float f)
  | Naked_int32 i -> int32 ~dbg i
  | Naked_int64 i -> int64 ~dbg i
  | Naked_nativeint t -> targetint ~dbg t

let simple ~dbg env s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:_ -> name env n)
    ~const:(fun c -> const ~dbg c, env, Ece.pure)

let name_static name =
  Name.pattern_match name
    ~var:(fun v -> `Var v)
    ~symbol:(fun s -> `Data [symbol_address (Symbol.linkage_name_as_string s)])

let const_static cst =
  match Reg_width_const.descr cst with
  | Naked_immediate i ->
    [cint (nativeint_of_targetint (Targetint_31_63.to_targetint' i))]
  | Tagged_immediate i ->
    [ cint
        (nativeint_of_targetint
           (tag_targetint (Targetint_31_63.to_targetint' i))) ]
  | Naked_float f -> [cfloat (Numeric_types.Float_by_bit_pattern.to_float f)]
  | Naked_int32 i -> [cint (Nativeint.of_int32 i)]
  | Naked_int64 i ->
    (* On 32-bit architectures, int64 values have to be split. *)
    if Target_system.is_32_bit
    then unsupported_32_bit ()
    else [cint (Int64.to_nativeint i)]
  | Naked_nativeint t -> [cint (nativeint_of_targetint t)]

let simple_static s =
  Simple.pattern_match s
    ~name:(fun n ~coercion:_ -> name_static n)
    ~const:(fun c -> `Data (const_static c))

let simple_list ~dbg env l =
  let aux (list, env, effs) x =
    let y, env, eff = simple ~dbg env x in
    y :: list, env, Ece.join eff effs
  in
  let args, env, effs = List.fold_left aux ([], env, Ece.pure) l in
  List.rev args, env, effs

let bound_parameters env l =
  let flambda_vars = Bound_parameters.vars l in
  let env, cmm_vars = To_cmm_env.create_variables env flambda_vars in
  let vars =
    List.map2
      (fun v v' -> v, machtype_of_kinded_parameter v')
      cmm_vars
      (Bound_parameters.to_list l)
  in
  env, vars

let invalid res ~message =
  let dbg = Debuginfo.none in
  let message_sym =
    Symbol.create
      (Compilation_unit.get_current_exn ())
      (Linkage_name.create (Variable.unique_name (Variable.create "invalid")))
  in
  let data_items =
    Cmm_helpers.emit_string_constant
      (Symbol.linkage_name_as_string message_sym, Global)
      message []
    |> To_cmm_result.add_archive_data_items res
  in
  let call_expr =
    extcall ~dbg ~alloc:false ~is_c_builtin:false ~returns:false ~ty_args:[XInt]
      "caml_flambda2_invalid" Cmm.typ_void [symbol ~dbg message_sym]
  in
  call_expr, data_items

let make_update env dbg kind ~symbol var ~index ~prev_updates =
  let e, env, _ece = To_cmm_env.inline_variable env var in
  let addr = field_address symbol index dbg in
  let update = store ~dbg kind Initialization ~addr ~new_value:e in
  match prev_updates with
  | None -> env, Some update
  | Some prev_updates -> env, Some (sequence prev_updates update)
