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

open! Flambda.Import

module C = struct
  include Cmm_helpers
  include To_cmm_shared
end

module SC = Static_const
module R = To_cmm_result

let static_value v =
  match (v : Field_of_static_block.t) with
  | Symbol s -> C.symbol_address (Symbol.linkage_name_as_string s)
  | Dynamically_computed _ -> C.cint 1n
  | Tagged_immediate i ->
    C.cint
      (C.nativeint_of_targetint
         (C.tag_targetint (Targetint_31_63.to_targetint i)))

let or_variable f default v cont =
  match (v : _ Or_variable.t) with
  | Const c -> f c cont
  | Var _ -> f default cont

let rec static_block_updates symb env acc i = function
  | [] -> env, acc
  | sv :: r -> (
    match (sv : Field_of_static_block.t) with
    | Symbol _ | Tagged_immediate _ ->
      static_block_updates symb env acc (i + 1) r
    | Dynamically_computed (var, dbg) ->
      let env, acc =
        C.make_update env dbg Word_val ~symbol:(C.symbol ~dbg symb) var ~index:i
          ~prev_updates:acc
      in
      static_block_updates symb env acc (i + 1) r)

let rec static_float_array_updates symb env acc i = function
  | [] -> env, acc
  | sv :: r -> (
    match (sv : _ Or_variable.t) with
    | Const _ -> static_float_array_updates symb env acc (i + 1) r
    | Var (var, dbg) ->
      let env, acc =
        C.make_update env dbg Double ~symbol:(C.symbol ~dbg symb) var ~index:i
          ~prev_updates:acc
      in
      static_float_array_updates symb env acc (i + 1) r)

let static_boxed_number ~kind ~env ~symbol ~default ~emit ~transl ~structured v
    r updates =
  let aux x cont =
    emit
      (Symbol.linkage_name_as_string symbol, Cmmgen_state.Global)
      (transl x) cont
  in
  let updates =
    match (v : _ Or_variable.t) with
    | Const c ->
      (* Add the const to the cmmgen_state structured constants table so that
         functions in cmm_helpers can short-circuit Unboxing of boxed constant
         symbols, particularly in Classic mode. *)
      let symbol_name = Symbol.linkage_name_as_string symbol in
      let structured_constant = structured (transl c) in
      Cmmgen_state.add_structured_constant symbol_name structured_constant;
      env, None
    | Var (v, dbg) ->
      C.make_update env dbg kind ~symbol:(C.symbol ~dbg symbol) v ~index:0
        ~prev_updates:updates
  in
  R.update_data r (or_variable aux default v), updates

let add_function env r ~params_and_body code_id p ~fun_dbg =
  let fundecl, r = params_and_body env r code_id p ~fun_dbg in
  R.add_function r fundecl

let add_functions env ~params_and_body r (code : Code.t) =
  add_function env r ~params_and_body (Code.code_id code)
    (Code.params_and_body code)
    ~fun_dbg:(Code.dbg code)

let preallocate_set_of_closures (r, updates, env) ~closure_symbols
    set_of_closures =
  let env, data, updates =
    let closure_symbols =
      closure_symbols |> Function_slot.Lmap.bindings
      |> Function_slot.Map.of_list
    in
    To_cmm_set_of_closures.let_static_set_of_closures env closure_symbols
      set_of_closures ~prev_updates:updates
  in
  let r = R.set_data r data in
  r, updates, env

let static_const0 env r ~updates (bound_static : Bound_static.Pattern.t)
    (static_const : Static_const.t) =
  match bound_static, static_const with
  | Block_like s, Block (tag, _mut, fields) ->
    let r = R.check_for_module_symbol r s in
    let tag = Tag.Scannable.to_int tag in
    let block_name = Symbol.linkage_name_as_string s, Cmmgen_state.Global in
    let header = C.black_block_header tag (List.length fields) in
    let static_fields =
      List.fold_right
        (fun v static_fields ->
          let static_field = static_value v in
          static_field :: static_fields)
        fields []
    in
    let block = C.emit_block block_name header static_fields in
    let env, updates = static_block_updates s env updates 0 fields in
    env, R.set_data r block, updates
  | Set_of_closures closure_symbols, Set_of_closures set_of_closures ->
    let r, updates, env =
      preallocate_set_of_closures (r, updates, env) ~closure_symbols
        set_of_closures
    in
    env, r, updates
  | Block_like symbol, Boxed_float v ->
    let default = Numeric_types.Float_by_bit_pattern.zero in
    let transl = Numeric_types.Float_by_bit_pattern.to_float in
    let structured f = Clambda.Uconst_float f in
    let r, (env, updates) =
      static_boxed_number ~kind:Double ~env ~symbol ~default
        ~emit:C.emit_float_constant ~transl ~structured v r updates
    in
    env, r, updates
  | Block_like symbol, Boxed_int32 v ->
    let structured i = Clambda.Uconst_int32 i in
    let r, (env, updates) =
      static_boxed_number ~kind:Word_int ~env ~symbol ~default:0l
        ~emit:C.emit_int32_constant ~transl:Fun.id ~structured v r updates
    in
    env, r, updates
  | Block_like symbol, Boxed_int64 v ->
    let structured i = Clambda.Uconst_int64 i in
    let r, (env, updates) =
      static_boxed_number ~kind:Word_int ~env ~symbol ~default:0L
        ~emit:C.emit_int64_constant ~transl:Fun.id ~structured v r updates
    in
    env, r, updates
  | Block_like symbol, Boxed_nativeint v ->
    let default = Targetint_32_64.zero in
    let transl = C.nativeint_of_targetint in
    let structured i = Clambda.Uconst_nativeint i in
    let r, (env, updates) =
      static_boxed_number ~kind:Word_int ~env ~symbol ~default
        ~emit:C.emit_nativeint_constant ~transl ~structured v r updates
    in
    env, r, updates
  | Block_like s, (Immutable_float_block fields | Immutable_float_array fields)
    ->
    let aux =
      Or_variable.value_map ~default:0.
        ~f:Numeric_types.Float_by_bit_pattern.to_float
    in
    let static_fields = List.map aux fields in
    let float_array =
      C.emit_float_array_constant
        (Symbol.linkage_name_as_string s, Cmmgen_state.Global)
        static_fields
    in
    let env, e = static_float_array_updates s env updates 0 fields in
    env, R.update_data r float_array, e
  | Block_like s, Immutable_value_array fields ->
    let block_name = Symbol.linkage_name_as_string s, Cmmgen_state.Global in
    let header = C.black_block_header 0 (List.length fields) in
    let static_fields =
      List.fold_right
        (fun v static_fields ->
          let static_field = static_value v in
          static_field :: static_fields)
        fields []
    in
    let block = C.emit_block block_name header static_fields in
    let env, updates = static_block_updates s env updates 0 fields in
    env, R.set_data r block, updates
  | Block_like s, Empty_array ->
    (* Recall: empty arrays have tag zero, even if their kind is naked float. *)
    let block_name = Symbol.linkage_name_as_string s, Cmmgen_state.Global in
    let header = C.black_block_header 0 0 in
    let block = C.emit_block block_name header [] in
    env, R.set_data r block, updates
  | Block_like s, Mutable_string { initial_value = str }
  | Block_like s, Immutable_string str ->
    let name = Symbol.linkage_name_as_string s in
    let data = C.emit_string_constant (name, Cmmgen_state.Global) str in
    env, R.update_data r data, updates
  | Block_like _, Set_of_closures _ ->
    Misc.fatal_errorf
      "[Set_of_closures] values cannot be bound by [Block_like] bindings:@ %a"
      SC.print static_const
  | ( (Code _ | Set_of_closures _),
      ( Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
      | Immutable_value_array _ | Empty_array | Mutable_string _
      | Immutable_string _ ) ) ->
    Misc.fatal_errorf
      "Block-like constants cannot be bound by [Code] or [Set_of_closures] \
       bindings:@ %a"
      SC.print static_const
  | Code _, Set_of_closures _ ->
    Misc.fatal_errorf "Sets of closures cannot be bound by [Code] bindings:@ %a"
      SC.print static_const

let static_const_or_code env r ~updates (bound_static : Bound_static.Pattern.t)
    (static_const_or_code : Static_const_or_code.t) =
  let env, r, updates =
    match bound_static, static_const_or_code with
    | (Block_like _ | Set_of_closures _), Static_const static_const ->
      static_const0 env r ~updates bound_static static_const
    | Code code_id, Code code ->
      if not (Code_id.equal code_id (Code.code_id code))
      then
        Misc.fatal_errorf "Code ID mismatch:@ %a@ =@ %a"
          Bound_static.Pattern.print bound_static Code.print code;
      (* Nothing needs doing here as we've already added the code to the
         environment. *)
      env, r, updates
    | Code _, Deleted_code -> env, r, updates
    | Code _, Static_const static_const ->
      Misc.fatal_errorf "Only code can be bound by [Code] bindings:@ %a@ =@ %a"
        Bound_static.Pattern.print bound_static SC.print static_const
    | (Set_of_closures _ | Block_like _), Code code ->
      Misc.fatal_errorf
        "Pieces of code cannot be bound by [Block_like] or [Set_of_closures] \
         bindings:@ %a@ =@ %a"
        Bound_static.Pattern.print bound_static Code.print code
    | (Set_of_closures _ | Block_like _), Deleted_code ->
      Misc.fatal_errorf
        "Deleted code cannot be bound by [Block_like] or [Set_of_closures] \
         bindings:@ %a@ =@ <deleted code>"
        Bound_static.Pattern.print bound_static
  in
  env, R.archive_data r, updates

let static_consts0 env r ~params_and_body bound_static static_consts =
  (* We cannot both build the environment and compile any functions in one
     traversal, as the bodies may contain direct calls to the code IDs being
     defined. *)
  let static_consts' = Static_const_group.to_list static_consts in
  let bound_static' = Bound_static.to_list bound_static in
  if not (List.compare_lengths bound_static' static_consts' = 0)
  then
    Misc.fatal_errorf
      "Mismatch between [Bound_static] and [Static_const]s:@ %a@ =@ %a"
      Bound_static.print bound_static Static_const_group.print static_consts;
  let r =
    ListLabels.fold_left static_consts' ~init:r ~f:(fun r static_const ->
        match Static_const_or_code.to_code static_const with
        | None -> r
        | Some code -> add_functions env ~params_and_body r code)
  in
  ListLabels.fold_left2 bound_static' static_consts' ~init:(env, r, None)
    ~f:(fun (env, r, updates) bound_symbol_pat const ->
      static_const_or_code env r ~updates bound_symbol_pat const)

let static_consts env r ~params_and_body bound_static static_consts =
  try
    (* Gc roots: statically allocated blocks themselves do not need to be
       scanned, however if statically allocated blocks contain dynamically
       allocated contents, then that block has to be registered as Gc roots for
       the Gc to correctly patch it if/when it moves some of the dynamically
       allocated blocks. As a safe over-approximation, we thus register as
       gc_roots all symbols who have an associated computation (and thus are not
       fully_static). *)
    let roots =
      if Static_const_group.is_fully_static static_consts
      then []
      else Bound_static.gc_roots bound_static
    in
    let r = R.add_gc_roots r roots in
    static_consts0 env r ~params_and_body bound_static static_consts
  with Misc.Fatal_error as e ->
    (* Create a new "let symbol" with a dummy body to better print the bound
       symbols and static consts. *)
    let dummy_body = Expr.create_invalid To_cmm_dummy_body in
    let tmp_let_symbol =
      Let.create
        (Bound_pattern.static bound_static)
        (Named.create_static_consts static_consts)
        ~body:dummy_body ~free_names_of_body:(Known Name_occurrences.empty)
      |> Expr.create_let
    in
    Format.eprintf
      "\n@[<v 0>%sContext is:%s translating `let symbol' to Cmm:@ %a@."
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      Expr.print tmp_let_symbol;
    raise e
