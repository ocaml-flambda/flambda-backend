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

open! Flambda.Import

module C = struct
  include Cmm_helpers
  include To_cmm_shared
end

module SC = Static_const
module R = To_cmm_result

let static_value r v =
  match (v : Field_of_static_block.t) with
  | Symbol s -> To_cmm_result.static_symbol_address r s
  | Dynamically_computed _ -> C.cint 1n
  | Tagged_immediate i ->
    C.cint
      (C.nativeint_of_targetint
         (C.tag_targetint (Targetint_31_63.to_targetint' i)))

let or_variable f default v cont =
  match (v : _ Or_variable.t) with
  | Const c -> f c cont
  | Var _ -> f default cont

let rec static_block_updates symb env r acc i = function
  | [] -> env, acc
  | sv :: rem -> begin
    match (sv : Field_of_static_block.t) with
    | Symbol _ | Tagged_immediate _ ->
      static_block_updates symb env r acc (i + 1) rem
    | Dynamically_computed (var, dbg) ->
      let env, acc =
        C.make_update env dbg Word_val
          ~symbol:(R.expr_symbol_address r symb dbg)
          var ~index:i ~prev_updates:acc
      in
      static_block_updates symb env r acc (i + 1) rem
  end

let rec static_float_array_updates symb env r acc i = function
  | [] -> env, acc
  | sv :: rem -> begin
    match (sv : _ Or_variable.t) with
    | Const _ -> static_float_array_updates symb env r acc (i + 1) rem
    | Var (var, dbg) ->
      let env, acc =
        C.make_update env dbg Double
          ~symbol:(R.expr_symbol_address r symb dbg)
          var ~index:i ~prev_updates:acc
      in
      static_float_array_updates symb env r acc (i + 1) rem
  end

let static_boxed_number kind env symbol default emit transl v r updates =
  let aux x cont =
    emit
      (Symbol.linkage_name_as_string symbol, Cmmgen_state.Global)
      (transl x) cont
  in
  let updates =
    match (v : _ Or_variable.t) with
    | Const _ -> env, None
    | Var (v, dbg) ->
      C.make_update env dbg kind
        ~symbol:(R.expr_symbol_address r symbol dbg)
        v ~index:0 ~prev_updates:updates
  in
  R.update_data r (or_variable aux default v), updates

let add_function env r ~params_and_body code_id p ~fun_dbg =
  let fundecl, r = params_and_body env r code_id p ~fun_dbg in
  R.add_function r fundecl

let add_functions env ~params_and_body r (code : Code.t) =
  add_function env r ~params_and_body (Code.code_id code)
    (Code.params_and_body code)
    ~fun_dbg:(Code.dbg code)

let preallocate_set_of_closures (pass : To_cmm_set_of_closures.pass)
    (r, updates, env) ~closure_symbols set_of_closures =
  let env, r, data, updates =
    let closure_symbols =
      closure_symbols |> Function_slot.Lmap.bindings
      |> Function_slot.Map.of_list
    in
    To_cmm_set_of_closures.let_static_set_of_closures pass env r closure_symbols
      set_of_closures ~prev_updates:updates
  in
  let r = match pass with Offsets -> r | Data_items -> R.set_data r data in
  r, updates, env

let static_const_offsets0 env r ~updates (bound_static : Bound_static.Pattern.t)
    (static_const : Static_const.t) =
  match bound_static, static_const with
  | Block_like s, Block (_tag, _mut, fields) ->
    let is_module_symbol = R.is_module_symbol r s in
    let r =
      if is_module_symbol
      then r
      else
        R.record_symbol_offset r s
          ~size_in_words_excluding_header:(List.length fields)
    in
    env, r, updates
  | Set_of_closures closure_symbols, Set_of_closures set_of_closures ->
    let r, updates, env =
      preallocate_set_of_closures Offsets (r, updates, env) ~closure_symbols
        set_of_closures
    in
    env, r, updates
  | Block_like s, Boxed_float _ ->
    let r = R.record_symbol_offset r s ~size_in_words_excluding_header:1 in
    env, r, updates
  | Block_like s, Boxed_int32 _ ->
    let r = R.record_symbol_offset r s ~size_in_words_excluding_header:2 in
    env, r, updates
  | Block_like s, Boxed_int64 _ ->
    let r = R.record_symbol_offset r s ~size_in_words_excluding_header:2 in
    env, r, updates
  | Block_like s, Boxed_nativeint _ ->
    let r = R.record_symbol_offset r s ~size_in_words_excluding_header:2 in
    env, r, updates
  | Block_like s, (Immutable_float_block fields | Immutable_float_array fields)
    ->
    let r =
      R.record_symbol_offset r s
        ~size_in_words_excluding_header:(List.length fields)
    in
    env, r, updates
  | Block_like s, Empty_array ->
    let r = R.record_symbol_offset r s ~size_in_words_excluding_header:0 in
    env, r, updates
  | Block_like s, Mutable_string { initial_value = str }
  | Block_like s, Immutable_string str ->
    let r =
      R.record_symbol_offset r s
        ~size_in_words_excluding_header:((String.length str + 8) / 8)
    in
    env, r, updates
  | Block_like _, Set_of_closures _ ->
    Misc.fatal_errorf
      "[Set_of_closures] values cannot be bound by [Block_like] bindings:@ %a"
      SC.print static_const
  | ( (Code _ | Set_of_closures _),
      ( Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
      | Empty_array | Mutable_string _ | Immutable_string _ ) ) ->
    Misc.fatal_errorf
      "Block-like constants cannot be bound by [Code] or [Set_of_closures] \
       bindings:@ %a"
      SC.print static_const
  | Code _, Set_of_closures _ ->
    Misc.fatal_errorf "Sets of closures cannot be bound by [Code] bindings:@ %a"
      SC.print static_const

let static_const0 env r ~updates (bound_static : Bound_static.Pattern.t)
    (static_const : Static_const.t) =
  match bound_static, static_const with
  | Block_like s, Block (tag, _mut, fields) ->
    let r, is_module_symbol = R.check_for_module_symbol r s in
    let tag = Tag.Scannable.to_int tag in
    let block_name = Symbol.linkage_name_as_string s, Cmmgen_state.Global in
    let header = C.black_block_header tag (List.length fields) in
    let env, static_fields =
      List.fold_right
        (fun v (env, static_fields) ->
          let static_field = static_value r v in
          env, static_field :: static_fields)
        fields (env, [])
    in
    let block = C.emit_block block_name header static_fields in
    let env, updates = static_block_updates s env r updates 0 fields in
    env, R.set_data r block, updates, not is_module_symbol
  | Set_of_closures closure_symbols, Set_of_closures set_of_closures ->
    let r, updates, env =
      preallocate_set_of_closures Data_items (r, updates, env) ~closure_symbols
        set_of_closures
    in
    env, r, updates, true
  | Block_like s, Boxed_float v ->
    let default = Numeric_types.Float_by_bit_pattern.zero in
    let transl = Numeric_types.Float_by_bit_pattern.to_float in
    let r, (env, updates) =
      static_boxed_number Double env s default C.emit_float_constant transl v r
        updates
    in
    env, r, updates, true
  | Block_like s, Boxed_int32 v ->
    let r, (env, updates) =
      static_boxed_number Word_int env s 0l C.emit_int32_constant Fun.id v r
        updates
    in
    env, r, updates, true
  | Block_like s, Boxed_int64 v ->
    let r, (env, updates) =
      static_boxed_number Word_int env s 0L C.emit_int64_constant Fun.id v r
        updates
    in
    env, r, updates, true
  | Block_like s, Boxed_nativeint v ->
    let default = Targetint_32_64.zero in
    let transl = C.nativeint_of_targetint in
    let r, (env, updates) =
      static_boxed_number Word_int env s default C.emit_nativeint_constant
        transl v r updates
    in
    env, r, updates, true
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
    let env, e = static_float_array_updates s env r updates 0 fields in
    env, R.update_data r float_array, e, true
  | Block_like s, Empty_array ->
    (* Recall: empty arrays have tag zero, even if their kind is naked float. *)
    let block_name = Symbol.linkage_name_as_string s, Cmmgen_state.Global in
    let header = C.black_block_header 0 0 in
    let block = C.emit_block block_name header [] in
    env, R.set_data r block, updates, true
  | Block_like s, Mutable_string { initial_value = str }
  | Block_like s, Immutable_string str ->
    let name = Symbol.linkage_name_as_string s in
    let data = C.emit_string_constant (name, Cmmgen_state.Global) str in
    env, R.update_data r data, updates, true
  | Block_like _, Set_of_closures _ ->
    Misc.fatal_errorf
      "[Set_of_closures] values cannot be bound by [Block_like] bindings:@ %a"
      SC.print static_const
  | ( (Code _ | Set_of_closures _),
      ( Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _
      | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
      | Empty_array | Mutable_string _ | Immutable_string _ ) ) ->
    Misc.fatal_errorf
      "Block-like constants cannot be bound by [Code] or [Set_of_closures] \
       bindings:@ %a"
      SC.print static_const
  | Code _, Set_of_closures _ ->
    Misc.fatal_errorf "Sets of closures cannot be bound by [Code] bindings:@ %a"
      SC.print static_const

let static_const_or_code_offsets env r ~updates
    (bound_static : Bound_static.Pattern.t)
    (static_const_or_code : Static_const_or_code.t) =
  let env, r, updates =
    match bound_static, static_const_or_code with
    | Block_like _, Static_const static_const ->
      static_const_offsets0 env r ~updates bound_static static_const
    | Set_of_closures _, Static_const static_const ->
      static_const_offsets0 env r ~updates bound_static static_const
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
  let r =
    (* No data items should have been created. *)
    R.set_data r []
  in
  env, r, updates

let static_const_or_code env r ~updates (bound_static : Bound_static.Pattern.t)
    (static_const_or_code : Static_const_or_code.t) =
  let env, r, updates, is_offset_data =
    match bound_static, static_const_or_code with
    | Block_like _, Static_const static_const ->
      static_const0 env r ~updates bound_static static_const
    | Set_of_closures _, Static_const static_const ->
      static_const0 env r ~updates bound_static static_const
    | Code code_id, Code code ->
      if not (Code_id.equal code_id (Code.code_id code))
      then
        Misc.fatal_errorf "Code ID mismatch:@ %a@ =@ %a"
          Bound_static.Pattern.print bound_static Code.print code;
      (* Nothing needs doing here as we've already added the code to the
         environment. *)
      env, r, updates, false
    | Code _, Deleted_code -> env, r, updates, false
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
  let r =
    if is_offset_data then R.archive_offset_data r else R.archive_data r
  in
  env, r, updates

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
    (* There shouldn't be any data items pending at this point. *)
    R.set_data r []
  in
  let _env, r, _updates =
    ListLabels.fold_left2 bound_static' static_consts' ~init:(env, r, None)
      ~f:(fun (env, r, updates) bound_symbol_pat const ->
        static_const_or_code_offsets env r ~updates bound_symbol_pat const)
  in
  let env, r, updates =
    ListLabels.fold_left2 bound_static' static_consts' ~init:(env, r, None)
      ~f:(fun (env, r, updates) bound_symbol_pat const ->
        static_const_or_code env r ~updates bound_symbol_pat const)
  in
  let r =
    (* The function bodies must be translated after the static consts' data
       items have been emitted, otherwise the ordering of the data items might
       not match up with the assigned symbol offsets. *)
    ListLabels.fold_left static_consts' ~init:r ~f:(fun r static_const ->
        match Static_const_or_code.to_code static_const with
        | None -> r
        | Some code -> add_functions env ~params_and_body r code)
  in
  env, r, updates

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
