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

let static_value res v =
  match (v : Field_of_static_block.t) with
  | Symbol s -> C.symbol_address (R.symbol res s)
  | Dynamically_computed _ -> C.cint 1n
  | Tagged_immediate i ->
    C.cint
      (C.nativeint_of_targetint
         (C.tag_targetint (Targetint_31_63.to_targetint i)))

let or_variable f default v cont =
  match (v : _ Or_variable.t) with
  | Const c -> f c cont
  | Var _ -> f default cont

let rec static_block_updates symb env res acc i = function
  | [] -> env, res, acc
  | sv :: r -> (
    match (sv : Field_of_static_block.t) with
    | Symbol _ | Tagged_immediate _ ->
      static_block_updates symb env res acc (i + 1) r
    | Dynamically_computed (var, dbg) ->
      let env, res, acc =
        C.make_update env res dbg Word_val ~symbol:(C.symbol ~dbg symb) var
          ~index:i ~prev_updates:acc
      in
      static_block_updates symb env res acc (i + 1) r)

type maybe_int32 =
  | Int32
  | Int64_or_nativeint

(* The index [i] is always in the units of the size of the integer concerned,
   not units of 64-bit words. *)
let rec static_unboxed_int_array_updates symb env res acc maybe_int32 i =
  function
  | [] -> env, res, acc
  | sv :: r -> (
    match (sv : _ Or_variable.t) with
    | Const _ ->
      static_unboxed_int_array_updates symb env res acc maybe_int32 (i + 1) r
    | Var (var, dbg) ->
      let kind : C.update_kind =
        match maybe_int32 with
        | Int64_or_nativeint -> Word_int
        | Int32 -> Thirtytwo_signed
      in
      let env, res, acc =
        C.make_update env res dbg kind ~symbol:(C.symbol ~dbg symb) var ~index:i
          ~prev_updates:acc
      in
      static_unboxed_int_array_updates symb env res acc maybe_int32 (i + 1) r)

let rec static_float_array_updates symb env res acc i = function
  | [] -> env, res, acc
  | sv :: r -> (
    match (sv : _ Or_variable.t) with
    | Const _ -> static_float_array_updates symb env res acc (i + 1) r
    | Var (var, dbg) ->
      let env, res, acc =
        C.make_update env res dbg Double ~symbol:(C.symbol ~dbg symb) var
          ~index:i ~prev_updates:acc
      in
      static_float_array_updates symb env res acc (i + 1) r)

let static_boxed_number ~kind ~env ~symbol ~default ~emit ~transl ~structured v
    res updates =
  let symbol = R.symbol res symbol in
  let aux x cont = emit symbol (transl x) cont in
  let env, res, updates =
    match (v : _ Or_variable.t) with
    | Const c ->
      (* Add the const to the cmmgen_state structured constants table so that
         functions in cmm_helpers can short-circuit Unboxing of boxed constant
         symbols, particularly in Classic mode. *)
      let structured_constant = structured (transl c) in
      Cmmgen_state.add_structured_constant symbol structured_constant;
      env, res, updates
    | Var (v, dbg) ->
      C.make_update env res dbg kind ~symbol:(C.symbol ~dbg symbol) v ~index:0
        ~prev_updates:updates
  in
  R.update_data res (or_variable aux default v), env, updates

let add_function env res ~params_and_body code_id p ~fun_dbg ~check =
  let fundecl, res = params_and_body env res code_id p ~fun_dbg ~check in
  R.add_function res fundecl

let add_functions env ~params_and_body res (code : Code.t) =
  add_function env res ~params_and_body (Code.code_id code)
    (Code.params_and_body code)
    ~fun_dbg:(Code.dbg code) ~check:(Code.check code)

let preallocate_set_of_closures (res, updates, env) ~closure_symbols
    set_of_closures =
  let env, res, data, updates =
    let closure_symbols =
      closure_symbols |> Function_slot.Lmap.bindings
      |> Function_slot.Map.of_list
    in
    To_cmm_set_of_closures.let_static_set_of_closures env res closure_symbols
      set_of_closures ~prev_updates:updates
  in
  let res = R.set_data res data in
  res, updates, env

let immutable_unboxed_int_array_payload update_kind num_fields ~elts ~to_int64 =
  let int64_of_elts =
    List.map (Or_variable.value_map ~default:0L ~f:to_int64) elts
  in
  let packed_int64s =
    match update_kind with
    | Int32 ->
      let rec aux acc = function
        | [] -> List.rev acc
        | a :: [] -> List.rev (a :: acc)
        | a :: b :: r ->
          let i = Int64.(add (logand a 0xffffffffL) (shift_left b 32)) in
          aux (i :: acc) r
      in
      aux [] int64_of_elts
    | Int64_or_nativeint -> int64_of_elts
  in
  assert (List.length packed_int64s = num_fields);
  List.map (fun i -> Cmm.Cint (Int64.to_nativeint i)) packed_int64s

let immutable_unboxed_int_array env res updates update_kind ~symbol ~elts
    ~to_int64 ~custom_ops_symbol =
  let sym = R.symbol res symbol in
  let num_elts = List.length elts in
  let num_fields =
    match update_kind with
    | Int32 -> (1 + num_elts) / 2
    | Int64_or_nativeint -> num_elts
  in
  let header =
    C.black_custom_header
      ~size:(1 (* for the custom_operations pointer *) + num_fields)
  in
  let static_fields =
    let sym_base, sym_off = custom_ops_symbol ~num_elts in
    let address =
      match sym_off with
      | None -> C.symbol_address (Cmm.global_symbol sym_base)
      | Some sym_off -> C.symbol_offset (Cmm.global_symbol sym_base) sym_off
    in
    address
    :: immutable_unboxed_int_array_payload update_kind num_fields ~elts
         ~to_int64
  in
  let block = C.emit_block sym header static_fields in
  let env, res, updates =
    static_unboxed_int_array_updates sym env res updates update_kind 0 elts
  in
  env, R.set_data res block, updates

let static_const0 env res ~updates (bound_static : Bound_static.Pattern.t)
    (static_const : Static_const.t) =
  match bound_static, static_const with
  | Block_like s, Block (tag, _mut, fields) ->
    let sym = R.symbol res s in
    let res = R.check_for_module_symbol res s in
    let tag = Tag.Scannable.to_int tag in
    let header = C.black_block_header tag (List.length fields) in
    let static_fields =
      List.fold_right
        (fun v static_fields ->
          let static_field = static_value res v in
          static_field :: static_fields)
        fields []
    in
    let block = C.emit_block sym header static_fields in
    let env, res, updates = static_block_updates sym env res updates 0 fields in
    env, R.set_data res block, updates
  | Set_of_closures closure_symbols, Set_of_closures set_of_closures ->
    let res, updates, env =
      preallocate_set_of_closures (res, updates, env) ~closure_symbols
        set_of_closures
    in
    env, res, updates
  | Block_like symbol, Boxed_float v ->
    let default = Numeric_types.Float_by_bit_pattern.zero in
    let transl = Numeric_types.Float_by_bit_pattern.to_float in
    let structured f = Cmmgen_state.Const_float f in
    let res, env, updates =
      static_boxed_number ~kind:Double ~env ~symbol ~default
        ~emit:C.emit_float_constant ~transl ~structured v res updates
    in
    env, res, updates
  | Block_like symbol, Boxed_int32 v ->
    let structured i = Cmmgen_state.Const_int32 i in
    let res, env, updates =
      static_boxed_number ~kind:Word_int ~env ~symbol ~default:0l
        ~emit:C.emit_int32_constant ~transl:Fun.id ~structured v res updates
    in
    env, res, updates
  | Block_like symbol, Boxed_int64 v ->
    let structured i = Cmmgen_state.Const_int64 i in
    let res, env, updates =
      static_boxed_number ~kind:Word_int ~env ~symbol ~default:0L
        ~emit:C.emit_int64_constant ~transl:Fun.id ~structured v res updates
    in
    env, res, updates
  | Block_like symbol, Boxed_nativeint v ->
    let default = Targetint_32_64.zero in
    let transl = C.nativeint_of_targetint in
    let structured i = Cmmgen_state.Const_nativeint i in
    let res, env, updates =
      static_boxed_number ~kind:Word_int ~env ~symbol ~default
        ~emit:C.emit_nativeint_constant ~transl ~structured v res updates
    in
    env, res, updates
  | Block_like symbol, Boxed_vec128 v ->
    let default = Vector_types.Vec128.Bit_pattern.zero in
    let transl v =
      let { Vector_types.Vec128.Bit_pattern.high; low } =
        Vector_types.Vec128.Bit_pattern.to_bits v
      in
      { Cmm.high; low }
    in
    let structured { Cmm.high; low } =
      Cmmgen_state.Const_vec128 { high; low }
    in
    let res, env, updates =
      (* Unaligned because boxed vec128 constants are not aligned during code
         emission. Aligning them would complicate block layout. *)
      static_boxed_number ~kind:Onetwentyeight_unaligned ~env ~symbol ~default
        ~emit:C.emit_vec128_constant ~transl ~structured v res updates
    in
    env, res, updates
  | Block_like s, (Immutable_float_block fields | Immutable_float_array fields)
    ->
    let aux =
      Or_variable.value_map ~default:0.
        ~f:Numeric_types.Float_by_bit_pattern.to_float
    in
    let static_fields = List.map aux fields in
    let sym = R.symbol res s in
    let float_array = C.emit_float_array_constant sym static_fields in
    let env, res, e = static_float_array_updates sym env res updates 0 fields in
    env, R.update_data res float_array, e
  | Block_like symbol, Immutable_int32_array elts ->
    assert (Arch.size_int = 8);
    immutable_unboxed_int_array env res updates Int32 ~symbol ~elts
      ~to_int64:Int64.of_int32 ~custom_ops_symbol:(fun ~num_elts ->
<<<<<<< HEAD
        ( "caml_unboxed_int32_array_ops",
          Some (C.custom_ops_size * (num_elts mod 2)) ))
  | Block_like symbol, Immutable_int64_array elts ->
    immutable_unboxed_int_array env res updates Int64_or_nativeint ~symbol ~elts
      ~to_int64:Fun.id ~custom_ops_symbol:(fun ~num_elts:_ ->
        "caml_unboxed_int64_array_ops", None)
  | Block_like symbol, Immutable_nativeint_array elts ->
    immutable_unboxed_int_array env res updates Int64_or_nativeint ~symbol ~elts
      ~to_int64:Targetint_32_64.to_int64 ~custom_ops_symbol:(fun ~num_elts:_ ->
        "caml_unboxed_nativeint_array_ops", None)
=======
        if num_elts mod 2 = 0
        then "caml_unboxed_int32_even_array_ops"
        else "caml_unboxed_int32_odd_array_ops")
  | Block_like symbol, Immutable_int64_array elts ->
    immutable_unboxed_int_array env res updates Int64_or_nativeint ~symbol ~elts
      ~to_int64:Fun.id ~custom_ops_symbol:(fun ~num_elts:_ ->
        "caml_unboxed_int64_array_ops")
  | Block_like symbol, Immutable_nativeint_array elts ->
    immutable_unboxed_int_array env res updates Int64_or_nativeint ~symbol ~elts
      ~to_int64:Targetint_32_64.to_int64 ~custom_ops_symbol:(fun ~num_elts:_ ->
        "caml_unboxed_nativeint_array_ops")
>>>>>>> main
  | Block_like s, Immutable_value_array fields ->
    let sym = R.symbol res s in
    let header = C.black_block_header 0 (List.length fields) in
    let static_fields =
      List.fold_right
        (fun v static_fields ->
          let static_field = static_value res v in
          static_field :: static_fields)
        fields []
    in
    let block = C.emit_block sym header static_fields in
    let env, res, updates = static_block_updates sym env res updates 0 fields in
    env, R.set_data res block, updates
  | Block_like s, Empty_array Values_or_immediates_or_naked_floats ->
    (* Recall: empty arrays have tag zero, even if their kind is naked float. *)
    let sym = R.symbol res s in
    let header = C.black_block_header 0 0 in
    let block = C.emit_block sym header [] in
    env, R.set_data res block, updates
  | Block_like s, Empty_array Naked_int32s ->
    let block =
      C.emit_block (R.symbol res s)
        (C.black_custom_header ~size:1)
<<<<<<< HEAD
        [C.symbol_address (Cmm.global_symbol "caml_unboxed_int32_array_ops")]
=======
        [ C.symbol_address
            (Cmm.global_symbol "caml_unboxed_int32_even_array_ops") ]
>>>>>>> main
    in
    env, R.set_data res block, updates
  | Block_like s, Empty_array Naked_int64s ->
    let block =
      C.emit_block (R.symbol res s)
        (C.black_custom_header ~size:1)
        [C.symbol_address (Cmm.global_symbol "caml_unboxed_int64_array_ops")]
    in
    env, R.set_data res block, updates
  | Block_like s, Empty_array Naked_nativeints ->
    let block =
      C.emit_block (R.symbol res s)
        (C.black_custom_header ~size:1)
        [C.symbol_address (Cmm.global_symbol "caml_unboxed_nativeint_array_ops")]
    in
    env, R.set_data res block, updates
  | Block_like s, Mutable_string { initial_value = str }
  | Block_like s, Immutable_string str ->
    let data = C.emit_string_constant (R.symbol res s) str in
    env, R.update_data res data, updates
  | Block_like _, Set_of_closures _ ->
    Misc.fatal_errorf
      "[Set_of_closures] values cannot be bound by [Block_like] bindings:@ %a"
      SC.print static_const
  | ( (Code _ | Set_of_closures _),
      ( Block _ | Boxed_float _ | Boxed_int32 _ | Boxed_int64 _ | Boxed_vec128 _
      | Boxed_nativeint _ | Immutable_float_block _ | Immutable_float_array _
      | Immutable_int32_array _ | Immutable_int64_array _
      | Immutable_nativeint_array _ | Immutable_value_array _ | Empty_array _
      | Mutable_string _ | Immutable_string _ ) ) ->
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
    let bt = Printexc.get_raw_backtrace () in
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
      "\n@[<v 0>%tContext is:%t translating `let symbol' to Cmm:@ %a@."
      Flambda_colours.error Flambda_colours.pop Expr.print tmp_let_symbol;
    Printexc.raise_with_backtrace e bt
