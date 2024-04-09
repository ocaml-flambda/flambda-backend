(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSDE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import

let create_static_const dacc dbg (to_lift : T.to_lift) : RSC.t =
  let[@inline always] convert_fields fields =
    ListLabels.map fields ~f:(fun field ->
        let module F = Field_of_static_block in
        Simple.pattern_match' field
          ~var:(fun var ~coercion ->
            if not (Coercion.is_id coercion)
            then
              Misc.fatal_errorf "Expected identity coercion on variable:@ %a"
                Simple.print field;
            F.Dynamically_computed (var, dbg))
          ~symbol:(fun sym ~coercion ->
            if not (Coercion.is_id coercion)
            then
              Misc.fatal_errorf "Expected identity coercion on symbol:@ %a"
                Simple.print field;
            F.Symbol sym)
          ~const:(fun const ->
            match Reg_width_const.descr const with
            | Tagged_immediate imm -> F.Tagged_immediate imm
            | Naked_immediate _ | Naked_float _ | Naked_float32 _
            | Naked_int32 _ | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _
              ->
              Misc.fatal_errorf
                "Expected a constant of kind [Value] but got %a (dbg %a)"
                Reg_width_const.print const Debuginfo.print_compact dbg))
  in
  let art = DA.are_rebuilding_terms dacc in
  match to_lift with
  | Immutable_block { tag; is_unique; fields } ->
    let fields = convert_fields fields in
    let mut : Mutability.t =
      if is_unique then Immutable_unique else Immutable
    in
    RSC.create_block art tag mut ~fields
  | Boxed_float32 f -> RSC.create_boxed_float32 art (Const f)
  | Boxed_float f -> RSC.create_boxed_float art (Const f)
  | Boxed_int32 i -> RSC.create_boxed_int32 art (Const i)
  | Boxed_int64 i -> RSC.create_boxed_int64 art (Const i)
  | Boxed_nativeint i -> RSC.create_boxed_nativeint art (Const i)
  | Boxed_vec128 v -> RSC.create_boxed_vec128 art (Const v)
  | Immutable_float32_array { fields = _ } ->
    (* CR mslater: (float32) unboxed arrays *)
    assert false
  | Immutable_float_array { fields } ->
    let fields = List.map (fun f -> Or_variable.Const f) fields in
    RSC.create_immutable_float_array art fields
  | Immutable_int32_array { fields } ->
    let fields = List.map (fun f -> Or_variable.Const f) fields in
    RSC.create_immutable_int32_array art fields
  | Immutable_int64_array { fields } ->
    let fields = List.map (fun f -> Or_variable.Const f) fields in
    RSC.create_immutable_int64_array art fields
  | Immutable_nativeint_array { fields } ->
    let fields = List.map (fun f -> Or_variable.Const f) fields in
    RSC.create_immutable_nativeint_array art fields
  | Immutable_value_array { fields } ->
    let fields = convert_fields fields in
    RSC.create_immutable_value_array art fields
  | Empty_array array_kind -> RSC.create_empty_array art array_kind

let lift dacc ty ~bound_to static_const : _ Or_invalid.t * DA.t =
  let dacc, symbol =
    let existing_symbol =
      match RSC.to_const static_const with
      | None -> None
      | Some (Code _ | Deleted_code) -> None
      | Some (Static_const const) -> DA.find_shareable_constant dacc const
    in
    match existing_symbol with
    | Some symbol ->
      if Flambda_features.check_invariants ()
         && not (DE.mem_symbol (DA.denv dacc) symbol)
      then
        Misc.fatal_errorf
          "Constant with symbol %a is shareable but not in the environment:@ %a"
          Symbol.print symbol DA.print dacc;
      dacc, symbol
    | None ->
      let symbol =
        Symbol.create
          (Compilation_unit.get_current_exn ())
          (Linkage_name.of_string (Variable.unique_name bound_to))
      in
      if not (K.equal (T.kind ty) K.value)
      then
        Misc.fatal_errorf "Cannot lift non-[Value] variable: %a" Variable.print
          bound_to;
      let symbol_projections =
        let free_names = RSC.free_names static_const in
        NO.fold_variables free_names ~init:Variable.Map.empty
          ~f:(fun symbol_projections var ->
            match DE.find_symbol_projection (DA.denv dacc) var with
            | None -> symbol_projections
            | Some proj -> Variable.Map.add var proj symbol_projections)
      in
      let dacc =
        let denv = DA.denv dacc in
        LC.create_block_like symbol static_const denv ~symbol_projections ty
        |> LCS.singleton
        |> DA.add_to_lifted_constant_accumulator dacc ~also_add_to_env:()
      in
      let dacc =
        match RSC.to_const static_const with
        | None | Some (Code _ | Deleted_code) -> dacc
        | Some (Static_const static_const) ->
          DA.consider_constant_for_sharing dacc symbol static_const
      in
      dacc, symbol
  in
  let symbol = Simple.symbol symbol in
  let term = Named.create_simple symbol in
  let var_ty = T.alias_type_of (T.kind ty) symbol in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
        DE.add_equation_on_variable denv bound_to var_ty)
  in
  Ok (Simplified_named.create term), dacc

let try_to_reify dacc dbg (term : Simplified_named.t) ~bound_to
    ~kind_of_bound_to ~allow_lifting : _ Or_invalid.t * DA.t =
  let occ_kind = Bound_var.name_mode bound_to in
  let bound_to = Bound_var.var bound_to in
  let denv = DA.denv dacc in
  let ty =
    TE.find (DE.typing_env denv) (Name.var bound_to) (Some kind_of_bound_to)
  in
  let typing_env = DE.typing_env denv in
  let reify_result =
    T.reify ~allowed_if_free_vars_defined_in:typing_env
      ~var_is_defined_at_toplevel:(fun var ->
        DE.is_defined_at_toplevel denv var)
      ~var_is_symbol_projection:(fun var ->
        Option.is_some (DE.find_symbol_projection denv var))
      typing_env ty
  in
  match reify_result with
  | Lift to_lift ->
    if Name_mode.is_normal occ_kind && allow_lifting
    then
      let static_const = create_static_const dacc dbg to_lift in
      lift dacc ty ~bound_to static_const
    else Ok term, dacc
  | Simple simple when Simple.equal simple (Simple.var bound_to) ->
    (* This could happen if [ty] is an alias type to a canonical that has
       [In_types], meaning that it cannot be returned when reifying at mode
       [Normal] as we are here. However, since the equations arising from
       simplification of the primitive (in our caller) have already been added
       to the environment, [bound_to] can in fact be returned by
       [get_canonical_simple] during [T.reify]. As such we need to catch this
       case here to avoid creating circular let-bindings. *)
    Ok term, dacc
  | Simple simple ->
    let dacc =
      let ty = T.alias_type_of (T.kind ty) simple in
      let denv = DE.add_equation_on_variable denv bound_to ty in
      DA.with_denv dacc denv
    in
    Ok (Simplified_named.create (Named.create_simple simple)), dacc
  | Cannot_reify -> Ok term, dacc
  | Invalid -> Invalid, dacc
