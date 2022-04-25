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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

let create_static_const dacc dbg (to_lift : T.to_lift) : Rebuilt_static_const.t
    =
  match to_lift with
  | Immutable_block { tag; is_unique; fields } ->
    let fields =
      ListLabels.map fields
        ~f:(fun
             (field : T.var_or_symbol_or_tagged_immediate)
             :
             Field_of_static_block.t
           ->
          match field with
          | Var var -> Dynamically_computed (var, dbg)
          | Symbol sym -> Symbol sym
          | Tagged_immediate imm -> Tagged_immediate imm)
    in
    let mut : Mutability.t =
      if is_unique then Immutable_unique else Immutable
    in
    Rebuilt_static_const.create_block
      (DA.are_rebuilding_terms dacc)
      tag mut ~fields
  | Boxed_float f ->
    Rebuilt_static_const.create_boxed_float
      (DA.are_rebuilding_terms dacc)
      (Const f)
  | Boxed_int32 i ->
    Rebuilt_static_const.create_boxed_int32
      (DA.are_rebuilding_terms dacc)
      (Const i)
  | Boxed_int64 i ->
    Rebuilt_static_const.create_boxed_int64
      (DA.are_rebuilding_terms dacc)
      (Const i)
  | Boxed_nativeint i ->
    Rebuilt_static_const.create_boxed_nativeint
      (DA.are_rebuilding_terms dacc)
      (Const i)
  | Empty_array ->
    Rebuilt_static_const.create_empty_array (DA.are_rebuilding_terms dacc)

let lift dacc ty ~bound_to static_const =
  let dacc, symbol =
    let existing_symbol =
      match Rebuilt_static_const.to_const static_const with
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
          (Linkage_name.create (Variable.unique_name bound_to))
      in
      if not (K.equal (T.kind ty) K.value)
      then
        Misc.fatal_errorf "Cannot lift non-[Value] variable: %a" Variable.print
          bound_to;
      let free_names = Rebuilt_static_const.free_names static_const in
      let symbol_projections =
        Name_occurrences.fold_variables free_names ~init:Variable.Map.empty
          ~f:(fun symbol_projections var ->
            match DE.find_symbol_projection (DA.denv dacc) var with
            | None -> symbol_projections
            | Some proj -> Variable.Map.add var proj symbol_projections)
      in
      let dacc =
        let denv = DA.denv dacc in
        LC.create_block_like symbol static_const denv ~symbol_projections ty
        |> LCS.singleton
        |> DA.add_to_lifted_constant_accumulator dacc
      in
      let dacc =
        match Rebuilt_static_const.to_const static_const with
        | None | Some (Code _ | Deleted_code) -> dacc
        | Some (Static_const static_const) ->
          DA.consider_constant_for_sharing dacc symbol static_const
      in
      let dacc =
        DA.map_denv dacc ~f:(fun denv -> DE.add_symbol denv symbol ty)
      in
      dacc, symbol
  in
  let symbol' = Simple.symbol symbol in
  let term = Named.create_simple symbol' in
  let var_ty = T.alias_type_of (T.kind ty) symbol' in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
        DE.add_equation_on_variable denv bound_to var_ty)
  in
  Simplified_named.reachable term ~try_reify:false, dacc

let try_to_reify dacc dbg (term : Simplified_named.t) ~bound_to
    ~kind_of_bound_to ~allow_lifting =
  let occ_kind = Bound_var.name_mode bound_to in
  let bound_to = Bound_var.var bound_to in
  let denv = DA.denv dacc in
  let ty =
    TE.find (DE.typing_env denv) (Name.var bound_to) (Some kind_of_bound_to)
  in
  match term with
  | Invalid ->
    let ty = T.bottom_like ty in
    let denv = DE.add_equation_on_variable denv bound_to ty in
    Simplified_named.invalid (), DA.with_denv dacc denv
  | Reachable _ | Reachable_try_reify _ -> (
    let typing_env = DE.typing_env denv in
    let reify_result =
      T.reify ~allowed_if_free_vars_defined_in:typing_env
        ~additional_free_var_criterion:(fun var ->
          DE.is_defined_at_toplevel denv var
          || Option.is_some (DE.find_symbol_projection denv var))
        ~allow_unique:true typing_env ~min_name_mode:NM.normal ty
    in
    match reify_result with
    | Lift to_lift ->
      if Name_mode.is_normal occ_kind && allow_lifting
      then
        let static_const = create_static_const dacc dbg to_lift in
        lift dacc ty ~bound_to static_const
      else term, dacc
    | Simple simple ->
      (* CR mshinwell: Think about whether this is the best way of handling
         this. *)
      (* It is possible that the only [Simple] that [reify] could return is in
         fact [bound_to] -- for example when all other aliases are of an
         unsuitable occurrence kind. *)
      let dacc =
        if Simple.equal simple (Simple.var bound_to)
        then dacc
        else
          let ty = T.alias_type_of (T.kind ty) simple in
          let denv = DE.add_equation_on_variable denv bound_to ty in
          DA.with_denv dacc denv
      in
      if Simple.equal (Simple.var bound_to) simple
      then term, dacc
      else
        ( Simplified_named.reachable
            (Named.create_simple simple)
            ~try_reify:false,
          dacc )
    | Lift_set_of_closures _ (* already dealt with in [Simplify_named] *)
    | Cannot_reify ->
      term, dacc
    | Invalid ->
      let ty = T.bottom_like ty in
      let denv = DE.add_equation_on_variable denv bound_to ty in
      Simplified_named.invalid (), DA.with_denv dacc denv)
