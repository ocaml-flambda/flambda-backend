(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

let rebuild_let simplify_named_result
      removed_operations ~lifted_constants_from_defining_expr
      ~at_unit_toplevel ~(closure_info:Closure_info.t)
      ~body uacc ~after_rebuild =
  let lifted_constants_from_defining_expr =
    match closure_info with
  | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
    assert false
  | Closure _ ->
    lifted_constants_from_defining_expr
  | Not_in_a_closure -> begin
      (* TODO: do only if we are actually rebuilding terms *)
      LCS.fold lifted_constants_from_defining_expr ~init:LCS.empty
        ~f:(fun acc lifted_constant ->
          let bound = LC.bound_symbols lifted_constant in
          let code_id_live =
            match UA.reachable_code_ids uacc with
            | Unknown ->
              Bound_symbols.binds_code bound
            | Known { live_code_ids = _; ancestors_of_live_code_id; } ->
              not (Code_id.Set.intersection_is_empty
                     (Bound_symbols.code_being_defined bound)
                     ancestors_of_live_code_id)
          in
          let symbol_live =
            not (Name.Set.intersection_is_empty
                   (Name.set_of_symbol_set (Bound_symbols.being_defined bound))
                   (UA.required_names uacc))
          in
          if symbol_live || code_id_live then
            LCS.add acc lifted_constant
          else
            acc)
    end
  in
  let lifted_constants_from_defining_expr =
    Sort_lifted_constants.sort lifted_constants_from_defining_expr
  in
  (* At this point, the free names in [uacc] are the free names of [body],
     plus all used closure vars seen in the whole compilation unit. *)
  let no_constants_from_defining_expr =
    LCS.is_empty lifted_constants_from_defining_expr
  in
  (* The lifted constants present in [uacc] are the ones arising from
     the simplification of [body] which still have to be placed.  We
     augment these with any constants arising from the simplification of
     the defining expression.  Then we place (some of) them and/or return
     them in [uacc] for an outer [Let]-binding to deal with. *)
  let lifted_constants_from_body = UA.lifted_constants uacc in
  let no_constants_to_place =
    no_constants_from_defining_expr
      && LCS.is_empty lifted_constants_from_body
  in
  let uacc = UA.notify_removed ~operation:removed_operations uacc in
  let bindings =
    Simplify_named_result.bindings_to_place_in_any_order simplify_named_result
  in
  (* Return as quickly as possible if there is nothing to do.  In this
     case, all constants get floated up to an outer binding. *)
  if no_constants_to_place || not at_unit_toplevel then
    let uacc =
      (* Avoid re-allocating [uacc] unless necessary. *)
      if no_constants_from_defining_expr then uacc
      else
        LCS.union_ordered ~innermost:lifted_constants_from_body
          ~outermost:lifted_constants_from_defining_expr
        |> UA.with_lifted_constants uacc
    in
    let body, uacc =
      EB.make_new_let_bindings uacc ~bindings_outermost_first:bindings ~body
    in
    (* The let binding was removed. *)
    after_rebuild body uacc
  else
    let critical_deps_of_bindings =
      ListLabels.fold_left bindings
        ~init:Name_occurrences.empty
        ~f:(fun critical_deps { Simplify_named_result.let_bound; _ } ->
          Name_occurrences.union (Bindable_let_bound.free_names let_bound)
            critical_deps)
    in
    let body, uacc =
      EB.place_lifted_constants uacc
        ~lifted_constants_from_defining_expr
        ~lifted_constants_from_body
        ~put_bindings_around_body:
          (fun uacc ~body ->
            EB.make_new_let_bindings uacc ~bindings_outermost_first:bindings
              ~body)
        ~body
        ~critical_deps_of_bindings
    in
    after_rebuild body uacc

let simplify_let ~simplify_expr ~simplify_toplevel dacc let_expr ~down_to_up =
  let module L = Flambda.Let in
  L.pattern_match let_expr ~f:(fun bindable_let_bound ~body ->
    (* Remember then clear the lifted constants memory in [DA] so we can
       easily find out which constants are generated during simplification
       of the defining expression and the [body]. *)
    let dacc, prior_lifted_constants = DA.get_and_clear_lifted_constants dacc in
    (* Simplify the defining expression. *)
    let simplify_named_result, removed_operations =
      Simplify_named.simplify_named dacc bindable_let_bound
        (L.defining_expr let_expr)
        ~simplify_toplevel
    in
    let dacc = Simplify_named_result.dacc simplify_named_result in
    (* First accumulate variable usage information. *)
    (* CR gbury/pchambart : in the case of an invalid, we currently
       over-approximate the uses. In case of an invalid, we might want to
       instead flush the uses of the current control flow branch (but this
       would require a more precise stack). *)
    (* We currently over-approximate the use of variables in symbols: both in
       the lifted constants, and in the bound constants, which we consider to be
       always used, leading to the free_names in their defining expressions to
       be considered as used unconditionally. *)
    let dacc =
      DA.map_data_flow dacc ~f:(fun data_flow ->
        let data_flow =
          match DE.closure_info (DA.denv dacc) with
          | Closure _ ->
            (* lifted constants defined in a closure appear twice when simplifying
               terms: once at their "definition" in the source, and once when
               we finish simplifying the closure and we continue the simplification
               at toplevel. Thus, when we are inside a closure we don't need to record
               those dependencies: indeed we need the dependencies at toplevel
               where the lifted constants will be placed, but not in the closure,
               so it's better to have those dependencies in the dataflow at toplevel
               rather than the data_flow inside the closure. *)
            data_flow
          | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
            assert false
          | Not_in_a_closure ->
            LCS.fold (DA.get_lifted_constants dacc)
              ~init:data_flow ~f:(fun data_flow lifted_constant ->
                ListLabels.fold_left (LC.definitions lifted_constant)
                  ~init:data_flow ~f:(fun data_flow definition ->
                    match LC.Definition.descr definition with
                    | Code code_id ->
                      let free_names = LC.Definition.free_names definition in
                      Data_flow.record_code_id_binding code_id free_names data_flow
                    | Block_like { symbol; _ } ->
                      let free_names = LC.Definition.free_names definition in
                      Data_flow.record_symbol_binding symbol free_names data_flow
                    | Set_of_closures { closure_symbols_with_types; _ } ->
                      let expr = LC.Definition.defining_expr definition in
                      match Rebuilt_static_const.to_const expr with
                      | Some (Set_of_closures set_of_closures) ->
                        let free_names =
                          Function_declarations.free_names
                            (Set_of_closures.function_decls set_of_closures)
                        in
                        let closure_elements =
                          Set_of_closures.closure_elements set_of_closures
                        in
                        Closure_id.Lmap.fold (fun _ (symbol, _) data_flow ->
                          let data_flow =
                            Data_flow.record_symbol_binding symbol free_names data_flow
                          in
                          Var_within_closure.Map.fold (fun closure_var simple data_flow ->
                            Data_flow.record_closure_element_binding
                              (Name.symbol symbol) closure_var
                              (Simple.free_names simple) data_flow)
                            closure_elements data_flow
                        ) closure_symbols_with_types data_flow
                      | Some (
                        Code _
                        | Block _
                        | Boxed_float _
                        | Boxed_int32 _
                        | Boxed_int64 _
                        | Boxed_nativeint _
                        | Immutable_float_block _
                        | Immutable_float_array _
                        | Mutable_string _
                        | Immutable_string _)
                      | None ->
                        let free_names = LC.Definition.free_names definition in
                        Closure_id.Lmap.fold (fun _ (symbol, _) data_flow ->
                          Data_flow.record_symbol_binding symbol free_names data_flow
                        ) closure_symbols_with_types data_flow
                  )
              )
        in
        ListLabels.fold_left
          (Simplify_named_result.bindings_to_place_in_any_order
             simplify_named_result)
          ~init:data_flow
          ~f:(fun data_flow (binding : Simplify_named_result.binding_to_place) ->
            match binding.simplified_defining_expr with
            | Invalid _ -> data_flow
            | Reachable { free_names; named; cost_metrics = _; } ->
              let can_be_removed =
                match named with
                | Simple _ | Set_of_closures _ | Rec_info _ -> true
                | Prim (prim, _) -> P.at_most_generative_effects prim
              in
              if not can_be_removed then
                Data_flow.add_used_in_current_handler free_names data_flow
              else
                let generate_phantom_lets =
                  DE.generate_phantom_lets (DA.denv dacc)
                in
                (* TODO: use Bindable_let_bound.fold_all_bound_vars instead
                   of a match here. *)
                match binding.let_bound with
                | Singleton v ->
                  Data_flow.record_var_binding (VB.var v) free_names
                    ~generate_phantom_lets data_flow
                | Set_of_closures { closure_vars; name_mode = _; } ->
                  ListLabels.fold_left closure_vars ~init:data_flow
                    ~f:(fun data_flow v ->
                      Data_flow.record_var_binding (VB.var v) free_names
                        ~generate_phantom_lets data_flow)
                | Symbols _ ->
                  (* This cannot be reached, simplify_named does not return any symbol binding, they
                     have all been moved to lifted_constants *)
                  assert false))
    in
    (* Next remember any lifted constants that were generated during the
       simplification of the defining expression and sort them, since they
       may be mutually recursive.  Then add back in to [dacc]
       the [prior_lifted_constants] remembered above.  This results in the
       definitions and types for all these constants being available at a
       subsequent [Let_cont].  At such a point, [dacc] will be queried to
       retrieve all of the constants, which are then manually transferred
       into the computed [dacc] at the join point for subsequent
       simplification of the continuation handler(s).
       Note that no lifted constants are ever placed during the simplification
       of the defining expression.  (Not even in the case of a
       [Set_of_closures] binding, since "let symbol" is disallowed under a
       lambda.) *)
    let lifted_constants_from_defining_expr = DA.get_lifted_constants dacc in
    let dacc = DA.add_lifted_constants dacc prior_lifted_constants in
    let at_unit_toplevel = DE.at_unit_toplevel (DA.denv dacc) in
    let closure_info = DE.closure_info (DA.denv dacc) in
    (* Simplify the body of the let-expression and make the new [Let] bindings
       around the simplified body.  [Simplify_named] will already have
       prepared [dacc] with the necessary bindings for the simplification of
       the body. *)
    simplify_expr dacc body
      ~down_to_up:(fun dacc ~rebuild:rebuild_body ->
        down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
          rebuild_body uacc ~after_rebuild:(fun body uacc ->
            rebuild_let simplify_named_result
              removed_operations ~lifted_constants_from_defining_expr
              ~at_unit_toplevel ~closure_info ~body uacc ~after_rebuild))))
