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

open! Simplify_import

let keep_lifted_constant_only_if_used uacc acc lifted_constant =
  let bound = LC.bound_static lifted_constant in
  let code_ids_live =
    match UA.reachable_code_ids uacc with
    | Unknown -> Bound_static.binds_code bound
    | Known { live_code_ids = _; ancestors_of_live_code_ids } ->
      not
        (Code_id.Set.disjoint
           (Bound_static.code_being_defined bound)
           ancestors_of_live_code_ids)
  in
  let symbols_live =
    not
      (Name.Set.disjoint
         (Name.set_of_symbol_set (Bound_static.symbols_being_defined bound))
         (UA.required_names uacc))
  in
  if symbols_live || code_ids_live then LCS.add acc lifted_constant else acc

let rebuild_let simplify_named_result removed_operations ~rewrite_id
    ~lifted_constants_from_defining_expr ~at_unit_toplevel
    ~(closure_info : Closure_info.t) ~body uacc ~after_rebuild =
  let lifted_constants_from_defining_expr =
    match Closure_info.in_or_out_of_closure closure_info with
    | In_a_closure ->
      (* See the comment in [simplify_let], below; this case is analogous. *)
      lifted_constants_from_defining_expr
    | Not_in_a_closure ->
      (* We must filter even if not rebuilding terms, otherwise the free names
         of the terms might get out of sync with [Data_flow]. *)
      LCS.fold lifted_constants_from_defining_expr ~init:LCS.empty
        ~f:(keep_lifted_constant_only_if_used uacc)
  in
  (* At this point, the free names in [uacc] are the free names of [body], plus
     all used value slots seen in the whole compilation unit. *)
  let no_constants_from_defining_expr =
    LCS.is_empty lifted_constants_from_defining_expr
  in
  (* The lifted constants present in [uacc] are the ones arising from the
     simplification of [body] which still have to be placed. We augment these
     with any constants arising from the simplification of the defining
     expression. Then we either place all of them, if we are at toplevel, or
     else return them in [uacc] for an outer [Let]-binding to deal with.

     It may be surprising that lifted constants can arise from the
     simplification of the body in the case where they have also arisen from the
     defining expression (since the latter implies that we must be at toplevel).
     However this can happen in the case of recursive continuations, in which
     constants cannot be placed. *)
  (* CR mshinwell: We don't actually have to have this logic for placing lifted
     constants here; it could be done before any other kind of expression. *)
  let no_constants_to_place =
    no_constants_from_defining_expr && UA.no_lifted_constants uacc
  in
  let uacc = UA.notify_removed ~operation:removed_operations uacc in
  let bindings =
    Simplify_named_result.bindings_to_place simplify_named_result
  in
  let bindings =
    List.map
      (fun (binding : Expr_builder.binding_to_place) :
           Expr_builder.binding_to_place ->
        match binding with
        | Delete_binding _ -> binding
        | Keep_binding
            ({ let_bound; simplified_defining_expr; original_defining_expr = _ }
            as kept_binding) -> (
          match simplified_defining_expr.named with
          | Prim (prim, _dbg) -> (
            match Bound_pattern.must_be_singleton_opt let_bound with
            | None -> binding
            | Some bound_var -> (
              let simple = Simple.var (Bound_var.var bound_var) in
              match
                DA.find_debuginfo_rewrite (UA.creation_dacc uacc)
                  ~bound_to:simple
              with
              | None -> binding
              | Some dbg ->
                Keep_binding
                  { kept_binding with
                    simplified_defining_expr =
                      Simplified_named.create (Named.create_prim prim dbg)
                  }))
          | Simple _ | Set_of_closures _ | Rec_info _ -> binding))
      bindings
  in
  (* Phantom let creation *)
  let generate_phantom_lets = UA.generate_phantom_lets uacc in
  let free_names_of_body = UA.name_occurrences uacc in
  let compute_greatest_name_mode (bound_vars : Bound_pattern.t) =
    match bound_vars with
    | Singleton bound_var ->
      (* We avoid the closure allocation (below) in this case. *)
      Name_occurrences.greatest_name_mode_var free_names_of_body
        (VB.var bound_var)
    | Set_of_closures _ ->
      Bound_pattern.fold_all_bound_vars bound_vars
        ~init:Name_mode.Or_absent.absent ~f:(fun greatest_name_mode bound_var ->
          Name_occurrences.greatest_name_mode_var free_names_of_body
            (VB.var bound_var)
          |> Name_mode.Or_absent.join_in_terms greatest_name_mode)
    | Static _ -> assert false
    (* see below *)
  in
  let bindings =
    List.map
      (fun (binding_to_place : Expr_builder.binding_to_place) ->
        match binding_to_place with
        | Delete_binding _ -> binding_to_place
        | Keep_binding
            ({ let_bound = bound_vars;
               simplified_defining_expr;
               original_defining_expr
             } as binding) ->
          let greatest_name_mode = compute_greatest_name_mode bound_vars in
          let declared_name_mode = Bound_pattern.name_mode bound_vars in
          let mismatched_modes =
            match
              Name_mode.Or_absent.compare_partial_order greatest_name_mode
                (Name_mode.Or_absent.present declared_name_mode)
            with
            | None -> true
            | Some c -> c > 0
          in
          let { Simplified_named.named = defining_expr;
                free_names = _;
                cost_metrics = _
              } =
            simplified_defining_expr
          in
          let defining_expr = Simplified_named.to_named defining_expr in
          if mismatched_modes
          then
            Misc.fatal_errorf
              "[Let]-binding declares variable(s) %a (mode %a) to be bound to@ \
               %a,@ but there exist occurrences for such variable(s) at \
               incompatible mode(s)@ (compared to %a)@ in the body (free names \
               %a):@ %a"
              Bound_pattern.print bound_vars Name_mode.print declared_name_mode
              Named.print defining_expr Name_mode.Or_absent.print
              greatest_name_mode Name_occurrences.print free_names_of_body
              (RE.print (UA.are_rebuilding_terms uacc))
              body;
          let is_end_region =
            match defining_expr with
            | Prim (prim, _) -> P.is_end_region prim
            | Simple _ | Set_of_closures _ | Static_consts _ | Rec_info _ ->
              None
          in
          let is_end_region_for_unused_region, is_end_region_for_used_region =
            match is_end_region with
            | None -> false, false
            | Some region ->
              let is_used =
                Name.Set.mem (Name.var region) (UA.required_names uacc)
              in
              not is_used, is_used
          in
          if is_end_region_for_used_region
             || (not is_end_region_for_unused_region)
                && not (Named.at_most_generative_effects defining_expr)
          then (
            if not (Name_mode.is_normal declared_name_mode)
            then
              Misc.fatal_errorf
                "Cannot [Let]-bind non-normal variable(s) to a [Named] that \
                 has more than generative effects:@ %a@ =@ %a"
                Bound_pattern.print bound_vars Named.print defining_expr;
            binding_to_place)
          else
            let is_depth =
              match defining_expr with
              | Rec_info _ -> true
              | Simple _ | Prim _ | Set_of_closures _ | Static_consts _ -> false
            in
            let has_uses = Name_mode.Or_absent.is_present greatest_name_mode in
            let can_phantomise =
              (not is_depth)
              && Bound_pattern.exists_all_bound_vars bound_vars
                   ~f:(fun bound_var ->
                     Variable.user_visible (VB.var bound_var))
            in
            let will_delete_binding =
              if is_end_region_for_unused_region
              then true
              else
                (* CR-someday mshinwell: This should detect whether there is any
                   provenance info associated with the variable. If there isn't,
                   the [Let] can be deleted even if debugging information is
                   being generated. *)
                not (has_uses || (generate_phantom_lets && can_phantomise))
            in
            if will_delete_binding
            then Expr_builder.Delete_binding { original_defining_expr }
            else
              (* CR-someday mshinwell: When leaving behind phantom lets, maybe
                 we should turn the defining expressions into simpler ones by
                 using the type, if possible. For example an Unbox_naked_int64
                 or something could potentially turn into a variable. This
                 defining expression usually never exists as the types propagate
                 the information forward. mshinwell: this might be done now in
                 Simplify_named, check. *)
              let name_mode =
                match greatest_name_mode with
                | Absent -> Name_mode.phantom
                | Present name_mode -> name_mode
              in
              assert (Name_mode.can_be_in_terms name_mode);
              let bound_vars =
                Bound_pattern.with_name_mode bound_vars name_mode
              in
              Expr_builder.Keep_binding { binding with let_bound = bound_vars })
      bindings
  in
  let uacc, bindings =
    let Flow_types.Mutable_unboxing_result.{ let_rewrites; _ } =
      UA.mutable_unboxing_result uacc
    in
    match Named_rewrite_id.Map.find rewrite_id let_rewrites with
    | exception Not_found -> uacc, bindings
    | rewrite -> (
      match bindings with
      | [] -> uacc, []
      | _ :: _ :: _ -> assert false
      | [(Delete_binding _ as binding)] -> uacc, [binding]
      | [Keep_binding binding] -> (
        match rewrite, binding.original_defining_expr with
        | Prim_rewrite prim_rewrite, Some (Prim (original_prim, dbg)) ->
          let uacc =
            UA.notify_removed
              ~operation:(Removed_operations.prim original_prim)
              uacc
          in
          let new_bindings =
            match prim_rewrite with
            | Remove_prim -> (
              let greatest_name_mode =
                compute_greatest_name_mode binding.let_bound
              in
              match greatest_name_mode with
              | Absent ->
                [ Expr_builder.Delete_binding
                    { original_defining_expr = binding.original_defining_expr }
                ]
              | Present name_mode ->
                if Name_mode.is_phantom name_mode
                then
                  let let_bound =
                    Bound_pattern.with_name_mode binding.let_bound name_mode
                  in
                  [Expr_builder.Keep_binding { binding with let_bound }]
                else
                  Misc.fatal_errorf
                    "Binding for %a was supposed to be removed but occurs in \
                     free names:@ %a"
                    Bound_pattern.print binding.let_bound Name_occurrences.print
                    free_names_of_body)
            | Invalid k ->
              let prim : P.t = Nullary (Invalid k) in
              let simplified_defining_expr =
                Simplified_named.create (Named.create_prim prim dbg)
              in
              [ Expr_builder.Keep_binding
                  { binding with simplified_defining_expr } ]
            | Replace_by_binding { var; bound_to } ->
              let bv = Bound_pattern.must_be_singleton binding.let_bound in
              let var' = Bound_var.var bv in
              assert (Variable.equal var var');
              let simplified_defining_expr =
                Simplified_named.create (Named.create_simple bound_to)
              in
              [ Expr_builder.Keep_binding
                  { binding with simplified_defining_expr } ]
          in
          uacc, new_bindings
        | ( Prim_rewrite _,
            ( None
            | Some (Simple _ | Set_of_closures _ | Static_consts _ | Rec_info _)
              ) ) ->
          Misc.fatal_errorf "Prim_rewrite applied to a non-prim Named.t"))
  in
  (* Return as quickly as possible if there is nothing to do. In this case, all
     constants get floated up to an outer binding. *)
  if no_constants_to_place || not at_unit_toplevel
  then
    let uacc =
      (* Avoid re-allocating [uacc] unless necessary. *)
      if no_constants_from_defining_expr
      then uacc
      else
        let lifted_constants_from_body = UA.lifted_constants uacc in
        LCS.union lifted_constants_from_body lifted_constants_from_defining_expr
        |> UA.with_lifted_constants uacc
    in
    let body, uacc =
      EB.make_new_let_bindings uacc ~bindings_outermost_first:bindings ~body
    in
    after_rebuild body uacc
  else
    let uacc, lifted_constants_from_body =
      UA.get_and_clear_lifted_constants uacc
    in
    let body, uacc =
      EB.place_lifted_constants uacc ~lifted_constants_from_defining_expr
        ~lifted_constants_from_body
        ~put_bindings_around_body:(fun uacc ~body ->
          EB.make_new_let_bindings uacc ~bindings_outermost_first:bindings ~body)
        ~body
    in
    after_rebuild body uacc

let record_one_value_slot_for_data_flow symbol value_slot simple data_flow =
  Flow.Acc.record_value_slot (Name.symbol symbol) value_slot
    (Simple.free_names simple) data_flow

let record_one_function_slot_for_data_flow ~free_names ~value_slots _
    (symbol, _) data_flow =
  let data_flow = Flow.Acc.record_symbol_binding symbol free_names data_flow in
  Value_slot.Map.fold
    (record_one_value_slot_for_data_flow symbol)
    value_slots data_flow

let record_lifted_constant_definition_for_data_flow ~being_defined data_flow
    definition =
  let module D = LC.Definition in
  match D.descr definition with
  | Code code_id ->
    Flow.Acc.record_code_id_binding code_id
      (NO.union being_defined (D.free_names definition))
      data_flow
  | Block_like { symbol; _ } ->
    let free_names = NO.union being_defined (D.free_names definition) in
    Flow.Acc.record_symbol_binding symbol free_names data_flow
  | Set_of_closures { closure_symbols_with_types; _ } -> (
    let expr = D.defining_expr definition in
    match Rebuilt_static_const.to_const expr with
    | Some (Static_const const) ->
      let set_of_closures = Static_const.must_be_set_of_closures const in
      let free_names =
        NO.union being_defined
          (Function_declarations.free_names
             (Set_of_closures.function_decls set_of_closures))
      in
      let value_slots = Set_of_closures.value_slots set_of_closures in
      Function_slot.Lmap.fold
        (record_one_function_slot_for_data_flow ~free_names ~value_slots)
        closure_symbols_with_types data_flow
    | None | Some (Code _ | Deleted_code) ->
      let free_names = NO.union being_defined (D.free_names definition) in
      Function_slot.Lmap.fold
        (fun _ (symbol, _) data_flow ->
          Flow.Acc.record_symbol_binding symbol free_names data_flow)
        closure_symbols_with_types data_flow)

let record_lifted_constant_for_data_flow data_flow lifted_constant =
  let data_flow =
    (* Record all projections as potential dependencies. *)
    Variable.Map.fold
      (fun var proj data_flow ->
        Flow.Acc.record_symbol_projection var
          (Symbol_projection.free_names proj)
          data_flow)
      (LC.symbol_projections lifted_constant)
      data_flow
  in
  let being_defined =
    let bound_static = Lifted_constant.bound_static lifted_constant in
    (* Note: We're not registering code IDs in the set, because we can actually
       make the code bindings deleted individually. In particular, code IDs that
       are only used in the newer_version_of field of another binding will be
       deleted as expected. *)
    let symbols = Bound_static.symbols_being_defined bound_static in
    NO.empty
    |> Symbol.Set.fold
         (fun symbol acc -> NO.add_symbol acc symbol Name_mode.normal)
         symbols
  in
  ListLabels.fold_left
    (LC.definitions lifted_constant)
    ~init:data_flow
    ~f:(record_lifted_constant_definition_for_data_flow ~being_defined)

let record_new_defining_expression_binding_for_data_flow dacc ~rewrite_id
    data_flow (binding : Expr_builder.binding_to_place) : Flow.Acc.t =
  let generate_phantom_lets = DE.generate_phantom_lets (DA.denv dacc) in
  match binding with
  | Delete_binding _ -> data_flow
  | Keep_binding
      { let_bound; simplified_defining_expr; original_defining_expr = _ } ->
    Flow.Acc.record_let_binding ~rewrite_id ~generate_phantom_lets ~let_bound
      ~simplified_defining_expr data_flow

let update_data_flow dacc closure_info ~lifted_constants_from_defining_expr
    simplify_named_result ~rewrite_id data_flow =
  let data_flow =
    match Closure_info.in_or_out_of_closure closure_info with
    | In_a_closure ->
      (* The dependency information for lifted constants (stored in [Data_flow])
         is only required at the point when the constants are placed. That
         always happens at toplevel, never inside closures -- so we don't need
         to do anything here. *)
      data_flow
    | Not_in_a_closure ->
      LCS.fold lifted_constants_from_defining_expr ~init:data_flow
        ~f:record_lifted_constant_for_data_flow
  in
  ListLabels.fold_left
    (Simplify_named_result.bindings_to_place simplify_named_result)
    ~init:data_flow
    ~f:(record_new_defining_expression_binding_for_data_flow dacc ~rewrite_id)

let simplify_let0 ~simplify_expr ~simplify_function_body dacc let_expr
    ~down_to_up bound_pattern ~body =
  let module L = Flambda.Let in
  let original_dacc = dacc in
  (* Remember then clear the lifted constants memory in [DA] so we can easily
     find out which constants are generated during simplification of the
     defining expression and the [body]. *)
  let dacc, prior_lifted_constants = DA.get_and_clear_lifted_constants dacc in
  (* Simplify the defining expression. *)
  let defining_expr = L.defining_expr let_expr in
  let simplify_named_result, removed_operations =
    Simplify_named.simplify_named dacc bound_pattern defining_expr
      ~simplify_function_body
  in
  (* We must make sure that if [Invalid] is going to be produced, [uacc] doesn't
     contain any extraneous data for e.g. lifted constants that will never be
     placed, since this can lead to errors when loading .cmx files or similar.
     To avoid this we don't traverse [body]. *)
  match simplify_named_result with
  | Invalid ->
    down_to_up original_dacc ~rebuild:(fun uacc ~after_rebuild ->
        let uacc = UA.notify_removed ~operation:removed_operations uacc in
        EB.rebuild_invalid uacc
          (Defining_expr_of_let (bound_pattern, defining_expr))
          ~after_rebuild)
  | Ok simplify_named_result ->
    let dacc = Simplify_named_result.dacc simplify_named_result in
    (* First accumulate variable, symbol and code ID usage information. *)
    (* CR-someday gbury/pchambart : in the case of an invalid, we currently
       over-approximate the uses. In case of an invalid, we might want to
       instead flush the uses of the current control flow branch (but this would
       require a more precise stack). *)
    (* We currently over-approximate the use of variables in symbols: both in
       the lifted constants, and in the bound constants, which we consider to be
       always used, leading to the free_names in their defining expressions to
       be considered as used unconditionally. *)
    let closure_info = DE.closure_info (DA.denv dacc) in
    (* Next remember any lifted constants that were generated during the
       simplification of the defining expression and sort them, since they may
       be mutually recursive. Then add back in to [dacc] the
       [prior_lifted_constants] remembered above. This results in the
       definitions and types for all these constants being available at a
       subsequent [Let_cont]. At such a point, [dacc] will be queried to
       retrieve all of the constants, which are then manually transferred into
       the computed [dacc] at the join point for subsequent simplification of
       the continuation handler(s).

       Note that no lifted constants are ever placed during the simplification
       of the defining expression. (Not even in the case of a [Set_of_closures]
       binding, since "let symbol" is disallowed under a lambda.) *)
    let lifted_constants_from_defining_expr = DA.get_lifted_constants dacc in
    let dacc =
      DA.add_to_lifted_constant_accumulator dacc prior_lifted_constants
    in
    let rewrite_id = Named_rewrite_id.create () in
    let dacc =
      DA.map_flow_acc dacc
        ~f:
          (update_data_flow dacc closure_info ~rewrite_id
             ~lifted_constants_from_defining_expr simplify_named_result)
    in
    let at_unit_toplevel = DE.at_unit_toplevel (DA.denv dacc) in
    (* Simplify the body of the let-expression and make the new [Let] bindings
       around the simplified body. [Simplify_named] will already have prepared
       [dacc] with the necessary bindings for the simplification of the body. *)
    let down_to_up dacc ~rebuild:rebuild_body =
      let rebuild uacc ~after_rebuild =
        let after_rebuild body uacc =
          rebuild_let simplify_named_result removed_operations
            ~lifted_constants_from_defining_expr ~at_unit_toplevel ~closure_info
            ~body uacc ~after_rebuild ~rewrite_id
        in
        rebuild_body uacc ~after_rebuild
      in
      down_to_up dacc ~rebuild
    in
    simplify_expr dacc body ~down_to_up

let simplify_let ~simplify_expr ~simplify_function_body dacc let_expr
    ~down_to_up =
  let module L = Flambda.Let in
  L.pattern_match let_expr
    ~f:
      (simplify_let0 ~simplify_expr ~simplify_function_body dacc let_expr
         ~down_to_up)

let simplify_let_with_bound_pattern ~simplify_expr_with_bound_pattern
    ~simplify_function_body dacc let_expr ~down_to_up =
  let module L = Flambda.Let in
  L.pattern_match let_expr ~f:(fun bound_pattern ->
      simplify_let0
        ~simplify_expr:(fun dacc body ~down_to_up ->
          simplify_expr_with_bound_pattern dacc (bound_pattern, body)
            ~down_to_up)
        ~simplify_function_body dacc let_expr ~down_to_up bound_pattern)
