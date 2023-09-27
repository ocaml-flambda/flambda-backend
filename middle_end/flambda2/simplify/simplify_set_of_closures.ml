(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2022 OCamlPro SAS                                    *)
(*   Copyright 2014--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import

(* CR-someday mshinwell: Unused value slots should be deleted prior to
   simplification of sets of closures, taking the used-var-in-closures set from
   the previous round. *)

(* CR-someday mshinwell: We could consider making the functions in this file
   tail recursive, although it probably isn't necessary, as excessive levels of
   nesting of functions seems unlikely. *)

module C = Simplify_set_of_closures_context

let dacc_inside_function context ~outer_dacc ~params ~my_closure ~my_region
    ~my_depth function_slot_opt ~closure_bound_names_inside_function
    ~inlining_arguments ~absolute_history code_id ~return_continuation
    ~exn_continuation ~loopify_state code_metadata =
  let dacc = C.dacc_inside_functions context in
  let alloc_modes = Code_metadata.param_modes code_metadata in
  let denv =
    DE.add_parameters_with_unknown_types ~alloc_modes (DA.denv dacc) params
    |> DE.set_inlining_arguments inlining_arguments
    |> DE.set_inlining_history_tracker
         (Inlining_history.Tracker.inside_function absolute_history)
  in
  let denv =
    match function_slot_opt with
    | None ->
      (* This happens in the stub case, where we are only simplifying code, not
         a set of closures. *)
      DE.add_variable denv
        (Bound_var.create my_closure NM.normal)
        (T.unknown K.value)
    | Some function_slot -> (
      match
        Function_slot.Map.find function_slot closure_bound_names_inside_function
      with
      | exception Not_found ->
        Misc.fatal_errorf
          "No closure name for function slot %a.@ \
           closure_bound_names_inside_function = %a."
          Function_slot.print function_slot
          (Function_slot.Map.print Bound_name.print)
          closure_bound_names_inside_function
      | name ->
        let name = Bound_name.name name in
        DE.add_variable denv
          (Bound_var.create my_closure NM.normal)
          (T.alias_type_of K.value (Simple.name name)))
  in
  let denv =
    let my_region = Bound_var.create my_region Name_mode.normal in
    DE.add_variable denv my_region (T.unknown K.region)
  in
  let denv =
    let my_depth = Bound_var.create my_depth Name_mode.normal in
    DE.add_variable denv my_depth (T.unknown K.rec_info)
  in
  let denv =
    LCS.add_to_denv ~maybe_already_defined:() denv
      (DA.get_lifted_constants outer_dacc)
    |> DE.enter_closure code_id ~return_continuation ~exn_continuation
         ~my_closure
    |> DE.set_loopify_state loopify_state
    |> DE.increment_continuation_scope
  in
  let dacc = DA.with_denv dacc denv in
  let code_ids_to_remember = DA.code_ids_to_remember outer_dacc in
  let code_ids_to_never_delete = DA.code_ids_to_never_delete outer_dacc in
  let used_value_slots = DA.used_value_slots outer_dacc in
  let shareable_constants = DA.shareable_constants outer_dacc in
  let slot_offsets = DA.slot_offsets outer_dacc in
  (* CR vlaviron: maybe DA could be restructured so that it can keep track of
     the global values going around the loop here, so we don't forget to add one
     of these lines below... *)
  dacc
  |> DA.with_code_ids_to_remember ~code_ids_to_remember
  |> DA.with_code_ids_to_never_delete ~code_ids_to_never_delete
  |> DA.with_used_value_slots ~used_value_slots
  |> DA.with_shareable_constants ~shareable_constants
  |> DA.with_slot_offsets ~slot_offsets

let extract_accumulators_from_function outer_dacc ~dacc_after_body
    ~uacc_after_upwards_traversal =
  let lifted_consts_this_function =
    (* Subtle point: [uacc_after_upwards_traversal] must be used to retrieve all
       of the lifted constants generated during the simplification of the
       function, not [dacc_after_body]. The reason for this is that sometimes
       the constants in [DA] are cleared (but remembered) and reinstated
       afterwards, for example at a [Let_cont]. It follows that if the turning
       point where the downwards traversal turns into an upwards traversal is in
       such a context, not all of the constants may currently be present in
       [DA]. *)
    UA.lifted_constants uacc_after_upwards_traversal
  in
  let code_ids_to_remember = DA.code_ids_to_remember dacc_after_body in
  let code_ids_to_never_delete = DA.code_ids_to_never_delete dacc_after_body in
  let used_value_slots = UA.used_value_slots uacc_after_upwards_traversal in
  let shareable_constants =
    UA.shareable_constants uacc_after_upwards_traversal
  in
  let slot_offsets = DA.slot_offsets dacc_after_body in
  let code_age_relation =
    TE.code_age_relation (DA.typing_env dacc_after_body)
  in
  let outer_dacc =
    DA.add_to_lifted_constant_accumulator ~also_add_to_env:() outer_dacc
      lifted_consts_this_function
    |> DA.with_code_ids_to_remember ~code_ids_to_remember
    |> DA.with_code_ids_to_never_delete ~code_ids_to_never_delete
    |> DA.with_used_value_slots ~used_value_slots
    |> DA.with_shareable_constants ~shareable_constants
    |> DA.with_slot_offsets ~slot_offsets
    |> DA.with_code_age_relation ~code_age_relation
  in
  outer_dacc, lifted_consts_this_function

type simplify_function_body_result =
  { params : Bound_parameters.t;
    params_and_body : Rebuilt_expr.Function_params_and_body.t;
    dacc_at_function_entry : DA.t;
    dacc_after_body : DA.t;
    free_names_of_code : NO.t;
    return_cont_uses : Continuation_uses.t option;
    is_my_closure_used : bool;
    recursive : Recursive.t;
    uacc_after_upwards_traversal : UA.t
  }

let simplify_function_body context ~outer_dacc function_slot_opt
    ~closure_bound_names_inside_function ~inlining_arguments ~absolute_history
    code_id code ~return_continuation ~exn_continuation params ~body ~my_closure
    ~is_my_closure_used:_ ~my_region ~my_depth ~free_names_of_body:_ =
  let loopify_state =
    if Loopify_attribute.should_loopify (Code.loopify code)
    then Loopify_state.loopify (Continuation.create ~name:"self" ())
    else Loopify_state.do_not_loopify
  in
  let dacc_at_function_entry =
    dacc_inside_function context ~outer_dacc ~params ~my_closure ~my_region
      ~my_depth function_slot_opt ~closure_bound_names_inside_function
      ~inlining_arguments ~absolute_history code_id ~return_continuation
      ~exn_continuation ~loopify_state (Code.code_metadata code)
  in
  let dacc = dacc_at_function_entry in
  if not (DA.no_lifted_constants dacc)
  then
    Misc.fatal_errorf "Did not expect lifted constants in [dacc]:@ %a" DA.print
      dacc;
  assert (not (DE.at_unit_toplevel (DA.denv dacc)));
  match
    C.simplify_function_body context dacc body ~return_continuation
      ~exn_continuation ~return_arity:(Code.result_arity code)
      ~implicit_params:
        (Bound_parameters.create
           [ Bound_parameter.create my_closure
               Flambda_kind.With_subkind.any_value;
             Bound_parameter.create my_region Flambda_kind.With_subkind.region;
             Bound_parameter.create my_depth Flambda_kind.With_subkind.rec_info
           ])
      ~loopify_state ~params
  with
  | body, uacc ->
    let dacc_after_body = UA.creation_dacc uacc in
    let return_cont_uses =
      CUE.get_continuation_uses
        (DA.continuation_uses_env dacc_after_body)
        return_continuation
    in
    let free_names_of_body = UA.name_occurrences uacc in
    let params_and_body =
      RE.Function_params_and_body.create ~free_names_of_body
        ~return_continuation ~exn_continuation params ~body ~my_closure
        ~my_region ~my_depth
    in
    let is_my_closure_used = NO.mem_var free_names_of_body my_closure in
    let previously_free_depth_variables =
      NO.create_variables (C.previously_free_depth_variables context) NM.normal
    in
    let recursive : Recursive.t =
      if Name_occurrences.mem_var free_names_of_body my_depth
      then Recursive
      else Non_recursive
    in
    if NO.mem_var free_names_of_body my_region &&
       Lambda.is_heap_mode (Code.result_mode code) then
      Misc.fatal_errorf "Unexpected free my_region in code with heap result mode:\n"
        (RE.print (UA.are_rebuilding_terms uacc)) body;
    let free_names_of_code =
      free_names_of_body
      |> NO.remove_continuation ~continuation:return_continuation
      |> NO.remove_continuation ~continuation:exn_continuation
      |> NO.remove_var ~var:my_closure
      |> NO.remove_var ~var:my_region
      |> NO.remove_var ~var:my_depth
      |> NO.diff ~without:(Bound_parameters.free_names params)
      |> NO.diff ~without:previously_free_depth_variables
    in
    if not
         (NO.no_variables free_names_of_code
         && NO.no_continuations free_names_of_code)
    then
      Misc.fatal_errorf
        "Unexpected free name(s):@ %a@ in:@ \n\
         %a@ \n\
         Simplified version:@ fun %a %a %a %a ->@ \n\
        \  %a" NO.print free_names_of_code Code_id.print code_id
        Bound_parameters.print params Variable.print my_closure Variable.print
        my_region Variable.print my_depth
        (RE.print (UA.are_rebuilding_terms uacc))
        body;
    { params;
      params_and_body;
      dacc_at_function_entry;
      dacc_after_body;
      free_names_of_code;
      return_cont_uses;
      is_my_closure_used;
      recursive;
      uacc_after_upwards_traversal = uacc
    }
  | exception Misc.Fatal_error ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf
      "\n\
       %tContext is:%t simplifying function with function slot %a,@ params \
       %a,@ return continuation %a,@ exn continuation %a,@ my_closure %a,@ \
       body:@ %a@ with downwards accumulator:@ %a\n"
      Flambda_colours.error Flambda_colours.pop
      (Format.pp_print_option Function_slot.print)
      function_slot_opt Bound_parameters.print params Continuation.print
      return_continuation Continuation.print exn_continuation Variable.print
      my_closure Expr.print body DA.print dacc;
    Printexc.raise_with_backtrace Misc.Fatal_error bt

let compute_result_types ~is_a_functor ~return_cont_uses ~dacc_after_body
    ~dacc_at_function_entry ~return_cont_params ~lifted_consts_this_function
    ~params : _ Or_unknown_or_bottom.t =
  match
    Flambda_features.function_result_types ~is_a_functor, return_cont_uses
  with
  | false, _ -> Unknown
  | true, None -> Bottom
  | true, Some uses ->
    let env_at_fork =
      (* We use [C.dacc_inside_functions] not [C.dacc_prior_to_sets] to ensure
         that the environment contains bindings for any symbols being defined by
         the set of closures. *)
      DA.denv dacc_at_function_entry
    in
    let join =
      Join_points.compute_handler_env
        ~cut_after:(Scope.prev (DE.get_continuation_scope env_at_fork))
        (Continuation_uses.get_uses uses)
        ~is_recursive:false ~params:return_cont_params ~env_at_fork
        ~consts_lifted_during_body:lifted_consts_this_function
    in
    let params_and_results =
      Bound_parameters.var_set
        (Bound_parameters.append params return_cont_params)
    in
    let typing_env = DE.typing_env join.handler_env in
    let typing_env =
      TE.with_code_age_relation typing_env
        (TE.code_age_relation (DA.typing_env dacc_after_body))
    in
    let results_and_types =
      List.map
        (fun result ->
          let name = BP.name result in
          let kind = K.With_subkind.kind (BP.kind result) in
          let ty = TE.find typing_env name (Some kind) in
          name, ty)
        (Bound_parameters.to_list return_cont_params)
    in
    let env_extension =
      (* This call is important for compilation time performance, to cut down
         the size of the return types. *)
      T.make_suitable_for_environment typing_env
        (All_variables_except params_and_results) results_and_types
    in
    Ok (Result_types.create ~params ~results:return_cont_params env_extension)

type rebuilt_code =
  | Rebuilding of Code.t
  | Not_rebuilding

type simplify_function_result =
  { code_id : Code_id.t;
    code : (rebuilt_code * Rebuilt_static_const.t) option;
    outer_dacc : DA.t;
    should_resimplify : bool
  }

let simplify_function0 context ~outer_dacc function_slot_opt code_id code
    ~closure_bound_names_inside_function =
  let denv_prior_to_sets = C.dacc_prior_to_sets context |> DA.denv in
  let inlining_arguments_from_denv =
    denv_prior_to_sets |> DE.inlining_arguments
  in
  let absolute_history, relative_history =
    DE.inlining_history_tracker denv_prior_to_sets
    |> Inlining_history.Tracker.fundecl ~dbg:(Code.dbg code)
         ~function_relative_history:(Code.relative_history code)
         ~name:
           (match function_slot_opt with
           | Some function_slot -> Function_slot.name function_slot
           | None -> Code_id.name code_id)
  in
  (* Compute the set of inlining_arguments used to define this function by
     taking the "least powerful" set between the one set in the environment and
     the one used to define the symbol previously. This way, functions that were
     imported from a foreign compilation unit after inlining will still be
     considered with a set of inlining arguments coherent with the one used to
     compile the current file when inlining. *)
  let inlining_arguments =
    Inlining_arguments.meet
      (Code.inlining_arguments code)
      inlining_arguments_from_denv
  in
  let result_arity = Code.result_arity code in
  let return_cont_params =
    List.mapi
      (fun i kind_with_subkind ->
        BP.create
          (Variable.create ("result" ^ string_of_int i))
          kind_with_subkind)
      (Flambda_arity.unarized_components result_arity)
    |> Bound_parameters.create
  in
  let { params;
        params_and_body;
        dacc_at_function_entry;
        dacc_after_body;
        free_names_of_code;
        return_cont_uses;
        is_my_closure_used;
        recursive;
        uacc_after_upwards_traversal
      } =
    Function_params_and_body.pattern_match
      (Code.params_and_body code)
      ~f:
        (simplify_function_body context ~outer_dacc function_slot_opt
           ~closure_bound_names_inside_function ~inlining_arguments
           ~absolute_history code_id code)
  in
  let should_resimplify = UA.resimplify uacc_after_upwards_traversal in
  let outer_dacc, lifted_consts_this_function =
    extract_accumulators_from_function outer_dacc ~dacc_after_body
      ~uacc_after_upwards_traversal
  in
  let cost_metrics = UA.cost_metrics uacc_after_upwards_traversal in
  let old_code_id = code_id in
  let code_id, newer_version_of =
    match
      Code_id.Map.find old_code_id (C.old_to_new_code_ids_all_sets context)
    with
    | new_code_id -> new_code_id, Some old_code_id
    | exception Not_found -> old_code_id, None
  in
  let inlining_decision =
    let decision =
      Function_decl_inlining_decision.make_decision ~inlining_arguments
        ~inline:(Code.inline code) ~stub:(Code.stub code) ~cost_metrics
        ~is_a_functor:(Code.is_a_functor code) ~recursive
    in
    Inlining_report.record_decision_at_function_definition ~absolute_history
      ~code_metadata:(Code.code_metadata code) ~pass:After_simplify
      ~are_rebuilding_terms:(DA.are_rebuilding_terms dacc_after_body)
      decision;
    decision
  in
  let is_a_functor = Code.is_a_functor code in
  let result_types =
    compute_result_types ~is_a_functor ~return_cont_uses ~dacc_after_body
      ~dacc_at_function_entry ~return_cont_params ~lifted_consts_this_function
      ~params
  in
  let outer_dacc =
    (* This is the complicated part about slot offsets. We just traversed the
       body of the function and have accumulated constraints (via [Expr_builder]
       when rebuilding expressions involving sets of closures). These
       constraints are only useful if we end up keeping the current code
       binding. The slot offsets are registered in a map from code IDs to
       offsets in [DA]. A global map of slot offsets is accumulated in [UA],
       using the information from [DA], according to whether particular code
       bindings do indeed get kept. This is how we avoid traversing the code in
       the upwards pass to extract offsets. *)
    match UA.slot_offsets uacc_after_upwards_traversal with
    | Unknown -> outer_dacc
    | Known offsets ->
      let slot_offsets =
        Code_id.Map.add code_id offsets (DA.slot_offsets outer_dacc)
      in
      DA.with_slot_offsets outer_dacc ~slot_offsets
  in
  let loopify : Loopify_attribute.t =
    match Code.loopify code with
    | Always_loopify ->
      (* CR ncourant: in this case, the function had a [@loop] attribute, so we
         want to keep it in case we perform another pass. It might be better to
         only keep it that way if it is still recursive, however, but this is
         simpler. *)
      Always_loopify
    | Never_loopify -> Never_loopify
    | Already_loopified -> Already_loopified
    | Default_loopify_and_tailrec -> Already_loopified
    | Default_loopify_and_not_tailrec -> Never_loopify
  in
  let code_const, new_code =
    Rebuilt_static_const.create_code
      (DA.are_rebuilding_terms dacc_after_body)
      code_id ~params_and_body ~free_names_of_params_and_body:free_names_of_code
      ~newer_version_of ~params_arity:(Code.params_arity code)
      ~param_modes:(Code.param_modes code)
      ~first_complex_local_param:(Code.first_complex_local_param code)
      ~result_arity ~result_types
      ~result_mode:(Code.result_mode code)
      ~contains_no_escaping_local_allocs:
        (Code.contains_no_escaping_local_allocs code)
      ~stub:(Code.stub code) ~inline:(Code.inline code) ~check:(Code.check code)
      ~poll_attribute:(Code.poll_attribute code) ~is_a_functor
      ~recursive:(Code.recursive code) ~cost_metrics ~inlining_arguments
      ~dbg:(Code.dbg code) ~is_tupled:(Code.is_tupled code) ~is_my_closure_used
      ~inlining_decision ~absolute_history ~relative_history ~loopify
  in
  let code =
    let are_rebuilding = DA.are_rebuilding_terms dacc_after_body in
    match new_code with
    | None ->
      assert (not (Are_rebuilding_terms.are_rebuilding are_rebuilding));
      Not_rebuilding
    | Some new_code ->
      assert (Are_rebuilding_terms.are_rebuilding are_rebuilding);
      Rebuilding new_code
  in
  { code_id; code = Some (code, code_const); outer_dacc; should_resimplify }

let introduce_code dacc code_id code_const =
  let code = LC.create_code code_id code_const in
  DA.add_to_lifted_constant_accumulator ~also_add_to_env:() dacc
    (LCS.singleton code)

let simplify_function context ~outer_dacc function_slot code_id
    ~closure_bound_names_inside_function =
  let code_or_metadata =
    DE.find_code_exn (DA.denv (C.dacc_prior_to_sets context)) code_id
  in
  let code_id, outer_dacc =
    match Code_or_metadata.view code_or_metadata with
    | Code_present code when not (Code.stub code) ->
      let rec run ~outer_dacc ~code count =
        let { code_id; code = new_code; outer_dacc; should_resimplify } =
          simplify_function0 context ~outer_dacc (Some function_slot) code_id
            code ~closure_bound_names_inside_function
        in
        match new_code with
        | None -> code_id, outer_dacc
        | Some (Not_rebuilding, new_code_const) ->
          (* Not rebuilding: there is no code to resimplify *)
          let outer_dacc = introduce_code outer_dacc code_id new_code_const in
          code_id, outer_dacc
        | Some (Rebuilding new_code, new_code_const) ->
          let max_function_simplify_run =
            Flambda_features.Expert.max_function_simplify_run ()
          in
          if should_resimplify && count < max_function_simplify_run
          then run ~outer_dacc ~code:new_code (count + 1)
          else
            let outer_dacc = introduce_code outer_dacc code_id new_code_const in
            code_id, outer_dacc
      in
      run ~outer_dacc ~code 0
    | Code_present _ | Metadata_only _ ->
      (* No new code ID is created in this case: there is no function body to be
         simplified and all other code metadata will remain the same. *)
      code_id, outer_dacc
  in
  let code_ids_to_never_delete_this_set =
    let code_metadata = Code_or_metadata.code_metadata code_or_metadata in
    let never_delete =
      match Code_metadata.check code_metadata with
      | Default_check -> !Clflags.zero_alloc_check_assert_all
      | Ignore_assert_all Zero_alloc -> false
      | Assume { property = Zero_alloc; _ } -> false
      | Check { property = Zero_alloc; _ } -> true
    in
    if never_delete then Code_id.Set.singleton code_id else Code_id.Set.empty
  in
  code_id, outer_dacc, code_ids_to_never_delete_this_set

type simplify_set_of_closures0_result =
  { set_of_closures : Flambda.Set_of_closures.t;
    dacc : Downwards_acc.t
  }

let simplify_set_of_closures0 outer_dacc context set_of_closures
    ~closure_bound_names ~closure_bound_names_inside ~value_slots
    ~value_slot_types =
  let dacc = C.dacc_prior_to_sets context in
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let all_function_decls_in_set =
    Function_declarations.funs_in_order function_decls
  in
  if not (DA.no_lifted_constants dacc)
  then
    Misc.fatal_errorf "Did not expect lifted constants in [dacc]:@ %a" DA.print
      dacc;
  let ( (code_ids_to_never_delete_this_set, fun_types, outer_dacc),
        all_function_decls_in_set ) =
    Function_slot.Lmap.fold_left_map
      (fun (result_code_ids_to_never_delete_this_set, fun_types, outer_dacc)
           function_slot old_code_id ->
        let code_id, outer_dacc, code_ids_to_never_delete_this_set =
          simplify_function context ~outer_dacc function_slot old_code_id
            ~closure_bound_names_inside_function:closure_bound_names_inside
        in
        let function_type =
          let rec_info =
            (* This is the intrinsic type of the function as seen outside its
               own scope, so its [Rec_info] needs to say its depth is zero *)
            T.this_rec_info Rec_info_expr.initial
          in
          C.function_decl_type code_id ~rec_info
        in
        let fun_types =
          Function_slot.Map.add function_slot function_type fun_types
        in
        let code_ids_to_never_delete_this_set =
          Code_id.Set.union code_ids_to_never_delete_this_set
            result_code_ids_to_never_delete_this_set
        in
        (code_ids_to_never_delete_this_set, fun_types, outer_dacc), code_id)
      (Code_id.Set.empty, Function_slot.Map.empty, outer_dacc)
      all_function_decls_in_set
  in
  let code_ids_to_remember_this_set =
    Function_slot.Lmap.fold
      (fun _function_slot code_id code_ids -> Code_id.Set.add code_id code_ids)
      all_function_decls_in_set Code_id.Set.empty
  in
  let dacc =
    DA.add_code_ids_to_remember outer_dacc code_ids_to_remember_this_set
  in
  let dacc =
    DA.add_code_ids_to_never_delete dacc code_ids_to_never_delete_this_set
  in
  let closure_types_by_bound_name =
    let closure_types_via_aliases =
      Function_slot.Map.map
        (fun name ->
          T.alias_type_of K.value (Simple.name (Bound_name.name name)))
        closure_bound_names
    in
    Function_slot.Map.fold
      (fun function_slot _function_decl_type closure_types ->
        match Function_slot.Map.find function_slot closure_bound_names with
        | exception Not_found ->
          Misc.fatal_errorf "No bound variable for function slot %a"
            Function_slot.print function_slot
        | bound_name ->
          let closure_type =
            T.exactly_this_closure function_slot
              ~all_function_slots_in_set:fun_types
              ~all_closure_types_in_set:closure_types_via_aliases
              ~all_value_slots_in_set:value_slot_types
              (Alloc_mode.For_allocations.as_type
                 (Set_of_closures.alloc_mode set_of_closures))
          in
          (bound_name, closure_type) :: closure_types)
      fun_types []
  in
  let dacc =
    DA.map_denv dacc ~f:(fun denv ->
        List.fold_left
          (fun denv (bound_name, closure_type) ->
            (* In the lifting case the symbols could be defined already. *)
            let denv = DE.define_name_if_undefined denv bound_name K.value in
            DE.add_equation_on_name denv
              (Bound_name.name bound_name)
              closure_type)
          denv closure_types_by_bound_name)
  in
  let set_of_closures =
    Function_declarations.create all_function_decls_in_set
    |> Set_of_closures.create ~value_slots
         (Set_of_closures.alloc_mode set_of_closures)
  in
  { set_of_closures; dacc }

let simplify_and_lift_set_of_closures dacc ~closure_bound_vars_inverse
    ~closure_bound_vars set_of_closures ~value_slots ~symbol_projections
    ~simplify_function_body =
  let function_decls = Set_of_closures.function_decls set_of_closures in
  let closure_symbols =
    Function_slot.Lmap.mapi
      (fun function_slot _func_decl ->
        let name =
          function_slot |> Function_slot.rename |> Function_slot.to_string
          |> Linkage_name.of_string
        in
        Symbol.create (Compilation_unit.get_current_exn ()) name)
      (Function_declarations.funs_in_order function_decls)
  in
  let closure_symbols_map =
    Function_slot.Lmap.bindings closure_symbols |> Function_slot.Map.of_list
  in
  let closure_bound_names =
    Function_slot.Map.map Bound_name.create_symbol closure_symbols_map
  in
  let value_slot_types =
    Value_slot.Map.mapi
      (fun value_slot in_slot ->
        let kind = K.With_subkind.kind (Value_slot.kind value_slot) in
        Simple.pattern_match in_slot
          ~const:(fun _ -> T.alias_type_of kind in_slot)
          ~name:(fun name ~coercion ->
            Name.pattern_match name
              ~var:(fun var ->
                match Variable.Map.find var closure_bound_vars_inverse with
                | exception Not_found ->
                  assert (DE.mem_variable (DA.denv dacc) var);
                  T.alias_type_of kind in_slot
                | function_slot ->
                  let closure_symbol =
                    Function_slot.Map.find function_slot closure_symbols_map
                  in
                  let simple =
                    Simple.with_coercion (Simple.symbol closure_symbol) coercion
                  in
                  T.alias_type_of kind simple)
              ~symbol:(fun _sym -> T.alias_type_of kind in_slot)))
      value_slots
  in
  let context =
    C.create ~dacc_prior_to_sets:dacc ~simplify_function_body
      ~all_sets_of_closures:[set_of_closures]
      ~closure_bound_names_all_sets:[closure_bound_names]
      ~value_slot_types_all_sets:[value_slot_types]
  in
  let closure_bound_names_inside =
    C.closure_bound_names_inside_functions_exactly_one_set context
  in
  let { set_of_closures; dacc } =
    simplify_set_of_closures0 dacc context set_of_closures ~closure_bound_names
      ~closure_bound_names_inside ~value_slots ~value_slot_types
  in
  let closure_symbols_set =
    Symbol.Set.of_list (Function_slot.Lmap.data closure_symbols)
  in
  assert (
    Symbol.Set.cardinal closure_symbols_set
    = Function_slot.Map.cardinal closure_symbols_map);
  let denv = DA.denv dacc in
  let closure_symbols_with_types =
    Function_slot.Lmap.map
      (fun symbol ->
        let typ = DE.find_symbol denv symbol in
        symbol, typ)
      closure_symbols
  in
  let set_of_closures_lifted_constant =
    LC.create_set_of_closures denv ~closure_symbols_with_types
      ~symbol_projections
      (Rebuilt_static_const.create_set_of_closures
         (DE.are_rebuilding_terms denv)
         set_of_closures)
  in
  let dacc =
    DA.add_to_lifted_constant_accumulator ~also_add_to_env:() dacc
      (LCS.singleton set_of_closures_lifted_constant)
  in
  let denv, bindings =
    List.fold_left_map
      (fun denv (function_slot, closure_symbol) ->
        match Function_slot.Map.find function_slot closure_bound_vars with
        | exception Not_found ->
          Misc.fatal_errorf "No bound variable for function slot %a"
            Function_slot.print function_slot
        | bound_var ->
          let denv =
            let simple = Simple.symbol closure_symbol in
            let typ = T.alias_type_of K.value simple in
            DE.add_variable denv bound_var typ
          in
          let binding = bound_var, closure_symbol in
          denv, binding)
      (DA.denv dacc)
      (Function_slot.Lmap.bindings closure_symbols)
  in
  Simplify_named_result.create_have_lifted_set_of_closures
    (DA.with_denv dacc denv) bindings
    ~original_defining_expr:(Named.create_set_of_closures set_of_closures)

let simplify_non_lifted_set_of_closures0 dacc bound_vars ~closure_bound_vars
    set_of_closures ~value_slots ~value_slot_types ~simplify_function_body =
  let closure_bound_names =
    Function_slot.Map.map Bound_name.create_var closure_bound_vars
  in
  let context =
    C.create ~dacc_prior_to_sets:dacc ~simplify_function_body
      ~all_sets_of_closures:[set_of_closures]
      ~closure_bound_names_all_sets:[closure_bound_names]
      ~value_slot_types_all_sets:[value_slot_types]
  in
  let closure_bound_names_inside =
    C.closure_bound_names_inside_functions_exactly_one_set context
  in
  let { set_of_closures; dacc } =
    simplify_set_of_closures0 dacc context set_of_closures ~closure_bound_names
      ~closure_bound_names_inside ~value_slots ~value_slot_types
  in
  let defining_expr =
    let named = Named.create_set_of_closures set_of_closures in
    let find_code_characteristics code_id =
      let env = Downwards_acc.denv dacc in
      let code_metadata =
        DE.find_code_exn env code_id |> Code_or_metadata.code_metadata
      in
      Cost_metrics.
        { cost_metrics = Code_metadata.cost_metrics code_metadata;
          params_arity =
            Flambda_arity.num_params (Code_metadata.params_arity code_metadata)
        }
    in
    Simplified_named.create_with_known_free_names ~find_code_characteristics
      (Named.create_set_of_closures set_of_closures)
      ~free_names:(Named.free_names named)
  in
  Simplify_named_result.create dacc
    [ { Expr_builder.let_bound = bound_vars;
        simplified_defining_expr = defining_expr;
        original_defining_expr =
          Some (Named.create_set_of_closures set_of_closures)
      } ]

type lifting_decision_result =
  { can_lift : bool;
    value_slots : Simple.t Value_slot.Map.t;
    value_slot_types : T.t Value_slot.Map.t;
    symbol_projections : Symbol_projection.t Variable.Map.t
  }

let type_value_slots_and_make_lifting_decision_for_one_set dacc
    ~name_mode_of_bound_vars set_of_closures =
  (* By computing the types of the closure elements, attempt to show that the
     set of closures can be lifted, and hence statically allocated. Note that
     simplifying the bodies of the functions won't change the set-of-closures'
     eligibility for lifting. That this is so follows from the fact that closure
     elements cannot be deleted without an analysis across the whole source
     file, as an inlined function's body may reference them out of scope of the
     closure declaration. (Such deletions end up occurring during the upwards
     traversal and during [To_cmm], by which time the necessary information is
     available.) *)
  let value_slots, value_slot_types, symbol_projections =
    Value_slot.Map.fold
      (fun value_slot env_entry
           (value_slots, value_slot_types, symbol_projections) ->
        let env_entry, ty, symbol_projections =
          let ty =
            S.simplify_simple dacc env_entry
              ~min_name_mode:name_mode_of_bound_vars
          in
          let simple = T.get_alias_exn ty in
          (* Note down separately if [simple] remains a variable and is known to
             be equal to a projection from a symbol. *)
          let symbol_projections =
            Simple.pattern_match' simple
              ~const:(fun _ -> symbol_projections)
              ~symbol:(fun _ ~coercion:_ -> symbol_projections)
              ~var:(fun var ~coercion:_ ->
                (* [var] will already be canonical, by virtue of the semantics
                   of [S.simplify_simple], as we require for the symbol
                   projections map. *)
                match DE.find_symbol_projection (DA.denv dacc) var with
                | None -> symbol_projections
                | Some proj -> Variable.Map.add var proj symbol_projections)
          in
          simple, ty, symbol_projections
        in
        let value_slots = Value_slot.Map.add value_slot env_entry value_slots in
        let value_slot_types =
          Value_slot.Map.add value_slot ty value_slot_types
        in
        value_slots, value_slot_types, symbol_projections)
      (Set_of_closures.value_slots set_of_closures)
      (Value_slot.Map.empty, Value_slot.Map.empty, Variable.Map.empty)
  in
  let can_lift_coercion coercion =
    NO.no_variables (Coercion.free_names coercion)
  in
  (* Note that [closure_bound_vars_inverse] doesn't need to include variables
     binding closures in other mutually-recursive sets, since if we get here in
     the case where we are considering lifting a set that has not been lifted
     before, there are never any other mutually-recursive sets ([Named.t] does
     not allow them). *)
  let[@inline] variable_permits_lifting var =
    (* Variables (excluding ones bound to symbol projections; see below) in the
       definition of the set of closures will currently prevent lifting if the
       allocation mode is [Local] and we cannot show that such variables never
       hold locally-allocated blocks (pointers to which from
       statically-allocated blocks are forbidden). Also see comment in
       types/reify.ml.

       If [var] is known to be a symbol projection, it doesn't matter if it
       isn't in scope at the place where we will eventually insert the "let
       symbol", as the binding to the projection from the relevant symbol can
       always be rematerialised. Likewise no
       [never_holds_locally_allocated_values] check is needed in the symbol
       projection case, since we are projecting from a value that has already
       been deemed eligible for lifting. *)
    Variable.Map.mem var symbol_projections
    || DE.is_defined_at_toplevel (DA.denv dacc) var
       &&
       match Set_of_closures.alloc_mode set_of_closures with
       | Local _ -> (
         match
           T.never_holds_locally_allocated_values (DA.typing_env dacc) var
         with
         | Proved () -> true
         | Unknown -> false)
       | Heap -> true
  in
  let value_slot_permits_lifting _value_slot simple =
    can_lift_coercion (Simple.coercion simple)
    && Simple.pattern_match' simple
         ~const:(fun _ -> true)
         ~symbol:(fun _ ~coercion:_ -> true)
         ~var:(fun var ~coercion:_ -> variable_permits_lifting var)
  in
  let can_lift =
    Name_mode.is_normal name_mode_of_bound_vars
    && Value_slot.Map.for_all value_slot_permits_lifting value_slots
  in
  { can_lift; value_slots; value_slot_types; symbol_projections }

let simplify_non_lifted_set_of_closures dacc (bound_vars : Bound_pattern.t)
    set_of_closures =
  let closure_bound_vars = Bound_pattern.must_be_set_of_closures bound_vars in
  (* CR-someday mshinwell: This should probably be handled differently, but will
     require some threading through *)
  let name_mode_of_bound_vars = Bound_pattern.name_mode bound_vars in
  let closure_bound_vars, closure_bound_vars_inverse =
    List.fold_left2
      (fun (closure_bound_vars, closure_bound_vars_inverse) function_slot var ->
        ( Function_slot.Map.add function_slot var closure_bound_vars,
          Variable.Map.add (Bound_var.var var) function_slot
            closure_bound_vars_inverse ))
      (Function_slot.Map.empty, Variable.Map.empty)
      (Set_of_closures.function_decls set_of_closures
      |> Function_declarations.funs_in_order |> Function_slot.Lmap.keys)
      closure_bound_vars
  in
  let { can_lift; value_slots; value_slot_types; symbol_projections } =
    type_value_slots_and_make_lifting_decision_for_one_set dacc
      ~name_mode_of_bound_vars set_of_closures
  in
  if can_lift
  then
    simplify_and_lift_set_of_closures dacc ~closure_bound_vars_inverse
      ~closure_bound_vars set_of_closures ~value_slots ~symbol_projections
  else
    simplify_non_lifted_set_of_closures0 dacc bound_vars ~closure_bound_vars
      set_of_closures ~value_slots ~value_slot_types

let simplify_lifted_set_of_closures0 dacc context ~closure_symbols
    ~closure_bound_names_inside ~value_slots ~value_slot_types set_of_closures =
  let closure_bound_names =
    Function_slot.Lmap.map Bound_name.create_symbol closure_symbols
    |> Function_slot.Lmap.bindings |> Function_slot.Map.of_list
  in
  let { set_of_closures; dacc } =
    simplify_set_of_closures0 dacc context set_of_closures ~closure_bound_names
      ~closure_bound_names_inside ~value_slots ~value_slot_types
  in
  let set_of_closures_pattern =
    Bound_static.Pattern.set_of_closures closure_symbols
  in
  let set_of_closures_static_const =
    Rebuilt_static_const.create_set_of_closures
      (DA.are_rebuilding_terms dacc)
      set_of_closures
  in
  set_of_closures_pattern, set_of_closures_static_const, dacc

module List = struct
  include List

  let rec fold_left3 f accu l1 l2 l3 =
    match l1, l2, l3 with
    | [], [], [] -> accu
    | a1 :: l1, a2 :: l2, a3 :: l3 -> fold_left3 f (f accu a1 a2 a3) l1 l2 l3
    | _, _, _ -> invalid_arg "List.fold_left3"
end

let simplify_lifted_sets_of_closures dacc ~all_sets_of_closures_and_symbols
    ~closure_bound_names_all_sets ~simplify_function_body =
  let all_sets_of_closures = List.map snd all_sets_of_closures_and_symbols in
  let value_slots_and_types_all_sets =
    List.map
      (fun set_of_closures ->
        let { can_lift = _;
              value_slots;
              value_slot_types;
              symbol_projections = _
            } =
          type_value_slots_and_make_lifting_decision_for_one_set dacc
            ~name_mode_of_bound_vars:Name_mode.normal set_of_closures
        in
        value_slots, value_slot_types)
      all_sets_of_closures
  in
  let value_slot_types_all_sets = List.map snd value_slots_and_types_all_sets in
  let context =
    C.create ~dacc_prior_to_sets:dacc ~simplify_function_body
      ~all_sets_of_closures ~closure_bound_names_all_sets
      ~value_slot_types_all_sets
  in
  let closure_bound_names_inside_functions_all_sets =
    C.closure_bound_names_inside_functions_all_sets context
  in
  List.fold_left3
    (fun (patterns_acc, static_consts_acc, dacc)
         (closure_symbols, set_of_closures) closure_bound_names_inside
         (value_slots, value_slot_types) ->
      let pattern, static_const, dacc =
        simplify_lifted_set_of_closures0 dacc context ~closure_symbols
          ~closure_bound_names_inside ~value_slots ~value_slot_types
          set_of_closures
      in
      (* The order doesn't matter here -- see comment in [Simplify_static_const]
         where this function is called from. *)
      let static_const_group =
        Rebuilt_static_const.Group.add static_const static_consts_acc
      in
      ( Bound_static.concat (Bound_static.singleton pattern) patterns_acc,
        static_const_group,
        dacc ))
    (Bound_static.empty, Rebuilt_static_const.Group.empty, dacc)
    all_sets_of_closures_and_symbols
    closure_bound_names_inside_functions_all_sets value_slots_and_types_all_sets

let simplify_stub_function dacc code ~all_code ~simplify_function_body =
  let context = C.create_for_stub dacc ~all_code ~simplify_function_body in
  let closure_bound_names_inside_function =
    (* Unused, the type of the value slot is going to be unknown *)
    Function_slot.Map.empty
  in
  let { code_id = _; code; outer_dacc; should_resimplify = _ } =
    simplify_function0 context ~outer_dacc:dacc None (Code.code_id code) code
      ~closure_bound_names_inside_function
  in
  let code =
    match code with
    | None -> assert false
    | Some (_, code_constant) -> code_constant
  in
  code, outer_dacc
