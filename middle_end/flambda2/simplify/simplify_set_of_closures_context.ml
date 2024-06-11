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

type t =
  { dacc_prior_to_sets : DA.t;
    simplify_function_body : Simplify_common.simplify_function_body;
    dacc_inside_functions : DA.t;
    closure_bound_names_inside_functions_all_sets :
      Bound_name.t Function_slot.Map.t list;
    old_to_new_code_ids_all_sets : Code_id.t Code_id.Map.t;
    previously_free_depth_variables : Variable.Set.t
  }

let function_decl_type ?new_code_id ~rec_info old_code_id =
  let code_id = Option.value new_code_id ~default:old_code_id in
  Or_unknown_or_bottom.Ok (T.Function_type.create code_id ~rec_info)

let create_for_stub dacc ~all_code ~simplify_function_body =
  let dacc_inside_functions =
    (* We ensure that inlining cannot happen inside the code of stubs. This is
       to avoid compile-time performance problems where large functions (or
       other stubs) might get inlined repeatedly into stubs. One place where
       this was seen was code generated for labelled / optional argument
       application when there are a lot of parameters. *)
    DA.map_denv dacc ~f:(fun denv ->
        Code_id.Map.fold
          (fun code_id code denv -> DE.define_code denv ~code_id ~code)
          all_code
          (DE.enter_set_of_closures (DE.disable_inlining denv)))
  in
  { dacc_prior_to_sets = dacc;
    simplify_function_body;
    dacc_inside_functions;
    closure_bound_names_inside_functions_all_sets = [];
    old_to_new_code_ids_all_sets = Code_id.Map.empty;
    previously_free_depth_variables = Variable.Set.empty
  }

let simplify_function_body t = t.simplify_function_body

let dacc_prior_to_sets t = t.dacc_prior_to_sets

let dacc_inside_functions t = t.dacc_inside_functions

let old_to_new_code_ids_all_sets t = t.old_to_new_code_ids_all_sets

let closure_bound_names_inside_functions_all_sets t =
  t.closure_bound_names_inside_functions_all_sets

let closure_bound_names_inside_functions_exactly_one_set t =
  match t.closure_bound_names_inside_functions_all_sets with
  | [closure_bound_names_inside] -> closure_bound_names_inside
  | [] | _ :: _ :: _ -> Misc.fatal_error "Only one set of closures was expected"

let previously_free_depth_variables t = t.previously_free_depth_variables

let compute_value_slot_types_inside_function ~value_slot_types
    ~degraded_value_slots =
  Value_slot.Map.mapi
    (fun value_slot type_prior_to_sets ->
      let type_prior_to_sets =
        (* See comment below about [degraded_value_slots]. *)
        if Value_slot.Set.mem value_slot degraded_value_slots
        then T.any_value
        else type_prior_to_sets
      in
      type_prior_to_sets)
    value_slot_types

let compute_closure_types_inside_functions ~denv ~all_sets_of_closures
    ~closure_bound_names_all_sets ~value_slot_types_inside_functions_all_sets
    ~old_to_new_code_ids_all_sets =
  (* There is explicit recursion in closure types, meaning that aliases must be
     used in order to build the necessary types. In the case where the set of
     closures will be lifted (note that the lifting decision has already been
     made by this point), then the names used for the aliases are just the
     closure symbols. In the non-lifted case we just need a list of variables,
     so we reuse the "closure bound names". If any of these variables escape in
     the function result types they will be captured by the name abstraction
     used there. *)
  let closure_types_via_aliases_all_sets =
    List.map
      (fun closure_bound_names_inside ->
        Function_slot.Map.map
          (fun name ->
            T.alias_type_of K.value (Simple.name (Bound_name.name name)))
          closure_bound_names_inside)
      closure_bound_names_all_sets
  in
  let closure_types_inside_functions =
    List.map2
      (fun set_of_closures
           (closure_types_via_aliases, value_slot_types_inside_function) ->
        let function_decls = Set_of_closures.function_decls set_of_closures in
        let all_function_slots_in_set =
          Function_slot.Map.mapi
            (fun function_slot
                 (old_code_id :
                   Function_declarations.code_id_in_function_declaration) ->
              match old_code_id with
              | Deleted _ -> Or_unknown_or_bottom.Unknown
              | Code_id old_code_id ->
                let code_or_metadata = DE.find_code_exn denv old_code_id in
                let new_code_id =
                  (* The types of the functions involved should reference the
                     _new_ code IDs (where such exist), so that direct recursive
                     calls can be compiled straight to the new code. *)
                  if Code_or_metadata.code_present code_or_metadata
                     && not
                          (Code_metadata.stub
                             (Code_or_metadata.code_metadata code_or_metadata))
                  then Code_id.Map.find old_code_id old_to_new_code_ids_all_sets
                  else old_code_id
                in
                let rec_info =
                  (* From inside their own bodies, every function in the set
                     currently being defined has an unknown recursion depth *)
                  T.unknown K.rec_info
                in
                let code_metadata =
                  code_or_metadata |> Code_or_metadata.code_metadata
                in
                let absolute_history, _relative_history =
                  DE.inlining_history_tracker denv
                  |> Inlining_history.Tracker.fundecl
                       ~dbg:(Code_metadata.dbg code_metadata)
                       ~function_relative_history:
                         (Code_metadata.relative_history code_metadata)
                       ~name:(Function_slot.name function_slot)
                in
                Inlining_report.record_decision_at_function_definition
                  ~absolute_history ~code_metadata ~pass:Before_simplify
                  ~are_rebuilding_terms:(DE.are_rebuilding_terms denv)
                  (Code_metadata.inlining_decision code_metadata);
                function_decl_type old_code_id ~new_code_id ~rec_info)
            (Function_declarations.funs function_decls)
        in
        Function_slot.Map.mapi
          (fun function_slot _function_decl ->
            T.exactly_this_closure function_slot ~all_function_slots_in_set
              ~all_closure_types_in_set:closure_types_via_aliases
              ~all_value_slots_in_set:value_slot_types_inside_function
              (Alloc_mode.For_allocations.as_type
                 (Set_of_closures.alloc_mode set_of_closures)))
          all_function_slots_in_set)
      all_sets_of_closures
      (List.combine closure_types_via_aliases_all_sets
         value_slot_types_inside_functions_all_sets)
  in
  closure_bound_names_all_sets, closure_types_inside_functions

let bind_closure_types_inside_functions denv_inside_functions
    ~closure_bound_names_inside_functions_all_sets
    ~closure_types_inside_functions_all_sets =
  let denv_inside_functions =
    List.fold_left
      (fun denv closure_bound_names_inside ->
        Function_slot.Map.fold
          (fun _function_slot bound_name denv ->
            let name = Bound_name.name bound_name in
            let in_types = not (Bound_name.is_symbol bound_name) in
            let bound_name =
              Bound_name.create name
                (if in_types then NM.in_types else NM.normal)
            in
            DE.define_name denv bound_name K.value)
          closure_bound_names_inside denv)
      denv_inside_functions closure_bound_names_inside_functions_all_sets
  in
  List.fold_left2
    (fun denv closure_bound_names_inside_functions_one_set
         closure_types_inside_functions_one_set ->
      Function_slot.Map.fold
        (fun function_slot closure_type denv ->
          match
            Function_slot.Map.find function_slot
              closure_bound_names_inside_functions_one_set
          with
          | exception Not_found ->
            Misc.fatal_errorf
              "No closure name for function slot %a.@ \
               closure_bound_names_inside_functions_one_set = %a."
              Function_slot.print function_slot
              (Function_slot.Map.print Bound_name.print)
              closure_bound_names_inside_functions_one_set
          | bound_name ->
            DE.add_equation_on_name denv
              (Bound_name.name bound_name)
              closure_type)
        closure_types_inside_functions_one_set denv)
    denv_inside_functions closure_bound_names_inside_functions_all_sets
    closure_types_inside_functions_all_sets

let compute_old_to_new_code_ids_all_sets denv ~all_sets_of_closures =
  List.fold_left
    (fun old_to_new_code_ids_all_sets set_of_closures ->
      let function_decls = Set_of_closures.function_decls set_of_closures in
      Function_slot.Map.fold
        (fun _
             (old_code_id :
               Function_declarations.code_id_in_function_declaration)
             old_to_new_code_ids ->
          match old_code_id with
          | Deleted _ -> old_to_new_code_ids
          | Code_id old_code_id ->
            let code =
              try DE.find_code_exn denv old_code_id
              with Not_found ->
                Misc.fatal_errorf "Missing code for %a" Code_id.print
                  old_code_id
            in
            if Code_or_metadata.code_present code
               && not (Code_metadata.stub (Code_or_metadata.code_metadata code))
            then
              let new_code_id = Code_id.rename old_code_id in
              Code_id.Map.add old_code_id new_code_id old_to_new_code_ids
            else old_to_new_code_ids)
        (Function_declarations.funs function_decls)
        old_to_new_code_ids_all_sets)
    Code_id.Map.empty all_sets_of_closures

let bind_existing_code_to_new_code_ids denv ~old_to_new_code_ids_all_sets =
  Code_id.Map.fold
    (fun old_code_id new_code_id denv ->
      let code = DE.find_code_exn denv old_code_id in
      if Code_or_metadata.code_present code
         && not (Code_metadata.stub (Code_or_metadata.code_metadata code))
      then
        let code =
          Code_or_metadata.get_code code
          |> Code.with_newer_version_of (Some old_code_id)
          |> Code.with_code_id new_code_id
        in
        DE.define_code denv ~code_id:new_code_id ~code
      else denv)
    old_to_new_code_ids_all_sets denv

let create ~dacc_prior_to_sets ~simplify_function_body ~all_sets_of_closures
    ~closure_bound_names_all_sets ~value_slot_types_all_sets =
  let denv = DA.denv dacc_prior_to_sets in
  let denv_inside_functions =
    denv |> DE.enter_set_of_closures
    (* Even if we are not rebuilding terms we should always rebuild them for
       local functions. The type of a function is dependent on its term and not
       knowing it prohibits us from inlining it. *)
    |> DE.set_rebuild_terms
  in
  (* We collect a set of "degraded value slots" whose types involve imported
     variables from missing .cmx files. Since we don't know the kind of these
     variables, we can't run the code below that checks if they might need
     binding as "never inline" depth variables (since we don't know if a given
     variable is a depth variable or not). Instead we will treat the whole value
     slot as having [Unknown] type. *)
  let degraded_value_slots = ref Value_slot.Set.empty in
  let free_depth_variables =
    List.concat_map
      (fun value_slot_types ->
        Value_slot.Map.mapi
          (fun value_slot ty ->
            let vars = TE.free_names_transitive (DE.typing_env denv) ty in
            NO.fold_variables vars ~init:Variable.Set.empty
              ~f:(fun free_depth_variables var ->
                let ty_opt =
                  TE.find_or_missing
                    (DE.typing_env denv_inside_functions)
                    (Name.var var)
                in
                match ty_opt with
                | None ->
                  degraded_value_slots
                    := Value_slot.Set.add value_slot !degraded_value_slots;
                  free_depth_variables
                | Some ty -> (
                  match T.kind ty with
                  | Rec_info -> Variable.Set.add var free_depth_variables
                  | Value | Naked_number _ | Region -> free_depth_variables)))
          value_slot_types
        |> Value_slot.Map.data)
      value_slot_types_all_sets
    |> Variable.Set.union_list
  in
  (* Pretend that any depth variables appearing free in the closure elements are
     bound to "never inline anything" in the function. This ensures that
     in-types depth variables do not end up in terms. *)
  (* CR-someday lmaurer: It would be better to propagate depth variables into
     closures properly, as this would allow things like unrolling [Seq.map]
     where the recursive call goes through a closure. For the moment, we often
     just stop unrolling cold in that situation. (It's important that we use
     [Rec_info_expr.do_not_inline] here so that we don't start unrolling, since
     without propagating the rec info into the closure, we don't know when to
     stop unrolling.)

     mshinwell: Leo and I have discussed allowing In_types variables in
     closures, which should cover this case, if we allowed such variables to be
     of kinds other than [Value]. *)
  let denv_inside_functions =
    Variable.Set.fold
      (fun dv env_inside_functions ->
        let name = Name.var dv in
        DE.add_equation_on_name env_inside_functions name
          (T.this_rec_info Rec_info_expr.do_not_inline))
      free_depth_variables denv_inside_functions
  in
  let value_slot_types_all_sets_inside_functions_rev =
    List.fold_left
      (fun value_slot_types_all_sets_inside_functions_rev value_slot_types ->
        let value_slot_types_inside_function =
          compute_value_slot_types_inside_function ~value_slot_types
            ~degraded_value_slots:!degraded_value_slots
        in
        value_slot_types_inside_function
        :: value_slot_types_all_sets_inside_functions_rev)
      [] value_slot_types_all_sets
  in
  let value_slot_types_inside_functions_all_sets =
    List.rev value_slot_types_all_sets_inside_functions_rev
  in
  let old_to_new_code_ids_all_sets =
    compute_old_to_new_code_ids_all_sets denv ~all_sets_of_closures
  in
  let ( closure_bound_names_inside_functions_all_sets,
        closure_types_inside_functions_all_sets ) =
    compute_closure_types_inside_functions ~denv ~all_sets_of_closures
      ~closure_bound_names_all_sets ~value_slot_types_inside_functions_all_sets
      ~old_to_new_code_ids_all_sets
  in
  let dacc_inside_functions =
    denv_inside_functions
    |> bind_existing_code_to_new_code_ids ~old_to_new_code_ids_all_sets
    |> bind_closure_types_inside_functions
         ~closure_bound_names_inside_functions_all_sets
         ~closure_types_inside_functions_all_sets
    |> DA.with_denv dacc_prior_to_sets
  in
  { dacc_prior_to_sets;
    dacc_inside_functions;
    closure_bound_names_inside_functions_all_sets;
    old_to_new_code_ids_all_sets;
    simplify_function_body;
    previously_free_depth_variables = free_depth_variables
  }
