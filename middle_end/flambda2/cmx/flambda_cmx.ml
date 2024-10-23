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

module EC = Exported_code
module T = Flambda2_types
module TE = Flambda2_types.Typing_env
module Imported_unit_map = Global_module.Name.Map

type loader =
  { get_module_info : Compilation_unit.t -> Flambda_cmx_format.t option;
    mutable imported_names : Name.Set.t;
    mutable imported_code : Exported_code.t;
    mutable imported_units : TE.Serializable.t option Imported_unit_map.t
  }

let load_cmx_file_contents loader comp_unit =
  let accessible_comp_unit =
    Compilation_unit.which_cmx_file comp_unit
      ~accessed_by:(Compilation_unit.get_current_exn ())
  in
  let cmx_file =
    Compilation_unit.to_global_name_without_prefix accessible_comp_unit
  in
  match Imported_unit_map.find cmx_file loader.imported_units with
  | typing_env_or_none -> typing_env_or_none
  | exception Not_found -> (
    match loader.get_module_info accessible_comp_unit with
    | None ->
      (* To make things easier to think about, we never retry after a .cmx load
         fails. *)
      loader.imported_units
        <- Imported_unit_map.add cmx_file None loader.imported_units;
      None
    | Some cmx ->
      let typing_env, all_code =
        Flambda_cmx_format.import_typing_env_and_code cmx
      in
      let newly_imported_names = TE.Serializable.name_domain typing_env in
      loader.imported_names
        <- Name.Set.union newly_imported_names loader.imported_names;
      loader.imported_code <- EC.merge all_code loader.imported_code;
      let offsets = Flambda_cmx_format.exported_offsets cmx in
      Exported_offsets.import_offsets offsets;
      loader.imported_units
        <- Imported_unit_map.add cmx_file (Some typing_env)
             loader.imported_units;
      Some typing_env)

let load_symbol_approx loader symbol : Code_or_metadata.t Value_approximation.t
    =
  let comp_unit = Symbol.compilation_unit symbol in
  match load_cmx_file_contents loader comp_unit with
  | None -> Value_unknown
  | Some typing_env ->
    let find_code code_id =
      match Exported_code.find loader.imported_code code_id with
      | Some code_or_meta -> code_or_meta
      | _ ->
        Misc.fatal_errorf
          "Failed to load informations for %a. Code id not found." Code_id.print
          code_id
    in
    T.Typing_env.Serializable.extract_symbol_approx typing_env symbol find_code

let all_predefined_exception_symbols () =
  let symbol_for_global id =
    Flambda2_import.Symbol.for_predef_ident id |> Symbol.create_wrapped
  in
  Predef.all_predef_exns |> List.map symbol_for_global |> Symbol.Set.of_list

let predefined_exception_typing_env () =
  let comp_unit = Compilation_unit.get_current () in
  Compilation_unit.set_current (Some Compilation_unit.predef_exn);
  let typing_env =
    TE.Serializable.predefined_exceptions (all_predefined_exception_symbols ())
  in
  Compilation_unit.set_current comp_unit;
  typing_env

let create_loader ~get_module_info =
  let loader =
    { get_module_info;
      imported_names = Name.Set.empty;
      imported_code = Exported_code.empty;
      imported_units = Imported_unit_map.empty
    }
  in
  let predefined_exception_typing_env = predefined_exception_typing_env () in
  loader.imported_units
    <- Imported_unit_map.singleton
         (Compilation_unit.Name.to_global_name Compilation_unit.Name.predef_exn)
         (Some predefined_exception_typing_env);
  loader.imported_names
    <- TE.Serializable.name_domain predefined_exception_typing_env;
  loader

let get_imported_names loader () = loader.imported_names

let get_imported_code loader () = loader.imported_code

let compute_reachable_names_and_code ~module_symbol ~free_names_of_name code =
  let rec fixpoint names_to_add names_already_added =
    if Name_occurrences.is_empty names_to_add
    then names_already_added
    else
      let names_already_added =
        Name_occurrences.union names_to_add names_already_added
      in
      let fold_code_id names_to_add code_id =
        if not
             (Code_id.in_compilation_unit code_id
                (Compilation_unit.get_current_exn ()))
        then
          (* Code in units upon which the current unit depends cannot reference
             this unit. *)
          names_to_add
        else
          match Exported_code.find code code_id with
          | None -> names_to_add
          | Some code_or_metadata ->
            let free_names = Code_or_metadata.free_names code_or_metadata in
            let names_to_consider =
              Name_occurrences
              .with_only_names_and_code_ids_promoting_newer_version_of
                free_names
            in
            let new_names =
              Name_occurrences.diff names_to_consider
                ~without:names_already_added
            in
            Name_occurrences.union new_names names_to_add
      in
      let fold_name names_to_add name =
        if not
             (Compilation_unit.equal
                (Name.compilation_unit name)
                (Compilation_unit.get_current_exn ()))
        then
          (* Names in units upon which the current unit depends cannot reference
             this unit. *)
          names_to_add
        else
          match free_names_of_name name with
          | Some ty_names ->
            let names_to_consider =
              Name_occurrences
              .with_only_names_and_code_ids_promoting_newer_version_of ty_names
            in
            let new_names =
              Name_occurrences.diff names_to_consider
                ~without:names_already_added
            in
            Name_occurrences.union new_names names_to_add
          | None ->
            (* A missing type cannot refer to names defined in the current
               unit *)
            names_to_add
      in
      let from_code_ids =
        Name_occurrences.fold_code_ids names_to_add ~init:Name_occurrences.empty
          ~f:fold_code_id
      in
      let from_names_and_code_ids =
        Name_occurrences.fold_names names_to_add ~init:from_code_ids
          ~f:fold_name
      in
      fixpoint from_names_and_code_ids names_already_added
  in
  let init_names =
    Name_occurrences.singleton_symbol module_symbol Name_mode.normal
  in
  fixpoint init_names Name_occurrences.empty

let prepare_cmx ~module_symbol create_typing_env ~free_names_of_name
    ~used_value_slots ~canonicalise ~exported_offsets all_code =
  let reachable_names =
    compute_reachable_names_and_code ~module_symbol ~free_names_of_name all_code
  in
  let all_code =
    (* CR mshinwell: do we need to remove unused function slot bindings from the
       result types too? *)
    all_code
    |> EC.remove_unused_value_slots_from_result_types_and_shortcut_aliases
         ~used_value_slots ~canonicalise
    |> EC.remove_unreachable ~reachable_names
  in
  let final_typing_env = create_typing_env reachable_names in
  (* We need to re-export offsets for everything reachable from the cmx file;
     value_slots/ids can be reachable from the typing env, and the exported
     code.

     In the case of the exported code, we already have offsets for all
     function_slots/vars reachable from the code of the current compilation
     unit, but since we also re-export code metadata (including return types)
     from other compilation units, we need to take those into account. *)
  (* CR gbury: it might be more efficient to not compute the free names for all
     exported code, but fold over the exported code to avoid allocating some
     free_names *)
  let free_names_of_all_code = EC.free_names all_code in
  let slots_used_in_typing_env =
    TE.Serializable.free_function_slots_and_value_slots final_typing_env
  in
  let exported_offsets =
    exported_offsets
    |> Exported_offsets.reexport_function_slots
         (Name_occurrences.all_function_slots free_names_of_all_code)
    |> Exported_offsets.reexport_value_slots
         (Name_occurrences.all_value_slots free_names_of_all_code)
    |> Exported_offsets.reexport_function_slots
         (Name_occurrences.all_function_slots slots_used_in_typing_env)
    |> Exported_offsets.reexport_value_slots
         (Name_occurrences.all_value_slots slots_used_in_typing_env)
  in
  let cmx =
    Flambda_cmx_format.create ~final_typing_env ~all_code ~exported_offsets
      ~used_value_slots
  in
  reachable_names, Some cmx

let prepare_cmx_file_contents ~final_typing_env ~module_symbol ~used_value_slots
    ~exported_offsets all_code =
  match final_typing_env with
  | None ->
    Name_occurrences.singleton_symbol module_symbol Name_mode.normal, None
  | Some _ when Flambda_features.opaque () ->
    Name_occurrences.singleton_symbol module_symbol Name_mode.normal, None
  | Some final_typing_env ->
    let typing_env, canonicalise =
      TE.Pre_serializable.create final_typing_env ~used_value_slots
    in
    let create_typing_env reachable_names =
      TE.Serializable.create typing_env ~reachable_names
    in
    let free_names_of_name name =
      Option.map T.free_names
        (TE.Pre_serializable.find_or_missing typing_env name)
    in
    prepare_cmx ~module_symbol create_typing_env ~free_names_of_name
      ~used_value_slots ~canonicalise ~exported_offsets all_code

let prepare_cmx_from_approx ~approxs ~module_symbol ~exported_offsets
    ~used_value_slots all_code =
  if Flambda_features.opaque ()
  then Name_occurrences.singleton_symbol module_symbol Name_mode.normal, None
  else
    let create_typing_env reachable_names =
      let approxs =
        Symbol.Map.filter
          (fun sym _ -> Name_occurrences.mem_symbol reachable_names sym)
          approxs
      in
      TE.Serializable.create_from_closure_conversion_approx approxs
    in
    let free_names_of_name name =
      let symbol = Name.must_be_symbol name in
      match Symbol.Map.find_opt symbol approxs with
      | None -> None
      | Some approx ->
        Some
          (Value_approximation.free_names
             ~code_free_names:Code_or_metadata.free_names approx)
    in
    prepare_cmx ~module_symbol create_typing_env ~free_names_of_name
      ~used_value_slots
      ~canonicalise:(fun id -> id)
      ~exported_offsets all_code
