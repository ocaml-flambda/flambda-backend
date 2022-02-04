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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module EC = Exported_code
module T = Flambda2_types
module TE = Flambda2_types.Typing_env

let rec load_cmx_file_contents ~get_global_info comp_unit ~imported_units
    ~imported_names ~imported_code =
  match Compilation_unit.Map.find comp_unit !imported_units with
  | typing_env_or_none -> typing_env_or_none
  | exception Not_found -> (
    match get_global_info comp_unit with
    | None ->
      (* To make things easier to think about, we never retry after a .cmx load
         fails. *)
      imported_units := Compilation_unit.Map.add comp_unit None !imported_units;
      None
    | Some cmx ->
      let resolver comp_unit =
        load_cmx_file_contents ~get_global_info comp_unit ~imported_names
          ~imported_code ~imported_units
      in
      let get_imported_names () = !imported_names in
      let typing_env, all_code =
        Flambda_cmx_format.import_typing_env_and_code cmx
      in
      let typing_env =
        TE.Serializable.to_typing_env ~resolver ~get_imported_names typing_env
      in
      let newly_imported_names = TE.name_domain typing_env in
      imported_names := Name.Set.union newly_imported_names !imported_names;
      imported_code := EC.merge all_code !imported_code;
      let offsets = Flambda_cmx_format.exported_offsets cmx in
      Exported_offsets.import_offsets offsets;
      imported_units
        := Compilation_unit.Map.add comp_unit (Some typing_env) !imported_units;
      Some typing_env)

let compute_reachable_names_and_code ~module_symbol typing_env code =
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
              Name_occurrences.diff names_to_consider names_already_added
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
          match TE.Pre_serializable.find_or_missing typing_env name with
          | Some ty ->
            let ty_names = T.free_names ty in
            let names_to_consider =
              Name_occurrences
              .with_only_names_and_code_ids_promoting_newer_version_of ty_names
            in
            let new_names =
              Name_occurrences.diff names_to_consider names_already_added
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

let prepare_cmx_file_contents ~final_typing_env ~module_symbol
    ~used_closure_vars ~exported_offsets all_code =
  match final_typing_env with
  | None -> None
  | Some _ when Flambda_features.opaque () -> None
  | Some final_typing_env ->
    let final_typing_env, canonicalise =
      TE.Pre_serializable.create final_typing_env ~used_closure_vars
    in
    let reachable_names =
      compute_reachable_names_and_code ~module_symbol final_typing_env all_code
    in
    let all_code =
      (* CR mshinwell: do we need to remove unused closure ID bindings from the
         result types too? *)
      all_code
      |> EC.remove_unused_closure_vars_from_result_types_and_shortcut_aliases
           ~used_closure_vars ~canonicalise
      |> EC.remove_unreachable ~reachable_names
    in
    let final_typing_env =
      TE.Serializable.create final_typing_env ~reachable_names
    in
    let closure_elts_used_in_typing_env =
      TE.Serializable.free_closure_ids_and_closure_vars final_typing_env
    in
    let exported_offsets =
      exported_offsets
      |> Closure_offsets.collect_used_closure_ids
           (Name_occurrences.closure_ids closure_elts_used_in_typing_env)
      |> Closure_offsets.collect_used_closure_vars
           (Name_occurrences.closure_vars closure_elts_used_in_typing_env)
    in
    Some
      (Flambda_cmx_format.create ~final_typing_env ~all_code ~exported_offsets
         ~used_closure_vars)
