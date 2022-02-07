(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type 'descr t =
  { descr : 'descr;
    mutable free_names : Name_occurrences.t option
  }

let create descr = { descr; free_names = None }

let[@inline always] descr t = t.descr

let[@inline always] free_names ~free_names_descr t =
  let descr = descr t in
  match t.free_names with
  | Some free_names -> free_names
  | None ->
    let free_names = free_names_descr descr in
    t.free_names <- Some free_names;
    free_names

let[@inline always] free_names_no_cache ~free_names_descr t =
  let descr = descr t in
  free_names_descr descr

let apply_renaming ~apply_renaming_descr ~free_names_descr t renaming =
  let free_names = free_names ~free_names_descr t in
  if (not (Renaming.has_import_map renaming))
     && not (Name_occurrences.affected_by_renaming free_names renaming)
  then t
  else
    let descr = apply_renaming_descr t.descr renaming in
    let free_names =
      (* CR lmaurer: Make extra-sure that [Name_occurrences.apply_renaming]
         returns a [phys_equal] result if no change, then consider moving this
         call in place of [affected_by_renaming] above to avoid traversing
         twice. *)
      Some (Name_occurrences.apply_renaming free_names renaming)
    in
    { descr; free_names }

let remove_unused_closure_vars_and_shortcut_aliases
    ~remove_unused_closure_vars_and_shortcut_aliases_descr t ~used_closure_vars
    ~canonicalise =
  let descr =
    remove_unused_closure_vars_and_shortcut_aliases_descr (descr t)
      ~used_closure_vars ~canonicalise
  in
  if descr == t.descr then t else { descr; free_names = None }

let project_variables_out ~free_names_descr ~to_project ~project_descr t =
  let free_names = free_names t ~free_names_descr in
  let has_variable_to_project =
    Variable.Set.fold
      (fun var has_variable_to_project ->
        has_variable_to_project || Name_occurrences.mem_var free_names var)
      to_project false
  in
  if has_variable_to_project
  then
    let descr' = project_descr t.descr in
    if descr' == t.descr then t else create descr'
  else t

let print ~print_descr ppf t = print_descr ppf (descr t)
