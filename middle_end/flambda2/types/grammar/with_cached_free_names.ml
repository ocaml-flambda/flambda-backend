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
  { mutable descr : 'descr;
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

let apply_renaming ~apply_renaming_descr ~free_names_descr t perm =
  let free_names = free_names ~free_names_descr t in
  if (not (Renaming.has_import_map perm))
     && not (Name_occurrences.affected_by_renaming free_names perm)
  then t
  else
    let descr = apply_renaming_descr t.descr perm in
    let free_names =
      (* CR lmaurer: Make extra-sure that [Name_occurrences.apply_renaming]
         returns a [phys_equal] result if no change, then consider moving this
         call in place of [affected_by_renaming] above to avoid traversing
         twice. *)
      Some (Name_occurrences.apply_renaming free_names perm)
    in
    { descr; free_names }

let remove_unused_closure_vars ~free_names_descr
    ~remove_unused_closure_vars_descr t ~used_closure_vars =
  let descr_known_to_contain_no_unused_closure_vars =
    (* If the free names are already computed (modulo application of a
       renaming), we can use them as a shortcut, to potentially avoid traversing
       the [descr]. *)
    if Option.is_some t.free_names
    then
      let free_names = free_names t ~free_names_descr in
      let closure_vars = Name_occurrences.closure_vars free_names in
      let unused_closure_vars =
        Var_within_closure.Set.diff closure_vars used_closure_vars
      in
      Var_within_closure.Set.is_empty unused_closure_vars
    else false
  in
  if descr_known_to_contain_no_unused_closure_vars
  then t
  else
    let descr = remove_unused_closure_vars_descr (descr t) ~used_closure_vars in
    t.descr <- descr;
    t.free_names <- None;
    t

let print ~print_descr ppf t = print_descr ppf (descr t)
