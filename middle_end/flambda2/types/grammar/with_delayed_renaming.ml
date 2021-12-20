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
    mutable delayed_permutation : Renaming.t;
    mutable free_names : Name_occurrences.t option
  }

let create descr =
  { descr; delayed_permutation = Renaming.empty; free_names = None }

let peek_descr t = t.descr

let[@inline always] descr ~apply_renaming_descr ~free_names_descr t =
  if Renaming.is_empty t.delayed_permutation
  then t.descr
  else
    let descr = apply_renaming_descr t.descr t.delayed_permutation in
    t.descr <- descr;
    let free_names =
      match t.free_names with
      | None -> free_names_descr descr
      | Some free_names ->
        Name_occurrences.apply_renaming free_names t.delayed_permutation
    in
    t.delayed_permutation <- Renaming.empty;
    t.free_names <- Some free_names;
    descr

let apply_renaming t perm =
  let delayed_permutation =
    Renaming.compose ~second:perm ~first:t.delayed_permutation
  in
  { t with delayed_permutation }

let[@inline always] free_names ~apply_renaming_descr ~free_names_descr t =
  let descr = descr ~apply_renaming_descr ~free_names_descr t in
  match t.free_names with
  | Some free_names -> free_names
  | None ->
    let free_names = free_names_descr descr in
    t.free_names <- Some free_names;
    free_names

let remove_unused_closure_vars ~apply_renaming_descr ~free_names_descr
    ~remove_unused_closure_vars_descr t ~used_closure_vars =
  let descr_known_to_contain_no_closure_vars =
    (* If the free names are already computed (modulo application of a
       renaming), we can use them as a shortcut, to potentially avoid traversing
       the [descr]. *)
    if Option.is_some t.free_names
    then
      let free_names = free_names t ~apply_renaming_descr ~free_names_descr in
      let closure_vars = Name_occurrences.closure_vars free_names in
      Var_within_closure.Set.is_empty closure_vars
    else false
  in
  if descr_known_to_contain_no_closure_vars
  then t
  else
    let descr =
      remove_unused_closure_vars_descr
        (descr ~apply_renaming_descr ~free_names_descr t)
        ~used_closure_vars
    in
    t.descr <- descr;
    assert (Renaming.is_empty t.delayed_permutation);
    t.free_names <- None;
    t

let print ~print_descr ~apply_renaming_descr ~free_names_descr ppf t =
  print_descr ppf (descr ~apply_renaming_descr ~free_names_descr t)
