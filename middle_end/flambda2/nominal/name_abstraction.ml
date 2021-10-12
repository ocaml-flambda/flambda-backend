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
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type Term = sig
  include Contains_names.S

  include Contains_ids.S with type t := t

  val print : Format.formatter -> t -> unit
end

module Make (Bindable : Bindable.S) (Term : Term) = struct
  type t = Bindable.t * Term.t

  let create name term = name, term

  let[@inline always] pattern_match (name, term) ~f =
    let fresh_name = Bindable.rename name in
    let perm = Bindable.name_permutation name ~guaranteed_fresh:fresh_name in
    let fresh_term = Term.apply_renaming term perm in
    f fresh_name fresh_term

  let [@ocamlformat "disable"] print ppf t =
    pattern_match t ~f:(fun name term ->
      Format.fprintf ppf "@[<hov 1>%s@<1>%s%s%a%s@<1>%s%s@ %a@]"
        (Flambda_colours.name_abstraction ())
        "["
        (Flambda_colours.normal ())
        Bindable.print name
        (Flambda_colours.name_abstraction ())
        "]"
        (Flambda_colours.normal ())
        Term.print term)

  let[@inline always] pattern_match_mapi t ~f =
    pattern_match t ~f:(fun fresh_name fresh_term ->
        let new_term = f fresh_name fresh_term in
        fresh_name, new_term)

  let[@inline always] pattern_match_map t ~f =
    pattern_match_mapi t ~f:(fun _fresh_name fresh_term -> f fresh_term)

  let[@inline always] pattern_match_pair (name0, term0) (name1, term1) ~f =
    let fresh_name = Bindable.rename name0 in
    let perm0 = Bindable.name_permutation name0 ~guaranteed_fresh:fresh_name in
    let perm1 = Bindable.name_permutation name1 ~guaranteed_fresh:fresh_name in
    let fresh_term0 = Term.apply_renaming term0 perm0 in
    let fresh_term1 = Term.apply_renaming term1 perm1 in
    f fresh_name fresh_term0 fresh_term1

  let apply_renaming ((name, term) as t) perm =
    if Renaming.is_empty perm
    then t
    else
      let name = Bindable.apply_renaming name perm in
      let term = Term.apply_renaming term perm in
      name, term

  let free_names (name, term) =
    let in_binding_position = Bindable.free_names name in
    let free_in_term = Term.free_names term in
    Name_occurrences.diff free_in_term in_binding_position

  let all_ids_for_export (name, term) =
    Ids_for_export.union
      (Bindable.all_ids_for_export name)
      (Term.all_ids_for_export term)
end
[@@inline always]
