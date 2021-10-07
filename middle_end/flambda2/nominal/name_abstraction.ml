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

type ('bindable, 'term) t = 'bindable * 'term

module Make (Bindable : Bindable.S) (Term : Term) = struct
  type nonrec t = (Bindable.t, Term.t) t

  let create bindable term = bindable, term

  let[@inline always] pattern_match (bindable, term) ~f =
    let fresh_bindable = Bindable.rename bindable in
    let perm =
      Bindable.name_permutation bindable ~guaranteed_fresh:fresh_bindable
    in
    let fresh_term = Term.apply_renaming term perm in
    f fresh_bindable fresh_term

  (* CR-someday: Use modular implicits to allow let<> to be used throughout the
     codebase. *)
  let[@inline always] ( let<> ) t f =
    pattern_match t ~f:(fun bindable term -> f (bindable, term))

  let print ppf ((unfreshened_bindable, unfreshened_term) as t) =
    let<> freshened_bindable, freshened_term = t in
    let bindable, term =
      if Flambda_features.freshen_when_printing ()
      then freshened_bindable, freshened_term
      else unfreshened_bindable, unfreshened_term
    in
    Format.fprintf ppf "@[<hov 1>%s@<1>%s%s%a%s@<1>%s%s@ %a@]"
      (Flambda_colours.name_abstraction ())
      "["
      (Flambda_colours.normal ())
      Bindable.print bindable
      (Flambda_colours.name_abstraction ())
      "]"
      (Flambda_colours.normal ())
      Term.print term

  let[@inline always] pattern_match_pair (bindable0, term0) (bindable1, term1)
      ~f =
    let fresh_bindable = Bindable.rename bindable0 in
    let perm0 =
      Bindable.name_permutation bindable0 ~guaranteed_fresh:fresh_bindable
    in
    let perm1 =
      Bindable.name_permutation bindable1 ~guaranteed_fresh:fresh_bindable
    in
    let fresh_term0 = Term.apply_renaming term0 perm0 in
    let fresh_term1 = Term.apply_renaming term1 perm1 in
    f fresh_bindable fresh_term0 fresh_term1

  let apply_renaming ((bindable, term) as t) perm =
    if Renaming.is_empty perm
    then t
    else
      let bindable' = Bindable.apply_renaming bindable perm in
      let term' = Term.apply_renaming term perm in
      if bindable == bindable' && term == term' then t else bindable', term'

  let free_names (bindable, term) =
    let in_binding_position = Bindable.free_names bindable in
    let free_in_term = Term.free_names term in
    Name_occurrences.diff free_in_term in_binding_position

  let all_ids_for_export (bindable, term) =
    Ids_for_export.union
      (Bindable.all_ids_for_export bindable)
      (Term.all_ids_for_export term)
end
[@@inline always]
