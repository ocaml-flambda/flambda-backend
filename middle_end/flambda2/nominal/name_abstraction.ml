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
  type t

  val apply_renaming : t -> Renaming.t -> t

  include Contains_ids.S with type t := t
end

type ('bindable, 'term) t = 'bindable * 'term

module Make_matching_and_renaming0
    (Bindable : Bindable.S) (Term : sig
      type t

      val apply_renaming : t -> Renaming.t -> t
    end) =
struct
  let[@inline always] pattern_match (bindable, term) ~f =
    let fresh_bindable = Bindable.rename bindable in
    let perm =
      Bindable.name_permutation bindable ~guaranteed_fresh:fresh_bindable
    in
    let fresh_term = Term.apply_renaming term perm in
    f fresh_bindable fresh_term

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

  let[@inline always] ( let<> ) t f =
    pattern_match t ~f:(fun bindable term -> f (bindable, term))
end
[@@inline always]

module Make_ids_for_export0 (Bindable : Bindable.S) (Term : Contains_ids.S) =
struct
  let all_ids_for_export (bindable, term) =
    Ids_for_export.union
      (Bindable.all_ids_for_export bindable)
      (Term.all_ids_for_export term)
end
[@@inline always]

module Make (Bindable : Bindable.S) (Term : Term) = struct
  type nonrec t = (Bindable.t, Term.t) t

  let create bindable term = bindable, term

  include Make_matching_and_renaming0 (Bindable) (Term)
  include Make_ids_for_export0 (Bindable) (Term)
end
[@@inline always]

module Make_matching_and_renaming
    (Bindable : Bindable.S) (Term : sig
      type t

      val apply_renaming : t -> Renaming.t -> t
    end) =
struct
  type nonrec t = (Bindable.t, Term.t) t

  include Make_matching_and_renaming0 (Bindable) (Term)
end
[@@inline always]

module Make_ids_for_export (Bindable : Bindable.S) (Term : Contains_ids.S) =
struct
  type nonrec t = (Bindable.t, Term.t) t

  include Make_ids_for_export0 (Bindable) (Term)
end
[@@inline always]

let peek_for_printing t =
  assert (not (Flambda_features.freshen_when_printing ()));
  t
