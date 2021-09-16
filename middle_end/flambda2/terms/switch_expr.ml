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

type t =
  { scrutinee : Simple.t;
    arms : Apply_cont_expr.t Targetint_31_63.Map.t
  }

let fprintf = Format.fprintf

let print_arms ppf arms =
  let arms =
    Targetint_31_63.Map.fold
      (fun discr action arms_inverse ->
        match Apply_cont_expr.Map.find action arms_inverse with
        | exception Not_found ->
          Apply_cont_expr.Map.add action
            (Targetint_31_63.Set.singleton discr)
            arms_inverse
        | discrs ->
          Apply_cont_expr.Map.add action
            (Targetint_31_63.Set.add discr discrs)
            arms_inverse)
      arms Apply_cont_expr.Map.empty
  in
  let spc = ref false in
  let arms =
    List.sort
      (fun (action1, discrs1) (action2, discrs2) ->
        let min1 = Targetint_31_63.Set.min_elt_opt discrs1 in
        let min2 = Targetint_31_63.Set.min_elt_opt discrs2 in
        match min1, min2 with
        | None, None -> Apply_cont_expr.compare action1 action2
        | None, Some _ -> -1
        | Some _, None -> 1
        | Some min1, Some min2 -> Targetint_31_63.compare min1 min2)
      (Apply_cont_expr.Map.bindings arms)
  in
  List.iter
    (fun (action, discrs) ->
      if !spc then fprintf ppf "@ " else spc := true;
      let discrs = Targetint_31_63.Set.elements discrs in
      fprintf ppf "@[<hov 2>@[<hov 0>| %a @<0>%s\u{21a6}@<0>%s@ @]%a@]"
        (Format.pp_print_list
           ~pp_sep:(fun ppf () -> Format.fprintf ppf "@ | ")
           Targetint_31_63.print)
        discrs (Flambda_colours.elide ())
        (Flambda_colours.normal ())
        Apply_cont_expr.print action)
    arms

let [@ocamlformat "disable"] print ppf { scrutinee; arms; } =
  fprintf ppf
    "@[<v 0>(@<0>%sswitch@<0>%s %a@ @[<v 0>%a@])@]"
    (Flambda_colours.expr_keyword ())
    (Flambda_colours.normal ())
    Simple.print scrutinee
    print_arms arms

let [@ocamlformat "disable"] print_with_cache ~cache:_ ppf t = print ppf t

let create ~scrutinee ~arms = { scrutinee; arms }

let if_then_else ~scrutinee ~if_true ~if_false =
  let arms =
    Targetint_31_63.Map.of_list
      [Targetint_31_63.bool_true, if_true; Targetint_31_63.bool_false, if_false]
  in
  create ~scrutinee ~arms

let iter t ~f = Targetint_31_63.Map.iter f t.arms

let num_arms t = Targetint_31_63.Map.cardinal t.arms

let scrutinee t = t.scrutinee

let arms t = t.arms

let free_names { scrutinee; arms } =
  let free_names_in_arms =
    Targetint_31_63.Map.fold
      (fun _discr action free_names ->
        Name_occurrences.union (Apply_cont_expr.free_names action) free_names)
      arms Name_occurrences.empty
  in
  Name_occurrences.union (Simple.free_names scrutinee) free_names_in_arms

let apply_renaming ({ scrutinee; arms } as t) perm =
  let scrutinee' = Simple.apply_renaming scrutinee perm in
  let arms' =
    Targetint_31_63.Map.map_sharing
      (fun action -> Apply_cont_expr.apply_renaming action perm)
      arms
  in
  if scrutinee == scrutinee' && arms == arms'
  then t
  else { scrutinee = scrutinee'; arms = arms' }

let all_ids_for_export { scrutinee; arms } =
  let scrutinee_ids = Ids_for_export.from_simple scrutinee in
  Targetint_31_63.Map.fold
    (fun _discr action ids ->
      Ids_for_export.union ids (Apply_cont_expr.all_ids_for_export action))
    arms scrutinee_ids
