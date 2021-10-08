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

module BP = Kinded_parameter
include BP.List

let create params =
  let params_set = BP.Set.of_list params in
  if List.length params <> BP.Set.cardinal params_set
  then
    Misc.fatal_errorf
      "Names provided to [Bound_parameters.create] must be disjoint:@ %a"
      BP.List.print params;
  params

let to_list t = t

let name_permutation t1 ~guaranteed_fresh:t2 =
  try
    List.fold_left2
      (fun renaming param1 param2 ->
        Renaming.add_variable renaming (BP.var param1) (BP.var param2))
      Renaming.empty t1 t2
  with Invalid_argument _ ->
    assert (List.compare_lengths t1 t2 <> 0);
    Misc.fatal_errorf "Parameter lists are of differing lengths:@ %a@ and@ %a"
      print t1 print t2

let add_to_name_permutation t1 ~guaranteed_fresh renaming =
  Renaming.compose
    ~second:(name_permutation t1 ~guaranteed_fresh)
    ~first:renaming

let singleton_occurrence_in_terms = free_names

let add_occurrence_in_terms t occs = Name_occurrences.union (free_names t) occs
