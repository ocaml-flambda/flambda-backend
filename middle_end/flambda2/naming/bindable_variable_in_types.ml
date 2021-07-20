(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 OCamlPro SAS                                          *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Variable

let print_with_cache ~cache:_ ppf t = print ppf t

let free_names t =
  Name_occurrences.singleton_variable t Name_mode.in_types

let apply_renaming t perm = Renaming.apply_variable perm t

let all_ids_for_export t =
  Ids_for_export.add_variable Ids_for_export.empty t

let rename t = rename t

let add_to_name_permutation t ~guaranteed_fresh perm =
  Renaming.add_fresh_variable perm t ~guaranteed_fresh

let name_permutation t ~guaranteed_fresh =
  add_to_name_permutation t ~guaranteed_fresh Renaming.empty

(* CR mshinwell: These shouldn't have "in_terms" on their names *)
let singleton_occurrence_in_terms t =
  Name_occurrences.singleton_variable t Name_mode.in_types

let add_occurrence_in_terms t occs =
  Name_occurrences.add_variable occs t Name_mode.in_types
