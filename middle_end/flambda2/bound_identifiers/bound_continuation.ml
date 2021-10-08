(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2019 OCamlPro SAS                                    *)
(*   Copyright 2018--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

include Continuation

let free_names t = Name_occurrences.singleton_continuation t

let apply_renaming t perm = Renaming.apply_continuation perm t

let all_ids_for_export t =
  Ids_for_export.add_continuation Ids_for_export.empty t

let add_to_name_permutation t ~guaranteed_fresh perm =
  Renaming.add_fresh_continuation perm t ~guaranteed_fresh

let name_permutation t ~guaranteed_fresh =
  add_to_name_permutation t ~guaranteed_fresh Renaming.empty
