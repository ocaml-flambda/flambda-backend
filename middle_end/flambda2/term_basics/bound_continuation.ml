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

include Continuation

let free_names t = Name_occurrences.singleton_continuation t

let apply_renaming t renaming = Renaming.apply_continuation renaming t

let ids_for_export t = Ids_for_export.singleton_continuation t

let renaming t ~guaranteed_fresh =
  Renaming.add_fresh_continuation Renaming.empty t ~guaranteed_fresh
