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

type t = Continuation_handler.t Continuation.Map.t

let [@ocamlformat "disable"] print_with_cache ~cache:_ _ppf _t =
  Misc.fatal_error "Continuation_handlers.print_with_cache not yet implemented"

let [@ocamlformat "disable"] print _ppf _t =
  Misc.fatal_error "Continuation_handlers.print not yet implemented"

let to_map t = t

let free_names t =
  Continuation.Map.fold
    (fun _k handler free_names ->
      Name_occurrences.union free_names
        (Continuation_handler.free_names handler))
    t Name_occurrences.empty

let apply_renaming t perm =
  Continuation.Map.fold
    (fun k handler result ->
      let k = Renaming.apply_continuation perm k in
      let handler = Continuation_handler.apply_renaming handler perm in
      Continuation.Map.add k handler result)
    t Continuation.Map.empty

let all_ids_for_export t =
  Continuation.Map.fold
    (fun k handler ids ->
      Ids_for_export.union ids
        (Ids_for_export.add_continuation
           (Continuation_handler.all_ids_for_export handler)
           k))
    t Ids_for_export.empty

let domain t = Continuation.Map.keys t

let contains_exn_handler t =
  Continuation.Map.exists
    (fun _cont handler -> Continuation_handler.is_exn_handler handler)
    t
