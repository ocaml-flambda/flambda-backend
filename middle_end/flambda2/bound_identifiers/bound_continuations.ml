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

type t = Continuation.t list

let print ppf t =
  Format.pp_print_list ~pp_sep:Format.pp_print_space Continuation.print ppf t

let create conts =
  match conts with
  | [] ->
    Misc.fatal_error "No continuations provided to [Bound_continuations.create]"
  | _ :: _ ->
    let conts_set = Continuation.Set.of_list conts in
    if List.length conts <> Continuation.Set.cardinal conts_set
    then
      Misc.fatal_errorf
        "Continuations provided to [Bound_continuations.create] must be \
         disjoint:@ %a"
        print conts;
    conts

let free_names t =
  List.fold_left
    (fun free_names cont ->
      (* [has_traps] is set to [true] to ensure that in [Name_abstraction] both
         trap and non-trap continuation occurrences are removed from the free
         names of the body. *)
      Name_occurrences.add_continuation free_names cont ~has_traps:true)
    Name_occurrences.empty t

let apply_renaming t renaming =
  List.map (Renaming.apply_continuation renaming) t

let ids_for_export t =
  List.fold_left Ids_for_export.add_continuation Ids_for_export.empty t

let rename t = List.map Continuation.rename t

let is_renamed_version_of t t' =
  Misc.Stdlib.List.equal Continuation.is_renamed_version_of t t'

let renaming t1 ~guaranteed_fresh:t2 =
  try List.fold_left2 Renaming.add_continuation Renaming.empty t1 t2
  with Invalid_argument _ ->
    assert (List.compare_lengths t1 t2 <> 0);
    Misc.fatal_errorf
      "Continuation lists are of differing lengths:@ %a@ and@ %a" print t1 print
      t2
