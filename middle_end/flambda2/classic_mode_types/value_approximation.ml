(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Pierre Chambart and Vincent Laviron, OCamlPro               *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Approximations used for cross-module inlining in Closure_conversion *)

type 'code t =
  | Value_unknown
  | Closure_approximation of Code_id.t * Closure_id.t * 'code
  | Block_approximation of 'code t array * Alloc_mode.t

let is_unknown = function
  | Value_unknown -> true
  | Closure_approximation _ | Block_approximation _ -> false

let rec free_names ~code_free_names approx =
  match approx with
  | Value_unknown -> Name_occurrences.empty
  | Block_approximation (approxs, _) ->
    Array.fold_left
      (fun names approx ->
        Name_occurrences.union names (free_names ~code_free_names approx))
      Name_occurrences.empty approxs
  | Closure_approximation (code_id, closure_id, code) ->
    Name_occurrences.add_code_id
      (Name_occurrences.add_closure_id (code_free_names code) closure_id
         Name_mode.normal)
      code_id Name_mode.normal
