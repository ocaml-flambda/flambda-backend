(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2022 OCamlPro SAS                                    *)
(*   Copyright 2014--2022 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Simplify_import

(* This module deals with a sub-problem of the problem of simplifying multiple
   possibly-recursive sets of closures, namely determining typing and contextual
   information that is the same no matter which set of closures in a given
   recursive group is being simplified. *)

type t

val create :
  dacc_prior_to_sets:DA.t ->
  simplify_function_body:Simplify_common.simplify_function_body ->
  all_sets_of_closures:Set_of_closures.t list ->
  closure_bound_names_all_sets:Bound_name.t Function_slot.Map.t list ->
  value_slot_types_all_sets:T.t Value_slot.Map.t list ->
  t

val create_for_stub :
  DA.t ->
  all_code:Code.t Code_id.Map.t ->
  simplify_function_body:Simplify_common.simplify_function_body ->
  t

val dacc_inside_functions : t -> DA.t

val dacc_prior_to_sets : t -> DA.t

(* This map only contains entries for functions where we definitely have the
   code (not just the metadata). *)
val old_to_new_code_ids_all_sets : t -> Code_id.t Code_id.Map.t

val closure_bound_names_inside_functions_all_sets :
  t -> Bound_name.t Function_slot.Map.t list

val closure_bound_names_inside_functions_exactly_one_set :
  t -> Bound_name.t Function_slot.Map.t

val simplify_function_body : t -> Simplify_common.simplify_function_body

val previously_free_depth_variables : t -> Variable.Set.t

val function_decl_type :
  ?new_code_id:Code_id.t ->
  rec_info:T.t ->
  Code_id.t ->
  Function_type.t Or_unknown_or_bottom.t
