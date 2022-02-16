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

type t = Flambda.Function_params_and_body.t Code0.t

let code_metadata = Code0.code_metadata

let code_id = Code0.code_id

let params_and_body = Code0.params_and_body

let newer_version_of = Code0.newer_version_of

let params_arity = Code0.params_arity

let num_leading_heap_params = Code0.num_leading_heap_params

let num_trailing_local_params = Code0.num_trailing_local_params

let result_arity = Code0.result_arity

let result_types = Code0.result_types

let stub = Code0.stub

let inline = Code0.inline

let is_a_functor = Code0.is_a_functor

let recursive = Code0.recursive

let cost_metrics = Code0.cost_metrics

let inlining_arguments = Code0.inlining_arguments

let dbg = Code0.dbg

let is_tupled = Code0.is_tupled

let is_my_closure_used = Code0.is_my_closure_used

let inlining_decision = Code0.inlining_decision

let contains_no_escaping_local_allocs = Code0.contains_no_escaping_local_allocs

let absolute_history = Code0.absolute_history

let relative_history = Code0.relative_history

let create code_id ~params_and_body ~free_names_of_params_and_body
    ~newer_version_of ~params_arity ~num_trailing_local_params ~result_arity
    ~result_types ~contains_no_escaping_local_allocs ~stub ~inline ~is_a_functor
    ~recursive ~cost_metrics ~inlining_arguments ~dbg ~is_tupled
    ~is_my_closure_used ~inlining_decision ~absolute_history ~relative_history =
  Code0.create
    ~print_function_params_and_body:Flambda.Function_params_and_body.print
    code_id ~params_and_body ~free_names_of_params_and_body ~newer_version_of
    ~params_arity ~num_trailing_local_params ~result_arity ~result_types
    ~contains_no_escaping_local_allocs ~stub ~inline ~is_a_functor ~recursive
    ~cost_metrics ~inlining_arguments ~dbg ~is_tupled ~is_my_closure_used
    ~inlining_decision ~absolute_history ~relative_history

let with_code_id = Code0.with_code_id

let with_params_and_body =
  Code0.with_params_and_body
    ~print_function_params_and_body:Flambda.Function_params_and_body.print

let with_newer_version_of = Code0.with_newer_version_of

let free_names = Code0.free_names

let apply_renaming =
  Code0.apply_renaming
    ~apply_renaming_function_params_and_body:
      Flambda.Function_params_and_body.apply_renaming

let print =
  Code0.print
    ~print_function_params_and_body:Flambda.Function_params_and_body.print

let all_ids_for_export =
  Code0.all_ids_for_export
    ~all_ids_for_export_function_params_and_body:
      Flambda.Function_params_and_body.all_ids_for_export

let map_result_types = Code0.map_result_types
