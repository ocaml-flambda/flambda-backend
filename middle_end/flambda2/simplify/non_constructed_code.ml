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

type t = unit Code0.t

let code_id = Code0.code_id

let is_non_callable = Code0.is_non_callable

let newer_version_of = Code0.newer_version_of

let params_arity = Code0.params_arity

let result_arity = Code0.result_arity

let stub = Code0.stub

let inline = Code0.inline

let is_a_functor = Code0.is_a_functor

let recursive = Code0.recursive

let cost_metrics = Code0.cost_metrics

let inlining_arguments = Code0.inlining_arguments

let dbg = Code0.dbg

let is_tupled = Code0.is_tupled

let inlining_decision = Code0.inlining_decision

let create code_id
    ~(free_names_of_params_and_body : _ Code.Params_and_body_state.t)
    ~newer_version_of ~params_arity ~result_arity ~stub ~inline ~is_a_functor
    ~recursive ~cost_metrics ~inlining_arguments ~dbg ~is_tupled
    ~inlining_decision =
  let params_and_body =
    match free_names_of_params_and_body with
    | Cannot_be_called -> Code.Params_and_body_state.cannot_be_called
    | Non_inlinable { is_my_closure_used } ->
      Code.Params_and_body_state.non_inlinable ~is_my_closure_used
    | Inlinable free_names_of_params_and_body ->
      Code.Params_and_body_state.inlinable ((), free_names_of_params_and_body)
  in
  Code0.create ~print_function_params_and_body:Unit.print code_id
    ~params_and_body ~newer_version_of ~params_arity ~result_arity ~stub ~inline
    ~is_a_functor ~recursive ~cost_metrics ~inlining_arguments ~dbg ~is_tupled
    ~inlining_decision

let make_not_callable = Code0.make_not_callable

let print = Code0.print ~print_function_params_and_body:Unit.print

let free_names = Code0.free_names

let apply_renaming =
  Code0.apply_renaming ~apply_renaming_function_params_and_body:(fun () _ -> ())
