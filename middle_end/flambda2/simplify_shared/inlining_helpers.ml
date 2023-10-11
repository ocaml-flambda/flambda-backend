(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
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

open! Flambda.Import
module RC = Apply.Result_continuation

let make_inlined_body ~callee ~region_inlined_into ~params ~args ~my_closure
    ~my_region ~my_depth ~rec_info ~body ~exn_continuation ~return_continuation
    ~apply_exn_continuation ~apply_return_continuation ~bind_params ~bind_depth
    ~apply_renaming =
  let renaming = Renaming.empty in
  let renaming =
    match (apply_return_continuation : RC.t) with
    | Return k -> Renaming.add_continuation renaming return_continuation k
    | Never_returns -> renaming
  in
  let renaming =
    Renaming.add_continuation renaming exn_continuation apply_exn_continuation
  in
  let renaming =
    (* Unlike for parameters, we know that the argument for the [my_region]
       parameter is fresh for [body], so we can use a permutation without fear
       of swapping out existing occurrences of such argument within [body]. *)
    Renaming.add_variable renaming my_region region_inlined_into
  in
  let body =
    match callee with
    | Some callee ->
      bind_params ~params:(my_closure :: params) ~args:(callee :: args) ~body
    | None -> bind_params ~params ~args ~body
  in
  let body = bind_depth ~my_depth ~rec_info ~body in
  apply_renaming body renaming

let wrap_inlined_body_for_exn_extra_args acc ~extra_args ~apply_exn_continuation
    ~apply_return_continuation ~result_arity ~make_inlined_body
    ~apply_cont_create ~let_cont_create =
  (* We need to add a wrapper for the exception handler so that exceptions
   * coming from the inlined body are raised with the correct extra arguments:
   *
   * let_cont_exn k1 (exn: val) = Apply_cont k_exn exn extra_args in
   * let_cont k_pop (args) = Apply_cont<pop k1> k args in
   * let_cont k_push () =
   *   (* inlined body here, was: Apply_expr f (args) <k_pop> «k1» *)
   * in
   * Apply_cont<push k1> k_push ()
   *)
  (* CR mshinwell: Maybe we could extend [Apply_cont_rewrite] to be able to do
     this rewriting during the normal traversal of the inlined body. *)
  let wrapper = Continuation.create () in
  let body_with_pop acc =
    match (apply_return_continuation : RC.t) with
    | Never_returns ->
      make_inlined_body acc ~apply_exn_continuation:wrapper
        ~apply_return_continuation
    | Return apply_return_continuation ->
      let pop_wrapper_cont = Continuation.create () in
      let body acc =
        make_inlined_body acc ~apply_exn_continuation:wrapper
          ~apply_return_continuation:(RC.Return pop_wrapper_cont)
      in
      let kinded_params =
        List.map
          (fun k -> Bound_parameter.create (Variable.create "wrapper_return") k)
          (Flambda_arity.unarized_components result_arity)
      in
      let trap_action =
        Trap_action.Pop { exn_handler = wrapper; raise_kind = None }
      in
      let args = List.map Bound_parameter.simple kinded_params in
      let handler acc =
        apply_cont_create acc ~trap_action apply_return_continuation ~args
          ~dbg:Debuginfo.none
      in
      let_cont_create acc pop_wrapper_cont
        ~handler_params:(Bound_parameters.create kinded_params)
        ~handler ~body ~is_exn_handler:false ~is_cold:false
  in
  let param = Variable.create "exn" in
  let wrapper_handler_params =
    [Bound_parameter.create param Flambda_kind.With_subkind.any_value]
    |> Bound_parameters.create
  in
  let exn_handler = Exn_continuation.exn_handler apply_exn_continuation in
  let trap_action = Trap_action.Pop { exn_handler; raise_kind = None } in
  let wrapper_handler acc =
    (* Backtrace building functions expect compiler-generated raises not to have
       any debug info *)
    apply_cont_create acc ~trap_action
      (Exn_continuation.exn_handler apply_exn_continuation)
      ~args:(Simple.var param :: List.map fst extra_args)
      ~dbg:Debuginfo.none
  in
  let body_with_push acc =
    (* Wrap the body between push and pop of the wrapper handler *)
    let push_wrapper_cont = Continuation.create () in
    let push_wrapper_handler = body_with_pop in
    let trap_action = Trap_action.Push { exn_handler = wrapper } in
    let body acc =
      apply_cont_create acc ~trap_action push_wrapper_cont ~args:[]
        ~dbg:Debuginfo.none
    in
    let_cont_create acc push_wrapper_cont ~handler_params:Bound_parameters.empty
      ~handler:push_wrapper_handler ~body ~is_exn_handler:false ~is_cold:false
  in
  let_cont_create acc wrapper ~handler_params:wrapper_handler_params
    ~handler:wrapper_handler ~body:body_with_push ~is_exn_handler:true
    ~is_cold:false

type attribute_kind =
  | Inlined
  | Unrolled

let string_of_kind = function
  | Inlined -> "[@inlined]"
  | Unrolled -> "[@unrolled]]"

let inlined_attribute_on_partial_application_msg kind =
  string_of_kind kind ^ " attributes may not be used on partial applications"
