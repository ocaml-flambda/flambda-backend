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

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import
module DA = Downwards_acc
module DE = Downwards_env
module FT = Flambda2_types.Function_type
module VB = Bound_var

let make_inlined_body ~callee ~unroll_to ~params ~args ~my_closure ~my_depth
    ~rec_info ~body ~exn_continuation ~return_continuation
    ~apply_exn_continuation ~apply_return_continuation =
  let perm = Renaming.empty in
  let perm =
    match (apply_return_continuation : Apply.Result_continuation.t) with
    | Return k -> Renaming.add_continuation perm return_continuation k
    | Never_returns -> perm
  in
  let perm =
    Renaming.add_continuation perm exn_continuation apply_exn_continuation
  in
  let callee, rec_info =
    match unroll_to with
    | None -> callee, rec_info
    | Some unroll_depth ->
      let unrolled_rec_info = Rec_info_expr.unroll_to unroll_depth rec_info in
      let coercion_from_callee_to_unrolled_callee =
        Coercion.change_depth ~from:rec_info ~to_:(Rec_info_expr.var my_depth)
      in
      let callee =
        Simple.apply_coercion_exn callee coercion_from_callee_to_unrolled_callee
      in
      callee, unrolled_rec_info
  in
  let body =
    Let.create
      (Bound_pattern.singleton (VB.create my_closure Name_mode.normal))
      (Named.create_simple callee)
      ~body
        (* Here and below, we don't need to give any name occurrence information
           (thank goodness!) since the entirety of the expression we're building
           will be re-simplified. *)
      ~free_names_of_body:Unknown
    |> Expr.create_let
  in
  let body =
    let bound = Bound_pattern.singleton (VB.create my_depth Name_mode.normal) in
    Let.create bound
      (Named.create_rec_info rec_info)
      ~body ~free_names_of_body:Unknown
    |> Expr.create_let
  in
  Expr.apply_renaming
    (Expr.bind_parameters_to_args_no_simplification ~params ~args ~body)
    perm

let wrap_inlined_body_for_exn_support ~extra_args ~apply_exn_continuation
    ~apply_return_continuation ~result_arity ~make_inlined_body =
  (* We need to add a wrapper for the exception handler, so that exceptions
     coming from the inlined body go through the wrapper and are re-raised with
     the correct extra arguments.

     This means we also need to add a push trap before the inlined body, and a
     pop trap after.

     The push trap is simply a matter of jumping to the body, while the pop trap
     needs to replace the body's return continuation with a wrapper that pops
     then jumps back. *)
  (*
   * As a result, the application [Apply_expr f (args) <k> «k_exn»]
   * is replaced (before the actual inlining) by:
   *
   * [let_cont_exn k1 (exn: val) =
   *   Apply_cont k_exn exn extra_args
   * in
   * let_cont k_pop (args) = Apply_cont<pop k1> k args in
   * let_cont k_push () = Apply_expr f (args) <k_pop> «k1» in
   * Apply_cont<push k1> k_push ()]
   *)
  (* This feels quite heavy, but is necessary because we can rewrite neither the
     definition and other uses of k_exn nor the uses of the exception
     continuation in the body of f, so we need two distinct exception
     continuations; and of course the new exception continuation needs to be
     correctly pushed and popped.

     The most annoying part of this is that it introduces trywith blocks that
     were not part of the initial program, will not be removed, and might be
     useless (if the function never raises).

     Maybe a better solution would be to propagate through dacc a lazy
     rewriting, that would add the correct extra args to all uses of the
     exception continuation in the body. *)
  let wrapper = Continuation.create () in
  let body_with_pop =
    match (apply_return_continuation : Apply.Result_continuation.t) with
    | Never_returns ->
      make_inlined_body ~apply_exn_continuation:wrapper
        ~apply_return_continuation
    | Return apply_return_continuation ->
      let pop_wrapper_cont = Continuation.create () in
      let pop_wrapper_handler =
        let kinded_params =
          List.map (fun k -> Variable.create "wrapper_return", k) result_arity
        in
        let trap_action =
          Trap_action.Pop { exn_handler = wrapper; raise_kind = None }
        in
        let args = List.map (fun (v, _) -> Simple.var v) kinded_params in
        let handler =
          Apply_cont.create ~trap_action apply_return_continuation ~args
            ~dbg:Debuginfo.none
          |> Expr.create_apply_cont
        in
        Continuation_handler.create
          (Bound_parameter.List.create kinded_params)
          ~handler ~free_names_of_handler:Unknown ~is_exn_handler:false
      in
      let new_apply_return_continuation =
        Apply.Result_continuation.Return pop_wrapper_cont
      in
      let body =
        make_inlined_body ~apply_exn_continuation:wrapper
          ~apply_return_continuation:new_apply_return_continuation
      in
      Let_cont.create_non_recursive pop_wrapper_cont pop_wrapper_handler ~body
        ~free_names_of_body:Unknown
  in
  let wrapper_handler =
    let param = Variable.create "exn" in
    let kinded_params = [BP.create param K.With_subkind.any_value] in
    let exn_handler = Exn_continuation.exn_handler apply_exn_continuation in
    let trap_action = Trap_action.Pop { exn_handler; raise_kind = None } in
    let handler =
      (* Backtrace building functions expect compiler-generated raises not to
         have any debug info *)
      Apply_cont.create ~trap_action
        (Exn_continuation.exn_handler apply_exn_continuation)
        ~args:(Simple.var param :: List.map fst extra_args)
        ~dbg:Debuginfo.none
      |> Expr.create_apply_cont
    in
    Continuation_handler.create kinded_params ~handler
      ~free_names_of_handler:Unknown ~is_exn_handler:true
  in
  let body_with_push =
    (* Wrap the body between push and pop of the wrapper handler *)
    let push_wrapper_cont = Continuation.create () in
    let handler = body_with_pop in
    let push_wrapper_handler =
      Continuation_handler.create [] ~handler ~free_names_of_handler:Unknown
        ~is_exn_handler:false
    in
    let trap_action = Trap_action.Push { exn_handler = wrapper } in
    let body =
      Apply_cont.create ~trap_action push_wrapper_cont ~args:[]
        ~dbg:Debuginfo.none
      |> Expr.create_apply_cont
    in
    Let_cont.create_non_recursive push_wrapper_cont push_wrapper_handler ~body
      ~free_names_of_body:Unknown
  in
  Let_cont.create_non_recursive wrapper wrapper_handler ~body:body_with_push
    ~free_names_of_body:Unknown

let inline dacc ~apply ~unroll_to function_decl =
  let callee = Apply.callee apply in
  let args = Apply.args apply in
  let apply_return_continuation = Apply.continuation apply in
  let apply_exn_continuation = Apply.exn_continuation apply in
  (* CR mshinwell: Add meet constraint to the return continuation *)
  let denv = DA.denv dacc in
  let code =
    match DE.find_code_exn denv (FT.code_id function_decl) with
    | Code_present code -> code
    | Metadata_only code_metadata ->
      Misc.fatal_errorf "Cannot inline using only code metadata:@ %a"
        Code_metadata.print code_metadata
  in
  let rec_info =
    match T.prove_rec_info (DE.typing_env denv) (FT.rec_info function_decl) with
    | Proved rec_info -> rec_info
    | Unknown -> Rec_info_expr.unknown
    | Invalid -> Rec_info_expr.do_not_inline
  in
  let denv = DE.enter_inlined_apply ~called_code:code ~apply denv in
  let params_and_body = Code.params_and_body code in
  Function_params_and_body.pattern_match params_and_body
    ~f:(fun
         ~return_continuation
         ~exn_continuation
         params
         ~body
         ~my_closure
         ~is_my_closure_used:_
         ~my_depth
         ~free_names_of_body:_
       ->
      let make_inlined_body =
        make_inlined_body ~callee ~unroll_to ~params ~args ~my_closure ~my_depth
          ~rec_info ~body ~exn_continuation ~return_continuation
      in
      let expr =
        match Exn_continuation.extra_args apply_exn_continuation with
        | [] ->
          make_inlined_body
            ~apply_exn_continuation:
              (Exn_continuation.exn_handler apply_exn_continuation)
            ~apply_return_continuation
        | extra_args ->
          wrap_inlined_body_for_exn_support ~extra_args ~apply_exn_continuation
            ~apply_return_continuation ~result_arity:(Code.result_arity code)
            ~make_inlined_body
      in
      (* Format.eprintf "Inlined body to be simplified:@ %a\n%!" Expr.print
         expr; *)
      (* Format.eprintf "Inlined body to be simplified:@ %a@ dacc:@ %a\n%!"
         Expr.print expr DA.print dacc; *)
      DA.with_denv dacc denv, expr)
