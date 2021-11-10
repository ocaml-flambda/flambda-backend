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
  let my_closure =
    Bound_parameter.create my_closure Flambda_kind.With_subkind.any_value
  in
  let bind_params = Expr.bind_parameters_to_args_no_simplification in
  let bind_depth ~my_depth ~rec_info ~body =
    let bound = Bound_pattern.singleton (VB.create my_depth Name_mode.normal) in
    Let.create bound
      (Named.create_rec_info rec_info)
      ~body ~free_names_of_body:Unknown
    |> Expr.create_let
  in
  let apply_renaming = Expr.apply_renaming in
  Inlining_helpers.make_inlined_body ~callee ~params ~args ~my_closure ~my_depth
    ~rec_info ~body ~exn_continuation ~return_continuation
    ~apply_exn_continuation ~apply_return_continuation ~bind_params ~bind_depth
    ~apply_renaming

let wrap_inlined_body_for_exn_support ~extra_args ~apply_exn_continuation
    ~apply_return_continuation ~result_arity ~make_inlined_body =
  let apply_cont_create () ~trap_action cont ~args ~dbg =
    Apply_cont.create ~trap_action cont ~args ~dbg |> Expr.create_apply_cont
  in
  let let_cont_create () cont ~handler_params ~handler ~body
      ~is_exn_handler =
    let handler =
      Continuation_handler.create handler_params ~handler:(handler ())
        ~free_names_of_handler:Unknown ~is_exn_handler
    in
    Let_cont.create_non_recursive cont handler ~body:(body ())
      ~free_names_of_body:Unknown
  in
  Inlining_helpers.wrap_inlined_body_for_exn_support () ~extra_args
    ~apply_exn_continuation ~apply_return_continuation ~result_arity
    ~make_inlined_body ~apply_cont_create ~let_cont_create

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
      let make_inlined_body () =
        make_inlined_body ~callee ~unroll_to ~params ~args ~my_closure ~my_depth
          ~rec_info ~body ~exn_continuation ~return_continuation
      in
      let expr =
        match Exn_continuation.extra_args apply_exn_continuation with
        | [] ->
          make_inlined_body ()
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
