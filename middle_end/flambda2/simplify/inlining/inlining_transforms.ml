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

open! Simplify_import
module DA = Downwards_acc
module DE = Downwards_env
module FT = Flambda2_types.Function_type
module VB = Bound_var

let make_inlined_body ~callee ~unroll_to ~params ~args ~my_closure ~my_region
    ~my_depth ~rec_info ~body ~exn_continuation ~return_continuation
    ~apply_exn_continuation ~apply_return_continuation =
  let callee, rec_info =
    match callee with
    | None ->
      (* CR ncourant: maybe instead of [None] for the callee we need something
         that allows the rec-info to be stored? *)
      None, Rec_info_expr.do_not_inline
    | Some callee -> (
      match unroll_to with
      | None -> Some callee, rec_info
      | Some unroll_depth ->
        let unrolled_rec_info = Rec_info_expr.unroll_to unroll_depth rec_info in
        let coercion_from_callee_to_unrolled_callee =
          Coercion.change_depth ~from:rec_info ~to_:(Rec_info_expr.var my_depth)
        in
        let callee =
          Simple.apply_coercion_exn callee
            coercion_from_callee_to_unrolled_callee
        in
        Some callee, unrolled_rec_info)
  in
  let my_closure =
    Bound_parameter.create my_closure Flambda_kind.With_subkind.any_value
  in
  let bind_params ~params ~args ~body =
    if List.compare_lengths params args <> 0
    then
      Misc.fatal_errorf "Mismatching parameters and arguments: %a and %a"
        Bound_parameters.print
        (Bound_parameters.create params)
        Simple.List.print args;
    ListLabels.fold_left2 (List.rev params) (List.rev args) ~init:body
      ~f:(fun expr param arg ->
        let var = Bound_var.create (BP.var param) Name_mode.normal in
        Let.create
          (Bound_pattern.singleton var)
          (Named.create_simple arg) ~body:expr ~free_names_of_body:Unknown
        |> Expr.create_let)
  in
  let bind_depth ~my_depth ~rec_info ~body =
    let bound = Bound_pattern.singleton (VB.create my_depth Name_mode.normal) in
    Let.create bound
      (Named.create_rec_info rec_info)
      ~body ~free_names_of_body:Unknown
    |> Expr.create_let
  in
  Inlining_helpers.make_inlined_body ~callee ~params ~args ~my_closure
    ~my_region ~my_depth ~rec_info ~body ~exn_continuation ~return_continuation
    ~apply_exn_continuation ~apply_return_continuation ~bind_params ~bind_depth
    ~apply_renaming:Expr.apply_renaming

let wrap_inlined_body_for_exn_extra_args ~extra_args ~apply_exn_continuation
    ~apply_return_continuation ~result_arity ~make_inlined_body =
  let apply_cont_create () ~trap_action cont ~args ~dbg =
    Apply_cont.create ~trap_action cont ~args ~dbg |> Expr.create_apply_cont
  in
  let let_cont_create () cont ~handler_params ~handler ~body ~is_exn_handler
      ~is_cold =
    let handler =
      Continuation_handler.create handler_params ~handler:(handler ())
        ~free_names_of_handler:Unknown ~is_exn_handler ~is_cold
    in
    Let_cont.create_non_recursive cont handler ~body:(body ())
      ~free_names_of_body:Unknown
  in
  Inlining_helpers.wrap_inlined_body_for_exn_extra_args () ~extra_args
    ~apply_exn_continuation ~apply_return_continuation ~result_arity
    ~make_inlined_body ~apply_cont_create ~let_cont_create

let inline dacc ~apply ~unroll_to ~was_inline_always function_decl =
  let callee = Apply.callee apply in
  let region_inlined_into =
    match Apply.call_kind apply with
    | Function { alloc_mode; _ } | Method { alloc_mode; _ } -> alloc_mode
    | C_call _ ->
      Misc.fatal_error
        "Trying to call [Inlining_transforms.inline] on a C call."
  in
  let args = Apply.args apply in
  let apply_return_continuation = Apply.continuation apply in
  let apply_exn_continuation = Apply.exn_continuation apply in
  (* CR-someday mshinwell: Add meet constraint to the return continuation *)
  let denv = DA.denv dacc in
  let code =
    Code_or_metadata.get_code (DE.find_code_exn denv (FT.code_id function_decl))
  in
  let rec_info =
    match T.meet_rec_info (DE.typing_env denv) (FT.rec_info function_decl) with
    | Known_result rec_info -> rec_info
    | Need_meet -> Rec_info_expr.unknown
    | Invalid -> (* CR vlaviron: ? *) Rec_info_expr.do_not_inline
  in
  match region_inlined_into, Code.result_mode code with
  | Heap, Alloc_local ->
    (* The alloc_mode of the application and of the code are incompatible.
       This should have been prevented by the typer; therefore we are in
       GADT-caused unreachable code; we replace the inlined body by [Invalid].
    *)
    dacc, Expr.create_invalid (Flambda.Invalid.Closure_type_was_invalid apply)
  | Local _, Alloc_heap (* This is allowed by subtyping *)
  | Local _, Alloc_local | Heap, Alloc_heap ->
  let denv =
    DE.enter_inlined_apply ~called_code:code ~apply ~was_inline_always denv
  in
  let params_and_body = Code.params_and_body code in
  Function_params_and_body.pattern_match params_and_body
    ~f:(fun
         ~return_continuation
         ~exn_continuation
         params
         ~body
         ~my_closure
         ~is_my_closure_used:_
         ~my_region
         ~my_depth
         ~free_names_of_body:_
       ->
      let make_inlined_body () =
        make_inlined_body ~callee ~region_inlined_into ~unroll_to
          ~params:(Bound_parameters.to_list params)
          ~args ~my_closure ~my_region ~my_depth ~rec_info ~body
          ~exn_continuation ~return_continuation
      in
      let expr =
        match Exn_continuation.extra_args apply_exn_continuation with
        | [] ->
          make_inlined_body ()
            ~apply_exn_continuation:
              (Exn_continuation.exn_handler apply_exn_continuation)
            ~apply_return_continuation
        | extra_args ->
          wrap_inlined_body_for_exn_extra_args ~extra_args
            ~apply_exn_continuation ~apply_return_continuation
            ~result_arity:(Code.result_arity code) ~make_inlined_body
      in
      DA.with_denv dacc denv, expr)
