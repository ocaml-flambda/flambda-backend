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

[@@@ocaml.warning "+a-30-40-41-42"]

open! Simplify_import

let inline_linearly_used_continuation uacc ~create_apply_cont ~params ~handler
      ~free_names_of_handler ~cost_metrics_of_handler =
  (* CR mshinwell: With -g, we can end up with continuations that are
     just a sequence of phantom lets then "goto".  These would normally
     be treated as aliases, but of course aren't in this scenario,
     unless the continuations are used linearly. *)
  create_apply_cont ~apply_cont_to_expr:(fun apply_cont ->
    assert (Option.is_none (AC.trap_action apply_cont));
    (* We can't easily call [simplify_expr] on the inlined body since
       [dacc] isn't the correct accumulator and environment any more.
       However there's no need to simplify the inlined body except to
       make use of parameter-to-argument bindings; we just leave them for
       a subsequent round of [Simplify] or [Un_cps] to clean up. *)
    let args = AC.args apply_cont in
    if List.compare_lengths params args <> 0 then begin
      Misc.fatal_errorf "Parameter list@ [%a]@ does not match argument \
          list@ [%a]@ when inlining at [Apply_cont]:@ %a@ \
          Handler to inline:@ %a"
        KP.List.print params
        Simple.List.print args
        Apply_cont.print apply_cont
        (RE.print (UA.are_rebuilding_terms uacc)) handler
    end;
    let bindings_outermost_first =
      ListLabels.map2 params args
        ~f:(fun param arg ->
          let let_bound =
            Var_in_binding_pos.create (KP.var param) Name_mode.normal
            |> Bindable_let_bound.singleton
          in
          let named = Named.create_simple arg in
          { Simplify_named_result.let_bound;
            simplified_defining_expr = Simplified_named.reachable named;
            original_defining_expr = Some named })
    in
    let expr, uacc =
      let uacc =
        UA.with_name_occurrences uacc ~name_occurrences:free_names_of_handler
        |> UA.with_cost_metrics cost_metrics_of_handler
      in
      EB.make_new_let_bindings uacc ~bindings_outermost_first ~body:handler
    in
    expr, UA.cost_metrics uacc, UA.name_occurrences uacc)

let rebuild_apply_cont apply_cont ~args ~rewrite_id uacc ~after_rebuild =
  let uenv = UA.uenv uacc in
  let cont = AC.continuation apply_cont in
  let rewrite = UE.find_apply_cont_rewrite uenv cont in
  let cont = UE.resolve_continuation_aliases uenv cont in
  let create_apply_cont ~apply_cont_to_expr =
    (* The function returned by this code accepts another function, which
       will be called with the [Apply_cont] expression after subjecting it
       to any rewrites (e.g. adding or removing parameters).  This gives the
       chance of replacing the [Apply_cont] with something else -- in
       particular an inlined continuation -- before it is wrapped in any
       [Let]-expressions needed as a result of the rewrite. *)
    let rewrite_use_result =
      let apply_cont = AC.update_continuation_and_args apply_cont cont ~args in
      let apply_cont =
        Simplify_common.clear_demoted_trap_action uacc apply_cont
      in
      match rewrite with
      | None -> EB.no_rewrite apply_cont
      | Some rewrite ->
        EB.rewrite_use uacc rewrite ~ctx:Apply_cont rewrite_id apply_cont
    in
    match rewrite_use_result with
    | Apply_cont apply_cont ->
      let expr, cost_metrics, free_names = apply_cont_to_expr apply_cont in
      let uacc =
        UA.add_free_names uacc free_names
        |> UA.add_cost_metrics cost_metrics
      in
      after_rebuild expr uacc
    | Expr build_expr ->
      let expr, cost_metrics, free_names = build_expr ~apply_cont_to_expr in
      let uacc =
        UA.add_free_names uacc free_names
        |> UA.add_cost_metrics cost_metrics
      in
      after_rebuild expr uacc
  in
  match UE.find_continuation uenv cont with
  | Linearly_used_and_inlinable { params; handler;
      free_names_of_handler; cost_metrics_of_handler } ->
    (* We must not fail to inline here, since we've already decided that the
       relevant [Let_cont] is no longer needed. *)

    (* When removing continuations don't increment the removed branch counter.
       We can't be sure that removing a continuation maps to removing a branch
       as the decision will be taken later on by the backend. If we were able
       to track the number of times each continuation is used then we would be
       able to track this a bit better (or to create a new counter to count the
       number of continuations) that became linearly used.
       In any case the impact of branches is harder to quantify than the impact
       of allocating (branches can be moved by the backend, their runtime
       depends on the branch predictor...). Underestimating the number of
       removed branch is fine.

       let uacc =
        UA.notify_removed ~operation:Removed_operations.branch uacc
      in
    *)
    inline_linearly_used_continuation uacc ~create_apply_cont ~params ~handler
      ~free_names_of_handler ~cost_metrics_of_handler
  | Unreachable { arity = _; } ->
    (* We allow this transformation even if there is a trap action, on the
       basis that there wouldn't be any opportunity to collect any backtrace,
       even if the [Apply_cont] were compiled as "raise". *)
    after_rebuild (RE.create_invalid ()) uacc
  | Non_inlinable_zero_arity _ | Non_inlinable_non_zero_arity _
  | Toplevel_or_function_return_or_exn_continuation _ ->
    create_apply_cont ~apply_cont_to_expr:(fun apply_cont ->
      RE.create_apply_cont apply_cont,
      Cost_metrics.from_size (Code_size.apply_cont apply_cont),
      Apply_cont.free_names apply_cont)

let simplify_apply_cont dacc apply_cont ~down_to_up =
  let { S. simples = args; simple_tys = arg_types; } =
    S.simplify_simples dacc (AC.args apply_cont)
  in
  let dacc =
    DA.map_data_flow dacc ~f:(fun data_flow ->
      Data_flow.add_apply_cont_args
        (AC.continuation apply_cont)
        (List.map Simple.free_names args)
        data_flow)
  in
  let use_kind =
    Simplify_common.apply_cont_use_kind ~context:Apply_cont_expr apply_cont
  in
  let dacc, rewrite_id =
    DA.record_continuation_use dacc (AC.continuation apply_cont)
      use_kind ~env_at_use:(DA.denv dacc) ~arg_types
  in
  let dbg = AC.debuginfo apply_cont in
  let dbg = DE.add_inlined_debuginfo' (DA.denv dacc) dbg in
  let apply_cont = AC.with_debuginfo apply_cont ~dbg in
  down_to_up dacc ~rebuild:(rebuild_apply_cont apply_cont ~args ~rewrite_id)
