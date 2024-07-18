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

let simplify_toplevel_common dacc simplify ~params ~implicit_params
    ~return_continuation ~return_arity ~exn_continuation =
  (* The usage analysis needs a continuation whose handler holds the toplevel
     code of the function. Since such a continuation does not exist, we create a
     dummy one here. *)
  let dummy_toplevel_cont =
    Continuation.create ~name:"dummy_toplevel_continuation" ()
  in
  let dacc =
    DA.map_flow_acc dacc
      ~f:
        (Flow.Acc.init_toplevel ~dummy_toplevel_cont
           (Bound_parameters.append params implicit_params))
  in
  let expr, uacc =
    simplify dacc ~down_to_up:(fun dacc ~rebuild ->
        let dacc =
          DA.map_flow_acc dacc
            ~f:(Flow.Acc.exit_continuation dummy_toplevel_cont)
        in
        let data_flow = DA.flow_acc dacc in
        let closure_info = DE.closure_info (DA.denv dacc) in
        (* The code_age_relation and used value_slots are only correct at
           toplevel, and they are only necessary to compute the live code ids,
           which are only used when simplifying at the toplevel. So if we are in
           a closure, we use empty/dummy values for the code_age_relation and
           used_value_slots, and in return we do not use the reachable_code_id
           part of the data_flow analysis. *)
        let code_age_relation, used_value_slots, print_name =
          match closure_info with
          | Closure { code_id; _ } ->
            Code_age_relation.empty, Or_unknown.Unknown, Code_id.name code_id
          | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
            assert false
          | Not_in_a_closure ->
            ( DA.code_age_relation dacc,
              Or_unknown.Known (DA.used_value_slots dacc),
              "toplevel" )
        in
        (* CR mshinwell: maybe [code_ids_to_never_delete] should have
           "zero_alloc" in the name *)
        let code_ids_to_never_delete = DA.code_ids_to_never_delete dacc in
        let may_be_code_ids_kept_for_zero_alloc =
          not (Code_id.Set.is_empty code_ids_to_never_delete)
        in
        let flow_result =
          Flow.Analysis.analyze data_flow ~print_name ~code_age_relation
            ~used_value_slots ~code_ids_to_never_delete ~return_continuation
            ~exn_continuation
        in
        let code_ids_kept_for_zero_alloc =
          if not may_be_code_ids_kept_for_zero_alloc
          then Code_id.Set.empty
          else
            let flow_result_without_never_delete =
              Flow.Analysis.analyze data_flow ~print_name ~code_age_relation
                ~used_value_slots ~code_ids_to_never_delete:Code_id.Set.empty
                ~return_continuation ~exn_continuation
            in
            match flow_result.data_flow_result.reachable_code_ids with
            | Unknown ->
              (* This value will never be used (it means we were called not at
                 toplevel). *)
              Code_id.Set.empty
            | Known reachable_code_ids -> (
              match
                flow_result_without_never_delete.data_flow_result
                  .reachable_code_ids
              with
              | Unknown -> assert false
              | Known reachable_code_ids_without_never_delete ->
                Code_id.Set.diff reachable_code_ids.live_code_ids
                  reachable_code_ids_without_never_delete.live_code_ids)
        in
        let uenv =
          UE.add_function_return_or_exn_continuation
            (UE.create (DA.are_rebuilding_terms dacc))
            return_continuation return_arity
        in
        let uenv =
          UE.add_function_return_or_exn_continuation uenv exn_continuation
            (Flambda_arity.create_singletons [K.With_subkind.any_value])
        in
        let uacc =
          UA.create ~flow_result ~compute_slot_offsets:true
            ~code_ids_kept_for_zero_alloc uenv dacc
        in
        let uacc =
          if Flow.Analysis.did_perform_mutable_unboxing flow_result
          then UA.set_resimplify uacc
          else uacc
        in
        rebuild uacc ~after_rebuild:(fun expr uacc -> expr, uacc))
  in
  (* We don't check occurrences of variables or symbols here because the check
     required depends on whether we're dealing with a lambda or the whole
     compilation unit. Instead these checks are in [Simplify] or
     [Simplify_set_of_closures]. *)
  NO.fold_continuations_including_in_trap_actions (UA.name_occurrences uacc)
    ~init:() ~f:(fun () cont ->
      if (not (Continuation.equal cont return_continuation))
         && not (Continuation.equal cont exn_continuation)
      then
        Misc.fatal_errorf
          "Continuation %a should not be free in toplevel expression after \
           simplification (return continuation %a, exn continuation %a):@ %a"
          Continuation.print cont
          (RE.print (UA.are_rebuilding_terms uacc))
          expr Continuation.print return_continuation Continuation.print
          exn_continuation);
  expr, uacc

(* CR-someday mshinwell: Need to simplify each [dbg] we come across. *)
(* CR-someday mshinwell: Consider defunctionalising to remove the [k]. *)

let rec simplify_expr dacc expr ~down_to_up =
  match Expr.descr expr with
  | Let let_expr -> simplify_let dacc let_expr ~down_to_up
  | Let_cont let_cont ->
    Simplify_let_cont_expr.simplify_let_cont ~simplify_expr dacc let_cont
      ~down_to_up
  | Apply apply ->
    Simplify_apply_expr.simplify_apply ~simplify_expr dacc apply ~down_to_up
  | Apply_cont apply_cont ->
    Simplify_apply_cont_expr.simplify_apply_cont dacc apply_cont ~down_to_up
  | Switch switch ->
    Simplify_switch_expr.simplify_switch
      ~simplify_let:Simplify_let_expr.simplify_let ~simplify_function_body dacc
      switch ~down_to_up
  | Invalid { message } ->
    (* CR mshinwell: Make sure that a program can be simplified to just
       [Invalid]. *)
    down_to_up dacc ~rebuild:(fun uacc ~after_rebuild ->
        EB.rebuild_invalid uacc (Message message) ~after_rebuild)

and simplify_function_body dacc expr ~return_continuation ~return_arity
    ~exn_continuation ~(loopify_state : Loopify_state.t) ~params
    ~implicit_params =
  match loopify_state with
  | Do_not_loopify ->
    simplify_toplevel_common dacc
      (fun dacc -> simplify_expr dacc expr)
      ~params ~implicit_params ~return_continuation ~return_arity
      ~exn_continuation
  | Loopify cont ->
    let call_self_cont_expr =
      let args = Bound_parameters.simples params in
      Expr.create_apply_cont
        (Apply_cont_expr.create cont ~args ~dbg:Debuginfo.none)
    in
    let handlers =
      Continuation.Map.singleton cont
        (Continuation_handler.create params ~handler:expr
           ~free_names_of_handler:Unknown ~is_exn_handler:false ~is_cold:false)
    in
    simplify_toplevel_common dacc
      (fun dacc ->
        Simplify_let_cont_expr.simplify_as_recursive_let_cont ~simplify_expr
          dacc
          (call_self_cont_expr, handlers))
      ~params ~implicit_params ~return_continuation ~return_arity
      ~exn_continuation

and[@inline always] simplify_let dacc let_expr ~down_to_up =
  Simplify_let_expr.simplify_let ~simplify_expr ~simplify_function_body dacc
    let_expr ~down_to_up

let simplify_toplevel dacc expr ~return_continuation ~return_arity
    ~exn_continuation =
  let params = Bound_parameters.empty in
  let implicit_params = Bound_parameters.empty in
  simplify_toplevel_common dacc
    (fun dacc -> simplify_expr dacc expr)
    ~params ~implicit_params ~return_continuation ~return_arity
    ~exn_continuation
