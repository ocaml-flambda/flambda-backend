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

let simplify_toplevel_common dacc simplify
    ~(in_or_out_of_closure : Closure_info.in_or_out_of_closure)
    ~return_continuation ~return_arity ~exn_continuation ~return_cont_scope
    ~exn_cont_scope =
  (* The usage analysis needs a continuation whose handler holds the toplevel
     code of the function. Since such a continuation does not exist, we create a
     dummy one here. *)
  let dummy_toplevel_cont =
    Continuation.create ~name:"dummy_toplevel_continuation" ()
  in
  let dacc =
    DA.map_data_flow dacc ~f:(Data_flow.init_toplevel dummy_toplevel_cont [])
  in
  let expr, uacc =
    simplify dacc ~down_to_up:(fun dacc ~rebuild ->
        let dacc =
          DA.map_data_flow dacc
            ~f:(Data_flow.exit_continuation dummy_toplevel_cont)
        in
        let data_flow = DA.data_flow dacc in
        (* The code_age_relation and used value_slots are only correct at
           toplevel, and they are only necessary to compute the live code ids,
           which are only used when simplifying at the toplevel. So if we are in
           a closure, we use empty/dummy values for the code_age_relation and
           used_value_slots, and in return we do not use the reachable_code_id
           part of the data_flow analysis. *)
        let code_age_relation, used_value_slots =
          match in_or_out_of_closure with
          | In_a_closure -> Code_age_relation.empty, Or_unknown.Unknown
          | Not_in_a_closure ->
            ( DA.code_age_relation dacc,
              Or_unknown.Known (DA.used_value_slots dacc) )
        in
        let ({ required_names; reachable_code_ids } : Data_flow.result) =
          Data_flow.analyze data_flow ~code_age_relation ~used_value_slots
            ~return_continuation ~exn_continuation
        in
        (* The code_id part of the data_flow analysis is correct only at
           toplevel where all the code_ids are, so when in a closure, we state
           the the live code ids are unknown, which will prevent any from being
           mistakenly deleted. *)
        let reachable_code_ids : _ Or_unknown.t =
          match in_or_out_of_closure with
          | In_a_closure -> Unknown
          | Not_in_a_closure -> Known reachable_code_ids
        in
        let uenv =
          UE.add_function_return_or_exn_continuation
            (UE.create (DA.are_rebuilding_terms dacc))
            return_continuation return_cont_scope return_arity
        in
        let uenv =
          UE.add_function_return_or_exn_continuation uenv exn_continuation
            exn_cont_scope
            (Flambda_arity.With_subkinds.create [K.With_subkind.any_value])
        in
        let uacc =
          UA.create ~required_names ~reachable_code_ids
            ~compute_slot_offsets:true uenv dacc
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
    ~exn_continuation ~return_cont_scope ~exn_cont_scope
    ~(loopify_state : Loopify_state.t) ~params =
  match loopify_state with
  | Do_not_loopify ->
    simplify_toplevel_common dacc
      (fun dacc -> simplify_expr dacc expr)
      ~in_or_out_of_closure:In_a_closure ~return_continuation ~return_arity
      ~exn_continuation ~return_cont_scope ~exn_cont_scope
  | Loopify cont ->
    let call_self_cont_expr =
      let args = Bound_parameters.simples params in
      Expr.create_apply_cont (Apply_cont_expr.create cont ~args ~dbg:[])
    in
    let handlers =
      Continuation.Map.singleton cont
        (Continuation_handler.create params ~handler:expr
           ~free_names_of_handler:Unknown ~is_exn_handler:false)
    in
    simplify_toplevel_common dacc
      (fun dacc ->
        Simplify_let_cont_expr.simplify_as_recursive_let_cont ~simplify_expr
          dacc
          (call_self_cont_expr, handlers))
      ~in_or_out_of_closure:In_a_closure ~return_continuation ~return_arity
      ~exn_continuation ~return_cont_scope ~exn_cont_scope

and[@inline always] simplify_let dacc let_expr ~down_to_up =
  Simplify_let_expr.simplify_let ~simplify_expr ~simplify_function_body dacc
    let_expr ~down_to_up

let simplify_toplevel dacc expr ~return_continuation ~return_arity
    ~exn_continuation ~return_cont_scope ~exn_cont_scope =
  simplify_toplevel_common dacc
    (fun dacc -> simplify_expr dacc expr)
    ~in_or_out_of_closure:Not_in_a_closure ~return_continuation ~return_arity
    ~exn_continuation ~return_cont_scope ~exn_cont_scope
