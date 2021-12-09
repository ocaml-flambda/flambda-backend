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
    Simplify_switch_expr.simplify_switch ~simplify_let dacc switch ~down_to_up
  | Invalid _ ->
    (* CR mshinwell: Make sure that a program can be simplified to just
       [Invalid]. [To_cmm] should translate any [Invalid] that it sees as if it
       were [Halt_and_catch_fire]. *)
    down_to_up dacc ~rebuild:EB.rebuild_invalid

and simplify_toplevel dacc expr ~return_continuation ~return_arity
    ~exn_continuation ~return_cont_scope ~exn_cont_scope =
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
    simplify_expr dacc expr ~down_to_up:(fun dacc ~rebuild ->
        let dacc =
          DA.map_data_flow dacc
            ~f:(Data_flow.exit_continuation dummy_toplevel_cont)
        in
        let data_flow = DA.data_flow dacc in
        let closure_offsets = DA.closure_offsets dacc in
        let closure_info = DE.closure_info (DA.denv dacc) in
        (* The code_age_relation and used closure_vars are only correct at
           toplevel, and they are only necessary to compute the live code ids,
           which are only used when simplifying at the toplevel. So if we are in
           a closure, we use empty/dummy values for the code_age_relation and
           used_closure_vars, and in return we do not use the reachable_code_id
           part of the data_flow analysis. *)
        let code_age_relation, used_closure_vars =
          match Closure_info.in_or_out_of_closure closure_info with
          | In_a_closure -> Code_age_relation.empty, Or_unknown.Unknown
          | Not_in_a_closure ->
            ( DA.code_age_relation dacc,
              Or_unknown.Known (DA.used_closure_vars dacc) )
        in
        let ({ required_names; reachable_code_ids } : Data_flow.result) =
          Data_flow.analyze data_flow ~code_age_relation ~used_closure_vars
            ~return_continuation ~exn_continuation
        in
        (* The code_id part of the data_flow analysis is correct only at
           toplevel where all the code_ids are, so when in a closure, we state
           the the live code ids are unknown, which will prevent any from being
           mistakenly deleted. *)
        let reachable_code_ids : _ Or_unknown.t =
          match Closure_info.in_or_out_of_closure closure_info with
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
            exn_cont_scope [K.With_subkind.any_value]
        in
        let uacc =
          UA.create ~required_names ~reachable_code_ids ~closure_offsets uenv
            dacc
        in
        rebuild uacc ~after_rebuild:(fun expr uacc -> expr, uacc))
  in
  (* We don't check occurrences of variables or symbols here because the check
     required depends on whether we're dealing with a lambda or the whole
     compilation unit. Instead these checks are in [Simplify] or
     [Simplify_set_of_closures]. *)
  Name_occurrences.fold_continuations_including_in_trap_actions
    (UA.name_occurrences uacc) ~init:() ~f:(fun () cont ->
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

and[@inline always] simplify_let dacc let_expr ~down_to_up =
  Simplify_let_expr.simplify_let ~simplify_expr ~simplify_toplevel dacc let_expr
    ~down_to_up
