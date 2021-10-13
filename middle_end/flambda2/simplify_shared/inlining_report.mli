(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020--2020 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Report inlining decisions *)

type pass =
  | After_closure_conversion
  | Before_simplify
  | After_simplify

val record_decision_at_call_site_for_known_function :
  tracker:Inlining_history.Tracker.t ->
  unrolling_depth:int option ->
  apply:Apply_expr.t ->
  pass:pass ->
  callee:Inlining_history.Absolute.t ->
  are_rebuilding_terms:Are_rebuilding_terms.t ->
  Call_site_inlining_decision_type.t ->
  unit

val record_decision_at_call_site_for_unknown_function :
  tracker:Inlining_history.Tracker.t ->
  apply:Apply_expr.t ->
  pass:pass ->
  unit ->
  unit

val record_decision_at_function_definition :
  absolute_history:Inlining_history.Absolute.t ->
  code_metadata:Code_metadata.t ->
  pass:pass ->
  are_rebuilding_terms:Are_rebuilding_terms.t ->
  Function_decl_inlining_decision_type.t ->
  unit

(** Output the report for all recorded decisions up to that point, and
    clean/forget all decisions.

    Note that this function should be called once for each round of
    simplification. *)
val output_then_forget_decisions : output_prefix:string -> unit
