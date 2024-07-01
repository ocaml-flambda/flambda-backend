(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The type of expressions constructed by the simplifier.

    Using a type that is different from [Expr.t] enables us to robustly avoid
    rebuilding terms during simplification when unnecessary, e.g. during
    speculative inlining tests.

    Note that this module relies on the [UA.t] values passed in to determine the
    semantics of values of type [t]. As such, values of type [t] must not be
    passed between contexts where the value of [UA.do_not_rebuild_terms]
    differs. *)

open! Flambda

type t

type rebuilt_expr = t

val print : Are_rebuilding_terms.t -> Format.formatter -> t -> unit

(** This function may only be used when rebuilding terms. *)
val to_expr : t -> Are_rebuilding_terms.t -> Expr.t

val to_apply_cont : t -> Apply_cont.t option

val can_be_removed_as_invalid : t -> Are_rebuilding_terms.t -> bool

val term_not_rebuilt : t

(** This should only be used by [Expr_builder] to make sure occurrences of
    function slots and value slots are recorded. *)
val create_let :
  Are_rebuilding_terms.t ->
  Bound_pattern.t ->
  Named.t ->
  body:t ->
  free_names_of_body:Name_occurrences.t ->
  t

val create_apply : Are_rebuilding_terms.t -> Apply.t -> t

(** [Apply_cont] expressions are always rebuilt to allow optimisations in
    [Simplify_switch_expr] and [Simplify_let_cont_expr]. *)
val create_apply_cont : Apply_cont.t -> t

module Function_params_and_body : sig
  type t

  val create :
    return_continuation:Continuation.t ->
    exn_continuation:Continuation.t ->
    Bound_parameters.t ->
    body:rebuilt_expr ->
    free_names_of_body:Name_occurrences.t ->
    my_closure:Variable.t ->
    my_region:Variable.t ->
    my_ghost_region:Variable.t ->
    my_depth:Variable.t ->
    t

  (** This function may only be used when rebuilding terms. *)
  val to_function_params_and_body :
    t -> Are_rebuilding_terms.t -> Function_params_and_body.t

  val is_my_closure_used : t -> bool
end

module Continuation_handler : sig
  type t

  val print :
    cont:Continuation.t ->
    recursive:Recursive.t ->
    Format.formatter ->
    t ->
    unit

  val create :
    Are_rebuilding_terms.t ->
    Bound_parameters.t ->
    handler:rebuilt_expr ->
    free_names_of_handler:Name_occurrences.t ->
    is_exn_handler:bool ->
    is_cold:bool ->
    t

  val create' :
    Are_rebuilding_terms.t ->
    Bound_parameters.t ->
    handler:rebuilt_expr ->
    is_exn_handler:bool ->
    is_cold:bool ->
    t
end

val create_non_recursive_let_cont :
  Are_rebuilding_terms.t ->
  Continuation.t ->
  Continuation_handler.t ->
  body:t ->
  free_names_of_body:Name_occurrences.t ->
  t

val create_non_recursive_let_cont' :
  Are_rebuilding_terms.t ->
  Continuation.t ->
  Continuation_handler.t ->
  body:t ->
  num_free_occurrences_of_cont_in_body:Num_occurrences.t ->
  is_applied_with_traps:bool ->
  t

val create_non_recursive_let_cont_without_free_names :
  Are_rebuilding_terms.t ->
  Continuation.t ->
  Continuation_handler.t ->
  body:t ->
  t

val create_recursive_let_cont :
  Are_rebuilding_terms.t ->
  invariant_params:Bound_parameters.t ->
  Continuation_handler.t Continuation.Map.t ->
  body:t ->
  t

val create_switch : Are_rebuilding_terms.t -> Switch_expr.t -> t

val create_invalid : Invalid.t -> t

val bind_no_simplification :
  Are_rebuilding_terms.t ->
  bindings:(Bound_var.t * Code_size.t * Named.t) list ->
  body:t ->
  cost_metrics_of_body:Cost_metrics.t ->
  free_names_of_body:Name_occurrences.t ->
  t * Cost_metrics.t * Name_occurrences.t
