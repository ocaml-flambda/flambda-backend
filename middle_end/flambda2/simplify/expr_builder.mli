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

(** Functions for rebuilding expressions that are used during simplification.
    Unlike the basic creation functions in [Expr] these functions do things such
    as keeping track of free names and avoiding generation of unused bindings.
    They also elide construction of the terms themselves if required, e.g.
    during speculative inlining.

    Some expressions are rebuilt directly using the functions in [Rebuilt_expr]
    rather than using this module. *)

open! Flambda.Import

val create_let_binding :
  Upwards_acc.t ->
  Bound_pattern.t ->
  Named.t ->
  free_names_of_defining_expr:Name_occurrences.t ->
  body:Rebuilt_expr.t ->
  cost_metrics_of_defining_expr:Cost_metrics.t ->
  Rebuilt_expr.t * Upwards_acc.t

type binding_to_place =
  { let_bound : Bound_pattern.t;
    simplified_defining_expr : Simplified_named.t;
    original_defining_expr : Named.t option
  }

(** Create [Let] binding(s) around a given body. (The type of this function
    prevents it from being used to create "let symbol" bindings; use the other
    functions in this module instead.) Bindings will be elided if they are
    unused.

    The [name_occurrences] in the provided [uacc] must contain exactly the free
    names of the [body]. *)
val make_new_let_bindings :
  Upwards_acc.t ->
  bindings_outermost_first:binding_to_place list ->
  body:Rebuilt_expr.t ->
  Rebuilt_expr.t * Upwards_acc.t

(** Create the "let symbol" binding(s) around a given body necessary to define
    the given lifted constant.

    Value slots are removed if they are not used according to the given [uacc].
    (Such [uacc] must have seen all uses in the whole compilation unit.)

    The [name_occurrences] in the provided [uacc] must contain exactly the free
    names of the [body]. *)
val create_let_symbols :
  Upwards_acc.t ->
  Lifted_constant.t ->
  body:Rebuilt_expr.t ->
  Rebuilt_expr.t * Upwards_acc.t

(** Place lifted constants arising from a let-expr (coming from both the
    defining_expr and the body). *)
val place_lifted_constants :
  Upwards_acc.t ->
  lifted_constants_from_defining_expr:Lifted_constant_state.t ->
  lifted_constants_from_body:Lifted_constant_state.t ->
  put_bindings_around_body:
    (Upwards_acc.t -> body:Rebuilt_expr.t -> Rebuilt_expr.t * Upwards_acc.t) ->
  body:Rebuilt_expr.t ->
  Rebuilt_expr.t * Upwards_acc.t

(** Create a [Switch] expression, save that zero-arm switches are converted to
    [Invalid], and one-arm switches to [Apply_cont]. *)
val create_switch :
  Upwards_acc.t ->
  condition_dbg:Debuginfo.t ->
  scrutinee:Simple.t ->
  arms:Apply_cont.t Targetint_31_63.Map.t ->
  Rebuilt_expr.t * Upwards_acc.t

type new_let_cont =
  { cont : Continuation.t;
    handler : Rebuilt_expr.Continuation_handler.t;
    free_names_of_handler : Name_occurrences.t;
    cost_metrics_of_handler : Cost_metrics.t
  }

val bind_let_conts :
  Upwards_acc.t ->
  body:Rebuilt_expr.t ->
  new_let_cont list ->
  Upwards_acc.t * Rebuilt_expr.t

val rebuild_invalid :
  Upwards_acc.t ->
  Flambda.Invalid.t ->
  after_rebuild:
    (Rebuilt_expr.t -> Upwards_acc.t -> Rebuilt_expr.t * Upwards_acc.t) ->
  Rebuilt_expr.t * Upwards_acc.t

(** Handling of the rewriting of continuation use sites. *)

type rewrite_apply_cont_result = private
  | Invalid of { message : string }
  | Apply_cont of Apply_cont.t
  | Expr of
      (apply_cont_to_expr:
         (Apply_cont.t -> Rebuilt_expr.t * Cost_metrics.t * Name_occurrences.t) ->
      Rebuilt_expr.t * Cost_metrics.t * Name_occurrences.t)

type rewrite_switch_arm_result = private
  | Invalid of { message : string }
  | Apply_cont of Apply_cont.t
  | New_wrapper of new_let_cont

val no_rewrite_apply_cont : Apply_cont.t -> rewrite_apply_cont_result

val rewrite_apply_cont :
  Upwards_acc.t ->
  Apply_cont_rewrite.t ->
  Apply_cont_rewrite_id.t ->
  Apply_cont.t ->
  rewrite_apply_cont_result

val rewrite_switch_arm :
  Upwards_acc.t ->
  Apply_cont.t ->
  use_id:Apply_cont_rewrite_id.t ->
  [`Unarized] Flambda_arity.t ->
  rewrite_switch_arm_result

val rewrite_fixed_arity_apply :
  Upwards_acc.t ->
  use_id:Apply_cont_rewrite_id.t option ->
  [`Unarized] Flambda_arity.t ->
  Apply.t ->
  Upwards_acc.t * Rebuilt_expr.t

val rewrite_exn_continuation :
  Apply_cont_rewrite.t ->
  Apply_cont_rewrite_id.t ->
  Exn_continuation.t ->
  Exn_continuation.t
