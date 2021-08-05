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

(** Functions for rebuilding expressions that are used during
    simplification.  Unlike the basic creation functions in [Expr] these
    functions do things such as keeping track of free names and
    avoiding generation of unused bindings.  They also elide construction of
    the terms themselves if required, e.g. during speculative inlining.

    Some expressions are rebuilt directly using the functions in [Rebuilt_expr]
    rather than using this module. *)

[@@@ocaml.warning "+a-30-40-41-42"]

open! Flambda.Import

(** Create [Let] binding(s) around a given body.  (The type of this function
    prevents it from being used to create "let symbol" bindings; use the
    other functions in this module instead.)  Bindings will be elided if they
    are unused.
    The [name_occurrences] in the provided [uacc] must contain exactly the
    free names of the [body]. *)
val make_new_let_bindings
   : Upwards_acc.t
  -> bindings_outermost_first:Simplify_named_result.binding_to_place list
  -> body:Rebuilt_expr.t
  -> Rebuilt_expr.t * Upwards_acc.t

(** Create the "let symbol" binding(s) around a given body necessary to define
    the given lifted constant.  Two optimisations are performed:
    1. Best efforts are made not to create the binding(s) if it/they
       would be redundant.
    2. Closure variables are removed if they are not used according to the
       given [uacc].  (Such [uacc] must have seen all uses in the whole
       compilation unit.)
    The [name_occurrences] in the provided [uacc] must contain exactly the
    free names of the [body]. *)
val create_let_symbols
   : Upwards_acc.t
  -> Lifted_constant.t
  -> body:Rebuilt_expr.t
  -> Rebuilt_expr.t * Upwards_acc.t

(** Place lifted constants whose defining expressions involve [Name]s (for
    example those bound by a [Let] or a [Let_cont]) that are about to go out
    of scope.
    The [name_occurrences] in the provided [uacc] must contain exactly the
    free names of the [body]. *)
val place_lifted_constants
   : Upwards_acc.t
  -> lifted_constants_from_defining_expr:Lifted_constant_state.t
  -> lifted_constants_from_body:Lifted_constant_state.t
  -> put_bindings_around_body:
       (Upwards_acc.t
         -> body:Rebuilt_expr.t
         -> Rebuilt_expr.t * Upwards_acc.t)
  -> body:Rebuilt_expr.t
  -> critical_deps_of_bindings:Name_occurrences.t
  -> Rebuilt_expr.t * Upwards_acc.t

(** Create a [Switch] expression, save that zero-arm switches are converted
    to [Invalid], and one-arm switches to [Apply_cont]. *)
val create_switch
   : Upwards_acc.t
  -> scrutinee:Simple.t
  -> arms:Apply_cont.t Targetint_31_63.Map.t
  -> Rebuilt_expr.t * Upwards_acc.t

val rebuild_invalid
   : Upwards_acc.t
  -> after_rebuild:(Rebuilt_expr.t
       -> Upwards_acc.t
       -> (Rebuilt_expr.t * Upwards_acc.t))
  -> Rebuilt_expr.t * Upwards_acc.t

type add_wrapper_for_switch_arm_result = private
  | Apply_cont of Apply_cont.t
  | New_wrapper of Continuation.t * Rebuilt_expr.Continuation_handler.t
      * Name_occurrences.t * Cost_metrics.t

val add_wrapper_for_switch_arm
   : Upwards_acc.t
  -> Apply_cont.t
  -> use_id:Apply_cont_rewrite_id.t
  -> Flambda_arity.With_subkinds.t
  -> add_wrapper_for_switch_arm_result

val add_wrapper_for_fixed_arity_apply
   : Upwards_acc.t
  -> use_id:Apply_cont_rewrite_id.t
  -> Flambda_arity.With_subkinds.t
  -> Apply.t
  -> Rebuilt_expr.t * Upwards_acc.t

type rewrite_use_ctx =
  | Apply_cont
  | Apply_expr of Simple.t list

type rewrite_use_result = private
  | Apply_cont of Apply_cont.t
  | Expr of (
       apply_cont_to_expr:(Apply_cont.t
         -> (Rebuilt_expr.t * Cost_metrics.t * Name_occurrences.t))
    -> Rebuilt_expr.t * Cost_metrics.t * Name_occurrences.t)

val no_rewrite : Apply_cont.t -> rewrite_use_result

val rewrite_use
   : Upwards_acc.t
  -> Apply_cont_rewrite.t
  -> ctx:rewrite_use_ctx
  -> Apply_cont_rewrite_id.t
  -> Apply_cont.t
  -> rewrite_use_result

val rewrite_exn_continuation
   : Apply_cont_rewrite.t
  -> Apply_cont_rewrite_id.t
  -> Exn_continuation.t
  -> Exn_continuation.t
