(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Miscellaneous utility functions and types used by the simplifier. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda

type 'a after_rebuild = Rebuilt_expr.t -> Upwards_acc.t -> 'a

type 'a rebuild = Upwards_acc.t -> after_rebuild:'a after_rebuild -> 'a

type ('a, 'b) down_to_up = Downwards_acc.t -> rebuild:'a rebuild -> 'b

type 'a expr_simplifier =
  Downwards_acc.t ->
  'a ->
  down_to_up:
    (Rebuilt_expr.t * Upwards_acc.t, Rebuilt_expr.t * Upwards_acc.t) down_to_up ->
  Rebuilt_expr.t * Upwards_acc.t

type simplify_toplevel =
  Downwards_acc.t ->
  Expr.t ->
  return_continuation:Continuation.t ->
  return_arity:Flambda_arity.With_subkinds.t ->
  exn_continuation:Continuation.t ->
  return_cont_scope:Scope.t ->
  exn_cont_scope:Scope.t ->
  Rebuilt_expr.t * Upwards_acc.t

val is_self_tail_call : Downwards_acc.t -> Apply_expr.t -> bool

val simplify_projection :
  Downwards_acc.t ->
  original_term:Named.t ->
  deconstructing:Flambda2_types.t ->
  shape:Flambda2_types.t ->
  result_var:Bound_var.t ->
  result_kind:Flambda_kind.t ->
  Simplified_named.t * Downwards_acc.t

val update_exn_continuation_extra_args :
  Upwards_acc.t ->
  exn_cont_use_id:Apply_cont_rewrite_id.t ->
  Apply_expr.t ->
  Apply_expr.t

(** Create a projection from a tuple (assumed to be a [size]-tuple of OCaml
    values). *)
val project_tuple :
  dbg:Debuginfo.t -> size:int -> field:int -> Simple.t -> Named.t

(** Split a direct over-application into a full application followed by the
    application of the leftover arguments. *)
val split_direct_over_application :
  Apply_expr.t ->
  param_arity:Flambda_arity.With_subkinds.t ->
  result_arity:Flambda_arity.With_subkinds.t ->
  apply_alloc_mode:Alloc_mode.t ->
  contains_no_escaping_local_allocs:bool ->
  Expr.t

type apply_cont_context =
  | Apply_cont_expr
  | Switch_branch

val apply_cont_use_kind :
  context:apply_cont_context -> Apply_cont.t -> Continuation_use_kind.t

val clear_demoted_trap_action_and_patch_unused_exn_bucket :
  Upwards_acc.t -> Apply_cont.t -> Apply_cont.t

val specialise_array_kind :
  Downwards_acc.t ->
  Flambda_primitive.Array_kind.t ->
  array_ty:Flambda2_types.t ->
  Flambda_primitive.Array_kind.t Or_bottom.t
