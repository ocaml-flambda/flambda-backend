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

open! Flambda

(** There are two (nested, higher-order) levels of continuations here:

    - one of them, the "term-level continuation" expresses the traversal order
    (enabling values to be held in the closures corresponding to meta-level
    continuations until they are needed later);

    - the other, the "compiler-level continuation" ensures that the simplifier
    is tail recursive.

    In the future the first continuation might be removed by
    defunctionalisation. *)

(** [rebuild] and [after_rebuild] form the compiler-level continuation. *)

type 'a after_rebuild = Rebuilt_expr.t -> Upwards_acc.t -> 'a

type 'a rebuild = Upwards_acc.t -> after_rebuild:'a after_rebuild -> 'a

(** [down_to_up] is the term-level continuation.

    Calling [down_to_up] doesn't mean that the downwards pass has been
    completely finished and we are ready to start the upwards pass. It means
    that the downwards pass has reached the end of some subexpression, at which
    point another subexpression can be traversed downwards, or (after all such
    subexpressions have been traversed) the upwards pass may begin. *)
type ('a, 'b) down_to_up = Downwards_acc.t -> rebuild:'a rebuild -> 'b

(** The environments and accumulators for simplification are as follows:

    - [Downwards_env], which operates like a normal typing environment following
    the scope of terms. It is discarded when the end of a given subexpression (a
    "terminator" expression, such as an [Apply]) is reached, upon which point
    the current [down_to_up] is called;

    - [Downwards_acc], which contains [Downwards_env] in addition to extra
    information (not scope-based) that is accumulated across all subexpressions;

    - [Upwards_env], which operates like a normal environment for the upwards
    pass, mainly used to record the handler expressions of continuations. It is
    discarded when the end of a given subexpression is reached.

    - [Upwards_acc], which contains [Upwards_env] in addition to extra
    information that is accumulated across all subexpressions.

    Upon changing from the downwards to the upwards pass, some information is
    propagated from the [Downwards_acc] to the [Upwards_acc]. (In fact the
    [Upwards_acc] contains the [Downwards_acc] from which it was created.) *)
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
  return_arity:[`Unarized] Flambda_arity.t ->
  exn_continuation:Continuation.t ->
  Rebuilt_expr.t * Upwards_acc.t

type simplify_function_body =
  Downwards_acc.t ->
  Expr.t ->
  return_continuation:Continuation.t ->
  return_arity:[`Unarized] Flambda_arity.t ->
  exn_continuation:Continuation.t ->
  loopify_state:Loopify_state.t ->
  params:Bound_parameters.t ->
  implicit_params:Bound_parameters.t ->
  Rebuilt_expr.t * Upwards_acc.t

val simplify_projection :
  Downwards_acc.t ->
  original_term:Named.t ->
  deconstructing:Flambda2_types.t ->
  shape:Flambda2_types.t ->
  result_var:Bound_var.t ->
  result_kind:Flambda_kind.t ->
  Simplify_primitive_result.t

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
  apply_alloc_mode:Alloc_mode.For_types.t ->
  current_region:Variable.t ->
  callee's_code_id:Code_id.t ->
  callee's_code_metadata:Code_metadata.t ->
  Expr.t

type apply_cont_context =
  | Apply_cont_expr
  | Switch_branch

val apply_cont_use_kind :
  context:apply_cont_context -> Apply_cont.t -> Continuation_use_kind.t

val clear_demoted_trap_action_and_patch_unused_exn_bucket :
  Upwards_acc.t -> Apply_cont.t -> Apply_cont.t

(** Warning: This function relies on [T.meet_is_flat_float_array], which could
    return any kind for empty arrays. So this function is only safe for
    operations that are invalid on empty arrays. *)
val specialise_array_kind :
  Downwards_acc.t ->
  Flambda_primitive.Array_kind.t ->
  array_ty:Flambda2_types.t ->
  Flambda_primitive.Array_kind.t Or_bottom.t

(** General notes about symbol projections (applicable to [Block_load] and
    [Project_value_slot] primitives):

    Projections from symbols bound to variables are important to remember, since
    if such a variable occurs in a set of closures environment or other value
    that can potentially be lifted, the knowledge that the variable is equal to
    a symbol projection can make the difference between being able to lift and
    not being able to lift. We try to avoid recording symbol projections whose
    answer is known (in particular the answer is a symbol or a constant), since
    such symbol projection knowledge doesn't affect lifting decisions.

    We only need to record a projection if the defining expression remains as a
    [Prim]. In particular if the defining expression simplified to a variable
    (via the [Simple] constructor), then in the event that the variable is
    itself a symbol projection, the environment will already know this fact.

    We don't need to record a projection if we are currently at toplevel, since
    any variable involved in a constant to be lifted from that position will
    also be at toplevel. *)
val add_symbol_projection :
  Downwards_acc.t ->
  projected_from:Simple.t ->
  Symbol_projection.Projection.t ->
  projection_bound_to:Bound_var.t ->
  kind:Flambda_kind.With_subkind.t ->
  Downwards_acc.t
