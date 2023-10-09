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

(** Rewrites applied to [Apply_cont] expressions in order to reflect changes in
    continuation arities consequential to addition or removal of parameters.

    The rewrites are actually applied via [Expr_builder]. *)

type t

type used =
  | Unused
  | Used
  | Used_as_invariant

val print : Format.formatter -> t -> unit

(** [extra_args] (and hence [extra_params]) must be given in order: later
    extra-args may refer to earlier extra-args, but not vice-versa. *)
val create :
  original_params:Bound_parameters.t ->
  extra_params_and_args:Continuation_extra_params_and_args.t ->
  decide_param_usage:(Bound_parameter.t -> used) ->
  t

val does_nothing : t -> bool

val get_used_params : t -> Bound_parameters.t * Bound_parameters.t

val get_unused_params : t -> Bound_parameters.t

val original_params_arity : t -> [> ] Flambda_arity.t

type rewrite_apply_cont_ctx =
  | Apply_cont
  | Apply_expr of Simple.t list

val make_rewrite :
  t ->
  ctx:rewrite_apply_cont_ctx ->
  Apply_cont_rewrite_id.t ->
  Simple.t list ->
  (Bound_var.t * Code_size.t * Flambda.Named.t) list * Simple.t list

val rewrite_exn_continuation :
  t -> Apply_cont_rewrite_id.t -> Exn_continuation.t -> Exn_continuation.t
