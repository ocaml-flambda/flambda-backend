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

(** Rewrites applied to [Apply_cont] expressions in order to reflect
    changes in continuation arities consequential to addition or removal of
    parameters.

    The rewrites are actually applied via [Expr_builder]. *)

[@@@ocaml.warning "+a-30-40-41-42"]

type t

type used = private Used | Unused

val print : Format.formatter -> t -> unit

(** [extra_args] (and hence [extra_params]) must be given in order: later
    extra-args may refer to earlier extra-args, but not vice-versa. *)
val create
   : original_params:Kinded_parameter.t list
  -> used_params:Kinded_parameter.Set.t
  -> extra_params:Kinded_parameter.t list
  -> extra_args:Continuation_extra_params_and_args.Extra_arg.t list
       Apply_cont_rewrite_id.Map.t
  -> used_extra_params:Kinded_parameter.Set.t
  -> t

val original_params : t -> Kinded_parameter.t list

val used_params : t -> Kinded_parameter.Set.t

val used_extra_params : t -> Kinded_parameter.t list

val extra_args
   : t
  -> Apply_cont_rewrite_id.t
  -> (Continuation_extra_params_and_args.Extra_arg.t * used) list

val original_params_arity : t -> Flambda_arity.With_subkinds.t

val does_nothing : t -> bool
