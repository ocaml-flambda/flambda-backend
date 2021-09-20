(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Maintenance of environments and associated calculations for common
    subexpression elimination, performed during Simplify. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module EPA = Continuation_extra_params_and_args
module BP = Bound_parameter
module P = Flambda_primitive
module RI = Apply_cont_rewrite_id
module T = Flambda_type
module TE = Flambda_type.Typing_env

type t

val print : Format.formatter -> t -> unit

val empty : t

(** If the [t] already has an equation for the given primitive, then [add] does
    nothing. (Expected usage is that this will correspond to outermost bindings
    taking precedence, but for simplicity, this function does not enforce
    that.) *)
val add : t -> P.Eligible_for_cse.t -> bound_to:Simple.t -> Scope.t -> t

val find : t -> P.Eligible_for_cse.t -> Simple.t option

module Join_result : sig
  type nonrec t = private
    { cse_at_join_point : t;
      extra_params : EPA.t;
      extra_equations : T.t Name.Map.t;
      extra_allowed_names : Name_occurrences.t
    }
end

(** [join] adds CSE equations into [cse_at_fork] at the next scope level after
    that given by the [typing_env_at_fork]. *)
val join :
  typing_env_at_fork:TE.t ->
  cse_at_fork:t ->
  use_info:'a list ->
  get_typing_env:('a -> TE.t) ->
  get_rewrite_id:('a -> RI.t) ->
  get_cse:('a -> t) ->
  params:BP.t list ->
  Join_result.t option
