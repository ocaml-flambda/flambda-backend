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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = sig
  type t

  (** We don't have an interface that insists on adding continuations before
      seeing their uses. This would be problematic when inserting wrappers,
      where we have already advanced past the point at which such wrappers would
      need to be defined, before knowing that a wrapper is needed. *)

  val record_continuation_use :
    t ->
    Continuation.t ->
    Continuation_use_kind.t ->
    env_at_use:Downwards_env.t ->
    arg_types:Flambda_type.t list ->
    t * Apply_cont_rewrite_id.t

  val delete_continuation_uses : t -> Continuation.t -> t

  val get_typing_env_no_more_than_one_use :
    t -> Continuation.t -> Flambda_type.Typing_env.t option

  val num_continuation_uses : t -> Continuation.t -> int

  val all_continuations_used : t -> Continuation.Set.t
end
