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

type t

val print : Format.formatter -> t -> unit

val empty : t

include Continuation_uses_env_intf.S with type t := t

val get_continuation_uses : t -> Continuation.t -> Continuation_uses.t option

val remove : t -> Continuation.t -> t

val clear_continuation_uses : t -> Continuation.t -> t

val union : t -> t -> t

val mark_non_inlinable : t -> t

(** Manually add a continuation use, with everything specified, including
    the id for the apply_cont (contrary to {!record_continuation_uses} which
    always generates a fresh id). Users of this function must take care to ensure
    there's only one use per id. *)
val add_continuation_use :
  t ->
  Continuation.t ->
  Continuation_use_kind.t ->
  id:Apply_cont_rewrite_id.t ->
  env_at_use:Downwards_env.t ->
  arg_types:Flambda2_types.t list ->
  t
