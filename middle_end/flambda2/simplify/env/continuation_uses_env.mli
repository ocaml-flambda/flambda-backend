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

type t

(** Print a downwards accumulator to a formatter. *)

val print : Format.formatter -> t -> unit

val empty : t

include Continuation_uses_env_intf.S with type t := t

(* CR mshinwell: refine interface *)
val get_uses : t -> Continuation_uses.t Continuation.Map.t

val remove : t -> Continuation.t -> t

val union : t -> t -> t
