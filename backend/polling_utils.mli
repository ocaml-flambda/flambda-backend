(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*      Xavier Leroy and Damien Doligez, projet Cambium, INRIA Paris      *)
(*               Sadiq Jaffer, OCaml Labs Consultancy Ltd                 *)
(*          Stephen Dolan and Mark Shinwell, Jane Street Europe           *)
(*                                                                        *)
(*   Copyright 2021 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2021 OCaml Labs Consultancy Ltd                            *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Analyses related to the insertion of poll operations (elements common to both pipelines). *)

val function_is_assumed_to_never_poll : string -> bool

val is_disabled : string -> bool

type polling_point =
  | Alloc
  | Poll
  | Function_call
  | External_call

type error = Poll_error of Debuginfo.t * (polling_point * Debuginfo.t) list

exception Error of error

type polls_before_prtc =
  | Might_not_poll
  | Always_polls

module Polls_before_prtc : sig
  type t = polls_before_prtc

  val bot : t

  val join : t -> t -> t

  val lessequal : t -> t -> bool
end
