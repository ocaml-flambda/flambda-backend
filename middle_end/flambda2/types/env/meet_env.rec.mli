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

(** Environments used for meet operations. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val print : Format.formatter -> t -> unit

val create
   : Typing_env.t
  -> t

val env : t -> Typing_env.t

(** Note that we are now in the process of meeting the given two
    [Simple]s. *)
val now_meeting : t -> Simple.t -> Simple.t -> t

(** Determine whether we are now in the process of meeting the given two
    [Simple]s.  The arguments do not have to be provided in the same order
    as when [now_meeting] was called. *)
val already_meeting : t -> Simple.t -> Simple.t -> bool

(* val with_typing_env : t -> Typing_env.t -> t *)
