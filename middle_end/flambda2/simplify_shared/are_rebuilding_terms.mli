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

(** Flag indicating whether terms are being rebuilt during simplification. This
    is not just [bool] to enforce that the setting in [DE] is used everywhere. *)

type t

val print : Format.formatter -> t -> unit

(* CR gbury: the terms/names used for creating values and for inspecting them
   would make more sens if they were swapped. *)
val are_rebuilding : t

val are_not_rebuilding : t

val do_rebuild_terms : t -> bool

val do_not_rebuild_terms : t -> bool
