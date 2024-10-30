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

val are_rebuilding : t -> bool
val are_not_rebuilding : t -> bool
val are_rebuilding_partially : t -> bool

val rebuild_nothing : t
val rebuild_everything : t
val partial_rebuilding : t

