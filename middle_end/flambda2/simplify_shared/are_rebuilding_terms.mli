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

(** Store information about whether terms are rebuilt during simplification,
    and if so, whether the full expression is being rebuilt (as is the case most
    of the time), or wheterh only part of the expressions if being rebuilt (as is
    the case for continuation specialization where a single handler is rebuilt). *)
type t

(** Print *)
val print : Format.formatter -> t -> unit

(** This function returns [true] iff we are **not** rebuilding terms during the
    upwards pass. *)
val not_rebuilding_terms : t -> bool

(** This function returns [true] iff we are rebuilding terms **and** we do so
    partially (i.e. in the context of continuation specialization). *)
val are_rebuilding_partially : t -> bool

(** The value for when we do **not** rebuild terms. *)
val rebuild_nothing : t

(** The value for when we rebuild everything (i.e. most of the time) *)
val rebuild_everything : t

(** The value for when we rebuild terms, but only partially, i.e. in the
    context of continuation specialization where only one handler is rebuilt. *)
val partial_rebuilding : t
