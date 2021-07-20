(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t = Mutable | Immutable | Immutable_unique
(** Mutable means that contents may vary at any moment.
    Immutable means that not only contents will never vary,
    but we're also allowed to share or duplicate identical values at will.
    Immutable_unique means that the contents will never vary,
    but physical equality is meaningful so the value must not be duplicated,
    nor shared. *)

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

val join : t -> t -> t

val to_lambda : t -> Asttypes.mutable_flag

val is_mutable : t -> bool
