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

(** Values of type ['a] must not contain names! *)

type 'a t = Const of 'a | Var of Variable.t

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int

val value_map : 'a t -> default:'b -> f:('a -> 'b) -> 'b

val free_names : _ t -> Name_occurrences.t

val apply_renaming : 'a t -> Renaming.t -> 'a t
