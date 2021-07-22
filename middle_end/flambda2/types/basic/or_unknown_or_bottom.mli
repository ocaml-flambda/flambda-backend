(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type 'a t =
  | Unknown
  | Ok of 'a
  | Bottom

val print
   : (Format.formatter -> 'a -> unit)
  -> Format.formatter
  -> 'a t
  -> unit

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

val map : 'a t -> f:('a -> 'b) -> 'b t

val map_sharing : 'a t -> f:('a -> 'a) -> 'a t

val of_or_unknown : 'a Or_unknown.t -> 'a t
