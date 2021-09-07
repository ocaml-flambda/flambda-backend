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

type 'a t = Ok of 'a | Bottom

val print : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

val both : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

val map : 'a t -> f:('a -> 'b) -> 'b t

val value_map : 'a t -> bottom:'b -> f:('a -> 'b) -> 'b

val all : 'a t list -> 'a list t

val bind : 'a t -> f:('a -> 'b t) -> 'b t
