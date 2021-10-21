(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module DE = Downwards_env
module LC = Lifted_constant

type t

val empty : t

val is_empty : t -> bool

val print : Format.formatter -> t -> unit

val singleton : LC.t -> t

(* Use if the order of constants doesn't matter. *)
val add : t -> LC.t -> t

val add_innermost : t -> LC.t -> t

val add_outermost : t -> LC.t -> t

val singleton_sorted_array_of_constants : innermost_first:LC.t array -> t

(* Use if the order of constants doesn't matter. *)
val union : t -> t -> t

val union_ordered : innermost:t -> outermost:t -> t

(* Use if the order of constants doesn't matter. *)
val fold : t -> init:'a -> f:('a -> LC.t -> 'a) -> 'a

val fold_outermost_first : t -> init:'a -> f:('a -> LC.t -> 'a) -> 'a

val fold_innermost_first : t -> init:'a -> f:('a -> LC.t -> 'a) -> 'a

val all_defined_symbols : t -> Symbol.Set.t

val add_to_denv : ?maybe_already_defined:unit -> DE.t -> t -> DE.t

val add_singleton_to_denv : DE.t -> LC.t -> DE.t

val add_list_to_denv : DE.t -> LC.t list -> DE.t

val sort : t -> t
