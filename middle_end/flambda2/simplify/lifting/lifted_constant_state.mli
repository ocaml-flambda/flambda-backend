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

module DE = Downwards_env
module LC = Lifted_constant

type t

val empty : t

val is_empty : t -> bool

val print : Format.formatter -> t -> unit

val singleton : LC.t -> t

val add : t -> LC.t -> t

val singleton_list_of_constants : LC.t list -> t

val union : t -> t -> t

val fold : t -> init:'a -> f:('a -> LC.t -> 'a) -> 'a

val all_defined_symbols : t -> Symbol.Set.t

val add_to_denv : ?maybe_already_defined:unit -> DE.t -> t -> DE.t

type sort_result = private { innermost_first : LC.t array }

val sort : t -> sort_result
