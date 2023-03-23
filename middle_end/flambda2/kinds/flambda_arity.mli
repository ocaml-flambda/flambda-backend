(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Arities are lists of kinds (with subkinds) used to describe things
    such as the kinding of function and continuation parameter lists. *)

type t

val nullary : t

val create : Flambda_kind.With_subkind.t list -> t

val to_list : t -> Flambda_kind.With_subkind.t list

val cardinal : t -> int

val is_singleton_value : t -> bool

val print : Format.formatter -> t -> unit

val equal_ignoring_subkinds : t -> t -> bool

(* It's usually a mistake to use this function, but it's needed for
   [Compare]. *)
val equal_exact : t -> t -> bool
