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

[@@@ocaml.warning "+a-30-40-41-42"]

type t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val print : Format.formatter -> t -> unit

val create : Continuation.t list -> t

val rename : t -> t

val name_permutation : t -> guaranteed_fresh:t -> Renaming.t

val add_to_name_permutation :
  t -> guaranteed_fresh:t -> Renaming.t -> Renaming.t

val singleton_occurrence_in_terms : t -> Name_occurrences.t

val add_occurrence_in_terms : t -> Name_occurrences.t -> Name_occurrences.t
