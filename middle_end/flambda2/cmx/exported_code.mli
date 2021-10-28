(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t

include Contains_ids.S with type t := t

val apply_renaming : Code_id.t Code_id.Map.t -> Renaming.t -> t -> t

val print : Format.formatter -> t -> unit

val empty : t

val add_code : Code.t Code_id.Map.t -> t -> t

val mark_as_imported : t -> t

val merge : t -> t -> t

val mem : Code_id.t -> t -> bool

val find_code : t -> Code_id.t -> Code.t option

val find_code_if_not_imported : t -> Code_id.t -> Code.t option

val find_code_metadata : t -> Code_id.t -> Code_metadata.t

val remove_unreachable : t -> reachable_names:Name_occurrences.t -> t

val iter : t -> (Code_id.t -> Code.t -> unit) -> unit
