(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Vincent Laviron, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2020 OCamlPro SAS                                          *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
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

val free_names : t -> Name_occurrences.t

val add_code : keep_code:(Code_id.t -> bool) -> Code.t Code_id.Map.t -> t -> t

val mark_as_imported : t -> t

val merge : t -> t -> t

val mem : Code_id.t -> t -> bool

(** This function raises an exception if the code ID is unbound. *)
val find_exn : t -> Code_id.t -> Code_or_metadata.t

(** This function is only really for use in unusual cases where there needs to
    be special handling if a code ID is unbound (see comment in the .ml file) *)
val find : t -> Code_id.t -> Code_or_metadata.t option

val remove_unreachable : reachable_names:Name_occurrences.t -> t -> t

val remove_unused_closure_vars_from_result_types_and_shortcut_aliases :
  used_closure_vars:Var_within_closure.Set.t ->
  canonicalise:(Simple.t -> Simple.t) ->
  t ->
  t

val iter_code : t -> f:(Code.t -> unit) -> unit
