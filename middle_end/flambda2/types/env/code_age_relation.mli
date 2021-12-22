(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** Tracking of new versions of code such that it can be determined, for any two
    pieces of code, which one is newer (or that the pieces of code are
    unrelated). *)

(* CR-someday lwhite/mshinwell: Perhaps inlining benefit could be attached to
   the edges of this graph *)

type t

val print : Format.formatter -> t -> unit

val empty : t

val add : t -> newer:Code_id.t -> older:Code_id.t -> t

val get_older_version_of : t -> Code_id.t -> Code_id.t option

(** [meet] calculates which of the given pieces of code is newer, or identifies
    that the pieces of code are unrelated. *)
val meet :
  t ->
  resolver:(Compilation_unit.t -> t option) ->
  Code_id.t ->
  Code_id.t ->
  Code_id.t Or_bottom.t

(** [join] calculates the newest common ancestor of the given pieces of code, or
    identifies that the pieces of code are unrelated. *)
val join :
  target_t:t ->
  resolver:(Compilation_unit.t -> t option) ->
  t ->
  t ->
  Code_id.t ->
  Code_id.t ->
  Code_id.t Or_unknown.t

val union : t -> t -> t

val all_code_ids_for_export : t -> Code_id.Set.t

val apply_renaming : t -> Renaming.t -> t

val clean_for_export : t -> reachable_names:Name_occurrences.t -> t
