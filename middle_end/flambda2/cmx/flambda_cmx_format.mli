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

(** Contents of middle-end-specific portion of .cmx files when using Flambda. *)

type t

type raw

val to_raw : t -> raw * Flambda_backend_utils.File_sections.t

val from_raw : sections:Flambda_backend_utils.File_sections.t -> raw -> t

val create :
  final_typing_env:Flambda2_types.Typing_env.Serializable.t ->
  all_code:Exported_code.t ->
  exported_offsets:Exported_offsets.t ->
  used_value_slots:Value_slot.Set.t ->
  t

val import_typing_env_and_code :
  t ->
  in_current_dir:In_current_dir.t ->
  Flambda2_types.Typing_env.Serializable.t * Exported_code.t

val exported_offsets : t -> Exported_offsets.t

val with_exported_offsets : t -> Exported_offsets.t -> t

(** Aggregate several cmx into one for packs *)
val merge : t option -> t option -> t option

(** For ocamlobjinfo *)
val print : Format.formatter -> t -> unit
