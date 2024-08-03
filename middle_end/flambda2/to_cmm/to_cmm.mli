(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Translate Flambda compilation units into Cmm *)

(** Translate a compilation unit. *)
val unit :
  offsets:Exported_offsets.t ->
  all_code:Exported_code.t ->
  reachable_names:Name_occurrences.t ->
  code_ids_kept_for_zero_alloc:Code_id.Set.t ->
  Flambda_unit.t ->
  Cmm.phrase list
