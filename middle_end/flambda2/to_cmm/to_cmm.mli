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
  make_symbol:(?unitname:string -> string option -> string) ->
  Flambda_unit.t ->
  all_code:Exported_code.t ->
  Cmm.phrase list * Exported_offsets.t
