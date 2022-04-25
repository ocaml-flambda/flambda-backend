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

[@@@ocaml.warning "+a-30-40-41-42"]

(** Translation of Flambda primitives to Cmm. *)

val prim :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Debuginfo.t ->
  Flambda_primitive.t ->
  Cmm.expression
  * To_cmm_env.extra_info option
  * To_cmm_env.t
  * To_cmm_result.t
  * Effects_and_coeffects.t
