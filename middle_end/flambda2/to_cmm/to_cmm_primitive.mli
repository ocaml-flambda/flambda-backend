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

(** Translation of Flambda primitives to Cmm. *)

val trans_prim : To_cmm_env.t To_cmm_env.trans_prim

val prim_simple :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Debuginfo.t ->
  Flambda_primitive.t ->
  To_cmm_env.simple To_cmm_env.bound_expr
  * To_cmm_env.extra_info option
  * To_cmm_env.t
  * To_cmm_result.t
  * Effects_and_coeffects.t

val prim_complex :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Debuginfo.t ->
  Flambda_primitive.t ->
  To_cmm_env.complex To_cmm_env.bound_expr
  * To_cmm_env.t
  * To_cmm_result.t
  * Effects_and_coeffects.t
