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

(** Helper functions and values for Flambda 2 to Cmm translation. Functions in
    this module, unlike the ones in [Cmm_helpers], depend on Flambda 2 data
    types. *)

val unsupported_32_bit : unit -> 'a

val exttype_of_kind : Flambda_kind.t -> Cmm.exttype

val machtype_of_kind : Flambda_kind.t -> Cmm.machtype_component array

val machtype_of_kinded_parameter :
  Bound_parameter.t -> Cmm.machtype_component array

(** Create a constant int expression from a targetint. *)
val targetint : dbg:Debuginfo.t -> Targetint_32_64.t -> Cmm.expression

val tag_targetint : Targetint_32_64.t -> Targetint_32_64.t

val nativeint_of_targetint : Targetint_32_64.t -> Nativeint.t

(* Only used to be used for symbols without offsets *)
val symbol_from_linkage_name :
  dbg:Debuginfo.t -> Linkage_name.t -> Cmm.expression

val symbol : To_cmm_result.t -> dbg:Debuginfo.t -> Symbol.t -> Cmm.expression

val name :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Name.t ->
  Cmm.expression * To_cmm_env.t * Effects_and_coeffects.t

val const : dbg:Debuginfo.t -> Reg_width_const.t -> Cmm.expression

val simple :
  dbg:Debuginfo.t ->
  To_cmm_env.t ->
  To_cmm_result.t ->
  Simple.t ->
  Cmm.expression * To_cmm_env.t * Effects_and_coeffects.t

val simple_static :
  To_cmm_result.t ->
  Simple.t ->
  [`Data of Cmm.data_item list | `Var of Variable.t]

val simple_list :
  dbg:Debuginfo.t ->
  To_cmm_env.t ->
  To_cmm_result.t ->
  Simple.t list ->
  Cmm.expression list * To_cmm_env.t * Effects_and_coeffects.t

val bound_parameters :
  To_cmm_env.t ->
  Bound_parameters.t ->
  To_cmm_env.t * (Backend_var.With_provenance.t * Cmm.machtype) list

val invalid :
  To_cmm_result.t -> message:string -> Cmm.expression * To_cmm_result.t

(** Make an update to a statically-allocated block. *)
val make_update :
  To_cmm_env.t ->
  Debuginfo.t ->
  Cmm.memory_chunk ->
  symbol:Cmm.expression ->
  Variable.t ->
  index:int ->
  prev_updates:Cmm.expression option ->
  To_cmm_env.t * Cmm.expression option
