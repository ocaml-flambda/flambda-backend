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

val remove_var_with_provenance :
  To_cmm_env.free_vars -> Backend_var.With_provenance.t -> To_cmm_env.free_vars

val remove_vars_with_machtype :
  To_cmm_env.free_vars ->
  (Backend_var.With_provenance.t * Cmm.machtype) list ->
  To_cmm_env.free_vars

val exttype_of_kind : Flambda_kind.t -> Cmm.exttype

val machtype_of_kind : Flambda_kind.With_subkind.t -> Cmm.machtype

val extended_machtype_of_kind :
  Flambda_kind.With_subkind.t -> Cmm_helpers.Extended_machtype.t

val machtype_of_kinded_parameter : Bound_parameter.t -> Cmm.machtype

val memory_chunk_of_kind : Flambda_kind.With_subkind.t -> Cmm.memory_chunk

(** Create a constant int expression from a targetint. *)
val targetint : dbg:Debuginfo.t -> Targetint_32_64.t -> Cmm.expression

val tag_targetint : Targetint_32_64.t -> Targetint_32_64.t

val nativeint_of_targetint : Targetint_32_64.t -> Nativeint.t

(** This does not inline effectful expressions. *)
val name :
  To_cmm_env.t -> To_cmm_result.t -> Name.t -> To_cmm_env.translation_result

val const : dbg:Debuginfo.t -> Reg_width_const.t -> Cmm.expression

(** The default behaviour is to use
    [Flambda_features.Expert.inline_effects_in_cmm], which defaults to [false]
    if no command-line flag is given. *)
val simple :
  ?consider_inlining_effectful_expressions:bool ->
  dbg:Debuginfo.t ->
  To_cmm_env.t ->
  To_cmm_result.t ->
  Simple.t ->
  To_cmm_env.translation_result

val simple_static :
  To_cmm_result.t ->
  Simple.t ->
  [`Data of Cmm.data_item list | `Var of Variable.t]

(** This function translates the [Simple] at the head of the list first.
    Regarding [consider_inlining_effectful_expressions], see [simple] above. *)
val simple_list :
  ?consider_inlining_effectful_expressions:bool ->
  dbg:Debuginfo.t ->
  To_cmm_env.t ->
  To_cmm_result.t ->
  Simple.t list ->
  Cmm.expression list
  * To_cmm_env.free_vars
  * To_cmm_env.t
  * To_cmm_result.t
  * Effects_and_coeffects.t

val bound_parameters :
  To_cmm_env.t ->
  Bound_parameters.t ->
  To_cmm_env.t * (Backend_var.With_provenance.t * Cmm.machtype) list

val invalid :
  To_cmm_result.t -> message:string -> Cmm.expression * To_cmm_result.t

type update_kind =
  | Word_val
  | Word_int
  | Storage_single
  | Real_single
  | Double
  | Thirtytwo_signed
  | Onetwentyeight_unaligned

(** Make an update to a statically-allocated block. *)
val make_update :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Debuginfo.t ->
  update_kind ->
  symbol:Cmm.expression ->
  Variable.t ->
  index:int ->
  prev_updates:To_cmm_env.expr_with_info option ->
  To_cmm_env.t * To_cmm_result.t * To_cmm_env.expr_with_info option

val check_arity : _ Flambda_arity.t -> _ list -> bool

val extended_machtype_of_return_arity :
  [`Unarized] Flambda_arity.t -> Cmm_helpers.Extended_machtype.t
