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

val machtype_of_kinded_parameter :
  Bound_parameter.t -> Cmm.machtype_component array

(** Create a constant int expression from a targetint. *)
val targetint : dbg:Debuginfo.t -> Targetint_32_64.t -> Cmm.expression

val tag_targetint : Targetint_32_64.t -> Targetint_32_64.t

val nativeint_of_targetint : Targetint_32_64.t -> Nativeint.t

val symbol_from_linkage_name :
  dbg:Debuginfo.t -> Linkage_name.t -> Cmm.expression

val symbol : dbg:Debuginfo.t -> Symbol.t -> Cmm.expression

(** This does not inline effectful expressions. *)
val name :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Name.t ->
  Cmm.expression
  * Cmm.machtype
  * To_cmm_env.t
  * To_cmm_result.t
  * Effects_and_coeffects.t

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
  Cmm.expression * To_cmm_env.t * To_cmm_result.t * Effects_and_coeffects.t

val simple_with_machtype :
  ?consider_inlining_effectful_expressions:bool ->
  dbg:Debuginfo.t ->
  To_cmm_env.t ->
  To_cmm_result.t ->
  Simple.t ->
  Cmm.expression
  * Cmm.machtype
  * To_cmm_env.t
  * To_cmm_result.t
  * Effects_and_coeffects.t

val simple_static :
  Simple.t -> [`Data of Cmm.data_item list | `Var of Variable.t]

(** This function translates the [Simple] at the head of the list first.
    Regarding [consider_inlining_effectful_expressions], see [simple] above. *)
val simple_list :
  ?consider_inlining_effectful_expressions:bool ->
  dbg:Debuginfo.t ->
  To_cmm_env.t ->
  To_cmm_result.t ->
  Simple.t list ->
  Cmm.expression list * To_cmm_env.t * To_cmm_result.t * Effects_and_coeffects.t

val simple_with_machtype_list :
  ?consider_inlining_effectful_expressions:bool ->
  dbg:Debuginfo.t ->
  To_cmm_env.t ->
  To_cmm_result.t ->
  Simple.t list ->
  (Cmm.expression * Cmm.machtype) list
  * To_cmm_env.t
  * To_cmm_result.t
  * Effects_and_coeffects.t

val bound_parameters :
  To_cmm_env.t ->
  Bound_parameters.t ->
  To_cmm_env.t * (Backend_var.With_provenance.t * Cmm.machtype) list

val invalid :
  To_cmm_result.t -> message:string -> Cmm.expression * To_cmm_result.t

(** Make an update to a statically-allocated block. *)
val make_update :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Debuginfo.t ->
  Cmm.memory_chunk ->
  symbol:Cmm.expression ->
  Variable.t ->
  index:int ->
  prev_updates:Cmm.expression option ->
  To_cmm_env.t * To_cmm_result.t * Cmm.expression option

val check_arity : Flambda_arity.With_subkinds.t -> _ list -> bool

val machtype_of_return_arity : Flambda_arity.t -> Cmm.machtype
