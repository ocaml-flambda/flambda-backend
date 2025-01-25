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

val const_static : Reg_width_const.t -> Cmm.data_item list

val remove_skipped_params :
  (Backend_var.With_provenance.t * Cmm.machtype To_cmm_env.param_type) list ->
  (Backend_var.With_provenance.t * Cmm.machtype) list

val remove_skipped_args : 'a list -> _ To_cmm_env.param_type list -> 'a list

val remove_var_with_provenance :
  To_cmm_env.free_vars -> Backend_var.With_provenance.t -> To_cmm_env.free_vars

val remove_vars_with_machtype :
  To_cmm_env.free_vars ->
  (Backend_var.With_provenance.t * _) list ->
  To_cmm_env.free_vars

val exttype_of_kind : Flambda_kind.t -> Cmm.exttype

val machtype_of_kind : Flambda_kind.With_subkind.t -> Cmm.machtype

val extended_machtype_of_kind :
  Flambda_kind.With_subkind.t -> Cmm_helpers.Extended_machtype.t

val machtype_of_kinded_parameter : Bound_parameter.t -> Cmm.machtype

val param_machtype_of_kinded_parameter :
  Bound_parameter.t -> Cmm.machtype To_cmm_env.param_type

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
  [> `Static_data of Cmm.data_item list | `Var of Variable.t]

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

val continuation_bound_parameters :
  To_cmm_env.t ->
  Bound_parameters.t ->
  To_cmm_env.t
  * (Backend_var.With_provenance.t * Cmm.machtype To_cmm_env.param_type) list

val function_bound_parameters :
  To_cmm_env.t ->
  Bound_parameters.t ->
  To_cmm_env.t * (Backend_var.With_provenance.t * Cmm.machtype) list

val invalid :
  To_cmm_result.t -> message:string -> Cmm.expression * To_cmm_result.t

module Update_kind : sig
  type t

  (** The number of words written when applying this update to a block. *)
  val field_size_in_words : t -> int

  val pointers : t

  val tagged_immediates : t

  (** Tightly packed; the byte offset is [index * 4].  ([index] is as for
      [make_update], below.) *)
  val naked_int32s : t

  (** Assumes each field is a word; the byte offset is [index * size_addr]. *)
  val naked_int32_fields : t

  (** Tightly packed; the byte offset is [index * 8]. *)
  val naked_int64s : t

  (** Tightly packed; the byte offset is [index * size_float]. *)
  val naked_floats : t

  (** Tightly packed; the byte offset is [index * 4]. *)
  val naked_float32s : t

  (** Assumes each field is a word; the byte offset is [index * size_addr]. *)
  val naked_float32_fields : t

  (** Tightly packed (two words each); the byte offset is [index * 16]. *)
  val naked_vec128s : t

  (** Assumes each field is a word; the byte offset is [index * size_addr].
      Note that in this case the index is still based on word-width fields! *)
  val naked_vec128_fields : t
end

(** Make an update to a statically-allocated block. *)
val make_update :
  To_cmm_env.t ->
  To_cmm_result.t ->
  Debuginfo.t ->
  Update_kind.t ->
  symbol:Cmm.expression ->
  Variable.t ->
  index:int ->
  prev_updates:To_cmm_env.expr_with_info option ->
  To_cmm_env.t * To_cmm_result.t * To_cmm_env.expr_with_info option

val check_arity : _ Flambda_arity.t -> _ list -> bool

val extended_machtype_of_return_arity :
  [`Unarized] Flambda_arity.t -> Cmm_helpers.Extended_machtype.t

val alloc_mode_for_applications_to_cmx :
  Alloc_mode.For_applications.t -> Cmx_format.alloc_mode

val alloc_mode_for_allocations_to_cmm :
  Alloc_mode.For_allocations.t -> Cmm.Alloc_mode.t
