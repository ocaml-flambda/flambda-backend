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

(** Helper functions for Flambda 2 to Cmm translation. These functions, unlike
    the ones in [Cmm_helpers], depend on Flambda 2 data types. *)

(** {2 Useful misc values} *)

val unsupported_32_bits : unit -> 'a

(** [arch32] is [true] iff we are compiling for a 32-bit target. *)
val arch32 : bool

(** [arch64] is [true] iff we are compiling for a 64-bit target. *)
val arch64 : bool

val exttype_of_kind : Flambda_kind.t -> Cmm.exttype

(** {2 Kinds and types} *)

val check_arity : Flambda_arity.With_subkinds.t -> _ list -> bool

val machtype_of_kind : Flambda_kind.t -> Cmm.machtype_component array

val machtype_of_kinded_parameter :
  Bound_parameter.t -> Cmm.machtype_component array

(** {2 Cmm values} *)

(** Create a constant int expression from a targetint. *)
val targetint : ?dbg:Debuginfo.t -> Targetint_32_64.t -> Cmm.expression

(** {2 Block creation} *)

(** Create an array using the given fields. *)
val make_array :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.Array_kind.t ->
  Alloc_mode.t ->
  Cmm.expression list ->
  Cmm.expression

(** Create a block using the given fields. *)
val make_block :
  ?dbg:Debuginfo.t ->
  Flambda_primitive.Block_kind.t ->
  Alloc_mode.t ->
  Cmm.expression list ->
  Cmm.expression

(** Create a closure block. *)
val make_closure_block :
  ?dbg:Debuginfo.t -> Alloc_mode.t -> Cmm.expression list -> Cmm.expression

val convert_alloc_mode : Alloc_mode.t -> Lambda.alloc_mode

(** {2 Numeric conversions} *)

val tag_targetint : Targetint_32_64.t -> Targetint_32_64.t

val targetint_of_imm : Targetint_31_63.t -> Targetint_32_64.t

val nativeint_of_targetint : Targetint_32_64.t -> Nativeint.t

(** {2 [Simple]s, constants, etc.} *)

val symbol_from_linkage_name :
  ?dbg:Debuginfo.t -> Linkage_name.t -> Cmm.expression

val symbol : ?dbg:Debuginfo.t -> Symbol.t -> Cmm.expression

val name :
  To_cmm_env.t ->
  Name.t ->
  Cmm.expression * To_cmm_env.t * Effects_and_coeffects.t

val const : To_cmm_env.t -> Reg_width_const.t -> Cmm.expression

val simple :
  To_cmm_env.t ->
  Simple.t ->
  Cmm.expression * To_cmm_env.t * Effects_and_coeffects.t

val arg_list :
  To_cmm_env.t ->
  Simple.t list ->
  Cmm.expression list * To_cmm_env.t * Effects_and_coeffects.t

val param_list :
  To_cmm_env.t ->
  Bound_parameters.t ->
  To_cmm_env.t * (Backend_var.With_provenance.t * Cmm.machtype) list

(** {2 Expression combinators} *)

(** Translate the raise kind found on Pop trap actions into a Cmm raise kind *)
val raise_kind : Trap_action.raise_kind option -> Lambda.raise_kind

val invalid :
  To_cmm_result.t -> message:string -> Cmm.expression * To_cmm_result.t

(** Make an update to a statically-allocated block. *)
val make_update :
  To_cmm_env.t ->
  Cmm.memory_chunk ->
  symbol:Cmm.expression ->
  Variable.t ->
  index:int ->
  prev_updates:Cmm.expression option ->
  To_cmm_env.t * Cmm.expression option

val simple_static :
  To_cmm_env.t ->
  Simple.t ->
  To_cmm_env.t * [`Data of Cmm.data_item list | `Var of Variable.t]
