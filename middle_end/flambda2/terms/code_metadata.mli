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

type t

type code_metadata = t

module type Metadata_view_type = sig
  type 'a t

  val metadata : 'a t -> code_metadata
end

module type Code_metadata_accessors_result_type = sig
  type 'a t

  val code_id : 'a t -> Code_id.t

  val newer_version_of : 'a t -> Code_id.t option

  val params_arity : 'a t -> [`Complex] Flambda_arity.t

  val param_modes : 'a t -> Alloc_mode.For_types.t list

  (* Zero-indexed position of the first local param, to be able to determine the
     allocation modes of partial applications. If there is no local parameter,
     equal to the number of (complex) parameters. *)
  val first_complex_local_param : 'a t -> int

  val result_arity : 'a t -> [`Unarized] Flambda_arity.t

  val result_types : 'a t -> Result_types.t Or_unknown_or_bottom.t

  val result_mode : 'a t -> Lambda.alloc_mode

  val stub : 'a t -> bool

  val inline : 'a t -> Inline_attribute.t

  val check : 'a t -> Check_attribute.t

  val poll_attribute : 'a t -> Poll_attribute.t

  val is_a_functor : 'a t -> bool

  val recursive : 'a t -> Recursive.t

  val cost_metrics : 'a t -> Cost_metrics.t

  val inlining_arguments : 'a t -> Inlining_arguments.t

  val dbg : 'a t -> Debuginfo.t

  val is_tupled : 'a t -> bool

  val is_my_closure_used : 'a t -> bool

  val inlining_decision : 'a t -> Function_decl_inlining_decision_type.t

  val contains_no_escaping_local_allocs : 'a t -> bool

  val absolute_history : 'a t -> Inlining_history.Absolute.t

  val relative_history : 'a t -> Inlining_history.Relative.t

  val loopify : 'a t -> Loopify_attribute.t
end

module Code_metadata_accessors : functor (X : Metadata_view_type) ->
  Code_metadata_accessors_result_type with type 'a t := 'a X.t

include Code_metadata_accessors_result_type with type 'a t := t

type 'a create_type =
  Code_id.t ->
  newer_version_of:Code_id.t option ->
  params_arity:[`Complex] Flambda_arity.t ->
  param_modes:Alloc_mode.For_types.t list ->
  first_complex_local_param:int ->
  result_arity:[`Unarized] Flambda_arity.t ->
  result_types:Result_types.t Or_unknown_or_bottom.t ->
  result_mode:Lambda.alloc_mode ->
  contains_no_escaping_local_allocs:bool ->
  stub:bool ->
  inline:Inline_attribute.t ->
  check:Check_attribute.t ->
  poll_attribute:Poll_attribute.t ->
  is_a_functor:bool ->
  recursive:Recursive.t ->
  cost_metrics:Cost_metrics.t ->
  inlining_arguments:Inlining_arguments.t ->
  dbg:Debuginfo.t ->
  is_tupled:bool ->
  is_my_closure_used:bool ->
  inlining_decision:Function_decl_inlining_decision_type.t ->
  absolute_history:Inlining_history.Absolute.t ->
  relative_history:Inlining_history.Relative.t ->
  loopify:Loopify_attribute.t ->
  'a

val createk : (t -> 'a) -> 'a create_type

val create : t create_type

val with_code_id : Code_id.t -> t -> t

val with_newer_version_of : Code_id.t option -> t -> t

val with_cost_metrics : Cost_metrics.t -> t -> t

val print : Format.formatter -> t -> unit

(** [free_names] does not return occurrences of value slots inside the
    [result_types]. *)
include Contains_names.S with type t := t

val ids_for_export : t -> Ids_for_export.t

val approx_equal : t -> t -> bool

val map_result_types : t -> f:(Flambda2_types.t -> Flambda2_types.t) -> t
