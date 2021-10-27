(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

(** A piece of code, comprising of the parameters and body of a function,
    together with a field indicating whether the piece of code is a newer
    version of one that existed previously (and may still exist), for example
    after a round of simplification. *)
type t = Flambda.Function_params_and_body.t Code0.t

module Params_and_body_state = Code0.Params_and_body_state

val code_id : t -> Code_id.t

val params_and_body :
  t -> Flambda.Function_params_and_body.t Params_and_body_state.t

val newer_version_of : t -> Code_id.t option

val params_arity : t -> Flambda_arity.With_subkinds.t

val result_arity : t -> Flambda_arity.With_subkinds.t

val stub : t -> bool

val inline : t -> Inline_attribute.t

val is_a_functor : t -> bool

val recursive : t -> Recursive.t

val cost_metrics : t -> Cost_metrics.t

val inlining_arguments : t -> Inlining_arguments.t

val dbg : t -> Debuginfo.t

val is_tupled : t -> bool

val inlining_decision : t -> Function_decl_inlining_decision_type.t

val create :
  Code_id.t ->
  params_and_body:
    (Flambda.Function_params_and_body.t * Name_occurrences.t)
    Params_and_body_state.t ->
  newer_version_of:Code_id.t option ->
  params_arity:Flambda_arity.With_subkinds.t ->
  result_arity:Flambda_arity.With_subkinds.t ->
  stub:bool ->
  inline:Inline_attribute.t ->
  is_a_functor:bool ->
  recursive:Recursive.t ->
  cost_metrics:Cost_metrics.t ->
  inlining_arguments:Inlining_arguments.t ->
  dbg:Debuginfo.t ->
  is_tupled:bool ->
  inlining_decision:Function_decl_inlining_decision_type.t ->
  t

val with_code_id : Code_id.t -> t -> t

val with_params_and_body :
  (Flambda.Function_params_and_body.t * Name_occurrences.t)
  Params_and_body_state.t ->
  cost_metrics:Cost_metrics.t ->
  t ->
  t

val with_newer_version_of : Code_id.t option -> t -> t

val print : Format.formatter -> t -> unit

include Contains_names.S with type t := t

val all_ids_for_export : t -> Ids_for_export.t

(** See comment in code0.mli. *)
val make_non_inlinable : t -> t

val make_not_callable : t -> t

val is_non_callable : t -> bool
