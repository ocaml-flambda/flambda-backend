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

(** Code without any function bodies, but with all the associated metadata, e.g.
    free names. *)

type t = unit Code0.t

val code_id : t -> Code_id.t

val is_non_callable : t -> bool

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

val is_my_closure_used : t -> bool

val inlining_decision : t -> Function_decl_inlining_decision_type.t

val create :
  Code_id.t ->
  free_names_of_params_and_body:Name_occurrences.t Code.Params_and_body_state.t ->
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
  is_my_closure_used:bool ->
  inlining_decision:Function_decl_inlining_decision_type.t ->
  t

include Contains_names.S with type t := t

val print : Format.formatter -> t -> unit

val make_not_callable : t -> t
