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

[@@@ocaml.warning "+a-30-40-41-42"]

type 'function_params_and_body t

val code_metadata : _ t -> Code_metadata.t

val code_id : 'function_params_and_body t -> Code_id.t

val params_and_body : 'function_params_and_body t -> 'function_params_and_body

val newer_version_of : 'function_params_and_body t -> Code_id.t option

val params_arity : 'function_params_and_body t -> Flambda_arity.With_subkinds.t

val result_arity : 'function_params_and_body t -> Flambda_arity.With_subkinds.t

val stub : 'function_params_and_body t -> bool

val inline : 'function_params_and_body t -> Inline_attribute.t

val is_a_functor : 'function_params_and_body t -> bool

val recursive : 'function_params_and_body t -> Recursive.t

val cost_metrics : 'function_params_and_body t -> Cost_metrics.t

val inlining_arguments : 'function_params_and_body t -> Inlining_arguments.t

val dbg : 'function_params_and_body t -> Debuginfo.t

val is_tupled : 'function_params_and_body t -> bool

val is_my_closure_used : 'function_params_and_body t -> bool

val inlining_decision :
  'function_params_and_body t -> Function_decl_inlining_decision_type.t

val create :
  print_function_params_and_body:
    (Format.formatter -> 'function_params_and_body -> unit) ->
  Code_id.t (** needed for [compare], although useful otherwise too *) ->
  params_and_body:'function_params_and_body ->
  free_names_of_params_and_body:Name_occurrences.t ->
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
  'function_params_and_body t

val with_code_id :
  Code_id.t -> 'function_params_and_body t -> 'function_params_and_body t

val with_params_and_body :
  print_function_params_and_body:
    (Format.formatter -> 'function_params_and_body -> unit) ->
  params_and_body:'function_params_and_body ->
  free_names_of_params_and_body:Name_occurrences.t ->
  cost_metrics:Cost_metrics.t ->
  'function_params_and_body t ->
  'function_params_and_body t

val with_newer_version_of :
  Code_id.t option -> 'function_params_and_body t -> 'function_params_and_body t

val free_names : _ t -> Name_occurrences.t

val apply_renaming :
  apply_renaming_function_params_and_body:
    ('function_params_and_body -> Renaming.t -> 'function_params_and_body) ->
  'function_params_and_body t ->
  Renaming.t ->
  'function_params_and_body t

val print :
  print_function_params_and_body:
    (Format.formatter -> 'function_params_and_body -> unit) ->
  Format.formatter ->
  'function_params_and_body t ->
  unit

val all_ids_for_export :
  all_ids_for_export_function_params_and_body:
    ('function_params_and_body -> Ids_for_export.t) ->
  'function_params_and_body t ->
  Ids_for_export.t

val compare : 'function_params_and_body t -> 'function_params_and_body t -> int
