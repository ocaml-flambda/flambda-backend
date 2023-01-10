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

type 'function_params_and_body t

val code_metadata : _ t -> Code_metadata.t

val params_and_body : 'function_params_and_body t -> 'function_params_and_body

include
  Code_metadata.Code_metadata_accessors_result_type
    with type 'function_params_and_body t := 'function_params_and_body t

val create_with_metadata :
  print_function_params_and_body:
    (Format.formatter -> 'function_params_and_body -> unit) ->
  params_and_body:'function_params_and_body ->
  free_names_of_params_and_body:Name_occurrences.t ->
  code_metadata:Code_metadata.t ->
  'function_params_and_body t

val create :
  print_function_params_and_body:
    (Format.formatter -> 'function_params_and_body -> unit) ->
  params_and_body:'function_params_and_body ->
  free_names_of_params_and_body:Name_occurrences.t ->
  'function_params_and_body t Code_metadata.create_type

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

val ids_for_export :
  ids_for_export_function_params_and_body:
    ('function_params_and_body -> Ids_for_export.t) ->
  'function_params_and_body t ->
  Ids_for_export.t

val compare : 'function_params_and_body t -> 'function_params_and_body t -> int

val map_result_types :
  'function_params_and_body t ->
  f:(Flambda2_types.t -> Flambda2_types.t) ->
  'function_params_and_body t

val free_names_of_params_and_body :
  'function_params_and_body t -> Name_occurrences.t
