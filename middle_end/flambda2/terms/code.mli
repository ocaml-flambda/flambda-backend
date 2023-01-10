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

(** A piece of code, comprising of the parameters and body of a function,
    together with a field indicating whether the piece of code is a newer
    version of one that existed previously (and may still exist), for example
    after a round of simplification. *)
type t = Flambda.Function_params_and_body.t Code0.t

val code_metadata : t -> Code_metadata.t

val params_and_body : t -> Flambda.Function_params_and_body.t

include Code_metadata.Code_metadata_accessors_result_type with type 'a t := t

val create_with_metadata :
  params_and_body:Flambda.Function_params_and_body.t ->
  free_names_of_params_and_body:Name_occurrences.t ->
  code_metadata:Code_metadata.t ->
  t

val create :
  params_and_body:Flambda.Function_params_and_body.t ->
  free_names_of_params_and_body:Name_occurrences.t ->
  t Code_metadata.create_type

val with_code_id : Code_id.t -> t -> t

val with_params_and_body :
  params_and_body:Flambda.Function_params_and_body.t ->
  free_names_of_params_and_body:Name_occurrences.t ->
  cost_metrics:Cost_metrics.t ->
  t ->
  t

val with_newer_version_of : Code_id.t option -> t -> t

val print : Format.formatter -> t -> unit

include Contains_names.S with type t := t

val ids_for_export : t -> Ids_for_export.t

val map_result_types : t -> f:(Flambda2_types.t -> Flambda2_types.t) -> t

val free_names_of_params_and_body : t -> Name_occurrences.t
