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

(** Code without any function bodies, but with all the associated metadata, e.g.
    free names. *)

type t = unit Code0.t

val code_metadata : t -> Code_metadata.t

include Code_metadata.Code_metadata_accessors_result_type with type 'a t := t

val create_with_metadata :
  free_names_of_params_and_body:Name_occurrences.t ->
  code_metadata:Code_metadata.t ->
  t

val create :
  free_names_of_params_and_body:Name_occurrences.t ->
  t Code_metadata.create_type

include Contains_names.S with type t := t

val print : Format.formatter -> t -> unit
