(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t

val create :
  params:Bound_parameter.t list ->
  results:
    (Bound_parameter.t
    * Flambda2_types.Typing_env_extension.With_extra_variables.t)
    list ->
  t

val create_default :
  params:Bound_parameter.t list ->
  result_arity:Flambda_arity.With_subkinds.t ->
  t

val pattern_match :
  t ->
  f:
    (params:Bound_parameter.t list ->
    results:
      (Bound_parameter.t
      * Flambda2_types.Typing_env_extension.With_extra_variables.t)
      list ->
    'a) ->
  'a

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val print : Format.formatter -> t -> unit

val map_result_types : t -> f:(Flambda2_types.t -> Flambda2_types.t) -> t
