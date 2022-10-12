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

type t

val create :
  params:Bound_parameters.t ->
  results:Bound_parameters.t ->
  Flambda2_types.Typing_env_extension.With_extra_variables.t ->
  t

val pattern_match :
  t ->
  f:
    (params:Bound_parameters.t ->
    results:Bound_parameters.t ->
    Flambda2_types.Typing_env_extension.With_extra_variables.t ->
    'a) ->
  'a

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val print : Format.formatter -> t -> unit

val map_result_types : t -> f:(Flambda2_types.t -> Flambda2_types.t) -> t
