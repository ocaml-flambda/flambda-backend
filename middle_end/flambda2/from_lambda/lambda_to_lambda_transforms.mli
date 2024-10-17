(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2016--2024 OCamlPro SAS                                    *)
(*   Copyright 2016--2024 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type primitive_transform_result = private
  | Primitive of Lambda.primitive * Lambda.lambda list * Lambda.scoped_location
  | Transformed of Lambda.lambda

val rec_catch_for_while_loop :
  Lambda_to_flambda_env.t ->
  Lambda.lambda ->
  Lambda.lambda ->
  Lambda_to_flambda_env.t * Lambda.lambda

val rec_catch_for_for_loop :
  Lambda_to_flambda_env.t ->
  Lambda.scoped_location ->
  Ident.t ->
  Lambda.lambda ->
  Lambda.lambda ->
  Asttypes.direction_flag ->
  Lambda.lambda ->
  Lambda_to_flambda_env.t * Lambda.lambda

val switch_for_if_then_else :
  cond:Lambda.lambda ->
  ifso:Lambda.lambda ->
  ifnot:Lambda.lambda ->
  kind:Lambda.layout ->
  Lambda.lambda

val transform_primitive :
  Lambda_to_flambda_env.t ->
  Lambda.primitive ->
  Lambda.lambda list ->
  Lambda.scoped_location ->
  Lambda_to_flambda_env.t * primitive_transform_result
