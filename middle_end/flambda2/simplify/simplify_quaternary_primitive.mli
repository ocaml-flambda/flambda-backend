(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Simplification of primitives taking four arguments. *)

val simplify_quaternary_primitive :
  Downwards_acc.t ->
  Flambda_primitive.t ->
  Flambda_primitive.quaternary_primitive ->
  arg1:Simple.t ->
  arg1_ty:Flambda2_types.t ->
  arg2:Simple.t ->
  arg2_ty:Flambda2_types.t ->
  arg3:Simple.t ->
  arg3_ty:Flambda2_types.t ->
  arg4:Simple.t ->
  arg4_ty:Flambda2_types.t ->
  Debuginfo.t ->
  result_var:Bound_var.t ->
  Simplify_primitive_result.t
