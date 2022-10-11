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

(** Simplification of statically-allocated constants bound to symbols. *)

open! Flambda

val simplify_static_consts :
  Downwards_acc.t ->
  Bound_static.t ->
  Static_const_group.t ->
  simplify_function_body:Simplify_common.simplify_function_body ->
  Bound_static.t * Rebuilt_static_const.Group.t * Downwards_acc.t
