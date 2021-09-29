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

(** Simplification of the right-hand sides of [Let] bindings. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

val simplify_named :
  Downwards_acc.t ->
  Bound_pattern.t ->
  Flambda.Named.t ->
  simplify_toplevel:Simplify_common.simplify_toplevel ->
  Simplify_named_result.t * Removed_operations.t
