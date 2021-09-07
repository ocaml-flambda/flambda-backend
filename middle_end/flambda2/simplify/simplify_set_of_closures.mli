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

(** Simplification of recursive groups of sets of closures. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Simplify_import

(** Simplify a single, non-lifted set of closures, as may occur on the
    right-hand side of a [Let] binding. *)
val simplify_non_lifted_set_of_closures :
  Downwards_acc.t ->
  Bindable_let_bound.t ->
  Set_of_closures.t ->
  simplify_toplevel:Simplify_common.simplify_toplevel ->
  Simplify_named_result.t

(** Simplify a group of possibly-recursive sets of closures, as may occur on the
    right-hand side of a [Let_symbol] binding. *)
val simplify_lifted_sets_of_closures :
  Downwards_acc.t ->
  all_sets_of_closures_and_symbols:
    (Symbol.t Closure_id.Lmap.t * Set_of_closures.t) list ->
  closure_bound_names_all_sets:Name_in_binding_pos.t Closure_id.Map.t list ->
  simplify_toplevel:Simplify_common.simplify_toplevel ->
  Bound_symbols.t * Rebuilt_static_const.Group.t * Downwards_acc.t
