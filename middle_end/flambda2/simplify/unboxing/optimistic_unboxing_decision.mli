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

(* Note that, if the continuation is recursive, we need to prevent generating
   decisions that unbox variants and closures, as we cannot be sure that
   reasonable extra_args can be computed for all use sites, since we have not
   yet seen all use sites at this point. For instance: *)
(*
 * let rec cont k x y =
 *   switch y with
 *   | 0 -> k (Some x)
 *   | 1 -> k (f x) (* for some function f in scope *)
 *)
(* In this case, even if we know that x is an option, to unbox it we'd need to
   introduce a switch in the `1` branch. This is: 1) not implemented (although
   technically possible) 2) not efficient or beneficial in most cases. *)
(** Unfold a continuation parameter's type into an unboxing decision tree. This
    does not take account of the types of the corresponding arguments at the
    continuation's use site(s). *)
val make_optimistic_decision :
  depth:int ->
  recursive:bool ->
  Flambda2_types.Typing_env.t ->
  param_type:Flambda2_types.t ->
  Unboxing_types.decision
