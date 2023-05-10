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

(** Simplification functions on [Simple.t]. *)

(** This function is guaranteed to return an alias type. *)
val simplify_simple :
  Downwards_acc.t ->
  Simple.t ->
  min_name_mode:Name_mode.t ->
  Flambda2_types.t * Simple.t

val simplify_simple_if_in_scope :
  Downwards_acc.t ->
  Simple.t ->
  min_name_mode:Name_mode.t ->
  Flambda2_types.t option

type simplify_simples_result = private
  { simples : Simple.t list;
    simple_tys : Flambda2_types.t list
  }

val simplify_simples :
  Downwards_acc.t -> Simple.t list -> simplify_simples_result
