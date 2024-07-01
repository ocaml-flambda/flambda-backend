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

(** The identifiers (implicit and explicit function parameters, together with
    return and exception continuations) bound at a lambda in the term language. *)

type t

val create :
  return_continuation:Continuation.t ->
  exn_continuation:Continuation.t ->
  params:Bound_parameters.t ->
  my_closure:Variable.t ->
  my_region:Variable.t ->
  my_ghost_region:Variable.t ->
  my_depth:Variable.t ->
  t

val return_continuation : t -> Continuation.t

val exn_continuation : t -> Continuation.t

val params : t -> Bound_parameters.t

val my_closure : t -> Variable.t

val my_region : t -> Variable.t

val my_ghost_region : t -> Variable.t

val my_depth : t -> Variable.t

include Bindable.S with type t := t
