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

[@@@ocaml.warning "+a-30-40-41-42"]

type t

include Contains_names.S with type t := t

include Contains_ids.S with type t := t

val print : Format.formatter -> t -> unit

val create :
  return_continuation:Continuation.t ->
  exn_continuation:Exn_continuation.t ->
  params:Kinded_parameter.t list ->
  my_closure:Variable.t ->
  my_depth:Variable.t ->
  t

val return_continuation : t -> Continuation.t

val exn_continuation : t -> Exn_continuation.t

val params : t -> Kinded_parameter.t list

val my_closure : t -> Variable.t

val my_depth : t -> Variable.t

val rename : t -> t

val name_permutation : t -> guaranteed_fresh:t -> Renaming.t
