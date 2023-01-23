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

open! Simplify_import

module Decisions : sig
  type t

  val print : Format.formatter -> t -> unit
end

val make_do_not_unbox_decisions : Bound_parameters.t -> Decisions.t

type continuation_arg_types =
  | Recursive
  | Non_recursive of Continuation_uses.arg_types_by_use_id

val make_decisions :
  continuation_arg_types:continuation_arg_types ->
  DE.t ->
  Bound_parameters.t ->
  T.t list ->
  DE.t * Decisions.t

val compute_extra_params_and_args :
  Decisions.t ->
  arg_types_by_use_id:Continuation_uses.arg_types_by_use_id ->
  EPA.t ->
  EPA.t
