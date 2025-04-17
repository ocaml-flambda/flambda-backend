(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Nathanaëlle Courant, Pierre Chambart, OCamlPro               *)
(*                                                                        *)
(*   Copyright 2024 OCamlPro SAS                                          *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type continuation_handler =
  { handler : Flambda.Continuation_handler.t;
    free_names : Name_occurrences.t
  }

type continuation_handlers =
  { handlers : Flambda.Continuation_handler.t Continuation.Lmap.t;
    free_names : Name_occurrences.t
  }

type t =
  { expr : Flambda.expr;
    free_names : Name_occurrences.t
  }

val create_let : Bound_pattern.t -> Flambda.named -> body:t -> t

val create_continuation_handler :
  Bound_parameters.t ->
  handler:t ->
  is_exn_handler:bool ->
  is_cold:bool ->
  continuation_handler

val create_continuation_handlers :
  continuation_handler Continuation.Lmap.t -> continuation_handlers

val create_non_recursive_let_cont :
  Continuation.t -> continuation_handler -> body:t -> t

val create_recursive_let_cont :
  invariant_params:Bound_parameters.t ->
  continuation_handler Continuation.Lmap.t ->
  body:t ->
  t

val from_expr : expr:Flambda.expr -> free_names:Name_occurrences.t -> t
