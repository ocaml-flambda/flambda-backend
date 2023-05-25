(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Flambda.Import

type t

val create : Downwards_acc.t -> Expr_builder.binding_to_place list -> t

val create_have_lifted_set_of_closures :
  Downwards_acc.t ->
  (Bound_var.t * Symbol.t) list ->
  original_defining_expr:Named.t ->
  t

val dacc : t -> Downwards_acc.t

val bindings_to_place : t -> Expr_builder.binding_to_place list

val no_bindings : t -> bool

val was_lifted_set_of_closures : t -> bool

val with_dacc : dacc:Downwards_acc.t -> t -> t
