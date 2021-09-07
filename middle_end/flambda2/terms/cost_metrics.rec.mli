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

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Cost metrics are a group of metrics tracking the impact of simplifying an
    expression. One of these is an approximation of the size of the generated
    machine code for this expression. It also tracks the number of operations
    that should have been executed but were removed by the simplifier.*)

type t

(* It's best to avoid calling this function too much as it is quite slow. *)
val expr_size : find_code:(Code_id.t -> Code.t) -> Expr.t -> Code_size.t

val zero : t

val from_size : Code_size.t -> t

val size : t -> Code_size.t

val print : Format.formatter -> t -> unit

val ( + ) : t -> t -> t

type code_characteristics = { cost_metrics : t; params_arity : int }

val set_of_closures :
  find_code_characteristics:(Code_id.t -> code_characteristics) ->
  Set_of_closures.t ->
  t

val increase_due_to_let_expr :
  is_phantom:bool -> cost_metrics_of_defining_expr:t -> t

val increase_due_to_let_cont_non_recursive : cost_metrics_of_handler:t -> t

val increase_due_to_let_cont_recursive : cost_metrics_of_handlers:t -> t

val notify_added : code_size:Code_size.t -> t -> t

val notify_removed : operation:Removed_operations.t -> t -> t

val evaluate : args:Inlining_arguments.t -> t -> float
