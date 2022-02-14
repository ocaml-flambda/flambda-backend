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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

val have_simplified_to_zero_terms : Downwards_acc.t -> t

(** Note that even though there is one term, the binding might contain multiple
    bound variables, in the case of a set of closures. *)
val have_simplified_to_single_term :
  Downwards_acc.t ->
  Bound_pattern.t ->
  Simplified_named.t ->
  original_defining_expr:Flambda.Named.t ->
  t

val have_lifted_set_of_closures :
  Downwards_acc.t ->
  Symbol.t Bound_var.Map.t ->
  original_defining_expr:Flambda.Named.t ->
  t

type descr = private
  | Zero_terms
  | Single_term of
      { let_bound : Bound_pattern.t;
        simplified_defining_expr : Simplified_named.t;
        original_defining_expr : Flambda.Named.t
      }
  | Multiple_bindings_to_symbols of
      { bound_vars_to_symbols : Symbol.t Bound_var.Map.t;
        original_defining_expr : Flambda.Named.t
      }

val descr : t -> descr

val dacc : t -> Downwards_acc.t

type binding_to_place =
  { let_bound : Bound_pattern.t;
    simplified_defining_expr : Simplified_named.t;
    original_defining_expr : Flambda.Named.t
  }

val bindings_to_place_in_any_order : t -> binding_to_place list

val with_dacc : dacc:Downwards_acc.t -> t -> t

val is_invalid : t -> bool
