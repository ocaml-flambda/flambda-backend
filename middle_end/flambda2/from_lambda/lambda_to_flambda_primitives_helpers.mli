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

type failure =
  | Division_by_zero
  | Index_out_of_bounds
  | Address_was_misaligned

type expr_primitive =
  | Simple of Simple.t
  | Nullary of Flambda_primitive.nullary_primitive
  | Unary of Flambda_primitive.unary_primitive * simple_or_prim
  | Binary of
      Flambda_primitive.binary_primitive * simple_or_prim * simple_or_prim
  | Ternary of
      Flambda_primitive.ternary_primitive
      * simple_or_prim
      * simple_or_prim
      * simple_or_prim
  | Variadic of Flambda_primitive.variadic_primitive * simple_or_prim list
  | Checked of
      { validity_conditions : expr_primitive list;
            (** The [validity_conditions] return untagged immediates
                representing boolean values. *)
        primitive : expr_primitive;
        failure : failure;
        (* Predefined exception *)
        dbg : Debuginfo.t
      }
  | If_then_else of
      expr_primitive
      * expr_primitive
      * expr_primitive
      * Flambda_kind.With_subkind.t list
  | Sequence of expr_primitive list
  | Unboxed_product of expr_primitive list

and simple_or_prim =
  | Simple of Simple.t
  | Prim of expr_primitive

val maybe_create_unboxed_product : expr_primitive list -> expr_primitive

val print_expr_primitive : Format.formatter -> expr_primitive -> unit

val print_simple_or_prim : Format.formatter -> simple_or_prim -> unit

val print_list_of_simple_or_prim :
  Format.formatter -> simple_or_prim list -> unit

val print_list_of_lists_of_simple_or_prim :
  Format.formatter -> simple_or_prim list list -> unit

open Closure_conversion_aux

val bind_recs :
  Acc.t ->
  Exn_continuation.t option ->
  register_const0:(Acc.t -> Static_const.t -> string -> Acc.t * Symbol.t) ->
  expr_primitive ->
  Debuginfo.t ->
  (Acc.t -> Flambda.Named.t list -> Expr_with_acc.t) ->
  Expr_with_acc.t
