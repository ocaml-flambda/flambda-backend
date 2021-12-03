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

(** Conversions of basic Lambda data types to their Flambda equivalents. *)

val value_kind : Lambda.value_kind -> Flambda_kind.With_subkind.t

val inline_attribute : Lambda.inline_attribute -> Inline_attribute.t

val inlined_attribute : Lambda.inlined_attribute -> Inlined_attribute.t

val kind_of_primitive_native_repr : Primitive.native_repr -> Flambda_kind.t

val method_kind : Lambda.meth_kind -> Call_kind.method_kind

val raise_kind : Lambda.raise_kind -> Trap_action.raise_kind

val convert_block_shape :
  Lambda.block_shape ->
  num_fields:int ->
  Flambda_primitive.Block_of_values_field.t list

val convert_mutable_flag : Lambda.mutable_flag -> Mutability.t

val convert_integer_comparison_prim :
  Lambda.integer_comparison -> Flambda_primitive.binary_primitive

val convert_boxed_integer_comparison_prim :
  Lambda.boxed_integer ->
  Lambda.integer_comparison ->
  Flambda_primitive.binary_primitive

val convert_float_comparison :
  Lambda.float_comparison -> Flambda_primitive.comparison

val boxable_number_of_boxed_integer :
  Lambda.boxed_integer -> Flambda_kind.Boxable_number.t

val standard_int_of_boxed_integer :
  Lambda.boxed_integer -> Flambda_kind.Standard_int.t

val standard_int_or_float_of_boxed_integer :
  Lambda.boxed_integer -> Flambda_kind.Standard_int_or_float.t

val convert_block_access_field_kind :
  Lambda.immediate_or_pointer -> Flambda_primitive.Block_access_field_kind.t

val convert_init_or_assign :
  Lambda.initialization_or_assignment -> Flambda_primitive.Init_or_assign.t

type converted_array_kind = private
  | Array_kind of Flambda_primitive.Array_kind.t
  | Float_array_opt_dynamic

val convert_array_kind : Lambda.array_kind -> converted_array_kind

type converted_duplicate_array_kind = private
  | Duplicate_array_kind of Flambda_primitive.Duplicate_array_kind.t
  | Float_array_opt_dynamic

val convert_array_kind_to_duplicate_array_kind :
  Lambda.array_kind -> converted_duplicate_array_kind

val convert_bigarray_kind :
  Lambda.bigarray_kind -> Flambda_primitive.bigarray_kind option

val convert_bigarray_layout :
  Lambda.bigarray_layout -> Flambda_primitive.bigarray_layout option

val convert_field_read_semantics : Lambda.field_read_semantics -> Mutability.t

val convert_lambda_block_size : int -> Targetint_31_63.Imm.t Or_unknown.t
