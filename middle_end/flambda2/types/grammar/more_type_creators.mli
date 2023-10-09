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

(** Type creation functions that augment the ones in [Type_grammar] but do not
    require direct access to the representation. *)

val unknown : Flambda_kind.t -> Type_grammar.t

val unknown_like : Type_grammar.t -> Type_grammar.t

val bottom : Flambda_kind.t -> Type_grammar.t

val bottom_like : Type_grammar.t -> Type_grammar.t

val these_naked_immediates : Targetint_31_63.Set.t -> Type_grammar.t

val these_naked_floats :
  Numeric_types.Float_by_bit_pattern.Set.t -> Type_grammar.t

val these_naked_int32s : Numeric_types.Int32.Set.t -> Type_grammar.t

val these_naked_int64s : Numeric_types.Int64.Set.t -> Type_grammar.t

val these_naked_nativeints : Targetint_32_64.Set.t -> Type_grammar.t

val these_naked_vec128s :
  Vector_types.Vec128.Bit_pattern.Set.t -> Type_grammar.t

val any_tagged_immediate : Type_grammar.t

val these_tagged_immediates0 : Targetint_31_63.Set.t -> Type_grammar.t

val these_tagged_immediates : Targetint_31_63.Set.t -> Type_grammar.t

val any_tagged_bool : Type_grammar.t

val any_naked_bool : Type_grammar.t

val this_boxed_float :
  Numeric_types.Float_by_bit_pattern.t ->
  Alloc_mode.For_types.t ->
  Type_grammar.t

val this_boxed_int32 : int32 -> Alloc_mode.For_types.t -> Type_grammar.t

val this_boxed_int64 : int64 -> Alloc_mode.For_types.t -> Type_grammar.t

val this_boxed_nativeint :
  Targetint_32_64.t -> Alloc_mode.For_types.t -> Type_grammar.t

val this_boxed_vec128 :
  Vector_types.Vec128.Bit_pattern.t -> Alloc_mode.For_types.t -> Type_grammar.t

val these_boxed_floats :
  Numeric_types.Float_by_bit_pattern.Set.t ->
  Alloc_mode.For_types.t ->
  Type_grammar.t

val these_boxed_int32s :
  Numeric_types.Int32.Set.t -> Alloc_mode.For_types.t -> Type_grammar.t

val these_boxed_int64s :
  Numeric_types.Int64.Set.t -> Alloc_mode.For_types.t -> Type_grammar.t

val these_boxed_nativeints :
  Targetint_32_64.Set.t -> Alloc_mode.For_types.t -> Type_grammar.t

val any_boxed_float : Type_grammar.t

val any_boxed_int32 : Type_grammar.t

val any_boxed_int64 : Type_grammar.t

val any_boxed_nativeint : Type_grammar.t

val any_block : Type_grammar.t

(* Note this is only for blocks (variants, tuples, etc), not arrays! *)
val blocks_with_these_tags :
  Tag.Set.t -> Alloc_mode.For_types.t -> Type_grammar.t Or_unknown.t

val immutable_block :
  is_unique:bool ->
  Tag.t ->
  field_kind:Flambda_kind.t ->
  Alloc_mode.For_types.t ->
  fields:Type_grammar.t list ->
  Type_grammar.t

val immutable_block_with_size_at_least :
  tag:Tag.t Or_unknown.t ->
  n:Targetint_31_63.t ->
  field_kind:Flambda_kind.t ->
  field_n_minus_one:Variable.t ->
  Type_grammar.t

val variant :
  const_ctors:Type_grammar.t ->
  non_const_ctors:Type_grammar.t list Tag.Scannable.Map.t ->
  Alloc_mode.For_types.t ->
  Type_grammar.t

val exactly_this_closure :
  Function_slot.t ->
  all_function_slots_in_set:
    Type_grammar.function_type Or_unknown_or_bottom.t Function_slot.Map.t ->
  all_closure_types_in_set:Type_grammar.t Function_slot.Map.t ->
  all_value_slots_in_set:Type_grammar.t Value_slot.Map.t ->
  Alloc_mode.For_types.t ->
  Type_grammar.t

val closure_with_at_least_these_function_slots :
  this_function_slot:Function_slot.t ->
  Simple.t Function_slot.Map.t ->
  Type_grammar.t

val closure_with_at_least_these_value_slots :
  this_function_slot:Function_slot.t ->
  (Variable.t * Flambda_kind.With_subkind.t) Value_slot.Map.t ->
  Type_grammar.t

val closure_with_at_least_this_value_slot :
  this_function_slot:Function_slot.t ->
  Value_slot.t ->
  value_slot_var:Variable.t ->
  value_slot_kind:Flambda_kind.With_subkind.t ->
  Type_grammar.t

val type_for_const : Reg_width_const.t -> Type_grammar.t

val kind_for_const : Reg_width_const.t -> Flambda_kind.t

val is_alias_of_name : Type_grammar.t -> Name.t -> bool

val check_equation : Name.t -> Type_grammar.t -> unit

val arity_of_list : Type_grammar.t list -> [`Unarized] Flambda_arity.t

val unknown_with_subkind :
  ?alloc_mode:Alloc_mode.For_types.t ->
  Flambda_kind.With_subkind.t ->
  Type_grammar.t

(** For each of the kinds in an arity, create an "unknown" type. *)
val unknown_types_from_arity :
  [`Unarized] Flambda_arity.t -> Type_grammar.t list
