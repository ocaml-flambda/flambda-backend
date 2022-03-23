(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** A [proof_of_operation] gives us the result of a particular operation. The
    corresponding functions provide efficient alternatives to the generic meet
    function, with similar semantics. *)
type 'a proof_of_operation = private
  | Known_result of 'a  (** Result has been succesfully computed *)
  | Unknown  (** Exact result could not be computed or is Top *)
  | Invalid  (** Result is Bottom *)

(** A [proof_of_property] tells us whether the input type matches a given
    property. The extra type parameter can hold additional information, for
    instance [prove_is_boxed_number] can return which kind of boxed number. *)
type 'a proof_of_property = private
  | Proved of 'a
  | Unknown
  | Wrong_kind

(** If this returns a simple, it is bound with mode Normal *)
val prove_equals_to_simple_of_kind_value :
  Typing_env.t -> Type_grammar.t -> Simple.t proof_of_property

(* CR mshinwell: Should remove "_equals_" from these names *)
val prove_equals_tagged_immediates :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.Set.t proof_of_property

val check_equals_tagged_immediates :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.Set.t proof_of_operation

val check_naked_immediates :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.Set.t proof_of_operation

val check_equals_single_tagged_immediate :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.t proof_of_operation

val check_naked_floats :
  Typing_env.t ->
  Type_grammar.t ->
  Numeric_types.Float_by_bit_pattern.Set.t proof_of_operation

val check_naked_int32s :
  Typing_env.t -> Type_grammar.t -> Numeric_types.Int32.Set.t proof_of_operation

val check_naked_int64s :
  Typing_env.t -> Type_grammar.t -> Numeric_types.Int64.Set.t proof_of_operation

val check_naked_nativeints :
  Typing_env.t -> Type_grammar.t -> Targetint_32_64.Set.t proof_of_operation

type variant_like_proof = private
  { const_ctors : Targetint_31_63.Set.t Or_unknown.t;
    non_const_ctors_with_sizes : Targetint_31_63.t Tag.Scannable.Map.t
  }

val check_variant_like :
  Typing_env.t -> Type_grammar.t -> variant_like_proof proof_of_operation

val prove_variant_like :
  Typing_env.t -> Type_grammar.t -> variant_like_proof proof_of_property

type boxed_or_tagged_number = private
  | Boxed of Flambda_kind.Boxable_number.t
  | Tagged_immediate

val prove_is_a_boxed_or_tagged_number :
  Typing_env.t -> Type_grammar.t -> boxed_or_tagged_number proof_of_property

val prove_is_a_tagged_immediate :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val prove_is_a_boxed_float :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val prove_is_a_boxed_int32 :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val prove_is_a_boxed_int64 :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val prove_is_a_boxed_nativeint :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val prove_is_or_is_not_a_boxed_float :
  Typing_env.t -> Type_grammar.t -> bool proof_of_property

val check_boxed_floats :
  Typing_env.t ->
  Type_grammar.t ->
  Numeric_types.Float_by_bit_pattern.Set.t proof_of_operation

val check_boxed_int32s :
  Typing_env.t -> Type_grammar.t -> Numeric_types.Int32.Set.t proof_of_operation

val check_boxed_int64s :
  Typing_env.t -> Type_grammar.t -> Numeric_types.Int64.Set.t proof_of_operation

val check_boxed_nativeints :
  Typing_env.t -> Type_grammar.t -> Targetint_32_64.Set.t proof_of_operation

val prove_unique_tag_and_size :
  Typing_env.t ->
  Type_grammar.t ->
  (Tag.t * Targetint_31_63.t) proof_of_property

val prove_is_int : Typing_env.t -> Type_grammar.t -> bool proof_of_property

val prove_get_tag :
  Typing_env.t -> Type_grammar.t -> Tag.Set.t proof_of_property

val prove_unique_fully_constructed_immutable_heap_block :
  Typing_env.t ->
  Type_grammar.t ->
  (Tag_and_size.t * Simple.t list) proof_of_property

type array_kind_compatibility =
  | Exact
  | Compatible
  | Incompatible

val check_is_array_with_element_kind :
  Typing_env.t ->
  Type_grammar.t ->
  element_kind:Flambda_kind.With_subkind.t ->
  array_kind_compatibility proof_of_operation

val check_single_closures_entry :
  Typing_env.t ->
  Type_grammar.t ->
  (Function_slot.t
  * Alloc_mode.t Or_unknown.t
  * Type_grammar.Closures_entry.t
  * Type_grammar.Function_type.t)
  proof_of_operation

val prove_single_closures_entry :
  Typing_env.t ->
  Type_grammar.t ->
  (Function_slot.t
  * Alloc_mode.t Or_unknown.t
  * Type_grammar.Closures_entry.t
  * Type_grammar.Function_type.t)
  proof_of_property

val check_strings :
  Typing_env.t -> Type_grammar.t -> String_info.Set.t proof_of_operation

val prove_tagging_of_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t proof_of_property

val check_tagging_of_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t proof_of_operation

val check_boxed_float_containing_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t proof_of_operation

val check_boxed_int32_containing_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t proof_of_operation

val check_boxed_int64_containing_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t proof_of_operation

val check_boxed_nativeint_containing_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t proof_of_operation

val check_block_field_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Targetint_31_63.t ->
  Simple.t proof_of_operation

val check_variant_field_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Tag.t ->
  Targetint_31_63.t ->
  Simple.t proof_of_operation

val check_project_value_slot_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Value_slot.t ->
  Simple.t proof_of_operation

val check_project_function_slot_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Function_slot.t ->
  Simple.t proof_of_operation

val check_rec_info :
  Typing_env.t -> Type_grammar.t -> Rec_info_expr.t proof_of_operation

val prove_alloc_mode_of_boxed_number :
  Typing_env.t -> Type_grammar.t -> Alloc_mode.t proof_of_property

val never_holds_locally_allocated_values :
  Typing_env.t -> Variable.t -> Flambda_kind.t -> unit proof_of_property
