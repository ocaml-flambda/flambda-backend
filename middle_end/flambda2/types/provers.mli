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

(** A [meet_shortcut] gives us the result of a particular operation. There are
    various cases where we meet with a shape containing a particular variable
    and make decisions depending on the type associated to that variable. But a
    generic meet can be expensive, so this file contains functions that can
    return an equivalent result in the easy cases. *)
type 'a meet_shortcut = private
  | Known_result of 'a  (** Result has been succesfully computed *)
  | Need_meet  (** Exact result could not be computed or is Top *)
  | Invalid  (** Result is Bottom *)

(** A [proof_of_property] tells us whether the input type matches a given
    property. The extra type parameter can hold additional information, for
    instance [prove_is_boxed_number] can return which kind of boxed number. *)
type 'a proof_of_property = private
  | Proved of 'a
  | Unknown

(** If this returns a simple, it is bound with mode Normal *)
val prove_equals_to_simple_of_kind_value :
  Typing_env.t -> Type_grammar.t -> Simple.t proof_of_property

(* CR mshinwell: Should remove "_equals_" from these names *)
val prove_equals_tagged_immediates :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.Set.t proof_of_property

val meet_equals_tagged_immediates :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.Set.t meet_shortcut

val meet_naked_immediates :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.Set.t meet_shortcut

val prove_naked_immediates :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.Set.t proof_of_property

val meet_equals_single_tagged_immediate :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.t meet_shortcut

val meet_naked_float32s :
  Typing_env.t ->
  Type_grammar.t ->
  Numeric_types.Float32_by_bit_pattern.Set.t meet_shortcut

val meet_naked_floats :
  Typing_env.t ->
  Type_grammar.t ->
  Numeric_types.Float_by_bit_pattern.Set.t meet_shortcut

val meet_naked_int32s :
  Typing_env.t -> Type_grammar.t -> Numeric_types.Int32.Set.t meet_shortcut

val meet_naked_int64s :
  Typing_env.t -> Type_grammar.t -> Numeric_types.Int64.Set.t meet_shortcut

val meet_naked_nativeints :
  Typing_env.t -> Type_grammar.t -> Targetint_32_64.Set.t meet_shortcut

val meet_naked_vec128s :
  Typing_env.t ->
  Type_grammar.t ->
  Vector_types.Vec128.Bit_pattern.Set.t meet_shortcut

type variant_like_proof = private
  { const_ctors : Targetint_31_63.Set.t Or_unknown.t;
    non_const_ctors_with_sizes :
      (Targetint_31_63.t * Flambda_kind.Block_shape.t) Tag.Scannable.Map.t
  }

val meet_variant_like :
  Typing_env.t -> Type_grammar.t -> variant_like_proof meet_shortcut

val prove_variant_like :
  Typing_env.t -> Type_grammar.t -> variant_like_proof proof_of_property

type boxed_or_tagged_number = private
  | Boxed of
      Alloc_mode.For_types.t * Flambda_kind.Boxable_number.t * Type_grammar.t
  | Tagged_immediate

val prove_is_a_boxed_or_tagged_number :
  Typing_env.t -> Type_grammar.t -> boxed_or_tagged_number proof_of_property

val prove_is_a_tagged_immediate :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val prove_is_a_boxed_float32 :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val prove_is_a_boxed_float :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val prove_is_a_boxed_int32 :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val prove_is_a_boxed_int64 :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val prove_is_a_boxed_nativeint :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val prove_is_a_boxed_vec128 :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val prove_is_or_is_not_a_boxed_float :
  Typing_env.t -> Type_grammar.t -> bool proof_of_property

val prove_unique_tag_and_size :
  Typing_env.t ->
  Type_grammar.t ->
  (Tag.t * Flambda_kind.Block_shape.t * Targetint_31_63.t) proof_of_property

val prove_is_int : Typing_env.t -> Type_grammar.t -> bool proof_of_property

val meet_is_int_variant_only :
  Typing_env.t -> Type_grammar.t -> bool meet_shortcut

val prove_get_tag :
  Typing_env.t -> Type_grammar.t -> Tag.Set.t proof_of_property

val prove_unique_fully_constructed_immutable_heap_block :
  Typing_env.t ->
  Type_grammar.t ->
  (Tag.t * Flambda_kind.Block_shape.t * Targetint_31_63.t * Simple.t list)
  proof_of_property

val meet_is_naked_number_array :
  Typing_env.t ->
  Type_grammar.t ->
  Flambda_kind.Naked_number_kind.t ->
  bool meet_shortcut

val meet_is_immutable_array :
  Typing_env.t ->
  Type_grammar.t ->
  (Flambda_kind.With_subkind.t Or_unknown_or_bottom.t
  * Type_grammar.t
  * Alloc_mode.For_types.t)
  meet_shortcut

val prove_is_immediates_array :
  Typing_env.t -> Type_grammar.t -> unit proof_of_property

val meet_single_closures_entry :
  Typing_env.t ->
  Type_grammar.t ->
  (Function_slot.t
  * Alloc_mode.For_types.t (* CR vlaviron: remove the Closures_entry.t field *)
  * Type_grammar.Closures_entry.t
  * Type_grammar.Function_type.t)
  meet_shortcut

val prove_single_closures_entry :
  Typing_env.t ->
  Type_grammar.t ->
  (Function_slot.t
  * Alloc_mode.For_types.t (* CR vlaviron: remove the Closures_entry.t field *)
  * Type_grammar.Closures_entry.t
  * Type_grammar.Function_type.t)
  proof_of_property

val meet_strings :
  Typing_env.t -> Type_grammar.t -> String_info.Set.t meet_shortcut

val prove_strings :
  Typing_env.t ->
  Type_grammar.t ->
  (Alloc_mode.For_types.t * String_info.Set.t) proof_of_property

val prove_tagging_of_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t proof_of_property

val meet_tagging_of_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t meet_shortcut

val meet_boxed_float32_containing_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t meet_shortcut

val meet_boxed_float_containing_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t meet_shortcut

val meet_boxed_int32_containing_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t meet_shortcut

val meet_boxed_int64_containing_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t meet_shortcut

val meet_boxed_nativeint_containing_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t meet_shortcut

val meet_boxed_vec128_containing_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Simple.t meet_shortcut

val meet_block_field_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  field_kind:Flambda_kind.t ->
  Type_grammar.t ->
  Targetint_31_63.t ->
  Simple.t meet_shortcut

val meet_project_value_slot_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Value_slot.t ->
  Simple.t meet_shortcut

val meet_project_function_slot_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Function_slot.t ->
  Simple.t meet_shortcut

val meet_rec_info :
  Typing_env.t -> Type_grammar.t -> Rec_info_expr.t meet_shortcut

val prove_alloc_mode_of_boxed_number :
  Typing_env.t -> Type_grammar.t -> Alloc_mode.For_types.t proof_of_property

val never_holds_locally_allocated_values :
  Typing_env.t -> Variable.t -> unit proof_of_property

val prove_physical_equality :
  Typing_env.t -> Type_grammar.t -> Type_grammar.t -> bool proof_of_property
