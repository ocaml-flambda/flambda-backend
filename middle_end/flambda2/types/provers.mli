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

[@@@ocaml.warning "+a-30-40-41-42"]

type 'a proof = private
  | Proved of 'a
  | Unknown
  | Invalid

type 'a proof_allowing_kind_mismatch = private
  | Proved of 'a
  | Unknown
  | Invalid
  | Wrong_kind

type var_or_symbol_or_tagged_immediate = private
  | Var of Variable.t
  | Symbol of Symbol.t
  | Tagged_immediate of Targetint_31_63.t

val prove_equals_to_var_or_symbol_or_tagged_immediate :
  Typing_env.t ->
  Type_grammar.t ->
  (var_or_symbol_or_tagged_immediate * Coercion.t) proof

(* CR mshinwell: Should remove "_equals_" from these names *)
val prove_equals_tagged_immediates :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.Set.t proof

val prove_naked_immediates :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.Set.t proof

val prove_equals_single_tagged_immediate :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.t proof

val prove_naked_floats :
  Typing_env.t ->
  Type_grammar.t ->
  Numeric_types.Float_by_bit_pattern.Set.t proof

val prove_naked_int32s :
  Typing_env.t -> Type_grammar.t -> Numeric_types.Int32.Set.t proof

val prove_naked_int64s :
  Typing_env.t -> Type_grammar.t -> Numeric_types.Int64.Set.t proof

val prove_naked_nativeints :
  Typing_env.t -> Type_grammar.t -> Targetint_32_64.Set.t proof

type variant_like_proof = private
  { const_ctors : Targetint_31_63.Set.t Or_unknown.t;
    non_const_ctors_with_sizes : Targetint_31_63.Imm.t Tag.Scannable.Map.t
  }

val prove_variant_like :
  Typing_env.t ->
  Type_grammar.t ->
  variant_like_proof proof_allowing_kind_mismatch

(** If [ty] is known to represent a boxed number or a tagged integer,
    [prove_is_a_boxed_number env ty] is [Proved kind]. [kind] is the kind of the
    unboxed number.

    If [ty] is known to represent something of kind value that is not a number
    [prove_is_a_boxed_number env ty] is [Invalid].

    Otherwise it is [Unknown] or [Wrong_kind] when [ty] is not of kind value. *)
val prove_is_a_boxed_number :
  Typing_env.t ->
  Type_grammar.t ->
  Flambda_kind.Boxable_number.t proof_allowing_kind_mismatch

val prove_is_a_tagged_immediate :
  Typing_env.t -> Type_grammar.t -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_float :
  Typing_env.t -> Type_grammar.t -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_int32 :
  Typing_env.t -> Type_grammar.t -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_int64 :
  Typing_env.t -> Type_grammar.t -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_nativeint :
  Typing_env.t -> Type_grammar.t -> unit proof_allowing_kind_mismatch

val prove_is_or_is_not_a_boxed_float :
  Typing_env.t -> Type_grammar.t -> bool proof_allowing_kind_mismatch

val prove_boxed_floats :
  Typing_env.t ->
  Type_grammar.t ->
  Numeric_types.Float_by_bit_pattern.Set.t proof

val prove_boxed_int32s :
  Typing_env.t -> Type_grammar.t -> Numeric_types.Int32.Set.t proof

val prove_boxed_int64s :
  Typing_env.t -> Type_grammar.t -> Numeric_types.Int64.Set.t proof

val prove_boxed_nativeints :
  Typing_env.t -> Type_grammar.t -> Targetint_32_64.Set.t proof

val prove_tags_and_sizes :
  Typing_env.t -> Type_grammar.t -> Targetint_31_63.Imm.t Tag.Map.t proof

val prove_tags_must_be_a_block :
  Typing_env.t -> Type_grammar.t -> Tag.Set.t proof

val prove_unique_tag_and_size :
  Typing_env.t ->
  Type_grammar.t ->
  (Tag.t * Targetint_31_63.Imm.t) proof_allowing_kind_mismatch

val prove_is_int : Typing_env.t -> Type_grammar.t -> bool proof

type array_kind_compatibility =
  | Exact
  | Compatible
  | Incompatible

(** This function deems non-[Array] types of kind [Value] to be [Invalid]. *)
val prove_is_array_with_element_kind :
  Typing_env.t ->
  Type_grammar.t ->
  element_kind:Flambda_kind.With_subkind.t ->
  array_kind_compatibility proof

(* CR mshinwell: Fix comment and/or function name *)

(** Prove that the given type, of kind [Value], is a closures type describing
    exactly one set of closures. The function declaration type corresponding to
    such closure is returned together with its closure ID, if it is known. *)
val prove_single_closures_entry :
  Typing_env.t ->
  Type_grammar.t ->
  (Closure_id.t * Type_grammar.Closures_entry.t * Type_grammar.Function_type.t)
  proof

val prove_single_closures_entry' :
  Typing_env.t ->
  Type_grammar.t ->
  (Closure_id.t * Type_grammar.Closures_entry.t * Type_grammar.Function_type.t)
  proof_allowing_kind_mismatch

val prove_strings : Typing_env.t -> Type_grammar.t -> String_info.Set.t proof

(** Attempt to show that the provided type describes the tagged version of a
    unique naked immediate [Simple].

    This function will return [Unknown] if values of the provided type might
    sometimes, but not always, be a tagged immediate (for example if it is a
    variant type involving blocks). *)
val prove_is_always_tagging_of_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> Type_grammar.t -> Simple.t proof

(** Attempt to show that the provided type _can_ describe, but might not always
    describe, the tagged version of a unique naked immediate [Simple]. It is
    guaranteed that if a [Simple] is returned, the type does not describe any
    other tagged immediate. *)
val prove_could_be_tagging_of_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> Type_grammar.t -> Simple.t proof

val prove_boxed_float_containing_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> Type_grammar.t -> Simple.t proof

val prove_boxed_int32_containing_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> Type_grammar.t -> Simple.t proof

val prove_boxed_int64_containing_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> Type_grammar.t -> Simple.t proof

val prove_boxed_nativeint_containing_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> Type_grammar.t -> Simple.t proof

val prove_block_field_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Targetint_31_63.t ->
  Simple.t proof

val prove_variant_field_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Tag.t ->
  Targetint_31_63.t ->
  Simple.t proof

val prove_project_var_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  Type_grammar.t ->
  Var_within_closure.t ->
  Simple.t proof

val prove_rec_info : Typing_env.t -> Type_grammar.t -> Rec_info_expr.t proof
