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

(** The interface to the Flambda type system. This is parameterised over the
    expression language via [Code_id]. *)

type t

type flambda_type = t

val print : Format.formatter -> t -> unit

val arity_of_list : t list -> [`Unarized] Flambda_arity.t

val apply_renaming : t -> Renaming.t -> t

include Contains_ids.S with type t := t

val remove_unused_value_slots_and_shortcut_aliases :
  t ->
  used_value_slots:Value_slot.Set.t ->
  canonicalise:(Simple.t -> Simple.t) ->
  t

type typing_env

type typing_env_extension

module Code_age_relation : sig
  type t

  val print : Format.formatter -> t -> unit

  val empty : t

  val get_older_version_of : t -> Code_id.t -> Code_id.t option

  val all_code_ids_for_export : t -> Code_id.Set.t

  val apply_renaming : t -> Renaming.t -> t

  val union : t -> t -> t

  val meet :
    t ->
    resolver:(Compilation_unit.t -> t option) ->
    Code_id.t ->
    Code_id.t ->
    Code_id.t Or_bottom.t
end

module Typing_env_extension : sig
  type t = typing_env_extension

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val empty : t

  val one_equation : Name.t -> flambda_type -> t

  val add_or_replace_equation : t -> Name.t -> flambda_type -> t

  val meet : typing_env -> t -> t -> t Or_bottom.t

  module With_extra_variables : sig
    type t

    val print : Format.formatter -> t -> unit

    val empty : t

    val add_definition : t -> Variable.t -> Flambda_kind.t -> t

    val add_or_replace_equation : t -> Name.t -> flambda_type -> t

    val map_types : t -> f:(flambda_type -> flambda_type) -> t

    val existential_vars : t -> Variable.Set.t

    include Contains_ids.S with type t := t

    include Contains_names.S with type t := t
  end
end

module Typing_env : sig
  type t = typing_env

  module Pre_serializable : sig
    type t

    (* This function ensures that all occurrences of aliases in the returned
       environment are canonical. But some types, like function return types,
       live outside the typing environment. To ensure that they can be exported
       safely, they must go through
       [remove_unused_closure_vars_and_shortcut_aliases] too, with the
       [canonicalise] argument set to the function returned here. *)
    val create :
      typing_env ->
      used_value_slots:Value_slot.Set.t ->
      t * (Simple.t -> Simple.t)

    val find_or_missing : t -> Name.t -> flambda_type option
  end

  module Serializable : sig
    type t

    val create : Pre_serializable.t -> reachable_names:Name_occurrences.t -> t

    val create_from_closure_conversion_approx :
      'a Value_approximation.t Symbol.Map.t -> t

    val predefined_exceptions : Symbol.Set.t -> t

    val free_function_slots_and_value_slots : t -> Name_occurrences.t

    val print : Format.formatter -> t -> unit

    val name_domain : t -> Name.Set.t

    val ids_for_export : t -> Ids_for_export.t

    val apply_renaming : t -> Renaming.t -> t

    val merge : t -> t -> t

    val extract_symbol_approx :
      t -> Symbol.t -> (Code_id.t -> 'code) -> 'code Value_approximation.t
  end

  val print : Format.formatter -> t -> unit

  val create :
    resolver:(Compilation_unit.t -> Serializable.t option) ->
    get_imported_names:(unit -> Name.Set.t) ->
    t

  val closure_env : t -> t

  val resolver : t -> Compilation_unit.t -> Serializable.t option

  val code_age_relation_resolver :
    t -> Compilation_unit.t -> Code_age_relation.t option

  val current_scope : t -> Scope.t

  val increment_scope : t -> t

  val add_definition : t -> Bound_name.t -> Flambda_kind.t -> t

  val add_definitions_of_params : t -> params:Bound_parameters.t -> t

  val add_symbol_definition : t -> Symbol.t -> t

  val add_symbol_definitions : t -> Symbol.Set.t -> t

  val add_symbol_projection : t -> Variable.t -> Symbol_projection.t -> t

  val find_symbol_projection : t -> Variable.t -> Symbol_projection.t option

  val add_equation : t -> Name.t -> flambda_type -> t

  val add_equations_on_params :
    t -> params:Bound_parameters.t -> param_types:flambda_type list -> t

  val mem : ?min_name_mode:Name_mode.t -> t -> Name.t -> bool

  val mem_simple : ?min_name_mode:Name_mode.t -> t -> Simple.t -> bool

  val find : t -> Name.t -> Flambda_kind.t option -> flambda_type

  val find_or_missing : t -> Name.t -> flambda_type option

  val find_params : t -> Bound_parameters.t -> flambda_type list

  val add_env_extension : t -> Typing_env_extension.t -> t

  val add_env_extension_with_extra_variables :
    t -> Typing_env_extension.With_extra_variables.t -> t

  (** Raises [Not_found] if no canonical [Simple] was found.
      [name_mode_of_existing_simple] can be provided to improve performance of
      this function. *)
  val get_canonical_simple_exn :
    t ->
    ?min_name_mode:Name_mode.t ->
    ?name_mode_of_existing_simple:Name_mode.t ->
    Simple.t ->
    Simple.t

  (** Raises [Not_found] if no canonical [Simple] was found. *)
  val type_simple_in_term_exn :
    t -> ?min_name_mode:Name_mode.t -> Simple.t -> flambda_type

  (** Raises [Not_found] if no canonical [Simple] was found. *)
  val get_alias_then_canonical_simple_exn :
    t ->
    ?min_name_mode:Name_mode.t ->
    ?name_mode_of_existing_simple:Name_mode.t ->
    flambda_type ->
    Simple.t

  val code_age_relation : t -> Code_age_relation.t

  val with_code_age_relation : t -> Code_age_relation.t -> t

  val add_to_code_age_relation :
    t -> new_code_id:Code_id.t -> old_code_id:Code_id.t option -> t

  val free_names_transitive : t -> flambda_type -> Name_occurrences.t

  module Alias_set : sig
    type t

    val filter : t -> f:(Simple.t -> bool) -> t

    val get_singleton : t -> Simple.t option

    val find_best : t -> Simple.t option

    val inter : t -> t -> t

    val singleton : Simple.t -> t

    val print : Format.formatter -> t -> unit
  end

  val aliases_of_simple :
    t -> min_name_mode:Name_mode.t -> Simple.t -> Alias_set.t
end

val meet : Typing_env.t -> t -> t -> (t * Typing_env_extension.t) Or_bottom.t

val meet_shape :
  Typing_env.t ->
  t ->
  shape:t ->
  result_var:Bound_var.t ->
  result_kind:Flambda_kind.t ->
  Typing_env_extension.t Or_bottom.t

val join :
  ?bound_name:Name.t ->
  Typing_env.t ->
  left_env:Typing_env.t ->
  left_ty:t ->
  right_env:Typing_env.t ->
  right_ty:t ->
  t

val cut_and_n_way_join :
  Typing_env.t ->
  (Typing_env.t * Apply_cont_rewrite_id.t * Continuation_use_kind.t) list ->
  params:Bound_parameters.t ->
  cut_after:Scope.t ->
  extra_lifted_consts_in_use_envs:Symbol.Set.t ->
  extra_allowed_names:Name_occurrences.t ->
  Typing_env.t

module Function_type : sig
  type t

  val create : Code_id.t -> rec_info:flambda_type -> t

  val code_id : t -> Code_id.t

  val rec_info : t -> flambda_type
end

module Closures_entry : sig
  type t

  val value_slot_types : t -> flambda_type Value_slot.Map.t
end

val free_names : t -> Name_occurrences.t

type to_erase =
  | Everything_not_in of Typing_env.t
  | All_variables_except of Variable.Set.t

(** Adjust a type so it can be used in a different environment. There are two
    modes of operation: either a target environment can be specified, in which
    the resulting type is to be valid; or a set of variables may be supplied
    which are the only ones allowed to occur in the resulting type. *)
val make_suitable_for_environment :
  Typing_env.t ->
  to_erase ->
  (Name.t * flambda_type) list ->
  Typing_env_extension.With_extra_variables.t

val apply_coercion : flambda_type -> Coercion.t -> flambda_type

(** Construct a bottom type of the given kind. *)
val bottom : Flambda_kind.t -> t

(** Construct a top ("unknown") type of the given kind. *)
val unknown : Flambda_kind.t -> t

val unknown_with_subkind :
  ?alloc_mode:Alloc_mode.For_types.t -> Flambda_kind.With_subkind.t -> t

(** Create an bottom type with the same kind as the given type. *)
val bottom_like : t -> t

(** Create an "unknown" type with the same kind as the given type. *)
val unknown_like : t -> t

val any_value : t

val any_tagged_immediate : t

val any_tagged_bool : t

val any_boxed_float : t

val any_boxed_int32 : t

val any_boxed_int64 : t

val any_boxed_nativeint : t

val any_naked_immediate : t

val any_naked_bool : t

val any_naked_float : t

val any_naked_int32 : t

val any_naked_int64 : t

val any_naked_nativeint : t

val any_region : t

val any_rec_info : t

(** Building of types representing tagged / boxed values from specified
    constants. *)
val this_tagged_immediate : Targetint_31_63.t -> t

val this_boxed_float :
  Numeric_types.Float_by_bit_pattern.t -> Alloc_mode.For_types.t -> t

val this_boxed_int32 : Numeric_types.Int32.t -> Alloc_mode.For_types.t -> t

val this_boxed_int64 : Numeric_types.Int64.t -> Alloc_mode.For_types.t -> t

val this_boxed_nativeint : Targetint_32_64.t -> Alloc_mode.For_types.t -> t

val this_boxed_vec128 :
  Vector_types.Vec128.Bit_pattern.t -> Alloc_mode.For_types.t -> t

val these_tagged_immediates : Targetint_31_63.Set.t -> t

val these_boxed_floats :
  Numeric_types.Float_by_bit_pattern.Set.t -> Alloc_mode.For_types.t -> t

val these_boxed_int32s :
  Numeric_types.Int32.Set.t -> Alloc_mode.For_types.t -> t

val these_boxed_int64s :
  Numeric_types.Int64.Set.t -> Alloc_mode.For_types.t -> t

val these_boxed_nativeints :
  Targetint_32_64.Set.t -> Alloc_mode.For_types.t -> t

(** Building of types representing untagged / unboxed values from specified
    constants. *)
val this_naked_immediate : Targetint_31_63.t -> t

val this_naked_float : Numeric_types.Float_by_bit_pattern.t -> t

val this_naked_int32 : Numeric_types.Int32.t -> t

val this_naked_int64 : Numeric_types.Int64.t -> t

val this_naked_nativeint : Targetint_32_64.t -> t

val this_rec_info : Rec_info_expr.t -> t

val these_naked_immediates : Targetint_31_63.Set.t -> t

val these_naked_floats : Numeric_types.Float_by_bit_pattern.Set.t -> t

val these_naked_int32s : Numeric_types.Int32.Set.t -> t

val these_naked_int64s : Numeric_types.Int64.Set.t -> t

val these_naked_nativeints : Targetint_32_64.Set.t -> t

val boxed_float_alias_to : naked_float:Variable.t -> Alloc_mode.For_types.t -> t

val boxed_int32_alias_to : naked_int32:Variable.t -> Alloc_mode.For_types.t -> t

val boxed_int64_alias_to : naked_int64:Variable.t -> Alloc_mode.For_types.t -> t

val boxed_nativeint_alias_to :
  naked_nativeint:Variable.t -> Alloc_mode.For_types.t -> t

val boxed_vec128_alias_to :
  naked_vec128:Variable.t -> Alloc_mode.For_types.t -> t

val box_float : t -> Alloc_mode.For_types.t -> t

val box_int32 : t -> Alloc_mode.For_types.t -> t

val box_int64 : t -> Alloc_mode.For_types.t -> t

val box_nativeint : t -> Alloc_mode.For_types.t -> t

val box_vec128 : t -> Alloc_mode.For_types.t -> t

val tagged_immediate_alias_to : naked_immediate:Variable.t -> t

val tag_immediate : t -> t

val is_int_for_scrutinee : scrutinee:Simple.t -> t

val get_tag_for_block : block:Simple.t -> t

val any_block : t

(** The type of an immutable block with a known tag, size and field types. *)
val immutable_block :
  is_unique:bool ->
  Tag.t ->
  field_kind:Flambda_kind.t ->
  Alloc_mode.For_types.t ->
  fields:t list ->
  t

(** The type of an immutable block with at least [n] fields and an unknown tag.
    The type of the [n - 1]th field is taken to be an [Equals] to the given
    variable. *)
val immutable_block_with_size_at_least :
  tag:Tag.t Or_unknown.t ->
  n:Targetint_31_63.t ->
  field_kind:Flambda_kind.t ->
  field_n_minus_one:Variable.t ->
  t

val mutable_block : Alloc_mode.For_types.t -> t

val variant :
  const_ctors:t ->
  non_const_ctors:t list Tag.Scannable.Map.t ->
  Alloc_mode.For_types.t ->
  t

val this_immutable_string : string -> t

val mutable_string : size:int -> t

val exactly_this_closure :
  Function_slot.t ->
  all_function_slots_in_set:
    Function_type.t Or_unknown_or_bottom.t Function_slot.Map.t ->
  all_closure_types_in_set:t Function_slot.Map.t ->
  all_value_slots_in_set:flambda_type Value_slot.Map.t ->
  Alloc_mode.For_types.t ->
  flambda_type

val closure_with_at_least_these_function_slots :
  this_function_slot:Function_slot.t ->
  Simple.t Function_slot.Map.t ->
  flambda_type

val closure_with_at_least_this_value_slot :
  this_function_slot:Function_slot.t ->
  Value_slot.t ->
  value_slot_var:Variable.t ->
  value_slot_kind:Flambda_kind.With_subkind.t ->
  flambda_type

val closure_with_at_least_these_value_slots :
  this_function_slot:Function_slot.t ->
  (Variable.t * Flambda_kind.With_subkind.t) Value_slot.Map.t ->
  flambda_type

val array_of_length :
  element_kind:Flambda_kind.With_subkind.t Or_unknown_or_bottom.t ->
  length:flambda_type ->
  Alloc_mode.For_types.t ->
  flambda_type

val mutable_array :
  element_kind:Flambda_kind.With_subkind.t Or_unknown_or_bottom.t ->
  length:flambda_type ->
  Alloc_mode.For_types.t ->
  flambda_type

val immutable_array :
  element_kind:Flambda_kind.With_subkind.t Or_unknown_or_bottom.t ->
  fields:flambda_type list ->
  Alloc_mode.For_types.t ->
  flambda_type

(** Construct a type equal to the type of the given name. (The name must be
    present in the given environment when calling e.g. [join].) *)
val alias_type_of : Flambda_kind.t -> Simple.t -> t

(** Determine the (unique) kind of a type. *)
val kind : t -> Flambda_kind.t

val get_alias_exn : t -> Simple.t

(** For each of the kinds in an arity, create an "unknown" type. *)
val unknown_types_from_arity : [`Unarized] Flambda_arity.t -> t list

(** Whether the given type says that a term of that type can never be
    constructed (in other words, it is [Invalid]). *)
val is_bottom : Typing_env.t -> t -> bool

val is_unknown : Typing_env.t -> t -> bool

val type_for_const : Reg_width_const.t -> t

val kind_for_const : Reg_width_const.t -> Flambda_kind.t

type 'a meet_shortcut = private
  | Known_result of 'a
  | Need_meet
  | Invalid

type 'a proof_of_property = private
  | Proved of 'a
  | Unknown

(* CR mshinwell: Should remove "_equals_" from these names *)
val prove_equals_tagged_immediates :
  Typing_env.t -> t -> Targetint_31_63.Set.t proof_of_property

val meet_equals_tagged_immediates :
  Typing_env.t -> t -> Targetint_31_63.Set.t meet_shortcut

val meet_naked_immediates :
  Typing_env.t -> t -> Targetint_31_63.Set.t meet_shortcut

val meet_equals_single_tagged_immediate :
  Typing_env.t -> t -> Targetint_31_63.t meet_shortcut

val meet_naked_floats :
  Typing_env.t -> t -> Numeric_types.Float_by_bit_pattern.Set.t meet_shortcut

val meet_naked_int32s :
  Typing_env.t -> t -> Numeric_types.Int32.Set.t meet_shortcut

val meet_naked_int64s :
  Typing_env.t -> t -> Numeric_types.Int64.Set.t meet_shortcut

val meet_naked_nativeints :
  Typing_env.t -> t -> Targetint_32_64.Set.t meet_shortcut

type variant_like_proof = private
  { const_ctors : Targetint_31_63.Set.t Or_unknown.t;
    non_const_ctors_with_sizes : Targetint_31_63.t Tag.Scannable.Map.t
  }

val meet_variant_like : Typing_env.t -> t -> variant_like_proof meet_shortcut

val prove_variant_like :
  Typing_env.t -> t -> variant_like_proof proof_of_property

(** If [ty] is known to represent a boxed number or a tagged integer,
    [prove_is_a_boxed_number env ty] is [Proved (alloc_mode,kind,contents_ty)].
    [kind] is the kind of the unboxed number.

    If [ty] is known to represent something of kind value that is not a number
    [prove_is_a_boxed_number env ty] is [Invalid].

    Otherwise it is [Unknown] or [Wrong_kind] when [ty] is not of kind value. *)
type boxed_or_tagged_number = private
  | Boxed of Alloc_mode.For_types.t * Flambda_kind.Boxable_number.t * t
  | Tagged_immediate

val prove_is_a_boxed_or_tagged_number :
  Typing_env.t -> t -> boxed_or_tagged_number proof_of_property

val prove_is_a_tagged_immediate : Typing_env.t -> t -> unit proof_of_property

val prove_is_a_boxed_float : Typing_env.t -> t -> unit proof_of_property

val prove_is_a_boxed_int32 : Typing_env.t -> t -> unit proof_of_property

val prove_is_a_boxed_int64 : Typing_env.t -> t -> unit proof_of_property

val prove_is_a_boxed_nativeint : Typing_env.t -> t -> unit proof_of_property

val prove_is_a_boxed_vec128 : Typing_env.t -> t -> unit proof_of_property

val prove_is_or_is_not_a_boxed_float :
  Typing_env.t -> t -> bool proof_of_property

val prove_unique_tag_and_size :
  Typing_env.t -> t -> (Tag.t * Targetint_31_63.t) proof_of_property

val prove_unique_fully_constructed_immutable_heap_block :
  Typing_env.t -> t -> (Tag_and_size.t * Simple.t list) proof_of_property

val prove_is_int : Typing_env.t -> t -> bool proof_of_property

val meet_is_flat_float_array : Typing_env.t -> t -> bool meet_shortcut

val prove_is_immediates_array : Typing_env.t -> t -> unit proof_of_property

val meet_is_immutable_array :
  Typing_env.t ->
  t ->
  (Flambda_kind.With_subkind.t Or_unknown_or_bottom.t
  * t
  * Alloc_mode.For_types.t)
  meet_shortcut

val meet_single_closures_entry :
  Typing_env.t ->
  t ->
  (Function_slot.t
  * Alloc_mode.For_types.t
  * Closures_entry.t
  * Function_type.t)
  meet_shortcut

val prove_single_closures_entry :
  Typing_env.t ->
  t ->
  (Function_slot.t
  * Alloc_mode.For_types.t
  * Closures_entry.t
  * Function_type.t)
  proof_of_property

val meet_strings : Typing_env.t -> t -> String_info.Set.t meet_shortcut

val prove_strings :
  Typing_env.t ->
  t ->
  (Alloc_mode.For_types.t * String_info.Set.t) proof_of_property

(** Attempt to show that the provided type describes the tagged version of a
    unique naked immediate [Simple].

    This function will return [Unknown] if values of the provided type might
    sometimes, but not always, be a tagged immediate (for example if it is a
    variant type involving blocks). *)
val prove_tagging_of_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> t -> Simple.t proof_of_property

(** Attempt to show that the provided type _can_ describe, but might not always
    describe, the tagged version of a unique naked immediate [Simple]. It is
    guaranteed that if a [Simple] is returned, the type does not describe any
    other tagged immediate. *)
val meet_tagging_of_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> t -> Simple.t meet_shortcut

val meet_boxed_float_containing_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> t -> Simple.t meet_shortcut

val meet_boxed_int32_containing_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> t -> Simple.t meet_shortcut

val meet_boxed_int64_containing_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> t -> Simple.t meet_shortcut

val meet_boxed_nativeint_containing_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> t -> Simple.t meet_shortcut

val meet_boxed_vec128_containing_simple :
  Typing_env.t -> min_name_mode:Name_mode.t -> t -> Simple.t meet_shortcut

val meet_block_field_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  field_kind:Flambda_kind.t ->
  t ->
  Targetint_31_63.t ->
  Simple.t meet_shortcut

val meet_project_value_slot_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  t ->
  Value_slot.t ->
  Simple.t meet_shortcut

val meet_project_function_slot_simple :
  Typing_env.t ->
  min_name_mode:Name_mode.t ->
  t ->
  Function_slot.t ->
  Simple.t meet_shortcut

val meet_rec_info : Typing_env.t -> t -> Rec_info_expr.t meet_shortcut

val prove_alloc_mode_of_boxed_number :
  Typing_env.t -> t -> Alloc_mode.For_types.t proof_of_property

val prove_physical_equality : Typing_env.t -> t -> t -> bool proof_of_property

type to_lift = private
  | Immutable_block of
      { tag : Tag.Scannable.t;
        is_unique : bool;
        fields : Simple.t list
      }
  | Boxed_float of Numeric_types.Float_by_bit_pattern.t
  | Boxed_int32 of Numeric_types.Int32.t
  | Boxed_int64 of Numeric_types.Int64.t
  | Boxed_nativeint of Targetint_32_64.t
  | Boxed_vec128 of Vector_types.Vec128.Bit_pattern.t
  | Immutable_float_array of
      { fields : Numeric_types.Float_by_bit_pattern.t list }
  | Immutable_value_array of { fields : Simple.t list }
  | Empty_array

type reification_result = private
  | Lift of to_lift
  | Simple of Simple.t
  | Cannot_reify
  | Invalid

val reify :
  allowed_if_free_vars_defined_in:Typing_env.t ->
  var_is_defined_at_toplevel:(Variable.t -> bool) ->
  var_is_symbol_projection:(Variable.t -> bool) ->
  Typing_env.t ->
  t ->
  reification_result

val never_holds_locally_allocated_values :
  Typing_env.t -> Variable.t -> unit proof_of_property

val remove_outermost_alias : Typing_env.t -> t -> t
