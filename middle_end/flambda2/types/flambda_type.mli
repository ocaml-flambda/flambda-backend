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

(** The interface to the Flambda type system.  This is parameterised over
    the expression language via [Code_id]. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR mshinwell: Add invariant checks, including e.g. on the bodies of
   functions in types. *)

module Float = Numeric_types.Float_by_bit_pattern
module Int32 = Numeric_types.Int32
module Int64 = Numeric_types.Int64

type t
type flambda_type = t

val print : Format.formatter -> t -> unit

val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit

val invariant : Invariant_env.t -> t -> unit

val arity_of_list : t list -> Flambda_arity.t

type typing_env
type typing_env_extension

module Typing_env_extension : sig
  type t = typing_env_extension

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val empty : unit -> t

  val one_equation : Name.t -> flambda_type -> t

  val add_or_replace_equation : t -> Name.t -> flambda_type -> t

  module With_extra_variables : sig
    type t

    val print : Format.formatter -> t -> unit

    val empty : unit -> t

    val add_definition
       : t
      -> Variable.t
      -> Flambda_kind.t
      -> flambda_type
      -> t

    val add_or_replace_equation : t -> Name.t -> flambda_type -> t
  end
end

module Typing_env : sig
  type t = typing_env

  val invariant : t -> unit

  val print : Format.formatter -> t -> unit

  val create
     : resolver:(Compilation_unit.t -> t option)
    -> get_imported_names:(unit -> Name.Set.t)
    -> get_imported_code:(unit -> Exported_code.t)
    -> t

  val closure_env : t -> t

  val resolver : t -> (Compilation_unit.t -> t option)

  val code_age_relation_resolver
     : t
    -> (Compilation_unit.t -> Code_age_relation.t option)

  val name_domain : t -> Name.Set.t

  val current_scope : t -> Scope.t

  val increment_scope : t -> t

  val all_code : t -> Flambda.Code.t Code_id.Map.t

  val add_definition : t -> Name_in_binding_pos.t -> Flambda_kind.t -> t

  val add_definitions_of_params : t -> params:Kinded_parameter.t list -> t

  val add_symbol_definition : t -> Symbol.t -> t

  val add_symbol_definitions : t -> Symbol.Set.t -> t

  val add_symbol_projection : t -> Variable.t -> Symbol_projection.t -> t

  val find_symbol_projection : t -> Variable.t -> Symbol_projection.t option

  val add_equation : t -> Name.t -> flambda_type -> t

  val add_equations_on_params
     : t
    -> params:Kinded_parameter.t list
    -> param_types:flambda_type list
    -> t

  val meet_equations_on_params
     : t
    -> params:Kinded_parameter.t list
    -> param_types:flambda_type list
    -> t

  val mem : ?min_name_mode:Name_mode.t -> t -> Name.t -> bool

  val mem_simple : ?min_name_mode:Name_mode.t -> t -> Simple.t -> bool

  val find : t -> Name.t -> Flambda_kind.t option -> flambda_type

  val find_or_missing : t -> Name.t -> flambda_type option

  val find_params : t -> Kinded_parameter.t list -> flambda_type list

  val add_env_extension : t -> Typing_env_extension.t -> t

  val add_env_extension_with_extra_variables
     : t
    -> Typing_env_extension.With_extra_variables.t
    -> t

  (** Raises [Not_found] if no canonical [Simple] was found.
      [name_mode_of_existing_simple] can be provided to improve performance
      of this function. *)
  val get_canonical_simple_exn
     : t
    -> ?min_name_mode:Name_mode.t
    -> ?name_mode_of_existing_simple:Name_mode.t
    -> Simple.t
    -> Simple.t

  (** Raises [Not_found] if no canonical [Simple] was found. *)
  val type_simple_in_term_exn
     : t
    -> ?min_name_mode:Name_mode.t
    -> Simple.t
    -> flambda_type

  (** Raises [Not_found] if no canonical [Simple] was found. *)
  val get_alias_then_canonical_simple_exn
     : t
    -> ?min_name_mode:Name_mode.t
    -> ?name_mode_of_existing_simple:Name_mode.t
    -> flambda_type
    -> Simple.t

  val define_code
     : t
    -> new_code_id:Code_id.t
    -> old_code_id:Code_id.t option
    -> Flambda.Code.t
    -> t

  val mem_code : t -> Code_id.t -> bool

  val find_code : t -> Code_id.t -> Flambda.Code.t option

  val code_age_relation : t -> Code_age_relation.t

  val with_code_age_relation : t -> Code_age_relation.t -> t

  val cut_and_n_way_join
     : t
    -> (t * Apply_cont_rewrite_id.t * Continuation_use_kind.t) list
    -> params:Kinded_parameter.t list
    -> unknown_if_defined_at_or_later_than:Scope.t
    -> extra_lifted_consts_in_use_envs:Symbol.Set.t
    -> extra_allowed_names:Name_occurrences.t
    -> t

  val free_names_transitive
     : t
    -> flambda_type
    -> Name_occurrences.t

  val aliases_of_simple
     : t
    -> min_name_mode:Name_mode.t
    -> Simple.t
    -> Aliases.Alias_set.t

  val clean_for_export : t -> reachable_names:Name_occurrences.t -> t

  module Serializable : sig
    type typing_env = t
    type t

    val create : typing_env -> t

    val print : Format.formatter -> t -> unit

    val to_typing_env
       : t
      -> resolver:(Compilation_unit.t -> typing_env option)
      -> get_imported_names:(unit -> Name.Set.t)
      -> get_imported_code:(unit -> Exported_code.t)
      -> typing_env

    val all_ids_for_export : t -> Ids_for_export.t

    val apply_renaming : t -> Renaming.t -> t

    val merge : t -> t -> t
  end
end

val meet : Typing_env.t -> t -> t -> (t * Typing_env_extension.t) Or_bottom.t

val meet_shape
   : Typing_env.t
  -> t
  -> shape:t
  -> result_var:Var_in_binding_pos.t
  -> result_kind:Flambda_kind.t
  -> Typing_env_extension.t Or_bottom.t

val join
   : ?bound_name:Name.t
  -> Typing_env.t
  -> left_env:Typing_env.t
  -> left_ty:t
  -> right_env:Typing_env.t
  -> right_ty:t
  -> t

(* CR mshinwell: Substitute out this alias once it's finalised *)
type 'a type_accessor = Typing_env.t -> 'a

module Function_declaration_type : sig
  module Inlinable : sig
    type t

    val code_id : t -> Code_id.t
    val rec_info : t -> flambda_type
    val must_be_inlined : t -> bool
  end

  module Non_inlinable : sig
    type t

    val code_id : t -> Code_id.t
  end

  type t0 = private
    | Inlinable of Inlinable.t
    | Non_inlinable of Non_inlinable.t

  type t = t0 Or_unknown_or_bottom.t
end

module Closures_entry : sig
  type t

  val closure_types : t -> flambda_type Closure_id.Map.t
  val function_decl_types : t -> Function_declaration_type.t Closure_id.Map.t
  val closure_var_types : t -> flambda_type Var_within_closure.Map.t
end

val free_names : t -> Name_occurrences.t

(** This function takes a type [t] and an environment [env] that assigns types
    to all the free names of [t].  It also takes an environment, called
    [suitable_for], in which we would like to use [t].  The function
    identifies which free names (if any) of [t] would be unbound in
    [suitable_for].  For each such name a fresh variable is assigned and
    irrelevantly bound in [suitable_for]; the returned type is like [t]
    except that the names that would otherwise be unbound are replaced by
    these fresh variables.  The fresh variables are assigned types in the
    returned environment extension on a best effort basis. *)
(* CR mshinwell: update comment *)
val make_suitable_for_environment
   : t
  -> Typing_env.t
  -> suitable_for:Typing_env.t
  -> bind_to:Name.t
  -> Typing_env_extension.With_extra_variables.t

val apply_coercion : flambda_type -> Coercion.t -> flambda_type Or_bottom.t

(* Remove any information from the inside of the type, leaving only its
   outer structure, and in some cases leaving only "Unknown".  Alias types
   are only preserved if they indicate equalities to constants or symbols.
   The resulting type will be suitable for a fast join operation. *)
val eviscerate : t -> Typing_env.t -> t

(** Construct a bottom type of the given kind. *)
val bottom : Flambda_kind.t -> t

(** Construct a top ("unknown") type of the given kind. *)
val unknown : Flambda_kind.t -> t

val unknown_with_subkind : Flambda_kind.With_subkind.t -> t

(** Create an bottom type with the same kind as the given type. *)
val bottom_like : t -> t

(** Create an "unknown" type with the same kind as the given type. *)
val unknown_like : t -> t

val any_value : unit -> t

val any_tagged_immediate : unit -> t
val any_tagged_bool : unit -> t

val any_boxed_float : unit -> t
val any_boxed_int32 : unit -> t
val any_boxed_int64 : unit -> t
val any_boxed_nativeint : unit -> t

val any_naked_immediate : unit -> t
val any_naked_bool : unit -> t
val any_naked_float : unit -> t
val any_naked_int32 : unit -> t
val any_naked_int64 : unit -> t
val any_naked_nativeint : unit -> t

val any_rec_info : unit -> t

(** Building of types representing tagged / boxed values from specified
    constants. *)
val this_tagged_immediate : Targetint_31_63.t -> t
val this_boxed_float : Numeric_types.Float_by_bit_pattern.t -> t
val this_boxed_int32 : Int32.t -> t
val this_boxed_int64 : Int64.t -> t
val this_boxed_nativeint : Targetint_32_64.t -> t

val these_tagged_immediates : Targetint_31_63.Set.t -> t
val these_boxed_floats : Numeric_types.Float_by_bit_pattern.Set.t -> t
val these_boxed_int32s : Int32.Set.t -> t
val these_boxed_int64s : Int64.Set.t -> t
val these_boxed_nativeints : Targetint_32_64.Set.t -> t

(** Building of types representing untagged / unboxed values from
    specified constants. *)
val this_naked_immediate : Targetint_31_63.t -> t
val this_naked_float : Numeric_types.Float_by_bit_pattern.t -> t
val this_naked_int32 : Int32.t -> t
val this_naked_int64 : Int64.t -> t
val this_naked_nativeint : Targetint_32_64.t -> t

val this_rec_info : Rec_info_expr.t -> t

val these_naked_immediates : Targetint_31_63.Set.t -> t
val these_naked_floats : Numeric_types.Float_by_bit_pattern.Set.t -> t
val these_naked_int32s : Int32.Set.t -> t
val these_naked_int64s : Int64.Set.t -> t
val these_naked_nativeints : Targetint_32_64.Set.t -> t

val boxed_float_alias_to : naked_float:Variable.t -> t
val boxed_int32_alias_to : naked_int32:Variable.t -> t
val boxed_int64_alias_to : naked_int64:Variable.t -> t
val boxed_nativeint_alias_to : naked_nativeint:Variable.t -> t

val box_float : t -> t
val box_int32 : t -> t
val box_int64 : t -> t
val box_nativeint : t -> t

val tagged_immediate_alias_to : naked_immediate:Variable.t -> t
val tag_immediate : t -> t

val is_int_for_scrutinee : scrutinee:Simple.t -> t
val get_tag_for_block : block:Simple.t -> t

val any_block : unit -> t

(* CR mshinwell: decide on exact strategy for mutable blocks *)

(** The type of an immutable block with a known tag, size and field types. *)
val immutable_block
   : is_unique:bool
  -> Tag.t
  -> field_kind:Flambda_kind.t
  -> fields:t list
  -> t

(** The type of an immutable block with at least [n] fields and an unknown
    tag. The type of the [n - 1]th field is taken to be an [Equals] to the
    given variable. *)
val immutable_block_with_size_at_least
   : tag:Tag.t Or_unknown.t
  -> n:Targetint_31_63.Imm.t
  -> field_kind:Flambda_kind.t
  -> field_n_minus_one:Variable.t
  -> t

val variant
   : const_ctors:t
  -> non_const_ctors:t list Tag.Scannable.Map.t
  -> t

val open_variant_from_const_ctors_type : const_ctors:t -> t

val open_variant_from_non_const_ctor_with_size_at_least
   : n:Targetint_31_63.Imm.t
  -> field_n_minus_one:Variable.t
  -> t

val this_immutable_string : string -> t

val mutable_string : size:int -> t

(* CR lmaurer: Returning a pair here seems clumsy. Could also keep the inlining
   decision in the [Function_declaration_type.t] at the cost of a bit more
   storage that hangs around long after its only use. *)

(** Create a description of a function declaration whose code is known.
    It may be considered inlinable. *)
val create_function_declaration
   : code:Flambda.Code.t
  -> rec_info:t
  -> Function_declaration_type.t * Function_decl_inlining_decision.t

(** Create a description of a function declaration whose code is unknown.
    Such declarations cannot be inlined, but can be direct called. *)
val create_non_inlinable_function_declaration
   : code_id:Code_id.t
  -> Function_declaration_type.t

val exactly_this_closure
   : Closure_id.t
  -> all_function_decls_in_set:Function_declaration_type.t Closure_id.Map.t
  -> all_closures_in_set:t Closure_id.Map.t
  -> all_closure_vars_in_set:flambda_type Var_within_closure.Map.t
  -> flambda_type

val at_least_the_closures_with_ids
   : this_closure:Closure_id.t
  -> Simple.t Closure_id.Map.t
  -> flambda_type

val closure_with_at_least_this_closure_var
   : this_closure:Closure_id.t
  -> Var_within_closure.t
  -> closure_element_var:Variable.t
  -> flambda_type

val closure_with_at_least_these_closure_vars
   : this_closure:Closure_id.t
  -> Variable.t Var_within_closure.Map.t
  -> flambda_type

val array_of_length : length:flambda_type -> flambda_type

(** Construct a type equal to the type of the given name.  (The name
    must be present in the given environment when calling e.g. [join].) *)
val alias_type_of : Flambda_kind.t -> Simple.t -> t

(** Determine the (unique) kind of a type. *)
val kind : t -> Flambda_kind.t

val get_alias_exn : t -> Simple.t

(** For each of the kinds in an arity, create an "unknown" type. *)
val unknown_types_from_arity : Flambda_arity.t -> t list

val unknown_types_from_arity_with_subkinds
   : Flambda_arity.With_subkinds.t
  -> t list

(** For each of the kinds in an arity, create an "bottom" type. *)
val bottom_types_from_arity : Flambda_arity.t -> t list

(** Whether the given type says that a term of that type can never be
    constructed (in other words, it is [Invalid]). *)
val is_bottom : (t -> bool) type_accessor

val is_obviously_bottom : t -> bool

val type_for_const : Reg_width_const.t -> t
val kind_for_const : Reg_width_const.t -> Flambda_kind.t

type 'a proof = private
  | Proved of 'a
  | Unknown
  | Invalid

type 'a proof_allowing_kind_mismatch = private
  | Proved of 'a
  | Unknown
  | Invalid
  | Wrong_kind

(* CR mshinwell: Should remove "_equals_" from these names *)
val prove_equals_tagged_immediates
   : Typing_env.t
  -> t
  -> Targetint_31_63.Set.t proof

val prove_naked_immediates
   : Typing_env.t
  -> t
  -> Targetint_31_63.Set.t proof

val prove_equals_single_tagged_immediate
   : Typing_env.t
  -> t
  -> Targetint_31_63.t proof

val prove_naked_floats
   : Typing_env.t
  -> t
  -> Numeric_types.Float_by_bit_pattern.Set.t proof

val prove_naked_int32s : Typing_env.t -> t -> Numeric_types.Int32.Set.t proof

val prove_naked_int64s : Typing_env.t -> t -> Numeric_types.Int64.Set.t proof

val prove_naked_nativeints : Typing_env.t -> t -> Targetint_32_64.Set.t proof

type variant_like_proof = private {
  const_ctors : Targetint_31_63.Set.t Or_unknown.t;
  non_const_ctors_with_sizes : Targetint_31_63.Imm.t Tag.Scannable.Map.t;
}

val prove_variant_like
   : Typing_env.t
  -> t
  -> variant_like_proof proof_allowing_kind_mismatch

(** If [ty] is known to represent a boxed number or a tagged integer,
    [prove_is_a_boxed_number env ty] is [Proved kind]. [kind] is the kind of
    the unboxed number.

    If [ty] is known to represent something of kind value that is not a number
    [prove_is_a_boxed_number env ty] is [Invalid].

    Otherwise it is [Unknown] or [Wrong_kind] when [ty] is not of kind value.
*)
val prove_is_a_boxed_number
   : Typing_env.t
  -> t
  -> Flambda_kind.Boxable_number.t proof_allowing_kind_mismatch

val prove_is_a_tagged_immediate
   : Typing_env.t
  -> t
  -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_float
   : Typing_env.t
  -> t
  -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_int32
   : Typing_env.t
  -> t
  -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_int64
   : Typing_env.t
  -> t
  -> unit proof_allowing_kind_mismatch

val prove_is_a_boxed_nativeint
   : Typing_env.t
  -> t
  -> unit proof_allowing_kind_mismatch

val prove_boxed_floats : Typing_env.t -> t -> Float.Set.t proof
val prove_boxed_int32s : Typing_env.t -> t -> Numeric_types.Int32.Set.t proof
val prove_boxed_int64s : Typing_env.t -> t -> Numeric_types.Int64.Set.t proof
val prove_boxed_nativeints : Typing_env.t -> t -> Targetint_32_64.Set.t proof

val prove_tags_and_sizes
   : Typing_env.t
  -> t
  -> Targetint_31_63.Imm.t Tag.Map.t proof

val prove_unique_tag_and_size
   : Typing_env.t
  -> t
  -> (Tag.t * Targetint_31_63.Imm.t) proof_allowing_kind_mismatch

val prove_is_int : Typing_env.t -> t -> bool proof

(** Prove that the given type, of kind [Value], is a closures type describing
    exactly one set of closures. The function declaration type corresponding to
    such closure is returned together with its closure ID, if it is known. *)
(* CR mshinwell: Fix comment and/or function name *)
val prove_single_closures_entry
   : Typing_env.t
  -> t
  -> (Closure_id.t * Closures_entry.t * Function_declaration_type.t) proof

val prove_single_closures_entry'
   : Typing_env.t
  -> t
  -> (Closure_id.t * Closures_entry.t * Function_declaration_type.t)
       proof_allowing_kind_mismatch

val prove_strings : Typing_env.t -> t -> String_info.Set.t proof

(** Attempt to show that the provided type describes the tagged version of
    a unique naked immediate [Simple].

    This function will return [Unknown] if values of the provided type might
    sometimes, but not always, be a tagged immediate (for example if it is a
    variant type involving blocks). *)
val prove_is_always_tagging_of_simple
   : Typing_env.t
  -> min_name_mode:Name_mode.t
  -> t
  -> Simple.t proof

(** Attempt to show that the provided type _can_ describe, but might not
    always describe, the tagged version of a unique naked immediate [Simple].
    It is guaranteed that if a [Simple] is returned, the type does not
    describe any other tagged immediate. *)
val prove_could_be_tagging_of_simple
   : Typing_env.t
  -> min_name_mode:Name_mode.t
  -> t
  -> Simple.t proof

val prove_boxed_float_containing_simple
   : Typing_env.t
  -> min_name_mode:Name_mode.t
  -> t
  -> Simple.t proof

val prove_boxed_int32_containing_simple
   : Typing_env.t
  -> min_name_mode:Name_mode.t
  -> t
  -> Simple.t proof

val prove_boxed_int64_containing_simple
   : Typing_env.t
  -> min_name_mode:Name_mode.t
  -> t
  -> Simple.t proof

val prove_boxed_nativeint_containing_simple
   : Typing_env.t
  -> min_name_mode:Name_mode.t
  -> t
  -> Simple.t proof

val prove_block_field_simple
   : Typing_env.t
  -> min_name_mode:Name_mode.t
  -> t
  -> Targetint_31_63.t
  -> Simple.t proof

val prove_variant_field_simple
   : Typing_env.t
  -> min_name_mode:Name_mode.t
  -> t
  -> Tag.t
  -> Targetint_31_63.t
  -> Simple.t proof

val prove_project_var_simple
   : Typing_env.t
  -> min_name_mode:Name_mode.t
  -> t
  -> Var_within_closure.t
  -> Simple.t proof

val prove_rec_info
   : Typing_env.t
  -> t
  -> Rec_info_expr.t proof

type var_or_symbol_or_tagged_immediate = private
  | Var of Variable.t
  | Symbol of Symbol.t
  | Tagged_immediate of Targetint_31_63.t

type to_lift = (* private *) (* CR mshinwell: resurrect *)
  | Immutable_block of
      { tag : Tag.Scannable.t;
        is_unique : bool;
        fields : (var_or_symbol_or_tagged_immediate list);
      }
  | Boxed_float of Float.t
  | Boxed_int32 of Int32.t
  | Boxed_int64 of Int64.t
  | Boxed_nativeint of Targetint_32_64.t

type reification_result = private
  | Lift of to_lift  (* CR mshinwell: rename? *)
  | Lift_set_of_closures of {
      closure_id : Closure_id.t;
      function_decls : Function_declaration_type.Inlinable.t Closure_id.Map.t;
      closure_vars : Simple.t Var_within_closure.Map.t;
    }
  | Simple of Simple.t
  | Cannot_reify
  | Invalid

val reify
   : ?allowed_if_free_vars_defined_in:Typing_env.t
  -> ?additional_free_var_criterion:(Variable.t -> bool)
  -> ?disallowed_free_vars:Variable.Set.t
  -> ?allow_unique:bool
  -> Typing_env.t
  -> min_name_mode:Name_mode.t
  -> t
  -> reification_result
