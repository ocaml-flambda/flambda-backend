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

type typing_env

type t = typing_env

module Pre_serializable : sig
  type t

  val create :
    typing_env ->
    used_value_slots:Value_slot.Set.t ->
    t * (Simple.t -> Simple.t)

  val find_or_missing : t -> Name.t -> Type_grammar.t option
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

module Join_env : sig
  type t

  val print : Format.formatter -> t -> unit

  val create : typing_env -> left_env:typing_env -> right_env:typing_env -> t

  val target_join_env : t -> typing_env

  val left_join_env : t -> typing_env

  val right_join_env : t -> typing_env

  type now_joining_result = private
    | Continue of t
    | Stop

  val now_joining : t -> Simple.t -> Simple.t -> now_joining_result

  val already_joining : t -> Simple.t -> Simple.t -> bool
end

type 'a meet_return_value =
  | Left_input
  | Right_input
  | Both_inputs
  | New_result of 'a

type meet_type =
  t ->
  Type_grammar.t ->
  Type_grammar.t ->
  (Type_grammar.t meet_return_value * t) Or_bottom.t

val print : Format.formatter -> t -> unit

val create :
  resolver:(Compilation_unit.t -> Serializable.t option) ->
  get_imported_names:(unit -> Name.Set.t) ->
  t

val is_bottom : t -> bool

val closure_env : t -> t

val resolver : t -> Compilation_unit.t -> Serializable.t option

val code_age_relation_resolver :
  t -> Compilation_unit.t -> Code_age_relation.t option

val current_scope : t -> Scope.t

val increment_scope : t -> t

val add_definition : t -> Bound_name.t -> Flambda_kind.t -> t

val add_equation : t -> Name.t -> Type_grammar.t -> meet_type:meet_type -> t

val add_equation_strict :
  t -> Name.t -> Type_grammar.t -> meet_type:meet_type -> t Or_bottom.t

val add_definitions_of_params : t -> params:Bound_parameters.t -> t

val add_symbol_definition : t -> Symbol.t -> t

val add_symbol_definitions : t -> Symbol.Set.t -> t

val add_symbol_projection : t -> Variable.t -> Symbol_projection.t -> t

val find_symbol_projection : t -> Variable.t -> Symbol_projection.t option

val add_equations_on_params :
  t ->
  params:Bound_parameters.t ->
  param_types:Type_grammar.t list ->
  meet_type:meet_type ->
  t

(** If the kind of the name is known, it should be specified, otherwise it can
    be omitted. Such omission will cause an error if the name satisfies
    [variable_is_from_missing_cmx_file]. *)
val find : t -> Name.t -> Flambda_kind.t option -> Type_grammar.t

val find_or_missing : t -> Name.t -> Type_grammar.t option

val find_params : t -> Bound_parameters.t -> Type_grammar.t list

val variable_is_from_missing_cmx_file : t -> Name.t -> bool

val mem : ?min_name_mode:Name_mode.t -> t -> Name.t -> bool

val mem_simple : ?min_name_mode:Name_mode.t -> t -> Simple.t -> bool

val alias_is_bound_strictly_earlier :
  t -> bound_name:Name.t -> alias:Simple.t -> bool

(* CR vlaviron: If the underlying level in the extension defines several
   variables, then there is no guarantee that the binding order in the result
   will match the binding order used to create the level. If they don't match,
   then adding equations in the wrong order can make equations disappear. *)
val add_env_extension : t -> Typing_env_extension.t -> meet_type:meet_type -> t

val add_env_extension_maybe_bottom :
  t -> Typing_env_extension.t -> meet_type:meet_type -> t

val add_env_extension_strict :
  t -> Typing_env_extension.t -> meet_type:meet_type -> t Or_bottom.t

val add_env_extension_with_extra_variables :
  t -> Typing_env_extension.With_extra_variables.t -> meet_type:meet_type -> t

val add_env_extension_from_level :
  t -> Typing_env_level.t -> meet_type:meet_type -> t

val type_simple_in_term_exn :
  t -> ?min_name_mode:Name_mode.t -> Simple.t -> Type_grammar.t * Simple.t

(** [name_mode_of_existing_simple] can be provided to improve performance of
    this function. *)
val get_canonical_simple_exn :
  t ->
  ?min_name_mode:Name_mode.t ->
  ?name_mode_of_existing_simple:Name_mode.t ->
  Simple.t ->
  Simple.t

val get_alias_then_canonical_simple_exn :
  t ->
  ?min_name_mode:Name_mode.t ->
  ?name_mode_of_existing_simple:Name_mode.t ->
  Type_grammar.t ->
  Simple.t

val aliases_of_simple :
  t -> min_name_mode:Name_mode.t -> Simple.t -> Aliases.Alias_set.t

val aliases_of_simple_allowable_in_types : t -> Simple.t -> Aliases.Alias_set.t

val add_to_code_age_relation :
  t -> new_code_id:Code_id.t -> old_code_id:Code_id.t option -> t

val code_age_relation : t -> Code_age_relation.t

val with_code_age_relation : t -> Code_age_relation.t -> t

val cut : t -> cut_after:Scope.t -> Typing_env_level.t

val cut_as_extension : t -> cut_after:Scope.t -> Typing_env_extension.t

val free_names_transitive : t -> Type_grammar.t -> Name_occurrences.t

val bump_current_level_scope : t -> t

val compute_joined_aliases : t -> Name.Set.t -> t list -> t
