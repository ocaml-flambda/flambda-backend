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

open! Flambda.Import

type resolver = Compilation_unit.t -> Flambda_type.Typing_env.t option

type get_imported_names = unit -> Name.Set.t

type get_imported_code = unit -> Exported_code.t

type t

val invariant : t -> unit

(** Print a human-readable version of the given environment. *)
val print : Format.formatter -> t -> unit

(** Create a new environment, marked as being at the toplevel of a compilation
    unit. *)
val create :
  round:int ->
  backend:(module Flambda_backend_intf.S) ->
  resolver:resolver ->
  get_imported_names:get_imported_names ->
  get_imported_code:get_imported_code ->
  float_const_prop:bool ->
  unit_toplevel_exn_continuation:Continuation.t ->
  unit_toplevel_return_continuation:Continuation.t ->
  t

(** Obtain the first-class module that gives information about the compiler
    backend being used for compilation. *)
val backend : t -> (module Flambda_backend_intf.S)

val resolver : t -> Compilation_unit.t -> Flambda_type.Typing_env.t option

val float_const_prop : t -> bool

val at_unit_toplevel : t -> bool

val set_not_at_unit_toplevel : t -> t

val set_at_unit_toplevel_state : t -> bool -> t

val is_defined_at_toplevel : t -> Variable.t -> bool

val add_symbol_projection : t -> Variable.t -> Symbol_projection.t -> t

val find_symbol_projection : t -> Variable.t -> Symbol_projection.t option

val unit_toplevel_return_continuation : t -> Continuation.t

val unit_toplevel_exn_continuation : t -> Continuation.t

val enter_set_of_closures : t -> t

val increment_continuation_scope_level : t -> t

val increment_continuation_scope_level_twice : t -> t

val get_continuation_scope_level : t -> Scope.t

val now_defining_symbol : t -> Symbol.t -> t

val no_longer_defining_symbol : t -> Symbol.t -> t

val symbol_is_currently_being_defined : t -> Symbol.t -> bool

val symbols_currently_being_defined : t -> Symbol.Set.t

val typing_env : t -> Flambda_type.Typing_env.t

val define_variable : t -> Var_in_binding_pos.t -> Flambda_kind.t -> t

val add_name : t -> Name_in_binding_pos.t -> Flambda_type.t -> t

val add_variable : t -> Var_in_binding_pos.t -> Flambda_type.t -> t

val add_equation_on_variable : t -> Variable.t -> Flambda_type.t -> t

val mem_variable : t -> Variable.t -> bool

val add_symbol : t -> Symbol.t -> Flambda_type.t -> t

val define_symbol : t -> Symbol.t -> Flambda_kind.t -> t

val define_symbol_if_undefined : t -> Symbol.t -> Flambda_kind.t -> t

val mem_symbol : t -> Symbol.t -> bool

val find_symbol : t -> Symbol.t -> Flambda_type.t

val add_equation_on_symbol : t -> Symbol.t -> Flambda_type.t -> t

val define_name : t -> Name_in_binding_pos.t -> Flambda_kind.t -> t

val define_name_if_undefined : t -> Name_in_binding_pos.t -> Flambda_kind.t -> t

val add_equation_on_name : t -> Name.t -> Flambda_type.t -> t

val define_parameters : t -> params:Kinded_parameter.t list -> t

val define_parameters_as_bottom : t -> params:Kinded_parameter.t list -> t

val add_parameters :
  ?at_unit_toplevel:bool ->
  t ->
  Kinded_parameter.t list ->
  param_types:Flambda_type.t list ->
  t

val add_parameters_with_unknown_types :
  ?at_unit_toplevel:bool -> t -> Kinded_parameter.t list -> t

val add_parameters_with_unknown_types' :
  ?at_unit_toplevel:bool ->
  t ->
  Kinded_parameter.t list ->
  t * Flambda_type.t list

val mark_parameters_as_toplevel : t -> Kinded_parameter.t list -> t

val add_variable_and_extend_typing_environment :
  t ->
  Var_in_binding_pos.t ->
  Flambda_type.t ->
  Flambda_type.Typing_env_extension.t ->
  t

val with_typing_env : t -> Flambda_type.Typing_env.t -> t

val map_typing_env :
  t -> f:(Flambda_type.Typing_env.t -> Flambda_type.Typing_env.t) -> t

val check_variable_is_bound : t -> Variable.t -> unit

val check_symbol_is_bound : t -> Symbol.t -> unit

val check_name_is_bound : t -> Name.t -> unit

val check_simple_is_bound : t -> Simple.t -> unit

val check_code_id_is_bound : t -> Code_id.t -> unit

val define_code : t -> code_id:Code_id.t -> code:Code.t -> t

val mem_code : t -> Code_id.t -> bool

val find_code : t -> Code_id.t -> Code.t option

(** Appends the locations of inlined call-sites to the given debuginfo and sets
    the resulting debuginfo as the current one in the environment. *)
val add_inlined_debuginfo : t -> Debuginfo.t -> t
(* CR mshinwell: remove? *)

val set_inlined_debuginfo : t -> Debuginfo.t -> t

val add_inlined_debuginfo' : t -> Debuginfo.t -> Debuginfo.t

val get_inlined_debuginfo : t -> Debuginfo.t

val round : t -> int

(** Prevent function inlining from occurring in the given environment. *)
val disable_function_inlining : t -> t

val can_inline : t -> bool

val set_inlining_state : t -> Inlining_state.t -> t

val get_inlining_state : t -> Inlining_state.t

val add_cse :
  t -> Flambda_primitive.Eligible_for_cse.t -> bound_to:Simple.t -> t

val find_cse : t -> Flambda_primitive.Eligible_for_cse.t -> Simple.t option

val cse : t -> Common_subexpression_elimination.t

val with_cse : t -> Common_subexpression_elimination.t -> t

(* The following are only used for getting use information out of
   [Simplify_unary_primitive.simplify_project_var], to avoid that file depending
   on [DA]. The main closure var use accumulator is in [DA]. *)
val add_use_of_closure_var : t -> Var_within_closure.t -> t

val closure_var_uses : t -> Var_within_closure.Set.t

val without_closure_var_uses : t -> t

val set_do_not_rebuild_terms_and_disable_inlining : t -> t

val set_rebuild_terms : t -> t

type are_rebuilding_terms

val are_rebuilding_terms : t -> are_rebuilding_terms

val are_rebuilding_terms_to_bool : are_rebuilding_terms -> bool

val enter_closure : Code_id.t -> Continuation.t -> Exn_continuation.t -> t -> t

val closure_info : t -> Closure_info.t

val inlining_arguments : t -> Inlining_arguments.t

val set_inlining_arguments : Inlining_arguments.t -> t -> t

val enter_inlined_apply : called_code:Code.t -> apply:Apply.t -> t -> t

val generate_phantom_lets : t -> bool
