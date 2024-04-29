(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Typing of type definitions and primitive definitions *)

open Types
open Format

val transl_type_decl:
    Env.t -> Asttypes.rec_flag -> Parsetree.type_declaration list ->
    Typedtree.type_declaration list * Env.t * Shape.t list

val transl_exception:
    Env.t -> Parsetree.extension_constructor ->
    Typedtree.extension_constructor * Env.t * Shape.t

val transl_type_exception:
    Env.t ->
    Parsetree.type_exception -> Typedtree.type_exception * Env.t * Shape.t

val transl_type_extension:
    bool -> Env.t -> Location.t -> Parsetree.type_extension ->
    Typedtree.type_extension * Env.t * Shape.t list

val transl_value_decl:
    Env.t -> Location.t ->
    Parsetree.value_description -> Typedtree.value_description * Env.t

(* If the [fixed_row_path] optional argument is provided,
   the [Parsetree.type_declaration] argument should satisfy [is_fixed_type] *)
val transl_with_constraint:
    Ident.t -> ?fixed_row_path:Path.t ->
    sig_env:Env.t -> sig_decl:Types.type_declaration ->
    outer_env:Env.t -> Parsetree.type_declaration ->
    Typedtree.type_declaration

val transl_package_constraint:
  loc:Location.t -> type_expr -> Types.type_declaration

val abstract_type_decl:
  injective:bool ->
  jkind:Jkind.t ->
  (* [jkind_annotation] is what the user wrote, and is just used when printing
     the type produced by this function. *)
  jkind_annotation:Jkind.annotation option ->
  params:Jkind.t list ->
  type_declaration

val approx_type_decl:
    Parsetree.type_declaration list -> (Ident.t * type_declaration) list
val check_recmod_typedecl:
    Env.t -> Location.t -> Ident.t list -> Path.t -> type_declaration -> unit

(* Returns an updated decl that may include improved jkind estimates, but it's
   sound to throw it away. *)
val check_coherence:
    Env.t -> Location.t -> Path.t -> type_declaration -> type_declaration

(* for fixed types *)
val is_fixed_type : Parsetree.type_declaration -> bool

type native_repr_kind = Unboxed | Untagged

(* Records reason for a jkind representability requirement in errors. *)
type jkind_sort_loc =
  | Cstr_tuple
  | Record
  | Unboxed_record
  | External
  | External_with_layout_poly

type reaching_type_path = reaching_type_step list
and reaching_type_step =
  | Expands_to of type_expr * type_expr
  | Contains of type_expr * type_expr

type mixed_record_violation =
  | Runtime_support_not_enabled
  | Value_prefix_too_long of
      { value_prefix_len : int;
        max_value_prefix_len : int;
      }
  | Flat_field_expected of
      { boxed_lbl : Ident.t;
        non_value_lbl : Ident.t;
      }
  | Insufficient_level of
      { required_layouts_level : Language_extension.maturity }

type bad_jkind_inference_location =
  | Check_constraints
  | Delayed_checks

type error =
    Repeated_parameter
  | Duplicate_constructor of string
  | Too_many_constructors
  | Duplicate_label of string
  | Recursive_abbrev of string * Env.t * reaching_type_path
  | Cycle_in_def of string * Env.t * reaching_type_path
  | Definition_mismatch of type_expr * Env.t * Includecore.type_mismatch option
  | Constraint_failed of Env.t * Errortrace.unification_error
  | Inconsistent_constraint of Env.t * Errortrace.unification_error
  | Type_clash of Env.t * Errortrace.unification_error
  | Non_regular of {
      definition: Path.t;
      used_as: type_expr;
      defined_as: type_expr;
      reaching_path: reaching_type_path;
    }
  | Null_arity_external
  | Missing_native_external
  | Unbound_type_var of type_expr * type_declaration
  | Cannot_extend_private_type of Path.t
  | Not_extensible_type of Path.t
  | Extension_mismatch of Path.t * Env.t * Includecore.type_mismatch
  | Rebind_wrong_type of
      Longident.t * Env.t * Errortrace.unification_error
  | Rebind_mismatch of Longident.t * Path.t * Path.t
  | Rebind_private of Longident.t
  | Variance of Typedecl_variance.error
  | Unavailable_type_constructor of Path.t
  | Unbound_type_var_ext of type_expr * extension_constructor
  | Val_in_structure
  | Multiple_native_repr_attributes
  | Cannot_unbox_or_untag_type of native_repr_kind
  | Deep_unbox_or_untag_attribute of native_repr_kind
  | Jkind_mismatch_of_type of type_expr * Jkind.Violation.t
  | Jkind_mismatch_of_path of Path.t * Jkind.Violation.t
  | Jkind_mismatch_due_to_bad_inference of
      type_expr * Jkind.Violation.t * bad_jkind_inference_location
  | Jkind_sort of
      { kloc : jkind_sort_loc
      ; typ : type_expr
      ; err : Jkind.Violation.t
      }
  | Jkind_empty_record
  | Non_value_in_sig of Jkind.Violation.t * string * type_expr
  | Invalid_jkind_in_block of type_expr * Jkind.Sort.const * jkind_sort_loc
  | Illegal_mixed_record of mixed_record_violation
  | Separability of Typedecl_separability.error
  | Bad_unboxed_attribute of string
  | Boxed_and_unboxed
  | Nonrec_gadt
  | Invalid_private_row_declaration of type_expr
  | Local_not_enabled
  | Unexpected_jkind_any_in_primitive of string
  | Useless_layout_poly
  | Modalities_on_value_description
  | Missing_unboxed_attribute_on_non_value_sort of Jkind.Sort.const
  | Non_value_sort_not_upstream_compatible of Jkind.Sort.const
  | Zero_alloc_attr_unsupported of Builtin_attributes.check_attribute
  | Zero_alloc_attr_non_function
  | Zero_alloc_attr_bad_user_arity

exception Error of Location.t * error

val report_error: formatter -> error -> unit
