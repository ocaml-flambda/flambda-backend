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

(* Type inference for the core language *)

open Asttypes
open Types

(* This variant is used for printing which type of comprehension something is
   found in; it's used by [type_forcing_context], which see. *)
type comprehension_type =
  | List_comprehension
  | Array_comprehension of mutability

(* This variant is used to print improved error messages, and does not affect
   the behavior of the typechecker itself.

   It describes possible explanation for types enforced by a keyword of the
   language; e.g. "if" requires the condition to be of type bool, and the
   then-branch to be of type unit if there is no else branch; "for" requires
   indices to be of type int, and the body to be of type unit.
*)
type type_forcing_context =
  | If_conditional
  | If_no_else_branch
  | While_loop_conditional
  | While_loop_body
  | For_loop_start_index
  | For_loop_stop_index
  | For_loop_body
  | Assert_condition
  | Sequence_left_hand_side
  | When_guard
  | Comprehension_in_iterator of comprehension_type
  | Comprehension_for_start
  | Comprehension_for_stop
  | Comprehension_when
  | Error_message_attr of string

(* The combination of a type and a "type forcing context". The intent is that it
   describes a type that is "expected" (required) by the context. If unifying
   with such a type fails, then the "explanation" field explains why it was
   required, in order to display a more enlightening error message.
*)
type type_expected = private {
  ty: type_expr;
  explanation: type_forcing_context option;
}

(* Variables in patterns *)
type pattern_variable =
  {
    pv_id: Ident.t;
    pv_uid: Uid.t;
    pv_mode: Mode.Value.l;
    pv_type: type_expr;
    pv_loc: Location.t;
    pv_as_var: bool;
    pv_attributes: Typedtree.attributes;
  }

val mk_expected:
  ?explanation:type_forcing_context ->
  type_expr ->
  type_expected

val is_nonexpansive: Typedtree.expression -> bool

module Datatype_kind : sig
  type t = Record | Variant
  val type_name : t -> string
  val label_name : t -> string
end

type wrong_name = {
  type_path: Path.t;
  kind: Datatype_kind.t;
  name: string loc;
  valid_names: string list;
}

type wrong_kind_context =
  | Pattern
  | Expression of type_forcing_context option

type wrong_kind_sort =
  | Constructor
  | Record
  | Boolean
  | List
  | Unit

type existential_restriction =
  | At_toplevel (** no existential types at the toplevel *)
  | In_group (** nor with [let ... and ...] *)
  | In_rec (** or recursive definition *)
  | With_attributes (** or [let[@any_attribute] = ...] *)
  | In_class_args (** or in class arguments [class c (...) = ...] *)
  | In_class_def (** or in [class c = let ... in ...] *)
  | In_self_pattern (** or in self pattern *)

type module_patterns_restriction =
  | Modules_allowed of { scope: int }
  | Modules_rejected
  | Modules_ignored

val type_binding:
        Env.t -> rec_flag ->
          ?force_toplevel:bool ->
          Parsetree.value_binding list ->
          Typedtree.value_binding list * Env.t
val type_let:
        existential_restriction -> Env.t -> rec_flag ->
          Parsetree.value_binding list ->
          Typedtree.value_binding list * Env.t
val type_expression:
        Env.t -> Parsetree.expression -> Typedtree.expression
val type_representable_expression:
        why:Jkind.History.concrete_creation_reason ->
        Env.t -> Parsetree.expression -> Typedtree.expression * Jkind.sort
val type_class_arg_pattern:
        string -> Env.t -> Env.t -> arg_label -> Parsetree.pattern ->
        Typedtree.pattern *
        (Ident.t * Ident.t * type_expr) list *
        Env.t * Env.t
val type_self_pattern:
        Env.t -> Parsetree.pattern ->
        Typedtree.pattern * pattern_variable list
val check_partial:
        ?lev:int -> Env.t -> type_expr ->
        Location.t -> Typedtree.value Typedtree.case list -> Typedtree.partial
val type_expect:
        Env.t -> Parsetree.expression -> type_expected -> Typedtree.expression
val type_exp:
        Env.t -> Parsetree.expression -> Typedtree.expression
val type_approx:
        Env.t -> Parsetree.expression -> type_expr -> unit
val type_argument:
        Env.t -> Parsetree.expression ->
        type_expr -> type_expr -> Typedtree.expression

val type_option_some:
        Env.t -> Parsetree.expression ->
        type_expr-> type_expr -> Typedtree.expression
val type_option_none:
        Env.t -> type_expr -> Location.t -> Typedtree.expression
val generalizable: int -> type_expr -> bool
val generalize_structure_exp: Typedtree.expression -> unit
val reset_delayed_checks: unit -> unit
val force_delayed_checks: unit -> unit

val reset_allocations: unit -> unit
val optimise_allocations: unit -> unit

val has_poly_constraint : Parsetree.pattern -> bool


val name_pattern : string -> Typedtree.pattern list -> Ident.t
val name_cases : string -> Typedtree.value Typedtree.case list -> Ident.t

(* Why are we calling [submode]? This tells us why. *)
type submode_reason =
  | Application of type_expr
      (* Check that the result of an application is a submode of the expected mode
         from the context *)

  | Other (* add more cases here for better hints *)

val escape : loc:Location.t -> env:Env.t -> reason:submode_reason -> (Mode.allowed * 'r) Mode.Value.t -> unit

val self_coercion : (Path.t * Location.t list ref) list ref

type contention_context =
  | Read_mutable
  | Write_mutable

type unsupported_stack_allocation =
  | Lazy
  | Module
  | Object
  | List_comprehension
  | Array_comprehension

type error =
  | Constructor_arity_mismatch of Longident.t * int * int
  | Constructor_labeled_arg
  | Partial_tuple_pattern_bad_type
  | Extra_tuple_label of string option * type_expr
  | Missing_tuple_label of string option * type_expr
  | Label_mismatch of Longident.t * Errortrace.unification_error
  | Pattern_type_clash :
      Errortrace.unification_error * Parsetree.pattern_desc option
      -> error
  | Or_pattern_type_clash of Ident.t * Errortrace.unification_error
  | Multiply_bound_variable of string
  | Orpat_vars of Ident.t * Ident.t list
  | Expr_type_clash of
      Errortrace.unification_error * type_forcing_context option
      * Parsetree.expression_desc option
  | Function_arity_type_clash of
      { syntactic_arity :  int;
        type_constraint : type_expr;
        trace : Errortrace.unification_error;
      }
  | Apply_non_function of {
      funct : Typedtree.expression;
      func_ty : type_expr;
      res_ty : type_expr;
      previous_arg_loc : Location.t;
      extra_arg_loc : Location.t;
    }
  | Apply_wrong_label of arg_label * type_expr * bool
  | Label_multiply_defined of string
  | Label_missing of Ident.t list
  | Label_not_mutable of Longident.t
  | Wrong_name of string * type_expected * wrong_name
  | Name_type_mismatch of
      Datatype_kind.t * Longident.t * (Path.t * Path.t) * (Path.t * Path.t) list
  | Invalid_format of string
  | Not_an_object of type_expr * type_forcing_context option
  | Non_value_object of Jkind.Violation.t * type_forcing_context option
  | Non_value_let_rec of Jkind.Violation.t * type_expr
  | Undefined_method of type_expr * string * string list option
  | Undefined_self_method of string * string list
  | Virtual_class of Longident.t
  | Private_type of type_expr
  | Private_label of Longident.t * type_expr
  | Private_constructor of constructor_description * type_expr
  | Unbound_instance_variable of string * string list
  | Instance_variable_not_mutable of string
  | Not_subtype of Errortrace.Subtype.error
  | Outside_class
  | Value_multiply_overridden of string
  | Coercion_failure of
      Errortrace.expanded_type * Errortrace.unification_error * bool
  | Not_a_function of type_expr * type_forcing_context option
  | Too_many_arguments of type_expr * type_forcing_context option
  | Abstract_wrong_label of
      { got           : arg_label
      ; expected      : arg_label
      ; expected_type : type_expr
      ; explanation   : type_forcing_context option
      }
  | Scoping_let_module of string * type_expr
  | Not_a_polymorphic_variant_type of Longident.t
  | Incoherent_label_order
  | Less_general of string * Errortrace.unification_error
  | Modules_not_allowed
  | Cannot_infer_signature
  | Not_a_packed_module of type_expr
  | Unexpected_existential of existential_restriction * string * string list
  | Invalid_interval
  | Invalid_for_loop_index
  | Invalid_comprehension_for_range_iterator_index
  | No_value_clauses
  | Exception_pattern_disallowed
  | Mixed_value_and_exception_patterns_under_guard
  | Inlined_record_escape
  | Inlined_record_expected
  | Unrefuted_pattern of Typedtree.pattern
  | Invalid_extension_constructor_payload
  | Not_an_extension_constructor
  | Probe_format
  | Probe_name_format of string
  | Probe_name_undefined of string
  (* CR-soon mshinwell: Use an inlined record *)
  | Probe_is_enabled_format
  | Extension_not_enabled : _ Language_extension.t -> error
  | Literal_overflow of string
  | Unknown_literal of string * char
  | Float32_literal of string
  | Illegal_letrec_pat
  | Illegal_letrec_expr
  | Illegal_class_expr
  | Letop_type_clash of string * Errortrace.unification_error
  | Andop_type_clash of string * Errortrace.unification_error
  | Bindings_type_clash of Errortrace.unification_error
  | Unbound_existential of Ident.t list * type_expr
  | Missing_type_constraint
  | Wrong_expected_kind of wrong_kind_sort * wrong_kind_context * type_expr
  | Expr_not_a_record_type of type_expr
  | Submode_failed of
      Mode.Value.error * submode_reason *
      Env.closure_context option *
      contention_context option *
      Env.shared_context option
  | Curried_application_complete of
      arg_label * Mode.Alloc.error * [`Prefix|`Single_arg|`Entire_apply]
  | Param_mode_mismatch of Mode.Alloc.equate_error
  | Uncurried_function_escapes of Mode.Alloc.error
  | Local_return_annotation_mismatch of Location.t
  | Function_returns_local
  | Tail_call_local_returning
  | Bad_tail_annotation of [`Conflict|`Not_a_tailcall]
  | Optional_poly_param
  | Exclave_in_nontail_position
  | Exclave_returns_not_local
  | Unboxed_int_literals_not_supported
  | Function_type_not_rep of type_expr * Jkind.Violation.t
  | Invalid_label_for_src_pos of arg_label
  | Nonoptional_call_pos_label of string
  | Cannot_stack_allocate of Env.closure_context option
  | Unsupported_stack_allocation of unsupported_stack_allocation
  | Not_allocation

exception Error of Location.t * Env.t * error
exception Error_forward of Location.error

val report_error: loc:Location.t -> Env.t -> error -> Location.error
 (** @deprecated.  Use {!Location.error_of_exn}, {!Location.print_report}. *)

(* Forward declaration, to be filled in by Typemod.type_module *)
val type_module:
  (Env.t -> Parsetree.module_expr -> Typedtree.module_expr * Shape.t) ref
(* Forward declaration, to be filled in by Typemod.type_open *)
val type_open:
  (?used_slot:bool ref -> override_flag -> Env.t -> Location.t ->
   Longident.t loc -> Path.t * Env.t)
    ref
(* Forward declaration, to be filled in by Typemod.type_open_decl *)
val type_open_decl:
  (?used_slot:bool ref -> Env.t -> Parsetree.open_declaration ->
   Typedtree.open_declaration * Types.signature * Env.t)
    ref
(* Forward declaration, to be filled in by Typeclass.class_structure *)
val type_object:
  (Env.t -> Location.t -> Parsetree.class_structure ->
   Typedtree.class_structure * string list) ref
val type_package:
  (Env.t -> Parsetree.module_expr -> Path.t -> (Longident.t * type_expr) list ->
  Typedtree.module_expr * (Longident.t * type_expr) list) ref

val constant: Parsetree.constant -> (Typedtree.constant, error) result

val annotate_recursive_bindings :
  Env.t -> Typedtree.value_binding list -> Typedtree.value_binding list
val check_recursive_class_bindings :
  Env.t -> Ident.t list -> Typedtree.class_expr list -> unit

val src_pos : Location.t -> Typedtree.attributes -> Env.t -> Typedtree.expression
