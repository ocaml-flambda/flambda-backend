(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Liam Stevenson, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Sort = struct
  type const =
    | Void
    | Value
    | Float64
    | Word
    | Bits32
    | Bits64

  type t =
    | Var of var
    | Const of const

  and var = t option ref

  type change = var * t option
end

module Layout = struct
  type ('type_expr, 'sort) layout =
    | Sort of 'sort
    | Any
    | Non_null_value

  type 'type_expr t = ('type_expr, Sort.t) layout
end

module Externality = struct
  type t =
    | External
    | External64
    | Internal
end

module Modes = struct
  include Mode.Alloc.Const
end

module Jkind_desc = struct
  type 'type_expr t =
    { layout : 'type_expr Layout.t;
      modes_upper_bounds : Modes.t;
      externality_upper_bound : Externality.t
    }
end

type concrete_jkind_reason =
  | Match
  | Constructor_declaration of int
  | Label_declaration of Ident.t
  | Unannotated_type_parameter of Path.t
  | Record_projection
  | Record_assignment
  | Let_binding
  | Function_argument
  | Function_result
  | Structure_item_expression
  | External_argument
  | External_result
  | Statement
  | Wildcard
  | Unification_var
  | Optional_arg_default
  | Layout_poly_in_external
  | Array_element

type value_creation_reason =
  | Class_let_binding
  | Tuple_element
  | Probe
  | Object
  | Instance_variable
  | Object_field
  | Class_field
  | Boxed_record
  | Boxed_variant
  | Extensible_variant
  | Primitive of Ident.t
  | Type_argument of
      { parent_path : Path.t;
        position : int;
        arity : int
      }
  | Tuple
  | Row_variable
  | Polymorphic_variant
  | Arrow
  | Tfield
  | Tnil
  | First_class_module
  | Separability_check
  | Univar
  | Polymorphic_variant_field
  | Default_type_jkind
  | Existential_type_variable
  | Array_comprehension_element
  | Lazy_expression
  | Class_type_argument
  | Class_term_argument
  | Structure_element
  | Debug_printer_argument
  | V1_safety_check
  | Captured_in_object
  | Recmod_fun_arg
  | Unknown of string

type immediate_creation_reason =
  | Empty_record
  | Enumeration
  | Primitive of Ident.t
  | Immediate_polymorphic_variant

type immediate64_creation_reason = Separability_check

type void_creation_reason = |

type any_creation_reason =
  | Missing_cmi of Path.t
  | Initial_typedecl_env
  | Dummy_jkind
  | Type_expression_call
  | Inside_of_Tarrow
  | Wildcard
  | Unification_var
  | Array_type_argument

type float64_creation_reason = Primitive of Ident.t

type word_creation_reason = Primitive of Ident.t

type bits32_creation_reason = Primitive of Ident.t

type bits64_creation_reason = Primitive of Ident.t

type annotation_context =
  | Type_declaration of Path.t
  | Type_parameter of Path.t * string option
  | Newtype_declaration of string
  | Constructor_type_parameter of Path.t * string
  | Univar of string
  | Type_variable of string
  | Type_wildcard of Location.t
  | With_error_message of string * annotation_context

type creation_reason =
  | Annotated of annotation_context * Location.t
  | Missing_cmi of Path.t
  | Value_creation of value_creation_reason
  | Immediate_creation of immediate_creation_reason
  | Immediate64_creation of immediate64_creation_reason
  | Void_creation of void_creation_reason
  | Any_creation of any_creation_reason
  | Float64_creation of float64_creation_reason
  | Word_creation of word_creation_reason
  | Bits32_creation of bits32_creation_reason
  | Bits64_creation of bits64_creation_reason
  | Concrete_creation of concrete_jkind_reason
  | Imported
  | Imported_type_argument of
      { parent_path : Path.t;
        position : int;
        arity : int
      }
  | Generalized of Ident.t option * Location.t

type interact_reason =
  | Gadt_equation of Path.t
  | Tyvar_refinement_intersection
  | Subjkind

type 'type_expr history =
  | Interact of
      { reason : interact_reason;
        lhs_jkind : 'type_expr Jkind_desc.t;
        lhs_history : 'type_expr history;
        rhs_jkind : 'type_expr Jkind_desc.t;
        rhs_history : 'type_expr history
      }
  | Creation of creation_reason

type 'type_expr t =
  { jkind : 'type_expr Jkind_desc.t;
    history : 'type_expr history
  }

type const =
  | Any
  | Value
  | Void
  | Immediate64
  | Immediate
  | Float64
  | Word
  | Bits32
  | Bits64
  | Non_null_value

type annotation = const * Jane_asttypes.jkind_annotation
