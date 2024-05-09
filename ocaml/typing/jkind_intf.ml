(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*               Richard Eisenberg, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2024 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This module contains definitions that we do not otherwise need to repeat
   between the various Jkind modules. See comment in jkind_types.mli. *)

module type Sort = sig
  (** A sort classifies how a type is represented at runtime. Every concrete
      jkind has a sort, and knowing the sort is sufficient for knowing the
      calling convention of values of a given type. *)
  type t

  (** These are the constant sorts -- fully determined and without variables *)
  type const =
    | Void  (** No run time representation at all *)
    | Value  (** Standard ocaml value representation *)
    | Float64  (** Unboxed 64-bit floats *)
    | Word  (** Unboxed native-size integers *)
    | Bits32  (** Unboxed 32-bit integers *)
    | Bits64  (** Unboxed 64-bit integers *)

  (** A sort variable that can be unified during type-checking. *)
  type var

  (** Create a new sort variable that can be unified. *)
  val new_var : unit -> t

  val of_const : const -> t

  val of_var : var -> t

  val void : t

  val value : t

  val float64 : t

  val word : t

  val bits32 : t

  val bits64 : t

  (** These names are generated lazily and only when this function is called,
      and are not guaranteed to be efficient to create *)
  val var_name : var -> string

  (** This checks for equality, and sets any variables to make two sorts
      equal, if possible *)
  val equate : t -> t -> bool

  val equal_const : const -> const -> bool

  val format : Format.formatter -> t -> unit

  val format_const : Format.formatter -> const -> unit

  (** Defaults any variables to value; leaves other sorts alone *)
  val default_to_value : t -> unit

  (** Checks whether this sort is [void], defaulting to [value] if a sort
      variable is unfilled. *)
  val is_void_defaulting : t -> bool

  (** [get_default_value] extracts the sort as a `const`.  If it's a variable,
      it is set to [value] first. *)
  val get_default_value : t -> const

  (** To record changes to sorts, for use with `Types.{snapshot, backtrack}` *)
  type change

  val undo_change : change -> unit

  module Debug_printers : sig
    val t : Format.formatter -> t -> unit

    val var : Format.formatter -> var -> unit
  end

  (* CR layouts: These are sorts for the types of ocaml expressions that are
     currently required to be values, but for which we expect to relax that
     restriction in versions 2 and beyond.  Naming them makes it easy to find
     where in the translation to lambda they are assume to be value. *)
  (* CR layouts: add similarly named jkinds and use those names everywhere (not
     just the translation to lambda) rather than writing specific jkinds and
     sorts in the code. *)
  val for_class_arg : t

  val for_instance_var : t

  val for_lazy_body : t

  val for_tuple_element : t

  val for_variant_arg : t

  val for_record : t

  val for_block_element : t

  val for_array_get_result : t

  val for_array_comprehension_element : t

  val for_list_element : t

  (** These are sorts for the types of ocaml expressions that we expect will
      always be "value".  These names are used in the translation to lambda to
      make the code clearer. *)
  val for_function : t

  val for_probe_body : t

  val for_poly_variant : t

  val for_object : t

  val for_initializer : t

  val for_method : t

  val for_module : t

  val for_predef_value : t (* Predefined value types, e.g. int and string *)

  val for_tuple : t
end

module History = struct
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

  type annotation_context =
    | Type_declaration of Path.t
    | Type_parameter of Path.t * string option
    | Newtype_declaration of string
    | Constructor_type_parameter of Path.t * string
    | Univar of string
    | Type_variable of string
    | Type_wildcard of Location.t
    | With_error_message of string * annotation_context

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
    (* [position] is 1-indexed *)
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
    | Unknown of string (* CR layouts: get rid of these *)

  type immediate_creation_reason =
    | Empty_record
    | Enumeration
    | Primitive of Ident.t
    | Immediate_polymorphic_variant

  type immediate64_creation_reason = Separability_check

  (* CR layouts v5: make new void_creation_reasons *)
  type void_creation_reason = |

  type any_creation_reason =
    | Missing_cmi of Path.t
    | Initial_typedecl_env
    | Dummy_jkind
      (* This is used when the jkind is about to get overwritten;
         key example: when creating a fresh tyvar that is immediately
         unified to correct levels *)
    | Type_expression_call
    | Inside_of_Tarrow
    | Wildcard
    | Unification_var
    | Array_type_argument

  type float64_creation_reason = Primitive of Ident.t

  type word_creation_reason = Primitive of Ident.t

  type bits32_creation_reason = Primitive of Ident.t

  type bits64_creation_reason = Primitive of Ident.t

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
    (* [position] is 1-indexed *)
    | Generalized of Ident.t option * Location.t

  type interact_reason =
    | Gadt_equation of Path.t
    | Tyvar_refinement_intersection
    (* CR layouts: this needs to carry a type_expr, but that's loopy *)
    | Subjkind
end
