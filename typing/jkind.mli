(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                Chris Casinghino, Jane Street, New York                 *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* This module is named Jkind, with a 'y', to distinguish jkinds
   as used here from type kinds (which might be abstract or record or variant,
   etc.). This is clearly far from ideal, but the current scheme has these
   positives:

   * It allows us to call jkinds "kinds" to users, connecting that word with
     a word that actually appears in the code.

   * It allows us to use "layout" to refer to the component of a jkind that
     describes the in-memory/in-register layout of a type. Using "layout"
     to refer to the whole jkind seems worse than the already-terrible "jkind".

   * We could imagine renaming the existing "kind" to something else ("shape"?),
     but that would introduce merge conflicts. Perhaps, with broad support from
     OCaml developers, we can make this switch someday.

   * It is very easy to search for and replace when we have a better name.
*)

module Sort : sig
  (** A sort classifies how a type is represented at runtime. Every concrete
      jkind has a sort, and knowing the sort is sufficient for knowing the
      calling convention of values of a given type. *)
  type t

  (** These are the constant sorts -- fully determined and without variables *)
  type const =
    | Void  (** No run time representation at all *)
    | Value  (** Standard ocaml value representation *)
    | Float64  (** Unboxed 64-bit floats *)

  (** A sort variable that can be unified during type-checking. *)
  type var

  (** Create a new sort variable that can be unified. *)
  val new_var : unit -> t

  val of_const : const -> t

  val of_var : var -> t

  val void : t

  val value : t

  val float64 : t

  (** These names are generated lazily and only when this function is called,
      and are not guaranteed to be efficient to create *)
  val var_name : var -> string

  (** This checks for equality, and sets any variables to make two sorts
      equal, if possible *)
  val equate : t -> t -> bool

  val equal_const : const -> const -> bool

  val format : Format.formatter -> t -> unit

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

  val change_log : (change -> unit) ref

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

  val for_record : t

  val for_constructor_arg : t

  val for_block_element : t

  val for_array_get_result : t

  val for_array_element : t

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

type sort = Sort.t

(* This module describes jkinds, which classify types. Jkinds are arranged
   in the following lattice:

   {[
               any
             /    \
          value  void
            |
        immediate64
            |
        immediate
   ]}
*)

(** A Jkind.t is a full description of the runtime representation of values
    of a given type. It includes sorts, but also the abstract top jkind
    [Any] and subjkinds of other sorts, such as [Immediate]. *)
type t

(******************************)
(* errors *)

type concrete_jkind_reason =
  | Match
  | Constructor_declaration of int
  | Label_declaration of Ident.t
  | Unannotated_type_parameter
  | Record_projection
  | Record_assignment
  | Let_binding
  | Function_argument
  | Function_result
  | Structure_item_expression
  | V1_safety_check
  | External_argument
  | External_result
  | Statement

type annotation_context =
  | Type_declaration of Path.t
  | Type_parameter of Path.t * string option
  | With_constraint of string
  | Newtype_declaration of string
  | Constructor_type_parameter of Path.t * string
  | Univar of string
  | Type_variable of string
  | Type_wildcard of Location.t

type value_creation_reason =
  | Class_let_binding
  | Tuple_element
  | Probe
  | Package_hack
  | Object
  | Instance_variable
  | Object_field
  | Class_field
  | Boxed_record
  | Boxed_variant
  | Extensible_variant
  | Primitive of Ident.t
  | Type_argument (* CR layouts: Should this take a Path.t? *)
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
  | Float_record_field
  | Existential_type_variable
  | Array_element
  | Lazy_expression
  | Class_argument
  | Structure_element
  | Debug_printer_argument
  | V1_safety_check
  | Captured_in_object
  | Unknown of string (* CR layouts: get rid of these *)

type immediate_creation_reason =
  | Empty_record
  | Enumeration
  | Primitive of Ident.t
  | Immediate_polymorphic_variant
  | Gc_ignorable_check
  | Value_kind

type immediate64_creation_reason =
  | Local_mode_cross_check
  | Gc_ignorable_check
  | Separability_check

type void_creation_reason = V1_safety_check

type any_creation_reason =
  | Missing_cmi of Path.t
  | Wildcard
  | Unification_var
  | Initial_typedecl_env
  | Dummy_jkind
    (* This is used when the jkind is about to get overwritten;
       key example: when creating a fresh tyvar that is immediately
       unified to correct levels *)
  | Type_expression_call

type float64_creation_reason = Primitive of Ident.t

type creation_reason =
  | Annotated of annotation_context * Location.t
  | Value_creation of value_creation_reason
  | Immediate_creation of immediate_creation_reason
  | Immediate64_creation of immediate64_creation_reason
  | Void_creation of void_creation_reason
  | Any_creation of any_creation_reason
  | Float64_creation of float64_creation_reason
  | Concrete_creation of concrete_jkind_reason
  | Imported

type interact_reason =
  | Gadt_equation of Path.t
  | Tyvar_refinement_intersection
  (* CR layouts: this needs to carry a type_expr, but that's loopy *)
  | Subjkind

module Violation : sig
  type violation =
    | Not_a_subjkind of t * t
    | No_intersection of t * t

  type t

  val of_ : violation -> t

  (** Mark a [t] as having arisen from a missing cmi *)
  val record_missing_cmi : missing_cmi_for:Path.t -> t -> t

  (** Is this error from a missing cmi? *)
  val is_missing_cmi : t -> bool

  (* CR layouts: The [offender] arguments below are always
     [Printtyp.type_expr], so we should either stash that in a ref (like with
     [set_printtyp_path] below) or just move all the printing machinery
     downstream of both [Jkinds] and [Printtyp]. *)

  (* CR layouts: Having these options for printing a violation was a choice
     made based on the needs of expedient debugging during development, but
     probably should be rethought at some point. *)

  (** Prints a violation and the thing that had an unexpected jkind
      ([offender], which you supply an arbitrary printer for). *)
  val report_with_offender :
    offender:(Format.formatter -> unit) -> Format.formatter -> t -> unit

  (** Like [report_with_offender], but additionally prints that the issue is
      that a representable jkind was expected. *)
  val report_with_offender_sort :
    offender:(Format.formatter -> unit) -> Format.formatter -> t -> unit

  (** Simpler version of [report_with_offender] for when the thing that had an
      unexpected jkind is available as a string. *)
  val report_with_name : name:string -> Format.formatter -> t -> unit
end

(******************************)
(* constants *)

(** Constant jkinds are used both for user-written annotations and within
    the type checker when we know a jkind has no variables *)
type const = Jane_asttypes.const_jkind =
  | Any
  | Value
  | Void
  | Immediate64
  | Immediate
  | Float64

val string_of_const : const -> string

val equal_const : const -> const -> bool

(** This jkind is the top of the jkind lattice. All types have jkind [any].
    But we cannot compile run-time manipulations of values of types with jkind
    [any]. *)
val any : why:any_creation_reason -> t

(** Value of types of this jkind are not retained at all at runtime *)
val void : why:void_creation_reason -> t

(** This is the jkind of normal ocaml values *)
val value : why:value_creation_reason -> t

(** Values of types of this jkind are immediate on 64-bit platforms; on other
    platforms, we know nothing other than that it's a value. *)
val immediate64 : why:immediate64_creation_reason -> t

(** We know for sure that values of types of this jkind are always immediate *)
val immediate : why:immediate_creation_reason -> t

(** This is the jkind of unboxed 64-bit floats.  They have sort Float64. *)
val float64 : why:float64_creation_reason -> t

(******************************)
(* construction *)

(** Create a fresh sort variable, packed into a jkind, returning both
    the resulting kind and the sort. *)
val of_new_sort_var : why:concrete_jkind_reason -> t * sort

(** Create a fresh sort variable, packed into a jkind. *)
val of_new_sort : why:concrete_jkind_reason -> t

val of_const : why:creation_reason -> const -> t

(* CR layouts v1.5: remove legacy_immediate when the old attributes mechanism
   is rerouted away from the new annotations mechanism *)
val of_annotation :
  ?legacy_immediate:bool ->
  context:annotation_context ->
  Jane_asttypes.jkind_annotation ->
  t

val of_annotation_option_default :
  ?legacy_immediate:bool ->
  default:t ->
  context:annotation_context ->
  Jane_asttypes.jkind_annotation option ->
  t

(** Find a jkind in attributes.  Returns error if a disallowed jkind is
    present, but always allows immediate attributes if ~legacy_immediate is
    true.  See comment on [Builtin_attributes.jkind].  *)
val of_attributes :
  legacy_immediate:bool ->
  context:annotation_context ->
  Parsetree.attributes ->
  (t option, Jane_asttypes.jkind_annotation) result

(** Find a jkind in attributes, defaulting to ~default.  Returns error if a
    disallowed jkind is present, but always allows immediate if
    ~legacy_immediate is true.  See comment on [Builtin_attributes.jkind]. *)
val of_attributes_default :
  legacy_immediate:bool ->
  context:annotation_context ->
  default:t ->
  Parsetree.attributes ->
  (t, Jane_asttypes.jkind_annotation) result

(** Choose an appropriate jkind for a boxed record type, given whether
    all of its fields are [void]. *)
val for_boxed_record : all_void:bool -> t

(** Choose an appropriate jkind for a boxed variant type, given whether
    all of the fields of all of its constructors are [void]. *)
val for_boxed_variant : all_voids:bool -> t

(******************************)
(* elimination and defaulting *)

(* The description of a jkind, used as a return type from [get]. *)
type desc =
  | Const of const
  | Var of Sort.var

(** Extract the [const] from a [Jkind.t], looking through unified
    sort variables. Returns [Var] if the final, non-variable jkind has not
    yet been determined. *)
val get : t -> desc

(** [get_default_value] extracts the jkind as a `const`.  If it's a sort
    variable, it is set to [value] first. *)
val get_default_value : t -> const

(** [default_to_value t] is [ignore (get_default_value t)] *)
val default_to_value : t -> unit

(** [is_void t] is [Void = get_default_value t].  In particular, it will
    default the jkind to value if needed to make this false. *)
val is_void_defaulting : t -> bool
(* CR layouts v5: When we have proper support for void, we'll want to change
   these three functions to default to void - it's the most efficient thing
   when we have a choice. *)

(** Returns the sort corresponding to the jkind.  Call only on representable
    jkinds - raises on Any. *)
val sort_of_jkind : t -> sort

(*********************************)
(* pretty printing *)

val to_string : t -> string

val format : Format.formatter -> t -> unit

(** Format the history of this jkind: what interactions it has had and why
    it is the jkind that it is. Might be a no-op: see [display_histories]
    in the implementation of the [Jkind] module.

    The [intro] is something like "The jkind of t is". *)
val format_history :
  intro:(Format.formatter -> unit) -> Format.formatter -> t -> unit

(** Provides the [Printtyp.path] formatter back up the dependency chain to
    this module. *)
val set_printtyp_path : (Format.formatter -> Path.t -> unit) -> unit

(******************************)
(* relations *)

(** This checks for equality, and sets any variables to make two jkinds
    equal, if possible. e.g. [equate] on a var and [value] will set the
    variable to be [value] *)
val equate : t -> t -> bool

(** This checks for equality, but has the invariant that it can only be called
    when there is no need for unification; e.g. [equal] on a var and [value]
    will crash.

    CR layouts (v1.5): At the moment, this is actually the same as [equate]! *)
val equal : t -> t -> bool

(** Finds the intersection of two jkinds, constraining sort variables to
    create one if needed, or returns a [Violation.t] if an intersection does
    not exist.  Can update the jkinds.  The returned jkind's history
    consists of the provided reason followed by the history of the first
    jkind argument.  That is, due to histories, this function is asymmetric;
    it should be thought of as modifying the first jkind to be the
    intersection of the two, not something that modifies the second jkind. *)
val intersection : reason:interact_reason -> t -> t -> (t, Violation.t) Result.t

(** [sub t1 t2] returns [Ok ()] iff [t1] is a subjkind of
  of [t2].  The current hierarchy is:

  Any > Sort Value > Immediate64 > Immediate
  Any > Sort Void

  Returns [Error _] if the coercion is not possible. *)
val sub : t -> t -> (unit, Violation.t) result

(** Like [sub], but returns the subjkind with an updated history. *)
val sub_with_history : t -> t -> (t, Violation.t) result

(** Checks to see whether a jkind is any. Never does any mutation. *)
val is_any : t -> bool

(*********************************)
(* debugging *)

module Debug_printers : sig
  val t : Format.formatter -> t -> unit
end
