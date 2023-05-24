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

module Sort : sig
  (** A sort classifies how a type is represented at runtime. Every concrete
      layout has a sort, and knowing the sort is sufficient for knowing the
      calling convention of values of a given type. *)
  type t

  (** These are the constant sorts -- fully determined and without variables *)
  type const =
    | Void
      (** No run time representation at all *)
    | Value
      (** Standard ocaml value representation *)

  (** A sort variable that can be unified during type-checking. *)
  type var

  (** Create a new sort variable that can be unified. *)
  val new_var : unit -> t

  val of_const : const -> t
  val of_var : var -> t

  val void : t
  val value : t

  (** These names are generated lazily and only when this function is called,
      and are not guaranteed to be efficient to create *)
  val var_name : var -> string

  (** This checks for equality, and sets any variables to make two sorts
      equal, if possible *)
  val equate : t -> t -> bool

  module Debug_printers : sig
    val t : Format.formatter -> t -> unit
    val var : Format.formatter -> var -> unit
  end
end

type sort = Sort.t

(** This module describes layouts, which classify types. Layouts are arranged
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
module Layout : sig
  (** A Layout.t is a full description of the runtime representation of values
      of a given type. It includes sorts, but also the abstract top layout
      [Any] and sublayouts of other sorts, such as [Immediate]. *)
  type t

  (******************************)
  (* constants *)

  (** Constant layouts are used both for user-written annotations and within
      the type checker when we know a layout has no variables *)
  type const = Asttypes.const_layout =
    | Any
    | Value
    | Void
    | Immediate64
    | Immediate
  val string_of_const : const -> string
  val equal_const : const -> const -> bool

  (** This layout is the top of the layout lattice. All types have layout [any].
      But we cannot compile run-time manipulations of values of types with
      layout [any]. *)
  val any : t

  (** This is a variant of the [any] layout used when we have to fill it in
      because there's a missing .cmi file for the specified type. *)
  val missing_cmi_any : Path.t -> t

  (** Value of types of this layout are not retained at all at runtime *)
  val void : t

  (** This is the layout of normal ocaml values *)
  val value : t

  (** Values of types of this layout are immediate on 64-bit platforms; on other
      platforms, we know nothing other than that it's a value. *)
  val immediate64 : t

  (** We know for sure that values of types of this layout are always immediate *)
  val immediate : t

  (******************************)
  (* errors *)

  type fixed_layout_reason =
    | Let_binding
    | Tuple_element
    | Probe
    | Package_hack
    | Object
    | Instance_variable
    | Object_field
    | Class_field

  type concrete_layout_reason =
    | Match
    | Constructor_declaration of int
    | Label_declaration of Ident.t
    | Function_argument
    | Function_result

  type annotation_location =
    | Type_declaration of Path.t
    | Type_parameter of Path.t * string
    | With_constraint of Location.t
    | Newtype_declaration of string Location.loc

  type reason =
    | Fixed_layout of fixed_layout_reason
    | Concrete_layout of concrete_layout_reason
    | Annotated of annotation_location
    | Gadt_equation of Path.t
    | Unified_with_tvar of string option
        (* CR layouts v2: This really needs to carry a type, in case it
           gets further unified. But that makes layouts recursive with
           types, which will involve a painful restructuring. Still, RAE
           thinks it's inevitable. *)
    | V1_safety_check
    | Dummy_reason_result_ignored

  module Violation : sig
    type message =
      | Not_a_sublayout of t * t
      | No_intersection of t * t

    type violation = private
      { message : message
      ; missing_cmi : bool
          (** Was this error caused by a missing .cmi file?  This is redundant
              with information stored in the [message], but is more easily
              inspectable by external code.  The error-printing code does not
              inspect this value; it's only used for program logic. *)
      }

    val not_a_sublayout : t -> t -> violation

    val no_intersection : t -> t -> violation

    (* CR layouts: Is [missing_cmi] really the best thing?  Maybe some functions
       need to return success | error | missing_cmi. *)

    (** If we later discover that the left-hand layout was from a missing .cmi
        file, and if that layout is [any], this function will update that layout
        to report what type caused that (a la [missing_cmi_any]). *)
    val add_missing_cmi_for_lhs :
      missing_cmi_for:Path.t -> violation -> violation

    (* CR layouts: The [offender] arguments below are always
       [Printtyp.type_expr], so we should either stash that in a ref (like with
       [set_printtyp_path] below) or just move all the printing machinery
       downstream of both [Layouts] and [Printtyp]. *)

    (* CR layouts: Having these options for printing a violation was a choice
       made based on the needs of expedient debugging during development, but
       probably should be rethought at some point. *)
    (** Prints a violation and the thing that had an unexpected layout
        ([offender], which you supply an arbitrary printer for). *)
    val report_with_offender :
      offender:(Format.formatter -> unit) ->
      Format.formatter -> violation -> unit

    (** Like [report_with_offender], but additionally prints that the issue is
        that a representable layout was expected. *)
    val report_with_offender_sort :
      offender:(Format.formatter -> unit) ->
      Format.formatter -> violation -> unit

    (** Simpler version of [report_with_offender] for when the thing that had an
        unexpected layout is available as a string. *)
    val report_with_name : name:string -> Format.formatter -> violation -> unit

    (** Provides the [Printtyp.path] formatter back up the dependency chain to
        this module. *)
    val set_printtyp_path : (Format.formatter -> Path.t -> unit) -> unit
  end

  (******************************)
  (* construction *)

  (** Create a fresh sort variable, packed into a layout. *)
  val of_new_sort_var : unit -> t

  val of_sort : sort -> t
  val of_const : const -> t

  (** Find a layout in attributes.  Returns error if a disallowed layout is
      present, but always allows immediate attributes if ~legacy_immediate is
      true.  See comment on [Builtin_attributes.layout].  *)
  val of_attributes :
    legacy_immediate:bool -> reason:annotation_location -> Parsetree.attributes ->
    (t option, Location.t * const) result

  (** Find a layout in attributes, defaulting to ~default.  Returns error if a
      disallowed layout is present, but always allows immediate if
      ~legacy_immediate is true.  See comment on [Builtin_attributes.layout]. *)
  val of_attributes_default :
    legacy_immediate:bool -> reason:annotation_location ->
    default:t -> Parsetree.attributes ->
    (t, Location.t * const) result

  (******************************)
  (* elimination and defaulting *)

  type desc =
    | Const of const
    | Var of Sort.var

  (** Extract the [desc] from a [Layout.t], looking through unified
      sort variables. Returns [Var] if the final, non-variable layout has not
      yet been determined. *)
  val repr : t -> desc

  (** [repr_default_value] extracts the layout as a `const`.  If it's a sort
      variable, it is set to [value] first. *)
  val repr_default_value : t -> const

  (** [default_to_value t] is [ignore (get_default_value t)] *)
  val default_to_value : t -> unit

  (** [is_void t] is [Void = get_default_value t].  In particular, it will
      default the layout to value if needed to make this false. *)
  val is_void : t -> bool
  (* CR layouts v5: When we have proper support for void, we'll want to change
     these three functions to default to void - it's the most efficient thing
     when we have a choice. *)

  val of_desc : desc -> t

  (** Returns the sort corresponding to the layout.  Call only on representable
      layouts - raises on Any. *)
  val sort_of_layout : t -> sort

  (*********************************)
  (* pretty printing *)

  val to_string : t -> string
  val format : Format.formatter -> t -> unit
  val format_history :
    pp_name:(Format.formatter -> 'a -> unit) -> name:'a ->
    Format.formatter -> t -> unit

  (******************************)
  (* relations *)

  (** This checks for equality, and sets any variables to make two layouts
      equal, if possible. e.g. [equate] on a var and [value] will set the
      variable to be [value].

      This function ignores the [missing_cmi_for] medatadata for [any]s. *)
  val equate : t -> t -> bool

  (** This checks for equality, but has the invariant that it can only be called
      when there is no need for unification; e.g. [equal] on a var and [value]
      will crash.

      This function ignores the [missing_cmi_for] medatadata for [any]s.

      CR layouts v2: At the moment, this is actually the same as [equate]! Fix. *)
  val equal : t -> t -> bool

  (** Finds the intersection of two layouts, constraining sort variables to
      create one if needed, or returns a [Violation.violation] if an
      intersection does not exist.  Can update the layouts.  The returned
      layout's history consists of the provided reason followed by the history
      of the first layout argument.  That is, due to histories, this function is
      asymmetric; it should be thought of as modifying the first layout to be
      the intersection of the two, not something that modifies the second
      layout. *)
  val intersection :
    reason:reason -> t -> t -> (t, Violation.violation) Result.t

  (** [sub t1 t2] returns [Ok t1] iff [t1] is a sublayout of
    of [t2].  The current hierarchy is:

    Any > Sort Value > Immediate64 > Immediate
    Any > Sort Void

    Returns [Error _] if the coercion is not possible. *)
  val sub : t -> t -> (unit, Violation.violation) result

  (*********************************)
  (* defaulting *)

  (*********************************)
  (* debugging *)

  module Debug_printers : sig
    val t : Format.formatter -> t -> unit
  end
end

type layout = Layout.t
