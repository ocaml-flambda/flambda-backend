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

(* This module is named Jkind, with a 'j', to distinguish jkinds
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

module Type : sig
  (* The externality mode. This tracks whether or not an expression is external
     to the type checker; something external to the type checker can be skipped
     during garbage collection.

     This will eventually be incorporated into the mode
     solver, but it is defined here because we do not yet track externalities
     on expressions, just in jkinds. *)
  (* CR externals: Move to mode.ml. But see
     https://github.com/goldfirere/flambda-backend/commit/d802597fbdaaa850e1ed9209a1305c5dcdf71e17
     first, which was reisenberg's attempt to do so. *)
  module Externality : sig
    type t = Jkind_types.Type.Externality.t =
      | External (* not managed by the garbage collector *)
      | External64 (* not managed by the garbage collector on 64-bit systems *)
      | Internal (* managed by the garbage collector *)

    val le : t -> t -> bool

    val print : Format.formatter -> t -> unit
  end

  module Sort : Jkind_intf.Sort with type const = Jkind_types.Type.Sort.const

  type sort = Sort.t

  (* The layout of a type describes its memory layout. A layout is either the
     indeterminate [Any] or a sort, which is a concrete memory layout. *)
  module Layout : sig
    module Const : sig
      type t = Jkind_types.Type.Layout.Const.t

      val get_sort : t -> Sort.Const.t option

      val to_string : t -> string

      (* CR layouts v2.8: remove this *)
      module Legacy : sig
        type t = Jkind_types.Type.Layout.Const.Legacy.t =
          | Any
          | Value
          | Void
          | Immediate64
          | Immediate
          | Float64
          | Float32
          | Word
          | Bits32
          | Bits64

        val to_string : t -> string
      end
    end
  end

  (** A Jkind.Type.t is a full description of the runtime representation of values
      of a given type. It includes sorts, but also the abstract top jkind
      [Any] and subjkinds of other sorts, such as [Immediate]. *)
  type t = Types.type_expr Jkind_types.type_jkind

  type desc = Types.type_expr Jkind_types.Type.Jkind_desc.t

  (******************************)
  (* constants *)

  module Const : sig
    (** Constant jkinds are used for user-written annotations *)
    type t = Types.type_expr Jkind_types.Type.Const.t

    val to_out_jkind_const : t -> Outcometree.out_jkind_const

    val format : Format.formatter -> t -> unit

    val equal : t -> t -> bool

    (** Gets the layout of a constant jkind. Never does mutation. *)
    val get_layout : t -> Layout.Const.t

    (* CR layouts v2.8: remove this *)

    (** Gets the legacy layout of a constant jkind. Never does mutation. *)
    val get_legacy_layout : t -> Layout.Const.Legacy.t

    (** Gets the maximum modes for types of this constant jkind. *)
    val get_modal_upper_bounds : t -> Mode.Alloc.Const.t

    (** Gets the maximum mode on the externality axis for types of this constant jkind. *)
    val get_externality_upper_bound : t -> Externality.t

    module Primitive : sig
      type nonrec t =
        { jkind : t;
          name : string
        }

      (** This jkind is the top of the  *type* jkind lattice. All types have
        jkind [any]. But we cannot compile run-time manipulations of values of
        types with jkind [any]. *)
      val any : t

      (** Value of types of this jkind are not retained at all at runtime *)
      val void : t

      (** This is the jkind of normal ocaml values *)
      val value : t

      (** Values of types of this jkind are immediate on 64-bit platforms; on other
      platforms, we know nothing other than that it's a value. *)
      val immediate64 : t

      (** We know for sure that values of types of this jkind are always immediate *)
      val immediate : t

      (** This is the jkind of unboxed 64-bit floats.  They have sort
      Float64. Mode-crosses. *)
      val float64 : t

      (** This is the jkind of unboxed 32-bit floats.  They have sort
      Float32. Mode-crosses. *)
      val float32 : t

      (** This is the jkind of unboxed native-sized integers. They have sort
      Word. Does not mode-cross. *)
      val word : t

      (** This is the jkind of unboxed 32-bit integers. They have sort Bits32. Does
      not mode-cross. *)
      val bits32 : t

      (** This is the jkind of unboxed 64-bit integers. They have sort Bits64. Does
      not mode-cross. *)
      val bits64 : t

      (** A list of all primitive jkinds *)
      val all : t list
    end

    module Sort : module type of struct
      include Sort.Const
    end

    module Layout : module type of struct
      include Layout.Const
    end
  end

  module Primitive : sig
    open Jkind_intf

    (** This jkind is the top of the  *type* jkind lattice. All types have
      jkind [any]. But we cannot compile run-time manipulations of values of
      types with jkind [any]. *)
    val any : why:History.any_creation_reason -> t

    (** Value of types of this jkind are not retained at all at runtime *)
    val void : why:History.void_creation_reason -> t

    (** This is the jkind of normal ocaml values *)
    val value : why:History.value_creation_reason -> t

    (** Values of types of this jkind are immediate on 64-bit platforms; on other
      platforms, we know nothing other than that it's a value. *)
    val immediate64 : why:History.immediate64_creation_reason -> t

    (** We know for sure that values of types of this jkind are always immediate *)
    val immediate : why:History.immediate_creation_reason -> t

    (** This is the jkind of unboxed 64-bit floats.  They have sort
      Float64. Mode-crosses. *)
    val float64 : why:History.float64_creation_reason -> t

    (** This is the jkind of unboxed 32-bit floats.  They have sort
      Float32. Mode-crosses. *)
    val float32 : why:History.float32_creation_reason -> t

    (** This is the jkind of unboxed native-sized integers. They have sort
      Word. Does not mode-cross. *)
    val word : why:History.word_creation_reason -> t

    (** This is the jkind of unboxed 32-bit integers. They have sort Bits32. Does
      not mode-cross. *)
    val bits32 : why:History.bits32_creation_reason -> t

    (** This is the jkind of unboxed 64-bit integers. They have sort Bits64. Does
      not mode-cross. *)
    val bits64 : why:History.bits64_creation_reason -> t
  end

  (** Take an existing [t] and add an ability to mode-cross along all the axes. *)
  val add_mode_crossing : t -> t

  (******************************)
  (* construction *)

  (** Create a fresh sort variable, packed into a jkind, returning both
      the resulting kind and the sort. *)
  val of_new_sort_var : why:Jkind_intf.History.concrete_jkind_reason -> t * sort

  (** Create a fresh sort variable, packed into a jkind. *)
  val of_new_sort : why:Jkind_intf.History.concrete_jkind_reason -> t

  val of_const : why:Jkind_intf.History.creation_reason -> Const.t -> t

  (** Choose an appropriate jkind for a boxed record type, given whether
      all of its fields are [void]. *)
  val for_boxed_record : all_void:bool -> t

  (** Choose an appropriate jkind for a boxed variant type, given whether
      all of the fields of all of its constructors are [void]. *)
  val for_boxed_variant : all_voids:bool -> t

  (******************************)
  (* elimination and defaulting *)

  module Desc : sig
    (** The description of a jkind, used as a return type from [get]. *)
    type t =
      | Const of Const.t
      | Var of Sort.var
  end

  (** Extract the [const] from a [Jkind.t], looking through unified
      sort variables. Returns [Var] if the final, non-variable jkind has not
      yet been determined. *)
  val get : t -> Desc.t

  (** [default_to_value_and_get] extracts the jkind as a `const`.  If it's a sort
      variable, it is set to [value] first. *)
  val default_to_value_and_get : t -> Const.t

  (** [default_to_value t] is [ignore (default_to_value_and_get t)] *)
  val default_to_value : t -> unit

  (** Returns the sort corresponding to the jkind.  Call only on representable
    jkinds - raises on Any. *)
  val sort_of_jkind : t -> sort

  (** [is_void t] is [Void = default_to_value_and_get t].  In particular, it will
      default the jkind to value if needed to make this false. *)
  val is_void_defaulting : t -> bool
  (* CR layouts v5: When we have proper support for void, we'll want to change
     these three functions to default to void - it's the most efficient thing
     when we have a choice. *)

  (** Gets the layout of a jkind; returns [None] if the layout is still unknown.
      Never does mutation. *)
  val get_layout : t -> Layout.Const.t option

  (** Gets the maximum modes for types of this jkind. *)
  val get_modal_upper_bounds : t -> Mode.Alloc.Const.t

  (** Gets the maximum mode on the externality axis for types of this jkind. *)
  val get_externality_upper_bound : t -> Externality.t

  (** Computes a jkind that is the same as the input but with an updated maximum
      mode for the externality axis *)
  val set_externality_upper_bound : t -> Externality.t -> t

  (*********************************)
  (* debugging *)

  module Debug_printers : sig
    val t : Format.formatter -> t -> unit
  end
end

(** A Jkind.t is a type representing general, higher-order kinds. Its special
    case is a zeroth-order kind of runtime-representable types (Jkind.Type.t). *)
type t = Types.type_expr Jkind_types.t

type desc = Types.type_expr Jkind_types.Jkind_desc.t

module History : sig
  val has_imported_history : t -> bool

  val update_reason : t -> Jkind_intf.History.creation_reason -> t

  (* Mark the jkind as having produced a compiler warning. *)
  val with_warning : t -> t

  (* Whether this jkind has produced a compiler warning. *)
  val has_warned : t -> bool
end

(******************************)
(* constants *)

module Const : sig
  type t = Types.type_expr Jkind_types.Const.t

  val of_type_jkind : Type.Const.t -> t

  val format : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end

module Primitive : sig
  (** Top element of the jkind lattice, including higher jkinds *)
  val top : why:Jkind_intf.History.top_creation_reason -> t
end

(******************************)
(* sorts *)

module Sort : sig
  type t = Types.type_expr Jkind_types.Sort.t

  and desc = Types.type_expr Jkind_types.Sort.desc

  and var = Types.type_expr Jkind_types.Sort.var

  module Var : sig
    type t = var

    (** These names are generated lazily and only when this function is called,
      and are not guaranteed to be efficient to create *)
    val name : t -> string
  end

  (** To record changes to sorts, for use with `Types.{snapshot, backtrack}` *)
  type change

  val undo_change : change -> unit

  val set_change_log : (change -> unit) -> unit
end

(******************************)
(* construction *)
(* See Jkind.Type for details *)

val to_const : t -> Const.t option

(** Create a fresh sort variable, packed into a jkind, returning both
    the resulting kind and the sort. *)
val of_new_sort_var :
  why:Jkind_intf.History.higher_concrete_jkind_reason -> t * Sort.t

(** Create a fresh sort variable, packed into a jkind. *)
val of_new_sort : why:Jkind_intf.History.higher_concrete_jkind_reason -> t

val of_const : why:Jkind_intf.History.creation_reason -> Const.t -> t

val const_of_user_written_annotation :
  context:Jkind_intf.History.annotation_context ->
  Jane_syntax.Jkind.annotation ->
  Const.t

(** The typed jkind together with its user-written annotation. *)
type annotation = Types.type_expr Jkind_types.annotation

val of_annotation :
  context:Jkind_intf.History.annotation_context ->
  Jane_syntax.Jkind.annotation ->
  t * annotation

val of_annotation_option_default :
  default:t ->
  context:Jkind_intf.History.annotation_context ->
  Jane_syntax.Jkind.annotation option ->
  t * annotation option

(** Find a jkind from a type declaration. Type declarations are special because
    the jkind may have been provided via [: jkind] syntax (which goes through
    Jane Syntax) or via the old-style [[@@immediate]] or [[@@immediate64]]
    attributes, and [of_type_decl] needs to look in two different places on the
    [type_declaration] to account for these two alternatives.

    Returns the jkind, the user-written annotation, and the remaining unconsumed
    attributes. (The attributes include old-style [[@@immediate]] or
    [[@@immediate64]] attributes if those are present, but excludes any
    attribute used by Jane Syntax to encode a [: jkind]-style jkind.)

    Raises if a disallowed or unknown jkind is present.
*)
val of_type_decl :
  context:Jkind_intf.History.annotation_context ->
  Parsetree.type_declaration ->
  (t * annotation * Parsetree.attributes) option

(** Find a jkind from a type declaration in the same way as [of_type_decl],
    defaulting to ~default.

    Raises if a disallowed or unknown jkind is present.
*)
val of_type_decl_default :
  context:Jkind_intf.History.annotation_context ->
  default:t ->
  Parsetree.type_declaration ->
  t * annotation option * Parsetree.attributes

(** Assert this jkind is not higher order - it is a type jkind. Raises otherwise. *)
val to_type_jkind : t -> Type.t

(** Convert a type jkind into a general jkind *)
val of_type_jkind : Type.t -> t

(** Construct the jkind from an arrow with a given history *)
val of_arrow :
  history:Types.type_expr Jkind_types.history -> t Jkind_types.Arrow.t -> t

(******************************)
(* elimination and defaulting *)

module Desc : sig
  (** The description of a jkind, used as a return type from [get]. *)
  type nonrec t =
    | Var of Sort.var
    | Type of Type.t
    | Arrow of t Jkind_types.Arrow.t
    | Top
end

(** [default_to_value_and_get] extracts the jkind as a `const`.  If it's a sort
    variable, it is set to [value] first. *)
val default_to_value_and_get : t -> Const.t

(** [default_to_value t] is [ignore (default_to_value_and_get t)] *)
val default_to_value : t -> unit

(** Extract the [const] from a [Jkind.Type.t], looking through unified
    sort variables. Returns [Var] if the final, non-variable jkind has not
    yet been determined. *)
val get : t -> Desc.t

(*********************************)
(* pretty printing *)

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
(* errors *)

module Violation : sig
  type violation =
    | Not_a_subjkind of t * t
    | No_intersection of t * t
    | No_union of t * t

  type t

  (** Set [?missing_cmi] to mark [t] as having arisen from a missing cmi *)
  val of_ : ?missing_cmi:Path.t -> violation -> t

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

(** Attempt to resolve to [Some] arrow, [None] otherwise indicating error *)
val equate_to_arrow : arity:int -> t -> t Jkind_types.Arrow.t option

(** Checks whether two jkinds have a non-empty intersection. Might mutate
    sort variables. *)
val has_intersection : t -> t -> bool

(** Finds the intersection of two jkinds, constraining sort variables to
    create one if needed, or returns a [Violation.t] if an intersection does
    not exist.  Can update the jkinds.  The returned jkind's history
    consists of the provided reason followed by the history of the first
    jkind argument.  That is, due to histories, this function is asymmetric;
    it should be thought of as modifying the first jkind to be the
    intersection of the two, not something that modifies the second jkind. *)
val intersection_or_error :
  reason:Jkind_intf.History.interact_reason ->
  t ->
  t ->
  (t, Violation.t) Result.t

(** [sub t1 t2] says whether [t1] is a subjkind of [t2]. Might update
    either [t1] or [t2] to make their layouts equal.*)
val sub : t -> t -> bool

(** [sub_or_error t1 t2] returns [Ok ()] iff [t1] is a subjkind of
  of [t2]. Otherwise returns an appropriate error to report to the user. *)
val sub_or_error : t -> t -> (unit, Violation.t) result

(** Like [sub], but returns the subjkind with an updated history. *)
val sub_with_history : t -> t -> (t, Violation.t) result

(** Checks to see whether a jkind is top. Never does any mutation. *)
val is_max : t -> bool

(** Checks to see whether a jkind is has layout. Never does any mutation. *)
val has_layout_any : t -> bool

(*********************************)
(* debugging *)

module Debug_printers : sig
  val t : Format.formatter -> t -> unit
end
