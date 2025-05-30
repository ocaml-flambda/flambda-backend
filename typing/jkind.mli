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

open Allowance

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

(* The externality mode. This tracks whether or not an expression is external
   to the type checker; something external to the type checker can be skipped
   during garbage collection.

   This will eventually be incorporated into the mode
   solver, but it is defined here because we do not yet track externalities
   on expressions, just in jkinds. *)

module Sort : sig
  include
    Jkind_intf.Sort
      with type t = Jkind_types.Sort.t
       and type base = Jkind_types.Sort.base
       and type Const.t = Jkind_types.Sort.Const.t

  module Flat : sig
    (** A flat sort is returned from [get]. *)
    type t =
      | Var of Var.id (* [Var.id] is for debugging / printing only *)
      | Base of base
  end
end

type sort = Sort.t

module Sub_failure_reason : sig
  type t =
    | Axis_disagreement of Jkind_axis.Axis.packed
    | Layout_disagreement
    | Constrain_ran_out_of_fuel
end

module Sub_result : sig
  type t =
    | Equal
    | Less
    | Not_le of Sub_failure_reason.t Misc.Nonempty_list.t

  val of_le_result :
    failure_reason:(unit -> Sub_failure_reason.t Misc.Nonempty_list.t) ->
    Misc.Le_result.t ->
    t

  val combine : t -> t -> t

  val require_le : t -> (unit, Sub_failure_reason.t Misc.Nonempty_list.t) result

  val is_le : t -> bool
end

(* The layout of a type describes its memory layout. A layout is either the
   indeterminate [Any] or a sort, which is a concrete memory layout. *)
module Layout : sig
  type 'sort t = 'sort Jkind_types.Layout.t =
    | Sort of 'sort
    | Product of 'sort t list
    | Any

  module Const : sig
    type t = Jkind_types.Layout.Const.t

    val get_sort : t -> Sort.Const.t option

    val of_sort_const : Sort.Const.t -> t

    val to_string : t -> string
  end

  val of_const : Const.t -> Sort.t t

  val sub : Sort.t t -> Sort.t t -> Sub_result.t

  module Debug_printers : sig
    val t :
      (Format.formatter -> 'sort -> unit) -> Format.formatter -> 'sort t -> unit
  end
end

module Mod_bounds : sig
  type t = Types.Jkind_mod_bounds.t

  val to_mode_crossing : t -> Mode.Crossing.t
end

module With_bounds : sig
  val debug_print_types : Format.formatter -> Types.with_bounds_types -> unit

  val debug_print : Format.formatter -> ('l * 'r) Types.with_bounds -> unit

  val map_type_expr :
    (Types.type_expr -> Types.type_expr) ->
    ('l * 'r) Types.with_bounds ->
    ('l * 'r) Types.with_bounds

  val format : Format.formatter -> ('l * 'r) Types.with_bounds -> unit
end

(** A [jkind] is a full description of the runtime representation of values
    of a given type. It includes sorts, but also the abstract top jkind
    [Any] and subjkinds of other sorts, such as [Immediate].

    The type parameter gives information about whether the jkind can
    meaningfully appear to the left of a subjkind check (this is an l-jkind)
    or on the right of a subjkind check (this is an r-jkind).

    It may be convenient to use synonyms exported from [Types]:

    * [jkind_l]: This is the jkind of an actual type; it is returned from
    [estimate_type_jkind], for example. We can compute the joins (unions) of
    l-jkinds and check that an l-jkind is less than an r-jkind.

    * [jkind_r]: This is the jkind we want some type to have. Type variables
    have r-jkinds (because we will someday instantiate those variables).
    The type passed to [constrain_type_jkind] is an r-jkind. We can compute
    the meets (intersections) of r-jkinds and check that an l-jkind is less
    than an r-jkind.
*)

include Allowance.Allow_disallow with type (_, _, 'd) sided = 'd Types.jkind

(** Try to treat this jkind as an r-jkind. *)
val try_allow_r : ('l * 'r) Types.jkind -> ('l * allowed) Types.jkind option

module History : sig
  include module type of struct
    include Jkind_intf.History
  end

  (* history *)

  val is_imported : 'd Types.jkind -> bool

  val update_reason : 'd Types.jkind -> creation_reason -> 'd Types.jkind

  (* Mark the jkind as having produced a compiler warning. *)
  val with_warning : 'd Types.jkind -> 'd Types.jkind

  (* Whether this jkind has produced a compiler warning. *)
  val has_warned : 'd Types.jkind -> bool
end

(******************************)
(* errors *)

module Violation : sig
  type violation =
    (* [Not_a_subjkind] allows l-jkinds on the right so that it can be used
       in [sub_jkind_l]. There is no downside to this, as the printing
       machinery works over l-jkinds. *)
    | Not_a_subjkind :
        (allowed * 'r1) Types.jkind
        * ('l * 'r2) Types.jkind
        * Sub_failure_reason.t list
        -> violation
    | No_intersection : 'd Types.jkind * ('l * allowed) Types.jkind -> violation

  type t

  (** Set [?missing_cmi] to mark [t] as having arisen from a missing cmi *)

  val of_ :
    jkind_of_type:(Types.type_expr -> Types.jkind_l option) ->
    ?missing_cmi:Path.t ->
    violation ->
    t

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

module Const : sig
  (** Constant jkinds are used for user-written annotations. They are not
      actually constant, though: they might contain variables in [with]-types.
      The "constant" refers to the fact that there are no sort variables here.
      The existence of [with]-types means, though, that we still need the
      allowance machinery here. *)
  type 'd t constraint 'd = 'l * 'r

  include Allowance.Allow_disallow with type (_, _, 'd) sided = 'd t

  val to_out_jkind_const : 'd t -> Outcometree.out_jkind_const

  (** This returns [true] iff both types have no with-bounds and they are equal.
      Normally, we want an equality check to happen only on values that are
      allowed on both the left and the right. But a type with no with-bounds is
      allowed on the left and the right, so we test for that condition first
      before doing the proper equality check. *)
  val no_with_bounds_and_equal : 'd1 t -> 'd2 t -> bool

  (* CR layouts: Remove this once we have a better story for printing with jkind
     abbreviations. *)
  module Builtin : sig
    type nonrec t =
      { jkind : (allowed * allowed) t;
        name : string
      }

    (** This jkind is the top of the jkind lattice. All types have jkind [any].
    But we cannot compile run-time manipulations of values of types with jkind
    [any]. *)
    val any : t

    (** [any], except for null pointers. *)
    val any_non_null : t

    (** Value of types of this jkind are not retained at all at runtime *)
    val void : t

    (** This is the jkind of normal ocaml values or null pointers *)
    val value_or_null : t

    (** This is the jkind of normal ocaml values *)
    val value : t

    (** Immutable non-float values that don't contain functions. *)
    val immutable_data : t

    (** Atomically mutable non-float values that don't contain functions. *)
    val sync_data : t

    (** Mutable non-float values that don't contain functions. *)
    val mutable_data : t

    (** Values of types of this jkind are immediate on 64-bit platforms; on other
    platforms, we know nothing other than that it's a value. *)
    val immediate64 : t

    (** We know for sure that values of types of this jkind are always immediate *)
    val immediate : t

    (** Values of types of this jkind are either immediate or null pointers *)
    val immediate_or_null : t

    (** The jkind of unboxed 64-bit floats with no mode crossing. *)
    val float64 : t

    (** The jkind of unboxed 64-bit floats with mode crossing. *)
    val kind_of_unboxed_float : t

    (** The jkind of unboxed 32-bit floats with no mode crossing. *)
    val float32 : t

    (** The jkind of unboxed 32-bit floats with mode crossing. *)
    val kind_of_unboxed_float32 : t

    (** The jkind of unboxed 32-bit native-sized integers with no mode crossing. *)
    val word : t

    (** The jkind of unboxed 32-bit native-sized integers with mode crossing. *)
    val kind_of_unboxed_nativeint : t

    (** The jkind of unboxed 32-bit integers with no mode crossing. *)
    val bits32 : t

    (** The jkind of unboxed 32-bit integers with mode crossing. *)
    val kind_of_unboxed_int32 : t

    (** The jkind of unboxed 64-bit integers with no mode crossing. *)
    val bits64 : t

    (** The jkind of unboxed 64-bit integers with mode crossing. *)
    val kind_of_unboxed_int64 : t

    (** The jkind of unboxed 128-bit vectors with no mode crossing. *)
    val vec128 : t

    (** The jkind of unboxed 128-bit vectors with mode crossing. *)
    val kind_of_unboxed_128bit_vectors : t

    (** A list of all Builtin jkinds *)
    val all : t list
  end
end

module Builtin : sig
  (** This jkind is the top of the jkind lattice. All types have jkind [any].
    But we cannot compile run-time manipulations of values of types with jkind
    [any]. *)
  val any : why:History.any_creation_reason -> 'd Types.jkind

  (* CR layouts v3: change to [any_separable]. *)

  (** Jkind of array elements. *)
  val any_non_null : why:History.any_creation_reason -> 'd Types.jkind

  (** Value of types of this jkind are not retained at all at runtime *)
  val void : why:History.void_creation_reason -> ('l * disallowed) Types.jkind

  val value_or_null :
    why:History.value_or_null_creation_reason -> 'd Types.jkind

  (** This is the jkind of normal ocaml values *)
  val value : why:History.value_creation_reason -> 'd Types.jkind

  (** This is suitable for records or variants without mutable fields. *)
  val immutable_data : why:History.value_creation_reason -> 'd Types.jkind

  (** This is suitable for records or variants with atomically mutable fields. *)
  val sync_data : why:History.value_creation_reason -> 'd Types.jkind

  (** This is suitable for records or variants with mutable fields. *)
  val mutable_data : why:History.value_creation_reason -> 'd Types.jkind

  (** We know for sure that values of types of this jkind are always immediate *)
  val immediate :
    why:History.immediate_creation_reason -> ('l * disallowed) Types.jkind

  (** Values of types of this jkind are either immediate or null pointers *)
  val immediate_or_null :
    why:History.immediate_or_null_creation_reason -> 'd Types.jkind

  (** Build a jkind of unboxed products, from a list of types with
      their layouts. Errors if zero inputs are given.

      Precondition: both input lists are the same length.

      This returns an [jkind_l] simply as a matter of convenience; it can be
      generalized if need be.

      The resulting jkind has quality [Best], because all the components of the product
      are represented in the with-bounds. *)
  val product :
    why:History.product_creation_reason ->
    (Types.type_expr * Mode.Modality.Value.Const.t) list ->
    Sort.t Layout.t list ->
    Types.jkind_l

  (** Build a jkind of unboxed products, given only an arity. This jkind will not
      mode-cross (and has kind [Not_best] accordingly), even though unboxed products
      generally should. This is useful when creating an initial jkind in Typedecl. *)
  val product_of_sorts :
    why:History.product_creation_reason -> int -> Types.jkind_l
end

(** Forcibly change the mod- and with-bounds of a [t] based on the mod- and with-bounds of [from]. *)
val unsafely_set_bounds :
  from:'d Types.jkind -> 'd Types.jkind -> 'd Types.jkind

(** Take an existing [jkind_l] and add some with-bounds. *)
val add_with_bounds :
  modality:Mode.Modality.Value.Const.t ->
  type_expr:Types.type_expr ->
  Types.jkind_l ->
  Types.jkind_l

(** Does this jkind have with-bounds? *)
val has_with_bounds : Types.jkind_l -> bool

(** Mark the given jkind as {i best}, meaning we can never learn any more information
    about it that will cause it to become lower in the preorder of kinds*)
val mark_best : ('l * 'r) Types.jkind -> ('l * disallowed) Types.jkind

(** Is the given kind best? *)
val is_best : ('l * disallowed) Types.jkind -> bool

(******************************)
(* construction *)

(** Create a fresh sort variable, packed into a jkind, returning both
    the resulting kind and the sort. *)
val of_new_sort_var :
  why:History.concrete_creation_reason -> 'd Types.jkind * sort

(** Create a fresh sort variable, packed into a jkind. *)
val of_new_sort : why:History.concrete_creation_reason -> 'd Types.jkind

(** Same as [of_new_sort_var], but the jkind is lowered to [Non_null]
    to mirror "legacy" OCaml values.
    Defaulting the sort variable produces exactly [value].  *)
val of_new_legacy_sort_var :
  why:History.concrete_legacy_creation_reason -> 'd Types.jkind * sort

(** Same as [of_new_sort], but the jkind is lowered to [Non_null]
    to mirror "legacy" OCaml values.
    Defaulting the sort variable produces exactly [value].  *)
val of_new_legacy_sort :
  why:History.concrete_legacy_creation_reason -> 'd Types.jkind

(** Construct a jkind from a constant jkind, at quality [Not_best] *)
val of_const :
  annotation:Parsetree.jkind_annotation option ->
  why:History.creation_reason ->
  quality:'d Types.jkind_quality ->
  'd Const.t ->
  'd Types.jkind

(** Construct a jkind from a builtin kind, at quality [Best]. *)
val of_builtin :
  why:History.creation_reason ->
  Const.Builtin.t ->
  ('l * disallowed) Types.jkind

val of_annotation :
  context:('l * allowed) History.annotation_context ->
  Parsetree.jkind_annotation ->
  ('l * allowed) Types.jkind

val of_annotation_option_default :
  default:('l * allowed) Types.jkind ->
  context:('l * allowed) History.annotation_context ->
  Parsetree.jkind_annotation option ->
  ('l * allowed) Types.jkind

(** Find a jkind from a type declaration. Type declarations are special because
    the jkind may have been provided via [: jkind] syntax (which goes through
    Jane Syntax) or via the old-style [[@@immediate]] or [[@@immediate64]]
    attributes, and [of_type_decl] needs to look in two different places on the
    [type_declaration] to account for these two alternatives.

    Returns the jkind (at quality [Not_best]) and the user-written annotation.

    Raises if a disallowed or unknown jkind is present.
*)
val of_type_decl :
  context:History.annotation_context_l ->
  transl_type:(Parsetree.core_type -> Types.type_expr) ->
  Parsetree.type_declaration ->
  (Types.jkind_l * Parsetree.jkind_annotation option) option

(** Find a jkind from a type declaration in the same way as [of_type_decl],
    defaulting to ~default. Returns a jkind at quality [Not_best]; call [mark_best] to
    mark it as [Best].

    Raises if a disallowed or unknown jkind is present.
*)
val of_type_decl_default :
  context:History.annotation_context_l ->
  transl_type:(Parsetree.core_type -> Types.type_expr) ->
  default:Types.jkind_l ->
  Parsetree.type_declaration ->
  Types.jkind_l

(** Choose an appropriate jkind for a boxed record type *)
val for_boxed_record : Types.label_declaration list -> Types.jkind_l

(** Choose an appropriate jkind for an unboxed record type. *)
val for_unboxed_record : Types.label_declaration list -> Types.jkind_l

(** Choose an appropriate jkind for a boxed variant type. *)
val for_boxed_variant : Types.constructor_declaration list -> Types.jkind_l

(** Choose an appropriate jkind for a boxed tuple type. *)
val for_boxed_tuple : (string option * Types.type_expr) list -> Types.jkind_l

(** Choose an appropriate jkind for a row type. *)
val for_boxed_row : Types.row_desc -> Types.jkind_l

(** The jkind of an arrow type. *)
val for_arrow : Types.jkind_l

(** The jkind of an object type.  *)
val for_object : Types.jkind_l

(** The jkind of a float. *)
val for_float : Ident.t -> Types.jkind_l

(** The jkind for values that are not floats. *)
val for_non_float : why:History.value_creation_reason -> 'd Types.jkind

(******************************)
(* elimination and defaulting *)

module Desc : sig
  (** The description of a jkind, used as a return type from [get].  This
      description has no sort variables, but it might have [with]-types and thus
      needs the allowance machinery. *)
  type 'd t = (Sort.Flat.t Layout.t, 'd) Types.layout_and_axes

  val get_const : 'd t -> 'd Const.t option

  val format : Format.formatter -> 'd t -> unit
end

(** Get a description of a jkind. *)
val get : 'd Types.jkind -> 'd Desc.t

(** [get_layout_defaulting_to_value] extracts a constant layout, defaulting
    any sort variable to [value]. *)
val get_layout_defaulting_to_value : 'd Types.jkind -> Layout.Const.t

(** [get_const] returns a [Const.t] if the layout has no sort variables,
    returning [None] otherwise *)
val get_const : 'd Types.jkind -> 'd Const.t option

(** [default_to_value t] is [ignore (get_layout_defaulting_to_value t)] *)
val default_to_value : 'd Types.jkind -> unit

(** [is_void t] is [Void = get_layout_defaulting_to_value t].  In particular, it
    will default the jkind to value if needed to make this false. *)
val is_void_defaulting : 'd Types.jkind -> bool
(* CR layouts v5: When we have proper support for void, we'll want to change
   these three functions to default to void - it's the most efficient thing
   when we have a choice. *)

(** Returns the sort corresponding to the jkind.  Call only on representable
    jkinds - raises on Any. *)
val sort_of_jkind : Types.jkind_l -> sort

(** Gets the layout of a jkind; returns [None] if the layout is still unknown.
    Never does mutation. *)
val get_layout : 'd Types.jkind -> Layout.Const.t option

(* CR reisenberg: do we need [extract_layout]? *)

(** Gets the layout of a jkind, without looking through sort variables. *)
val extract_layout : 'd Types.jkind -> Sort.t Layout.t

(** Gets the mode crossing for types of this jkind. *)
val get_mode_crossing :
  jkind_of_type:(Types.type_expr -> Types.jkind_l option) ->
  'd Types.jkind ->
  Mode.Crossing.t

val to_unsafe_mode_crossing : Types.jkind_l -> Types.unsafe_mode_crossing

val get_externality_upper_bound :
  jkind_of_type:(Types.type_expr -> Types.jkind_l option) ->
  'd Types.jkind ->
  Jkind_axis.Externality.t

(** Computes a jkind that is the same as the input but with an updated maximum
    mode for the externality axis *)
val set_externality_upper_bound :
  Types.jkind_r -> Jkind_axis.Externality.t -> Types.jkind_r

(** Gets the nullability from a jkind. *)
val get_nullability :
  jkind_of_type:(Types.type_expr -> Types.jkind_l option) ->
  'd Types.jkind ->
  Jkind_axis.Nullability.t

(** Computes a jkind that is the same as the input but with an updated maximum
    mode for the nullability axis *)
val set_nullability_upper_bound :
  Types.jkind_r -> Jkind_axis.Nullability.t -> Types.jkind_r

(** Computes a jkind that is the same as the input but with an updated maximum
    mode for the separability axis *)
val set_separability_upper_bound :
  Types.jkind_r -> Jkind_axis.Separability.t -> Types.jkind_r

(** Sets the layout in a jkind. *)
val set_layout : 'd Types.jkind -> Sort.t Layout.t -> 'd Types.jkind

(** Change a jkind to be appropriate for a type that appears under a
    modality. This means that the jkind will definitely cross the axes
    modified by the modality, by setting the mod-bounds appropriately
    and propagating the modality into any with-bounds. *)
val apply_modality_l :
  Mode.Modality.Value.Const.t -> (allowed * 'r) Types.jkind -> Types.jkind_l

(** Change a jkind to be appropriate for an expectation of a type under
    a modality. This means that the jkind's axes affected by the modality
    will all be top. The with-bounds are left unchanged. *)
val apply_modality_r :
  Mode.Modality.Value.Const.t -> ('l * allowed) Types.jkind -> Types.jkind_r

(** Extract out component jkinds from the product. Because there are no product
    jkinds, this is a bit of a lie: instead, this decomposes the layout but just
    reuses the non-layout parts of the original jkind. Never does any mutation.
    Because it just reuses the mode information, the resulting jkinds are higher
    in the jkind lattice than they might need to be.
    *)
val decompose_product : 'd Types.jkind -> 'd Types.jkind list option

(** Get an annotation (that a user might write) for this [t]. *)
val get_annotation : 'd Types.jkind -> Parsetree.jkind_annotation option

(*********************************)
(* normalization *)

type normalize_mode =
  | Require_best
      (** Normalize a jkind without losing any precision. That is, keep any with-bounds
          if the kind of the type is not best (a stronger kind may be found). *)
  | Ignore_best
      (** Normalize a left jkind, conservatively rounding up. That is, if the kind of a
          type is not best, use the not-best kind. The resulting jkind will have no
          with-bounds. *)

val normalize :
  mode:normalize_mode ->
  jkind_of_type:(Types.type_expr -> Types.jkind_l option) ->
  Types.jkind_l ->
  Types.jkind_l

(*********************************)
(* pretty printing *)

(** Call these before trying to print. *)
val set_outcometree_of_type : (Types.type_expr -> Outcometree.out_type) -> unit

val set_outcometree_of_modalities_new :
  (Types.mutability ->
  Mode.Modality.Value.Const.t ->
  Outcometree.out_mode_new list) ->
  unit

(** Provides the [Printtyp.path] formatter back up the dependency chain to
    this module. *)
val set_printtyp_path : (Format.formatter -> Path.t -> unit) -> unit

(** Provides the [type_expr] formatter back up the dependency chain to this
    module. *)
val set_print_type_expr : (Format.formatter -> Types.type_expr -> unit) -> unit

(** Provides the [raw_type_expr] formatter back up the dependency chain to this
    module. *)
val set_raw_type_expr : (Format.formatter -> Types.type_expr -> unit) -> unit

val format : Format.formatter -> 'd Types.jkind -> unit

(** Format the history of this jkind: what interactions it has had and why
    it is the jkind that it is. Might be a no-op: see [display_histories]
    in the implementation of the [Jkind] module.

    The [intro] is something like "The jkind of t is".
*)
val format_history :
  intro:(Format.formatter -> unit) -> Format.formatter -> 'd Types.jkind -> unit

(******************************)
(* relations *)

(** This checks for equality, and sets any variables to make two jkinds
    equal, if possible. e.g. [equate] on a var and [value] will set the
    variable to be [value] *)
val equate : Types.jkind_lr -> Types.jkind_lr -> bool

(** This checks for equality, but has the invariant that it can only be called
    when there is no need for unification; e.g. [equal] on a var and [value]
    will crash.

    CR layouts (v1.5): At the moment, this is actually the same as [equate]! *)
val equal : Types.jkind_lr -> Types.jkind_lr -> bool

(** Checks whether two jkinds have a non-empty intersection. Might mutate
    sort variables. Works over any mix of l- and r-jkinds, because the only
    way not to have an intersection is by looking at the layout: all axes
    have a bottom element. *)
val has_intersection : 'd1 Types.jkind -> 'd2 Types.jkind -> bool

(** Finds the intersection of two jkinds, constraining sort variables to
    create one if needed, or returns a [Violation.t] if an intersection does
    not exist.  Can update the jkinds.  The returned jkind's history
    consists of the provided reason followed by the history of the first
    jkind argument.  That is, due to histories, this function is asymmetric;
    it should be thought of as modifying the first jkind to be the
    intersection of the two, not something that modifies the second jkind. *)
val intersection_or_error :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> Types.jkind_l option) ->
  reason:History.interact_reason ->
  ('l1 * allowed) Types.jkind ->
  ('l2 * allowed) Types.jkind ->
  (('l1 * allowed) Types.jkind, Violation.t) Result.t

(** [sub t1 t2] says whether [t1] is a subjkind of [t2]. Might update
    either [t1] or [t2] to make their layouts equal.*)
val sub :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> Types.jkind_l option) ->
  Types.jkind_l ->
  Types.jkind_r ->
  bool

type sub_or_intersect =
  | Sub  (** The first jkind is a subjkind of the second. *)
  | Disjoint of Sub_failure_reason.t Misc.Nonempty_list.t
      (** The two jkinds have no common ground. *)
  | Has_intersection of Sub_failure_reason.t Misc.Nonempty_list.t
      (** The first jkind is not a subjkind of the second, but the two jkinds have an
          intersection: try harder. *)

(** [sub_or_intersect t1 t2] does a subtype check, returning a [sub_or_intersect];
    see comments there for more info. *)
val sub_or_intersect :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> Types.jkind_l option) ->
  (allowed * 'r) Types.jkind ->
  ('l * allowed) Types.jkind ->
  sub_or_intersect

(** [sub_or_error t1 t2] does a subtype check, returning an appropriate
    [Violation.t] upon failure. *)
val sub_or_error :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> Types.jkind_l option) ->
  (allowed * 'r) Types.jkind ->
  ('l * allowed) Types.jkind ->
  (unit, Violation.t) result

(** Like [sub], but compares a left jkind against another left jkind.
    Pre-condition: the super jkind must be fully settled; no variables which
    might be filled in later.
*)
val sub_jkind_l :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> Types.jkind_l option) ->
  ?allow_any_crossing:bool ->
  Types.jkind_l ->
  Types.jkind_l ->
  (unit, Violation.t) result

(** "round up" a [jkind_l] to a [jkind_r] such that the input is less than the
    output. *)
val round_up :
  jkind_of_type:(Types.type_expr -> Types.jkind_l option) ->
  (allowed * 'r) Types.jkind ->
  ('l * allowed) Types.jkind

(** Map a function over types in [upper_bounds] *)
val map_type_expr :
  (Types.type_expr -> Types.type_expr) ->
  (allowed * 'r) Types.jkind ->
  (allowed * 'r) Types.jkind

(** Checks to see whether a jkind is {iobviously} the maximum jkind. Never does any
    mutation, preferring a quick check over a thorough one, and doesn't expand any
    with-bounds. Might return [false] even when the input is actually the maximum
    jkind. *)
val is_obviously_max : ('l * allowed) Types.jkind -> bool

(** Checks to see whether a jkind has layout any. Never does any mutation. *)
val has_layout_any : ('l * allowed) Types.jkind -> bool

(** Checks whether a jkind is [value]. This really should require a [jkind_lr],
    but it works on any [jkind], because it's used in printing and is somewhat
    unprincipled. *)
val is_value_for_printing : ignore_null:bool -> 'd Types.jkind -> bool

(*********************************)
(* debugging *)

module Debug_printers : sig
  val t : Format.formatter -> 'd Types.jkind -> unit

  module Const : sig
    val t : Format.formatter -> 'd Const.t -> unit
  end
end
