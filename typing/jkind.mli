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
(* CR externals: Move to mode.ml. But see
   https://github.com/goldfirere/flambda-backend/commit/d802597fbdaaa850e1ed9209a1305c5dcdf71e17
   first, which was reisenberg's attempt to do so. *)
module Externality : sig
  type t = Jkind_axis.Externality.t =
    | External (* not managed by the garbage collector *)
    | External64 (* not managed by the garbage collector on 64-bit systems *)
    | Internal (* managed by the garbage collector *)

  include module type of Jkind_axis.Externality with type t := t
end

module Nullability : sig
  type t = Jkind_axis.Nullability.t =
    | Non_null (* proven to not have NULL values *)
    | Maybe_null (* may have NULL values *)

  include module type of Jkind_axis.Nullability with type t := t
end

module Sort : sig
  include
    Jkind_intf.Sort
      with type base = Jkind_types.Sort.base
       and type Const.t = Jkind_types.Sort.Const.t

  module Flat : sig
    (** A flat sort is returned from [get]. *)
    type t =
      | Var of Var.id (* [Var.id] is for debugging / printing only *)
      | Base of base
  end
end

type sort = Sort.t

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

    val to_string : t -> string
  end
end

(** A Jkind.t is a full description of the runtime representation of values
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
type 'd t = (Types.type_expr, 'd) Jkind_types.t

type jkind_l := Types.jkind_l

type jkind_r := Types.jkind_r

type jkind_lr := Types.jkind_lr

type packed = Pack : 'd t -> packed [@@unboxed]

include Allowance.Allow_disallow with type (_, _, 'd) sided = 'd t

(** Try to treat this jkind as an r-jkind. *)
val try_allow_r : ('l * 'r) t -> ('l * allowed) t option

module History : sig
  include module type of struct
    include Jkind_intf.History
  end

  (* history *)

  val is_imported : 'd t -> bool

  val update_reason : 'd t -> creation_reason -> 'd t

  (* Mark the jkind as having produced a compiler warning. *)
  val with_warning : 'd t -> 'd t

  (* Whether this jkind has produced a compiler warning. *)
  val has_warned : 'd t -> bool
end

(******************************)
(* errors *)

module Violation : sig
  type violation =
    (* [Not_a_subjkind] allows l-jkinds on the right so that it can be used
       in [sub_jkind_l]. There is no downside to this, as the printing
       machinery works over l-jkinds. *)
    | Not_a_subjkind : (allowed * 'r1) t * ('l * 'r2) t -> violation
    | No_intersection : 'd t * ('l * allowed) t -> violation

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
(* constants *)

module Const : sig
  (** Constant jkinds are used for user-written annotations. They are not
      actually constant, though: they might contain variables in [with]-types.
      The "constant" refers to the fact that there are no sort variables here.
      The existence of [with]-types means, though, that we still need the
      allowance machinery here. *)
  type 'd t constraint 'd = 'l * 'r

  val to_out_jkind_const : 'd t -> Outcometree.out_jkind_const

  (** This returns [true] iff both types have no baggage and they are equal.
      Normally, we want an equality check to happen only on values that are
      allowed on both the left and the right. But a type with no baggage is
      allowed on the left and the right, so we test for that condition first
      before doing the proper equality check. *)
  val no_baggage_and_equal : 'd1 t -> 'd2 t -> bool

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

    (** Immutable values that don't contain functions. *)
    val immutable_data : t

    (** Mutable values that don't contain functions. *)
    val mutable_data : t

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

    (** This is the jkind of unboxed 128-bit simd vectors. They have sort Vec128. Does
    not mode-cross. *)
    val vec128 : t

    (** A list of all Builtin jkinds *)
    val all : t list
  end
end

module Builtin : sig
  (** This jkind is the top of the jkind lattice. All types have jkind [any].
    But we cannot compile run-time manipulations of values of types with jkind
    [any]. *)
  val any : why:History.any_creation_reason -> 'd t

  (** Value of types of this jkind are not retained at all at runtime *)
  val void : why:History.void_creation_reason -> 'd t

  val value_or_null : why:History.value_or_null_creation_reason -> 'd t

  (** This is the jkind of normal ocaml values *)
  val value : why:History.value_creation_reason -> 'd t

  (** This is suitable for records or variants without mutable fields. *)
  val immutable_data : why:History.value_creation_reason -> 'd t

  (** This is suitable for records or variants with mutable fields. *)
  val mutable_data : why:History.value_creation_reason -> 'd t

  (** We know for sure that values of types of this jkind are always immediate *)
  val immediate : why:History.immediate_creation_reason -> 'd t

  (** Build a jkind of unboxed products, from a list of types with
      their layouts. Errors if zero inputs are given. If only one input
      is given, returns the result of calling [jkind_of_first_type].

      Precondition: both input lists are the same length.

      This returns an [jkind_l] simply as a matter of convenience; it can be
      generalized if need be.  *)
  val product :
    jkind_of_first_type:(unit -> jkind_l) ->
    why:History.product_creation_reason ->
    Types.type_expr list ->
    Sort.t Layout.t list ->
    jkind_l

  (** Build a jkind of unboxed products, given only an arity. This jkind
      will not mode-cross, even though unboxed products generally should.
      This is useful when creating an initial jkind in Typedecl. *)
  val product_of_sorts : why:History.product_creation_reason -> int -> jkind_l
end

(** Take an existing [t] and add an ability to cross across the nullability axis. *)
val add_nullability_crossing : 'd t -> 'd t

(** Take an existing [t] and add some baggage. *)
val add_baggage : baggage:Types.type_expr -> jkind_l -> jkind_l

(** Does this jkind have baggage? *)
val has_baggage : jkind_l -> bool

(** Take an existing [t] and add an ability to mode-cross along the portability and
    contention axes, if [from] crosses the respective axes. Return the new jkind,
    along with a boolean of whether illegal crossing was added *)
val add_portability_and_contention_crossing :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> jkind_l option) ->
  from:'d t ->
  (allowed * 'r) t ->
  (allowed * 'r) t * bool

(******************************)
(* construction *)

(** Create a fresh sort variable, packed into a jkind, returning both
    the resulting kind and the sort. *)
val of_new_sort_var : why:History.concrete_creation_reason -> 'd t * sort

(** Create a fresh sort variable, packed into a jkind. *)
val of_new_sort : why:History.concrete_creation_reason -> 'd t

(** Same as [of_new_sort_var], but the jkind is lowered to [Non_null]
    to mirror "legacy" OCaml values.
    Defaulting the sort variable produces exactly [value].  *)
val of_new_legacy_sort_var :
  why:History.concrete_legacy_creation_reason -> 'd t * sort

(** Same as [of_new_sort], but the jkind is lowered to [Non_null]
    to mirror "legacy" OCaml values.
    Defaulting the sort variable produces exactly [value].  *)
val of_new_legacy_sort : why:History.concrete_legacy_creation_reason -> 'd t

val of_const :
  annotation:Parsetree.jkind_annotation option ->
  why:History.creation_reason ->
  'd Const.t ->
  'd t

val of_builtin : why:History.creation_reason -> Const.Builtin.t -> 'd t

val of_annotation :
  context:('l * allowed) History.annotation_context ->
  Parsetree.jkind_annotation ->
  ('l * allowed) t

val of_annotation_option_default :
  default:('l * allowed) t ->
  context:('l * allowed) History.annotation_context ->
  Parsetree.jkind_annotation option ->
  ('l * allowed) t

(** Find a jkind from a type declaration. Type declarations are special because
    the jkind may have been provided via [: jkind] syntax (which goes through
    Jane Syntax) or via the old-style [[@@immediate]] or [[@@immediate64]]
    attributes, and [of_type_decl] needs to look in two different places on the
    [type_declaration] to account for these two alternatives.

    Returns the jkind and the user-written annotation.

    Raises if a disallowed or unknown jkind is present.
*)
val of_type_decl :
  context:History.annotation_context_l ->
  transl_type:(Parsetree.core_type -> Types.type_expr) ->
  Parsetree.type_declaration ->
  (jkind_l * Parsetree.jkind_annotation option) option

(** Find a jkind from a type declaration in the same way as [of_type_decl],
    defaulting to ~default.

    Raises if a disallowed or unknown jkind is present.
*)
val of_type_decl_default :
  context:History.annotation_context_l ->
  transl_type:(Parsetree.core_type -> Types.type_expr) ->
  default:jkind_l ->
  Parsetree.type_declaration ->
  jkind_l

(** Choose an appropriate jkind for a boxed record type *)
val for_boxed_record : Types.label_declaration list -> jkind_l

(** Choose an appropriate jkind for an unboxed record type. Uses [jkind_of_type]
    only in the singleton case, where the jkind of the unboxed record must match
    that of the single field. *)
val for_unboxed_record :
  jkind_of_type:(Types.type_expr -> jkind_l) ->
  Types.label_declaration list ->
  jkind_l

(** Choose an appropriate jkind for a boxed variant type. *)
val for_boxed_variant : Types.constructor_declaration list -> jkind_l

(** The jkind of an arrow type. *)
val for_arrow : jkind_l

(** The jkind of an object type.  *)
val for_object : jkind_l

(******************************)
(* elimination and defaulting *)

module Desc : sig
  (** The description of a jkind, used as a return type from [get].  This
      description has no sort variables, but it might have [with]-types and thus
      needs the allowance machinery. *)
  type 'd t =
    (Types.type_expr, Sort.Flat.t Layout.t, 'd) Jkind_types.Layout_and_axes.t

  val get_const : 'd t -> 'd Const.t option

  val format : Format.formatter -> 'd t -> unit
end

(** Get a description of a jkind. *)
val get : 'd t -> 'd Desc.t

(** [get_layout_defaulting_to_value] extracts a constant layout, defaulting
    any sort variable to [value]. *)
val get_layout_defaulting_to_value : 'd t -> Layout.Const.t

(** [get_const] returns a [Const.t] if the layout has no sort variables,
    returning [None] otherwise *)
val get_const : 'd t -> 'd Const.t option

(** [default_to_value t] is [ignore (get_layout_defaulting_to_value t)] *)
val default_to_value : 'd t -> unit

(** [is_void t] is [Void = get_layout_defaulting_to_value t].  In particular, it
    will default the jkind to value if needed to make this false. *)
val is_void_defaulting : 'd t -> bool
(* CR layouts v5: When we have proper support for void, we'll want to change
   these three functions to default to void - it's the most efficient thing
   when we have a choice. *)

(** Returns the sort corresponding to the jkind.  Call only on representable
    jkinds - raises on Any. *)
val sort_of_jkind : jkind_l -> sort

(** Gets the layout of a jkind; returns [None] if the layout is still unknown.
    Never does mutation. *)
val get_layout : 'd t -> Layout.Const.t option

(* CR reisenberg: do we need [extract_layout]? *)

(** Gets the layout of a jkind, without looking through sort variables. *)
val extract_layout : 'd t -> Sort.t Layout.t

(** Gets the maximum modes for types of this jkind. *)
val get_modal_upper_bounds :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> jkind_l option) ->
  'd t ->
  Mode.Alloc.Const.t

(** Gets the maximum mode on the externality axis for types of this jkind. *)
val get_externality_upper_bound :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> jkind_l option) ->
  'd t ->
  Externality.t

(** Computes a jkind that is the same as the input but with an updated maximum
    mode for the externality axis *)
val set_externality_upper_bound : jkind_r -> Externality.t -> jkind_r

(** Extract out component jkinds from the product. Because there are no product
    jkinds, this is a bit of a lie: instead, this decomposes the layout but just
    reuses the non-layout parts of the original jkind. Never does any mutation.
    Because it just reuses the mode information, the resulting jkinds are higher
    in the jkind lattice than they might need to be.
    *)
val decompose_product : 'd t -> 'd t list option

(** Get an annotation (that a user might write) for this [t]. *)
val get_annotation : 'd t -> Parsetree.jkind_annotation option

(*********************************)
(* pretty printing *)

(** Call this before trying to print. *)
val set_print_type_expr : (Format.formatter -> Types.type_expr -> unit) -> unit

val format : Format.formatter -> 'd t -> unit

(** Format the history of this jkind: what interactions it has had and why
    it is the jkind that it is. Might be a no-op: see [display_histories]
    in the implementation of the [Jkind] module.

    The [intro] is something like "The jkind of t is". *)
val format_history :
  intro:(Format.formatter -> unit) -> Format.formatter -> 'd t -> unit

(** Provides the [Printtyp.path] formatter back up the dependency chain to
    this module. *)
val set_printtyp_path : (Format.formatter -> Path.t -> unit) -> unit

(******************************)
(* relations *)

(** This checks for equality, and sets any variables to make two jkinds
    equal, if possible. e.g. [equate] on a var and [value] will set the
    variable to be [value] *)
val equate : jkind_lr -> jkind_lr -> bool

(** This checks for equality, but has the invariant that it can only be called
    when there is no need for unification; e.g. [equal] on a var and [value]
    will crash.

    CR layouts (v1.5): At the moment, this is actually the same as [equate]! *)
val equal : jkind_lr -> jkind_lr -> bool

(** Checks whether two jkinds have a non-empty intersection. Might mutate
    sort variables. Works over any mix of l- and r-jkinds, because the only
    way not to have an intersection is by looking at the layout: all axes
    have a bottom element. *)
val has_intersection : 'd1 t -> 'd2 t -> bool

(** Finds the intersection of two jkinds, constraining sort variables to
    create one if needed, or returns a [Violation.t] if an intersection does
    not exist.  Can update the jkinds.  The returned jkind's history
    consists of the provided reason followed by the history of the first
    jkind argument.  That is, due to histories, this function is asymmetric;
    it should be thought of as modifying the first jkind to be the
    intersection of the two, not something that modifies the second jkind. *)
val intersection_or_error :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> jkind_l option) ->
  reason:History.interact_reason ->
  ('l1 * allowed) t ->
  ('l2 * allowed) t ->
  (('l1 * allowed) t, Violation.t) Result.t

(** [sub t1 t2] says whether [t1] is a subjkind of [t2]. Might update
    either [t1] or [t2] to make their layouts equal.*)
val sub :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> jkind_l option) ->
  jkind_l ->
  jkind_r ->
  bool

type sub_or_intersect =
  | Sub  (** The first jkind is a subjkind of the second. *)
  | Disjoint  (** The two jkinds have no common ground. *)
  | Has_intersection  (** The two jkinds have an intersection: try harder. *)

(** [sub_or_intersect t1 t2] does a subtype check, returning a [sub_or_intersect];
    see comments there for more info. *)
val sub_or_intersect :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> jkind_l option) ->
  (allowed * 'r) t ->
  ('l * allowed) t ->
  sub_or_intersect

(** [sub_or_error t1 t2] does a subtype check, returning an appropriate
    [Violation.t] upon failure. *)
val sub_or_error :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> jkind_l option) ->
  (allowed * 'r) t ->
  ('l * allowed) t ->
  (unit, Violation.t) result

(** Like [sub], but returns the subjkind with an updated history.
    Pre-condition: the super jkind must be fully settled; no variables which
    might be filled in later. Right now, if the super jkind has any
    [with]-types, the sub jkind must have exactly the same [with]-types, in the
    same order.

    CR layouts v2.8: Implement this properly.
*)
val sub_jkind_l :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> jkind_l option) ->
  jkind_l ->
  jkind_l ->
  (jkind_l, Violation.t) result

(** "round up" a [jkind_l] to a [jkind_r] such that the input is less than the
    output. *)
val round_up :
  type_equal:(Types.type_expr -> Types.type_expr -> bool) ->
  jkind_of_type:(Types.type_expr -> jkind_l option) ->
  (allowed * 'r) t ->
  ('l * allowed) t

(** Map a function over types in [upper_bounds] *)
val map_type_expr :
  (Types.type_expr -> Types.type_expr) -> (allowed * 'r) t -> (allowed * 'r) t

(** Checks to see whether a jkind is the maximum jkind. Never does any
    mutation. *)
val is_max : ('l * allowed) t -> bool

(** Checks to see whether a jkind has layout any. Never does any mutation. *)
val has_layout_any : ('l * allowed) t -> bool

(** Checks whether a jkind is [value]. This really should require a [jkind_lr],
    but it works on any [jkind], because it's used in printing and is somewhat
    unprincipled. *)
val is_value_for_printing : 'd t -> bool

(*********************************)
(* debugging *)

module Debug_printers : sig
  val t :
    print_type_expr:(Format.formatter -> Types.type_expr -> unit) ->
    Format.formatter ->
    'd t ->
    unit

  module Const : sig
    val t :
      print_type_expr:(Format.formatter -> Types.type_expr -> unit) ->
      Format.formatter ->
      'd Const.t ->
      unit
  end
end
