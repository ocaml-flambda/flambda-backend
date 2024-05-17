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
  type t = Jkind_types.Externality.t =
    | External (* not managed by the garbage collector *)
    | External64 (* not managed by the garbage collector on 64-bit systems *)
    | Internal (* managed by the garbage collector *)

  val le : t -> t -> bool
end

module Sort :
  Jkind_intf.Sort
    with type t = Jkind_types.Sort.t
     and type const = Jkind_types.Sort.const

type sort = Sort.t

(** The layout of a type describes its memory layout. A layout is either the
    indeterminate [Any], a sort, which is a concrete memory layout, or
    [Non_null_value], which is a sublayout of the sort [Value] describing types
    that do not allow the concrete value null. [Non_null_value] is also the
    layout of "classical" OCaml values used by the upstream compiler. *)
module Layout : sig
  module Const : sig
    type t = (Types.type_expr, Sort.const) Jkind_types.Layout.layout
  end
end

(** A Jkind.t is a full description of the runtime representation of values
    of a given type. It includes sorts, but also the abstract top jkind
    [Any] and subjkinds of other sorts, such as [Immediate]. *)
type t = Types.type_expr Jkind_types.t

include module type of struct
  include Jkind_intf.History
end

(******************************)
(* errors *)

module Violation : sig
  type violation =
    | Not_a_subjkind of t * t
    | No_intersection of t * t

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

(** Constant jkinds are used both for user-written annotations and within
    the type checker when we know a jkind has no variables *)
type const = Jkind_types.const =
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
  | Non_null_value

val const_of_user_written_annotation :
  context:annotation_context -> Jane_syntax.Jkind.annotation -> const

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

(** This is the jkind of unboxed 32-bit floats.  They have sort Float32. *)
val float32 : why:float32_creation_reason -> t

(** This is the jkind of unboxed native-sized integers. They have sort Word. *)
val word : why:word_creation_reason -> t

(** This is the jkind of unboxed 32-bit integers. They have sort Bits32. *)
val bits32 : why:bits32_creation_reason -> t

(** This is the jkind of unboxed 64-bit integers. They have sort Bits64. *)
val bits64 : why:bits64_creation_reason -> t

(******************************)
(* construction *)

(** Create a fresh sort variable, packed into a jkind, returning both
    the resulting kind and the sort. *)
val of_new_sort_var : why:concrete_jkind_reason -> t * sort

(** Create a fresh sort variable, packed into a jkind. *)
val of_new_sort : why:concrete_jkind_reason -> t

val of_const : why:creation_reason -> const -> t

(** The typed jkind together with its user-written annotation. *)
type annotation = Jkind_types.annotation

val of_annotation :
  context:annotation_context -> Jane_syntax.Jkind.annotation -> t * annotation

val of_annotation_option_default :
  default:t ->
  context:annotation_context ->
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
  context:annotation_context ->
  Parsetree.type_declaration ->
  (t * annotation * Parsetree.attributes) option

(** Find a jkind from a type declaration in the same way as [of_type_decl],
    defaulting to ~default.

    Raises if a disallowed or unknown jkind is present.
*)
val of_type_decl_default :
  context:annotation_context ->
  default:t ->
  Parsetree.type_declaration ->
  t * annotation option * Parsetree.attributes

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

(** Gets the layout of a jkind; returns [None] if the layout is still unknown.
    Never does mutation. *)
val get_layout : t -> Layout.Const.t option

(** Gets the maximum modes for types of this jkind. *)
val get_modal_upper_bounds : t -> Mode.Alloc.Const.t

(** Gets the maximum mode on the externality axis for types of this jkind. *)
val get_externality_upper_bound : t -> Externality.t

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
(* history *)

val has_imported_history : t -> bool

val update_reason : t -> creation_reason -> t

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

(** [sub t1 t2] says whether [t1] is a subjkind of [t2]. Might update
    either [t1] or [t2] to make their layouts equal.*)
val sub : t -> t -> bool

(** [sub_or_error t1 t2] returns [Ok ()] iff [t1] is a subjkind of
  of [t2]. Otherwise returns an appropriate error to report to the user. *)
val sub_or_error : t -> t -> (unit, Violation.t) result

(** Like [sub], but returns the subjkind with an updated history. *)
val sub_with_history : t -> t -> (t, Violation.t) result

(** Checks to see whether a jkind is any. Never does any mutation. *)
val is_any : t -> bool

(*********************************)
(* debugging *)

module Debug_printers : sig
  val t : Format.formatter -> t -> unit
end
