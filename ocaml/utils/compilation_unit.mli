(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Pierre Chambart and Pierrick Couderc, OCamlPro               *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Handling of the names of compilation units, including associated "-for-pack"
   prefixes.

   By "compilation unit" we mean the code and data associated with the
   compilation of a single .ml source file: that is to say, file-level
   entities having OCaml semantics.  The notion neither includes the special
   "startup" files nor external libraries.
*)

[@@@ocaml.warning "+a-9-40-41-42"]

module Name : sig
  (** The name of a compilation unit without any "-for-pack" prefix. *)
  type t

  (** Printing, comparison, sets, maps, etc. *)
  include Identifiable.S with type t := t

  (** [dummy] is a placeholder for units that does not have a valid name, as
      in the, or during initialisation of the compiler.  It is not a valid
      identifier and thus cannot be generated through [of_string]. *)
  val dummy : t

  (** [of_string s] checks the given module name is a valid compilation unit
      name and generates its representation. *)
  val of_string : string -> t

  val to_string : t -> string

  val persistent_ident : t -> Ident.t
end

module Prefix : sig
  (** A pack name prefix, as specified to "-for-pack".  Such a prefix may
      be empty. *)
  type t

  (** Printing, comparison, sets, maps, etc. *)
  include Identifiable.S with type t := t

  val empty : t

  (** [parse_for_pack p] returns the list of nested packed modules from a
      "-for-pack" argument. *)
  val parse_for_pack : string option -> t

  val from_clflags : unit -> t

  (** Return the list of names comprising the prefix, outermost first. *)
  val to_list : t -> Name.t list

  val to_string : t -> string

  val is_empty : t -> bool
end

(** The name of a compilation unit qualified with any "-for-pack" prefix that
    was specified when the unit was compiled.  For example if compiling foo.ml
    with "-for-pack Baz.Bar", the corresponding value of type [t] would
    represent "Baz.Bar.Foo", with its [name] representing "Foo" and its
    [prefix] representing "Baz.Bar". *)
type t

(** Printing, comparison, sets, maps, etc. *)
include Identifiable.S with type t := t

(** Print only the name of the given compilation unit. *)
val print_name : Format.formatter -> t -> unit

val print_debug : Format.formatter -> t -> unit

(** Create a compilation unit with the given [name] (which is not encoded or
    mangled in any way). *)
val create : Prefix.t -> Name.t -> t

(** Create a compilation unit from the given [name]. The "-for-pack" of
    prefix is extracted if there is any. *)
val of_string : string -> t

(** Create a global [Ident.t] representing this compilation unit. DO NOT USE
    if you can possibly avoid it. Most uses of [Ident.t]s that are known to be
    global should simply use [t] instead. *)
(* CR mshinwell: Delete this as soon as the functor packs work is finished. *)
val to_global_ident_for_legacy_code : t -> Ident.t

(** Find whether one compilation unit has another as a child. That is, whether
    the other unit has this one as its path prefix. *)
val is_parent : t -> child:t -> bool

(** A distinguished compilation unit for initialisation of mutable state. *)
val dummy : t

(** A distinguished compilation unit for predefined exceptions. *)
val predef_exn : t

(** The name of the compilation unit, excluding any [for_pack_prefix]. *)
val name : t -> Name.t

(** The name of the compilation unit, excluding any [for_pack_prefix], as
    as a string. *)

(* CR mshinwell: Try to delete this as soon as the functor packs work is
   finished. *)
val name_as_string : t -> string

(** The "-for-pack" prefix associated with the given compilation unit. *)
val for_pack_prefix : t -> Prefix.t

(** Replace the "-for-pack" prefix for the given compilation unit. *)
val with_for_pack_prefix : t -> Prefix.t -> t

(** Returns [true] iff the given compilation unit has a non-empty
    [for_pack_prefix]. *)
val is_packed : t -> bool

(** Returns the full path of the compilation unit.  The basename of the unit
    will be the last component of the returned list. *)
val full_path : t -> Name.t list

(** Returns the full path of the compilation unit, as a string, following
    usual conventions. *)
val full_path_as_string : t -> string

type error = private
  | Invalid_character of char
  | Bad_compilation_unit_name of string

(** The exception raised by conversion functions in this module. *)
exception Error of error

val set_current : t -> unit
val get_current : unit -> t option
val get_current_exn : unit -> t
val is_current : t -> bool
