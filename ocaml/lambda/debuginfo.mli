(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2006 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Scoped_location : sig
  type scope_item = private
    | Sc_anonymous_function
    | Sc_value_definition
    | Sc_module_definition
    | Sc_class_definition
    | Sc_method_definition
    | Sc_partial_or_eta_wrapper
    | Sc_lazy

  type scopes = private
    | Empty
    | Cons of {item: scope_item; str: string; str_fun: string; name : string; prev: scopes;
               assume_zero_alloc: bool}

  val string_of_scopes : scopes -> string

  val empty_scopes : scopes
  val enter_anonymous_function : scopes:scopes -> assume_zero_alloc:bool -> scopes
  val enter_value_definition : scopes:scopes -> assume_zero_alloc:bool -> Ident.t -> scopes
  val enter_compilation_unit : scopes:scopes -> Compilation_unit.t -> scopes
  val enter_module_definition : scopes:scopes -> Ident.t -> scopes
  val enter_class_definition : scopes:scopes -> Ident.t -> scopes
  val enter_method_definition : scopes:scopes -> Asttypes.label -> scopes
  val enter_lazy : scopes:scopes -> scopes
  val enter_partial_or_eta_wrapper : scopes:scopes -> scopes
  val set_assume_zero_alloc : scopes:scopes -> scopes
  val get_assume_zero_alloc : scopes:scopes -> bool

  type t =
    | Loc_unknown
    | Loc_known of
        { loc : Location.t;
          scopes : scopes; }

  val of_location : scopes:scopes -> Location.t -> t
  val to_location : t -> Location.t
  val string_of_scoped_location : t -> string

  val map_scopes : (scopes:scopes -> scopes) -> t -> t
end

type item = private {
  dinfo_file: string;
  dinfo_line: int;
  dinfo_char_start: int;
  dinfo_char_end: int;
  dinfo_start_bol: int;
  dinfo_end_bol: int;
  dinfo_end_line: int;
  dinfo_scopes: Scoped_location.scopes;
}

type t

type alloc_dbginfo_item =
  { alloc_words : int;
    alloc_dbg : t }
(** Due to Comballoc, a single Ialloc instruction may combine several
    unrelated allocations. Their Debuginfo.t (which may differ) are stored
    as a list of alloc_dbginfo. This list is in order of increasing memory
    address, which is the reverse of the original allocation order. Later
    allocations are consed to the front of this list by Comballoc. *)

type alloc_dbginfo = alloc_dbginfo_item list

val none : t

val is_none : t -> bool

val to_string : t -> string

val from_location : Scoped_location.t -> t

val to_location : t -> Location.t

val inline : t -> t -> t

val compare : t -> t -> int

val print_compact : Format.formatter -> t -> unit

val merge : into:t -> t -> t

val assume_zero_alloc : t -> bool

module Dbg : sig
  type t

  (** [compare] and [hash] ignore [dinfo_scopes] field of item *)

  val is_none : t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val to_list : t -> item list
  val length : t -> int
end

val get_dbg : t -> Dbg.t

