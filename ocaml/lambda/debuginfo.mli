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

module ZA = Zero_alloc_utils

module Scoped_location : sig
  type scope_item = private
    | Sc_anonymous_function
    | Sc_value_definition
    | Sc_module_definition
    | Sc_class_definition
    | Sc_method_definition
    | Sc_partial_or_eta_wrapper
    | Sc_lazy

  val equal_scope_item : scope_item -> scope_item -> bool

  type scopes = private
    | Empty
    | Cons of {item: scope_item; str: string; str_fun: string; name : string; prev: scopes;
               assume_zero_alloc: ZA.Assume_info.t}

  val string_of_scopes : include_zero_alloc:bool -> scopes -> string

  val compilation_unit : scopes -> Compilation_unit.t option

  val empty_scopes : scopes
  val enter_anonymous_function :
    scopes:scopes -> assume_zero_alloc:ZA.Assume_info.t -> scopes
  val enter_value_definition :
    scopes:scopes -> assume_zero_alloc:ZA.Assume_info.t -> Ident.t -> scopes
  val enter_compilation_unit : scopes:scopes -> Compilation_unit.t -> scopes
  val enter_module_definition : scopes:scopes -> Ident.t -> scopes
  val enter_class_definition : scopes:scopes -> Ident.t -> scopes
  val enter_method_definition : scopes:scopes -> Asttypes.label -> scopes
  val enter_lazy : scopes:scopes -> scopes
  val enter_partial_or_eta_wrapper : scopes:scopes -> scopes
  val update_assume_zero_alloc :
    scopes:scopes -> assume_zero_alloc:ZA.Assume_info.t -> scopes
  val get_assume_zero_alloc : scopes:scopes -> ZA.Assume_info.t

  type t =
    | Loc_unknown
    | Loc_known of
        { loc : Location.t;
          scopes : scopes; }

  val of_location : scopes:scopes -> Location.t -> t
  val to_location : t -> Location.t
  val string_of_scoped_location : include_zero_alloc:bool -> t -> string

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
  (** See the [Inlined_debuginfo] module in Flambda 2 for an explanation
      of the uid and function symbol fields.  (They are used for generation
      of DWARF inlined frame information.)  These fields should only be
      set to [Some] by Flambda 2. *)
  dinfo_uid: string option;
  dinfo_function_symbol: string option;
}

val item_with_uid_and_function_symbol : item -> dinfo_uid:string option
  -> dinfo_function_symbol:string option -> item

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

val of_items : item list -> t

val mapi_items : t -> f:(int -> item -> item) -> t

val to_items : t -> item list

val to_string : t -> string

val from_location : Scoped_location.t -> t

val to_location : t -> Location.t

val inline : t -> from_inlined_body:t -> t

val compare : t -> t -> int

val print_compact : Format.formatter -> t -> unit

(** Like [print_compact] but also prints uid and function symbol info. *)
val print_compact_extended : Format.formatter -> t -> unit

val merge : into:t -> t -> t

val assume_zero_alloc : t -> ZA.Assume_info.t

module Dbg : sig
  type t

  (** [compare] and [hash] ignore [dinfo_scopes] field of item *)

  val is_none : t -> bool

  (** [compare] Inner-most inlined debug info is used first. Allocates. *)
  val compare : t -> t -> int

  (** [compare_outer_first] Outer-most inlined debug info is used first.
      Does not allocate. *)
  val compare_outer_first : t -> t -> int

  val hash : t -> int
  val to_list : t -> item list
  val length : t -> int
end

val get_dbg : t -> Dbg.t

