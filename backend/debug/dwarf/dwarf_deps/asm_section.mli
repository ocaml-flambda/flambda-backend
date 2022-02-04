(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2016--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Representation of object file sections. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(** Sections that hold DWARF debugging information. *)
type dwarf_section =
  | Debug_info
  | Debug_abbrev
  | Debug_aranges
  | Debug_addr
  | Debug_loc
  | Debug_ranges
  | Debug_loclists
  | Debug_rnglists
  | Debug_str
  | Debug_line

type t = DWARF of dwarf_section

val to_string : t -> string

type flags_for_section = private
  { names : string list;
    flags : string option;
    args : string list
  }

val dwarf_sections_in_order : unit -> t list

(** The necessary information for a section directive. [first_occurrence] should
    be [true] iff the corresponding directive will be the first such in the
    relevant assembly file for the given section. *)
val flags : t -> first_occurrence:bool -> flags_for_section

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

val equal : t -> t -> bool
