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

(** The linker may share constants in [Eight_byte_literals] and
    [Sixteen_byte_literals] sections. *)
type t =
  | Text
  | Data
  | Read_only_data
  | Eight_byte_literals
  | Sixteen_byte_literals
  | Jump_tables
  | DWARF of dwarf_section

let to_string : t -> string = failwith "not implemented"

let all_sections_in_order : unit -> t list = failwith "not implemented"

(** Whether the section holds code. *)
let section_is_text : t -> bool = failwith "not implemented"

type flags_for_section = private {
  names : string list;
  flags : string option;
  args : string list;
}

(** The necessary information for a section directive.  [first_occurrence]
    should be [true] iff the corresponding directive will be the first such
    in the relevant assembly file for the given section. *)
let flags : t -> first_occurrence:bool -> flags_for_section = failwith "not implemented"

let print : Format.formatter -> t -> unit = failwith "not implemented"

let compare : t -> t -> int = failwith "not implemented"

let equal : t -> t -> bool = failwith "not implemented"