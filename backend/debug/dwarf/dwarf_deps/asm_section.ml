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

[@@@ocaml.warning "+a-4-30-40-41-42"]

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

type flags_for_section = {
  names : string list;
  flags : string option;
  args : string list;
}

let dwarf_sections_in_order () =
  let sections = [
    DWARF Debug_info;
    DWARF Debug_abbrev;
    DWARF Debug_aranges;
    DWARF Debug_str;
    DWARF Debug_line;
  ] in
  let dwarf_version_dependent_sections =
    match !Clflags.gdwarf_version with
    | Four ->
      [ DWARF Debug_loc;
        DWARF Debug_ranges;
      ]
    | Five ->
      [ DWARF Debug_addr;
        DWARF Debug_loclists;
        DWARF Debug_rnglists;
      ]
  in
  sections @ dwarf_version_dependent_sections

let flags t ~first_occurrence =
  let names, flags, args =
    match t, Config_typed.derived_system () with
    | DWARF dwarf, MacOS_like ->
      let name =
        match dwarf with
        | Debug_info -> "__debug_info"
        | Debug_abbrev -> "__debug_abbrev"
        | Debug_aranges -> "__debug_aranges"
        | Debug_addr -> "__debug_addr"
        | Debug_loc -> "__debug_loc"
        | Debug_ranges -> "__debug_ranges"
        | Debug_loclists -> "__debug_loclists"
        | Debug_rnglists -> "__debug_rnglists"
        | Debug_str -> "__debug_str"
        | Debug_line -> "__debug_line"
      in
      ["__DWARF"; name], None, ["regular"; "debug"]
    | DWARF dwarf, _ ->
      let name =
        match dwarf with
        | Debug_info -> ".debug_info"
        | Debug_abbrev -> ".debug_abbrev"
        | Debug_aranges -> ".debug_aranges"
        | Debug_addr -> ".debug_addr"
        | Debug_loc -> ".debug_loc"
        | Debug_ranges -> ".debug_ranges"
        | Debug_loclists -> ".debug_loclists"
        | Debug_rnglists -> ".debug_rnglists"
        | Debug_str -> ".debug_str"
        | Debug_line -> ".debug_line"
      in
      let flags =
        if first_occurrence then
          Some ""
        else
          None
      in
      let args =
        if first_occurrence then
          ["%progbits"]
        else
          []
      in
      [name], flags, args
  in
  { names; flags; args; }

let to_string t =
  let { names; flags = _; args = _; } = flags t ~first_occurrence:true in
  String.concat " " names

let print ppf t =
  let str =
    match t with
    | DWARF Debug_info -> "(DWARF Debug_info)"
    | DWARF Debug_abbrev -> "(DWARF Debug_abbrev)"
    | DWARF Debug_aranges -> "(DWARF Debug_aranges)"
    | DWARF Debug_addr -> "(DWARF Debug_addr)"
    | DWARF Debug_loc -> "(DWARF Debug_loc)"
    | DWARF Debug_ranges -> "(DWARF Debug_ranges)"
    | DWARF Debug_loclists -> "(DWARF Debug_loclists)"
    | DWARF Debug_rnglists -> "(DWARF Debug_rnglists)"
    | DWARF Debug_str -> "(DWARF Debug_str)"
    | DWARF Debug_line -> "(DWARF Debug_line)"
  in
  Format.pp_print_string ppf str

let compare t1 t2 =
  Stdlib.compare t1 t2

let equal t1 t2 =
  Stdlib.compare t1 t2 = 0
