(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2014-2022 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

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

type t =
  | DWARF of dwarf_section
  | Text

type section_details =
  { names : string list;
    flags : string option;
    args : string list
  }

let dwarf_sections_in_order () =
  let sections =
    [ DWARF Debug_info;
      DWARF Debug_abbrev;
      DWARF Debug_aranges;
      DWARF Debug_str;
      DWARF Debug_line ]
  in
  let dwarf_version_dependent_sections =
    match !Dwarf_flags.gdwarf_version with
    | Four -> [DWARF Debug_loc; DWARF Debug_ranges]
    | Five -> [DWARF Debug_addr; DWARF Debug_loclists; DWARF Debug_rnglists]
  in
  sections @ dwarf_version_dependent_sections

let is_delayed = function
  | DWARF Debug_line -> true
  | DWARF
      ( Debug_info | Debug_abbrev | Debug_aranges | Debug_str | Debug_loclists
      | Debug_rnglists | Debug_addr | Debug_loc | Debug_ranges )
  | Text ->
    false

let details t ~first_occurrence =
  let names, flags, args =
    match t, Target_system.derived_system () with
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
        match first_occurrence, dwarf with
        | true, Debug_str -> Some "MS"
        | true, _ -> Some ""
        | false, _ -> None
      in
      let args =
        match first_occurrence, dwarf with
        | true, Debug_str -> ["%progbits,1"]
        | true, _ -> ["%progbits"]
        | false, _ -> []
      in
      [name], flags, args
    | Text, _ -> Misc.fatal_error "Not yet implemented"
  in
  { names; flags; args }

let to_string t =
  let { names; flags = _; args = _ } = details t ~first_occurrence:true in
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
    | Text -> "Text"
  in
  Format.pp_print_string ppf str

let compare t1 t2 = Stdlib.compare t1 t2

let equal t1 t2 = Stdlib.compare t1 t2 = 0
