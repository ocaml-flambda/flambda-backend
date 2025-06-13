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

open! Int_replace_polymorphic_compare

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
  | Data
  | Read_only_data
  | Eight_byte_literals
  | Sixteen_byte_literals
  | Thirtytwo_byte_literals
  | Sixtyfour_byte_literals
  | Jump_tables
  | Text
  | Stapsdt_base
  | Stapsdt_note
  | Probes
  | Note_ocaml_eh

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
  (* Only .debug_line and .debug_frames are delayed. All other sections should
     be emitted directly. See PR #1719. *)
  | DWARF Debug_line -> true
  | DWARF
      ( Debug_info | Debug_abbrev | Debug_aranges | Debug_str | Debug_loclists
      | Debug_rnglists | Debug_addr | Debug_loc | Debug_ranges )
  | Data | Read_only_data | Eight_byte_literals | Sixteen_byte_literals
  | Thirtytwo_byte_literals | Sixtyfour_byte_literals | Jump_tables | Text
  | Stapsdt_base | Stapsdt_note | Probes | Note_ocaml_eh ->
    false

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
    | Data -> "Data"
    | Read_only_data -> "Read_only_data"
    | Eight_byte_literals -> "Eight_byte_literals"
    | Sixteen_byte_literals -> "Sixteen_byte_literals"
    | Thirtytwo_byte_literals -> "Thirtytwo_byte_literals"
    | Sixtyfour_byte_literals -> "Sixtyfour_byte_literals"
    | Jump_tables -> "Jump_tables"
    | Text -> "Text"
    | Stapsdt_base -> "Stapsdt_base"
    | Stapsdt_note -> "Stapsdt_note"
    | Probes -> "Probes"
    | Note_ocaml_eh -> "Note_ocaml_eh"
  in
  Format.pp_print_string ppf str

let compare t1 t2 = Stdlib.compare t1 t2

let equal t1 t2 = Stdlib.compare t1 t2 = 0

let section_is_text = function
  | Text -> true
  | Data | Read_only_data | Eight_byte_literals | Sixteen_byte_literals
  | Thirtytwo_byte_literals | Sixtyfour_byte_literals | Jump_tables | DWARF _
  | Stapsdt_base | Stapsdt_note | Probes | Note_ocaml_eh ->
    false

type section_details =
  { names : string list;
    flags : string option;
    args : string list;
    is_delayed : bool
  }

let details t ~first_occurrence =
  let text () = [".text"], None, [] in
  let data () = [".data"], None, [] in
  let rodata () = [".rodata"], None, [] in
  let system = Target_system.derived_system () in
  let names, flags, args =
    match t, Target_system.architecture (), system with
    | Text, _, _ -> text ()
    | Data, _, _ -> data ()
    | DWARF dwarf, _, MacOS_like ->
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
    | DWARF dwarf, _, _ ->
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
        | true, Debug_str -> Some "MS" (* #3078 *)
        | true, _ -> Some ""
        | false, _ -> None
      in
      let args =
        match first_occurrence, dwarf with
        | true, Debug_str -> ["%progbits,1"] (* #3078 *)
        | true, _ -> ["%progbits"]
        | false, _ -> []
      in
      [name], flags, args
    (* Eight Byte Literals; based on corresponding upstream secions *)
    | Eight_byte_literals, _, MacOS_like ->
      ["__TEXT"; "__literal8"], None, ["8byte_literals"]
    | Eight_byte_literals, _, (MinGW_64 | Cygwin) -> [".rdata"], Some "dr", []
    | Eight_byte_literals, _, Win64 -> data ()
    | Eight_byte_literals, _, _ ->
      [".rodata.cst8"], Some "aM", ["@progbits"; "8"]
    (* Sixteen Byte Literals; based on corresponding upstream secions *)
    | Sixteen_byte_literals, _, MacOS_like ->
      ["__TEXT"; "__literal16"], None, ["16byte_literals"]
    | Sixteen_byte_literals, _, (MinGW_64 | Cygwin) -> [".rdata"], Some "dr", []
    | Sixteen_byte_literals, _, Win64 -> data ()
    | Sixteen_byte_literals, _, _ ->
      [".rodata.cst16"], Some "aM", ["@progbits"; "16"]
    | Thirtytwo_byte_literals, _, (MinGW_64 | Cygwin) ->
      [".rdata"], Some "dr", []
    | Thirtytwo_byte_literals, _, Win64 -> data ()
    | Thirtytwo_byte_literals, _, _ ->
      [".rodata.cst32"], Some "aM", ["@progbits"; "32"]
    | Sixtyfour_byte_literals, _, (MinGW_64 | Cygwin) ->
      [".rdata"], Some "dr", []
    | Sixtyfour_byte_literals, _, Win64 -> data ()
    | Sixtyfour_byte_literals, _, _ ->
      [".rodata.cst64"], Some "aM", ["@progbits"; "64"]
    | Jump_tables, _, (MinGW_64 | Cygwin) -> [".rdata"], Some "dr", []
    | Jump_tables, _, (MinGW_32 | Win32) -> data ()
    | Jump_tables, _, (MacOS_like | Win64) ->
      text () (* with LLVM/OS X and MASM, use the text segment *)
    | Jump_tables, _, _ -> [".rodata"], None, []
    | Read_only_data, _, (MinGW_32 | Win32) -> data ()
    | Read_only_data, _, (MinGW_64 | Cygwin) -> [".rdata"], Some "dr", []
    | Read_only_data, _, _ -> rodata ()
    | Stapsdt_base, _, Linux ->
      [".stapsdt.base"], Some "aG", ["\"progbits\""; ".stapsdt.base"; "comdat"]
    | Stapsdt_base, _, _ ->
      Misc.fatal_error "stapsdt not supported on platforms other than Linux."
    | Stapsdt_note, _, MacOS_like ->
      ["__DATA"; "__note_stapsdt"], None, ["regular"]
      (* NOTE: This is section is currently not tested. *)
    | Stapsdt_note, _, (GNU | Solaris | Linux | Generic_BSD | BeOS) ->
      [".note.stapsdt"], Some "?", ["\"note\""]
    | Stapsdt_note, _, _ ->
      Misc.fatal_error "Target systems does not support stapsdt."
    | Probes, _, MacOS_like -> ["__TEXT"; "__probes"], None, ["regular"]
    | Probes, _, _ -> [".probes"], Some "wa", ["\"progbits\""]
    | Note_ocaml_eh, _, _ -> [".note.ocaml_eh"], Some "?", ["\"note\""]
  in
  let is_delayed = is_delayed t in
  { names; flags; args; is_delayed }

let to_string t =
  let { names; flags = _; args = _; is_delayed = _ } =
    details t ~first_occurrence:true
  in
  String.concat " " names
