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

type label =
  | Int of int
  | String of string

type t =
  { section : Asm_section.t;
    label : label
  }

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 = Stdlib.compare t1.label t2.label

  let equal t1 t2 = compare t1 t2 = 0

  let hash t = Hashtbl.hash t.label

  let print ppf t =
    match t.label with
    | Int i -> Format.fprintf ppf "L%d" i
    | String s -> Format.fprintf ppf "L%s" s

  let output _ _ = Misc.fatal_error "Not yet implemented"
end)

let create_int section label =
  assert (label >= 0);
  { section; label = Int label }

let contains_escapable_char label =
  let found_escapable_char = ref false in
  String.iter
    (fun c ->
      if Asm_symbol.should_be_escaped c then found_escapable_char := true)
    label;
  !found_escapable_char

let create_string section label =
  assert (not (contains_escapable_char label));
  { section; label = String label }

let label_prefix =
  match Target_system.assembler () with MacOS -> "L" | MASM | GAS_like -> ".L"

let encode (t : t) =
  match t.label with
  | Int label -> label_prefix ^ string_of_int label
  | String label -> label_prefix ^ label

let section t = t.section

let new_label_ref =
  ref (fun _section ->
      Misc.fatal_error "[Asm_label.initialize] has not been called")

let initialize ~new_label =
  new_label_ref := fun section -> create_int section (new_label ())

let create (section : Asm_section.t) = !new_label_ref section

(* CR-someday poechsel: With a function such as `index :
   Asm_section.dwarf_section -> int`, we could maintain an association array and
   get rid of all these variables plus the pattern matching in
   for_dwarf_section. *)

let debug_info_label = lazy (create (DWARF Debug_info))

let debug_abbrev_label = lazy (create (DWARF Debug_abbrev))

let debug_aranges_label = lazy (create (DWARF Debug_aranges))

let debug_addr_label = lazy (create (DWARF Debug_addr))

let debug_loc_label = lazy (create (DWARF Debug_loc))

let debug_ranges_label = lazy (create (DWARF Debug_ranges))

let debug_loclists_label = lazy (create (DWARF Debug_loclists))

let debug_rnglists_label = lazy (create (DWARF Debug_rnglists))

let debug_str_label = lazy (create (DWARF Debug_str))

let debug_line_label = lazy (create (DWARF Debug_line))

let for_dwarf_section (dwarf_section : Asm_section.dwarf_section) =
  match dwarf_section with
  | Debug_info -> Lazy.force debug_info_label
  | Debug_abbrev -> Lazy.force debug_abbrev_label
  | Debug_aranges -> Lazy.force debug_aranges_label
  | Debug_addr -> Lazy.force debug_addr_label
  | Debug_loc -> Lazy.force debug_loc_label
  | Debug_ranges -> Lazy.force debug_ranges_label
  | Debug_loclists -> Lazy.force debug_loclists_label
  | Debug_rnglists -> Lazy.force debug_rnglists_label
  | Debug_str -> Lazy.force debug_str_label
  | Debug_line -> Lazy.force debug_line_label

let for_section (section : Asm_section.t) =
  match section with
  | DWARF dwarf_section -> for_dwarf_section dwarf_section
  | Text -> Misc.fatal_error "Not yet implemented"
