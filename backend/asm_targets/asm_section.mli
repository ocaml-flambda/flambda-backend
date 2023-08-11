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

type t =
  | DWARF of dwarf_section
  | Text

val to_string : t -> string

type section_details = private
  { names : string list;
    flags : string option;
    args : string list
  }

val dwarf_sections_in_order : unit -> t list

(* If [is_delayed] = true the corresponding section will be emitted after
   text *)
val is_delayed : t -> bool

(** The necessary information for a section directive. [first_occurrence] should
    be [true] iff the corresponding directive will be the first such in the
    relevant assembly file for the given section. *)
val details : t -> first_occurrence:bool -> section_details

val print : Format.formatter -> t -> unit

val compare : t -> t -> int

val equal : t -> t -> bool
