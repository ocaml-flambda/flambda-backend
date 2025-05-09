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

(** A label in a specific section within the assembly stream. They may be either
    numeric or textual. (Numeric ones are converted to textual ones by this
    module.) The argument to [String] should not include any platform-specific
    prefix (such as "L", ".L", etc).

    Label's numeric or textual names are unique within the assembly stream for a
    single compilation unit, even across sections. [Asm_label] keeps track of
    the section a label belongs to, and doesn't check uniqueness.

    Note: Labels are not symbols in the usual sense---they are a construct in
    the assembler's metalanguage and not accessible in the object
    file---although on macOS the terminology for labels appears to be "assembler
    local symbols". *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

(** Create a fresh integer-valued label (using the [new_label] function passed
    to [initialize], below). *)
val create : Asm_section.t -> t

(** Create an integer-valued label. The int must be positive. *)
val create_int : Asm_section.t -> int -> t

(** Create a textual label. The supplied name must not require escaping. *)
val create_string : Asm_section.t -> string -> t

(** Convert a label to the corresponding textual form, suitable for direct
    emission into an assembly file. This may be useful e.g. when emitting an
    instruction referencing a label. *)
val encode : t -> string

(** To be called by the emitter at the very start of code generation.
    [new_label] should always be [Cmm.new_label]. Needed to avoid a circular
    dependency. *)
val initialize : new_label:(unit -> int) -> unit

(** Which section a label is in. *)
val section : t -> Asm_section.t

include Identifiable.S with type t := t

(** Retrieve a distinguished label that is suitable for identifying the start of
    the given section within a given compilation unit's assembly file. *)
val for_section : Asm_section.t -> t

(** Like [for_section], but for DWARF sections only. *)
val for_dwarf_section : Asm_section.dwarf_section -> t
