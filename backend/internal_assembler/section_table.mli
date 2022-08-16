(* MIT License

   Copyright (c) 2022 Jane Street Group LLC

   Permission is hereby granted, free of charge, to any person obtaining a copy
   of this software and associated documentation files (the "Software"), to deal
   in the Software without restriction, including without limitation the rights
   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
   copies of the Software, and to permit persons to whom the Software is
   furnished to do so, subject to the following conditions:

   The above copyright notice and this permission notice shall be included in
   all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
   IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
   AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
   OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE. *)
(** Construction of ELF section tables. **)

type t

val create : unit -> t

val add_section :
  t -> X86_proc.Section_name.t -> ?body:bytes -> Compiler_owee.Owee_elf.section -> unit

val current_offset : t -> int64

val num_sections : t -> int

val get_sec_idx : t -> X86_proc.Section_name.t -> int

val get_section : t -> X86_proc.Section_name.t -> Compiler_owee.Owee_elf.section

val get_section_opt :
  t -> X86_proc.Section_name.t -> Compiler_owee.Owee_elf.section option

val get_sections : t -> Compiler_owee.Owee_elf.section array

val write_bodies : t -> Compiler_owee.Owee_buf.t -> unit
