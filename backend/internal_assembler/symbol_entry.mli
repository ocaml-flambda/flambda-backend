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
(** Represents an entry into a symbol table. **)
type t

type bind =
  | Local
  | Global

type symbol_type =
  | NoType
  | Object
  | Func
  | Section
  | File
  | Common
  | Tls

val create_undef_symbol : string -> String_table.t -> t

val create_section_symbol : Compiler_owee.Owee_buf.u16 -> t

val create_symbol :
  X86_binary_emitter.symbol -> 'a -> Section_table.t -> String_table.t -> t

val create_got_symbol : 'a -> String_table.t -> t

val get_bind : t -> bind

val get_type : t -> symbol_type

val get_shndx : t -> Compiler_owee.Owee_buf.u16

val get_name_str : t -> string

val write : t -> Compiler_owee.Owee_buf.cursor -> unit
