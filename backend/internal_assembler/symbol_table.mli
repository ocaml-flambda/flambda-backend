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
(** Construction of ELF symbol tables. *)
module String = Misc.Stdlib.String

type t

val create : unit -> t

val add_symbol : t -> Symbol_entry.t -> unit

val add_label : t -> X86_binary_emitter.symbol -> Symbol_entry.t -> unit

val get_label :
  t -> String.Tbl.key -> X86_binary_emitter.symbol * Symbol_entry.t

val get_label_idx : t -> String.Tbl.key -> X86_binary_emitter.symbol * int

val get_symbol_idx_opt : t -> String.Tbl.key -> int option

val num_symbols : t -> int

val num_locals : t -> int

val make_undef_symbol : t -> string -> String_table.t -> unit

val make_section_symbol : t -> Compiler_owee.Owee_buf.u16 -> 'a -> Symbol_entry.t

val make_symbol :
  t -> X86_binary_emitter.symbol -> Section_table.t -> String_table.t -> unit

val write : t -> int64 -> Compiler_owee.Owee_buf.t -> unit
