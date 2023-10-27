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

type t =
  { section : X86_proc.Section_name.t;
    mutable relocations : Relocation_entry.t list
  }

module StringMap = X86_binary_emitter.StringMap

let create section = { section; relocations = [] }

let make_relocation t relocation symbol_table string_table =
  t.relocations
    <- Relocation_entry.create_relocation relocation symbol_table string_table
       :: t.relocations

let num_relocations t = List.length t.relocations

let section_name t = t.section

let write t section_table buf =
  match
    Section_table.get_section_opt section_table
      (X86_proc.Section_name.of_string
         (".rela" ^ X86_proc.Section_name.to_string t.section))
  with
  | Some table ->
    List.iteri
      (fun i relocation ->
        let open Compiler_owee.Owee_buf in
        (* 24 is the size of each relocation entry *)
        let idx = (i * 24) + Int64.to_int table.sh_offset in
        Relocation_entry.write relocation (cursor buf ~at:idx))
      t.relocations
  | None -> ()
