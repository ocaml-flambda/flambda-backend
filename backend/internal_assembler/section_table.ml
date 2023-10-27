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
module Section_name = X86_proc.Section_name

type section_body =
  { offset : int;
    body : bytes
  }

type t =
  { mutable sections : Compiler_owee.Owee_elf.section list;
    mutable num_sections : int;
    section_tb : int Section_name.Tbl.t;
    mutable section_bodies : section_body list;
    mutable current_offset : int64
  }

open Compiler_owee.Owee_elf

let create () =
  { sections =
      [ { sh_name = 0;
          sh_type = 0;
          sh_flags = 0L;
          sh_addr = 0L;
          sh_offset = 0L;
          sh_size = 0L;
          sh_link = 0;
          sh_info = 0;
          sh_addralign = 0L;
          sh_entsize = 0L;
          sh_name_str = ""
        } ];
    num_sections = 1;
    section_tb = Section_name.Tbl.create 100;
    section_bodies = [];
    current_offset = 64L
  }

let add_section t name ?body section =
  t.sections <- section :: t.sections;
  Section_name.Tbl.add t.section_tb name t.num_sections;
  t.num_sections <- t.num_sections + 1;
  t.current_offset <- Int64.add t.current_offset section.sh_size;
  match body with
  | Some body ->
    t.section_bodies
      <- { offset = Int64.to_int section.sh_offset; body } :: t.section_bodies
  | None -> ()

let current_offset t = t.current_offset

let num_sections t = t.num_sections

let get_sec_idx t name = Section_name.Tbl.find t.section_tb name

let get_section t name =
  List.find
    (fun section ->
      String.equal section.sh_name_str (X86_proc.Section_name.to_string name))
    t.sections

let get_section_opt t name =
  List.find_opt
    (fun section ->
      String.equal section.sh_name_str (X86_proc.Section_name.to_string name))
    t.sections

let get_sections t = Array.of_list (List.rev t.sections)

let _get_section_bodies t = t.section_bodies

let write_bodies t buf =
  List.iter
    (fun sec_body ->
      Compiler_owee.Owee_buf.Write.fixed_bytes
        (Compiler_owee.Owee_buf.cursor buf ~at:sec_body.offset)
        (Bytes.length sec_body.body)
        sec_body.body)
    t.section_bodies
