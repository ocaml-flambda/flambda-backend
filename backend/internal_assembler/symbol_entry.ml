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

(* CR mshinwell: fix properly using -enable-dev PR's changes *)
[@@@ocaml.warning "-27-32"]

type t =
  { st_name : Compiler_owee.Owee_buf.u32;
    st_info : Compiler_owee.Owee_buf.u8;
    st_other : Compiler_owee.Owee_buf.u8;
    st_shndx : Compiler_owee.Owee_buf.u16;
    st_value : Compiler_owee.Owee_buf.u64;
    st_size : Compiler_owee.Owee_buf.u64;
    st_name_str : string
  }

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

let create_undef_symbol name string_table =
  let symbol_entry =
    { st_name = String_table.current_length string_table;
      st_info = 1 lsl 4;
      st_other = 0;
      st_shndx = 0;
      st_value = 0L;
      st_size = 0L;
      st_name_str = name
    }
  in
  String_table.add_string string_table name;
  symbol_entry

let create_section_symbol st_shndx =
  let symbol_entry =
    { st_name = 0;
      st_info = 3;
      st_other = 0;
      st_shndx;
      st_value = 0L;
      st_size = 0L;
      st_name_str = ""
    }
  in
  symbol_entry

let create_symbol (symbol : X86_binary_emitter.symbol) symbol_table sections
    string_table =
  let value = Option.value ~default:0 symbol.sy_pos in
  let size = Option.value ~default:0 symbol.sy_size in
  let bind =
    match symbol.sy_type with
    (* CR mcollins - modify types to avoid string comparisons *)
    | Some "@function" -> 2
    | Some "object" -> 1
    | Some "tls_object" -> 6
    | Some "common" -> 5
    | Some "notype" -> 0
    | Some s -> failwith ("Unknown symbol type" ^ s)
    | None -> 0
  in
  let global = Bool.to_int symbol.sy_global in
  let symbol_entry =
    { st_name = String_table.current_length string_table;
      st_info = (global lsl 4) lor bind;
      st_other = if symbol.sy_protected then 3 else 0;
      st_shndx =
        Section_table.get_sec_idx sections
          (X86_proc.Section_name.of_string symbol.sy_sec.sec_name);
      (* st_shname_str = symbol.sy_sec.sec_name; *)
      st_value = Int64.of_int value;
      st_size = Int64.of_int size;
      st_name_str = symbol.sy_name
    }
  in
  String_table.add_string string_table symbol.sy_name;
  symbol_entry

let create_got_symbol symbol_table string_table =
  create_undef_symbol "_GLOBAL_OFFSET_TABLE_" string_table

let get_bind t =
  match t.st_info lsr 4 with
  | 0 -> Local
  | 1 -> Global
  | _ -> failwith "Invalid bind"

let get_type t =
  match t.st_info land 0xf with
  | 0 -> NoType
  | 1 -> Object
  | 2 -> Func
  | 3 -> Section
  | 4 -> File
  | 5 -> Common
  | 6 -> Tls
  | _ -> failwith "OS/Arch specific flags not supported"

let get_shndx t = t.st_shndx

let get_name_str t = t.st_name_str

open Compiler_owee.Owee_buf

let write t cursor =
  Write.u32 cursor t.st_name;
  Write.u8 cursor t.st_info;
  Write.u8 cursor t.st_other;
  Write.u16 cursor t.st_shndx;
  Write.u64 cursor t.st_value;
  Write.u64 cursor t.st_size
