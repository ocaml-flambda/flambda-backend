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
  { r_offset : Compiler_owee.Owee_buf.u64;
    r_info : Compiler_owee.Owee_buf.u64;
    r_addend : Compiler_owee.Owee_buf.u64
  }

let create_relocation ~offset ~info ~addend =
  { r_offset = offset; r_info = info; r_addend = addend }

let is_label name =
  String.length name >= 2 && Char.equal name.[0] '.' && Char.equal name.[1] 'L'

let get_reloc_info ~relocation_type ~addend name symbol_table string_table =
  (* Labels are turned into relocations that are relative to the section *)
  if is_label name
  then
    let label, idx = Symbol_table.get_label_idx symbol_table name in
    ( relocation_type,
      idx,
      Int64.add addend (Int64.of_int (Option.value ~default:0 label.sy_pos)) )
  else
    let symbol =
      match Symbol_table.get_symbol_idx_opt symbol_table name with
      | Some i -> i
      | None ->
        Symbol_table.make_undef_symbol symbol_table name string_table;
        Symbol_table.get_symbol_idx_opt symbol_table name |> Option.get
    in
    relocation_type, symbol, addend

let create_relocation (relocation : X86_binary_emitter.Relocation.t)
    symbol_table string_table =
  let relocation_type, relocation_symbol, addend =
    match relocation.kind with
    | DIR64 (name, addend) ->
      (get_reloc_info ~relocation_type:1 (* R_X86_64_64 *) ~addend name
         symbol_table)
        string_table
    | DIR32 (_, _) -> failwith "cannot generate dir32"
    | REL32 (name, addend) -> (
      match String.split_on_char '@' name with
      | [name; "GOTPCREL"] ->
        (get_reloc_info ~relocation_type:9 (* R_X86_64_GOTPCREL *)
           ~addend:(Int64.sub addend 4L) name symbol_table)
          string_table
      | [name; "PLT"] | [name] ->
        (get_reloc_info ~relocation_type:4 (* R_X86_64_PLT32 *)
           ~addend:(Int64.sub addend 4L) name symbol_table)
          string_table
      | _ -> failwith (Printf.sprintf "Invalid symbol %s\n" name))
  in
  { r_offset = Int64.of_int relocation.offset_from_section_beginning;
    r_info =
      Int64.logor
        (Int64.shift_left (Int64.of_int relocation_symbol) 32)
        (Int64.of_int relocation_type);
    r_addend = addend
  }

open Compiler_owee.Owee_buf

let write t cursor =
  Write.u64 cursor t.r_offset;
  Write.u64 cursor t.r_info;
  Write.u64 cursor t.r_addend
