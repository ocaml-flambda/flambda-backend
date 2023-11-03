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

module String = Misc.Stdlib.String
module Section_name = X86_proc.Section_name
module StringMap = X86_binary_emitter.StringMap

let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.equal (String.sub s2 0 (String.length s1)) s1

let is_label name =
  String.length name >= 2 && Char.equal name.[0] '.' && Char.equal name.[1] 'L'

let make_identification : Compiler_owee.Owee_elf.identification =
  { elf_class = 2;
    elf_data = 1;
    elf_version = 1;
    elf_osabi = 0;
    elf_abiversion = 0
  }

let make_header identification shnum e_shoff : Compiler_owee.Owee_elf.header =
  { e_ident = identification;
    e_type = 1;
    e_machine = 0x3E;
    e_version = 1;
    e_entry = 0L;
    e_phoff = 0L;
    e_shoff;
    e_flags = 0;
    e_ehsize = 64;
    e_phentsize = 0;
    e_phnum = 0;
    e_shentsize = 64;
    e_shnum = shnum;
    e_shstrndx = shnum - 1
  }

let create_section name ~sh_type ~size ~offset ?align ?entsize ?flags ?sh_link
    ?sh_info shstrtab =
  let sh_addralign = Option.value ~default:1L align in
  let sh_entsize = Option.value ~default:0L entsize in
  let sh_flags = Option.value ~default:0L flags in
  let sh_link = Option.value ~default:0 sh_link in
  let sh_info = Option.value ~default:0 sh_info in
  let sh_name_str = X86_proc.Section_name.to_string name in
  let section : Compiler_owee.Owee_elf.section =
    { sh_name = String_table.current_length shstrtab;
      sh_type;
      sh_flags;
      sh_addr = 0L;
      sh_offset = offset;
      sh_size = size;
      sh_link;
      sh_info;
      sh_addralign;
      sh_entsize;
      sh_name_str
    }
  in
  String_table.add_string shstrtab sh_name_str;
  section

let make_section sections name ~sh_type ~size ?align ?entsize ?flags ?sh_link
    ?sh_info ?body shstrtab =
  let section : Compiler_owee.Owee_elf.section =
    create_section name ~sh_type ~size
      ~offset:(Section_table.current_offset sections)
      ?align ?entsize ?flags ?sh_link ?sh_info shstrtab
  in
  Section_table.add_section sections name ?body section

let make_text sections name raw_section ~align sh_string_table =
  make_section sections name ~sh_type:1
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~flags:0x6L sh_string_table ~align
    ~body:(X86_binary_emitter.contents_mut raw_section)

let make_data sections name raw_section ~align sh_string_table =
  make_section sections name ~sh_type:1
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~flags:0x3L sh_string_table ~align
    ~body:(X86_binary_emitter.contents_mut raw_section)

let make_shstrtab sections sh_string_table =
  let name = ".shstrtab" in
  make_section sections
    (Section_name.of_string name)
    ~sh_type:3
    ~size:
      (Int64.of_int
         (String_table.current_length sh_string_table + 1 + String.length name))
    sh_string_table

let parse_flags flags =
  let flags = Option.value ~default:"" flags in
  let rec inner acc = function
    | Seq.Nil -> acc
    | Seq.Cons (c, tl) ->
      let flag =
        match c with
        (* CR mcollins - modify types to avoid string comparisons *)
        (* https://sourceware.org/binutils/docs/as/Section.html *)
        | 'a' -> 0x2L
        | 'w' -> 0x1L
        | 'x' -> 0x4L
        | 'M' -> 0x10L
        | 'S' -> 0x20L
        (* CR mcollins - do we need to worry about group flags? *)
        | '?' -> 0x0L
        | 'G' -> 0x0L
        | _ -> failwith (Printf.sprintf "Unknown flag %c in flags %s\n" c flags)
      in
      inner (Int64.logor acc flag) (tl ())
  in
  inner 0L (String.to_seq flags ())

let make_custom_section sections name raw_section sh_string_table =
  let flags = parse_flags (X86_proc.Section_name.flags name) in
  let align = X86_proc.Section_name.alignment name in
  make_section sections name
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~align ~flags
    ~body:(X86_binary_emitter.contents_mut raw_section)
    sh_string_table

let make_relocation_section sections ~sym_tbl_idx relocation_table
    sh_string_table =
  let name = Relocation_table.section_name relocation_table in
  let size =
    Int64.mul 24L (* relocation entry size *)
      (Int64.of_int (Relocation_table.num_relocations relocation_table))
  in
  let idx = Section_table.get_sec_idx sections name in
  make_section sections
    (Section_name.of_string (".rela" ^ Section_name.to_string name))
    ~sh_type:4 (* SHT_RELA *) ~size ~entsize:24L
    ~flags:0x40L (* SHF_INFO_LINK *) ~sh_link:sym_tbl_idx sh_string_table
    ~align:8L ~sh_info:idx

let assemble_one_section ~name instructions =
  let align =
    List.fold_left
      (fun acc i ->
        match i with X86_ast.Align (data, n) when n > acc -> n | _ -> acc)
      0 instructions
  in
  align,
  X86_binary_emitter.assemble_section X64
    { X86_binary_emitter.sec_name = X86_proc.Section_name.to_string name;
      sec_instrs = Array.of_list instructions
    }

let get_sections ~delayed sections =
  let get acc sections =
    List.fold_left (fun acc (name, instructions) ->
      Section_name.Map.add name (assemble_one_section ~name instructions) acc)
      acc sections
  in
  (* DWARF sections must be emitted after .text and .data because they
     contain information that is produced when .text and .data are emitted.
     For example, DWARF sections need to know the offset of some instructions
     from the start of the .text section.
     Additionally, DWARF sections may add relocations to the object file's
     relocation table. *)
  let acc = Section_name.Map.empty in
  let acc = get acc sections in
  Emitaux.Dwarf_helpers.emit_delayed_dwarf ();
  get acc (delayed ())

let make_compiler_sections section_table compiler_sections symbol_table
    sh_string_table =
  let section_symbols = Section_name.Tbl.create 100 in
  Section_name.Map.iter
    (fun name (align, raw_section) ->
      if Section_name.is_text_like name
      then
        make_text section_table name raw_section ~align:(Int64.of_int align)
          sh_string_table
      else if Section_name.is_data_like name
      then
        make_data section_table name raw_section ~align:(Int64.of_int align)
          sh_string_table
      else if Section_name.is_note_like name
      then
        make_custom_section section_table name raw_section ~sh_type:7
          (* SHT_NOTE *) sh_string_table
      else
        make_custom_section section_table name raw_section ~sh_type:1
          (* SHT_PROGBITS *) sh_string_table;
      Section_name.Tbl.add section_symbols name
        (Symbol_table.make_section_symbol symbol_table
           (Section_table.num_sections section_table - 1)
           section_table))
    compiler_sections;
  section_symbols

let make_symbols section_tables compiler_sections symbol_table section_symbols
    string_table =
  Section_name.Map.iter
    (fun section (align, raw_section) ->
      let symbols = X86_binary_emitter.labels raw_section in
      String.Tbl.iter
        (fun name symbol ->
          match is_label name with
          | true ->
            Symbol_table.add_label symbol_table symbol
              (Section_name.Tbl.find section_symbols section)
          | false ->
            Symbol_table.make_symbol symbol_table symbol section_tables
              string_table)
        symbols)
    compiler_sections

let create_relocation_tables compiler_sections symbol_table string_table =
  (List.filter_map (fun (section, (align, raw_section)) ->
       match X86_binary_emitter.relocations raw_section with
       | [] -> None
       | l ->
         let relocation_table = Relocation_table.create section in
         List.iter
           (fun relocation ->
             Relocation_table.make_relocation relocation_table relocation
               symbol_table string_table)
           l;
         Some relocation_table))
    (Section_name.Map.bindings compiler_sections)

let write buf header section_table symbol_table relocation_tables string_table =
  Compiler_owee.Owee_elf.write_elf buf header (Section_table.get_sections section_table);
  Section_table.write_bodies section_table buf;
  let symtab =
    Section_table.get_section section_table
      (X86_proc.Section_name.make [".symtab"] None [])
  in
  let strtab =
    Section_table.get_section section_table
      (X86_proc.Section_name.make [".strtab"] None [])
  in
  Symbol_table.write symbol_table symtab.sh_offset buf;
  List.iter
    (fun t -> Relocation_table.write t section_table buf)
    relocation_tables;
  String_table.write string_table strtab.sh_offset buf

let assemble unix ~delayed asm output_file =
  let compiler_sections = get_sections ~delayed asm in
  let string_table = String_table.create () in
  let sh_string_table = String_table.create () in
  let sections = Section_table.create () in
  let symbol_table = Symbol_table.create () in
  let section_symbols =
    make_compiler_sections sections compiler_sections symbol_table
      sh_string_table
  in
  make_symbols sections compiler_sections symbol_table section_symbols
    string_table;
  Symbol_table.make_undef_symbol symbol_table "_GLOBAL_OFFSET_TABLE_"
    string_table;
  let relocation_tables =
    create_relocation_tables compiler_sections symbol_table string_table
  in
  let sym_tbl_idx =
    Section_table.num_sections sections + List.length relocation_tables
  in
  List.iter
    (fun relocation_table ->
      make_relocation_section sections ~sym_tbl_idx relocation_table
        sh_string_table)
    relocation_tables;
  let num_locals = Symbol_table.num_locals symbol_table in
  let strtabidx = 1 + Section_table.num_sections sections in
  make_section sections
    (Section_name.of_string ".symtab")
    ~sh_type:2 (* SHT_PROGBITS *) ~entsize:24L (* symbol entry size *)
    ~size:(Int64.of_int (24 * Symbol_table.num_symbols symbol_table))
    ~align:8L ~sh_link:strtabidx ~sh_info:num_locals sh_string_table;
  make_section sections
    (Section_name.of_string ".strtab")
    ~sh_type:3 (* SHT_STRTAB *)
    ~size:(Int64.of_int (String_table.current_length string_table))
    sh_string_table;
  make_shstrtab sections sh_string_table;
  let identification = make_identification in
  let header =
    make_header identification
      (Section_table.num_sections sections)
      (Section_table.current_offset sections)
  in
  let elf =
    Compiler_owee.Owee_buf.map_binary_write unix output_file
      (Int64.to_int (Section_table.current_offset sections)
      + (header.e_shnum * header.e_shentsize))
  in
  write elf header sections symbol_table relocation_tables string_table
