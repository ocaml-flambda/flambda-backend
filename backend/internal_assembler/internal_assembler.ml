module SectionName = X86_proc.SectionName
module StringMap = X86_binary_emitter.StringMap
module StringTbl = X86_binary_emitter.StringTbl
module SectionMap = Map.Make (SectionName)
module SectionTbl = Hashtbl.Make (SectionName)

let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.equal (String.sub s2 0 (String.length s1)) s1

let is_label name =
  String.length name >= 2 && Char.equal name.[0] '.' && Char.equal name.[1] 'L'

let make_identification : Owee.Owee_elf.identification =
  { elf_class = 2;
    elf_data = 1;
    elf_version = 1;
    elf_osabi = 0;
    elf_abiversion = 0
  }

let make_header identification shnum e_shoff : Owee.Owee_elf.header =
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
  let sh_name_str = X86_proc.SectionName.name name in
  let section : Owee.Owee_elf.section =
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
  let section : Owee.Owee_elf.section =
    create_section name ~sh_type ~size
      ~offset:(Section_table.current_offset sections)
      ?align ?entsize ?flags ?sh_link ?sh_info shstrtab
  in
  Section_table.add_section sections name ?body section

let make_text sections name raw_section align sh_string_table =
  make_section sections name ~sh_type:1
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~flags:0x6L sh_string_table ~align
    ~body:(X86_binary_emitter.contents_mut raw_section)

let make_data sections name raw_section align sh_string_table =
  make_section sections name ~sh_type:1
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~flags:0x3L sh_string_table ~align
    ~body:(X86_binary_emitter.contents_mut raw_section)

let make_shstrtab sections sh_string_table =
  let name = ".shstrtab" in
  make_section sections
    (SectionName.from_name name)
    ~sh_type:3
    ~size:
      (Int64.of_int
         (String_table.current_length sh_string_table + 1 + String.length name))
    sh_string_table

let make_custom_section sections name raw_section sh_string_table =
  let flags = X86_proc.SectionName.flags name in
  let align = X86_proc.SectionName.alignment name in
  make_section sections name
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~align ~flags
    ~body:(X86_binary_emitter.contents_mut raw_section)
    sh_string_table

let make_relocation_section sections sym_tbl_idx relocation_table
    sh_string_table =
  let name = Relocation_table.section_name relocation_table in
  let size =
    Int64.mul 24L
      (Int64.of_int (Relocation_table.num_relocations relocation_table))
  in
  let idx = Section_table.get_sec_idx sections name in
  make_section sections
    (SectionName.from_name (".rela" ^ SectionName.name name))
    ~sh_type:4 ~size ~entsize:24L ~flags:0x40L ~sh_link:sym_tbl_idx
    sh_string_table ~align:8L ~sh_info:idx

let get_sections sections =
  List.fold_left
    (fun acc (name, instructions) ->
      let align =
        List.fold_left
          (fun acc i ->
            match i with X86_ast.Align (data, n) when n > acc -> n | _ -> acc)
          0 instructions
      in
      SectionMap.add name
        ( align,
          X86_binary_emitter.assemble_section X64
            { X86_binary_emitter.sec_name = X86_proc.SectionName.name name;
              sec_instrs = Array.of_list instructions
            } )
        acc)
    SectionMap.empty sections

let make_compiler_sections section_table compiler_sections symbol_table
    sh_string_table =
  let section_symbols = SectionTbl.create 100 in
  SectionMap.iter
    (fun name (align, raw_section) ->
      if isprefix ".text" (X86_proc.SectionName.name name)
      then
        make_text section_table name raw_section (Int64.of_int align)
          sh_string_table
      else if isprefix ".data" (X86_proc.SectionName.name name)
      then
        make_data section_table name raw_section (Int64.of_int align)
          sh_string_table
      else
        make_custom_section section_table name raw_section ~sh_type:1
          sh_string_table;
      SectionTbl.add section_symbols name
        (Symbol_table.make_section_symbol symbol_table
           (Section_table.num_sections section_table - 1)
           section_table))
    compiler_sections;
  section_symbols

let make_symbols section_tables compiler_sections symbol_table section_symbols
    string_table =
  SectionMap.iter
    (fun section (align, raw_section) ->
      let symbols = X86_binary_emitter.labels raw_section in
      StringTbl.iter
        (fun name symbol ->
          match is_label name with
          | true ->
            Symbol_table.add_label symbol_table symbol
              (SectionTbl.find section_symbols section)
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
    (SectionMap.bindings compiler_sections)

let write buf header section_table symbol_table relocation_tables string_table =
  Owee.Owee_elf.write_elf buf header (Section_table.get_sections section_table);
  Section_table.write_bodies section_table buf;
  let symtab =
    Section_table.get_section section_table
      (X86_proc.SectionName.make [".symtab"] None [])
  in
  let strtab =
    Section_table.get_section section_table
      (X86_proc.SectionName.make [".strtab"] None [])
  in
  Symbol_table.write symbol_table symtab.sh_offset buf;
  List.iter
    (fun t -> Relocation_table.write t section_table buf)
    relocation_tables;
  String_table.write string_table strtab.sh_offset buf

let assemble asm output_file =
  let compiler_sections = get_sections asm in
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
  let symtab_idx =
    Section_table.num_sections sections + List.length relocation_tables
  in
  List.iter
    (fun relocation_table ->
      make_relocation_section sections symtab_idx relocation_table
        sh_string_table)
    relocation_tables;
  let num_locals = Symbol_table.num_locals symbol_table in
  let strtabidx = 1 + Section_table.num_sections sections in
  make_section sections
    (SectionName.from_name ".symtab")
    ~sh_type:2 ~entsize:24L
    ~size:(Int64.of_int (24 * Symbol_table.num_symbols symbol_table))
    ~align:8L ~sh_link:strtabidx ~sh_info:num_locals sh_string_table;
  make_section sections
    (SectionName.from_name ".strtab")
    ~sh_type:3
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
    Owee.Owee_buf.map_binary_write output_file
      (Int64.to_int (Section_table.current_offset sections)
      + (header.e_shnum * header.e_shentsize))
  in
  write elf header sections symbol_table relocation_tables string_table
