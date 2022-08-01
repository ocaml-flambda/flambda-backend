open Int_replace_polymorphic_compare
module StringMap = X86_binary_emitter.StringMap
module StringTbl = X86_binary_emitter.StringTbl

let name s_l s_opt s_l' =
  let first = String.concat "," s_l in
  let mid = Option.fold ~none:"" ~some:(Printf.sprintf ",%S") s_opt in
  let last = match s_l' with [] -> "" | l -> "," ^ String.concat "," l in
  first ^ mid ^ last

let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.equal (String.sub s2 0 (String.length s1)) s1

let from_program l =
  let open X86_ast in
  let sections_tbl = StringTbl.create 100 in
  Profile.record_call ~accumulate:true "iter" (fun () ->
      List.fold_left
        (fun curr_sec x ->
          match x with
          | Section (s_l, s_opt, s_l') -> (
            let name = String.concat "," s_l in
            match StringTbl.find_opt sections_tbl name with
            | Some (_, s) -> s
            | None ->
              let new_sec = ref [] in
              StringTbl.add sections_tbl name ((s_l, s_opt, s_l'), new_sec);
              new_sec)
          | i ->
            curr_sec := i :: !curr_sec;
            curr_sec)
        (ref []) l
      |> ignore);
  Profile.record ~accumulate:true "after"
    (StringTbl.fold
       (fun n (sn, s) acc -> StringMap.add n (sn, List.rev !s) acc)
       sections_tbl)
    StringMap.empty

let parse_flags flags =
  let rec inner acc = function
    | Seq.Nil -> acc
    | Seq.Cons (c, tl) ->
      let flag =
        match c with
        (* CR mcollins - modify types to avoid string comparisons *)
        | 'a' -> 0x2L
        | 'w' -> 0x1L
        | 'x' -> 0x4L
        | 'M' -> 0x10L
        | 'S' -> 0x20L
        | '?' -> 0x0L
        | 'G' -> 0x0L
        | _ -> failwith (Printf.sprintf "Unknown flag %c in flags %s\n" c flags)
      in
      inner (Int64.logor acc flag) (tl ())
  in
  inner 0L (String.to_seq flags ())

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
      sh_name_str = name
    }
  in
  String_table.add_string shstrtab name;
  section

let make_section sections name sh_type ~size ?align ?entsize ?flags ?sh_link
    ?sh_info ?body shstrtab =
  let section : Owee.Owee_elf.section =
    create_section name ~sh_type ~size
      ~offset:(Section_table.current_offset sections)
      ?align ?entsize ?flags ?sh_link ?sh_info shstrtab
  in
  Section_table.add_section sections ?body section

let make_text sections (s_l, s_opt, s_l') raw_section align sh_string_table =
  make_section sections (String.concat "," s_l) 1
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~flags:0x6L sh_string_table ~align
    ~body:(X86_binary_emitter.contents raw_section)

let make_data sections (s_l, s_opt, s_l') raw_section align sh_string_table =
  make_section sections (String.concat "," s_l) 1
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~flags:0x3L sh_string_table ~align
    ~body:(X86_binary_emitter.contents raw_section)

let make_shstrtab sections sh_string_table =
  let name = ".shstrtab" in
  make_section sections name 3
    ~size:
      (Int64.of_int
         (String_table.current_length sh_string_table + 1 + String.length name))
    sh_string_table

let make_custom_section sections (s_l, s_opt, s_l') raw_section sh_string_table
    =
  let rec align = function
    | [] -> 0L
    | [hd] -> Option.value ~default:0L (Int64.of_string_opt hd)
    | hd :: tl -> align tl
  in
  let flags = Option.fold ~none:0L ~some:parse_flags s_opt in
  let align = align s_l' in
  make_section sections (String.concat "," s_l)
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~align ~flags
    ~body:(X86_binary_emitter.contents raw_section)
    sh_string_table

let make_relocation_section sections sym_tbl_idx relocation_table
    sh_string_table =
  let name = Relocation_table.name relocation_table in
  let size =
    Int64.mul 24L
      (Int64.of_int (Relocation_table.num_relocations relocation_table))
  in
  let idx = Section_table.get_sec_idx sections name in
  make_section sections (".rela" ^ name) 4 ~size ~entsize:24L ~flags:0x40L
    ~sh_link:sym_tbl_idx sh_string_table ~align:8L ~sh_info:idx

let get_sections asm =
  let sections =
    Profile.record ~accumulate:true "from_program" from_program asm
  in
  StringMap.mapi
    (fun key (s, instructions) ->
      let align =
        List.fold_left
          (fun acc i ->
            match i with X86_ast.Align (data, n) when n > acc -> n | _ -> acc)
          0 instructions
      in
      (* CR mcollins - the type of s should be changed, this will require
         several changes *)
      ( s,
        align,
        X86_binary_emitter.assemble_section X64
          { X86_binary_emitter.sec_name = key;
            sec_instrs = Array.of_list instructions
          } ))
    sections

let make_compiler_sections section_table compiler_sections symbol_table
    sh_string_table =
  let section_symbols = StringTbl.create 100 in
  StringMap.iter
    (fun name (section_info, align, raw_section) ->
      if isprefix ".text" name
      then
        make_text section_table section_info raw_section (Int64.of_int align)
          sh_string_table
      else if isprefix ".data" name
      then
        make_data section_table section_info raw_section (Int64.of_int align)
          sh_string_table
      else
        make_custom_section section_table section_info raw_section 1
          sh_string_table;
      StringTbl.add section_symbols name
        (Symbol_table.make_section_symbol symbol_table
           (Section_table.num_sections section_table - 1)
           section_table))
    compiler_sections;
  section_symbols

let make_symbols section_tables compiler_sections symbol_table section_symbols
    string_table =
  StringMap.iter
    (fun key (section_info, align, raw_section) ->
      let symbols = X86_binary_emitter.labels raw_section in
      StringTbl.iter
        (fun name symbol ->
          match is_label name with
          | true ->
            Symbol_table.add_label symbol_table symbol
              (StringTbl.find section_symbols key)
          | false ->
            Symbol_table.make_symbol symbol_table symbol section_tables
              string_table)
        symbols)
    compiler_sections

let create_relocation_tables binary_sections symbol_table string_table =
  (List.filter_map (fun (key, ((s_l, _, _), align, raw_section)) ->
       match X86_binary_emitter.relocations raw_section with
       | [] -> None
       | l ->
         let relocation_table =
           Relocation_table.create (String.concat "," s_l)
         in
         List.iter
           (fun relocation ->
             Relocation_table.make_relocation relocation_table relocation
               symbol_table string_table)
           l;
         Some relocation_table))
    (StringMap.bindings binary_sections)

let write buf header section_table symbol_table relocation_tables string_table =
  Owee.Owee_elf.write_elf buf header (Section_table.get_sections section_table);
  Section_table.write_bodies section_table buf;
  let symtab = Section_table.get_section section_table ".symtab" in
  let strtab = Section_table.get_section section_table ".strtab" in
  Symbol_table.write symbol_table symtab.sh_offset buf;
  List.iter
    (fun t -> Relocation_table.write t section_table buf)
    relocation_tables;
  String_table.write string_table strtab.sh_offset buf

let assemble asm output_file =
  let binary_sections =
    Profile.record ~accumulate:true "X86_binary_emitter" get_sections asm
  in

  let string_table = String_table.create () in
  let sh_string_table = String_table.create () in
  let sections = Section_table.create () in
  let symbol_table = Symbol_table.create () in

  let section_symbols =
    Profile.record ~accumulate:true "make_compiler_sections"
      (make_compiler_sections sections binary_sections symbol_table)
      sh_string_table
  in

  Profile.record ~accumulate:true "make_symbols"
    (make_symbols sections binary_sections symbol_table section_symbols)
    string_table;
  Symbol_table.make_undef_symbol symbol_table "_GLOBAL_OFFSET_TABLE_"
    string_table;

  let relocation_tables =
    create_relocation_tables binary_sections symbol_table string_table
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
  make_section sections ".symtab" 2 ~entsize:24L
    ~size:(Int64.of_int (24 * Symbol_table.num_symbols symbol_table))
    ~align:8L ~sh_link:strtabidx ~sh_info:num_locals sh_string_table;
  make_section sections ".strtab" 3
    ~size:(Int64.of_int (String_table.current_length string_table))
    sh_string_table;
  make_shstrtab sections sh_string_table;

  let identification = make_identification in
  let header =
    make_header identification
      (Section_table.num_sections sections)
      (Section_table.current_offset sections)
  in
  Printf.printf "File: %s\n" output_file;
  Printf.printf "Total size: %d\n"
    (Int64.to_int (Section_table.current_offset sections)
    + (header.e_shnum * header.e_shentsize));
  let elf =
    Owee.Owee_buf.map_binary_write output_file
      (Int64.to_int (Section_table.current_offset sections)
      + (header.e_shnum * header.e_shentsize))
  in
  write elf header sections symbol_table relocation_tables string_table
