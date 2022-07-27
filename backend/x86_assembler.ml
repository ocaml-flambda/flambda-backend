[@@@warning "-26"]

module StringMap = X86_binary_emitter.StringMap

let name s_l s_opt s_l' =
  let first = String.concat "," s_l in
  let mid = match s_opt with None -> "" | Some s -> Printf.sprintf ",%S" s in
  let last = match s_l' with [] -> "" | l -> "," ^ String.concat "," l in
  first ^ mid ^ last

let append key (s, l) t =
  StringMap.update key
    (function None -> Some (s, l) | Some (s, l') -> Some (s, l' @ l))
    t

let from_program l =
  let open X86_ast in
  let rec aux acc current_section_name current_section current_instrs l =
    let add_current () =
      append current_section_name (current_section, List.rev current_instrs) acc
    in
    match l with
    | [] -> add_current ()
    | Section (s_l, s_opt, s_l') :: tl ->
      let acc = add_current () in
      let current_section_name =
        String.concat "," s_l
        (*name s_l s_opt s_l'*)
      in
      let current_instrs = [] in
      aux acc current_section_name (s_l, s_opt, s_l') current_instrs tl
    | (_ as instr) :: tl ->
      aux acc current_section_name current_section (instr :: current_instrs) tl
  in
  match l with
  | [] -> StringMap.empty
  | Section (s_l, s_opt, s_l') :: tl ->
    let current_section_name = String.concat "," s_l (*name s_l s_opt s_l'*) in
    let current_instrs = [] in
    aux StringMap.empty current_section_name (s_l, s_opt, s_l') current_instrs
      tl
  | _line :: _ -> failwith "Invalid program, should start with section"

let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.sub s2 0 (String.length s1) = s1

type string_table =
  { mutable current_length : int;
    mutable strings : string list
  }

type sections =
  { mutable sections : Owee.Owee_elf.section list;
    mutable section_bodies : (int * string) list;
    mutable current_offset : int64
  }

type symbol_entry =
  { st_name : Owee.Owee_buf.u32;
    st_info : Owee.Owee_buf.u8;
    st_other : Owee.Owee_buf.u8;
    st_value : Owee.Owee_buf.u64;
    st_size : Owee.Owee_buf.u64;
    st_name_str : string;
    st_shname_str : string
  }

type symbol_table =
  { mutable symbols : symbol_entry list;
    mutable virtual_symbols :
      (X86_binary_emitter.symbol * symbol_entry) StringMap.t
  }

type relocation_entry =
  { r_offset : Owee.Owee_buf.u64;
    r_info : Owee.Owee_buf.u64;
    r_addend : Owee.Owee_buf.u64
  }

let parse_flags flags =
  let rec inner acc = function
    | Seq.Nil -> acc
    | Seq.Cons (c, tl) ->
      let flag =
        match c with
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

let add_string string_table string =
  string_table.current_length
    <- string_table.current_length + String.length string + 1;
  string_table.strings <- string :: string_table.strings

let create_symbol (symbol : X86_binary_emitter.symbol) symbol_table string_table
    =
  let value =
    match symbol.sy_pos with None -> 0L | Some n -> Int64.of_int n
  in
  let st_info =
    (* CR mcollins - modify types to avoid string comparisons *)
    (match symbol.sy_type with
    | Some "@function" -> 2
    | Some "object" -> 1
    | Some "tls_object" -> 6
    | Some "common" -> 5
    | Some "notype" -> 0
    | Some s -> failwith ("Unknown symbol type" ^ s)
    | None -> 0)
    lor if symbol.sy_global then 1 lsl 4 else 0
  in
  let symbol_entry =
    { st_name = string_table.current_length;
      st_info;
      st_other = 0;
      st_shname_str = symbol.sy_sec.sec_name;
      st_value = value;
      st_size =
        (match symbol.sy_size with None -> 0L | Some n -> Int64.of_int n);
      st_name_str = symbol.sy_name
    }
  in
  add_string string_table symbol.sy_name;
  symbol_table.symbols <- symbol_entry :: symbol_table.symbols

let create_got_symbol string_table =
  let symbol_entry =
    { st_name = string_table.current_length;
      st_info = 1 lsl 4;
      st_other = 0;
      st_value = 0L;
      st_size = 0L;
      st_name_str = "_GLOBAL_OFFSET_TABLE_";
      st_shname_str = ""
    }
  in
  add_string string_table "_GLOBAL_OFFSET_TABLE_";
  symbol_entry

let create_undef_symbol name string_table =
  let symbol_entry =
    { st_name = string_table.current_length;
      st_info = 1 lsl 4;
      st_other = 0;
      st_value = 0L;
      st_size = 0L;
      st_name_str = name;
      st_shname_str = ""
    }
  in
  add_string string_table name;
  symbol_entry

let get_or_create_symbol_idx name symbol_table string_table =
  match
    List.find_map
      (fun (i, x) -> if x.st_name_str = name then Some i else None)
      (List.mapi (fun i x -> i, x) symbol_table.symbols)
  with
  | Some i -> i
  | None ->
    symbol_table.symbols
      <- symbol_table.symbols @ [create_undef_symbol name string_table];
    List.length symbol_table.symbols - 1

let create_relocation (relocation : X86_binary_emitter.Relocation.t)
    symbol_table string_table =
  let relocation_type, relocation_symbol, addend =
    match relocation.kind with
    | DIR64 (name, addend) ->
      1, get_or_create_symbol_idx name symbol_table string_table, addend
    | DIR32 (_, _) -> failwith "cannot generate dir32"
    | REL32 (name, addend) -> (
      match String.split_on_char '@' name with
      | [name; "GOTPCREL"] ->
        ( 9,
          get_or_create_symbol_idx name symbol_table string_table,
          Int64.sub addend 4L )
      | [name; "PLT"] | [name] ->
        ( 4,
          get_or_create_symbol_idx name symbol_table string_table,
          Int64.sub addend 4L )
      | _ -> failwith (Printf.sprintf "Invalid symbol %s\n" name))
  in
  let reloc =
    { r_offset = Int64.of_int relocation.offset_from_section_beginning;
      r_info =
        Int64.logor
          (Int64.shift_left (Int64.of_int relocation_symbol) 32)
          (Int64.of_int relocation_type);
      r_addend = addend
    }
  in
  reloc

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
  let sh_addralign = match align with Some i -> i | None -> 1L in
  let sh_entsize = match entsize with Some i -> i | None -> 0L in
  let sh_flags = match flags with Some i -> i | None -> 0L in
  let sh_link = match sh_link with Some i -> i | None -> 0 in
  let sh_info = match sh_info with Some i -> i | None -> 0 in
  let section : Owee.Owee_elf.section =
    { sh_name = shstrtab.current_length;
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
  add_string shstrtab name;
  section

let make_section sections name sh_type ~size ?align ?entsize ?flags ?sh_link
    ?sh_info ?body shstrtab =
  let section : Owee.Owee_elf.section =
    create_section name ~sh_type ~size ~offset:sections.current_offset ?align
      ?entsize ?flags ?sh_link ?sh_info shstrtab
  in
  sections.sections <- section :: sections.sections;
  sections.current_offset <- Int64.add sections.current_offset size;
  match body with
  | Some body ->
    sections.section_bodies
      <- (Int64.to_int section.sh_offset, body) :: sections.section_bodies
  | _ -> ()

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

let make_null sections sh_string_table =
  let section =
    create_section "" ~sh_type:0 ~size:0L ~offset:0L ~align:0L sh_string_table
  in
  sections.sections <- sections.sections @ [section]

let make_shstrtab sections sh_string_table =
  let name = ".shstrtab" in
  make_section sections name 3
    ~size:
      (Int64.of_int (sh_string_table.current_length + 1 + String.length name))
    sh_string_table

let make_custom_section sections (s_l, s_opt, s_l') raw_section sh_string_table
    =
  let rec align = function
    | [] -> 0L
    | [hd] -> ( match Int64.of_string_opt hd with Some i -> i | None -> 0L)
    | hd :: tl -> align tl
  in
  let flags = match s_opt with None -> 0L | Some f -> parse_flags f in
  let align = align s_l' in
  make_section sections (String.concat "," s_l)
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~align ~flags
    ~body:(X86_binary_emitter.contents raw_section)
    sh_string_table

let make_relocation_section sections sym_tbl_idx name size sh_string_table =
  let open Owee.Owee_elf in
  let num_sections = List.length sections.sections in
  let i, section =
    List.find
      (fun (_, s) -> s.sh_name_str = name)
      (List.mapi (fun i x -> i, x) sections.sections)
  in
  let idx = num_sections - i - 1 in
  let section =
    make_section sections (".rela" ^ name) 4 ~size ~entsize:24L ~flags:0x40L
      ~sh_link:sym_tbl_idx sh_string_table ~align:8L ~sh_info:idx
  in
  section

(* let rec insert l i e = *)
(*   match i, l with *)
(*   | 0, l -> e :: l *)
(*   | i, hd :: tl -> *)
(*     { hd with *)
(*       sh_info = (if hd.sh_type = 4 then hd.sh_info + 1 else hd.sh_info); *)
(*       sh_offset = Int64.add hd.sh_offset size *)
(*     } *)
(*     :: insert tl (i - 1) e *)
(*   | _ -> failwith "error list too short" *)
(* in *)

(* sections.sections *)
(*   <- List.map *)
(*        (function *)
(* | { sh_type = 4 } as x -> { x with sh_link = sym_tbl_idx } | x -> x) *)
(*        sections.sections; *)
(* sections.sections <- insert sections.sections i section; *)
(* sections.current_offset <- Int64.add sections.current_offset size *)

let assemble asm output_file =
  let sections = from_program asm in
  let binary_sections =
    StringMap.mapi
      (fun key (s, instructions) ->
        let align =
          List.fold_left
            (fun acc i ->
              match i with
              | X86_ast.Align (data, n) when n > acc -> n
              | _ -> acc)
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
  in

  let string_table = { current_length = 0; strings = [] } in
  let sh_string_table = { current_length = 0; strings = [] } in
  let sections = { sections = []; section_bodies = []; current_offset = 64L } in

  (* An index of 0 in the string table is reserved for the null string *)
  add_string string_table "";

  make_null sections sh_string_table;
  let text_sections, other_sections =
    StringMap.partition (fun name _ -> isprefix ".text" name) binary_sections
  in
  StringMap.iter
    (fun _ (section_info, align, raw_section) ->
      make_text sections section_info raw_section (Int64.of_int align)
        sh_string_table)
    text_sections;

  let data_sections, other_sections =
    StringMap.partition (fun name _ -> isprefix ".data" name) other_sections
  in
  StringMap.iter
    (fun _ (section_info, align, raw_section) ->
      make_data sections section_info raw_section (Int64.of_int align)
        sh_string_table)
    data_sections;

  StringMap.iter
    (fun key (section_info, align, raw_section) ->
      make_custom_section sections section_info raw_section 1 sh_string_table)
    other_sections;

  let symbol_table = { symbols = []; virtual_symbols = StringMap.empty } in
  StringMap.iter
    (fun key (section_info, align, raw_section) ->
      StringMap.iter
        (fun name symbol -> create_symbol symbol symbol_table string_table)
        (X86_binary_emitter.labels raw_section))
    binary_sections;
  symbol_table.symbols <- create_got_symbol string_table :: symbol_table.symbols;
  symbol_table.symbols
    <- List.stable_sort
         (fun a b -> (a.st_info lsl 4) - (b.st_info lsl 4))
         symbol_table.symbols;
  let raw_relocations =
    List.filter_map
      (fun (key, ((s_l, _, _), align, raw_section)) ->
        match
          List.map
            (fun relocation ->
              create_relocation relocation symbol_table string_table)
            (X86_binary_emitter.relocations raw_section)
        with
        | [] -> None
        | l -> Some (String.concat "," s_l, l))
      (StringMap.bindings binary_sections)
  in
  let symtab_idx =
    List.length sections.sections + List.length raw_relocations
  in
  List.iter
    (fun (name, relocations) ->
      make_relocation_section sections symtab_idx name
        (Int64.of_int (24 * List.length relocations))
        sh_string_table)
    raw_relocations;
  let num_locals =
    List.length
      (List.filter (fun x -> x.st_info lsr 4 = 0) symbol_table.symbols)
  in

  let strtabidx = 1 + List.length sections.sections in
  make_section sections ".symtab" 2 ~entsize:24L
    ~size:(Int64.of_int (24 * List.length symbol_table.symbols))
    ~align:8L ~sh_link:strtabidx ~sh_info:num_locals sh_string_table;
  make_section sections ".strtab" 3
    ~size:(Int64.of_int string_table.current_length)
    sh_string_table;
  make_shstrtab sections sh_string_table;

  let identification = make_identification in
  let header =
    make_header identification
      (List.length sections.sections)
      sections.current_offset
  in
  Printf.printf "File: %s\n" output_file;
  Printf.printf "Total size: %d\n"
    (Int64.to_int sections.current_offset
    + (header.e_shnum * header.e_shentsize));
  let elf =
    Owee.Owee_buf.map_binary_write output_file
      (Int64.to_int sections.current_offset
      + (header.e_shnum * header.e_shentsize))
  in
  Owee.Owee_elf.write_elf elf header
    (Array.of_list (List.rev sections.sections));
  List.iter
    (fun (pos, body) ->
      Owee.Owee_buf.Write.fixed_string
        (Owee.Owee_buf.cursor elf ~at:pos)
        (String.length body) body)
    sections.section_bodies;
  let symtab =
    List.find
      (fun (section : Owee.Owee_elf.section) -> section.sh_name_str = ".symtab")
      sections.sections
  in
  let strtab =
    List.find
      (fun (section : Owee.Owee_elf.section) -> section.sh_name_str = ".strtab")
      sections.sections
  in
  List.iteri
    (fun i symbol ->
      let open Owee.Owee_buf in
      let t = cursor elf ~at:((i * 24) + Int64.to_int symtab.sh_offset) in
      let st_shndx =
        match
          List.find_map
            (fun ((i, s) : int * Owee.Owee_elf.section) ->
              if s.sh_name_str = symbol.st_shname_str then Some i else None)
            (List.mapi (fun i x -> i, x) (List.rev sections.sections))
        with
        | Some n -> n
        | None ->
          if symbol.st_shname_str = ""
          then 0
          else
            failwith
              (Printf.sprintf "Can't find section for symbol %s"
                 symbol.st_shname_str)
      in
      Write.u32 t symbol.st_name;
      Write.u8 t symbol.st_info;
      Write.u8 t symbol.st_other;
      Write.u16 t st_shndx;
      Write.u64 t symbol.st_value;
      Write.u64 t symbol.st_size)
    symbol_table.symbols;
  List.iter
    (fun (name, relocations) ->
      match
        List.find_opt
          (fun (section : Owee.Owee_elf.section) ->
            section.sh_name_str = ".rela" ^ name)
          sections.sections
      with
      | Some table ->
        List.iteri
          (fun i relocation ->
            let open Owee.Owee_buf in
            let t = cursor elf ~at:((i * 24) + Int64.to_int table.sh_offset) in
            Write.u64 t relocation.r_offset;
            Write.u64 t relocation.r_info;
            Write.u64 t relocation.r_addend)
          relocations
      | None -> ())
    raw_relocations;
  let t = Owee.Owee_buf.cursor elf ~at:(Int64.to_int strtab.sh_offset) in
  List.iter
    (fun string ->
      Owee.Owee_buf.Write.zero_string t string ~maxlen:(String.length string))
    (List.rev string_table.strings);
  ()
