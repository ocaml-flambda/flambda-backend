module StringMap = Map.Make (String)

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
      let current_section_name = name s_l s_opt s_l' in
      let current_instrs = [] in
      aux acc current_section_name (s_l, s_opt, s_l') current_instrs tl
    | (_ as instr) :: tl ->
      aux acc current_section_name current_section (instr :: current_instrs) tl
  in
  match l with
  | [] -> StringMap.empty
  | Section (s_l, s_opt, s_l') :: tl ->
    let current_section_name = name s_l s_opt s_l' in
    let current_instrs = [] in
    aux StringMap.empty current_section_name (s_l, s_opt, s_l') current_instrs
      tl
  | _line :: _ -> failwith "Invalid program, should start with section"

let hexa = "0123456789abcdef"

and hexa1 =
  "0000000000000000111111111111111122222222222222223333333333333333444444444444444455555555555555556666666666666666777777777777777788888888888888889999999999999999aaaaaaaaaaaaaaaabbbbbbbbbbbbbbbbccccccccccccccccddddddddddddddddeeeeeeeeeeeeeeeeffffffffffffffff"

and hexa2 =
  "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"

let of_string_fast s =
  let len = String.length s in
  let buf = Bytes.create (len * 2) in
  for i = 0 to len - 1 do
    Bytes.unsafe_set buf (i * 2)
      (String.unsafe_get hexa1 (Char.code (String.unsafe_get s i)));
    Bytes.unsafe_set buf
      (succ (i * 2))
      (String.unsafe_get hexa2 (Char.code (String.unsafe_get s i)))
  done;
  Bytes.to_string buf

type string_table =
  { mutable current_length : int;
    mutable strings : string list
  }

type sections =
  { mutable sections : Owee.Owee_elf.section list;
    mutable current_offset : int64
  }

let add_string string_table string =
  string_table.current_length
    <- string_table.current_length + String.length string + 1;
  string_table.strings <- string :: string_table.strings

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

let create_section name ~sh_type ~size ~offset ?align ?entsize ?flags shstrtab =
  let align = match align with Some i -> i | None -> 1L in
  let entsize = match entsize with Some i -> i | None -> 0L in
  let flags = match flags with Some i -> i | None -> 0L in
  let section : Owee.Owee_elf.section =
    { sh_name = shstrtab.current_length;
      sh_type;
      sh_flags = flags;
      sh_addr = 0L;
      sh_offset = offset;
      sh_size = size;
      sh_link = 0;
      sh_info = 0;
      sh_addralign = align;
      sh_entsize = entsize;
      sh_name_str = name
    }
  in
  add_string shstrtab name;
  section

let make_section sections name sh_type ~size ?align ?entsize ?flags shstrtab =
  let section =
    create_section name ~sh_type ~size ~offset:sections.current_offset ?align
      ?entsize ?flags shstrtab
  in
  sections.sections <- section :: sections.sections;
  sections.current_offset <- Int64.add sections.current_offset size

let make_text sections raw_section align sh_string_table =
  make_section sections ".text" 1
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~flags:0x6L sh_string_table ~align

let make_data sections raw_section align sh_string_table =
  make_section sections ".data" 1
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~flags:0x3L sh_string_table ~align

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
  let rec entsize = function
    | [] -> 0L
    | [hd] -> ( match Int64.of_string_opt hd with Some i -> i | None -> 0L)
    | hd :: tl -> entsize tl
  in
  let rec flags acc = function
    | Seq.Nil -> acc
    | Seq.Cons (c, tl) ->
      let flag =
        match c with
        | 'a' -> 0x2L
        | 'w' -> 0x1L
        | 'x' -> 0x4L
        | 'M' -> 0x10L
        | 'S' -> 0x20L
        | _ -> failwith "Unknown flag"
      in
      flags (Int64.logor acc flag) (tl ())
  in
  let flags =
    match s_opt with None -> 0L | Some f -> flags 0L (String.to_seq f ())
  in
  let entsize = entsize s_l' in
  make_section sections (String.concat "," s_l)
    ~size:(Int64.of_int (X86_binary_emitter.size raw_section))
    ~entsize sh_string_table ~flags

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
        ( s,
          align,
          X86_binary_emitter.assemble_section X64
            { X86_binary_emitter.sec_name = key;
              sec_instrs = Array.of_list instructions
            } ))
      sections
  in
  Printf.printf "%s\n" output_file;

  let sh_string_table = { current_length = 0; strings = [] } in
  let sections = { sections = []; current_offset = 64L } in

  make_null sections sh_string_table;

  let _, align, raw_section = StringMap.find ".text" binary_sections in
  make_text sections raw_section (Int64.of_int align) sh_string_table;
  let binary_sections = StringMap.remove ".text" binary_sections in

  let _, align, raw_section = StringMap.find ".data" binary_sections in
  make_data sections raw_section (Int64.of_int align) sh_string_table;
  let binary_sections = StringMap.remove ".data" binary_sections in

  List.iter
    (fun (key, (section_info, align, raw_section)) ->
      make_custom_section sections section_info raw_section 0 sh_string_table)
    (StringMap.bindings binary_sections);

  make_section sections ".symtab" 2 ~entsize:24L ~size:0L sh_string_table;
  make_section sections ".strtab" 3 ~size:0L sh_string_table;
  make_shstrtab sections sh_string_table;

  let identification = make_identification in
  let header =
    make_header identification
      (List.length sections.sections)
      sections.current_offset
  in
  let elf =
    Owee.Owee_buf.map_binary_write output_file
      (Int64.to_int sections.current_offset
      + (header.e_shnum * header.e_shentsize))
  in
  Owee.Owee_elf.write_elf elf header
    (Array.of_list (List.rev sections.sections));
  ()
