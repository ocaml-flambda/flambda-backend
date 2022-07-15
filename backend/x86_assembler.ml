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

let make_header identification shnum : Owee.Owee_elf.header =
  { e_ident = identification;
    e_type = 1;
    e_machine = 0x3E;
    e_version = 1;
    e_entry = 0L;
    e_phoff = 0L;
    e_shoff = 1136L (* TODO: calculate *);
    e_flags = 0;
    e_ehsize = 64;
    e_phentsize = 0;
    e_phnum = 0;
    e_shentsize = 64;
    e_shnum = shnum;
    e_shstrndx = shnum - 1
  }

let make_section name sh_type ?size ?align ?entsize ?flags shstrtab =
  let size = match size with Some i -> i | None -> 0L in
  let align = match align with Some i -> i | None -> 1L in
  let entsize = match entsize with Some i -> i | None -> 0L in
  let flags = match flags with Some i -> i | None -> 0L in
  let section : Owee.Owee_elf.section =
    { sh_name = shstrtab.current_length;
      sh_type;
      sh_flags = flags;
      sh_addr = 0L;
      sh_offset = 0x410L;
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

let make_text (_, section) sh_string_table =
  (* TODO: calculate alignment *)
  make_section ".text" 1
    ~size:(Int64.of_int (X86_binary_emitter.size section))
    ~flags:0x6L sh_string_table

let make_data (_, section) sh_string_table =
  (* TODO: calculate alignment *)
  make_section ".data" 1
    ~size:(Int64.of_int (X86_binary_emitter.size section))
    ~flags:0x3L sh_string_table

let make_custom_section ((s_l, s_opt, s_l'), section) sh_string_table =
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
  make_section (String.concat "," s_l)
    ~size:(Int64.of_int (X86_binary_emitter.size section))
    ~entsize sh_string_table ~flags

let assemble asm output_file =
  let sections = from_program asm in
  let sections =
    StringMap.mapi
      (fun key (s, instructions) ->
        ( s,
          X86_binary_emitter.assemble_section X64
            { X86_binary_emitter.sec_name = key;
              sec_instrs = Array.of_list instructions
            } ))
      sections
  in
  Printf.printf "%s\n" output_file;

  let elf = Owee.Owee_buf.map_binary_write output_file (1 * 1000 * 1000) in
  let sh_string_table = { current_length = 0; strings = [] } in

  let tmp = make_section "" 0 sh_string_table in

  let text = make_text (StringMap.find ".text" sections) sh_string_table in
  let sections = StringMap.remove ".text" sections in

  let data = make_data (StringMap.find ".data" sections) sh_string_table in
  let sections = StringMap.remove ".data" sections in

  let sections =
    List.rev_map
      (fun (key, value) -> make_custom_section value 0 sh_string_table)
      (StringMap.bindings sections)
  in

  let symtab = make_section ".symtab" 2 ~entsize:24L sh_string_table in

  let strtab = make_section ".strtab" 3 sh_string_table in

  let shstrtab = make_section ".shstrtab" 3 sh_string_table in
  let shstrtab =
    { shstrtab with sh_size = Int64.of_int sh_string_table.current_length }
  in

  let sections =
    [tmp; text; data; tmp; tmp] @ sections @ [tmp; tmp; symtab; strtab; shstrtab]
  in
  let identification = make_identification in
  let header = make_header identification (List.length sections) in
  Owee.Owee_elf.write_elf elf header (Array.of_list sections);
  ()
