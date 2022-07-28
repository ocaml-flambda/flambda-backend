open Int_replace_polymorphic_compare
module StringMap = X86_binary_emitter.StringMap

let name s_l s_opt s_l' =
  let first = String.concat "," s_l in
  let mid = Option.fold ~none:"" ~some:(Printf.sprintf ",%S") s_opt in
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
  && String.equal (String.sub s2 0 (String.length s1)) s1

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
    st_shndx : Owee.Owee_buf.u16;
    st_value : Owee.Owee_buf.u64;
    st_size : Owee.Owee_buf.u64;
    st_name_str : string
  }

module StringTbl = Hashtbl.Make (struct
  type t = string

  let equal = String.equal

  let hash = Hashtbl.hash
end)

module IntTbl = Hashtbl.Make (struct
  type t = int

  let equal = ( = )

  let hash = Hashtbl.hash
end)

module SymbolTable : sig
  type t

  val create : unit -> t

  val add_symbol : t -> symbol_entry -> unit

  val add_label : t -> X86_binary_emitter.symbol -> symbol_entry -> unit

  val get_label_idx :
    t -> X86_binary_emitter.StringMap.key -> X86_binary_emitter.symbol * int

  val get_symbol_idx_opt : t -> StringTbl.key -> int option

  val num_symbols : t -> int

  val finalise : t -> unit

  val num_locals : t -> int

  val get_symbols : t -> symbol_entry list
end = struct
  open X86_binary_emitter

  type t =
    { mutable symbols : symbol_entry list;
      mutable num_symbols : int;
      labels_tbl : (X86_binary_emitter.symbol * symbol_entry) StringTbl.t;
      symbols_tbl : int StringTbl.t;
      section_symbol_tbl : int IntTbl.t
    }

  let create () =
    { symbols = [];
      num_symbols = 0;
      labels_tbl = StringTbl.create 100;
      symbols_tbl = StringTbl.create 100;
      section_symbol_tbl = IntTbl.create 100
    }

  let add_symbol t symbol =
    (* CR mcollins - properly handle case where symbols are added after finalise
       has been called *)
    t.num_symbols <- t.num_symbols + 1;
    t.symbols <- symbol :: t.symbols

  let add_label t label symbol =
    StringTbl.add t.labels_tbl label.sy_name (label, symbol)

  let get_label t name = StringTbl.find t.labels_tbl name

  let get_label_idx t name =
    let label, symbol = get_label t name in
    let idx = IntTbl.find t.section_symbol_tbl symbol.st_shndx in
    label, idx

  let get_symbol_idx_opt t name = StringTbl.find_opt t.symbols_tbl name

  let num_symbols t = t.num_symbols

  let finalise t =
    t.symbols
      <- List.stable_sort
           (fun a b -> (b.st_info lsl 4) - (a.st_info lsl 4))
           t.symbols;
    List.iteri
      (fun i x ->
        if x.st_info land 0xf = 3
        then IntTbl.add t.section_symbol_tbl x.st_shndx i
        else StringTbl.add t.symbols_tbl x.st_name_str i)
      (List.rev t.symbols)

  let num_locals t =
    List.length (List.filter (fun x -> x.st_info lsr 4 = 0) t.symbols)

  let get_symbols t = List.rev t.symbols
end

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

let add_string string_table string =
  string_table.current_length
    <- string_table.current_length + String.length string + 1;
  string_table.strings <- string :: string_table.strings

let create_undef_symbol name string_table =
  let symbol_entry =
    { st_name = string_table.current_length;
      st_info = 1 lsl 4;
      st_other = 0;
      st_shndx = 0;
      st_value = 0L;
      st_size = 0L;
      st_name_str = name
    }
  in
  add_string string_table name;
  symbol_entry

let make_section_symbol st_shndx sections symbol_table =
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
  SymbolTable.add_symbol symbol_table symbol_entry;
  symbol_entry

let make_symbol (symbol : X86_binary_emitter.symbol) symbol_table sections
    string_table =
  let rec pos name acc (l : Owee.Owee_elf.section list) =
    match l with
    | [] -> failwith "Not found"
    | { sh_name_str } :: tl when String.equal sh_name_str name -> acc + 1
    | _ :: tl -> pos name (acc + 1) tl
  in
  let pos name list = List.length list - pos name 0 list in
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
    { st_name = string_table.current_length;
      st_info = (global lsl 4) lor bind;
      st_other = 0;
      st_shndx = pos symbol.sy_sec.sec_name sections.sections;
      (* st_shname_str = symbol.sy_sec.sec_name; *)
      st_value = Int64.of_int value;
      st_size = Int64.of_int size;
      st_name_str = symbol.sy_name
    }
  in
  add_string string_table symbol.sy_name;
  SymbolTable.add_symbol symbol_table symbol_entry

let make_got_symbol symbol_table string_table =
  SymbolTable.add_symbol symbol_table
    (create_undef_symbol "_GLOBAL_OFFSET_TABLE_" string_table)

let get_reloc_info relocation_type addend name symbol_table string_table =
  let is_label = isprefix ".L" name in
  if is_label
  then
    Profile.record_call ~accumulate:true "weird stuff" (fun () ->
        let label, idx = SymbolTable.get_label_idx symbol_table name in
        ( relocation_type,
          idx,
          Int64.add addend (Int64.of_int (Option.value ~default:0 label.sy_pos))
        ))
  else
    let symbol =
      match SymbolTable.get_symbol_idx_opt symbol_table name with
      | Some i -> i
      | None ->
        SymbolTable.add_symbol symbol_table
          (create_undef_symbol name string_table);
        SymbolTable.num_symbols symbol_table - 1
    in
    relocation_type, symbol, addend

let create_relocation (relocation : X86_binary_emitter.Relocation.t)
    symbol_table string_table =
  let relocation_type, relocation_symbol, addend =
    match relocation.kind with
    | DIR64 (name, addend) ->
      Profile.record ~accumulate:true "get_reloc_info"
        (get_reloc_info 1 addend name symbol_table)
        string_table
    | DIR32 (_, _) -> failwith "cannot generate dir32"
    | REL32 (name, addend) -> (
      match String.split_on_char '@' name with
      | [name; "GOTPCREL"] ->
        Profile.record ~accumulate:true "get_reloc_info"
          (get_reloc_info 9 (Int64.sub addend 4L) name symbol_table)
          string_table
      | [name; "PLT"] | [name] ->
        Profile.record ~accumulate:true "get_reloc_info"
          (get_reloc_info 4 (Int64.sub addend 4L) name symbol_table)
          string_table
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
  let sh_addralign = Option.value ~default:1L align in
  let sh_entsize = Option.value ~default:0L entsize in
  let sh_flags = Option.value ~default:0L flags in
  let sh_link = Option.value ~default:0 sh_link in
  let sh_info = Option.value ~default:0 sh_info in
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

let make_relocation_section sections sym_tbl_idx name size sh_string_table =
  let open Owee.Owee_elf in
  let num_sections = List.length sections.sections in
  let i, section =
    List.find
      (fun (_, s) -> String.equal s.sh_name_str name)
      (List.mapi (fun i x -> i, x) sections.sections)
  in
  let idx = num_sections - i - 1 in
  let section =
    make_section sections (".rela" ^ name) 4 ~size ~entsize:24L ~flags:0x40L
      ~sh_link:sym_tbl_idx sh_string_table ~align:8L ~sh_info:idx
  in
  section

let get_sections asm =
  let sections = from_program asm in
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

let assemble asm output_file =
  let binary_sections =
    Profile.record ~accumulate:true "X86_binary_emitter" get_sections asm
  in

  let string_table = { current_length = 0; strings = [] } in
  let sh_string_table = { current_length = 0; strings = [] } in
  let sections = { sections = []; section_bodies = []; current_offset = 64L } in
  let symbol_table = SymbolTable.create () in
  (* An index of 0 in the string table is reserved for the null string *)
  add_string string_table "";

  make_null sections sh_string_table;

  let section_symbols =
    Profile.record ~accumulate:true "Make_sections"
      (StringMap.mapi (fun name (section_info, align, raw_section) ->
           if isprefix ".text" name
           then
             make_text sections section_info raw_section (Int64.of_int align)
               sh_string_table
           else if isprefix ".data" name
           then
             make_data sections section_info raw_section (Int64.of_int align)
               sh_string_table
           else
             make_custom_section sections section_info raw_section 1
               sh_string_table;
           make_section_symbol
             (List.length sections.sections - 1)
             sections symbol_table))
      binary_sections
  in

  Profile.record ~accumulate:true "symbols"
    (StringMap.iter (fun key (section_info, align, raw_section) ->
         let symbols = X86_binary_emitter.labels raw_section in
         let labels, symbols =
           StringMap.partition (fun name symbol -> isprefix ".L" name) symbols
         in
         StringMap.iter
           (fun name symbol ->
             SymbolTable.add_label symbol_table symbol
               (StringMap.find key section_symbols))
           labels;
         StringMap.iter
           (fun name symbol ->
             make_symbol symbol symbol_table sections string_table)
           symbols))
    binary_sections;
  make_got_symbol symbol_table string_table;
  Profile.record ~accumulate:true "finalise" SymbolTable.finalise symbol_table;
  (* List.iteri *)
  (*   (fun i x -> StringTable.add symbol_table.symbols_tbl x.st_name_str i) *)
  (*   symbol_table.symbols; *)
  let raw_relocations =
    Profile.record ~accumulate:true "relocation"
      (List.filter_map (fun (key, ((s_l, _, _), align, raw_section)) ->
           match
             List.map
               (fun relocation ->
                 Profile.record ~accumulate:true "create_relocation"
                   (create_relocation relocation symbol_table)
                   string_table)
               (X86_binary_emitter.relocations raw_section)
           with
           | [] -> None
           | l -> Some (String.concat "," s_l, l)))
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
  let num_locals = SymbolTable.num_locals symbol_table in

  let strtabidx = 1 + List.length sections.sections in
  make_section sections ".symtab" 2 ~entsize:24L
    ~size:(Int64.of_int (24 * SymbolTable.num_symbols symbol_table))
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
      (fun (section : Owee.Owee_elf.section) ->
        String.equal section.sh_name_str ".symtab")
      sections.sections
  in
  let strtab =
    List.find
      (fun (section : Owee.Owee_elf.section) ->
        String.equal section.sh_name_str ".strtab")
      sections.sections
  in
  List.iteri
    (fun i symbol ->
      let open Owee.Owee_buf in
      let t = cursor elf ~at:((i * 24) + Int64.to_int symtab.sh_offset) in
      Write.u32 t symbol.st_name;
      Write.u8 t symbol.st_info;
      Write.u8 t symbol.st_other;
      Write.u16 t symbol.st_shndx;
      Write.u64 t symbol.st_value;
      Write.u64 t symbol.st_size)
    (SymbolTable.get_symbols symbol_table);
  List.iter
    (fun (name, relocations) ->
      match
        List.find_opt
          (fun (section : Owee.Owee_elf.section) ->
            String.equal section.sh_name_str (".rela" ^ name))
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
