open Int_replace_polymorphic_compare
module StringMap = X86_binary_emitter.StringMap

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

let name s_l s_opt s_l' =
  let first = String.concat "," s_l in
  let mid = Option.fold ~none:"" ~some:(Printf.sprintf ",%S") s_opt in
  let last = match s_l' with [] -> "" | l -> "," ^ String.concat "," l in
  first ^ mid ^ last

let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.equal (String.sub s2 0 (String.length s1)) s1

module StringTable = struct
  type t =
    { mutable current_length : int;
      mutable strings : string list
    }

  (* An index of 0 in the string table is reserved for the null string *)
  let create () = { current_length = 1; strings = [""] }

  let add_string t string =
    t.current_length <- t.current_length + String.length string + 1;
    t.strings <- string :: t.strings

  let current_length t = t.current_length

  let get_strings t = List.rev t.strings
end

module SectionTable : sig
  type t

  val create : unit -> t

  val add_section : t -> ?body:string -> Owee.Owee_elf.section -> unit

  val current_offset : t -> int64

  val num_sections : t -> int

  val get_sec_idx : t -> string -> int

  val get_section : t -> string -> Owee.Owee_elf.section

  val get_section_opt : t -> string -> Owee.Owee_elf.section option

  val get_sections : t -> Owee.Owee_elf.section array

  val get_section_bodies : t -> (int * string) list
end = struct
  type t =
    { mutable sections : Owee.Owee_elf.section list;
      mutable num_sections : int;
      section_tb : int StringTbl.t;
      mutable section_bodies : (int * string) list;
      mutable current_offset : int64
    }

  open Owee.Owee_elf

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
      section_tb = StringTbl.create 100;
      section_bodies = [];
      current_offset = 64L
    }

  let add_section t ?body section =
    t.sections <- section :: t.sections;
    StringTbl.add t.section_tb section.sh_name_str t.num_sections;
    t.num_sections <- t.num_sections + 1;
    t.current_offset <- Int64.add t.current_offset section.sh_size;
    match body with
    | Some body ->
      t.section_bodies
        <- (Int64.to_int section.sh_offset, body) :: t.section_bodies
    | None -> ()

  let current_offset t = t.current_offset

  let num_sections t = t.num_sections

  let get_sec_idx t name = StringTbl.find t.section_tb name

  (* let rec pos name acc l = *)
  (*   match l with *)
  (*   | [] -> failwith "Not found" *)
  (*   | { sh_name_str } :: tl when String.equal sh_name_str name -> acc + 1 *)
  (*   | _ :: tl -> pos name (acc + 1) tl *)
  (* in *)
  (* num_sections t - pos name 0 t.sections *)

  let get_section t name =
    List.find (fun section -> String.equal section.sh_name_str name) t.sections

  let get_section_opt t name =
    List.find_opt
      (fun section -> String.equal section.sh_name_str name)
      t.sections

  let get_sections t = Array.of_list (List.rev t.sections)

  let get_section_bodies t = t.section_bodies
end

module SymbolEntry = struct
  type t =
    { st_name : Owee.Owee_buf.u32;
      st_info : Owee.Owee_buf.u8;
      st_other : Owee.Owee_buf.u8;
      st_shndx : Owee.Owee_buf.u16;
      st_value : Owee.Owee_buf.u64;
      st_size : Owee.Owee_buf.u64;
      st_name_str : string
    }

  let create_undef_symbol name string_table =
    let symbol_entry =
      { st_name = StringTable.current_length string_table;
        st_info = 1 lsl 4;
        st_other = 0;
        st_shndx = 0;
        st_value = 0L;
        st_size = 0L;
        st_name_str = name
      }
    in
    StringTable.add_string string_table name;
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
      { st_name = StringTable.current_length string_table;
        st_info = (global lsl 4) lor bind;
        st_other = 0;
        st_shndx = SectionTable.get_sec_idx sections symbol.sy_sec.sec_name;
        (* st_shname_str = symbol.sy_sec.sec_name; *)
        st_value = Int64.of_int value;
        st_size = Int64.of_int size;
        st_name_str = symbol.sy_name
      }
    in
    StringTable.add_string string_table symbol.sy_name;
    symbol_entry

  let create_got_symbol symbol_table string_table =
    create_undef_symbol "_GLOBAL_OFFSET_TABLE_" string_table

  let get_bind t = t.st_info lsr 4

  let get_type t = t.st_info land 0xf

  let get_shndx t = t.st_shndx

  let get_name_str t = t.st_name_str

  open Owee.Owee_buf

  let write t cursor =
    Write.u32 cursor t.st_name;
    Write.u8 cursor t.st_info;
    Write.u8 cursor t.st_other;
    Write.u16 cursor t.st_shndx;
    Write.u64 cursor t.st_value;
    Write.u64 cursor t.st_size
end

module SymbolTable = struct
  open X86_binary_emitter

  type t =
    { mutable symbols : SymbolEntry.t list;
      mutable num_symbols : int;
      labels_tbl : (X86_binary_emitter.symbol * SymbolEntry.t) StringTbl.t;
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
           (fun a b -> SymbolEntry.get_bind b - SymbolEntry.get_bind a)
           t.symbols;
    List.iteri
      (fun i x ->
        if SymbolEntry.get_type x = 3
        then IntTbl.add t.section_symbol_tbl (SymbolEntry.get_shndx x) i
        else StringTbl.add t.symbols_tbl (SymbolEntry.get_name_str x) i)
      (List.rev t.symbols)

  let num_locals t =
    List.length (List.filter (fun x -> SymbolEntry.get_bind x = 0) t.symbols)

  let get_symbols t = List.rev t.symbols

  let make_undef_symbol t name string_table =
    add_symbol t (SymbolEntry.create_undef_symbol name string_table)

  let make_section_symbol t st_shndx sections =
    let symbol_entry = SymbolEntry.create_section_symbol st_shndx in
    add_symbol t symbol_entry;
    symbol_entry

  let make_symbol symbol symbol_table sections string_table =
    let symbol_entry =
      SymbolEntry.create_symbol symbol symbol_table sections string_table
    in
    add_symbol symbol_table symbol_entry
end

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
  Profile.record_call ~accumulate:true "after_other" (fun () ->
      let new_tbl = StringTbl.create (StringTbl.length sections_tbl) in
      StringTbl.iter
        (fun n (sn, s) -> StringTbl.add new_tbl n (sn, !s))
        sections_tbl);
  Profile.record ~accumulate:true "after"
    (StringTbl.fold
       (fun n (sn, s) acc -> StringMap.add n (sn, List.rev !s) acc)
       sections_tbl)
    StringMap.empty

module RelocationEntry = struct
  type t =
    { r_offset : Owee.Owee_buf.u64;
      r_info : Owee.Owee_buf.u64;
      r_addend : Owee.Owee_buf.u64
    }

  let create_relocation ~offset ~info ~addend =
    { r_offset = offset; r_info = info; r_addend = addend }

  open Owee.Owee_buf

  let write t cursor =
    Write.u64 cursor t.r_offset;
    Write.u64 cursor t.r_info;
    Write.u64 cursor t.r_addend
end

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

let get_reloc_info relocation_type addend name symbol_table string_table =
  let is_label = isprefix ".L" name in
  if is_label
  then
    let label, idx = SymbolTable.get_label_idx symbol_table name in
    ( relocation_type,
      idx,
      Int64.add addend (Int64.of_int (Option.value ~default:0 label.sy_pos)) )
  else
    let symbol =
      match SymbolTable.get_symbol_idx_opt symbol_table name with
      | Some i -> i
      | None ->
        SymbolTable.make_undef_symbol symbol_table name string_table;
        SymbolTable.num_symbols symbol_table - 1
    in
    relocation_type, symbol, addend

let create_relocation (relocation : X86_binary_emitter.Relocation.t)
    symbol_table string_table =
  let relocation_type, relocation_symbol, addend =
    match relocation.kind with
    | DIR64 (name, addend) ->
      (get_reloc_info 1 addend name symbol_table) string_table
    | DIR32 (_, _) -> failwith "cannot generate dir32"
    | REL32 (name, addend) -> (
      match String.split_on_char '@' name with
      | [name; "GOTPCREL"] ->
        (get_reloc_info 9 (Int64.sub addend 4L) name symbol_table) string_table
      | [name; "PLT"] | [name] ->
        (get_reloc_info 4 (Int64.sub addend 4L) name symbol_table) string_table
      | _ -> failwith (Printf.sprintf "Invalid symbol %s\n" name))
  in
  RelocationEntry.create_relocation
    ~offset:(Int64.of_int relocation.offset_from_section_beginning)
    ~info:
      (Int64.logor
         (Int64.shift_left (Int64.of_int relocation_symbol) 32)
         (Int64.of_int relocation_type))
    ~addend

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
    { sh_name = StringTable.current_length shstrtab;
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
  StringTable.add_string shstrtab name;
  section

let make_section sections name sh_type ~size ?align ?entsize ?flags ?sh_link
    ?sh_info ?body shstrtab =
  let section : Owee.Owee_elf.section =
    create_section name ~sh_type ~size
      ~offset:(SectionTable.current_offset sections)
      ?align ?entsize ?flags ?sh_link ?sh_info shstrtab
  in
  SectionTable.add_section sections ?body section

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
         (StringTable.current_length sh_string_table + 1 + String.length name))
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
  let idx = SectionTable.get_sec_idx sections name in
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
  StringMap.mapi
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
      SymbolTable.make_section_symbol symbol_table
        (SectionTable.num_sections section_table - 1)
        section_table)
    compiler_sections

let make_symbols section_tables compiler_sections symbol_table section_symbols
    string_table =
  StringMap.iter
    (fun key (section_info, align, raw_section) ->
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
          SymbolTable.make_symbol symbol symbol_table section_tables
            string_table)
        symbols)
    compiler_sections

let assemble asm output_file =
  let binary_sections =
    Profile.record ~accumulate:true "X86_binary_emitter" get_sections asm
  in

  let string_table = StringTable.create () in
  let sh_string_table = StringTable.create () in
  let sections = SectionTable.create () in
  let symbol_table = SymbolTable.create () in

  let section_symbols =
    Profile.record ~accumulate:true "make_compiler_sections"
      (make_compiler_sections sections binary_sections symbol_table)
      sh_string_table
  in

  Profile.record ~accumulate:true "make_symbols"
    (make_symbols sections binary_sections symbol_table section_symbols)
    string_table;
  SymbolTable.make_undef_symbol symbol_table "_GLOBAL_OFFSET_TABLE_"
    string_table;
  Profile.record ~accumulate:true "finalise" SymbolTable.finalise symbol_table;

  let raw_relocations =
    Profile.record ~accumulate:true "relocation"
      (List.filter_map (fun (key, ((s_l, _, _), align, raw_section)) ->
           match
             List.map
               (fun relocation ->
                 (create_relocation relocation symbol_table) string_table)
               (X86_binary_emitter.relocations raw_section)
           with
           | [] -> None
           | l -> Some (String.concat "," s_l, l)))
      (StringMap.bindings binary_sections)
  in
  let symtab_idx =
    SectionTable.num_sections sections + List.length raw_relocations
  in
  List.iter
    (fun (name, relocations) ->
      make_relocation_section sections symtab_idx name
        (Int64.of_int (24 * List.length relocations))
        sh_string_table)
    raw_relocations;
  let num_locals = SymbolTable.num_locals symbol_table in

  let strtabidx = 1 + SectionTable.num_sections sections in
  make_section sections ".symtab" 2 ~entsize:24L
    ~size:(Int64.of_int (24 * SymbolTable.num_symbols symbol_table))
    ~align:8L ~sh_link:strtabidx ~sh_info:num_locals sh_string_table;
  make_section sections ".strtab" 3
    ~size:(Int64.of_int (StringTable.current_length string_table))
    sh_string_table;
  make_shstrtab sections sh_string_table;

  let identification = make_identification in
  let header =
    make_header identification
      (SectionTable.num_sections sections)
      (SectionTable.current_offset sections)
  in
  Printf.printf "File: %s\n" output_file;
  Printf.printf "Total size: %d\n"
    (Int64.to_int (SectionTable.current_offset sections)
    + (header.e_shnum * header.e_shentsize));
  let elf =
    Owee.Owee_buf.map_binary_write output_file
      (Int64.to_int (SectionTable.current_offset sections)
      + (header.e_shnum * header.e_shentsize))
  in
  Owee.Owee_elf.write_elf elf header (SectionTable.get_sections sections);
  List.iter
    (fun (pos, body) ->
      Owee.Owee_buf.Write.fixed_string
        (Owee.Owee_buf.cursor elf ~at:pos)
        (String.length body) body)
    (SectionTable.get_section_bodies sections);
  let symtab = SectionTable.get_section sections ".symtab" in
  let strtab = SectionTable.get_section sections ".strtab" in
  List.iteri
    (fun i symbol ->
      let idx = (i * 24) + Int64.to_int symtab.sh_offset in
      SymbolEntry.write symbol (Owee.Owee_buf.cursor elf ~at:idx))
    (SymbolTable.get_symbols symbol_table);
  List.iter
    (fun (name, relocations) ->
      match SectionTable.get_section_opt sections (".rela" ^ name) with
      | Some table ->
        List.iteri
          (fun i relocation ->
            let open Owee.Owee_buf in
            let idx = (i * 24) + Int64.to_int table.sh_offset in
            RelocationEntry.write relocation (cursor elf ~at:idx))
          relocations
      | None -> ())
    raw_relocations;
  let t = Owee.Owee_buf.cursor elf ~at:(Int64.to_int strtab.sh_offset) in
  List.iter
    (fun string ->
      Owee.Owee_buf.Write.zero_string t string ~maxlen:(String.length string))
    (StringTable.get_strings string_table);
  ()
