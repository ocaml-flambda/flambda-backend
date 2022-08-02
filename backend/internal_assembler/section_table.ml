module StringTbl = X86_binary_emitter.StringTbl
module SectionTbl = Hashtbl.Make (X86_proc.SectionName)

type t =
  { mutable sections : Owee.Owee_elf.section list;
    mutable num_sections : int;
    section_tb : int SectionTbl.t;
    mutable section_bodies : (int * bytes) list;
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
    section_tb = SectionTbl.create 100;
    section_bodies = [];
    current_offset = 64L
  }

let add_section t name ?body section =
  t.sections <- section :: t.sections;
  SectionTbl.add t.section_tb name t.num_sections;
  t.num_sections <- t.num_sections + 1;
  t.current_offset <- Int64.add t.current_offset section.sh_size;
  match body with
  | Some body ->
    t.section_bodies
      <- (Int64.to_int section.sh_offset, body) :: t.section_bodies
  | None -> ()

let current_offset t = t.current_offset

let num_sections t = t.num_sections

let get_sec_idx t name = SectionTbl.find t.section_tb name

let get_section t name =
  List.find
    (fun section ->
      String.equal section.sh_name_str (X86_proc.SectionName.name name))
    t.sections

let get_section_opt t name =
  List.find_opt
    (fun section ->
      String.equal section.sh_name_str (X86_proc.SectionName.name name))
    t.sections

let get_sections t = Array.of_list (List.rev t.sections)

let get_section_bodies t = t.section_bodies

let write_bodies t buf =
  List.iter
    (fun (pos, body) ->
      Owee.Owee_buf.Write.fixed_bytes
        (Owee.Owee_buf.cursor buf ~at:pos)
        (Bytes.length body) body)
    t.section_bodies
