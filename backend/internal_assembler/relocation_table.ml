type relocation

type t =
  { section : X86_proc.SectionName.t;
    mutable relocations : Relocation_entry.t list
  }

module StringMap = X86_binary_emitter.StringMap

let create section = { section; relocations = [] }

let make_relocation t relocation symbol_table string_table =
  t.relocations
    <- Relocation_entry.create_relocation relocation symbol_table string_table
       :: t.relocations

let num_relocations t = List.length t.relocations

let section_name t = t.section

let write t section_table buf =
  match
    Section_table.get_section_opt section_table
      (X86_proc.SectionName.from_name
         (".rela" ^ X86_proc.SectionName.name t.section))
  with
  | Some table ->
    List.iteri
      (fun i relocation ->
        let open Owee.Owee_buf in
        let idx = (i * 24) + Int64.to_int table.sh_offset in
        Relocation_entry.write relocation (cursor buf ~at:idx))
      t.relocations
  | None -> ()
