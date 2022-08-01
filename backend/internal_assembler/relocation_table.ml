type relocation

type t =
  { name : string;
    mutable relocations : Relocation_entry.t list
  }

module StringMap = X86_binary_emitter.StringMap

let create name = { name; relocations = [] }

let make_relocation t relocation symbol_table string_table =
  t.relocations
    <- Relocation_entry.create_relocation relocation symbol_table string_table
       :: t.relocations

let name t = t.name

let num_relocations t = List.length t.relocations

let write t section_table buf =
  match Section_table.get_section_opt section_table (".rela" ^ t.name) with
  | Some table ->
    List.iteri
      (fun i relocation ->
        let open Owee.Owee_buf in
        let idx = (i * 24) + Int64.to_int table.sh_offset in
        Relocation_entry.write relocation (cursor buf ~at:idx))
      t.relocations
  | None -> ()
