type t =
  { r_offset : Owee.Owee_buf.u64;
    r_info : Owee.Owee_buf.u64;
    r_addend : Owee.Owee_buf.u64
  }

let create_relocation ~offset ~info ~addend =
  { r_offset = offset; r_info = info; r_addend = addend }

let is_label name =
  String.length name >= 2 && Char.equal name.[0] '.' && Char.equal name.[1] 'L'

let get_reloc_info relocation_type addend name symbol_table string_table =
  if is_label name
  then
    let label, idx = Symbol_table.get_label_idx symbol_table name in
    ( relocation_type,
      idx,
      Int64.add addend (Int64.of_int (Option.value ~default:0 label.sy_pos)) )
  else
    let symbol =
      match Symbol_table.get_symbol_idx_opt symbol_table name with
      | Some i -> i
      | None ->
        Symbol_table.make_undef_symbol symbol_table name string_table;
        Symbol_table.num_symbols symbol_table - 1
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
  { r_offset = Int64.of_int relocation.offset_from_section_beginning;
    r_info =
      Int64.logor
        (Int64.shift_left (Int64.of_int relocation_symbol) 32)
        (Int64.of_int relocation_type);
    r_addend = addend
  }

open Owee.Owee_buf

let write t cursor =
  Write.u64 cursor t.r_offset;
  Write.u64 cursor t.r_info;
  Write.u64 cursor t.r_addend
