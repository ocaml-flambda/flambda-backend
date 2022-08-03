open X86_binary_emitter
module String = Misc.Stdlib.String

module IntTbl = Hashtbl.Make (struct
  type t = int

  let equal = ( = )

  let hash = Hashtbl.hash
end)

type t =
  { mutable global_symbols : Symbol_entry.t list;
    mutable global_num_symbols : int;
    mutable local_symbols : Symbol_entry.t list;
    mutable local_num_symbols : int;
    global_symbols_tbl : int String.Tbl.t;
    local_symbols_tbl : int String.Tbl.t;
    labels_tbl : (X86_binary_emitter.symbol * Symbol_entry.t) String.Tbl.t;
    section_symbol_tbl : int IntTbl.t
  }

let create () =
  { global_symbols = [];
    global_num_symbols = 0;
    global_symbols_tbl = String.Tbl.create 100;
    local_symbols = [];
    local_num_symbols = 0;
    local_symbols_tbl = String.Tbl.create 100;
    labels_tbl = String.Tbl.create 100;
    section_symbol_tbl = IntTbl.create 100
  }

let add_symbol t symbol =
  match Symbol_entry.get_bind symbol with
  | Local ->
    (match Symbol_entry.get_type symbol with
    | Section ->
      IntTbl.add t.section_symbol_tbl
        (Symbol_entry.get_shndx symbol)
        t.local_num_symbols
    | _ ->
      String.Tbl.add t.local_symbols_tbl
        (Symbol_entry.get_name_str symbol)
        t.local_num_symbols);
    t.local_num_symbols <- t.local_num_symbols + 1;
    t.local_symbols <- symbol :: t.local_symbols
  | Global ->
    String.Tbl.add t.global_symbols_tbl
      (Symbol_entry.get_name_str symbol)
      t.global_num_symbols;
    t.global_num_symbols <- t.global_num_symbols + 1;
    t.global_symbols <- symbol :: t.global_symbols

let add_label t label symbol =
  String.Tbl.add t.labels_tbl label.sy_name (label, symbol)

let get_label t name = String.Tbl.find t.labels_tbl name

let get_label_idx t name =
  let label, symbol = get_label t name in
  let idx = IntTbl.find t.section_symbol_tbl (Symbol_entry.get_shndx symbol) in
  label, idx

let get_symbol_idx_opt t name =
  match String.Tbl.find_opt t.global_symbols_tbl name with
  | Some idx -> Some (t.local_num_symbols + idx)
  | None -> String.Tbl.find_opt t.local_symbols_tbl name

let num_symbols t = t.local_num_symbols + t.global_num_symbols

let num_locals t = t.local_num_symbols

let make_undef_symbol t name string_table =
  add_symbol t (Symbol_entry.create_undef_symbol name string_table)

let make_section_symbol t st_shndx sections =
  let symbol_entry = Symbol_entry.create_section_symbol st_shndx in
  add_symbol t symbol_entry;
  symbol_entry

let make_symbol t symbol sections string_table =
  let symbol_entry =
    Symbol_entry.create_symbol symbol t sections string_table
  in
  add_symbol t symbol_entry

let write t sh_offset buf =
  List.iteri
    (fun i symbol ->
      let idx = (i * 24) + Int64.to_int sh_offset in
      Symbol_entry.write symbol (Owee.Owee_buf.cursor buf ~at:idx))
    (List.rev t.local_symbols @ List.rev t.global_symbols)
