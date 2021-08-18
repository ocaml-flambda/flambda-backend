[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = Asm_directives_intf.S
module type Arg = Asm_directives_intf.Arg

module Make ( A : Asm_directives_intf.Arg ) : Asm_directives_intf.S = struct

  module D = A.D

  let switch_to_section section =
    A.emit_line ("switch_to_section: " ^ Asm_section.to_string section)

  let initialize () =
    (* Forward label references are illegal in GAS *)
    begin match Config_typed.assembler () with
    | MASM | MacOS -> ()
    | GAS_like -> List.iter switch_to_section (Asm_section.dwarf_sections_in_order ())
    end;
    (* Stop dsymutil complaining about empty __debug_line sections (produces
      bogus error "line table parameters mismatch") by making sure such sections
      are never empty. *)
    D.file ~file_num:1 ~file_name:"none";  (* also PR#7037 *)
    D.loc ~file_num:1 ~line:1 ~col:1 ();
    D.text ()

  let int8 ?comment:_ _num = A.emit_line "int8"
  let int16 ?comment:_ _num = A.emit_line "int16"
  let int32 ?comment:_ _num = A.emit_line "int32"
  let int64 ?comment:_ _num = A.emit_line "int64"
  let uint8 ?comment:_ _num = A.emit_line "uint8"
  let uint16 ?comment:_ _num = A.emit_line "uint16"
  let uint32 ?comment:_ _num = A.emit_line "uint32"
  let uint64 ?comment:_ _num = A.emit_line "uint64"
  let targetint ?comment:_ _num = A.emit_line "targetint"
  let uleb128 ?comment:_ _num = A.emit_line "uleb128"
  let sleb128 ?comment:_ _num = A.emit_line "sleb128"
  let string ?comment:_ _num = A.emit_line "string"

  let cache_string ?comment:_ section _str =
    A.emit_line "cache_string";
    Asm_label.create section 

  let emit_cached_strings () = A.emit_line "emit_cached_strings"

  let comment str = A.emit_line ("comment: " ^ str)

  let new_line () = A.emit_line "new_line"

  let define_data_symbol _sym = A.emit_line "define_data_symbol: XXX"

  let global _sym = A.emit_line "global: XXX"

  let symbol ?comment:_ _sym = A.emit_line "symbol"

  let define_label _lab = A.emit_line "define_label"

  let label ?comment:_ _lab = A.emit_line "label"

  let symbol_plus_offset _sym ~offset_in_bytes:_ = A.emit_line "symbol_plus_offset"
  
  let between_symbols_in_current_unit ~upper:_ ~lower:_ = A.emit_line "between_symbols_in_current_unit"
  
  let between_labels_16_bit ?comment:_ ~upper:_ ~lower:_ () = A.emit_line "between_labels_16_bit"
  let between_labels_32_bit ?comment:_ ~upper:_ ~lower:_ () = A.emit_line "between_labels_32_bit"
  let between_labels_64_bit ?comment:_ ~upper:_ ~lower:_ () = A.emit_line "between_labels_64_bit"

  let between_symbol_in_current_unit_and_label_offset 
  ?comment:_ ~upper:_ ~lower:_ ~offset_upper:_ () = A.emit_line "between_symbol_in_current_unit_and_label_offset"

  let offset_into_dwarf_section_label
    ?comment:_
    _section
    _label
    ~width:_
    = A.emit_line "offset_into_dwarf_section_label"
  
  let offset_into_dwarf_section_symbol
    ?comment:_
    _section
    _symbol
    ~width:_
    = A.emit_line "offset_into_dwarf_section_symbol"
end