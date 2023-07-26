(* Some instructions omitted *)
type call_frame_instr =
  | Advance_loc of int
  | Offset of int * int
  | Restore of int
  | Nop
  | Advance_loc1 of int
  | Advance_loc2 of int
  | Advance_loc4 of int
  | Offset_extended of int * int
  | Restore_extended of int
  | Undefined of int
  | Same_value of int
  | Register of int * int
  | Remember_state
  | Restore_state
  | Def_cfa of int * int
  | Def_cfa_register of int
  | Def_cfa_offset of int
  | Offset_extended_sf of int * int
  | Def_cfa_sf of int * int
  | Def_cfa_offset_sf of int
  | Val_offset of int * int
  | Val_offset_sf of int * int

type t = unit

let null_byte = Dwarf_value.uint8 Numbers.Uint8.zero

let cie_id =
  match !Dwarf_flags.gdwarf_format with
  | Thirty_two ->
    Dwarf_value.uint32 (Numbers.Uint32.of_nonnegative_int64_exn 0xffffffffL)
  | Sixty_four ->
    Dwarf_value.uint64
      (Numbers.Uint64.of_nonnegative_int64_exn 0xffffffffffffffffL)

let augmentation = null_byte

let address_size_int =
  Dwarf_value.absolute_address Targetint.zero
  |> Dwarf_value.size |> Dwarf_int.to_int64 |> Int64.to_int

let address_size =
  Dwarf_value.uint8 (Numbers.Uint8.of_nonnegative_int_exn address_size_int)

let segment_size = null_byte

let code_alignment_factor =
  Numbers.Uint64.of_nonnegative_int_exn 1 |> Dwarf_value.uleb128

let data_alignment_factor = Dwarf_value.sleb128 (-8L)

let return_address_register =
  Numbers.Uint64.of_nonnegative_int_exn 16 |> Dwarf_value.uleb128

let initial_instructions = [Def_cfa (7, 8); Offset (16, 1)]

let create () = ()

let encode = function
  | Advance_loc delta ->
    [ (0b01 lsl 6) lor (delta land 0b00111111)
      |> Numbers.Uint8.of_nonnegative_int_exn |> Dwarf_value.uint8 ]
  | Offset (register, offset) ->
    [ (0b10 lsl 6) lor (register land 0b00111111)
      |> Numbers.Uint8.of_nonnegative_int_exn |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn offset |> Dwarf_value.uleb128 ]
  | Restore register ->
    [ (0b11 lsl 6) lor (register land 0b00111111)
      |> Numbers.Uint8.of_nonnegative_int_exn |> Dwarf_value.uint8 ]
  | Nop -> [null_byte]
  | Advance_loc1 delta ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x02 |> Dwarf_value.uint8;
      Numbers.Uint8.of_nonnegative_int_exn delta |> Dwarf_value.uint8 ]
  | Advance_loc2 delta ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x03 |> Dwarf_value.uint8;
      Numbers.Uint16.of_nonnegative_int_exn delta |> Dwarf_value.uint16 ]
  | Advance_loc4 delta ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x04 |> Dwarf_value.uint8;
      Numbers.Uint32.of_nonnegative_int_exn delta |> Dwarf_value.uint32 ]
  | Offset_extended (register, offset) ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x05 |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn register |> Dwarf_value.uleb128;
      Numbers.Uint64.of_nonnegative_int_exn offset |> Dwarf_value.uleb128 ]
  | Restore_extended register ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x06 |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn register |> Dwarf_value.uleb128 ]
  | Undefined register ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x07 |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn register |> Dwarf_value.uleb128 ]
  | Same_value register ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x08 |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn register |> Dwarf_value.uleb128 ]
  | Register (register1, register2) ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x09 |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn register1 |> Dwarf_value.uleb128;
      Numbers.Uint64.of_nonnegative_int_exn register2 |> Dwarf_value.uleb128 ]
  | Remember_state ->
    [Numbers.Uint8.of_nonnegative_int_exn 0x0a |> Dwarf_value.uint8]
  | Restore_state ->
    [Numbers.Uint8.of_nonnegative_int_exn 0x0b |> Dwarf_value.uint8]
  | Def_cfa (register, offset) ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x0c |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn register |> Dwarf_value.uleb128;
      Numbers.Uint64.of_nonnegative_int_exn offset |> Dwarf_value.uleb128 ]
  | Def_cfa_register register ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x0d |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn register |> Dwarf_value.uleb128 ]
  | Def_cfa_offset offset ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x0e |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn offset |> Dwarf_value.uleb128 ]
  | Offset_extended_sf (register, offset) ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x11 |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn register |> Dwarf_value.uleb128;
      Numbers.Uint64.of_nonnegative_int_exn offset |> Dwarf_value.uleb128 ]
  | Def_cfa_sf (register, offset) ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x12 |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn register |> Dwarf_value.uleb128;
      Int64.of_int offset |> Dwarf_value.sleb128 ]
  | Def_cfa_offset_sf offset ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x13 |> Dwarf_value.uint8;
      Int64.of_int offset |> Dwarf_value.sleb128 ]
  | Val_offset (register, factored_offset) ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x14 |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn register |> Dwarf_value.uleb128;
      Numbers.Uint64.of_nonnegative_int_exn factored_offset
      |> Dwarf_value.uleb128 ]
  | Val_offset_sf (register, factored_offset) ->
    [ Numbers.Uint8.of_nonnegative_int_exn 0x15 |> Dwarf_value.uint8;
      Numbers.Uint64.of_nonnegative_int_exn register |> Dwarf_value.uleb128;
      Int64.of_int factored_offset |> Dwarf_value.sleb128 ]

let instr_size instr =
  let ( + ) = Dwarf_int.add in
  List.fold_left
    (fun size value -> size + Dwarf_value.size value)
    (Dwarf_int.zero ()) (encode instr)

let program_size program =
  let ( + ) = Dwarf_int.add in
  List.fold_left
    (fun size instr -> size + instr_size instr)
    (Dwarf_int.zero ()) program

let initial_length_size =
  Dwarf_int.zero () |> Initial_length.create |> Initial_length.size

let cie_size_without_padding_or_first_word =
  let ( + ) = Dwarf_int.add in
  Dwarf_value.size cie_id + Dwarf_version.size Dwarf_version.four
  + Dwarf_value.size augmentation
  + Dwarf_value.size address_size
  + Dwarf_value.size segment_size
  + Dwarf_value.size code_alignment_factor
  + Dwarf_value.size data_alignment_factor
  + Dwarf_value.size return_address_register
  + program_size initial_instructions

let cie_padding_size =
  let size_without_padding_or_first_word =
    Dwarf_int.to_int64 cie_size_without_padding_or_first_word
  in
  let size_without_padding =
    Int64.add size_without_padding_or_first_word
      (Dwarf_int.to_int64 initial_length_size)
  in
  let padding =
    address_size_int - (Int64.to_int size_without_padding mod address_size_int)
  in
  if padding = address_size_int then 0 else padding

let cie_size_without_first_word =
  Dwarf_int.add cie_size_without_padding_or_first_word
    (Dwarf_int.of_host_int_exn cie_padding_size)

let cie_size = Dwarf_int.add initial_length_size cie_size_without_first_word

let size t = cie_size

let emit_cie ~asm_directives =
  let initial_length = Initial_length.create cie_size_without_first_word in
  Initial_length.emit ~asm_directives initial_length;
  Dwarf_value.emit ~asm_directives cie_id;
  Dwarf_version.emit ~asm_directives Dwarf_version.four;
  Dwarf_value.emit ~asm_directives augmentation;
  Dwarf_value.emit ~asm_directives address_size;
  Dwarf_value.emit ~asm_directives segment_size;
  Dwarf_value.emit ~asm_directives code_alignment_factor;
  Dwarf_value.emit ~asm_directives data_alignment_factor;
  Dwarf_value.emit ~asm_directives return_address_register;
  List.iter (Dwarf_value.emit ~asm_directives) (List.map encode initial_instructions);


let emit ~asm_directives t =

  Dwarf_version.emit ~asm_directives Dwarf_version.four;
  Dwarf_int.emit ~asm_directives (header_length t);
  Dwarf_value.emit ~asm_directives minimum_instruction_length;
  Dwarf_value.emit ~asm_directives maximum_operations_per_instruction;
  Dwarf_value.emit ~asm_directives default_is_stmt;
  Dwarf_value.emit ~asm_directives line_base;
  Dwarf_value.emit ~asm_directives line_range;
  Dwarf_value.emit ~asm_directives opcode_base;
  List.iter (Dwarf_value.emit ~asm_directives) standard_opcode_lengths;
  Dwarf_value.emit ~asm_directives null_byte;
  emit_file_names ~asm_directives t;
  emit_line_number_program ~asm_directives t
