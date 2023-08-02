open Asm_targets

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

type fde =
  { start_address : int;
    length : int;
    instructions : call_frame_instr list;
    current_address : int;
    current_cfa_offset : int
  }

type debug_frame_state =
  { current_fde : fde option;
    complete_fdes : fde list
  }

type t =
  { mutable state : debug_frame_state;
    mutable checkpoint_state : debug_frame_state option;
    code_begin : Asm_symbol.t
  }

let checkpoint t = t.checkpoint_state <- Some t.state

let rollback t =
  match t.checkpoint_state with
  | None ->
    failwith "debug_frame section: attempting to rollback without a checkpoint"
  | Some s ->
    t.state <- s;
    t.checkpoint_state <- None

let null_byte = Dwarf_value.uint8 Numbers.Uint8.zero

let cie_id =
  if !Dwarf_flags.gdwarf_use_eh_frame
  then Dwarf_value.uint32 Numbers.Uint32.zero
  else
    match !Dwarf_flags.gdwarf_format with
    | Thirty_two ->
      Dwarf_value.uint32 (Numbers.Uint32.of_nonnegative_int64_exn 0xffffffffL)
    | Sixty_four ->
      Dwarf_value.uint64
        (Numbers.Uint64.of_nonnegative_int64_exn 0xffffffffffffffffL)

let version =
  if !Dwarf_flags.gdwarf_use_eh_frame
  then Dwarf_value.uint8 Numbers.Uint8.one
  else Dwarf_value.uint8 (Numbers.Uint8.of_nonnegative_int_exn 4)

let augmentation = null_byte

let address_size_int () =
  Dwarf_value.absolute_address Targetint.zero
  |> Dwarf_value.size |> Dwarf_int.to_int64 |> Int64.to_int

let address_size () =
  Dwarf_value.uint8 (Numbers.Uint8.of_nonnegative_int_exn (address_size_int ()))

let segment_size = null_byte

let code_alignment_factor =
  Numbers.Uint64.of_nonnegative_int_exn 1 |> Dwarf_value.uleb128

let data_alignment_factor = Dwarf_value.sleb128 (-8L)

let return_address_register =
  Numbers.Uint64.of_nonnegative_int_exn 16 |> Dwarf_value.uleb128

let initial_cfa_offset = 8

let initial_instructions = [Def_cfa (7, initial_cfa_offset); Offset (16, 1)]

let cie_label =
  lazy (Asm_label.create (Asm_section.DWARF Asm_section.Debug_frame))

let cie_pointer () = Dwarf_value.offset_into_debug_frame (Lazy.force cie_label)

let create ~code_begin =
  { state = { current_fde = None; complete_fdes = [] };
    checkpoint_state = None;
    code_begin
  }

let process_cfi_startproc t ~address =
  t.state
    <- (match t.state.current_fde with
       | Some _ ->
         failwith
           "debug_frame section: cfi_startproc before previous function was \
            closed"
       | None ->
         { t.state with
           current_fde =
             Some
               { start_address = address;
                 length = 0;
                 instructions = [];
                 current_address = address;
                 current_cfa_offset = initial_cfa_offset
               }
         })

let process_cfi_adjust_cfa_offset t ~address ~offset =
  match t.state.current_fde with
  | None ->
    failwith
      "debug_frame section: cfi_adjust_cfa_offset when no function is open"
  | Some fde ->
    let required_loc_advance = address - fde.current_address in
    let new_cfa_offset = fde.current_cfa_offset + offset in
    let instructions =
      if required_loc_advance = 0
      then fde.instructions
      else if required_loc_advance < 64
      then Advance_loc required_loc_advance :: fde.instructions
      else if required_loc_advance < 256
      then Advance_loc1 required_loc_advance :: fde.instructions
      else if required_loc_advance < 65536
      then Advance_loc2 required_loc_advance :: fde.instructions
      else if required_loc_advance < 4294967296
      then Advance_loc4 required_loc_advance :: fde.instructions
      else failwith "debug_frame_section: location advance too large"
    in
    let instructions =
      if offset = 0
      then instructions
      else Def_cfa_offset new_cfa_offset :: instructions
    in
    t.state
      <- { t.state with
           current_fde =
             Some
               { fde with
                 instructions;
                 current_address = address;
                 current_cfa_offset = new_cfa_offset
               }
         }

let process_cfi_endproc t ~address =
  t.state
    <- (match t.state.current_fde with
       | None ->
         failwith "debug_frame section: cfi_endproc when no function is open"
       | Some fde ->
         { current_fde = None;
           complete_fdes =
             { fde with length = address - fde.start_address }
             :: t.state.complete_fdes
         })

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

let instructions_size instructions =
  let ( + ) = Dwarf_int.add in
  List.fold_left
    (fun size instr -> size + instr_size instr)
    (Dwarf_int.zero ()) instructions

let emit_instructions ~asm_directives instructions =
  List.map encode instructions
  |> List.iter (List.iter (Dwarf_value.emit ~asm_directives))

let emit_padding ~asm_directives size =
  List.init size (fun _ -> Nop) |> emit_instructions ~asm_directives

let initial_length_size () =
  if !Dwarf_flags.gdwarf_use_eh_frame
  then Dwarf_value.uint32 Numbers.Uint32.zero |> Dwarf_value.size
  else Dwarf_int.zero () |> Initial_length.create |> Initial_length.size

let cie_size_without_padding_or_first_word () =
  let ( + ) = Dwarf_int.add in
  Dwarf_value.size cie_id + Dwarf_value.size version
  + Dwarf_value.size augmentation
  + (if !Dwarf_flags.gdwarf_use_eh_frame
    then Dwarf_int.zero ()
    else Dwarf_value.size (address_size ()) + Dwarf_value.size segment_size)
  + Dwarf_value.size code_alignment_factor
  + Dwarf_value.size data_alignment_factor
  + Dwarf_value.size return_address_register
  + instructions_size initial_instructions

let required_padding_size length =
  let padding = address_size_int () - (length mod address_size_int ()) in
  if padding = address_size_int () then 0 else padding

let cie_padding_size () =
  Dwarf_int.add
    (cie_size_without_padding_or_first_word ())
    (initial_length_size ())
  |> Dwarf_int.to_int64 |> Int64.to_int |> required_padding_size

let cie_size_without_first_word () =
  Dwarf_int.add
    (cie_size_without_padding_or_first_word ())
    (Dwarf_int.of_host_int_exn (cie_padding_size ()))

let cie_size () =
  Dwarf_int.add (initial_length_size ()) (cie_size_without_first_word ())

let fde_size_without_padding_or_first_word fde =
  let ( + ) = Dwarf_int.add in
  (if !Dwarf_flags.gdwarf_use_eh_frame
  then Dwarf_value.uint32 Numbers.Uint32.zero |> Dwarf_value.size
  else Dwarf_value.size (cie_pointer ()))
  + Dwarf_value.size (Dwarf_value.absolute_address Targetint.zero)
  + Dwarf_value.size (Dwarf_value.absolute_address Targetint.zero)
  + instructions_size fde.instructions

let fde_padding_size fde =
  Dwarf_int.add
    (fde_size_without_padding_or_first_word fde)
    (initial_length_size ())
  |> Dwarf_int.to_int64 |> Int64.to_int |> required_padding_size

let fde_size_without_first_word fde =
  Dwarf_int.add
    (fde_size_without_padding_or_first_word fde)
    (Dwarf_int.of_host_int_exn (fde_padding_size fde))

let fde_size fde =
  Dwarf_int.add (initial_length_size ()) (fde_size_without_first_word fde)

let size t =
  let ( + ) = Dwarf_int.add in
  List.fold_left ( + ) (cie_size ()) (List.map fde_size t.state.complete_fdes)

let emit_initial_length_field ~asm_directives length =
  if !Dwarf_flags.gdwarf_use_eh_frame
  then
    let length_field =
      Dwarf_value.uint32
        (length |> Dwarf_int.to_int64 |> Numbers.Uint32.of_nonnegative_int64_exn)
    in
    Dwarf_value.emit ~asm_directives length_field
  else
    let initial_length = Initial_length.create length in
    Initial_length.emit ~asm_directives initial_length

let emit_cie ~asm_directives =
  emit_initial_length_field ~asm_directives (cie_size_without_first_word ());
  Dwarf_value.emit ~asm_directives cie_id;
  (* Dwarf_version is not used because the version field is one byte, not two
     bytes *)
  Dwarf_value.emit ~asm_directives version;
  Dwarf_value.emit ~asm_directives augmentation;
  if not !Dwarf_flags.gdwarf_use_eh_frame
  then (
    Dwarf_value.emit ~asm_directives (address_size ());
    Dwarf_value.emit ~asm_directives segment_size);
  Dwarf_value.emit ~asm_directives code_alignment_factor;
  Dwarf_value.emit ~asm_directives data_alignment_factor;
  Dwarf_value.emit ~asm_directives return_address_register;
  emit_instructions ~asm_directives initial_instructions;
  emit_padding ~asm_directives (cie_padding_size ())

let emit_fde ~asm_directives ~code_begin ~cie_offset fde =
  emit_initial_length_field ~asm_directives (fde_size_without_first_word fde);
  if !Dwarf_flags.gdwarf_use_eh_frame
  then
    Dwarf_value.emit ~asm_directives
      (Dwarf_int.add !cie_offset (initial_length_size ())
      |> Dwarf_int.to_int64 |> Numbers.Uint32.of_nonnegative_int64_exn
      |> Dwarf_value.uint32)
  else Dwarf_value.emit ~asm_directives (cie_pointer ());
  Dwarf_value.emit ~asm_directives
    (Dwarf_value.code_address_from_symbol_plus_bytes code_begin
       (Targetint.of_int fde.start_address));
  Dwarf_value.emit ~asm_directives
    (Dwarf_value.absolute_address (Targetint.of_int fde.length));
  emit_instructions ~asm_directives (List.rev fde.instructions);
  emit_padding ~asm_directives (fde_padding_size fde);
  cie_offset := Dwarf_int.add !cie_offset (fde_size fde)

let emit ~asm_directives t =
  let module A = (val asm_directives : Asm_directives.S) in
  A.define_label (Lazy.force cie_label);
  emit_cie ~asm_directives;
  let cie_offset = ref (cie_size ()) in
  List.iter
    (emit_fde ~asm_directives ~code_begin:t.code_begin ~cie_offset)
    (List.rev t.state.complete_fdes)
