(* CR mshinwell: add file headers here & elsewhere *)

open Asm_targets

type line_number_program_instr =
  | Special_opcode of Dwarf_value.t
  | Copy
  | Advance_pc of Dwarf_value.t
  | Advance_line of Dwarf_value.t
  | Set_file of Dwarf_value.t
  | Set_column of Dwarf_value.t
  | Const_add_pc
  | End_sequence
  | Set_address of Dwarf_value.t
  | Set_discriminator of Dwarf_value.t

(* CR mshinwell: Use: module Int = Numbers.Int *)
module IntMap = Map.Make (Int)

type line_number_state =
  { line_number_program : line_number_program_instr list;
    address_reg : int;
    file_reg : int;
    line_reg : int;
    column_reg : int;
    discriminator_reg : int;
    source_files : Dwarf_value.t IntMap.t
  }

type t =
  { mutable current : line_number_state;
    mutable checkpoint : line_number_state option
  }

let checkpoint t = t.checkpoint <- Some t.current

let rollback t =
  match t.checkpoint with
  | None ->
    failwith "debug_line section: attempting to rollback without a checkpoint"
  | Some s ->
    t.current <- s;
    t.checkpoint <- None

let rec range low high =
  if high < low
  then []
  else if low = high
  then [low]
  else low :: range (low + 1) high

let default_file_name = Dwarf_value.string "none"

let minimum_instruction_length_int = 1

let minimum_instruction_length =
  Numbers.Uint8.of_nonnegative_int_exn minimum_instruction_length_int
  |> Dwarf_value.uint8

let maximum_operations_per_instruction = Numbers.Uint8.one |> Dwarf_value.uint8

let default_is_stmt = Dwarf_value.bool true

let line_base_int = -5

let line_base = Numbers.Int8.of_int_exn line_base_int |> Dwarf_value.int8

let line_range_int = 14

let line_range =
  Numbers.Uint8.of_nonnegative_int_exn line_range_int |> Dwarf_value.uint8

let opcode_base_int = 13

let opcode_base =
  Numbers.Uint8.of_nonnegative_int_exn opcode_base_int |> Dwarf_value.uint8

let standard_opcode_lengths =
  List.map
    (fun length ->
      Numbers.Uint8.of_nonnegative_int_exn length |> Dwarf_value.uint8)
    [0; 1; 1; 1; 1; 0; 0; 0; 1; 0; 0; 1]

let uleb128_zero = Dwarf_value.uleb128 Numbers.Uint64.zero

let null_byte = Dwarf_value.uint8 Numbers.Uint8.zero

let max_special_opcode_operation_advance =
  (255 - opcode_base_int) / line_range_int

(* Hopefully code_begin is the right symbol to use *)
let create ~code_begin =
  { current =
      { line_number_program =
          [Set_address (Dwarf_value.code_address_from_symbol code_begin)];
        address_reg = 0;
        file_reg = 1;
        line_reg = 1;
        column_reg = 0;
        discriminator_reg = 0;
        source_files = IntMap.empty
      };
    checkpoint = None
  }

let add_source_file t ~file_name ~file_num =
  (match IntMap.find_opt file_num t.current.source_files with
  | Some v -> failwith "debug_line section: multiple files with same number"
  | None -> ());
  t.current
    <- { t.current with
         source_files =
           IntMap.add file_num
             (Dwarf_value.string file_name)
             t.current.source_files
       }

let maybe_add_set_column_instr col_int state =
  if col_int != state.column_reg
  then
    let col =
      Numbers.Uint64.of_nonnegative_int_exn col_int |> Dwarf_value.uleb128
    in
    { state with
      line_number_program = Set_column col :: state.line_number_program;
      column_reg = col_int
    }
  else state

let maybe_add_set_file_instr file_num_int state =
  if file_num_int != state.file_reg
  then
    let file_num =
      Numbers.Uint64.of_nonnegative_int_exn file_num_int |> Dwarf_value.uleb128
    in
    { state with
      line_number_program = Set_file file_num :: state.line_number_program;
      file_reg = file_num_int
    }
  else state

let maybe_add_set_discriminator_instr discriminator_int state =
  if discriminator_int != state.discriminator_reg
  then
    let discriminator =
      Numbers.Uint64.of_nonnegative_int_exn discriminator_int
      |> Dwarf_value.uleb128
    in
    { state with
      line_number_program =
        Set_discriminator discriminator :: state.line_number_program;
      discriminator_reg = discriminator_int
    }
  else state

let maybe_add_advance_line_instr line state =
  let line_delta_int = line - state.line_reg in
  let line_delta_in_special_opcode_range =
    line_delta_int >= line_base_int
    && line_delta_int < line_base_int + line_range_int
  in
  if (not line_delta_in_special_opcode_range) && line_delta_int <> 0
  then
    let line_delta = Int64.of_int line_delta_int |> Dwarf_value.sleb128 in
    { state with
      line_number_program = Advance_line line_delta :: state.line_number_program;
      line_reg = line
    }
  else state

let maybe_add_const_add_pc_instr ~instr_address ~line state =
  let line_delta = line - state.line_reg in
  let operation_advance =
    (instr_address - state.address_reg) / minimum_instruction_length_int
  in
  let line_delta_in_special_opcode_range =
    line_delta >= line_base_int && line_delta < line_base_int + line_range_int
  in
  if not line_delta_in_special_opcode_range
  then state
  else
    let special_opcode_required_now =
      line_delta - line_base_int
      + (line_range_int * operation_advance)
      + opcode_base_int
    in
    let special_opcode_required_after_const_add_pc_instr =
      special_opcode_required_now
      - (line_range_int * max_special_opcode_operation_advance)
    in
    if special_opcode_required_now > 255
       && special_opcode_required_after_const_add_pc_instr <= 255
    then
      { state with
        line_number_program = Const_add_pc :: state.line_number_program;
        address_reg =
          state.address_reg
          + max_special_opcode_operation_advance
            * minimum_instruction_length_int
      }
    else state

let maybe_add_advance_pc_instr ~instr_address ~line state =
  let line_delta = line - state.line_reg in
  let operation_advance_int =
    (instr_address - state.address_reg) / minimum_instruction_length_int
  in
  let line_delta_in_special_opcode_range =
    line_delta >= line_base_int && line_delta < line_base_int + line_range_int
  in
  let operation_advance_in_special_opcode_range =
    line_delta - line_base_int
    + (line_range_int * operation_advance_int)
    + opcode_base_int
    <= 255
  in
  if ((not line_delta_in_special_opcode_range)
     || not operation_advance_in_special_opcode_range)
     && operation_advance_int <> 0
  then
    let operation_advance =
      Numbers.Uint64.of_nonnegative_int_exn operation_advance_int
      |> Dwarf_value.uleb128
    in
    { state with
      line_number_program =
        Advance_pc operation_advance :: state.line_number_program;
      address_reg = instr_address
    }
  else state

let add_copy_instr_or_special_opcode_instr ~instr_address ~line state =
  let line_delta = line - state.line_reg in
  let operation_advance =
    (instr_address - state.address_reg) / minimum_instruction_length_int
  in
  if operation_advance = 0 && line_delta = 0
  then
    { state with
      line_number_program = Copy :: state.line_number_program;
      discriminator_reg = 0
    }
  else
    let opcode =
      line_delta - line_base_int
      + (line_range_int * operation_advance)
      + opcode_base_int
      |> Numbers.Uint8.of_nonnegative_int_exn |> Dwarf_value.uint8
    in
    { state with
      line_number_program = Special_opcode opcode :: state.line_number_program;
      discriminator_reg = 0;
      address_reg = instr_address;
      line_reg = line
    }

(* If this row is identical to the last row we added, we do nothing: we do not
   add a duplicate row. *)
let add_line_number_matrix_row t ~instr_address ~file_num ~line ~col
    ~discriminator =
  let desired_discriminator = Option.value discriminator ~default:0 in
  let col_num = if col < 0 then 0 else col in
  if instr_address = t.current.address_reg
     && file_num = t.current.file_reg
     && line = t.current.line_reg
     && col_num = t.current.column_reg
     && desired_discriminator = t.current.discriminator_reg
  then ()
  else if instr_address < t.current.address_reg
  then
    failwith
      "attempt to add line number matrix row with an address lower than the \
       previous row"
  else
    t.current
      <- maybe_add_set_file_instr file_num t.current
         |> maybe_add_set_column_instr col_num
         |> maybe_add_set_discriminator_instr desired_discriminator
         |> maybe_add_advance_line_instr line
         |> maybe_add_const_add_pc_instr ~instr_address ~line
         |> maybe_add_advance_pc_instr ~instr_address ~line
         |> add_copy_instr_or_special_opcode_instr ~instr_address ~line

let file_names_size t =
  let ( + ) = Dwarf_int.add in
  let max_binding = IntMap.max_binding_opt t.current.source_files in
  let max_index_opt = Option.map fst max_binding in
  let max_index = Option.value max_index_opt ~default:0 in
  let size =
    List.fold_left
      (fun size index ->
        size
        + (match IntMap.find_opt index t.current.source_files with
          | None -> Dwarf_value.size default_file_name
          | Some file_name -> Dwarf_value.size file_name)
        + Dwarf_value.size uleb128_zero
        + Dwarf_value.size uleb128_zero
        + Dwarf_value.size uleb128_zero)
      (Dwarf_int.zero ()) (range 1 max_index)
  in
  size + Dwarf_value.size null_byte

let emit_file_names ~asm_directives t =
  let max_binding = IntMap.max_binding_opt t.current.source_files in
  let max_index_opt = Option.map fst max_binding in
  let max_index = Option.value max_index_opt ~default:0 in
  List.iter
    (fun index ->
      (match IntMap.find_opt index t.current.source_files with
      | None -> Dwarf_value.emit ~asm_directives default_file_name
      | Some file_name -> Dwarf_value.emit ~asm_directives file_name);
      (* Null terminate string *)
      Dwarf_value.emit ~asm_directives null_byte;
      Dwarf_value.emit ~asm_directives uleb128_zero;
      Dwarf_value.emit ~asm_directives uleb128_zero;
      Dwarf_value.emit ~asm_directives uleb128_zero)
    (range 1 max_index);
  Dwarf_value.emit ~asm_directives null_byte

let encode = function
  | Special_opcode opcode -> [opcode]
  | Copy -> [Dwarf_value.uint8 (Numbers.Uint8.of_nonnegative_int_exn 0x01)]
  | Advance_pc operation_advance ->
    [ Dwarf_value.uint8 (Numbers.Uint8.of_nonnegative_int_exn 0x02);
      operation_advance ]
  | Advance_line line_delta ->
    [Dwarf_value.uint8 (Numbers.Uint8.of_nonnegative_int_exn 0x03); line_delta]
  | Set_file file ->
    [Dwarf_value.uint8 (Numbers.Uint8.of_nonnegative_int_exn 0x04); file]
  | Set_column column ->
    [Dwarf_value.uint8 (Numbers.Uint8.of_nonnegative_int_exn 0x05); column]
  | Const_add_pc ->
    [Dwarf_value.uint8 (Numbers.Uint8.of_nonnegative_int_exn 0x08)]
  | End_sequence ->
    [ null_byte;
      Dwarf_value.uleb128 (Numbers.Uint64.of_nonnegative_int_exn 1);
      Dwarf_value.uint8 (Numbers.Uint8.of_nonnegative_int_exn 0x01) ]
  | Set_address address ->
    [ null_byte;
      Dwarf_value.size address |> Dwarf_int.to_int64 |> Int64.succ
      |> Numbers.Uint64.of_nonnegative_int64_exn |> Dwarf_value.uleb128;
      Dwarf_value.uint8 (Numbers.Uint8.of_nonnegative_int_exn 0x02);
      address ]
  | Set_discriminator discriminator ->
    [ null_byte;
      Dwarf_value.size discriminator
      |> Dwarf_int.to_int64 |> Int64.succ
      |> Numbers.Uint64.of_nonnegative_int64_exn |> Dwarf_value.uleb128;
      Dwarf_value.uint8 (Numbers.Uint8.of_nonnegative_int_exn 0x04);
      discriminator ]

let instr_size instr =
  let ( + ) = Dwarf_int.add in
  List.fold_left
    (fun size value -> size + Dwarf_value.size value)
    (Dwarf_int.zero ()) (encode instr)

let line_number_program_size t =
  let ( + ) = Dwarf_int.add in
  List.fold_left
    (fun size instr -> size + instr_size instr)
    (Dwarf_int.zero ())
    (End_sequence :: t.current.line_number_program)

let emit_line_number_program ~asm_directives t =
  List.iter
    (fun instr -> List.iter (Dwarf_value.emit ~asm_directives) (encode instr))
    (List.rev (End_sequence :: t.current.line_number_program))

(* Value of the header_length field in the header. This is the size of the
   header minus the first three fields (DWARF-4 standard section 6.2.4)*)
let header_length t =
  let ( + ) = Dwarf_int.add in
  Dwarf_value.size minimum_instruction_length
  + Dwarf_value.size maximum_operations_per_instruction
  + Dwarf_value.size default_is_stmt
  + Dwarf_value.size line_base
  + Dwarf_value.size line_range
  + Dwarf_value.size opcode_base
  + List.fold_left ( + ) (Dwarf_int.zero ())
      (List.map Dwarf_value.size standard_opcode_lengths)
  + Dwarf_value.size null_byte + file_names_size t

let size_without_first_word t =
  let ( + ) = Dwarf_int.add in
  let header_length = header_length t in
  Dwarf_version.size Dwarf_version.four
  + Dwarf_int.size header_length
  + header_length + line_number_program_size t

let size t =
  let size_without_first_word = size_without_first_word t in
  let initial_length = Initial_length.create size_without_first_word in
  Dwarf_int.add (Initial_length.size initial_length) size_without_first_word

let emit ~asm_directives t =
  let initial_length = Initial_length.create (size_without_first_word t) in
  Initial_length.emit ~asm_directives initial_length;
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
