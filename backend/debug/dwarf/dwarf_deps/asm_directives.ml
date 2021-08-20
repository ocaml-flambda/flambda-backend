[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = Asm_directives_intf.S
module type Arg = Asm_directives_intf.Arg

module Cached_string = struct
  type t = {
    section : Asm_section.t;
    str : string;
    comment : string option;
  }

  include Identifiable.Make (struct
    type nonrec t = t

    let compare { section = section1; str = str1; comment = comment1; }
          { section = section2; str = str2; comment = comment2; } =
      let c = Asm_section.compare section1 section2 in
      if c <> 0 then c
      else
        let c = String.compare str1 str2 in
        if c <> 0 then c
        else
          Option.compare String.compare comment1 comment2

    let equal t1 t2 =
      compare t1 t2 = 0

    let hash t = Hashtbl.hash t

    let print _ _ = Misc.fatal_error "Not yet implemented"
    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end

module Make ( A : Asm_directives_intf.Arg ) : Asm_directives_intf.S = struct

  module Int8 = Numbers_extra.Int8
  module Int16 = Numbers_extra.Int16

  module Uint8 = Numbers_extra.Uint8
  module Uint16 = Numbers_extra.Uint16
  module Uint32 = Numbers_extra.Uint32
  module Uint64 = Numbers_extra.Uint64

  module D = A.D

  let sections_seen = ref []

  let current_section_ref = ref None

  let cached_strings = ref Cached_string.Map.empty

  let temp_var_counter = ref 0

  let is_macos () = 
    match Config_typed.assembler () with
    | MASM | GAS_like -> false
    | MacOS -> true

  let if_not_masm f =
    match Config_typed.assembler () with
    | MASM -> ()
    | GAS_like 
    | MacOS -> f ()

  (* gas can silently emit corrupted line tables if a .file directive
     contains a number but an empty filename. *)
  let file ~file_num ~file_name =
    let file_name =
      if String.length file_name = 0 then "none"
      else file_name
    in
    if_not_masm (fun () -> D.file ~file_num ~file_name)
  
  let loc ~file_num ~line ~col =
    if_not_masm (fun () -> D.loc ~file_num ~line ~col ())

  let new_line () = D.new_line ()

  let not_initialized () =
    Misc.fatal_error "[Asm_directives.initialize] has not been called"

  let define_label label =
    let lbl_section = Asm_label.section label in
    let this_section =
      match !current_section_ref with
      | None -> not_initialized ()
      | Some this_section -> this_section
    in
    if not (Asm_section.equal lbl_section this_section) then begin
      Misc.fatal_errorf "Cannot define label %a intended for section %a \
          in section %a"
        Asm_label.print label
        Asm_section.print lbl_section
        Asm_section.print this_section
    end;
    (* CR-someday bkhajwal: Be aware of MASM label type annotation *)
    D.label (Asm_label.encode label)

  let switch_to_section section =
    let first_occurrence =
      if List.mem section !sections_seen then false
      else begin
        sections_seen := section::!sections_seen;
        true
      end
    in
    match !current_section_ref with
    | Some section' when Asm_section.equal section section' ->
      assert (not first_occurrence);
      ()
    | _ ->
      current_section_ref := Some section;
      let ({ names; flags; args; } : Asm_section.flags_for_section) =
        Asm_section.flags section ~first_occurrence
      in
      if not first_occurrence then begin
        new_line ()
      end;
      D.section names flags args;
      if first_occurrence then begin
        define_label (Asm_label.for_section section)
      end

  let initialize () =
    cached_strings := Cached_string.Map.empty;
    sections_seen := [];
    temp_var_counter := 0;
    (* Forward label references are illegal in GAS *)
    begin match Config_typed.assembler () with
    | MASM | MacOS -> ()
    | GAS_like -> List.iter switch_to_section (Asm_section.dwarf_sections_in_order ())
    end;
    (* Stop dsymutil complaining about empty __debug_line sections (produces
      bogus error "line table parameters mismatch") by making sure such sections
      are never empty. *)
    file ~file_num:1 ~file_name:"none";  (* also PR#7037 *)
    loc ~file_num:1 ~line:1 ~col:1;
    D.text ()

  let with_comment f ?comment x =
    Option.iter D.comment comment;
    f x

  let (>>) f g x = g (f x)
  
  let int8 = with_comment (Int8.to_int >> Int64.of_int >> D.const_int64 >> D.byte)
  let int16 = with_comment (Int16.to_int >> Int64.of_int >> D.const_int64 >> D.word)
  let int32 = with_comment (Int64.of_int32 >> D.const_int64 >> D.long)
  let int64 = with_comment (D.const_int64 >> D.qword)

  let uint8 = with_comment (Uint8.to_int >> Int64.of_int >> D.const_int64 >> D.byte)
  let uint16 = with_comment (Uint16.to_int >> Int64.of_int >> D.const_int64 >> D.word)
  let uint32 = with_comment (Uint32.to_int64 >> D.const_int64 >> D.long)
  let uint64 = with_comment (Uint64.to_int64 >> D.const_int64 >> D.qword)

  let targetint ?comment num = 
    match Targetint_extra.repr num with
    | Int32 n -> int32 ?comment n
    | Int64 n -> int64 ?comment n

  let uleb128 = with_comment (Uint64.to_int64 >> D.const_int64 >> D.uleb128)
  let sleb128 = with_comment (D.const_int64 >> D.sleb128)

  let string = with_comment (fun str -> D.bytes str)

  let cache_string ?comment section str =
    let cached : Cached_string.t = { section; str; comment; } in
    match Cached_string.Map.find cached !cached_strings with
    | label -> label
    | exception Not_found ->
      let label = Asm_label.create section in
      cached_strings := Cached_string.Map.add cached label !cached_strings;
      label

  let emit_cached_strings () =
    Cached_string.Map.iter (fun { section; str; comment; } label_name ->
        switch_to_section section;
        define_label label_name;
        string ?comment str;
        int8 Int8.zero)
      !cached_strings;
    cached_strings := Cached_string.Map.empty

  let comment str = D.comment str

  let define_data_symbol symbol =
    (* 
      CR-someday bkhajwal: Asm_symbol currently is just a string, if section-related
      information is added, should be worth doing this check.

      check_symbol_for_definition_in_current_section symbol;
    *)
    let symbol = Asm_symbol.encode symbol in
    let data_type = 
      match Machine_width.of_int_exn Sys.word_size with
      | Thirty_two -> D.DWORD
      | Sixty_four -> D.QWORD
    in
    D.label ~data_type symbol;
    begin match Config_typed.assembler (), Config_typed.is_windows () with
    | GAS_like, false -> D.type_ symbol "STT_OBJECT"
    | GAS_like, true
    | MacOS, _
    | MASM, _ -> ()
    end

  let global sym = D.global (Asm_symbol.encode sym)

  let const_machine_width const =
    match Machine_width.of_int_exn Sys.word_size with
    | Thirty_two -> D.long const
    | Sixty_four -> D.qword const

  let symbol = with_comment (fun sym ->
    let lab = D.const_label (Asm_symbol.encode sym) in
    const_machine_width lab
  )

  let label ?comment:_ _lab = A.emit_line "label"

  let symbol_plus_offset _sym ~offset_in_bytes:_ = A.emit_line "symbol_plus_offset"
  
  let between_symbols_in_current_unit ~upper ~lower =
    (* CR-someday bkhajwal: Add checks below from gdb-names-gpr
      check_symbol_in_current_unit upper;
      check_symbol_in_current_unit lower;
      check_symbols_in_same_section upper lower;
    *)
    let upper = D.const_label (Asm_symbol.encode upper) in
    let lower = D.const_label (Asm_symbol.encode lower) in
    (* CR-someday bkhajwal: Add `force_assembly_time_constant` *)
    const_machine_width (D.const_sub upper lower)
  
  let between_labels_16_bit ?comment:_ ~upper:_ ~lower:_ () = A.emit_line "between_labels_16_bit"
  let between_labels_32_bit ?comment:_ ~upper:_ ~lower:_ () = A.emit_line "between_labels_32_bit"
  let between_labels_64_bit ?comment:_ ~upper:_ ~lower:_ () = A.emit_line "between_labels_64_bit"

  let between_symbol_in_current_unit_and_label_offset 
  ?comment:_ ~upper:_ ~lower:_ ~offset_upper:_ () = A.emit_line "between_symbol_in_current_unit_and_label_offset"

  let new_temp_var () =
    let id = !temp_var_counter in
    incr temp_var_counter;
    Printf.sprintf "Ltemp%d" id

  let force_assembly_time_constant _section expr =
    if not (is_macos ()) then
      expr
    else begin
      (* This ensures the correct result is obtained on macOS.  (Apparently
        just writing expressions such as "L100 - L101" inline can cause
        unexpected results when one of the labels is on a section boundary,
        for example.) *)
      let temp = new_temp_var () in
      D.direct_assignment temp expr;
      (* TODO: Insert logic
      let compilation_unit = Compilation_unit.get_current_exn ()  in
      let sym =
        Asm_symbol.of_external_name_no_prefix section compilation_unit temp
      in
      Symbol sym  (* not really a symbol, but OK. *)
      *)
      Misc.fatal_error "not implemented"
    end

  let offset_into_dwarf_section_label ?comment section upper ~width = 
    let upper_section = Asm_label.section upper in
    let expected_section : Asm_section.t = DWARF section in
    if not (Asm_section.equal upper_section expected_section) then begin
      Misc.fatal_errorf "Label %a (in section %a) is not in section %a"
        Asm_label.print upper
        Asm_section.print upper_section
        Asm_section.print expected_section
    end;
    if !Clflags.keep_asm_file then begin
      let expected_section = Asm_section.to_string expected_section in
      match comment with
      | None ->
        D.comment (Format.asprintf "offset into %s" expected_section)
      | Some comment ->
        D.comment (Format.asprintf "%s (offset into %s)" comment expected_section)
    end;
    (* macOS does not use relocations in DWARF sections in places, such as
      here, where they might be expected.  Instead dsymutil and other tools
      parse DWARF sections properly and adjust offsets manually. *)
    let expr =
      if is_macos () then
        let lower = Asm_label.for_dwarf_section section in
        force_assembly_time_constant
          expected_section
          (D.const_sub
            (D.const_label (Asm_label.encode upper))
            (D.const_label (Asm_label.encode lower)))
      else
        D.const_label (Asm_label.encode upper)
    in
    match (width : Machine_width.t) with
    | Thirty_two -> D.long expr
    | Sixty_four -> D.qword expr
  
  let offset_into_dwarf_section_symbol
    ?comment:_
    _section
    _symbol
    ~width:_
    = A.emit_line "offset_into_dwarf_section_symbol"
end