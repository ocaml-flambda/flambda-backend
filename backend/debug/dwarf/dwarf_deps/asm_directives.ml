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

  (* Currently just ignore *)
  let new_line () = ()

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

  let int8 = with_comment (fun num -> D.byte (Int64.of_int (Int8.to_int num)))
  let int16 = with_comment (fun num -> D.word (Int64.of_int (Int16.to_int num)))
  let int32 = with_comment (fun num -> D.long (Int64.of_int32 num))
  let int64 = with_comment (fun num -> D.qword num)

  let uint8 = with_comment (fun num -> D.byte (Int64.of_int (Uint8.to_int num)))
  let uint16 = with_comment (fun num -> D.word (Int64.of_int (Uint16.to_int num)))
  let uint32 = with_comment (fun num -> D.long (Uint32.to_int64 num))
  let uint64 = with_comment (fun num -> D.qword (Uint64.to_int64 num))

  let targetint = with_comment (fun num -> 
      match Targetint_extra.repr num with
      | Int32 n -> D.long (Int64.of_int32 n)
      | Int64 n -> D.qword n
    )

  let uleb128 ?comment:_ _num = A.emit_line "uleb128"
  let sleb128 ?comment:_ _num = A.emit_line "sleb128"

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

  let define_data_symbol _sym = A.emit_line "define_data_symbol: XXX"

  let global _sym = A.emit_line "global: XXX"

  let symbol ?comment:_ _sym = A.emit_line "symbol"

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