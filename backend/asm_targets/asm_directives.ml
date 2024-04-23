(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2014-2022 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type S = Asm_directives_intf.S

module type Arg = Asm_directives_intf.Arg

module Cached_string = struct
  type t =
    { section : Asm_section.t;
      str : string;
      comment : string option
    }

  include Identifiable.Make (struct
    type nonrec t = t

    let compare { section = section1; str = str1; comment = comment1 }
        { section = section2; str = str2; comment = comment2 } =
      let c = Asm_section.compare section1 section2 in
      if c <> 0
      then c
      else
        let c = String.compare str1 str2 in
        if c <> 0 then c else Option.compare String.compare comment1 comment2

    let equal t1 t2 = compare t1 t2 = 0

    let hash t = Hashtbl.hash t

    let print _ _ = Misc.fatal_error "Not yet implemented"

    let output _ _ = Misc.fatal_error "Not yet implemented"
  end)
end

module Make (A : Asm_directives_intf.Arg) : Asm_directives_intf.S = struct
  module Int8 = Numbers.Int8
  module Int16 = Numbers.Int16
  module Uint8 = Numbers.Uint8
  module Uint16 = Numbers.Uint16
  module Uint32 = Numbers.Uint32
  module Uint64 = Numbers.Uint64
  module D = A.D

  let sections_seen = ref []

  let current_dwarf_section_ref = ref None

  let cached_strings = ref Cached_string.Map.empty

  let temp_var_counter = ref 0

  let is_macos () =
    match Target_system.assembler () with
    | MASM | GAS_like -> false
    | MacOS -> true

  let is_gas () =
    match Target_system.assembler () with
    | MASM | MacOS -> false
    | GAS_like -> true

  let is_masm () =
    match Target_system.assembler () with
    | MASM -> true
    | GAS_like | MacOS -> false

  let loc ~file_num ~line ~col =
    if not (is_masm ()) then D.loc ~file_num ~line ~col ()

  let new_line () = D.new_line ()

  let not_initialized () =
    Misc.fatal_error "[Asm_directives.initialize] has not been called"

  let define_label label =
    let lbl_section = Asm_label.section label in
    let this_section =
      match !current_dwarf_section_ref with
      | None -> not_initialized ()
      | Some this_section -> this_section
    in
    if not (Asm_section.equal lbl_section this_section)
    then
      Misc.fatal_errorf
        "Cannot define label %a intended for section %a in section %a"
        Asm_label.print label Asm_section.print lbl_section Asm_section.print
        this_section;
    (* CR-someday bkhajwal: Be aware of MASM label type annotation *)
    (* CR poechsel: This assumes that no [D.text] or [D.section] were emitted
       since the last call to [switch_to_section]. If we emit dwarf separately
       from other assembly everything will be fine, otherwise this might
       break. *)
    D.label (Asm_label.encode label)

  let switch_to_section section =
    let first_occurrence =
      if List.mem section !sections_seen
      then false
      else (
        sections_seen := section :: !sections_seen;
        true)
    in
    match !current_dwarf_section_ref with
    | Some section' when Asm_section.equal section section' ->
      assert (not first_occurrence);
      ()
    | _ ->
      current_dwarf_section_ref := Some section;
      let ({ names; flags; args } : Asm_section.section_details) =
        Asm_section.details section ~first_occurrence
      in
      if not first_occurrence then new_line ();
      D.section ~delayed:(Asm_section.is_delayed section) names flags args;
      if first_occurrence then define_label (Asm_label.for_section section)

  let initialize () =
    cached_strings := Cached_string.Map.empty;
    sections_seen := [];
    temp_var_counter := 0;
    current_dwarf_section_ref := None;
    (* Forward label references are illegal on some assemblers/platforms. To
       avoid errors, emit the beginning of all dwarf sections in advance. *)
    if is_gas () || is_macos ()
    then List.iter switch_to_section (Asm_section.dwarf_sections_in_order ());
    (* Stop dsymutil complaining about empty __debug_line sections (produces
       bogus error "line table parameters mismatch") by making sure such
       sections are never empty. *)
    let file_num = A.get_file_num "none" in
    loc ~file_num ~line:1 ~col:1;
    D.text ()

  let with_comment f ?comment x =
    if A.debugging_comments_in_asm_files then Option.iter D.comment comment;
    f x

  let ( >> ) f g x = g (f x)

  let int8 =
    with_comment (Int8.to_int >> Int64.of_int >> D.const_int64 >> D.byte)

  let int16 =
    with_comment (Int16.to_int >> Int64.of_int >> D.const_int64 >> D.word)

  let int32 = with_comment (Int64.of_int32 >> D.const_int64 >> D.long)

  let int64 = with_comment (D.const_int64 >> D.qword)

  let uint8 =
    with_comment (Uint8.to_int >> Int64.of_int >> D.const_int64 >> D.byte)

  let uint16 =
    with_comment (Uint16.to_int >> Int64.of_int >> D.const_int64 >> D.word)

  let uint32 = with_comment (Uint32.to_int64 >> D.const_int64 >> D.long)

  let uint64 = with_comment (Uint64.to_int64 >> D.const_int64 >> D.qword)

  let targetint ?comment num =
    match Targetint.repr num with
    | Int32 n -> int32 ?comment n
    | Int64 n -> int64 ?comment n

  let uleb128 = with_comment (Uint64.to_int64 >> D.const_int64 >> D.uleb128)

  let sleb128 = with_comment (D.const_int64 >> D.sleb128)

  let assert_string_has_no_null_bytes s =
    assert (not (String.contains s '\x00'))

  let string =
    with_comment (fun str ->
        assert_string_has_no_null_bytes str;
        D.bytes str)

  let cache_string ?comment section str =
    assert_string_has_no_null_bytes str;
    let comment = if A.debugging_comments_in_asm_files then comment else None in
    let cached : Cached_string.t = { section; str; comment } in
    match Cached_string.Map.find cached !cached_strings with
    | label -> label
    | exception Not_found ->
      let label = Asm_label.create section in
      cached_strings := Cached_string.Map.add cached label !cached_strings;
      label

  let emit_cached_strings () =
    let old_dwarf_section = !current_dwarf_section_ref in
    Cached_string.Map.iter
      (fun { section; str; comment } label_name ->
        switch_to_section section;
        define_label label_name;
        string ?comment str;
        int8 Int8.zero)
      !cached_strings;
    cached_strings := Cached_string.Map.empty;
    Option.iter switch_to_section old_dwarf_section

  let comment str = if A.debugging_comments_in_asm_files then D.comment str

  let define_data_symbol symbol =
    (* CR-someday bkhajwal: Asm_symbol currently is just a string, if
       section-related information is added, should be worth doing this check.

       check_symbol_for_definition_in_current_section symbol; *)
    let symbol = Asm_symbol.encode symbol in
    let data_type = D.QWORD in
    D.label ~data_type symbol;
    match Target_system.assembler (), Target_system.is_windows () with
    | GAS_like, false -> D.type_ symbol "STT_OBJECT"
    | GAS_like, true | MacOS, _ | MASM, _ -> ()

  let global sym = D.global (Asm_symbol.encode sym)

  let protected sym = D.protected (Asm_symbol.encode sym)

  let const_machine_width const = D.qword const

  let symbol =
    with_comment (fun sym ->
        let lab = D.const_label (Asm_symbol.encode sym) in
        const_machine_width lab)

  let label ?comment:comment' lab =
    Option.iter D.comment comment';
    let lab = D.const_label (Asm_label.encode lab) in
    const_machine_width lab

  let label_plus_offset ?comment:comment' lab ~offset_in_bytes =
    let offset_in_bytes = Targetint.to_int64 offset_in_bytes in
    Option.iter D.comment comment';
    let lab = D.const_label (Asm_label.encode lab) in
    const_machine_width (D.const_add lab (D.const_int64 offset_in_bytes))

  let symbol_plus_offset symbol ~offset_in_bytes =
    let offset_in_bytes = Targetint.to_int64 offset_in_bytes in
    const_machine_width
      (D.const_add
         (D.const_label (Asm_symbol.encode symbol))
         (D.const_int64 offset_in_bytes))

  let new_temp_var () =
    let id = !temp_var_counter in
    incr temp_var_counter;
    Printf.sprintf "temp%d" id

  let force_assembly_time_constant expr =
    if not (is_macos ())
    then expr
    else
      (* This ensures the correct result is obtained on macOS. (Apparently just
         writing expressions such as "L100 - L101" inline can cause unexpected
         results when one of the labels is on a section boundary, for
         example.) *)
      let temp = new_temp_var () in
      D.direct_assignment temp expr;
      D.const_label temp

  let between_symbols_in_current_unit ~upper ~lower =
    (* CR-someday bkhajwal: Add checks below from gdb-names-gpr
       check_symbol_in_current_unit upper; check_symbol_in_current_unit lower;
       check_symbols_in_same_section upper lower; *)
    let upper = D.const_label (Asm_symbol.encode upper) in
    let lower = D.const_label (Asm_symbol.encode lower) in
    let expr = D.const_sub upper lower in
    if is_macos ()
    then const_machine_width (force_assembly_time_constant expr)
    else const_machine_width expr

  let between_labels_16_bit ?comment:_ ~upper:_ ~lower:_ () =
    (* CR poechsel: use the arguments *)
    A.emit_line "between_labels_16_bit"

  let between_labels_32_bit ?comment:_ ~upper:_ ~lower:_ () =
    (* CR poechsel: use the arguments *)
    A.emit_line "between_labels_32_bit"

  let between_labels_64_bit ?comment:_ ~upper:_ ~lower:_ () =
    (* CR poechsel: use the arguments *)
    A.emit_line "between_labels_64_bit"

  let between_labels_64_bit_with_offsets ?comment:comment' ~upper ~upper_offset
      ~lower ~lower_offset () =
    Option.iter D.comment comment';
    let upper_offset = Targetint.to_int64 upper_offset in
    let lower_offset = Targetint.to_int64 lower_offset in
    let expr =
      D.const_sub
        (D.const_add
           (D.const_label (Asm_label.encode upper))
           (D.const_int64 upper_offset))
        (D.const_add
           (D.const_label (Asm_label.encode lower))
           (D.const_int64 lower_offset))
    in
    const_machine_width (force_assembly_time_constant expr)

  let between_symbol_in_current_unit_and_label_offset ?comment:comment' ~upper
      ~lower ~offset_upper () =
    (* CR mshinwell: add checks, as above: check_symbol_in_current_unit lower;
       check_symbol_and_label_in_same_section lower upper; *)
    Option.iter D.comment comment';
    if Targetint.compare offset_upper Targetint.zero = 0
    then
      let expr =
        D.const_sub
          (D.const_label (Asm_label.encode upper))
          (D.const_label (Asm_symbol.encode lower))
      in
      const_machine_width (force_assembly_time_constant expr)
    else
      let offset_upper = Targetint.to_int64 offset_upper in
      let expr =
        D.const_sub
          (D.const_add
             (D.const_label (Asm_label.encode upper))
             (D.const_int64 offset_upper))
          (D.const_label (Asm_symbol.encode lower))
      in
      const_machine_width (force_assembly_time_constant expr)

  let const ~width constant =
    match width with
    | Dwarf_flags.Thirty_two -> D.long constant
    | Dwarf_flags.Sixty_four -> D.qword constant

  let offset_into_dwarf_section_label ?comment ~width section upper =
    let upper_section = Asm_label.section upper in
    let expected_section : Asm_section.t = DWARF section in
    if not (Asm_section.equal upper_section expected_section)
    then
      Misc.fatal_errorf "Label %a (in section %a) is not in section %a"
        Asm_label.print upper Asm_section.print upper_section Asm_section.print
        expected_section;
    (if !Clflags.keep_asm_file
    then
      let expected_section = Asm_section.to_string expected_section in
      match comment with
      | None -> D.comment (Format.asprintf "offset into %s" expected_section)
      | Some comment ->
        D.comment
          (Format.asprintf "%s (offset into %s)" comment expected_section));
    (* macOS does not use relocations in DWARF sections in places, such as here,
       where they might be expected. Instead dsymutil and other tools parse
       DWARF sections properly and adjust offsets manually. *)
    let expr =
      if is_macos ()
      then
        let lower = Asm_label.for_dwarf_section section in
        if Asm_label.equal lower upper
        then D.const_int64 0L
        else
          force_assembly_time_constant
            (D.const_sub
               (D.const_label (Asm_label.encode upper))
               (D.const_label (Asm_label.encode lower)))
      else D.const_label (Asm_label.encode upper)
    in
    const ~width expr

  let offset_into_dwarf_section_symbol ?comment
      ~(width : Dwarf_flags.dwarf_format) section upper =
    (* CR mshinwell: code from previous DWARF work:

       let upper_section = Asm_symbol.section upper in if not (Asm_section.equal
       upper_section (DWARF section)) then Misc.fatal_errorf "Symbol %a (in
       section %a) not in section %a" Asm_symbol.print upper Asm_section.print
       upper_section Asm_section.print (Asm_section.DWARF section); *)
    (* The macOS assembler doesn't seem to allow "distance to undefined symbol
       from start of given section". As such we do not allow this function to be
       used for undefined symbols on macOS at the moment. Relevant link:
       <https://gcc.gnu.org/bugzilla/show_bug.cgi?id=82005>. *)
    (* CR mshinwell: try again to make this work on macOS, maybe using
     * something like the last .quad in the example below:
     *
     * extunit.s
     *
     *   .section __DWARF,__debug_info,regular,debug
     *   .quad 0x12345678
     *   .globl extunit_die
     * extunit_die:
     *   .quad 0xffffdddd
     *
     * ---
     *
     * unit.s
     *
     *   .section __DWARF,__debug_info,regular,debug
     * Ldebug_info:
     *   .quad 0x11112222
     *   .globl unit_die
     * unit_die1:
     *   .quad 0x9999888
     * unit_die:
     *   .quad 0xaaaabbbb
     *   .quad unit_die - __debug_info
     *   .set dist, unit_die - Ldebug_info
     *   .quad dist
     *   .quad extunit_die - __debug_info
     *)
    let comment =
      if not !Clflags.keep_asm_file
      then None
      else
        match comment with
        | None ->
          Some
            (Format.asprintf "offset into %s"
               (Asm_section.to_string (DWARF section)))
        | Some comment ->
          Some
            (Format.asprintf "%s (offset into %s)" comment
               (Asm_section.to_string (DWARF section)))
    in
    Option.iter D.comment comment;
    let expr =
      if is_macos ()
      then
        let in_current_unit =
          true
          (* CR mshinwell: old code was:

             Compilation_unit.equal (Compilation_unit.get_current_exn ())
             (Asm_symbol.compilation_unit upper) *)
        in
        if in_current_unit
        then
          let lower = Asm_label.for_dwarf_section section in
          (* Same note as in [offset_into_dwarf_section_label] applies here. *)
          force_assembly_time_constant
            (D.const_sub
               (D.const_label (Asm_symbol.encode upper))
               (D.const_label (Asm_label.encode lower)))
        else
          Misc.fatal_errorf
            "Don't know how to encode offset from start of section XXX to \
             undefined symbol %a on macOS (current compilation unit %a, symbol \
             in compilation unit XXX)"
            (* Asm_section.print upper_section *) Asm_symbol.print upper
            Compilation_unit.print
            (Compilation_unit.get_current_exn ())
            Compilation_unit.print
        (* (Asm_symbol.compilation_unit upper) *)
      else D.const_label (Asm_symbol.encode upper)
    in
    match width with Thirty_two -> D.long expr | Sixty_four -> D.qword expr
end
