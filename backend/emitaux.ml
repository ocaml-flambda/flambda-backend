(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Common functions for emitting assembly code *)

type error =
  | Stack_frame_too_large of int
  | Stack_frame_way_too_large of int
  | Inconsistent_probe_init of string * Debuginfo.t

exception Error of error

let output_channel = ref stdout

let emit_string s = output_string !output_channel s

let emit_int n = output_string !output_channel (Int.to_string n)

let emit_char c = output_char !output_channel c

let emit_nativeint n = output_string !output_channel (Nativeint.to_string n)

let emit_printf fmt =
  Printf.fprintf !output_channel fmt

let emit_int32 n = emit_printf "0x%lx" n

let emit_symbol esc s =
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    match c with
      'A'..'Z' | 'a'..'z' | '0'..'9' | '_' ->
        output_char !output_channel c
    | _ ->
        Printf.fprintf !output_channel "%c%02x" esc (Char.code c)
  done

let emit_string_literal s =
  let last_was_escape = ref false in
  emit_string "\"";
  for i = 0 to String.length s - 1 do
    let c = s.[i] in
    if c >= '0' && c <= '9' then
      if !last_was_escape
      then Printf.fprintf !output_channel "\\%o" (Char.code c)
      else output_char !output_channel c
    else if c >= ' ' && c <= '~' && c <> '"' (* '"' *) && c <> '\\' then begin
      output_char !output_channel c;
      last_was_escape := false
    end else begin
      Printf.fprintf !output_channel "\\%o" (Char.code c);
      last_was_escape := true
    end
  done;
  emit_string "\""

let emit_string_directive directive s =
  let l = String.length s in
  if l = 0 then ()
  else if l < 80 then begin
    emit_string directive;
    emit_string_literal s;
    emit_char '\n'
  end else begin
    let i = ref 0 in
    while !i < l do
      let n = min (l - !i) 80 in
      emit_string directive;
      emit_string_literal (String.sub s !i n);
      emit_char '\n';
      i := !i + n
    done
  end

let emit_bytes_directive directive s =
   let pos = ref 0 in
   for i = 0 to String.length s - 1 do
     if !pos = 0
     then emit_string directive
     else emit_char ',';
     emit_int(Char.code s.[i]);
     incr pos;
     if !pos >= 16 then begin emit_char '\n'; pos := 0 end
   done;
   if !pos > 0 then emit_char '\n'

let emit_float64_directive directive x =
  emit_printf "\t%s\t0x%Lx\n" directive x

let emit_float64_split_directive directive x =
  let lo = Int64.logand x 0xFFFF_FFFFL
  and hi = Int64.shift_right_logical x 32 in
  emit_printf "\t%s\t0x%Lx, 0x%Lx\n"
    directive
    (if Arch.big_endian then hi else lo)
    (if Arch.big_endian then lo else hi)

let emit_float32_directive directive x =
  emit_printf "\t%s\t0x%lx\n" directive x

(* Record live pointers at call points *)

type frame_debuginfo =
  | Dbg_alloc of Debuginfo.alloc_dbginfo
  | Dbg_raise of Debuginfo.t
  | Dbg_other of Debuginfo.t

type frame_descr =
  { fd_lbl: int;                        (* Return address *)
    fd_frame_size: int;                 (* Size of stack frame *)
    fd_live_offset: int list;           (* Offsets/regs of live addresses *)
    fd_debuginfo: frame_debuginfo;      (* Location, if any *)
    fd_long: bool;                      (* Use 32 instead of 16 bit format. *)
  }

let frame_descriptors = ref([] : frame_descr list)

let get_flags debuginfo =
  match debuginfo with
  | Dbg_other d | Dbg_raise d ->
    if Debuginfo.is_none d then 0 else 1
  | Dbg_alloc dbgs ->
    if !Clflags.debug &&
       List.exists (fun d ->
         not (Debuginfo.is_none d.Debuginfo.alloc_dbg)) dbgs
    then 3 else 2

let is_long n =
  assert (n >= 0);
  (* Long frames must fit in 32-bit integer and
     not truncated upon conversion from int on any target. *)
  if n > 0x3FFF_FFFF then
    raise (Error(Stack_frame_way_too_large n));
  n >= !Flambda_backend_flags.long_frames_threshold

let record_frame_descr ~label ~frame_size ~live_offset debuginfo =
  assert (frame_size land 3 = 0);
  let fd_long =
    is_long (frame_size + get_flags debuginfo) ||
    (* The checks below are redundant
       (if they fail, then frame size check above should have failed),
       but they make the safety of [emit_frame] clear. *)
    is_long (List.length live_offset) ||
    (List.exists is_long live_offset)
  in
  if fd_long && not !Flambda_backend_flags.allow_long_frames then
    raise (Error(Stack_frame_too_large frame_size));
  frame_descriptors :=     { fd_lbl = label;
      fd_frame_size = frame_size;
      fd_live_offset = List.sort_uniq (-) live_offset;
      fd_debuginfo = debuginfo;
      fd_long;
    } :: !frame_descriptors

type emit_frame_actions =
  { efa_code_label: int -> unit;
    efa_data_label: int -> unit;
    efa_8: int -> unit;
    efa_16: int -> unit;
    efa_32: int32 -> unit;
    efa_word: int -> unit;
    efa_align: int -> unit;
    efa_label_rel: int -> int32 -> unit;
    efa_def_label: int -> unit;
    efa_string: string -> unit }

let emit_frames a =
  let filenames = Hashtbl.create 7 in
  let label_filename name =
    try
      Hashtbl.find filenames name
    with Not_found ->
      let lbl = Cmm.new_label () in
      Hashtbl.add filenames name lbl;
      lbl
  in
  let defnames = Hashtbl.create 7 in
  let label_defname filename defname =
    try
      snd (Hashtbl.find defnames (filename, defname))
    with Not_found ->
      let file_lbl = label_filename filename in
      let def_lbl = Cmm.new_label () in
      Hashtbl.add defnames (filename, defname) (file_lbl, def_lbl);
      def_lbl
  in
  let module Label_table =
    Hashtbl.Make (struct
      type t = bool * Debuginfo.t

      let equal ((rs1 : bool), dbg1) (rs2, dbg2) =
        rs1 = rs2 && Debuginfo.compare dbg1 dbg2 = 0

      let hash (rs, dbg) =
        Hashtbl.hash (rs, Debuginfo.hash dbg)
    end)
  in
  let debuginfos = Label_table.create 7 in
  let label_debuginfos rs dbg =
    let rdbg = List.rev dbg in
    let key = (rs, rdbg) in
    try Label_table.find debuginfos key
    with Not_found ->
      let lbl = Cmm.new_label () in
      Label_table.add debuginfos key lbl;
      lbl
  in
  let emit_32 n = n |> Int32.of_int |> a.efa_32 in
  let emit_frame fd =
    let flags = get_flags fd.fd_debuginfo in
    a.efa_label_rel fd.fd_lbl 0l;
    (* For short format, the size is guaranteed
       to be less than the constant below. *)
    if fd.fd_long then begin
      a.efa_16 Flambda_backend_flags.max_long_frames_threshold;
      a.efa_align 4;
    end;
    let emit_16_or_32 =
      if fd.fd_long then emit_32 else a.efa_16
    in
    emit_16_or_32 (fd.fd_frame_size + flags);
    emit_16_or_32 (List.length fd.fd_live_offset);
    List.iter emit_16_or_32 fd.fd_live_offset;
    begin match fd.fd_debuginfo with
    | _ when flags = 0 ->
      ()
    | Dbg_other dbg ->
      a.efa_align 4;
      a.efa_label_rel (label_debuginfos false dbg) Int32.zero
    | Dbg_raise dbg ->
      a.efa_align 4;
      a.efa_label_rel (label_debuginfos true dbg) Int32.zero
    | Dbg_alloc dbg ->
      assert (List.length dbg < 256);
      a.efa_8 (List.length dbg);
      List.iter (fun Debuginfo.{alloc_words;_} ->
        (* Possible allocations range between 2 and 257 *)
        assert (2 <= alloc_words &&
                alloc_words - 1 <= Config.max_young_wosize &&
                Config.max_young_wosize <= 256);
        a.efa_8 (alloc_words - 2)) dbg;
      if flags = 3 then begin
        a.efa_align 4;
        List.iter (fun Debuginfo.{alloc_dbg; _} ->
          if Debuginfo.is_none alloc_dbg then
            a.efa_32 Int32.zero
          else
            a.efa_label_rel (label_debuginfos false alloc_dbg) Int32.zero) dbg
      end
    end;
    a.efa_align Arch.size_addr
  in
  let emit_filename name lbl =
    a.efa_def_label lbl;
    a.efa_string name
  in
  let emit_defname (_filename, defname) (file_lbl, lbl) =
    (* These must be 32-bit aligned, both because they contain a
       32-bit value, and because emit_debuginfo assumes the low 2 bits
       of their addresses are 0. *)
    a.efa_align 4;
    a.efa_def_label lbl;
    a.efa_label_rel file_lbl 0l;
    a.efa_string defname
  in
  let pack_info fd_raise d has_next =
    let line = min 0xFFFFF d.Debuginfo.dinfo_line
    and char_start = min 0xFF d.Debuginfo.dinfo_char_start
    and char_end = min 0x3FF d.Debuginfo.dinfo_char_end
    and kind = if fd_raise then 1 else 0
    and has_next = if has_next then 1 else 0 in
    Int64.(add (shift_left (of_int line) 44)
             (add (shift_left (of_int char_start) 36)
                (add (shift_left (of_int char_end) 26)
                   (add (shift_left (of_int kind) 1)
                      (of_int has_next)))))
  in
  let emit_debuginfo (rs, rdbg) lbl =
    (* Due to inlined functions, a single debuginfo may have multiple locations.
       These are represented sequentially in memory (innermost frame first),
       with the low bit of the packed debuginfo being 0 on the last entry. *)
    a.efa_align 4;
    a.efa_def_label lbl;
    let rec emit rs d rest =
      let open Debuginfo in
      let info = pack_info rs d (rest <> []) in
      let defname = Scoped_location.string_of_scopes d.dinfo_scopes in
      a.efa_label_rel
        (label_defname d.dinfo_file defname)
        (Int64.to_int32 info);
      a.efa_32 (Int64.to_int32 (Int64.shift_right info 32));
      match rest with
      | [] -> ()
      | d :: rest -> emit false d rest in
    match rdbg with
    | [] -> assert false
    | d :: rest -> emit rs d rest in
  a.efa_word (List.length !frame_descriptors);
  List.iter emit_frame !frame_descriptors;
  Label_table.iter emit_debuginfo debuginfos;
  Hashtbl.iter emit_filename filenames;
  Hashtbl.iter emit_defname defnames;
  a.efa_align Arch.size_addr;
  frame_descriptors := []

(* Detection of functions that can be duplicated between a DLL and
   the main program (PR#4690) *)

let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.sub s2 0 (String.length s1) = s1

let is_generic_function name =
  List.exists
    (fun p -> isprefix p name)
    ["caml_apply"; "caml_curry"; "caml_send"; "caml_tuplify"]

(* CFI directives *)

let is_cfi_enabled () =
  Config.asm_cfi_supported

let cfi_startproc () =
  if is_cfi_enabled () then
    emit_string "\t.cfi_startproc\n"

let cfi_endproc () =
  if is_cfi_enabled () then
    emit_string "\t.cfi_endproc\n"

let cfi_adjust_cfa_offset n =
  if is_cfi_enabled () then
  begin
    emit_string "\t.cfi_adjust_cfa_offset\t"; emit_int n; emit_string "\n";
  end

let cfi_offset ~reg ~offset =
  if is_cfi_enabled () then begin
    emit_string "\t.cfi_offset ";
    emit_int reg;
    emit_string ", ";
    emit_int offset;
    emit_string "\n"
  end

(* Emit debug information *)

(* This assoc list is expected to be very short *)
let file_pos_nums =
  (ref [] : (string * int) list ref)

(* Number of files *)
let file_pos_num_cnt = ref 1

(* Reset debug state at beginning of asm file *)
let reset_debug_info () =
  file_pos_nums := [];
  file_pos_num_cnt := 1

let get_file_num ~file_emitter file_name =
  try List.assoc file_name !file_pos_nums
  with Not_found ->
    let file_num = !file_pos_num_cnt in
    incr file_pos_num_cnt;
    file_emitter ~file_num ~file_name;
    file_pos_nums := (file_name,file_num) :: !file_pos_nums;
    file_num

(* We only display .file if the file has not been seen before. We
   display .loc for every instruction. *)
let emit_debug_info_gen ?discriminator dbg file_emitter loc_emitter =
  if is_cfi_enabled () &&
    (!Clflags.debug || Config.with_frame_pointers) then begin
    match List.rev dbg with
    | [] -> ()
    | { Debuginfo.dinfo_line = line;
        dinfo_char_start = col;
        dinfo_file = file_name; } :: _ ->
      if line > 0 then begin (* PR#6243 *)
        let file_num = get_file_num ~file_emitter file_name in
        loc_emitter ~file_num ~line ~col ?discriminator ()
      end
  end

let emit_debug_info ?discriminator dbg =
  emit_debug_info_gen dbg (fun ~file_num ~file_name ->
      emit_string "\t.file\t";
      emit_int file_num; emit_char '\t';
      emit_string_literal file_name; emit_char '\n';
    )
    (fun ~file_num ~line ~col:_ ?discriminator () ->
       emit_string "\t.loc\t";
       emit_int file_num; emit_char '\t';
       emit_int line;  emit_char '\t';
       begin match discriminator with
       | None -> ()
       | Some k -> emit_string "discriminator "; emit_int k
       end;
       emit_char '\n')

let reset () =
  reset_debug_info ();
  frame_descriptors := []

let binary_backend_available = ref false
let create_asm_file = ref true

let reduce_heap_size ~reset =
  let _minor, _promoted, major_words = Gc.counters () in
  (* Uses [major_words] because it doesn't require a heap traversal to compute and
     for this workload a majority of major words are live at this point. *)
  let heap_reduction_threshold =
    if !Flambda_backend_flags.heap_reduction_threshold >= 0 then
      float !Flambda_backend_flags.heap_reduction_threshold
    else
      Float.infinity
  in
  if major_words > heap_reduction_threshold then begin
    Profile.record_call "compact" (fun () ->
      reset ();
      Gc.compact ())
  end

module Dwarf_helpers = struct
  open Dwarf_ocaml

  let dwarf = ref None
  let sourcefile_for_dwarf = ref None

  let begin_dwarf ~build_asm_directives ~code_begin ~code_end ~file_emitter =
    match !sourcefile_for_dwarf with
    | None -> ()
    | Some sourcefile ->
      let asm_directives = build_asm_directives () in
      let (module Asm_directives : Asm_targets.Asm_directives_intf.S) = asm_directives in
      Asm_targets.Asm_label.initialize ~new_label:Cmm.new_label;
      Asm_directives.initialize ();
      let unit_name =
        (* CR lmaurer: This doesn't actually need to be an [Ident.t] *)
        Symbol.for_current_unit ()
        |> Symbol.linkage_name
        |> Linkage_name.to_string
        |> Ident.create_persistent
      in
      let code_begin = Asm_targets.Asm_symbol.create code_begin in
      let code_end = Asm_targets.Asm_symbol.create code_end in
      dwarf := Some (Dwarf.create
                       ~sourcefile
                       ~unit_name
                       ~asm_directives
                       ~get_file_id:(get_file_num ~file_emitter)
                       ~code_begin ~code_end)

  let reset_dwarf () =
    dwarf := None;
    sourcefile_for_dwarf := None

  let init ~disable_dwarf sourcefile =
    reset_dwarf ();
    let can_emit_dwarf =
      !Clflags.debug
      && not disable_dwarf
    in
    if !Flambda_backend_flags.internal_assembler then
      Dwarf_flags.set_debug_thing Dwarf_flags.Debug_source_lines;
    match can_emit_dwarf,
          Target_system.architecture (),
          Target_system.derived_system () with
    | true, X86_64, _ -> sourcefile_for_dwarf := Some sourcefile
    | true, _, _
    | false, _, _ -> ()

  let emit_dwarf () = Option.iter Dwarf_ocaml.Dwarf.emit !dwarf

  let record_dwarf_for_fundecl ~fun_name fun_dbg =
    match !dwarf with
    | None -> None
    | Some dwarf ->
      let label = Cmm.new_label () in
      let fun_end_label =
        Asm_targets.Asm_label.create_int Text label
      in
      let fundecl : Dwarf_concrete_instances.fundecl =
        { fun_name;
          fun_dbg;
          fun_end_label;
        }
      in
      Dwarf.dwarf_for_fundecl dwarf fundecl;
      Some label

  let record_dwarf_for_source_file ~file_name ~file_num =
    match !dwarf with
    | None -> ()
    | Some dwarf -> Dwarf.dwarf_for_source_file dwarf ~file_name ~file_num

  let record_dwarf_for_line_number_matrix_row ~instr_address ~file_num ~line ~col
      ~discriminator =
    match !dwarf with
    | None -> ()
    | Some dwarf -> Dwarf.dwarf_for_line_number_matrix_row dwarf ~instr_address
      ~file_num ~line ~col ~discriminator

  let debug_line_checkpoint () =
    match !dwarf with
    | None -> ()
    | Some dwarf -> Dwarf.debug_line_checkpoint dwarf

  let debug_line_rollback () =
    match !dwarf with
    | None -> ()
    | Some dwarf -> Dwarf.debug_line_rollback dwarf
end

let report_error ppf = function
  | Stack_frame_too_large n ->
    Format.fprintf ppf "stack frame too large (%d bytes). \n\
                        Use -long-frames compiler flag." n
  | Stack_frame_way_too_large n ->
    Format.fprintf ppf "stack frame too large (%d bytes)." n
  | Inconsistent_probe_init (name, dbg) ->
    Format.fprintf ppf "Inconsistent use of ~enabled_at_init in [%%probe %s ..] at %a"
      name Debuginfo.print_compact dbg

