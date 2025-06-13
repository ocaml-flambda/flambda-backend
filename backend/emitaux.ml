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

[@@@ocaml.warning "+a-40-41-42"]

(* Common functions for emitting assembly code *)

open! Int_replace_polymorphic_compare

type error =
  | Stack_frame_too_large of int
  | Stack_frame_way_too_large of int
  | Inconsistent_probe_init of string * Debuginfo.t

exception Error of error

let output_channel = ref stdout

let emit_string s = output_string !output_channel s

let emit_buffer b = Buffer.output_buffer !output_channel b

(* Record live pointers at call points *)

type frame_debuginfo =
  | Dbg_alloc of Cmm.alloc_dbginfo
  | Dbg_raise of Debuginfo.t
  | Dbg_other of Debuginfo.t

type frame_descr =
  { fd_lbl : Label.t; (* Return address *)
    fd_frame_size : int; (* Size of stack frame *)
    fd_live_offset : int list; (* Offsets/regs of live addresses *)
    fd_debuginfo : frame_debuginfo; (* Location, if any *)
    fd_long : bool (* Use 32 instead of 16 bit format. *)
  }

let frame_descriptors = ref ([] : frame_descr list)

let is_none_dbg d = Debuginfo.Dbg.is_none (Debuginfo.get_dbg d)

let get_flags debuginfo =
  match debuginfo with
  | Dbg_other d | Dbg_raise d -> if is_none_dbg d then 0 else 1
  | Dbg_alloc dbgs ->
    if !Clflags.debug
       && List.exists (fun d -> not (is_none_dbg d.Cmm.alloc_dbg)) dbgs
    then 3
    else 2

let is_long n =
  assert (n >= 0);
  (* Long frames must fit in 32-bit integer and not truncated upon conversion
     from int on any target. *)
  if n > 0x3FFF_FFFF then raise (Error (Stack_frame_way_too_large n));
  n >= !Oxcaml_flags.long_frames_threshold

let is_long_stack_index n =
  let is_reg n = n land 1 = 1 in
  (* allows negative reg offsets in runtime4 *)
  if is_reg n && not Config.runtime5 then false else is_long n

let record_frame_descr ~label ~frame_size ~live_offset debuginfo =
  assert (frame_size land 3 = 0);
  let fd_long =
    is_long (frame_size + get_flags debuginfo)
    (* The checks below are redundant (if they fail, then frame size check above
       should have failed), but they make the safety of [emit_frame] clear. *)
    || is_long (List.length live_offset)
    || List.exists is_long_stack_index live_offset
  in
  if fd_long && not !Oxcaml_flags.allow_long_frames
  then raise (Error (Stack_frame_too_large frame_size));
  frame_descriptors
    := { fd_lbl = label;
         fd_frame_size = frame_size;
         fd_live_offset = List.sort_uniq ( - ) live_offset;
         fd_debuginfo = debuginfo;
         fd_long
       }
       :: !frame_descriptors

type emit_frame_actions =
  { efa_code_label : Label.t -> unit;
    efa_data_label : Label.t -> unit;
    efa_i8 : Numbers.Int8.t -> unit;
    efa_i16 : Numbers.Int16.t -> unit;
    efa_i32 : Int32.t -> unit;
    efa_u8 : Numbers.Uint8.t -> unit;
    efa_u16 : Numbers.Uint16.t -> unit;
    efa_u32 : Numbers.Uint32.t -> unit;
    efa_word : int -> unit;
    efa_align : int -> unit;
    efa_label_rel : Label.t -> int32 -> unit;
    efa_def_label : Label.t -> unit;
    efa_string : string -> unit
  }

let emit_frames a =
  (* The emit functions below perform bounds checks for the corresponding ranges
     via the conversion functions that raise exceptions. [int32] does not have
     such a function so we perform the check manually here. *)
  let emit_u8 n =
    let n = Numbers.Uint8.of_nonnegative_int_exn n in
    a.efa_u8 n
  in
  let[@warning "-26"] emit_i8 n =
    (* unused, but here for completeness *)
    let n = Numbers.Int8.of_int_exn n in
    a.efa_i8 n
  in
  let emit_i16 n =
    let n = Numbers.Int16.of_int_exn n in
    a.efa_i16 n
  in
  let emit_u16 n =
    let n = Numbers.Uint16.of_nonnegative_int_exn n in
    a.efa_u16 n
  in
  let emit_i32 n =
    let min_i32 = Int64.neg (Int64.shift_left 1L 31) (* -0x8000_0000 *)
    and max_i32 = Int64.sub (Int64.shift_left 1L 31) 1L (* 0x7fff_ffff *)
    and n_64 = Int64.of_int n in
    if Int64.compare n_64 min_i32 < 0 || Int64.compare n_64 max_i32 > 0
    then
      Misc.fatal_errorf
        "attempting to emit signed 32-bit integer %d out of range" n
    else a.efa_i32 (Int32.of_int n)
  in
  let emit_u32 n =
    let n = Numbers.Uint32.of_nonnegative_int_exn n in
    a.efa_u32 n
  in
  let filenames = Hashtbl.create 7 in
  let label_filename name =
    try Hashtbl.find filenames name
    with Not_found ->
      let lbl = Cmm.new_label () in
      Hashtbl.add filenames name lbl;
      lbl
  in
  let defnames = Hashtbl.create 7 in
  let label_defname filename defname loc =
    try snd (Hashtbl.find defnames (filename, defname, loc))
    with Not_found ->
      let file_lbl = label_filename filename in
      let def_lbl = Cmm.new_label () in
      Hashtbl.add defnames (filename, defname, loc) (file_lbl, def_lbl);
      def_lbl
  in
  let module Label_table = Hashtbl.Make (struct
    type t = bool * Debuginfo.Dbg.t

    let equal ((rs1 : bool), dbg1) (rs2, dbg2) =
      Bool.equal rs1 rs2 && Debuginfo.Dbg.compare dbg1 dbg2 = 0

    let hash (rs, dbg) = Hashtbl.hash (rs, Debuginfo.Dbg.hash dbg)
  end) in
  let debuginfos = Label_table.create 7 in
  let label_debuginfos rs dbg =
    let dbg = Debuginfo.get_dbg dbg in
    let key = rs, dbg in
    try Label_table.find debuginfos key
    with Not_found ->
      let lbl = Cmm.new_label () in
      Label_table.add debuginfos key lbl;
      lbl
  in
  let emit_frame fd =
    let flags = get_flags fd.fd_debuginfo in
    a.efa_label_rel fd.fd_lbl 0l;
    (* For short format, the size is guaranteed to be less than the constant
       below. *)
    if fd.fd_long
    then (
      emit_u16 Oxcaml_flags.max_long_frames_threshold;
      a.efa_align 4);
    let emit_signed_16_or_32 = if fd.fd_long then emit_i32 else emit_i16 in
    let emit_unsigned_16_or_32 = if fd.fd_long then emit_u32 else emit_u16 in
    let emit_live_offset n =
      (* On runtime 4, the live offsets can be negative. As such, we emit them
         as signed integers (and truncate the upper bound to 0x7f...ff); on
         runtime 5 they are always unsigned. *)
      if Config.runtime5
      then emit_unsigned_16_or_32 n
      else emit_signed_16_or_32 n
    in
    emit_unsigned_16_or_32 (fd.fd_frame_size + flags);
    emit_unsigned_16_or_32 (List.length fd.fd_live_offset);
    List.iter emit_live_offset fd.fd_live_offset;
    (match fd.fd_debuginfo with
    | _ when flags = 0 -> ()
    | Dbg_other dbg ->
      a.efa_align 4;
      a.efa_label_rel (label_debuginfos false dbg) Int32.zero
    | Dbg_raise dbg ->
      a.efa_align 4;
      a.efa_label_rel (label_debuginfos true dbg) Int32.zero
    | Dbg_alloc dbg ->
      assert (List.length dbg < 256);
      emit_u8 (List.length dbg);
      List.iter
        (fun Cmm.{ alloc_words; _ } ->
          (* Possible allocations range between 2 and 257 *)
          assert (
            2 <= alloc_words
            && alloc_words - 1 <= Config.max_young_wosize
            && Config.max_young_wosize <= 256);
          emit_u8 (alloc_words - 2))
        dbg;
      if flags = 3
      then (
        a.efa_align 4;
        List.iter
          (fun Cmm.{ alloc_dbg; _ } ->
            if is_none_dbg alloc_dbg
            then emit_i32 0
            else a.efa_label_rel (label_debuginfos false alloc_dbg) Int32.zero)
          dbg));
    a.efa_align Arch.size_addr
  in
  let emit_filename name lbl =
    a.efa_def_label lbl;
    a.efa_string name
  in
  let emit_defname (_filename, defname, loc) (file_lbl, lbl) =
    let emit_loc (start_chr, end_chr, end_offset) =
      emit_u16 start_chr;
      emit_u16 end_chr;
      emit_i32 end_offset
    in
    (* These must be 32-bit aligned, both because they contain a 32-bit value,
       and because emit_debuginfo assumes the low 2 bits of their addresses are
       0. *)
    a.efa_align 4;
    a.efa_def_label lbl;
    a.efa_label_rel file_lbl 0l;
    (* Include the additional 64-bits of location information which didn't pack
       in the main 64-bit word *)
    Option.iter emit_loc loc;
    a.efa_string defname
  in
  let fully_pack_info fd_raise d has_next =
    (* See format in caml_debuginfo_location in runtime/backtrace-nat.c *)
    let open Debuginfo in
    let kind = if fd_raise then 1 else 0
    and has_next = if has_next then 1 else 0
    and char_end = d.dinfo_char_end + d.dinfo_start_bol - d.dinfo_end_bol in
    let char_end_offset = d.dinfo_end_bol - d.dinfo_start_bol in
    Int64.(
      add
        (shift_left (of_int d.dinfo_line) 51)
        (add
           (shift_left (of_int (d.dinfo_end_line - d.dinfo_line)) 48)
           (add
              (shift_left (of_int d.dinfo_char_start) 42)
              (add
                 (shift_left (of_int char_end) 35)
                 (add
                    (shift_left (of_int char_end_offset) 26)
                    (add (shift_left (of_int kind) 1) (of_int has_next)))))))
  in
  let partially_pack_info fd_raise d has_next =
    (* Partially packed debuginfo: 1lllllllllmmmmmmmmddddddddddddkn 1 - d points
       to a name_and_loc_info struct l (19 bits) - start line number m (18 bits)
       - offset of end line number from start d (24 bits) - memory offset to
       name_and_loc_info struct k (1 bit) - fd_raise flag n (1 bit) - has_next
       flag *)
    let open Debuginfo in
    let start_line = Int.min 0x7FFFF d.dinfo_line
    and end_line = Int.min 0x3FFFF (d.dinfo_end_line - d.dinfo_line)
    and kind = if fd_raise then 1 else 0
    and has_next = if has_next then 1 else 0 in
    Int64.(
      add (shift_left Int64.one 63)
        (add
           (shift_left (of_int start_line) 44)
           (add
              (shift_left (of_int end_line) 26)
              (add (shift_left (of_int kind) 1) (of_int has_next)))))
  in
  let emit_debuginfo (rs, dbg) lbl =
    let rdbg = dbg |> Debuginfo.Dbg.to_list |> List.rev in
    (* Due to inlined functions, a single debuginfo may have multiple locations.
       These are represented sequentially in memory (innermost frame first),
       with the low bit of the packed debuginfo being 0 on the last entry. *)
    a.efa_align 4;
    a.efa_def_label lbl;
    let rec emit rs d rest =
      let open Debuginfo in
      let defname =
        Scoped_location.string_of_scopes ~include_zero_alloc:false
          d.dinfo_scopes
      in
      let char_end = d.dinfo_char_end + d.dinfo_start_bol - d.dinfo_end_bol in
      let is_fully_packable =
        d.dinfo_line <= 0xFFF
        && d.dinfo_end_line - d.dinfo_line <= 0x7
        && d.dinfo_char_start <= 0x3F && char_end <= 0x7F
        && d.dinfo_end_bol - d.dinfo_start_bol <= 0x1FF
      in
      let info =
        if is_fully_packable
        then fully_pack_info rs d (not (Misc.Stdlib.List.is_empty rest))
        else partially_pack_info rs d (not (Misc.Stdlib.List.is_empty rest))
      in
      let loc =
        if is_fully_packable
        then None
        else
          Some
            ( Int.min 0xFFFF d.dinfo_char_start,
              (* start_chr *)
              Int.min 0xFFFF char_end,
              (* end_chr *)
              Int.min 0x3FFFFFFF d.dinfo_char_end )
        (* end_offset *)
      in
      a.efa_label_rel
        (label_defname d.dinfo_file defname loc)
        (Int64.to_int32 info);
      (* We use [efa_i32] directly here instead of [emit_i32] to avoid a
         round-trip via [int], which would break on 32-bit platforms. The right
         shift ensures that the integer is in range of [int32]. *)
      a.efa_i32 (Int64.to_int32 (Int64.shift_right info 32));
      match rest with [] -> () | d :: rest -> emit false d rest
    in
    match rdbg with [] -> assert false | d :: rest -> emit rs d rest
  in
  a.efa_word (List.length !frame_descriptors);
  List.iter emit_frame !frame_descriptors;
  Label_table.iter emit_debuginfo debuginfos;
  Hashtbl.iter emit_filename filenames;
  Hashtbl.iter emit_defname defnames;
  a.efa_align Arch.size_addr;
  frame_descriptors := []

(* Detection of functions that can be duplicated between a DLL and the main
   program (PR#4690) *)

let isprefix s1 s2 =
  String.length s1 <= String.length s2
  && String.equal (String.sub s2 0 (String.length s1)) s1

let is_generic_function name =
  List.exists
    (fun p -> isprefix p name)
    ["caml_apply"; "caml_curry"; "caml_send"; "caml_tuplify"]

(* CFI directives *)

let is_cfi_enabled () = Config.asm_cfi_supported

(* Emit debug information *)

(* This assoc list is expected to be very short *)
let file_pos_nums = (ref [] : (string * int) list ref)

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
    file_pos_nums := (file_name, file_num) :: !file_pos_nums;
    file_num

(* We only display .file if the file has not been seen before. We display .loc
   for every instruction. *)
let emit_debug_info_gen ?discriminator dbg file_emitter loc_emitter =
  let dbg = Debuginfo.Dbg.to_list (Debuginfo.get_dbg dbg) in
  if is_cfi_enabled () && (!Clflags.debug || Config.with_frame_pointers)
  then
    match List.rev dbg with
    | [] -> ()
    | { Debuginfo.dinfo_line = line;
        dinfo_char_start = col;
        dinfo_file = file_name;
        _
      }
      :: _ ->
      if line > 0
      then
        (* PR#6243 *)
        let file_num = get_file_num ~file_emitter file_name in
        loc_emitter ~file_num ~line ~col ?discriminator ()

let reset () =
  reset_debug_info ();
  frame_descriptors := []

let binary_backend_available = ref false

let reduce_heap_size ~reset =
  let _minor, _promoted, major_words = Gc.counters () in
  (* Uses [major_words] because it doesn't require a heap traversal to compute
     and for this workload a majority of major words are live at this point. *)
  let heap_reduction_threshold =
    if !Oxcaml_flags.heap_reduction_threshold >= 0
    then float !Oxcaml_flags.heap_reduction_threshold
    else Float.infinity
  in
  if Float.compare major_words heap_reduction_threshold > 0
  then
    Profile.record_call "compact" (fun () ->
        reset ();
        Gc.compact ())

module Dwarf_helpers = struct
  let dwarf = ref None

  let sourcefile_for_dwarf = ref None

  let begin_dwarf ~code_begin ~code_end ~file_emitter =
    match !sourcefile_for_dwarf with
    | None -> ()
    | Some sourcefile ->
      let asm_directives =
        Asm_targets.Asm_directives_dwarf.build_asm_directives ()
      in
      let get_file_num = get_file_num ~file_emitter in
      Asm_targets.Asm_directives.debug_header ~get_file_num;
      let unit_name =
        (* CR lmaurer: This doesn't actually need to be an [Ident.t] *)
        Symbol.for_current_unit () |> Symbol.linkage_name
        |> Linkage_name.to_string |> Ident.create_persistent
      in
      let code_begin = Asm_targets.Asm_symbol.create code_begin in
      let code_end = Asm_targets.Asm_symbol.create code_end in
      dwarf
        := Some
             (Dwarf.create ~sourcefile ~unit_name ~asm_directives
                ~get_file_id:get_file_num ~code_begin ~code_end)

  let reset_dwarf () =
    dwarf := None;
    sourcefile_for_dwarf := None

  let init ~disable_dwarf ~sourcefile =
    reset_dwarf ();
    let can_emit_dwarf =
      !Clflags.debug
      && ((not !Dwarf_flags.restrict_to_upstream_dwarf)
         || !Dwarf_flags.dwarf_inlined_frames)
      && not disable_dwarf
    in
    match
      ( can_emit_dwarf,
        Target_system.architecture (),
        Target_system.derived_system () )
    with
    | true, (X86_64 | AArch64), _ -> sourcefile_for_dwarf := sourcefile
    | true, (IA32 | ARM | POWER | Z | Riscv), _ | false, _, _ -> ()

  let emit_dwarf () =
    Option.iter
      (Dwarf.emit
         ~basic_block_sections:!Oxcaml_flags.basic_block_sections
         ~binary_backend_available:!binary_backend_available)
      !dwarf

  let emit_delayed_dwarf () =
    Option.iter
      (Dwarf.emit_delayed
         ~basic_block_sections:!Oxcaml_flags.basic_block_sections
         ~binary_backend_available:!binary_backend_available)
      !dwarf

  let record_dwarf_for_fundecl fundecl =
    match !dwarf with
    | None -> None
    | Some dwarf ->
      let fun_end_label = Cmm.new_label () in
      Some (Dwarf.dwarf_for_fundecl dwarf fundecl ~fun_end_label)
end

let report_error ppf = function
  | Stack_frame_too_large n ->
    Format.fprintf ppf
      "stack frame too large (%d bytes). \nUse -long-frames compiler flag." n
  | Stack_frame_way_too_large n ->
    Format.fprintf ppf "stack frame too large (%d bytes)." n
  | Inconsistent_probe_init (name, dbg) ->
    Format.fprintf ppf
      "Inconsistent use of ~enabled_at_init in [%%probe %s ..] at %a" name
      Debuginfo.print_compact dbg

type preproc_stack_check_result =
  { max_frame_size : int;
    contains_nontail_calls : bool
  }

let preproc_stack_check ~fun_body ~frame_size ~trap_size =
  let rec loop (i : Linear.instruction) fs max_fs nontail_flag =
    match i.desc with
    | Lend -> { max_frame_size = max_fs; contains_nontail_calls = nontail_flag }
    | Ladjust_stack_offset { delta_bytes } ->
      let s = fs + delta_bytes in
      loop i.next s (max s max_fs) nontail_flag
    | Lpushtrap _ ->
      let s = fs + trap_size in
      loop i.next s (max s max_fs) nontail_flag
    | Lpoptrap _ -> loop i.next (fs - trap_size) max_fs nontail_flag
    | Lop (Stackoffset n) ->
      let s = fs + n in
      loop i.next s (max s max_fs) nontail_flag
    | Lcall_op (Lcall_ind | Lcall_imm _) -> loop i.next fs max_fs true
    | Lprologue
    | Lop
        ( Move | Spill | Reload | Opaque | Begin_region | End_region | Dls_get
        | Poll | Const_int _ | Const_float32 _ | Const_float _ | Const_symbol _
        | Const_vec128 _ | Const_vec256 _ | Const_vec512 _ | Load _
        | Store (_, _, _)
        | Intop _
        | Intop_imm (_, _)
        | Intop_atomic _
        | Floatop (_, _)
        | Csel _ | Reinterpret_cast _ | Static_cast _ | Probe_is_enabled _
        | Specific _ | Name_for_debugger _ | Alloc _ )
    | Lcall_op (Ltailcall_ind | Ltailcall_imm _ | Lextcall _ | Lprobe _)
    | Lreloadretaddr | Lreturn | Llabel _ | Lbranch _ | Lcondbranch _
    | Lcondbranch3 _ | Lswitch _ | Lentertrap | Lraise _ ->
      loop i.next fs max_fs nontail_flag
    | Lstackcheck _ ->
      (* should not be already present *)
      assert false
  in
  loop fun_body frame_size frame_size false

let add_stack_checks_if_needed (fundecl : Linear.fundecl) ~stack_offset
    ~stack_threshold_size ~trap_size =
  if Config.no_stack_checks
  then fundecl
  else
    let frame_size =
      Proc.frame_size ~stack_offset ~num_stack_slots:fundecl.fun_num_stack_slots
        ~contains_calls:fundecl.fun_contains_calls
    in
    let { max_frame_size; contains_nontail_calls } =
      preproc_stack_check ~fun_body:fundecl.fun_body ~frame_size ~trap_size
    in
    let insert_stack_check =
      contains_nontail_calls || max_frame_size >= stack_threshold_size
    in
    if insert_stack_check
    then
      let fun_body =
        Linear.instr_cons
          (Lstackcheck { max_frame_size_bytes = max_frame_size })
          [||] [||] ~available_before:fundecl.fun_body.available_before
          ~available_across:fundecl.fun_body.available_across fundecl.fun_body
      in
      { fundecl with fun_body }
    else fundecl
