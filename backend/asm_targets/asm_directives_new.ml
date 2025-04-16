(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*          Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2016--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR-someday mshinwell: Eliminate uses of [bprintf] from the assembly
   generation code, then enable this warning. *)
[@@@ocaml.warning "-3"]

module Int8 = Numbers.Int8
module Int16 = Numbers.Int16
module Uint64 = Numbers.Uint64
module TS = Target_system

(* CR sspies: replace polymorphic compare *)
open! Int_replace_polymorphic_compare

(* CR sspies: Removed [dwarf_supported] *)

let big_endian_ref = ref None

let current_section_ref = ref None

let not_initialized () =
  Misc.fatal_error "[Asm_directives.initialize] has not been called"

let current_section () =
  match !current_section_ref with
  | None -> not_initialized ()
  | Some section -> section

let current_section_is_text () =
  match !current_section_ref with
  | None -> not_initialized ()
  | Some section -> Asm_section.section_is_text section

let big_endian () =
  match !big_endian_ref with
  | None -> not_initialized ()
  | Some big_endian -> big_endian

type symbol_type =
  | Function
  | Object

let bprintf = Printf.bprintf

module Directive = struct
  module Constant = struct
    type t =
      | Signed_int of Int64.t
      | Unsigned_int of Uint64.t
      | This
      | Named_thing of string
      | Add of t * t
      | Sub of t * t

    let rec print buf t =
      match t with
      | (Named_thing _ | Signed_int _ | Unsigned_int _ | This) as c ->
        print_subterm buf c
      | Add (c1, c2) -> bprintf buf "%a + %a" print_subterm c1 print_subterm c2
      | Sub (c1, c2) -> bprintf buf "%a - %a" print_subterm c1 print_subterm c2

    and print_subterm buf t =
      match t with
      | This -> (
        match TS.assembler () with
        | MacOS | GAS_like -> Buffer.add_string buf "."
        | MASM -> Buffer.add_string buf "THIS BYTE")
      | Named_thing name -> Buffer.add_string buf name
      | Signed_int n -> (
        match TS.assembler () with
        (* We use %Ld and not %Lx on Unix-like platforms to ensure that
           ".sleb128" directives do not end up with hex arguments (since this
           denotes a variable-length encoding it would not be clear where the
           sign bit is). *)
        | MacOS | GAS_like -> bprintf buf "%Ld" n
        | MASM ->
          if Int64.compare n 0x8000_0000L >= 0 &&
             Int64.compare n 0x7fff_ffffL <= 0
          then Buffer.add_string buf (Int64.to_string n)
          else bprintf buf "0%LxH" n)
      | Unsigned_int n ->
        (* We can use the printer for [Signed_int] since we always print as an
           unsigned hex representation. *)
        print_subterm buf (Signed_int (Uint64.to_int64 n))
      | Add (c1, c2) ->
        bprintf buf "(%a + %a)" print_subterm c1 print_subterm c2
      | Sub (c1, c2) ->
        bprintf buf "(%a - %a)" print_subterm c1 print_subterm c2

  end

  module Constant_with_width = struct
    type width_in_bytes =
      | Eight
      | Sixteen
      | Thirty_two
      | Sixty_four


    type t =
      { constant : Constant.t;
        width_in_bytes : width_in_bytes
      }

    let create constant width_in_bytes =
      (* CR sspies: Potentially enable range check again here.
         We currently emit numbers like 158 as an 8-bit constant,
         which breaks int8 range checks, but make the assembler
         perfectly happy. *)
      { constant; width_in_bytes }

    let constant t = t.constant

    let width_in_bytes t = t.width_in_bytes
  end

  type thing_after_label =
    | Code
    | Machine_width_data

  type comment = string

  type t =
    | Align of { bytes : int }
    | Bytes of
        { str : string;
          comment : string option
        }
    | Cfi_adjust_cfa_offset of int
    | Cfi_def_cfa_offset of int
    | Cfi_endproc
    | Cfi_offset of
        { reg : int;
          offset : int
        }
    | Cfi_startproc
    | Comment of comment
    | Const of
        { constant : Constant_with_width.t;
          comment : string option
        }
    | Direct_assignment of string * Constant.t
    | File of
        { file_num : int option;
          filename : string
        }
    | Global of string
    | Indirect_symbol of string
    | Loc of
        { file_num : int;
          line : int;
          col : int;
          discriminator : int option
        }
    | New_label of string * thing_after_label
    | New_line
    | Private_extern of string
    | Section of
        { names : string list;
          flags : string option;
          args : string list
        }
    | Size of string * Constant.t
    | Sleb128 of
        { constant : Constant.t;
          comment : string option
        }
    | Space of { bytes : int }
    | Type of string * symbol_type
    | Uleb128 of
        { constant : Constant.t;
          comment : string option
        }
    | Protected of string

  let bprintf = Printf.bprintf

  let emit_comments () = !Clflags.keep_asm_file

  (* CR sspies: This code is a duplicate with [emit_string_literal]
     in [emitaux.ml]. Hopefully, we can deduplicate this soon. *)
  let string_of_string_literal s =
    let between x low high =
      Char.compare x low >= 0 && Char.compare x high <= 0
    in
    let buf = Buffer.create (String.length s + 2) in
    let last_was_escape = ref false in
    for i = 0 to String.length s - 1 do
      let c = s.[i] in
      if between c '0' '9'
      then
        if !last_was_escape
        then Printf.bprintf buf "\\%o" (Char.code c)
        else Buffer.add_char buf c
      else if between c ' ' '~'
          && (not (Char.equal c '"' (* '"' *)))
          && not (Char.equal c '\\')
      then (
        Buffer.add_char buf c;
        last_was_escape := false)
      else (
        Printf.bprintf buf "\\%o" (Char.code c);
        last_was_escape := true)
    done;
    Buffer.contents buf

  let buf_bytes_directive buf ~directive s =
    let pos = ref 0 in
    for i = 0 to String.length s - 1 do
      if !pos = 0
      then (
        if i > 0 then Buffer.add_char buf '\n';
        Buffer.add_char buf '\t';
        Buffer.add_string buf directive;
        Buffer.add_char buf '\t')
      else Buffer.add_char buf ',';
      Printf.bprintf buf "%d" (Char.code s.[i]);
      incr pos;
      if !pos >= 16 then pos := 0
    done

  (* CR sspies: This code is based on [emit_string_directive] in [emitaux.ml].
     We break up the string into smaller chunks. *)
  let print_ascii_string_gas buf s =
    let l = String.length s in
    if l = 0
    then ()
    else
      (* We first print the string 80 characters at a time. *)
      let i = ref 0 in
      while l - !i > 80 do
        bprintf buf "\t.ascii\t\"%s\"\n"
          (string_of_string_literal (String.sub s !i 80));
        i := !i + 80
      done;
      (* Then we print the remainder. We do not append a new line, because every
         directive ends with a new line. *)
      bprintf buf "\t.ascii\t\"%s\""
        (string_of_string_literal (String.sub s !i (l - !i)))

  let print_gas buf t =
    let gas_comment_opt comment_opt =
      if not (emit_comments ())
      then ""
      else
        match comment_opt with
        | None -> ""
        | Some comment -> Printf.sprintf "\t/* %s */" comment
    in
    match t with
    | Align { bytes = n } ->
      (* Some assemblers interpret the integer n as a 2^n alignment and others
         as a number of bytes. *)
      let n =
        match TS.assembler (), TS.architecture () with
        | MacOS, _ | GAS_like, (ARM | AArch64 | POWER) -> Misc.log2 n
        | _, _ -> n
      in
      bprintf buf "\t.align\t%d" n
    | Const { constant; comment } ->
      let directive =
        (* Apple's documentation says that ".word" is i386-specific, so we use
           ".2byte" instead. Additionally, it appears on ARM 32-bit and ARM
           64-bit that ".word" 32 bits wide, not 16 bits.

           To be sure the size is the same on all architectures, we use ".2byte",
           ".4byte", and ".8byte" here. See #3857. *)
        match Constant_with_width.width_in_bytes constant with
        | Eight -> "byte"
        | Sixteen -> (
          match TS.system () with Solaris -> "value" | _ -> "2byte")
        | Thirty_two -> "4byte"
        | Sixty_four -> "8byte"
      in
      let comment = gas_comment_opt comment in
      bprintf buf "\t.%s\t%a%s" directive Constant.print
        (Constant_with_width.constant constant)
        comment
    | Bytes { str; comment } ->
      (match TS.system (), TS.architecture () with
      | Solaris, _ | _, POWER -> buf_bytes_directive buf ~directive:".byte" str
      | _ -> print_ascii_string_gas buf str);
      bprintf buf "%s" (gas_comment_opt comment)
    | Comment s -> bprintf buf "\t\t\t/* %s */" s
    | Global s -> bprintf buf "\t.globl\t%s" s
    | New_label (s, _typ) -> bprintf buf "%s:" s
    | New_line -> ()
    | Section { names = [".data"]; _ } -> bprintf buf "\t.data"
    | Section { names = [".text"]; _ } -> bprintf buf "\t.text"
    | Section { names; flags; args } -> (
      bprintf buf "\t.section %s" (String.concat "," names);
      (match flags with None -> () | Some flags -> bprintf buf ",%S" flags);
      match args with
      | [] -> ()
      | _ -> bprintf buf ",%s" (String.concat "," args))
    | Space { bytes } -> (
      match TS.system () with
      | Solaris -> bprintf buf "\t.zero\t%d" bytes
      | _ -> bprintf buf "\t.space\t%d" bytes)
    | Cfi_adjust_cfa_offset n -> bprintf buf "\t.cfi_adjust_cfa_offset %d" n
    | Cfi_def_cfa_offset n -> bprintf buf "\t.cfi_def_cfa_offset %d" n
    | Cfi_endproc -> bprintf buf "\t.cfi_endproc"
    | Cfi_offset { reg; offset } ->
      bprintf buf "\t.cfi_offset %d, %d" reg offset
    | Cfi_startproc -> bprintf buf "\t.cfi_startproc"
    | File { file_num = None; filename } ->
      bprintf buf "\t.file\t\"%s\"" filename
    | File { file_num = Some file_num; filename } ->
      bprintf buf "\t.file\t%d\t\"%s\"" file_num
        (string_of_string_literal filename)
    | Indirect_symbol s -> bprintf buf "\t.indirect_symbol %s" s
    | Loc { file_num; line; col; discriminator } ->
      (* PR#7726: Location.none uses column -1, breaks LLVM assembler *)
      (* If we don't set the optional column field, debug_line program gets the
         column value from the previous .loc directive. *)
      let print_col buf col =
        if col >= 0 then bprintf buf "\t%d" col else bprintf buf "\t0"
      in
      let print_discriminator buf dis =
        match dis with
        | None -> ()
        | Some dis -> bprintf buf "\tdiscriminator %d" dis
      in
      bprintf buf "\t.loc\t%d\t%d%a%a" file_num line print_col col
        print_discriminator discriminator
    | Private_extern s -> bprintf buf "\t.private_extern %s" s
    | Size (s, c) -> bprintf buf "\t.size %s,%a" s Constant.print c
    | Sleb128 { constant; comment } ->
      let comment = gas_comment_opt comment in
      bprintf buf "\t.sleb128 %a%s" Constant.print constant comment
    | Type (s, typ) ->
      (* We use the "STT" forms when they are supported as they are unambiguous
         across platforms (cf. https://sourceware.org/binutils/docs/as/Type.html
         ). *)
      let typ =
        match typ with Function -> "STT_FUNC" | Object -> "STT_OBJECT"
      in
      bprintf buf "\t.type %s %s" s typ
    | Uleb128 { constant; comment } ->
      let comment = gas_comment_opt comment in
      bprintf buf "\t.uleb128 %a%s" Constant.print constant comment
    | Direct_assignment (var, const) -> (
      match TS.assembler () with
      | MacOS -> bprintf buf "%s = %a" var Constant.print const
      | _ ->
        Misc.fatal_error
          "Cannot emit [Direct_assignment] except on macOS-like assemblers")
    | Protected s -> bprintf buf "\t.protected\t%s" s

  let print_masm buf t =
    let unsupported name =
      Misc.fatal_errorf "Unsupported asm directive [%s] for MASM" name
    in
    let masm_comment_opt comment_opt =
      if not (emit_comments ())
      then ""
      else
        match comment_opt with
        | None -> ""
        | Some comment -> Printf.sprintf "\t; %s" comment
    in
    match t with
    | Align { bytes } -> bprintf buf "\tALIGN\t%d" bytes
    | Bytes { str; comment } ->
      buf_bytes_directive buf ~directive:"BYTE" str;
      bprintf buf "%s" (masm_comment_opt comment)
    | Comment s -> bprintf buf " ; %s " s
    | Const { constant; comment } ->
      let directive =
        match Constant_with_width.width_in_bytes constant with
        | Eight -> "BYTE"
        | Sixteen -> "WORD"
        | Thirty_two -> "DWORD"
        | Sixty_four -> "QWORD"
      in
      let comment = masm_comment_opt comment in
      bprintf buf "\t%s\t%a%s" directive Constant.print
        (Constant_with_width.constant constant)
        comment
    | Global s -> bprintf buf "\tPUBLIC\t%s" s
    | Section { names = [".data"]; _ } -> bprintf buf "\t.DATA"
    | Section { names = [".text"]; _ } -> bprintf buf "\t.CODE"
    | Section _ -> Misc.fatal_error "Unknown section name for MASM emitter"
    | Space { bytes } -> bprintf buf "\tBYTE\t%d DUP (?)" bytes
    | New_label (label, Code) -> bprintf buf "%s:" label
    | New_label (label, Machine_width_data) -> (
      match TS.machine_width () with
      | Thirty_two -> bprintf buf "%s LABEL DWORD" label
      | Sixty_four -> bprintf buf "%s LABEL QWORD" label)
    | New_line -> ()
    | Cfi_adjust_cfa_offset _ -> unsupported "Cfi_adjust_cfa_offset"
    | Cfi_def_cfa_offset _ -> unsupported "Cfi_def_cfa_offset"
    | Cfi_endproc -> unsupported "Cfi_endproc"
    | Cfi_offset _ -> unsupported "Cfi_offset"
    | Cfi_startproc -> unsupported "Cfi_startproc"
    | File _ -> unsupported "File"
    | Indirect_symbol _ -> unsupported "Indirect_symbol"
    | Loc _ -> unsupported "Loc"
    | Private_extern _ -> unsupported "Private_extern"
    | Size _ -> unsupported "Size"
    | Sleb128 _ -> unsupported "Sleb128"
    | Type _ -> unsupported "Type"
    | Uleb128 _ -> unsupported "Uleb128"
    | Direct_assignment _ -> unsupported "Direct_assignment"
    | Protected _ -> unsupported "Protected"

  let print b t =
    match TS.assembler () with
    | MASM -> print_masm b t
    | MacOS | GAS_like -> print_gas b t
end

(* A higher-level version of [Constant.t] which contains some more abstractions
   (e.g. use of [Cmm.label], and in the future distinguished types to represent
   symbols and symbol references before they are mangled to plain [string]s for
   [Constant.t]). *)
type expr =
  | Signed_int of Int64.t
  | Unsigned_int of Uint64.t
  | This
  | Label of Asm_label.t
  | Symbol of Asm_symbol.t
  | Add of expr * expr
  | Sub of expr * expr

let rec lower_expr (cst : expr) : Directive.Constant.t =
  match cst with
  | Signed_int n -> Signed_int n
  | Unsigned_int n -> Unsigned_int n
  | This -> This
  | Label lbl -> Named_thing (Asm_label.encode lbl)
  | Symbol sym -> Named_thing (Asm_symbol.encode sym)
  | Add (cst1, cst2) -> Add (lower_expr cst1, lower_expr cst2)
  | Sub (cst1, cst2) -> Sub (lower_expr cst1, lower_expr cst2)

let const_sub c1 c2 = Sub (c1, c2)

let const_add c1 c2 = Add (c1, c2)

let const_label lbl = Label lbl

let const_symbol sym = Symbol sym

let const_int64 i : expr = Signed_int i

let emit_ref = ref None

let emit (d : Directive.t) =
  match !emit_ref with
  | Some emit -> emit d
  | None -> Misc.fatal_error "initialize not called"

let emit_non_masm (d : Directive.t) =
  match TS.assembler () with MASM -> () | MacOS | GAS_like -> emit d

let section ~names ~flags ~args = emit (Section { names; flags; args })

let align ~bytes = emit (Align { bytes })

let should_generate_cfi () =
  (* We generate CFI info even if we're not generating any other debugging
     information. This is in fact necessary on macOS, where it may be expected
     that OCaml stack frames are unwindable (see testsuite/tests/unwind/README
     for more information). *)
  Config.asm_cfi_supported

let cfi_adjust_cfa_offset ~bytes =
  if should_generate_cfi () && bytes <> 0
  then emit (Cfi_adjust_cfa_offset bytes)

let cfi_def_cfa_offset ~bytes =
  if should_generate_cfi () then emit (Cfi_def_cfa_offset bytes)

let cfi_endproc () = if should_generate_cfi () then emit Cfi_endproc

let cfi_offset ~reg ~offset =
  if should_generate_cfi () && offset <> 0
  then emit (Cfi_offset { reg; offset })

let cfi_startproc () = if should_generate_cfi () then emit Cfi_startproc

let comment text = if !Clflags.keep_asm_file then emit (Comment text)

let loc ~file_num ~line ~col ?discriminator () =
  emit_non_masm (Loc { file_num; line; col; discriminator })

let space ~bytes = emit (Space { bytes })

(* We do not perform any checks that the string does not contain null bytes. The
   reason is that we sometimes want to emit strings that have an explicit null
   byte added. *)
let string ?comment str = emit (Bytes { str; comment })

let global symbol = emit (Global (Asm_symbol.encode symbol))

let indirect_symbol symbol = emit (Indirect_symbol (Asm_symbol.encode symbol))

let private_extern symbol = emit (Private_extern (Asm_symbol.encode symbol))

let size symbol cst = emit (Size (Asm_symbol.encode symbol, lower_expr cst))

let type_ symbol ~type_ = emit (Type (symbol, type_))

let sleb128 ?comment i =
  emit (Sleb128 { constant = Directive.Constant.Signed_int i; comment })

let uleb128 ?comment i =
  emit (Uleb128 { constant = Directive.Constant.Unsigned_int i; comment })

let protected symbol = emit (Protected (Asm_symbol.encode symbol))

let direct_assignment var cst = emit (Direct_assignment (var, lower_expr cst))

let const ?comment constant
    (width : Directive.Constant_with_width.width_in_bytes) =
  let constant = lower_expr constant in
  let constant = Directive.Constant_with_width.create constant width in
  emit (Const { constant; comment })

let const_machine_width ?comment constant =
  match TS.machine_width () with
  | Thirty_two -> const ?comment constant Thirty_two
  | Sixty_four -> const ?comment constant Sixty_four

let float32 f =
  let comment =
    if !Clflags.keep_asm_file then Some (Printf.sprintf "%.12f" f) else None
  in
  let f_int32 = Int64.of_int32 (Int32.bits_of_float f) in
  const ?comment (Signed_int f_int32) Thirty_two

let float64_core f f_int64 =
  match TS.machine_width () with
  | Sixty_four ->
    let comment =
      if !Clflags.keep_asm_file then Some (Printf.sprintf "%.12g" f) else None
    in
    const ?comment (Signed_int f_int64) Sixty_four
  | Thirty_two ->
    let comment_lo =
      if !Clflags.keep_asm_file
      then Some (Printf.sprintf "low part of %.12g" f)
      else None
    in
    let comment_hi =
      if !Clflags.keep_asm_file
      then Some (Printf.sprintf "high part of %.12g" f)
      else None
    in
    let lo = Signed_int (Int64.logand f_int64 0xffff_ffffL) in
    let hi = Signed_int (Int64.shift_right_logical f_int64 32) in
    if big_endian ()
    then (
      const ?comment:comment_hi hi Thirty_two;
      const ?comment:comment_lo lo Thirty_two)
    else (
      const ?comment:comment_lo lo Thirty_two;
      const ?comment:comment_hi hi Thirty_two)

let float64 f = float64_core f (Int64.bits_of_float f)

let float64_from_bits f = float64_core (Int64.float_of_bits f) f

let size ?size_of symbol =
  match TS.system () with
  | GNU | Linux | FreeBSD | NetBSD | OpenBSD | Generic_BSD ->
    let size_of =
      match size_of with None -> symbol | Some size_of -> size_of
    in
    size size_of (Sub (This, Symbol symbol))
  | _ -> ()

let label ?comment label = const_machine_width ?comment (Label label)

let label_plus_offset ?comment lab ~offset_in_bytes =
  let offset_in_bytes = Targetint.to_int64 offset_in_bytes in
  let lab = const_label lab in
  const_machine_width ?comment (const_add lab (const_int64 offset_in_bytes))

let define_label label =
  let lbl_section = Asm_label.section label in
  let this_section =
    match !current_section_ref with
    | None -> not_initialized ()
    | Some this_section -> this_section
  in
  (* CR sspies: Thsis assumes no other mechanism was used to switch sections
     such as calling [D.text] directly. *)
  if not (Asm_section.equal lbl_section this_section)
  then
    Misc.fatal_errorf
      "Cannot define label %a intended for section %a in section %a"
      Asm_label.print label Asm_section.print lbl_section Asm_section.print
      this_section;
  let typ : Directive.thing_after_label =
    if current_section_is_text () then Code else Machine_width_data
  in
  emit (New_label (Asm_label.encode label, typ))

let new_line () = if !Clflags.keep_asm_file then emit New_line

let sections_seen = ref []

let switch_to_section ?(emit_label_on_first_occurrence = false) section =
  (* CR sspies: This code could be made more sensitive to the tracking of the current section:
     there is no need to emit the section again if it is the same as the current section. However,
     that excludes other code from directly switching the section, and it is not consistent with
     how we currently emit code. So for now we always emit the section, even if we are not switching.
  *)
  let first_occurrence =
    if List.mem section !sections_seen
    then false
    else (
      sections_seen := section :: !sections_seen;
      true)
  in
    current_section_ref := Some section;
    let ({ names; flags; args } : Asm_section.section_details) =
      Asm_section.details section ~first_occurrence
    in
    (* CR sspies: We do not print an empty line here to be consistent with Arm emission. *)
    emit (Section { names; flags; args });
    if first_occurrence && emit_label_on_first_occurrence then define_label (Asm_label.for_section section)

let switch_to_section_raw ~names ~flags ~args =
  emit (Section { names; flags; args })

let text () = switch_to_section Asm_section.Text

let data () = switch_to_section Asm_section.Data

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

let cached_strings = ref Cached_string.Map.empty

let temp_var_counter = ref 0

let reset () =
  cached_strings := Cached_string.Map.empty;
  sections_seen := [];
  current_section_ref := None;
  temp_var_counter := 0

let file ~file_num ~file_name () =
  (* gas can silently emit corrupted line tables if a .file directive contains a
     number but an empty filename. *)
  let file_name =
    match file_num with
    | None -> file_name
    | Some _file_num ->
      if String.length file_name <= 0 then "none" else file_name
  in
  emit_non_masm (File { file_num; filename = file_name })


let initialize ~big_endian ~(emit : Directive.t -> unit) =
  big_endian_ref := Some big_endian;
  emit_ref := Some emit;
  reset ()


let debug_header ~get_file_num =
  (* Forward label references are illegal on some assemblers/platforms. To
      avoid errors, emit the beginning of all dwarf sections in advance. *)
  if TS.is_gas () || TS.is_macos ()
  then List.iter (switch_to_section ~emit_label_on_first_occurrence:true) (Asm_section.dwarf_sections_in_order ());

  (* Stop dsymutil complaining about empty __debug_line sections (produces bogus
     error "line table parameters mismatch") by making sure such sections are
     never empty. *)
  let file_num = get_file_num "none" in
  loc ~file_num ~line:1 ~col:1 ();
  switch_to_section Asm_section.Text

let file ~file_num ~file_name = file ~file_num ~file_name ()

let define_data_symbol symbol =
  (* CR sspies: enable check again *)
  (* check_symbol_for_definition_in_current_section symbol; *)
  emit (New_label (Asm_symbol.encode symbol, Machine_width_data));
  match TS.assembler (), TS.windows () with
  | GAS_like, false -> type_ (Asm_symbol.encode symbol) ~type_:Object
  | GAS_like, true | MacOS, _ | MASM, _ -> ()

(* CR mshinwell: Rename to [define_text_symbol]? *)
let define_function_symbol symbol =
  (* CR sspies: enable check again *)
  (* check_symbol_for_definition_in_current_section symbol; *)
  (* CR mshinwell: This shouldn't be called "New_label" *)
  emit (New_label (Asm_symbol.encode symbol, Code));
  match TS.assembler (), TS.windows () with
  | GAS_like, false -> type_ (Asm_symbol.encode symbol) ~type_:Function
  | GAS_like, true | MacOS, _ | MASM, _ -> ()

let type_symbol symbol ~ty =
  match TS.assembler (), TS.windows () with
  | GAS_like, false -> type_ (Asm_symbol.encode symbol) ~type_:ty
  | GAS_like, true | MacOS, _ | MASM, _ -> ()

let type_label label ~ty =
  match TS.assembler (), TS.windows () with
  | GAS_like, false -> type_ (Asm_label.encode label) ~type_:ty
  | GAS_like, true | MacOS, _ | MASM, _ -> ()

let symbol ?comment sym = const_machine_width ?comment (Symbol sym)

let symbol_plus_offset symbol ~offset_in_bytes =
  let offset_in_bytes = Targetint.to_int64 offset_in_bytes in
  const_machine_width (Add (Symbol symbol, Signed_int offset_in_bytes))

let int8 ?comment i =
  const ?comment (Signed_int (Int64.of_int (Int8.to_int i))) Eight

let int16 ?comment i =
  const ?comment (Signed_int (Int64.of_int (Int16.to_int i))) Sixteen

let int32 ?comment i = const ?comment (Signed_int (Int64.of_int32 i)) Thirty_two

let int64 ?comment i = const ?comment (Signed_int i) Sixty_four

let uint8 ?comment i = const ?comment (Unsigned_int (Uint64.of_uint8 i)) Eight

let uint16 ?comment i =
  const ?comment (Unsigned_int (Uint64.of_uint16 i)) Sixteen

let uint32 ?comment i =
  const ?comment (Unsigned_int (Uint64.of_uint32 i)) Thirty_two

let uint64 ?comment i = const ?comment (Unsigned_int i) Sixty_four

let targetint ?comment n =
  match Targetint.repr n with
  | Int32 n -> int32 ?comment n
  | Int64 n -> int64 ?comment n

let cache_string ?comment section str =
  let cached : Cached_string.t = { section; str; comment } in
  match Cached_string.Map.find cached !cached_strings with
  | label -> label
  | exception Not_found ->
    let label = Asm_label.create section in
    cached_strings := Cached_string.Map.add cached label !cached_strings;
    label

let emit_cached_strings () =
  Cached_string.Map.iter
    (fun { section; str; comment } label_name ->
      switch_to_section section;
      define_label label_name;
      string ?comment str;
      int8 Int8.zero)
    !cached_strings;
  cached_strings := Cached_string.Map.empty

let mark_stack_non_executable () =
  let current_section = current_section () in
  match TS.system () with
  | Linux ->
    section ~names:[".note.GNU-stack"] ~flags:(Some "") ~args:["%progbits"];
    switch_to_section current_section
  | _ -> ()

let new_temp_var () =
  let id = !temp_var_counter in
  incr temp_var_counter;
  Printf.sprintf "temp%d" id

(* CR sspies: once there are richer symbols, make this function take the section
   again *)
let force_assembly_time_constant expr =
  if not (TS.is_macos ())
  then expr
  else
    (* This ensures the correct result is obtained on macOS. (Apparently just
       writing expressions such as "L100 - L101" inline can cause unexpected
       results when one of the labels is on a section boundary, for example.) *)
    let temp = new_temp_var () in
    direct_assignment temp expr;
    let lbl = Asm_label.create_string Data temp in
    const_label lbl (* not really a label, but OK. *)

let between_symbols_in_current_unit ~upper ~lower =
  (* CR-someday bkhajwal: Add checks below from gdb-names-gpr
     check_symbol_in_current_unit upper; check_symbol_in_current_unit lower;
     check_symbols_in_same_section upper lower; *)
  let upper = const_symbol upper in
  let lower = const_symbol lower in
  let expr = const_sub upper lower in
  (* CR sspies: is this check even needed? *)
  if TS.is_macos ()
  then const_machine_width (force_assembly_time_constant expr)
  else const_machine_width expr

let between_labels_16_bit ?comment:_ ~upper:_ ~lower:_ () =
  (* CR poechsel: use the arguments *)
  Misc.fatal_error "between_labels_16_bit not implemented yet"

let between_labels_32_bit ?comment:_ ~upper:_ ~lower:_ () =
  (* CR poechsel: use the arguments *)
  Misc.fatal_error "between_labels_32_bit not implemented yet"

let between_labels_64_bit ?comment:_ ~upper:_ ~lower:_ () =
  (* CR poechsel: use the arguments *)
  Misc.fatal_error "between_labels_64_bit not implemented yet"

let between_labels_64_bit_with_offsets ?comment:_comment ~upper ~upper_offset
    ~lower ~lower_offset () =
  Option.iter comment _comment;
  let upper_offset = Targetint.to_int64 upper_offset in
  let lower_offset = Targetint.to_int64 lower_offset in
  let expr =
    const_sub
      (const_add (const_label upper) (const_int64 upper_offset))
      (const_add (const_label lower) (const_int64 lower_offset))
  in
  const_machine_width (force_assembly_time_constant expr)

let between_this_and_label_offset_32bit_expr ~upper ~offset_upper =
  let upper_section = Asm_label.section upper in
  (match !current_section_ref with
  | None -> not_initialized ()
  | Some this_section ->
    if not (Asm_section.equal upper_section this_section)
    then
      Misc.fatal_errorf
        "Label %a in section %a is not in the current section %a"
        Asm_label.print upper Asm_section.print upper_section Asm_section.print
        this_section);
  let offset_upper = Targetint.to_int64 offset_upper in
  let expr = Sub (Add (Label upper, Signed_int offset_upper), This) in
  const expr Thirty_two

let between_symbol_in_current_unit_and_label_offset ?comment:_comment ~upper
    ~lower ~offset_upper () =
  (* CR mshinwell: add checks, as above: check_symbol_in_current_unit lower;
     check_symbol_and_label_in_same_section lower upper; *)
  Option.iter comment _comment;
  if Targetint.compare offset_upper Targetint.zero = 0
  then
    let expr = const_sub (const_label upper) (const_symbol lower) in
    const_machine_width (force_assembly_time_constant expr)
  else
    let offset_upper = Targetint.to_int64 offset_upper in
    let expr =
      const_sub
        (const_add (const_label upper) (const_int64 offset_upper))
        (const_symbol lower)
    in
    const_machine_width (force_assembly_time_constant expr)

let const_with_width ~width constant =
  match width with
  | Dwarf_flags.Thirty_two -> const constant Thirty_two
  | Dwarf_flags.Sixty_four -> const constant Sixty_four

let offset_into_dwarf_section_label ?comment:_comment ~width section upper =
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
    match _comment with
    | None -> comment (Format.asprintf "offset into %s" expected_section)
    | Some _comment ->
      comment (Format.asprintf "%s (offset into %s)" _comment expected_section));
  (* macOS does not use relocations in DWARF sections in places, such as here,
     where they might be expected. Instead dsymutil and other tools parse DWARF
     sections properly and adjust offsets manually. *)
  let expr =
    if TS.is_macos ()
    then
      let lower = Asm_label.for_dwarf_section section in
      if Asm_label.equal lower upper
      then const_int64 0L
      else
        force_assembly_time_constant
          (const_sub (const_label upper) (const_label lower))
    else const_label upper
  in
  const_with_width ~width expr

let offset_into_dwarf_section_symbol ?comment:_comment
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
  (*= CR mshinwell: try again to make this work on macOS, maybe using
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
  let _comment =
    if not !Clflags.keep_asm_file
    then None
    else
      match _comment with
      | None ->
        Some
          (Format.asprintf "offset into %s"
             (Asm_section.to_string (DWARF section)))
      | Some _comment ->
        Some
          (Format.asprintf "%s (offset into %s)" _comment
             (Asm_section.to_string (DWARF section)))
  in
  Option.iter comment _comment;
  let expr =
    if TS.is_macos ()
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
          (const_sub (const_symbol upper) (const_label lower))
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
    else const_symbol upper
  in
  match width with
  | Thirty_two -> const expr Thirty_two
  | Sixty_four -> const expr Sixty_four
