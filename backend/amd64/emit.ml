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

(* Emission of Intel x86_64 assembly code *)

(* Correctness: carefully consider any use of [Config], [Clflags],
   [Flambda_backend_flags] and shared variables. For details, see
   [asmgen.mli]. *)

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
open Arch
open Proc
open Reg
open Operation
open Linear
open Emitaux
open X86_ast_utils
open X86_proc
open X86_dsl
module String = Misc.Stdlib.String
module Simd_instrs = Amd64_simd_instrs

(* [Branch_relaxation] is not used in this file, but is required by emit.ml
   files for certain other targets; the reference here ensures that when
   releases are being prepared the .depend files are correct for all targets. *)
[@@@ocaml.warning "-66"]

open! Branch_relaxation
module ND = Asm_targets.Asm_directives_new
module S = Asm_targets.Asm_symbol
module L = Asm_targets.Asm_label

let rec to_x86_constant (c : ND.Directive.Constant.t) : X86_ast.constant =
  match c with
  | Signed_int i -> Const i
  | Unsigned_int i -> Const (Numbers.Uint64.to_int64 i)
  | This -> ConstThis
  | Named_thing s ->
    ConstLabel s
    (* both seem to be printed directly to the buffer without any conversion*)
  | Add (c1, c2) -> ConstAdd (to_x86_constant c1, to_x86_constant c2)
  | Sub (c1, c2) -> ConstSub (to_x86_constant c1, to_x86_constant c2)

let to_x86_constant_with_width (c : ND.Directive.Constant_with_width.t) :
    X86_ast.asm_line =
  let width = ND.Directive.Constant_with_width.width_in_bytes c in
  let const = ND.Directive.Constant_with_width.constant c in
  let const = to_x86_constant const in
  match width with
  | Eight -> Byte const
  (* on x86 Word is 2 bytes; warning this is not the same on Arm *)
  | Sixteen -> Word const
  | Thirty_two -> Long const
  | Sixty_four -> Quad const

let to_x86_directive (dir : ND.Directive.t) : X86_ast.asm_line list =
  let comment_lines comment =
    (* CR sspies: This check is usually done in the printing function of the new
       directives. Since we are skipping those at the moment (by emitting via
       the X86 DSL), we do the same check here in the conversion. *)
    if !Clflags.keep_asm_file && !Flambda_backend_flags.dasm_comments
    then Option.to_list (Option.map (fun s -> X86_ast.Comment s) comment)
    else []
  in
  match dir with
  | Align { bytes; data_section } ->
    [X86_ast.Align (data_section, bytes)]
    (* The data field is currently ignored by GAS and MASM, but used in the
       binary emitter. The bytes field is only converted to the final value when
       printing. *)
  | Bytes { str; comment } -> comment_lines comment @ [X86_ast.Bytes str]
  | Comment s -> comment_lines (Some s)
  | Const { constant; comment } ->
    comment_lines comment @ [to_x86_constant_with_width constant]
  | Direct_assignment (s, c) ->
    (* We use [.set s c] for direct assignments, since it evaluates [c]
       directly. The alternative, [s = c], is sensitive to relocations. *)
    [X86_ast.Set (s, to_x86_constant c)]
  | File { file_num = None; _ } ->
    Misc.fatal_error "file directive must always carry a number on x86"
  | File { file_num = Some file_num; filename } ->
    [X86_ast.File (file_num, filename)]
  | Global s -> [X86_ast.Global s]
  | Indirect_symbol s -> [X86_ast.Indirect_symbol s]
  | Loc { file_num; line; col; discriminator } ->
    (* Behavior differs for negative column values. x86 will not output
       anything, but new directives will output 0. *)
    [X86_ast.Loc { file_num; line; col; discriminator }]
  (* CR sspies: The [typ] matters only for MASM. The convention (implemented in
     asm directives) is that in the text section, we use Code (NONE) and in the
     data section, we use Machine_width_data (QWORD for amd64). The two will be
     emitted differently by MASM. Because some code such as the frame tables
     have moved from the data section to the text section (but were previously
     still emitted with QUAD), using the new directives below changes this
     behavior. *)
  | New_label (s, Code) -> [X86_ast.NewLabel (s, NONE)]
  | New_label (s, Machine_width_data) -> [X86_ast.NewLabel (s, QWORD)]
  | New_line -> [X86_ast.NewLine]
  | Private_extern s -> [X86_ast.Private_extern s]
  | Section { names; flags; args } ->
    [X86_ast.Section (names, flags, args, false)]
    (* delayed for this directive is always ignored in GAS printing, and section
       is not supported in binary emitter. In MASM, it only supports .text and
       .data. *)
  | Size (s, c) -> [X86_ast.Size (s, to_x86_constant c)]
  | Sleb128 { constant; comment } ->
    comment_lines comment @ [X86_ast.Sleb128 (to_x86_constant constant)]
  | Space { bytes } -> [Space bytes]
  | Type (n, st) ->
    let typ = ND.symbol_type_to_string st in
    [Type (n, typ)]
  | Uleb128 { constant; comment } ->
    comment_lines comment @ [X86_ast.Uleb128 (to_x86_constant constant)]
  | Cfi_adjust_cfa_offset n -> [X86_ast.Cfi_adjust_cfa_offset n]
  | Cfi_def_cfa_offset n -> [X86_ast.Cfi_def_cfa_offset n]
  | Cfi_endproc -> [X86_ast.Cfi_endproc]
  | Cfi_offset { reg; offset } -> [X86_ast.Cfi_offset (reg, offset)]
  | Cfi_startproc -> [X86_ast.Cfi_startproc]
  | Cfi_remember_state -> [X86_ast.Cfi_remember_state]
  | Cfi_restore_state -> [X86_ast.Cfi_restore_state]
  | Cfi_def_cfa_register r -> [X86_ast.Cfi_def_cfa_register r]
  | Protected s -> [X86_ast.Protected s]
  | Hidden s -> [X86_ast.Hidden s]
  | Weak s -> [X86_ast.Weak s]
  | External s -> [X86_ast.External (s, NEAR)]
  (* All uses of [.extrn] use NEAR as the type. *)
  | Reloc { offset; name = R_X86_64_PLT32; expr } ->
    [ X86_ast.Reloc
        { offset = to_x86_constant offset;
          name = R_X86_64_PLT32;
          expr = to_x86_constant expr
        } ]

(** Turn a Linear label into an assembly label. The section is checked against the
    section tracked by [D] when emitting label definitions. *)
let label_to_asm_label (l : label) ~(section : Asm_targets.Asm_section.t) : L.t
    =
  L.create_int section (Label.to_int l)

(* Override proc.ml *)

let int_reg_name : X86_ast.reg64 array =
  [| RAX; RBX; RDI; RSI; RDX; RCX; R8; R9; R12; R13; R10; R11; RBP |]

let float_reg_name = Array.init 16 (fun i -> X86_ast.XMM i)

let register_name typ r : X86_ast.arg =
  match (typ : Cmm.machtype_component) with
  | Int | Val | Addr -> Reg64 int_reg_name.(r)
  | Float | Float32 | Vec128 | Valx2 -> Regf float_reg_name.(r - 100)

let phys_rax = phys_reg Int 0

let phys_rdx = phys_reg Int 4

let phys_rcx = phys_reg Int 5

let phys_xmm0v () = phys_reg Vec128 100

let file_emitter ~file_num ~file_name =
  ND.file ~file_num:(Some file_num) ~file_name

let emit_debug_info ?discriminator dbg =
  emit_debug_info_gen ?discriminator dbg file_emitter ND.loc

let emit_debug_info_linear i =
  match i.fdo with
  | None -> emit_debug_info i.dbg
  | Some { discriminator; dbg } -> emit_debug_info ~discriminator dbg

let fp = Config.with_frame_pointers

(* Tradeoff between code size and code speed *)

let fastcode_flag = ref true

(* Layout of the stack frame *)
let stack_offset = ref 0

let num_stack_slots = Stack_class.Tbl.make 0

let prologue_required = ref false

let frame_required = ref false

let contains_calls = ref false

let frame_size () =
  Proc.frame_size ~stack_offset:!stack_offset ~num_stack_slots
    ~contains_calls:!contains_calls

let slot_offset loc stack_class =
  let offset =
    Proc.slot_offset loc ~stack_class ~stack_offset:!stack_offset
      ~fun_contains_calls:!contains_calls ~fun_num_stack_slots:num_stack_slots
  in
  match offset with
  | Bytes_relative_to_stack_pointer n -> n
  | Bytes_relative_to_domainstate_pointer _ ->
    Misc.fatal_errorf "Not a stack slot"

let emit_stack_offset n =
  if n < 0 then I.add (int (-n)) rsp else if n > 0 then I.sub (int n) rsp;
  if n <> 0 then ND.cfi_adjust_cfa_offset ~bytes:n;
  stack_offset := !stack_offset + n

let push r =
  I.push r;
  ND.cfi_adjust_cfa_offset ~bytes:8;
  stack_offset := !stack_offset + 8

let pop r =
  I.pop r;
  ND.cfi_adjust_cfa_offset ~bytes:(-8);
  stack_offset := !stack_offset - 8

(* Symbols *)

let emit_symbol s = S.encode (S.create s)

(* Record symbols used and defined - at the end generate extern for those used
   but not defined *)

let symbols_defined = ref String.Set.empty

let symbols_used = ref String.Set.empty

let add_def_symbol s = symbols_defined := String.Set.add s !symbols_defined

let add_used_symbol s = symbols_used := String.Set.add s !symbols_used

let imp_table = Hashtbl.create 16

let reset_imp_table () = Hashtbl.clear imp_table

let get_imp_symbol s =
  match Hashtbl.find imp_table s with
  | exception Not_found ->
    let imps = "__caml_imp_" ^ s in
    Hashtbl.add imp_table s imps;
    imps
  | imps -> imps

let emit_imp_table ~section () =
  let f s imps =
    ND.define_symbol_label ~section (S.create imps);
    ND.symbol (S.create s)
  in
  ND.data ();
  ND.comment "relocation table start";
  ND.align ~data_section:true ~bytes:8;
  Hashtbl.iter f imp_table;
  ND.comment "relocation table end"

let mem__imp s =
  let imp_s = get_imp_symbol s in
  mem64_rip QWORD (emit_symbol imp_s)

(* Output a label *)

let label_name lbl =
  if is_macosx system || is_win64 system then "L" ^ lbl else ".L" ^ lbl

let rel_plt (s : Cmm.symbol) =
  match (s.sym_global : Cmm.is_global) with
  | Local -> sym (label_name (emit_symbol s.sym_name))
  | Global ->
    if windows && !Clflags.dlcode
    then mem__imp s.sym_name
    else
      let s = emit_symbol s.sym_name in
      sym (if use_plt then s ^ "@PLT" else s)

let emit_call s = I.call (rel_plt s)

let emit_jump s = I.jmp (rel_plt s)

let domain_field f = mem64 QWORD (Domainstate.idx_of_field f * 8) R14

let emit_cmm_symbol (s : Cmm.symbol) =
  let sym = S.create s.sym_name in
  match (s.sym_global : Cmm.is_global) with
  | Global -> `Symbol sym
  (* This label is special in that it is not of the form "Lnumber". Instead, we
     take the symbol, encode it, and turn the resulting string into a label. The
     label will still be prefixed by ".L"/"L" when emitting. *)
  (* CR sspies: Extend the new directives code to support local symbols properly (as
    opposed to requiring chaining the label and symbol code).*)
  | Local -> `Label (L.create_string_unchecked Text (S.encode sym))

let emit_cmm_symbol_str (s : Cmm.symbol) =
  match emit_cmm_symbol s with
  | `Symbol s -> S.encode s
  | `Label l -> L.encode l

let load_symbol_addr (s : Cmm.symbol) arg =
  match (s.sym_global : Cmm.is_global) with
  | Local -> I.lea (mem64_rip NONE (label_name (emit_symbol s.sym_name))) arg
  | Global ->
    if !Clflags.dlcode
    then
      if windows
      then
        (* I.mov (mem__imp s) arg (\* mov __caml_imp_foo(%rip), ... *\) *)
        I.mov (sym (emit_symbol s.sym_name)) arg (* movabsq $foo, ... *)
      else I.mov (mem64_rip QWORD (emit_symbol s.sym_name ^ "@GOTPCREL")) arg
    else if !Clflags.pic_code
    then I.lea (mem64_rip NONE (emit_symbol s.sym_name)) arg
    else I.mov (sym (emit_symbol s.sym_name)) arg

(* Output .text section directive, or named .text.caml.<name> if enabled and
   supported on the target system. *)

let emit_named_text_section ?(suffix = "") func_name =
  if !Clflags.function_sections || !Flambda_backend_flags.basic_block_sections
  then (
    match[@ocaml.warning "-4"] system with
    | S_macosx
    (* Names of section segments in macosx are restricted to 16 characters, but
       function names are often longer, especially anonymous functions. *)
    | S_win64 | S_mingw64
    | S_cygwin
      (* Win systems provide named text sections, but configure on these systems
         does not support function sections. *) ->
      assert false
    | _ ->
      ND.switch_to_section_raw
        ~names:[Printf.sprintf ".text.caml.%s%s" (emit_symbol func_name) suffix]
        ~flags:(Some "ax") ~args:["@progbits"];
      (* Warning: We set the internal section ref to Text here, because it
         currently does not supported named text sections. In the rest of this
         file, we pretend the section is called Text rather than the function
         specific text section. *)
      (* CR sspies: Add proper support for named text sections. *)
      ND.unsafe_set_internal_section_ref Text)
  else ND.text ()

(* Name of current function *)
let function_name = ref ""

(* Keep the name of the current block section to get back to it after emitting
   data. *)
let current_basic_block_section = ref ""

let emit_function_or_basic_block_section_name () =
  let suffix =
    if String.length !current_basic_block_section = 0
    then ""
    else "." ^ !current_basic_block_section
  in
  emit_named_text_section !function_name ~suffix

let emit_Llabel fallthrough lbl section_name =
  (if !Flambda_backend_flags.basic_block_sections
  then
    match section_name with
    | Some name ->
      if not (String.equal name !current_basic_block_section)
      then (
        current_basic_block_section := name;
        ND.cfi_endproc ();
        emit_function_or_basic_block_section_name ();
        ND.cfi_startproc ())
    | None -> ());
  if (not fallthrough) && !fastcode_flag
  then ND.align ~data_section:false ~bytes:4;
  ND.define_label lbl

(* Output a pseudo-register *)

let x86_data_type_for_stack_slot : Cmm.machtype_component -> X86_ast.data_type =
  function
  | Float -> REAL8
  | Vec128 -> VEC128
  | Valx2 -> VEC128
  | Int | Addr | Val -> QWORD
  | Float32 -> REAL4

let reg : Reg.t -> X86_ast.arg =
 fun reg ->
  match reg with
  | { loc = Reg.Reg r; typ = ty; _ } -> register_name ty r
  | { loc = Stack (Domainstate n); typ = ty; _ } ->
    let ofs = n + (Domainstate.(idx_of_field Domain_extra_params) * 8) in
    mem64 (x86_data_type_for_stack_slot ty) ofs R14
  | { loc = Stack ((Reg.Local _ | Incoming _ | Outgoing _) as s); typ = ty; _ }
    as r ->
    let ofs = slot_offset s (Stack_class.of_machtype r.typ) in
    mem64 (x86_data_type_for_stack_slot ty) ofs RSP
  | { loc = Unknown; _ } -> assert false

let reg64 = function
  | { loc = Reg.Reg r; _ } -> int_reg_name.(r)
  | { loc = Stack _ | Unknown; _ } -> assert false

let res i n = reg i.res.(n)

let arg i n = reg i.arg.(n)

(* Output a reference to the lower 8, 16 or 32 bits of a register *)

let reg_low_8_name = Array.map (fun r -> X86_ast.Reg8L r) int_reg_name

let reg_low_16_name = Array.map (fun r -> X86_ast.Reg16 r) int_reg_name

let reg_low_32_name = Array.map (fun r -> X86_ast.Reg32 r) int_reg_name

let emit_subreg tbl typ r =
  match r.loc with
  | Reg.Reg r when r < 13 -> tbl.(r)
  | Stack s -> mem64 typ (slot_offset s (Stack_class.of_machtype r.Reg.typ)) RSP
  | Reg _ | Unknown -> assert false

let arg8 i n = emit_subreg reg_low_8_name BYTE i.arg.(n)

let arg16 i n = emit_subreg reg_low_16_name WORD i.arg.(n)

let arg32 i n = emit_subreg reg_low_32_name DWORD i.arg.(n)

let arg64 i n = reg64 i.arg.(n)

let res8 i n = emit_subreg reg_low_8_name BYTE i.res.(n)

let res16 i n = emit_subreg reg_low_16_name WORD i.res.(n)

let res32 i n = emit_subreg reg_low_32_name DWORD i.res.(n)

(* Output an addressing mode *)

let addressing addr typ i n =
  match addr with
  | Ibased (sym_name, sym_global, ofs) ->
    add_used_symbol sym_name;
    let sym_global : Cmm.is_global =
      match sym_global with Global -> Global | Local -> Local
    in
    mem64_rip typ (emit_cmm_symbol_str { sym_name; sym_global }) ~ofs
  | Iindexed d -> mem64 typ d (arg64 i n)
  | Iindexed2 d -> mem64 typ ~base:(arg64 i n) d (arg64 i (n + 1))
  | Iscaled (2, d) -> mem64 typ ~base:(arg64 i n) d (arg64 i n)
  | Iscaled (scale, d) -> mem64 typ ~scale d (arg64 i n)
  | Iindexed2scaled (scale, d) ->
    mem64 typ ~scale ~base:(arg64 i n) d (arg64 i (n + 1))

(* Record live pointers at call points -- see Emitaux *)

(* CR sspies: Consider whether more of [record_frame_label] can be shared with
   the Arm backend. *)

let record_frame_label live dbg =
  let lbl = Cmm.new_label () in
  let live_offset = ref [] in
  Reg.Set.iter
    (fun (r : Reg.t) ->
      match r with
      | { typ = Val; loc = Reg r; _ } ->
        assert (Reg_class.gc_regs_offset Val r = r);
        live_offset := ((r lsl 1) + 1) :: !live_offset
      | { typ = Val; loc = Stack s; _ } as reg ->
        live_offset
          := slot_offset s (Stack_class.of_machtype reg.typ) :: !live_offset
      | { typ = Valx2; loc = Reg r; _ } ->
        let n = Reg_class.gc_regs_offset Valx2 r in
        let encode n = (n lsl 1) + 1 in
        live_offset := encode n :: encode (n + 1) :: !live_offset
      | { typ = Valx2; loc = Stack s; _ } as reg ->
        let n = slot_offset s (Stack_class.of_machtype reg.typ) in
        live_offset := n :: (n + Arch.size_addr) :: !live_offset
      | { typ = Addr; _ } as r ->
        Misc.fatal_errorf "bad GC root %a" Printreg.reg r
      | { typ = Val | Valx2; loc = Unknown; _ } as r ->
        Misc.fatal_errorf "Unknown location %a" Printreg.reg r
      | { typ = Int | Float | Float32 | Vec128; _ } -> ())
    live;
  (* CR sspies: Consider changing [record_frame_descr] to [Asm_label.t] instead
     of linear labels. *)
  record_frame_descr ~label:lbl ~frame_size:(frame_size ())
    ~live_offset:!live_offset dbg;
  label_to_asm_label ~section:Text lbl

let record_frame live dbg =
  let lbl = record_frame_label live dbg in
  ND.define_label lbl

(* Record calls to the GC -- we've moved them out of the way *)

type gc_call =
  { gc_lbl : L.t; (* Entry label *)
    gc_return_lbl : L.t; (* Where to branch after GC *)
    gc_frame : L.t; (* Label of frame descriptor *)
    gc_dbg : Debuginfo.t (* Location of the original instruction *)
  }

let call_gc_sites = ref ([] : gc_call list)

let call_gc_local_sym : Cmm.symbol =
  { sym_name = "caml_call_gc_"; sym_global = Local }

let emit_call_gc gc =
  ND.define_label gc.gc_lbl;
  emit_debug_info gc.gc_dbg;
  emit_call call_gc_local_sym;
  ND.define_label gc.gc_frame;
  I.jmp (sym (L.encode gc.gc_return_lbl))

(* Record calls to local stack reallocation *)

type local_realloc_call =
  { lr_lbl : L.t;
    lr_return_lbl : L.t;
    lr_dbg : Debuginfo.t
  }

let local_realloc_sites = ref ([] : local_realloc_call list)

let emit_local_realloc lr =
  ND.define_label lr.lr_lbl;
  emit_debug_info lr.lr_dbg;
  emit_call (Cmm.global_symbol "caml_call_local_realloc");
  I.jmp (sym (L.encode lr.lr_return_lbl))

(* Record calls to caml_ml_array_bound_error and caml_ml_array_align_error. In
   -g mode we maintain one call per bound check site. Without -g, we can share a
   single call. *)

type safety_check =
  | Bound_check
  | Align_check

type safety_check_failure =
  { sc_lbl : L.t; (* Entry label *)
    sc_frame : L.t; (* Label of frame descriptor *)
    sc_dbg : Debuginfo.t (* As for [gc_call]. *)
  }

type safety_check_sites =
  { mutable sc_sites : safety_check_failure list;
    mutable sc_call : L.t option
  }

let bound_checks = { sc_sites = []; sc_call = None }

let align_checks = { sc_sites = []; sc_call = None }

let emit_call_safety_error kind sc =
  ND.define_label sc.sc_lbl;
  emit_debug_info sc.sc_dbg;
  (match kind with
  | Bound_check -> emit_call (Cmm.global_symbol "caml_ml_array_bound_error")
  | Align_check -> emit_call (Cmm.global_symbol "caml_ml_array_align_error"));
  ND.define_label sc.sc_frame

let clear_safety_checks () =
  bound_checks.sc_sites <- [];
  bound_checks.sc_call <- None;
  align_checks.sc_sites <- [];
  align_checks.sc_call <- None

let emit_call_safety_errors () =
  List.iter (emit_call_safety_error Bound_check) bound_checks.sc_sites;
  (match bound_checks.sc_call with
  | None -> ()
  | Some sc_call ->
    ND.define_label sc_call;
    emit_call (Cmm.global_symbol "caml_ml_array_bound_error"));
  List.iter (emit_call_safety_error Align_check) align_checks.sc_sites;
  match align_checks.sc_call with
  | None -> ()
  | Some sc_call ->
    ND.define_label sc_call;
    emit_call (Cmm.global_symbol "caml_ml_array_align_error")

(* Stack reallocation *)
type stack_realloc =
  { sc_label : L.t; (* Label of the reallocation code. *)
    sc_return : L.t; (* Label to return to after reallocation. *)
    sc_size_in_bytes : int (* Size for reallocation. *)
  }

let stack_realloc = ref ([] : stack_realloc list)

let clear_stack_realloc () = stack_realloc := []

let emit_stack_realloc () =
  List.iter
    (fun { sc_label; sc_return; sc_size_in_bytes } ->
      ND.define_label sc_label;
      (* Pass the desired frame size on the stack, since all of the
         argument-passing registers may be in use. Also serves to align the
         stack properly before the call *)
      I.push (int (Config.stack_threshold + (sc_size_in_bytes / 8)));
      ND.cfi_adjust_cfa_offset ~bytes:8;
      (* measured in words *)
      emit_call (Cmm.global_symbol "caml_call_realloc_stack");
      I.add (int 8) rsp;
      ND.cfi_adjust_cfa_offset ~bytes:(-8);
      I.jmp (sym (L.encode sc_return)))
    !stack_realloc

let emit_stack_check ~size_in_bytes ~save_registers =
  let overflow = L.create Text and ret = L.create Text in
  let threshold_offset =
    (Domainstate.stack_ctx_words * 8) + Stack_check.stack_threshold_size
  in
  if save_registers then I.push r10;
  I.lea (mem64 NONE (-(size_in_bytes + threshold_offset)) RSP) r10;
  I.cmp (domain_field Domainstate.Domain_current_stack) r10;
  if save_registers then I.pop r10;
  I.jb (sym (L.encode overflow));
  ND.define_label ret;
  stack_realloc
    := { sc_label = overflow;
         sc_return = ret;
         sc_size_in_bytes = size_in_bytes
       }
       :: !stack_realloc

(* Record jump tables *)
type jump_table =
  { table_lbl : L.t;
    elems : Linear.label array
  }

let jump_tables = ref ([] : jump_table list)

let emit_jump_table t =
  ND.define_label t.table_lbl;
  for i = 0 to Array.length t.elems - 1 do
    let upper = label_to_asm_label ~section:Text t.elems.(i) in
    ND.between_labels_32_bit ~upper ~lower:t.table_lbl ()
  done

let emit_jump_tables () =
  ND.align ~data_section:true ~bytes:4;
  List.iter emit_jump_table !jump_tables;
  jump_tables := []

(* Names for instructions *)

let instr_for_intop = function
  | Iadd -> I.add
  | Isub -> I.sub
  | Imul -> fun arg1 arg2 -> I.imul arg1 (Some arg2)
  | Iand -> I.and_
  | Ior -> I.or_
  | Ixor -> I.xor
  | Ilsl -> I.sal
  | Ilsr -> I.shr
  | Iasr -> I.sar
  | Idiv | Imod | Ipopcnt | Imulh _ | Iclz _ | Ictz _ | Icomp _ -> assert false

let instr_for_floatop (width : Cmm.float_width) op =
  match width, op with
  | Float64, Iaddf -> I.addsd
  | Float64, Isubf -> I.subsd
  | Float64, Imulf -> I.mulsd
  | Float64, Idivf -> I.divsd
  | Float32, Iaddf -> I.addss
  | Float32, Isubf -> I.subss
  | Float32, Imulf -> I.mulss
  | Float32, Idivf -> I.divss
  | (Float32 | Float64), (Inegf | Iabsf | Icompf _) -> assert false

let instr_for_floatarithmem (width : Cmm.float_width) op =
  match width, op with
  | Float64, Ifloatadd -> I.addsd
  | Float64, Ifloatsub -> I.subsd
  | Float64, Ifloatmul -> I.mulsd
  | Float64, Ifloatdiv -> I.divsd
  | Float32, Ifloatadd -> I.addss
  | Float32, Ifloatsub -> I.subss
  | Float32, Ifloatmul -> I.mulss
  | Float32, Ifloatdiv -> I.divss

let cond : Operation.integer_comparison -> X86_ast.condition = function
  | Isigned Ceq -> E
  | Isigned Cne -> NE
  | Isigned Cle -> LE
  | Isigned Cgt -> G
  | Isigned Clt -> L
  | Isigned Cge -> GE
  | Iunsigned Ceq -> E
  | Iunsigned Cne -> NE
  | Iunsigned Cle -> BE
  | Iunsigned Cgt -> A
  | Iunsigned Clt -> B
  | Iunsigned Cge -> AE

(* Output an = 0 or <> 0 test. *)

let output_test_zero arg =
  match arg.loc with
  | Reg.Reg _ -> I.test (reg arg) (reg arg)
  | Stack _ -> I.cmp (int 0) (reg arg)
  | Unknown ->
    Misc.fatal_errorf "Emit.output_test_zero: arg location unknown: %a"
      Printreg.reg arg

(* Output a floating-point compare and branch *)

let emit_float_test (width : Cmm.float_width) (cmp : Cmm.float_comparison) i
    ~(taken : X86_ast.condition -> unit) =
  (*= Effect of comisd on flags and conditional branches:
                     ZF PF CF  cond. branches taken
        unordered     1  1  1  je, jb, jbe, jp
        >             0  0  0  jne, jae, ja
        <             0  0  1  jne, jbe, jb
        =             1  0  0  je, jae, jbe.
     If FP traps are on (they are off by default),
     comisd traps on QNaN and SNaN but ucomisd traps on SNaN only.
  *)
  let ucomi, comi =
    match width with
    | Float64 -> I.ucomisd, I.comisd
    | Float32 -> I.ucomiss, I.comiss
  in
  match cmp with
  | CFeq when equal_arg (arg i 1) (arg i 0) ->
    ucomi (arg i 1) (arg i 0);
    taken NP
  | CFeq ->
    let next = L.create Text in
    ucomi (arg i 1) (arg i 0);
    I.jp (sym (L.encode next));
    (* skip if unordered *)
    taken E;
    (* branch taken if x=y *)
    ND.define_label next
  | CFneq when equal_arg (arg i 1) (arg i 0) ->
    ucomi (arg i 1) (arg i 0);
    taken P
  | CFneq ->
    ucomi (arg i 1) (arg i 0);
    taken P;
    (* branch taken if unordered *)
    taken NE (* branch taken if x<y or x>y *)
  | CFlt ->
    comi (arg i 0) (arg i 1);
    taken A (* branch taken if y>x i.e. x<y *)
  | CFnlt ->
    comi (arg i 0) (arg i 1);
    taken BE (* taken if unordered or y<=x i.e. !(x<y) *)
  | CFle ->
    comi (arg i 0) (arg i 1);
    (* swap compare *)
    taken AE (* branch taken if y>=x i.e. x<=y *)
  | CFnle ->
    comi (arg i 0) (arg i 1);
    (* swap compare *)
    taken B (* taken if unordered or y<x i.e. !(x<=y) *)
  | CFgt ->
    comi (arg i 1) (arg i 0);
    taken A (* branch taken if x>y *)
  | CFngt ->
    comi (arg i 1) (arg i 0);
    taken BE (* taken if unordered or x<=y i.e. !(x>y) *)
  | CFge ->
    comi (arg i 1) (arg i 0);
    (* swap compare *)
    taken AE (* branch taken if x>=y *)
  | CFnge ->
    comi (arg i 1) (arg i 0);
    (* swap compare *)
    taken B (* taken if unordered or x<y i.e. !(x>=y) *)

let emit_test i ~(taken : X86_ast.condition -> unit) = function
  | Itruetest ->
    output_test_zero i.arg.(0);
    taken NE
  | Ifalsetest ->
    output_test_zero i.arg.(0);
    taken E
  | Iinttest cmp ->
    I.cmp (arg i 1) (arg i 0);
    taken (cond cmp)
  | Iinttest_imm
      (((Isigned Ceq | Isigned Cne | Iunsigned Ceq | Iunsigned Cne) as cmp), 0)
    ->
    output_test_zero i.arg.(0);
    taken (cond cmp)
  | Iinttest_imm
      ( (( Isigned (Ceq | Cne | Clt | Cgt | Cle | Cge)
         | Iunsigned (Ceq | Cne | Clt | Cgt | Cle | Cge) ) as cmp),
        n ) ->
    I.cmp (int n) (arg i 0);
    taken (cond cmp)
  | Ifloattest (width, cmp) -> emit_float_test width cmp i ~taken
  | Ioddtest ->
    I.test (int 1) (arg8 i 0);
    taken NE
  | Ieventest ->
    I.test (int 1) (arg8 i 0);
    taken E

(* Deallocate the stack frame before a return or tail call *)

let output_epilogue f =
  if !frame_required
  then (
    let n = frame_size () - 8 - if fp then 8 else 0 in
    if n <> 0
    then (
      I.add (int n) rsp;
      ND.cfi_adjust_cfa_offset ~bytes:(-n));
    if fp then I.pop rbp;
    f ();
    (* reset CFA back cause function body may continue *)
    if n <> 0 then ND.cfi_adjust_cfa_offset ~bytes:n)
  else f ()

(* Floating-point constants *)

let float_constants = ref ([] : (int64 * L.t) list)

let add_float_constant cst =
  try List.assoc cst !float_constants
  with Not_found ->
    let lbl = L.create Eight_byte_literals in
    float_constants := (cst, lbl) :: !float_constants;
    lbl

let emit_float_constant f lbl =
  ND.define_label lbl;
  ND.float64_from_bits f

(* Vector constants *)

let vec128_constants = ref ([] : (Cmm.vec128_bits * L.t) list)

let add_vec128_constant bits =
  try List.assoc bits !vec128_constants
  with Not_found ->
    let lbl = L.create Sixteen_byte_literals in
    vec128_constants := (bits, lbl) :: !vec128_constants;
    lbl

let emit_vec128_constant ({ high; low } : Cmm.vec128_bits) lbl =
  (* SIMD vectors respect little-endian byte order *)
  ND.define_label lbl;
  ND.float64_from_bits low;
  ND.float64_from_bits high

let global_maybe_protected (sym : S.t) =
  ND.global sym;
  if !Flambda_backend_flags.symbol_visibility_protected
  then
    (* CR sspies: This match should probably moved into asm directives. Check
       what Arm does. *)
    match system with
    | S_macosx | S_win32 | S_win64 | S_mingw64 | S_cygwin | S_mingw | S_unknown
      ->
      ()
    | S_gnu | S_solaris | S_linux_elf | S_bsd_elf | S_beos | S_linux | S_freebsd
    | S_netbsd | S_openbsd ->
      (* Global symbols can be marked as being protected. Unlike in C we don't
         want them to be preempted as we're doing a lot of cross module
         inlining. *)
      ND.protected sym

(* CR sspies: The naming of these functions is confusing. *)
let emit_global_label_for_symbol ~section lbl =
  add_def_symbol lbl;
  let lbl = S.create lbl in
  global_maybe_protected lbl;
  ND.define_symbol_label ~section lbl

let emit_global_label ~section s =
  let lbl = Cmm_helpers.make_symbol s in
  emit_global_label_for_symbol ~section lbl

let move (src : Reg.t) (dst : Reg.t) =
  let distinct = not (Reg.same_loc src dst) in
  match src.typ, src.loc, dst.typ, dst.loc with
  | _, Stack _, _, Stack _ ->
    Misc.fatal_errorf "Illegal move between registers (%a to %a)\n" Printreg.reg
      src Printreg.reg dst
  | Float, Reg _, Float, Reg _
  | Float32, Reg _, Float32, Reg _
  | ( (Vec128 | Valx2),
      (Reg _ | Stack _),
      (Vec128 | Valx2),
      (Reg _ | Stack _ (* Vec128 stack slots are always aligned. *)) ) ->
    if distinct then I.movapd (reg src) (reg dst)
  | Float, (Reg _ | Stack _), Float, (Reg _ | Stack _) ->
    if distinct then I.movsd (reg src) (reg dst)
  | Float32, (Reg _ | Stack _), Float32, (Reg _ | Stack _) ->
    if distinct then I.movss (reg src) (reg dst)
  | (Int | Val | Addr), (Reg _ | Stack _), (Int | Val | Addr), (Reg _ | Stack _)
    ->
    if distinct then I.mov (reg src) (reg dst)
  | _, Unknown, _, (Reg _ | Stack _ | Unknown)
  | _, (Reg _ | Stack _), _, Unknown ->
    Misc.fatal_errorf
      "Illegal move with an unknown register location (%a to %a)\n" Printreg.reg
      src Printreg.reg dst
  | ( (Float | Float32 | Vec128 | Int | Val | Addr | Valx2),
      (Reg _ | Stack _),
      _,
      _ ) ->
    Misc.fatal_errorf
      "Illegal move between registers of differing types (%a to %a)\n"
      Printreg.reg src Printreg.reg dst

let stack_to_stack_move (src : Reg.t) (dst : Reg.t) =
  assert (Cmm.equal_machtype_component src.typ dst.typ);
  if not (Reg.equal_location src.loc dst.loc)
  then
    match src.typ with
    | Int | Val ->
      (* Not calling move because r15 is not in int_reg_name. *)
      I.mov (reg src) r15;
      I.mov r15 (reg dst)
    | Float | Addr | Vec128 | Valx2 | Float32 ->
      Misc.fatal_errorf
        "Unexpected register type for stack to stack move: from %a to %a\n"
        Printreg.reg src Printreg.reg dst

let move_allowing_stack_to_stack src dst =
  match Reg.is_stack src, Reg.is_stack dst with
  | true, true -> stack_to_stack_move src dst
  | _ -> move src dst

(* Entry point for tail recursive calls *)
let tailrec_entry_point = ref None

(* Emit tracing probes *)

type probe =
  { stack_offset : int;
    num_stack_slots : int Stack_class.Tbl.t;
    (* Record frame info held in the corresponding mutable variables. *)
    probe_label : label;
    (* Probe site, recorded in .note.stapsdt section for enabling and disabling
       the probes *)
    probe_insn : Linear.instruction
        (* Iprobe instruction, recorded at probe site and used for emitting the
           notes and the wrapper code at the end of the compilation unit. *)
  }

let probe_handler_wrapper_name probe_label =
  let w = Printf.sprintf "probe_wrapper_%s" (Label.to_string probe_label) in
  Cmm_helpers.make_symbol w |> S.create

let probes = ref []

let probe_semaphores = ref String.Map.empty

let stapsdt_base_emitted = ref false

let reset_probes () =
  probes := [];
  probe_semaphores := String.Map.empty

let find_or_add_semaphore name enabled_at_init dbg =
  match String.Map.find_opt name !probe_semaphores with
  | Some (label, e) ->
    (match e, enabled_at_init with
    | None, None -> ()
    | None, Some _ ->
      let d = label, enabled_at_init in
      probe_semaphores
        := String.Map.remove name !probe_semaphores |> String.Map.add name d
    | Some _, None ->
      (* [find_or_add_semaphore] is called with None for Iprobe_is_enabled
         during code emission only. [find_or_add_semaphore] us called with Some
         to emit probe notes only after all code is emitted. *)
      assert false
    | Some b, Some b' ->
      if not (Bool.equal b b')
      then raise (Emitaux.Error (Inconsistent_probe_init (name, dbg))));
    label
  | None ->
    let sym = "caml_probes_semaphore_" ^ name in
    let d = sym, enabled_at_init in
    probe_semaphores := String.Map.add name d !probe_semaphores;
    sym

let emit_call_probe_handler_wrapper i ~enabled_at_init ~probe_label =
  assert !frame_required;
  let wrap_label = probe_handler_wrapper_name probe_label in
  (* We emit a cmp instruction that is effectively a nop: it only sets flags
     that are not read anywhere. To enable the probe, cmp is replaced with call
     by changing the first byte of the encoding from 3d to e8. The operand of
     the call is a displacement relative to the next instruction. Hence, the
     immediate operand of cmp is set up to have that value. *)
  if enabled_at_init
  then I.call (sym (S.encode wrap_label))
  else if !Clflags.pic_code
  then (
    (* Manually emit encoding of cmp and an explicit relocation on it as needed
       for a call instruction, to ensure a correct result, instead of relying on
       an assembler that might choose a different encoding which produces an
       incorrect relocation and changes the meaning of the program. *)
    (* Emit the required encoding of "cmp $0, %eax" directly using .byte *)
    ND.int8 (Numbers.Int8.of_int_exn 0x3d);
    ND.int8 (Numbers.Int8.of_int_exn 0);
    ND.int8 (Numbers.Int8.of_int_exn 0);
    ND.int8 (Numbers.Int8.of_int_exn 0);
    ND.int8 (Numbers.Int8.of_int_exn 0);
    (* Emit the relocation for the call target *)
    (* [rel_size] is the number of bytes taken by the operand of cmp/call that
       needs to be relocated. It is used to form reloc's offset.
       [rel_offset_from_next] is the distance from the start of the relocated
       bytes to the start to the next instruction after the call. It used to
       form reloc's expr for call's operand (see above). [rel_size] is equal to
       [rel_offset_from_next] because the relocated operand is the last one and
       arch is little endian. *)
    let rel_size = 4L in
    let rel_offset_from_next = 4L in
    ND.reloc_x86_64_plt32 ~offset_from_this:rel_size ~target_symbol:wrap_label
      ~rel_offset_from_next)
  else
    (* Emit absolute value, no relocation. The immediate operand of cmp is the
       offset of the wrapper from the current instruction's address "." minus
       the length of the current instruction, which is 5, and the wrapper is
       emitted at the end of the compilation unit. *)
    I.cmp (sym (Printf.sprintf "%s - . - 5" (S.encode wrap_label))) eax;
  (* Live registers are saved by the probe wrapper, so they are not recorded as
     gc roots at the probe site. *)
  let stack_live = Reg.Set.filter Reg.is_stack i.live in
  record_frame stack_live (Dbg_other i.dbg)

(* Emit trap handler notes *)

type traps =
  { mutable push_traps : L.t list;
    mutable pop_traps : L.t list;
    mutable enter_traps : L.Set.t
  }

let traps = { push_traps = []; pop_traps = []; enter_traps = L.Set.empty }

let reset_traps () =
  traps.push_traps <- [];
  traps.pop_traps <- [];
  traps.enter_traps <- L.Set.empty

let emit_pop_trap_label () =
  let lbl = L.create Text in
  ND.define_label lbl;
  traps.pop_traps <- lbl :: traps.pop_traps

let emit_push_trap_label handler =
  let lbl = L.create Text in
  ND.define_label lbl;
  traps.push_traps <- lbl :: traps.push_traps;
  traps.enter_traps <- L.Set.add handler traps.enter_traps

(* Emit Code *)

module Address_sanitizer : sig
  type memory_access =
    | Load
    | Store_initialize
    | Store_modify

  (** Implements [https://github.com/google/sanitizers/wiki/AddressSanitizerAlgorithm#mapping]. *)
  val emit_sanitize :
    ?dependencies:X86_ast.arg array ->
    address:X86_ast.arg ->
    Cmm.memory_chunk ->
    memory_access ->
    unit
end = struct
  type memory_access =
    | Load
    | Store_initialize
    | Store_modify

  module Memory_chunk_size : sig
    type t

    val of_memory_chunk : Cmm.memory_chunk -> t

    val to_bytes : t -> int

    val to_bytes_log2 : t -> int

    val is_small : t -> bool
  end = struct
    type t =
      | I8
      | I16
      | I32
      | I64
      | I128

    let of_memory_chunk : Cmm.memory_chunk -> t = function
      | Byte_unsigned | Byte_signed -> I8
      | Sixteen_unsigned | Sixteen_signed -> I16
      | Thirtytwo_unsigned | Thirtytwo_signed | Single _ -> I32
      | Word_int | Word_val | Double -> I64
      | Onetwentyeight_unaligned | Onetwentyeight_aligned -> I128

    external to_bytes_log2 : t -> int = "%identity"

    let to_bytes t = 1 lsl to_bytes_log2 t

    let is_small = function I8 | I16 | I32 -> true | I64 | I128 -> false
  end

  let mov_address src dest =
    match (src : X86_ast.arg) with
    | Mem
        { scale = 1;
          base = None;
          sym = None;
          displ = 0;
          idx;
          arch = _;
          typ = _
        } ->
      I.mov (Reg64 idx) dest
    | Mem _ | Mem64_RIP _ | Imm _ | Sym _ | Reg8L _ | Reg8H _ | Reg16 _
    | Reg32 _ | Reg64 _ | Regf _ ->
      I.lea src dest

  let[@inline always] is_stack_16_byte_aligned () =
    (* Yes, sadly this does result in materially better assembly than
       [(!stack_offset mod 16) = 0]
       https://github.com/ocaml-flambda/flambda-backend/issues/2187 *)
    !stack_offset land 15 = 0

  let asan_report_function memory_chunk_size memory_access : X86_ast.arg =
    let index =
      (Memory_chunk_size.to_bytes_log2 memory_chunk_size lsl 1)
      +
      match memory_access with
      | Load -> 0
      | Store_initialize | Store_modify -> 1
    in
    (* We take extra care to structure our code such that these are statically
       allocated as manifest constants in a flat array. *)
    match index with
    | 0 -> Sym "caml_asan_report_load1_noabort"
    | 1 -> Sym "caml_asan_report_store1_noabort"
    | 2 -> Sym "caml_asan_report_load2_noabort"
    | 3 -> Sym "caml_asan_report_store2_noabort"
    | 4 -> Sym "caml_asan_report_load4_noabort"
    | 5 -> Sym "caml_asan_report_store4_noabort"
    | 6 -> Sym "caml_asan_report_load8_noabort"
    | 7 -> Sym "caml_asan_report_store8_noabort"
    | 8 -> Sym "caml_asan_report_load16_noabort"
    | 9 -> Sym "caml_asan_report_store16_noabort"
    | _ ->
      (* Larger loads and stores can be reported using
         [__asan_report_load_n_noabort], but we don't support this yet. *)
      assert false

  (* CR-soon ksvetlitski: find a way to accomplish this without breaking the
     abstraction barrier of [X86_ast]. *)
  let[@inline always] uses_register register (arg : X86_ast.arg) =
    match arg with
    | Reg8L register' | Reg16 register' | Reg32 register' | Reg64 register' ->
      equal_reg64 register register'
    | Mem { idx = register'; base = None; scale; _ } ->
      scale <> 0 && equal_reg64 register register'
    | Mem { idx = register'; base = Some register''; _ } ->
      equal_reg64 register register' || equal_reg64 register register''
    | Imm _ | Sym _ | Reg8H _ | Regf _ | Mem64_RIP (_, _, _) -> false

  (* The C code snippets in the comments throughout this function refer to the
     implementation given in
     [https://github.com/google/sanitizers/wiki/AddressSanitizerAlgorithm#mapping].
     I'd recommend reading that first for reference before touching this
     function. *)
  let emit_sanitize ?(dependencies = [||]) ~address
      (memory_chunk : Cmm.memory_chunk) (memory_access : memory_access) =
    let[@inline always] need_to_save_register register =
      uses_register register address
      || Array.exists (uses_register register) dependencies
    in
    let memory_chunk_size = Memory_chunk_size.of_memory_chunk memory_chunk in
    (* -------- Begin prologue -------- *)
    let need_to_save_rdi = need_to_save_register RDI in
    if need_to_save_rdi then push rdi;
    (* For the remainder of this function [rdi] will hold [address]. It's vital
       that we do this now before we change the contents of any other registers,
       because we don't want to clobber any of [address]'s component registers.

       You could do this at the end of the prologue if you wanted to; the point
       is just that you have to do this before you modify the contents of any
       registers (other than [rsp]). *)
    mov_address address rdi;
    let need_to_save_r11 = need_to_save_register R11 in
    if need_to_save_r11 then push r11;
    let need_to_save_r10 =
      Memory_chunk_size.is_small memory_chunk_size && need_to_save_register R10
    in
    if need_to_save_r10 then push r10;
    (* -------- End prologue -------- *)
    let asan_check_succeded_label = L.create Text in
    I.mov rdi r11;
    (* These constants come from
       [https://github.com/google/sanitizers/wiki/AddressSanitizerAlgorithm#64-bit]. *)
    I.shr (int 3) r11;
    let shadow_address = mem64 BYTE 0x7FFF8000 R11 in
    let () =
      if not (Memory_chunk_size.is_small memory_chunk_size)
      then (
        I.cmp (int 0) shadow_address;
        I.je (sym (L.encode asan_check_succeded_label))
        (* There is no slow-path check for word-sized and larger accesses *))
      else (
        I.movzx shadow_address r11;
        I.test (Reg8L R11) (Reg8L R11);
        I.je (sym (L.encode asan_check_succeded_label));
        (* Begin the [SlowPathCheck]. Place [last_accessed_byte] in [r10]. ```
           last_accessed_byte = (address & 7) + kAccessSize - 1; ``` *)
        I.mov rdi r10;
        I.and_ (int 7) r10;
        let () =
          (* [ + kAccessSize - 1 ] *)
          match Memory_chunk_size.to_bytes memory_chunk_size with
          | 1 -> ()
          | 2 -> I.inc r10
          | 4 -> I.add (int 3) r10
          | _ -> assert false
        in
        (* [ return (last_accessed_byte >= shadow_value) ] *)
        I.cmp (Reg8L R11) (Reg8L R10);
        I.jl (sym (L.encode asan_check_succeded_label)))
    in
    (* [ ReportError(address, kAccessSize, kIsWrite); ] *)
    let () =
      let need_to_align_stack = not (is_stack_16_byte_aligned ()) in
      if need_to_align_stack
      then
        (* [push rax] is a single-byte instruction, as opposed to something like
           [push 0] which is a 2-byte instruction. *)
        push rax;
      (* The asan report wrappers use a special calling convention via the C
         attribute [__attribute__((preserve_all))] so that all registers except
         for [r11] (which is clobbered) are callee-saved, in order to minimize
         the amount of spilling we have to do here. [address] is already in
         [rdi], and this function accepts just a single argument. *)
      I.call (asan_report_function memory_chunk_size memory_access);
      if need_to_align_stack then pop rax
    in
    ND.define_label asan_check_succeded_label;
    if need_to_save_r10 then pop r10;
    if need_to_save_r11 then pop r11;
    if need_to_save_rdi then pop rdi

  let[@inline always] emit_sanitize ?dependencies ~address memory_chunk
      memory_access =
    (* Checking [Config.with_address_sanitizer] is redundant, but we do it
       because it's a compile-time constant, so it enables the compiler to
       completely optimize-out the AddressSanitizer code when the compiler was
       configured without it. *)
    if Config.with_address_sanitizer && !Arch.is_asan_enabled
    then
      match memory_access with
      (* We can elide the ASAN check for stores made to initialize record fields
         on the grounds that the backing memory for freshly allocated records is
         provided directly by the runtime and guaranteed to be safe to use. *)
      | Store_initialize -> ()
      | Load | Store_modify ->
        emit_sanitize ?dependencies ~address memory_chunk memory_access
end

let emit_atomic instr (op : Cmm.atomic_op) (size : Cmm.atomic_bitwidth) addr =
  let first_memory_arg_index =
    match op with
    | Compare_set -> 2
    | Fetch_and_add -> 1
    | Add | Sub | Land | Lor | Lxor -> 1
    | Exchange -> 1
    | Compare_exchange -> 2
  in
  let src_index = first_memory_arg_index - 1 in
  let typ, src =
    match size with
    | Thirtytwo -> X86_ast.DWORD, arg32 instr src_index
    | Sixtyfour | Word -> X86_ast.QWORD, arg instr src_index
  in
  let dst = addressing addr typ instr first_memory_arg_index in
  Address_sanitizer.emit_sanitize ~dependencies:[| src |] ~address:dst
    Thirtytwo_unsigned Store_modify;
  match op with
  | Fetch_and_add ->
    assert (Reg.same_loc instr.res.(0) instr.arg.(0));
    I.lock_xadd src dst
  | Add -> I.lock_add src dst
  | Sub -> I.lock_sub src dst
  | Land -> I.lock_and src dst
  | Lor -> I.lock_or src dst
  | Lxor -> I.lock_xor src dst
  | Compare_set ->
    (* compare_with is already in rax, set_to is src *)
    assert (Reg.is_reg instr.arg.(1));
    assert (Reg.same_loc instr.arg.(0) phys_rax);
    let res8, res = res8 instr 0, res instr 0 in
    I.lock_cmpxchg src dst;
    I.set E res8;
    I.movzx res8 res
  | Compare_exchange ->
    (* compare_with is already in rax, set_to is src, res in rax *)
    assert (Reg.is_reg instr.arg.(1));
    assert (Reg.same_loc instr.arg.(0) phys_rax);
    assert (Reg.same_loc instr.res.(0) phys_rax);
    I.lock_cmpxchg src dst
  | Exchange ->
    (* no need for a "lock" prefix for XCHG with a memory operand *)
    assert (Reg.is_reg instr.arg.(0));
    I.xchg src dst

let emit_reinterpret_cast (cast : Cmm.reinterpret_cast) i =
  let distinct = not (Reg.same_loc i.arg.(0) i.res.(0)) in
  match cast with
  | Int_of_value | Value_of_int -> if distinct then I.mov (arg i 0) (res i 0)
  | Float_of_float32 | Float32_of_float ->
    if distinct then I.movss (arg i 0) (res i 0)
  | V128_of_v128 -> if distinct then I.movapd (arg i 0) (res i 0)
  | Float_of_int64 | Int64_of_float -> I.movq (arg i 0) (res i 0)
  | Float32_of_int32 -> I.movd (arg32 i 0) (res i 0)
  | Int32_of_float32 -> I.movd (arg i 0) (res32 i 0)

let emit_static_cast (cast : Cmm.static_cast) i =
  let distinct = not (Reg.same_loc i.arg.(0) i.res.(0)) in
  match cast with
  | Float_of_int Float64 -> I.cvtsi2sd (arg i 0) (res i 0)
  | Int_of_float Float64 -> I.cvttsd2si (arg i 0) (res i 0)
  | Float_of_int Float32 -> I.cvtsi2ss (arg i 0) (res i 0)
  | Int_of_float Float32 -> I.cvttss2si (arg i 0) (res i 0)
  | Float_of_float32 -> I.cvtss2sd (arg i 0) (res i 0)
  | Float32_of_float -> I.cvtsd2ss (arg i 0) (res i 0)
  | V128_of_scalar Float64x2 | Scalar_of_v128 Float64x2 ->
    if distinct then I.movsd (arg i 0) (res i 0)
  | Scalar_of_v128 Int64x2 | V128_of_scalar Int64x2 ->
    I.movq (arg i 0) (res i 0)
  | Scalar_of_v128 Int32x4 -> I.movd (arg i 0) (res32 i 0)
  | V128_of_scalar Int32x4 -> I.movd (arg32 i 0) (res i 0)
  | V128_of_scalar Float32x4 | Scalar_of_v128 Float32x4 ->
    if distinct then I.movss (arg i 0) (res i 0)
  | Scalar_of_v128 Int16x8 ->
    (* [movw] and [movzx] cannot operate on vector registers. We must zero
       extend as the result is an untagged positive int. CR mslater: (SIMD)
       remove zx once we have unboxed int16 *)
    I.movd (arg i 0) (res32 i 0);
    I.movzx (res16 i 0) (res i 0)
  | Scalar_of_v128 Int8x16 ->
    (* [movb] and [movzx] cannot operate on vector registers. We must zero
       extend as the result is an untagged positive int. CR mslater: (SIMD)
       remove zx once we have unboxed int8 *)
    I.movd (arg i 0) (res32 i 0);
    I.movzx (res8 i 0) (res i 0)
  | V128_of_scalar Int16x8 | V128_of_scalar Int8x16 ->
    (* [movw] and [movb] cannot operate on vector registers. Moving 32 bits is
       OK because the argument is an untagged positive int and these operations
       leave the top bits of the vector unspecified. CR mslater: (SIMD) don't
       load 32 bits once we have unboxed int16/int8 *)
    I.movd (arg32 i 0) (res i 0)

let assert_loc (loc : Simd.loc) arg =
  (match Reg.is_reg arg with
  | true -> assert (Simd.loc_allows_reg loc)
  | false -> assert (Simd.loc_allows_mem loc));
  match Simd.loc_is_pinned loc with
  | Some RAX -> assert (Reg.same_loc arg phys_rax)
  | Some RCX -> assert (Reg.same_loc arg phys_rcx)
  | Some RDX -> assert (Reg.same_loc arg phys_rdx)
  | Some XMM0 -> assert (Reg.same_loc arg (phys_xmm0v ()))
  | None -> ()

let check_simd_instr (simd : Simd.instr) imm instr =
  assert (Bool.equal simd.imm (Option.is_some imm));
  Array.iteri
    (fun j (arg : Simd.arg) -> assert_loc arg.loc instr.arg.(j))
    simd.args;
  match simd.res with
  | First_arg -> assert (Reg.same_loc instr.arg.(0) instr.res.(0))
  | Res { loc; _ } -> assert_loc loc instr.res.(0)

let to_arg_with_width loc instr i =
  match Simd.loc_requires_width loc with
  | Some Eight -> arg8 instr i
  | Some Sixteen -> arg16 instr i
  | Some Thirtytwo -> arg32 instr i
  | Some Sixtyfour | None -> arg instr i

let to_res_with_width loc instr i =
  match Simd.loc_requires_width loc with
  | Some Eight -> res8 instr i
  | Some Sixteen -> res16 instr i
  | Some Thirtytwo -> res32 instr i
  | Some Sixtyfour | None -> res instr i

let emit_simd_instr (simd : Simd.instr) imm instr =
  check_simd_instr simd imm instr;
  let total_args = Array.length instr.arg in
  if total_args <> Array.length simd.args
  then Misc.fatal_errorf "wrong number of arguments for %s" simd.mnemonic;
  let args =
    List.init total_args (fun i ->
        if Simd.arg_is_implicit simd.args.(i)
        then None
        else Some (to_arg_with_width simd.args.(i).loc instr i))
    |> List.filter_map (fun arg -> arg)
  in
  let args =
    match simd.res with
    | First_arg | Res { enc = Implicit; _ } -> args
    | Res { loc; enc = RM_r | RM_rm | Vex_v } -> (
      match Simd.loc_is_pinned loc with
      | Some _ -> args
      | None -> to_res_with_width loc instr 0 :: args)
  in
  let args =
    match imm with
    | None -> List.rev args
    | Some imm -> X86_dsl.int imm :: List.rev args
  in
  I.simd simd (Array.of_list args)

let emit_simd (op : Simd.operation) instr =
  let imm = op.imm in
  match op.instr with
  | Instruction simd -> emit_simd_instr simd imm instr
  | Sequence seq -> (
    match seq.id with
    | Sqrtss | Sqrtsd | Roundss | Roundsd ->
      (* Avoids partial register stall *)
      if not (equal_arg (arg instr 0) (res instr 0))
      then I.xorpd (res instr 0) (res instr 0);
      emit_simd_instr seq.instr imm instr
    | Pcompare_string p ->
      let cond : X86_ast.condition =
        match p with
        | Pcmpestra -> A
        | Pcmpestrc -> B
        | Pcmpestro -> O
        | Pcmpestrs -> S
        | Pcmpestrz -> E
        | Pcmpistra -> A
        | Pcmpistrc -> B
        | Pcmpistro -> O
        | Pcmpistrs -> S
        | Pcmpistrz -> E
      in
      emit_simd_instr seq.instr imm instr;
      I.set cond (res8 instr 0);
      I.movzx (res8 instr 0) (res instr 0))

let emit_simd_instr_with_memory_arg (simd : Simd.Mem.operation) i addr =
  let open Simd_instrs in
  assert (Reg.is_reg i.arg.(0));
  assert (not (Reg.is_reg i.arg.(1)));
  assert (Reg.same_loc i.arg.(0) i.res.(0));
  match simd with
  | SSE2 Add_f64 -> I.simd addpd [| addr; res i 0 |]
  | SSE2 Sub_f64 -> I.simd subpd [| addr; res i 0 |]
  | SSE2 Mul_f64 -> I.simd mulpd [| addr; res i 0 |]
  | SSE2 Div_f64 -> I.simd divpd [| addr; res i 0 |]
  | SSE Add_f32 -> I.simd addps [| addr; res i 0 |]
  | SSE Sub_f32 -> I.simd subps [| addr; res i 0 |]
  | SSE Mul_f32 -> I.simd mulps [| addr; res i 0 |]
  | SSE Div_f32 -> I.simd divps [| addr; res i 0 |]

(* Emit an instruction *)
let emit_instr ~first ~fallthrough i =
  emit_debug_info_linear i;
  match i.desc with
  | Lend -> ()
  | Lprologue ->
    assert !prologue_required;
    if fp
    then (
      I.push rbp;
      ND.cfi_adjust_cfa_offset ~bytes:8;
      I.mov rsp rbp);
    if !frame_required
    then
      let n = frame_size () - 8 - if fp then 8 else 0 in
      if n <> 0
      then (
        I.sub (int n) rsp;
        ND.cfi_adjust_cfa_offset ~bytes:n)
  | Lop (Move | Spill | Reload) -> move i.arg.(0) i.res.(0)
  | Lop (Const_int n) ->
    if Nativeint.equal n 0n
    then
      match i.res.(0).loc with
      | Reg _ ->
        (* Clearing the bottom half also clears the top half (except for
           64-bit-only registers where the behaviour is as if the operands were
           64 bit). *)
        I.xor (res32 i 0) (res32 i 0)
      | Stack _ -> I.mov (int 0) (res i 0)
      | Unknown ->
        Misc.fatal_errorf "Unknown register location %a\n" Printreg.reg
          i.res.(0)
    else if Nativeint.compare n 0n > 0 && Nativeint.compare n 0xFFFF_FFFFn <= 0
    then
      match i.res.(0).loc with
      | Reg _ ->
        (* Similarly, setting only the bottom half clears the top half. *)
        I.mov (nat n) (res32 i 0)
      | Stack _ -> I.mov (nat n) (res i 0)
      | Unknown ->
        Misc.fatal_errorf "Unknown register location %a\n" Printreg.reg
          i.res.(0)
    else I.mov (nat n) (res i 0)
  | Lop (Const_float32 f) -> (
    match f with
    | 0x0000_0000l ->
      (* +0.0 *)
      I.xorpd (res i 0) (res i 0)
    | _ ->
      (* float32 constants take up 8 bytes when we emit them with
         [float_literal] (see the conversion from int32 to int64 below). Thus,
         we load the lower half. Note that this is different from Cmm 32-bit
         floats ([Csingle]), which are emitted as 4-byte constants. *)
      let lbl = add_float_constant (Int64.of_int32 f) in
      I.movss (mem64_rip REAL4 (L.encode lbl)) (res i 0))
  | Lop (Const_float f) -> (
    match f with
    | 0x0000_0000_0000_0000L ->
      (* +0.0 *)
      I.xorpd (res i 0) (res i 0)
    | _ ->
      let lbl = add_float_constant f in
      I.movsd (mem64_rip REAL8 (L.encode lbl)) (res i 0))
  | Lop (Const_vec128 { high; low }) -> (
    match high, low with
    | 0x0000_0000_0000_0000L, 0x0000_0000_0000_0000L ->
      I.xorpd (res i 0) (res i 0)
    | _ ->
      let lbl = add_vec128_constant { high; low } in
      I.movapd (mem64_rip VEC128 (L.encode lbl)) (res i 0))
  | Lop (Const_symbol s) ->
    add_used_symbol s.sym_name;
    load_symbol_addr s (res i 0)
  | Lcall_op Lcall_ind ->
    I.call (arg i 0);
    record_frame i.live (Dbg_other i.dbg)
  | Lcall_op (Lcall_imm { func }) ->
    add_used_symbol func.sym_name;
    emit_call func;
    record_frame i.live (Dbg_other i.dbg)
  | Lcall_op Ltailcall_ind -> output_epilogue (fun () -> I.jmp (arg i 0))
  | Lcall_op (Ltailcall_imm { func }) ->
    if String.equal func.sym_name !function_name
    then
      match !tailrec_entry_point with
      | None -> Misc.fatal_error "jump to missing tailrec entry point"
      | Some tailrec_entry_point ->
        let tailrec_entry_point =
          label_to_asm_label ~section:Text tailrec_entry_point
        in
        I.jmp (sym (L.encode tailrec_entry_point))
    else
      output_epilogue (fun () ->
          add_used_symbol func.sym_name;
          emit_jump func)
  | Lcall_op (Lextcall { func; alloc; stack_ofs; _ }) ->
    add_used_symbol func;
    if Config.runtime5 && stack_ofs > 0
    then (
      I.mov rsp r13;
      I.lea (mem64 QWORD stack_ofs RSP) r12;
      load_symbol_addr (Cmm.global_symbol func) rax;
      emit_call (Cmm.global_symbol "caml_c_call_stack_args");
      record_frame i.live (Dbg_other i.dbg))
    else if alloc
    then (
      load_symbol_addr (Cmm.global_symbol func) rax;
      emit_call (Cmm.global_symbol "caml_c_call");
      record_frame i.live (Dbg_other i.dbg);
      if (not Config.runtime5) && not (is_win64 system)
      then
        (* In amd64.S, "caml_c_call" tail-calls the C function (in order to
           produce nicer backtraces), so we need to restore r15 manually after
           it returns (note that this increases code size).

           In amd64nt.asm (used for Win64), "caml_c_call" invokes the C function
           via a regular call, and restores r15 itself, thus avoiding the code
           size increase. *)
        I.mov (domain_field Domainstate.Domain_young_ptr) r15)
    else (
      if Config.runtime5
      then (
        I.mov rsp rbx;
        ND.cfi_remember_state ();
        ND.cfi_def_cfa_register ~reg:"rbx";
        (* NB: gdb has asserts on contiguous stacks that mean it will not unwind
           through this unless we were to tag this calling frame with
           cfi_signal_frame in it's definition. *)
        I.mov (domain_field Domainstate.Domain_c_stack) rsp);
      emit_call (Cmm.global_symbol func);
      if Config.runtime5
      then (
        I.mov rbx rsp;
        ND.cfi_restore_state ()))
  | Lop (Stackoffset n) -> emit_stack_offset n
  | Lop (Load { memory_chunk; addressing_mode; _ }) -> (
    let[@inline always] load ~dest data_type instruction =
      let address = addressing addressing_mode data_type i 0 in
      Address_sanitizer.emit_sanitize ~address memory_chunk Load;
      instruction address dest
    in
    match memory_chunk with
    | Word_int | Word_val -> load ~dest:(res i 0) QWORD I.mov
    | Byte_unsigned -> load ~dest:(res i 0) BYTE I.movzx
    | Byte_signed -> load ~dest:(res i 0) BYTE I.movsx
    | Sixteen_unsigned -> load ~dest:(res i 0) WORD I.movzx
    | Sixteen_signed -> load ~dest:(res i 0) WORD I.movsx
    | Thirtytwo_unsigned -> load ~dest:(res32 i 0) DWORD I.mov
    | Thirtytwo_signed -> load ~dest:(res i 0) DWORD I.movsxd
    | Onetwentyeight_unaligned -> load ~dest:(res i 0) VEC128 I.movupd
    | Onetwentyeight_aligned -> load ~dest:(res i 0) VEC128 I.movapd
    | Single { reg = Float64 } -> load ~dest:(res i 0) REAL4 I.cvtss2sd
    | Single { reg = Float32 } -> load ~dest:(res i 0) REAL4 I.movss
    | Double -> load ~dest:(res i 0) REAL8 I.movsd)
  | Lop (Store (chunk, addr, is_modify)) -> (
    let memory_access : Address_sanitizer.memory_access =
      if is_modify then Store_modify else Store_initialize
    in
    let[@inline always] store data_type arg_func instruction =
      let address = addressing addr data_type i 1 in
      let src = arg_func i 0 in
      Address_sanitizer.emit_sanitize ~dependencies:[| src |] ~address chunk
        memory_access;
      instruction src address
    in
    match chunk with
    | Word_int | Word_val -> store QWORD arg I.mov
    | Byte_unsigned | Byte_signed -> store BYTE arg8 I.mov
    | Sixteen_unsigned | Sixteen_signed -> store WORD arg16 I.mov
    | Thirtytwo_signed | Thirtytwo_unsigned -> store DWORD arg32 I.mov
    | Onetwentyeight_unaligned -> store VEC128 arg I.movupd
    | Onetwentyeight_aligned -> store VEC128 arg I.movapd
    | Single { reg = Float64 } ->
      let src = arg i 0 in
      I.cvtsd2ss src xmm15;
      let address = addressing addr REAL4 i 1 in
      Address_sanitizer.emit_sanitize ~dependencies:[| src; xmm15 |] ~address
        chunk memory_access;
      I.movss xmm15 address
    | Single { reg = Float32 } -> store REAL4 arg I.movss
    | Double -> store REAL8 arg I.movsd)
  | Lop (Specific (Istore_int (n, addr, is_modify))) ->
    let address = addressing addr QWORD i 0 in
    let src = nat n in
    let memory_access : Address_sanitizer.memory_access =
      if is_modify then Store_modify else Store_initialize
    in
    Address_sanitizer.emit_sanitize ~dependencies:[| src |] ~address Word_int
      memory_access;
    I.mov src address
  | Lop (Alloc { bytes = n; dbginfo; mode = Heap }) ->
    assert (n <= (Config.max_young_wosize + 1) * Arch.size_addr);
    if !fastcode_flag
    then (
      I.sub (int n) r15;
      I.cmp (domain_field Domainstate.Domain_young_limit) r15;
      let lbl_call_gc = L.create Text in
      let lbl_frame = record_frame_label i.live (Dbg_alloc dbginfo) in
      I.jb (sym (L.encode lbl_call_gc));
      let lbl_after_alloc = L.create Text in
      ND.define_label lbl_after_alloc;
      I.lea (mem64 NONE 8 R15) (res i 0);
      call_gc_sites
        := { gc_lbl = lbl_call_gc;
             gc_return_lbl = lbl_after_alloc;
             gc_dbg = i.dbg;
             gc_frame = lbl_frame
           }
           :: !call_gc_sites)
    else (
      (match n with
      | 16 -> emit_call (Cmm.global_symbol "caml_alloc1")
      | 24 -> emit_call (Cmm.global_symbol "caml_alloc2")
      | 32 -> emit_call (Cmm.global_symbol "caml_alloc3")
      | _ ->
        I.sub (int n) r15;
        emit_call (Cmm.global_symbol "caml_allocN"));
      let label = record_frame_label i.live (Dbg_alloc dbginfo) in
      ND.define_label label;
      I.lea (mem64 NONE 8 R15) (res i 0))
  | Lop (Alloc { bytes = n; dbginfo = _; mode = Local }) ->
    let r = res i 0 in
    I.mov (domain_field Domainstate.Domain_local_sp) r;
    I.sub (int n) r;
    I.mov r (domain_field Domainstate.Domain_local_sp);
    I.cmp (domain_field Domainstate.Domain_local_limit) r;
    let lbl_call = L.create Text in
    I.j L (sym (L.encode lbl_call));
    let lbl_after_alloc = L.create Text in
    ND.define_label lbl_after_alloc;
    I.add (domain_field Domainstate.Domain_local_top) r;
    I.add (int 8) r;
    local_realloc_sites
      := { lr_lbl = lbl_call; lr_dbg = i.dbg; lr_return_lbl = lbl_after_alloc }
         :: !local_realloc_sites
  | Lop Poll ->
    I.cmp (domain_field Domainstate.Domain_young_limit) r15;
    let gc_call_label = L.create Text in
    let lbl_after_poll = L.create Text in
    let lbl_frame = record_frame_label i.live (Dbg_alloc []) in
    I.jbe (sym (L.encode gc_call_label));
    call_gc_sites
      := { gc_lbl = gc_call_label;
           gc_return_lbl = lbl_after_poll;
           gc_dbg = i.dbg;
           gc_frame = lbl_frame
         }
         :: !call_gc_sites;
    ND.define_label lbl_after_poll
  | Lop (Intop (Icomp cmp)) ->
    I.cmp (arg i 1) (arg i 0);
    I.set (cond cmp) al;
    I.movzx al (res i 0)
  | Lop (Intop_imm (Icomp cmp, n)) ->
    I.cmp (int n) (arg i 0);
    I.set (cond cmp) al;
    I.movzx al (res i 0)
  | Lop (Intop_imm (Iand, n))
    when n >= 0 && n <= 0xFFFF_FFFF && Reg.is_reg i.res.(0) ->
    I.and_ (int n) (res32 i 0)
  | Lop (Intop Ixor)
    when Reg.equal_location i.arg.(1).loc i.res.(0).loc && Reg.is_reg i.res.(0)
    ->
    I.xor (res32 i 0) (res32 i 0)
  | Lop (Intop (Idiv | Imod)) ->
    I.cqo ();
    I.idiv (arg i 1)
  | Lop (Intop ((Ilsl | Ilsr | Iasr) as op)) ->
    (* We have i.arg.(0) = i.res.(0) and i.arg.(1) = %rcx *)
    instr_for_intop op cl (res i 0)
  | Lop (Intop (Imulh { signed = true })) -> I.imul (arg i 1) None
  | Lop (Intop (Imulh { signed = false })) -> I.mul (arg i 1)
  | Lop (Intop ((Iadd | Isub | Imul | Iand | Ior | Ixor) as op)) ->
    (* We have i.arg.(0) = i.res.(0) *)
    instr_for_intop op (arg i 1) (res i 0)
  | Lop (Intop_imm (Iadd, n))
    when not (Reg.equal_location i.arg.(0).loc i.res.(0).loc) ->
    I.lea (mem64 NONE n (arg64 i 0)) (res i 0)
  | Lop (Intop_imm (Iadd, 1) | Intop_imm (Isub, -1)) -> I.inc (res i 0)
  | Lop (Intop_imm (Iadd, -1) | Intop_imm (Isub, 1)) -> I.dec (res i 0)
  | Lop (Intop_imm (op, n)) ->
    (* We have i.arg.(0) = i.res.(0) *)
    instr_for_intop op (int n) (res i 0)
  | Lop (Intop_atomic { op; size; addr }) -> emit_atomic i op size addr
  | Lop (Floatop (Float64, Icompf cmp)) ->
    let cond, need_swap = float_cond_and_need_swap cmp in
    let a0, a1 = if need_swap then arg i 1, arg i 0 else arg i 0, arg i 1 in
    I.cmpsd cond a1 a0;
    I.movq a0 (res i 0);
    I.neg (res i 0)
  | Lop (Floatop (Float32, Icompf cmp)) ->
    let cond, need_swap = float_cond_and_need_swap cmp in
    let a0, a1 = if need_swap then arg i 1, arg i 0 else arg i 0, arg i 1 in
    I.cmpss cond a1 a0;
    I.movd a0 (res32 i 0);
    (* CMPSS only sets the bottom 32 bits of the result, so we sign-extend to
       copy the result to the top 32 bits. *)
    I.movsxd (res32 i 0) (res i 0);
    I.neg (res i 0)
  | Lop (Floatop (Float64, Inegf)) ->
    I.xorpd (mem64_rip VEC128 (emit_symbol "caml_negf_mask")) (res i 0)
  | Lop (Floatop (Float64, Iabsf)) ->
    I.andpd (mem64_rip VEC128 (emit_symbol "caml_absf_mask")) (res i 0)
  | Lop (Floatop (Float32, Inegf)) ->
    I.xorps (mem64_rip VEC128 (emit_symbol "caml_negf32_mask")) (res i 0)
  | Lop (Floatop (Float32, Iabsf)) ->
    I.andps (mem64_rip VEC128 (emit_symbol "caml_absf32_mask")) (res i 0)
  | Lop (Floatop (width, ((Iaddf | Isubf | Imulf | Idivf) as floatop))) ->
    instr_for_floatop width floatop (arg i 1) (res i 0)
  | Lop Opaque -> assert (Reg.equal_location i.arg.(0).loc i.res.(0).loc)
  | Lop (Specific (Ilea addr)) -> I.lea (addressing addr NONE i 0) (res i 0)
  | Lop (Specific (Ioffset_loc (n, addr))) ->
    I.add (int n) (addressing addr QWORD i 0)
  | Lop (Specific (Ifloatarithmem (Float64, op, addr))) ->
    let address = addressing addr REAL8 i 1 in
    let dest = res i 0 in
    Address_sanitizer.emit_sanitize ~dependencies:[| dest |] ~address Double
      Load;
    instr_for_floatarithmem Float64 op address dest
  | Lop (Specific (Ifloatarithmem (Float32, op, addr))) ->
    let address = addressing addr REAL4 i 1 in
    let dest = res i 0 in
    Address_sanitizer.emit_sanitize ~dependencies:[| dest |] ~address
      (Single { reg = Float32 })
      Load;
    instr_for_floatarithmem Float32 op address dest
  | Lop (Specific (Ibswap { bitwidth = Sixteen })) ->
    I.xchg ah al;
    I.movzx (res16 i 0) (res i 0)
  | Lop (Specific (Ibswap { bitwidth = Thirtytwo })) -> I.bswap (res32 i 0)
  | Lop (Specific (Ibswap { bitwidth = Sixtyfour })) -> I.bswap (res i 0)
  | Lop (Specific Isextend32) -> I.movsxd (arg32 i 0) (res i 0)
  | Lop (Specific Izextend32) -> I.mov (arg32 i 0) (res32 i 0)
  | Lop (Intop (Iclz { arg_is_non_zero })) ->
    (* CR-someday gyorsh: can we do it at selection? mshinwell: We need to
       address this and the similar CRs below. My feeling is that we should try
       to do this earlier, based on previous experience with similar things, but
       maybe the change should be left for later. mshinwell: The current
       situation is fine for now. *)
    if Arch.Extension.enabled LZCNT
    then I.lzcnt (arg i 0) (res i 0)
    else if arg_is_non_zero
    then (
      (* No need to handle that bsr is undefined on 0 input. *)
      I.bsr (arg i 0) (res i 0);
      (* We need (63 - result_of_bsr), which can be done with xor. *)
      I.xor (int 63) (res i 0))
    else
      let lbl_z = L.create Text in
      let lbl_nz = L.create Text in
      I.bsr (arg i 0) (res i 0);
      I.je (sym (L.encode lbl_z));
      I.xor (int 63) (res i 0);
      I.jmp (sym (L.encode lbl_nz));
      ND.define_label lbl_z;
      I.mov (int 64) (res i 0);
      ND.define_label lbl_nz
  | Lop (Intop (Ictz { arg_is_non_zero })) ->
    (* CR-someday gyorsh: can we do it at selection? *)
    if Arch.Extension.enabled BMI
    then I.tzcnt (arg i 0) (res i 0)
    else if arg_is_non_zero
    then
      (* No need to handle that bsf is undefined on 0 input. *)
      I.bsf (arg i 0) (res i 0)
    else
      let lbl_nz = L.create Text in
      I.bsf (arg i 0) (res i 0);
      I.jne (sym (L.encode lbl_nz));
      I.mov (int 64) (res i 0);
      ND.define_label lbl_nz
  | Lop (Intop Ipopcnt) ->
    assert (Arch.Extension.enabled POPCNT);
    I.popcnt (arg i 0) (res i 0)
  | Lop (Csel tst) ->
    let len = Array.length i.arg in
    let ifso = i.arg.(len - 2) in
    let ifnot = i.arg.(len - 1) in
    assert (Reg.same_loc ifnot i.res.(0));
    let taken c = I.cmov c (reg ifso) (res i 0) in
    emit_test i tst ~taken
  | Lop (Specific Irdtsc) -> (
    I.rdtsc ();
    let rdx = X86_ast.Reg64 RDX in
    (* The instruction fills in the low 32 bits of the result registers. *)
    (* Combine edx and eax into a single 64-bit result. *)
    I.sal (int 32) rdx;
    (* shift edx to the high part of rdx *)
    (* On processors that support the Intel 64 architecture, the high-order 32
       bits of each of RAX and RDX are cleared. *)
    match reg64 i.res.(0) with
    | RAX -> I.or_ rdx (res i 0) (* combine high and low into rax *)
    | RDX -> I.or_ rax (res i 0) (* combine high and low into rdx *)
    | RBX | RCX | RSP | RBP | RSI | RDI | R8 | R9 | R10 | R11 | R12 | R13 | R14
    | R15 ->
      (* combine high and low into res *)
      I.mov rax (res i 0);
      I.or_ rdx (res i 0))
  | Lop (Specific Irdpmc) ->
    assert (equal_reg64 (arg64 i 0) RCX);
    I.rdpmc ();
    let rdx = X86_ast.Reg64 RDX in
    (* The instruction fills in the low 32 bits of the result registers. *)
    (* Combine edx and eax into a single 64-bit result. *)
    I.sal (int 32) rdx;
    (* shift edx to the high part of rdx *)
    I.mov eax (res32 i 0);
    (* zero-extend eax *)
    I.or_ rdx (res i 0)
    (* combine high and low into rax *)
  | Lop (Specific Ilfence) -> I.lfence ()
  | Lop (Specific Isfence) -> I.sfence ()
  | Lop (Specific Imfence) -> I.mfence ()
  | Lop (Specific Ipackf32) ->
    let arg0, arg1 = i.arg.(0), i.arg.(1) in
    assert (Reg.is_reg arg0 && Reg.is_reg arg1 && Reg.same_loc arg0 i.res.(0));
    I.simd Simd_instrs.unpcklps [| arg i 1; res i 0 |]
  | Lop (Specific (Isimd op)) -> emit_simd op i
  | Lop (Specific (Isimd_mem (op, addressing_mode))) ->
    let address = addressing addressing_mode VEC128 i 1 in
    Address_sanitizer.emit_sanitize
      ~dependencies:[| res i 0 |]
      ~address Onetwentyeight_unaligned Store_modify;
    emit_simd_instr_with_memory_arg op i address
  | Lop (Static_cast cast) -> emit_static_cast cast i
  | Lop (Reinterpret_cast cast) -> emit_reinterpret_cast cast i
  | Lop (Specific Ipause) -> I.pause ()
  | Lop (Specific (Icldemote addr)) ->
    let address = addressing addr QWORD i 0 in
    (* This isn't really a [Load] or a [Store], but it is closer to [Store]
       semantically. *)
    Address_sanitizer.emit_sanitize ~address Word_val Store_modify;
    I.cldemote address
  | Lop (Specific (Iprefetch { is_write; locality; addr })) ->
    let address = addressing addr QWORD i 0 in
    let memory_access : Address_sanitizer.memory_access =
      if is_write then Store_modify else Load
    in
    (* While it is *technically* legal to issue a prefetch to an invalid
       address, it comes with a performance penalty. Quoting from the Intel
       Optimization Reference Manual section 9.3.1: > Prefetching to addresses
       that are not mapped to physical pages can experience non-deterministic
       performance penalty. For example specifying a NULL pointer (0L) as
       address for a prefetch can cause long delays.

       On these grounds I would consider prefetching invalid addresses to
       usually be a bug, and so we do sanitize such accesses. *)
    Address_sanitizer.emit_sanitize ~address Word_val memory_access;
    let locality : X86_ast.prefetch_temporal_locality_hint =
      match locality with
      | Nonlocal -> Nta
      | Low -> T2
      | Moderate -> T1
      | High -> T0
    in
    I.prefetch is_write locality address
  | Lop Begin_region ->
    I.mov (domain_field Domainstate.Domain_local_sp) (res i 0)
  | Lop End_region -> I.mov (arg i 0) (domain_field Domainstate.Domain_local_sp)
  | Lop (Name_for_debugger _) -> ()
  | Lcall_op (Lprobe { enabled_at_init; _ }) ->
    let probe_label = Cmm.new_label () in
    let probe =
      { probe_label;
        probe_insn = i;
        stack_offset = !stack_offset;
        num_stack_slots = Stack_class.Tbl.copy num_stack_slots
      }
    in
    probes := probe :: !probes;
    ND.define_label (label_to_asm_label ~section:Text probe_label);
    I.nop ();
    (* for uprobes and usdt probes as well *)
    (* A probe site does not directly call the probe handler. There is an
       intervening wrapper that deals with getting the arguments to the probe in
       the correct place and managing the spilling/reloading of live registers.
       See [emit_probe_handler_wrapper] below. *)
    emit_call_probe_handler_wrapper i ~enabled_at_init ~probe_label
  | Lop (Probe_is_enabled { name }) ->
    let semaphore_sym = find_or_add_semaphore name None i.dbg in
    (* Load unsigned 2-byte integer value of the semaphore. According to the
       documentation [1], semaphores are of type unsigned short. [1]
       https://sourceware.org/systemtap/wiki/UserSpaceProbeImplementation *)
    (* OCaml has it's own semaphore, in addition to system tap, to control ocaml
       probe handlers independently from stap probe handlers. It is placed
       immediately after stap semaphore, and is the same size - hence offset
       2. *)
    I.mov (addressing (Ibased (semaphore_sym, Global, 2)) WORD i 0) (res16 i 0);
    (* If the semaphore is 0, then the result is 0, otherwise 1. *)
    I.cmp (int 0) (res16 i 0);
    I.set (cond (Iunsigned Cne)) (res8 i 0);
    I.movzx (res8 i 0) (res i 0)
  | Lop Dls_get ->
    if Config.runtime5
    then I.mov (domain_field Domainstate.Domain_dls_root) (res i 0)
    else Misc.fatal_error "Dls is not supported in runtime4."
  | Lreloadretaddr -> ()
  | Lreturn -> output_epilogue (fun () -> I.ret ())
  | Llabel { label = lbl; section_name } ->
    let lbl = label_to_asm_label ~section:Text lbl in
    emit_Llabel fallthrough lbl section_name
  | Lbranch lbl ->
    let lbl = label_to_asm_label ~section:Text lbl in
    I.jmp (sym (L.encode lbl))
  | Lcondbranch (tst, lbl) ->
    let lbl = label_to_asm_label ~section:Text lbl in
    emit_test i tst ~taken:(fun c -> I.j c (sym (L.encode lbl)))
  | Lcondbranch3 (lbl0, lbl1, lbl2) -> (
    I.cmp (int 1) (arg i 0);
    (match lbl0 with
    | None -> ()
    | Some lbl ->
      let lbl = label_to_asm_label ~section:Text lbl in
      I.jb (sym (L.encode lbl)));
    (match lbl1 with
    | None -> ()
    | Some lbl ->
      let lbl = label_to_asm_label ~section:Text lbl in
      I.je (sym (L.encode lbl)));
    match lbl2 with
    | None -> ()
    | Some lbl ->
      let lbl = label_to_asm_label ~section:Text lbl in
      I.ja (sym (L.encode lbl)))
  | Lswitch jumptbl ->
    let lbl = L.create Text in
    (* rax and rdx are clobbered by the Lswitch, meaning that no variable that
       is live across the Lswitch is assigned to rax or rdx. However, the
       argument to Lswitch can still be assigned to one of these two registers,
       so we must be careful not to clobber it before use. *)
    let tmp1, tmp2 =
      if Reg.equal_location i.arg.(0).loc (Reg 0) (* rax *)
      then phys_rdx, phys_rax
      else phys_rax, phys_rdx
    in
    I.lea (mem64_rip NONE (L.encode lbl)) (reg tmp1);
    I.movsxd (mem64 DWORD 0 (arg64 i 0) ~scale:4 ~base:(reg64 tmp1)) (reg tmp2);
    I.add (reg tmp2) (reg tmp1);
    I.jmp (reg tmp1);
    let table = { table_lbl = lbl; elems = jumptbl } in
    jump_tables := table :: !jump_tables
  | Lentertrap ->
    if fp
    then
      let delta = frame_size () - 16 (* retaddr + rbp *) in
      I.lea (mem64 NONE delta RSP) rbp
  | Ladjust_stack_offset { delta_bytes } ->
    ND.cfi_adjust_cfa_offset ~bytes:delta_bytes;
    stack_offset := !stack_offset + delta_bytes
  | Lpushtrap { lbl_handler } ->
    let lbl_handler = label_to_asm_label ~section:Text lbl_handler in
    emit_push_trap_label lbl_handler;
    let load_label_addr s arg =
      if !Clflags.pic_code
      then I.lea (mem64_rip NONE (L.encode s)) arg
      else I.mov (sym (L.encode s)) arg
    in
    load_label_addr lbl_handler r11;
    I.push r11;
    ND.cfi_adjust_cfa_offset ~bytes:8;
    I.push (domain_field Domainstate.Domain_exn_handler);
    ND.cfi_adjust_cfa_offset ~bytes:8;
    I.mov rsp (domain_field Domainstate.Domain_exn_handler);
    stack_offset := !stack_offset + 16
  | Lpoptrap _ ->
    emit_pop_trap_label ();
    I.pop (domain_field Domainstate.Domain_exn_handler);
    ND.cfi_adjust_cfa_offset ~bytes:(-8);
    I.add (int 8) rsp;
    ND.cfi_adjust_cfa_offset ~bytes:(-8);
    stack_offset := !stack_offset - 16
  | Lraise k -> (
    match k with
    | Lambda.Raise_regular ->
      I.mov (int 0) (domain_field Domainstate.Domain_backtrace_pos);
      emit_call (Cmm.global_symbol "caml_raise_exn");
      record_frame Reg.Set.empty (Dbg_raise i.dbg)
    | Lambda.Raise_reraise ->
      emit_call
        (Cmm.global_symbol
           (if Config.runtime5 then "caml_reraise_exn" else "caml_raise_exn"));
      record_frame Reg.Set.empty (Dbg_raise i.dbg)
    | Lambda.Raise_notrace ->
      I.mov (domain_field Domainstate.Domain_exn_handler) rsp;
      I.pop (domain_field Domainstate.Domain_exn_handler);
      I.pop r11;
      I.jmp r11)
  | Lstackcheck { max_frame_size_bytes } ->
    emit_stack_check ~size_in_bytes:max_frame_size_bytes
      ~save_registers:(not first)

let emit_instr ~first ~fallthrough i =
  try emit_instr ~first ~fallthrough i
  with exn ->
    Format.eprintf "Exception whilst emitting instruction:@ %a\n"
      Printlinear.instr i;
    raise exn

let rec emit_all ~first ~fallthrough i =
  match i.desc with
  | Lend -> ()
  | Lprologue | Lreloadretaddr | Lreturn | Lentertrap | Lpoptrap _ | Lop _
  | Lcall_op _ | Llabel _ | Lbranch _
  | Lcondbranch (_, _)
  | Lcondbranch3 (_, _, _)
  | Lswitch _ | Ladjust_stack_offset _ | Lpushtrap _ | Lraise _ | Lstackcheck _
    ->
    (try emit_instr ~first ~fallthrough i
     with exn ->
       Format.eprintf "Exception whilst emitting instruction:@ %a\n"
         Printlinear.instr i;
       raise exn);
    emit_all ~first:false ~fallthrough:(Linear.has_fallthrough i.desc) i.next

let all_functions = ref []

let emit_function_type_and_size fun_sym =
  (* Note: Symbol types and sizes are only needed on some platforms/systems.
     These functions check internally whether they are needed. *)
  (* CR sspies: This does not match the old systems comparison exactly. The type
     symbol function checks for [GAS_like], which matches a few more systems
     than the old match. *)
  ND.type_symbol ~ty:Function fun_sym;
  if not !Flambda_backend_flags.basic_block_sections then ND.size fun_sym

(* Emission of a function declaration *)

let fundecl fundecl =
  let fun_end_label, fundecl =
    match Emitaux.Dwarf_helpers.record_dwarf_for_fundecl fundecl with
    | None -> None, fundecl
    | Some { fun_end_label; fundecl } -> Some fun_end_label, fundecl
  in
  function_name := fundecl.fun_name;
  fastcode_flag := fundecl.fun_fast;
  tailrec_entry_point := fundecl.fun_tailrec_entry_point_label;
  contains_calls := fundecl.fun_contains_calls;
  stack_offset := 0;
  call_gc_sites := [];
  local_realloc_sites := [];
  clear_safety_checks ();
  clear_stack_realloc ();
  Stack_class.Tbl.copy_values ~from:fundecl.fun_num_stack_slots
    ~to_:num_stack_slots;
  prologue_required := fundecl.fun_prologue_required;
  frame_required := fundecl.fun_frame_required;
  all_functions := fundecl :: !all_functions;
  current_basic_block_section
    := Option.value fundecl.fun_section_name ~default:"";
  emit_function_or_basic_block_section_name ();
  ND.align ~data_section:false ~bytes:16;
  add_def_symbol fundecl.fun_name;
  let fundecl_sym = S.create fundecl.fun_name in
  if is_macosx system
     && (not !Clflags.output_c_object)
     && is_generic_function fundecl.fun_name
  then (* PR#4690 *)
    ND.private_extern fundecl_sym
  else global_maybe_protected fundecl_sym;
  (* Even if the function name is Local, still emit an actual linker symbol for
     it. This provides symbols for perf, gdb, and similar tools *)
  (* CR sspies: The following two directives should be abstracted into a single function
    in the directives module. *)
  ND.define_symbol_label ~section:Text fundecl_sym;
  ND.define_label (L.create_string_unchecked Text (S.encode fundecl_sym));
  emit_debug_info fundecl.fun_dbg;
  ND.cfi_startproc ();
  if Config.runtime5
     && (not Config.no_stack_checks)
     && String.equal !Clflags.runtime_variant "d"
  then emit_call (Cmm.global_symbol "caml_assert_stack_invariants");
  emit_all ~first:true ~fallthrough:true fundecl.fun_body;
  List.iter emit_call_gc !call_gc_sites;
  List.iter emit_local_realloc !local_realloc_sites;
  emit_call_safety_errors ();
  emit_stack_realloc ();
  (if !frame_required
  then
    let n = frame_size () - 8 - if fp then 8 else 0 in
    if n <> 0 then ND.cfi_adjust_cfa_offset ~bytes:(-n));
  (match fun_end_label with
  | Some l -> ND.define_label (label_to_asm_label ~section:Text l)
  | None -> ());
  ND.cfi_endproc ();
  emit_function_type_and_size fundecl_sym

(* Emission of data *)

(* CR sspies: Share the [emit_item] code with the Arm backend in emitaux. *)
let emit_item : Cmm.data_item -> unit = function
  | Cdefine_symbol s -> (
    let sym = S.create s.sym_name in
    match s.sym_global with
    | Local -> ND.define_label (L.create_string_unchecked Data (S.encode sym))
    | Global ->
      global_maybe_protected sym;
      add_def_symbol s.sym_name;
      (* CR sspies: Figure out why we emit two labels for the function.*)
      ND.define_symbol_label ~section:Data sym;
      ND.define_label (L.create_string_unchecked Data (S.encode sym)))
  | Cint8 n -> ND.int8 (Numbers.Int8.of_int_exn n)
  | Cint16 n -> ND.int16 (Numbers.Int16.of_int_exn n)
  | Cint32 n -> ND.int32 (Numbers.Int64.to_int32_exn (Int64.of_nativeint n))
  (* CR mshinwell: Add [Targetint.of_nativeint] *)
  | Cint n -> ND.targetint (Targetint.of_int64 (Int64.of_nativeint n))
  | Csingle f -> ND.float32 f
  | Cdouble f -> ND.float64 f
  (* SIMD vectors respect little-endian byte order *)
  | Cvec128 { high; low } ->
    ND.float64_from_bits low;
    ND.float64_from_bits high
  | Csymbol_address s -> (
    add_used_symbol s.sym_name;
    match emit_cmm_symbol s with
    | `Symbol s -> ND.symbol s
    | `Label l -> ND.label l)
  | Csymbol_offset (s, o) -> (
    add_used_symbol s.sym_name;
    match emit_cmm_symbol s with
    | `Symbol s ->
      ND.symbol_plus_offset s ~offset_in_bytes:(Targetint.of_int_exn o)
    | `Label l ->
      ND.label_plus_offset l ~offset_in_bytes:(Targetint.of_int_exn o))
  | Cstring s -> ND.string s
  | Cskip n -> ND.space ~bytes:n
  | Calign n -> ND.align ~data_section:true ~bytes:n

let data l =
  ND.data ();
  ND.align ~data_section:true ~bytes:8;
  List.iter emit_item l

(* Beginning / end of an assembly file *)

let reset_all () =
  X86_proc.reset_asm_code ();
  Emitaux.reset ();
  reset_debug_info ();
  (* PR#5603 *)
  reset_imp_table ();
  reset_probes ();
  stapsdt_base_emitted := false;
  reset_traps ();
  float_constants := [];
  all_functions := []

let begin_assembly unix =
  reset_all ();
  if !Flambda_backend_flags.internal_assembler
     && !Emitaux.binary_backend_available
  then X86_proc.register_internal_assembler (Internal_assembler.assemble unix);
  (* We initialize the new assembly directives. *)
  Asm_targets.Asm_label.initialize ~new_label:(fun () ->
      Cmm.new_label () |> Label.to_int);
  ND.initialize ~big_endian:Arch.big_endian
    ~emit_assembly_comments:!Flambda_backend_flags.dasm_comments
      (* As a first step, we emit by calling the corresponding x86 emit
         directives. *) ~emit:(fun d ->
      List.iter directive (to_x86_directive d));
  let code_begin = Cmm_helpers.make_symbol "code_begin" in
  let code_end = Cmm_helpers.make_symbol "code_end" in
  Emitaux.Dwarf_helpers.begin_dwarf ~code_begin ~code_end ~file_emitter;
  if is_win64 system
  then (
    (* These symbols are emitted without additional encoding.*)
    (* CR sspies: Pre-define these symbols in [Asm_symbol]. *)
    ND.extrn (S.create ~already_encoded:true "caml_call_gc");
    ND.extrn (S.create ~already_encoded:true "caml_c_call");
    ND.extrn (S.create ~already_encoded:true "caml_allocN");
    ND.extrn (S.create ~already_encoded:true "caml_alloc1");
    ND.extrn (S.create ~already_encoded:true "caml_alloc2");
    ND.extrn (S.create ~already_encoded:true "caml_alloc3");
    ND.extrn (S.create ~already_encoded:true "caml_ml_array_bound_error");
    ND.extrn (S.create ~already_encoded:true "caml_raise_exn"));
  if !Clflags.dlcode || Arch.win64
  then (
    (* from amd64.S; could emit these constants on demand *)
    ND.switch_to_section Sixteen_byte_literals;
    ND.align ~data_section:true ~bytes:16;
    ND.define_symbol_label ~section:Sixteen_byte_literals
      (S.create "caml_negf_mask");
    ND.int64 0x8000000000000000L;
    ND.int64 0L;
    ND.align ~data_section:true ~bytes:16;
    ND.define_symbol_label ~section:Sixteen_byte_literals
      (S.create "caml_absf_mask");
    ND.int64 0x7FFFFFFFFFFFFFFFL;
    ND.int64 0xFFFFFFFFFFFFFFFFL;
    ND.define_symbol_label ~section:Sixteen_byte_literals
      (S.create "caml_negf32_mask");
    ND.int64 0x80000000L;
    ND.int64 0L;
    ND.align ~data_section:true ~bytes:16;
    ND.define_symbol_label ~section:Sixteen_byte_literals
      (S.create "caml_absf32_mask");
    ND.int64 0xFFFFFFFF7FFFFFFFL;
    ND.int64 0xFFFFFFFFFFFFFFFFL);
  ND.data ();
  emit_global_label ~section:Data "data_begin";
  emit_named_text_section code_begin;
  emit_global_label_for_symbol ~section:Text code_begin;
  if is_macosx system then I.nop ();
  (* PR#4690 *)
  (match emit_cmm_symbol call_gc_local_sym with
  | `Symbol sym -> ND.define_symbol_label ~section:Text sym
  | `Label lbl -> ND.define_label lbl);
  ND.cfi_startproc ();
  I.jmp (rel_plt (Cmm.global_symbol "caml_call_gc"));
  ND.cfi_endproc ();
  ()

let make_stack_loc ~offset n (r : Reg.t) =
  (* Use "Outgoing" stack locations, instead of "Local", because [slot_offset]
     emits (Outgoing n) directly as offset [n] from the stack pointer, rather
     than a calculation relative to the stack frame, which is incorrect for
     naked floats (arising from live variables, not probe arguments) in the
     wrapper's frame. *)
  let loc = Stack (Outgoing (offset + n)) in
  (* Manufacture stack entry with this register's type *)
  Reg.create_at_location r.typ loc

(* CR mshinwell: Not now, but after code review, it would be better to move this
   code so it's contiguous with the other probe code above. *)

(*= Calls to probe handler wrappers do not follow the standard calling
   convention for OCaml.  Instead, all registers are callee saved: the wrapper
   saves and restores all registers that are live across its (unique) call site.

   There is one wrapper per probe site in the code.  Probe wrappers can never
   be called from anywhere except their unique probe site, since they make
   assumptions about the layout of their caller's stack frame.  Probe wrappers
   create their own stack frames; these will be elided in OCaml backtraces
   but may be displayed in platform debuggers such as gdb.

   The current strategy for argument passing to a probe handler is to spill
   all arguments of the probe, plus all live registers, to stack and then
   reload the arguments into locations expected by the handler.
   This is gratuitously inefficient but easier to reason about correctness.
   This problem is a variation of the "sequentialization of a parallel
   assignment" problem, which admits an optimal and efficient solution.

   The stack layout of the wrapper, before the call to the handler, is as
   follows:
   |     ...    |
   |------------|<-- %rsp at probe site, 16-byte aligned by caller
   | ret        |
   |------------|
   | fp         | only present when Config.with_frame_pointers is true
   |------------|
   | live       | saved live registers
   |------------|
   | r15        | spill to use as a temporary for stack to stack move [2]
   |------------|
   | tmp        | spilled reg and stack arguments of the wrapper
   |------------|<-- 16-byte aligned [1]
   | stack args | arguments passed to handler on the stack (size in loc_offset)
   |------------|<-- %rsp at the call from wrapper to handler, 16-byte aligned

   [1] To ensure that the stack before the call from the wrapper to the handler
   is 16-byte aligned, we align this explicitly. It is a little wasteful,
   but reuses the calculation made by [Proc.loc_arguments].

   [2] Stack to stack moves are only used for probe arguments, which must be
   valid OCaml values and not naked floats, because probes are treated like
   calls.
   Therefore, for now, we do not need a temporary register of type float.
   This assumption might no longer hold in the presence of unboxed types.
*)

let size_of_regs regs =
  Array.fold_right
    (fun r acc ->
      match r.Reg.typ with
      | Int | Addr | Val -> acc + size_int
      | Float | Float32 ->
        (* Float32 slots still take up a full word *)
        acc + size_float
      | Vec128 | Valx2 -> acc + size_vec128)
    regs 0

let stack_locations ~offset regs =
  let _, locs =
    Array.fold_right
      (fun r (n, offsets) ->
        let next =
          n
          +
          match r.Reg.typ with
          | Int | Val | Addr -> size_int
          | Float | Float32 ->
            (* Float32 slots still take up a full word *)
            size_float
          | Vec128 | Valx2 -> size_vec128
        in
        next, make_stack_loc n r ~offset :: offsets)
      regs (0, [])
  in
  locs |> Array.of_list

let emit_probe_handler_wrapper p =
  let wrap_label = probe_handler_wrapper_name p.probe_label in
  let probe_name, handler_code_sym =
    match p.probe_insn.desc with
    | Lcall_op (Lprobe { name; handler_code_sym; enabled_at_init = _ }) ->
      name, handler_code_sym
    | Lcall_op
        (Lcall_ind | Ltailcall_ind | Lcall_imm _ | Ltailcall_imm _ | Lextcall _)
    | Lprologue | Lend | Lreloadretaddr | Lreturn | Lentertrap | Lpoptrap _
    | Lop _ | Llabel _ | Lbranch _
    | Lcondbranch (_, _)
    | Lcondbranch3 (_, _, _)
    | Lswitch _ | Ladjust_stack_offset _ | Lpushtrap _ | Lraise _
    | Lstackcheck _ ->
      assert false
  in
  (*= Restore stack frame info as it was at the probe site, so we can easily
     refer to slots in the corresponding frame.  (As per the comment above,
     recall that the wrapper does however have its own frame.) *)
  frame_required := true;
  stack_offset := p.stack_offset;
  Stack_class.Tbl.copy_values ~from:p.num_stack_slots ~to_:num_stack_slots;
  (* Account for the return address that is now pushed on the stack. *)
  stack_offset := !stack_offset + 8;
  (* Emit function entry code *)
  ND.comment (Printf.sprintf "probe %s %s" probe_name handler_code_sym);
  emit_named_text_section (S.encode wrap_label);
  ND.align ~data_section:false ~bytes:16;
  ND.define_symbol_label ~section:Text wrap_label;
  ND.cfi_startproc ();
  if fp
  then (
    push rbp;
    I.mov rsp rbp);
  (*= Prepare to call the handler: calculate and allocate stack space.
     Compute the size of stack slots for all live hard registers. *)
  let live =
    Reg.Set.elements p.probe_insn.live
    |> List.filter Reg.is_reg |> Array.of_list
  in
  let live_offset = size_of_regs live in
  (* Compute the size of stack slots for spilling all arguments of the probe. *)
  let aux_offset = 8 (* for saving r15 *) in
  let tmp_offset = size_of_regs p.probe_insn.arg in
  let loc_args, loc_offset = Proc.loc_arguments (Reg.typv p.probe_insn.arg) in
  (*= Ensure the stack is aligned.
     Assuming that the stack at the probe site is 16-byte aligned,
     [loc_arguments] ensures that [loc_offset] is 16-byte aligned.
     All temporaries are 8 bytes long, the return address is already pushed,
     and optionally the frame pointer is already pushed on the stack. *)
  let wrapper_frame_size k = 8 + (if fp then 8 else 0) + k in
  let k = live_offset + aux_offset + tmp_offset + loc_offset in
  assert (k mod 8 = 0);
  let padding = if wrapper_frame_size k mod 16 = 0 then 0 else 8 in
  let n = k + padding in
  (* Allocate stack space *)
  if Config.runtime5
     && (not Config.no_stack_checks)
     && n >= Stack_check.stack_threshold_size
  then emit_stack_check ~size_in_bytes:n ~save_registers:true;
  emit_stack_offset n;
  (* Save all live hard registers *)
  let offset = aux_offset + tmp_offset + loc_offset in
  let saved_live = stack_locations ~offset live in
  Array.iteri (fun i reg -> move reg saved_live.(i)) live;
  (* Spill r15 to free it to be used as a temporary register for stack to stack
     copying. *)
  let offset = tmp_offset + loc_offset in
  let saved_r15 = make_stack_loc 0 Reg.dummy ~offset in
  I.mov r15 (reg saved_r15);
  (* Spill all arguments of the probe. Some of these may already be on the
     stack, in which case a temporary is used for the move. *)
  let saved_args = stack_locations ~offset:loc_offset p.probe_insn.arg in
  Array.iteri
    (fun i reg -> move_allowing_stack_to_stack reg saved_args.(i))
    p.probe_insn.arg;
  (* Load probe arguments to correct locations for the handler *)
  Array.iteri
    (fun i reg -> move_allowing_stack_to_stack saved_args.(i) reg)
    loc_args;
  (* Reload spilled registers used as temporaries *)
  I.mov (reg saved_r15) r15;
  (* Emit call to handler *)
  add_used_symbol handler_code_sym;
  emit_call (Cmm.global_symbol handler_code_sym);
  (* Record a frame description for the wrapper *)
  let label = Cmm.new_label () in
  let live_offset =
    Array.fold_right
      (fun (r : Reg.t) acc ->
        match (r.loc : Reg.location) with
        | Stack (Outgoing k) -> (
          match r.typ with
          | Val -> k :: acc
          | Int | Float | Vec128 | Float32 -> acc
          | Valx2 -> k :: (k + Arch.size_addr) :: acc
          | Addr -> Misc.fatal_errorf "bad GC root %a" Printreg.reg r)
        | Stack (Incoming _ | Reg.Local _ | Domainstate _) | Reg _ | Unknown ->
          assert false)
      saved_live []
  in
  record_frame_descr ~label ~frame_size:(wrapper_frame_size n) ~live_offset
    (Dbg_other Debuginfo.none);
  ND.define_label (label_to_asm_label ~section:Text label);
  (* After the probe handler has finished executing, restore all live registers
     and free stack space. *)
  Array.iteri (fun i reg -> move saved_live.(i) reg) live;
  emit_stack_offset (-n);
  if fp then pop rbp;
  I.ret ();
  ND.cfi_endproc ();
  emit_function_type_and_size wrap_label

let emit_stapsdt_base_section () =
  if not !stapsdt_base_emitted
  then (
    stapsdt_base_emitted := true;
    ND.switch_to_section Stapsdt_base;
    (* Note that the Stapsdt symbols do not follow the usual symbol encoding
       convention. Hence, in this rare case, we create the symbol as a raw
       symbol for which no subsequent encoding will be done.*)
    let stapsdt_sym = S.create ~already_encoded:true "_.stapsdt.base" in
    ND.weak stapsdt_sym;
    ND.hidden stapsdt_sym;
    ND.define_symbol_label ~section:Stapsdt_base stapsdt_sym;
    ND.space ~bytes:1;
    ND.size_const stapsdt_sym
      1L (* 1 byte; alternative would be . - _.stapsdt.base *))

let emit_elf_note ~section ~owner ~typ ~emit_desc =
  ND.align ~data_section:true ~bytes:4;
  let a = L.create section in
  let b = L.create section in
  let c = L.create section in
  let d = L.create section in
  ND.between_labels_32_bit ~upper:b ~lower:a ();
  ND.between_labels_32_bit ~upper:d ~lower:c ();
  ND.int32 typ;
  ND.define_label a;
  ND.string (owner ^ "\000");
  ND.define_label b;
  ND.align ~data_section:true ~bytes:4;
  ND.define_label c;
  emit_desc ();
  ND.define_label d;
  ND.align ~data_section:true ~bytes:4

let emit_probe_notes0 () =
  ND.switch_to_section Stapsdt_note;
  let stap_arg arg =
    let arg_name =
      match arg.loc with
      | Stack s ->
        Printf.sprintf "%d(%%rsp)"
          (slot_offset s (Stack_class.of_machtype arg.Reg.typ))
      | Reg reg -> Reg_class.register_name arg.Reg.typ reg
      | Unknown ->
        Misc.fatal_errorf "Cannot create probe: illegal argument: %a"
          Printreg.reg arg
    in
    Printf.sprintf "%d@%s" (Select_utils.size_component arg.Reg.typ) arg_name
  in
  let describe_one_probe p =
    let probe_name, enabled_at_init =
      match p.probe_insn.desc with
      | Lcall_op (Lprobe { name; enabled_at_init; handler_code_sym = _ }) ->
        name, enabled_at_init
      | Lcall_op
          ( Lcall_ind | Ltailcall_ind | Lcall_imm _ | Ltailcall_imm _
          | Lextcall _ )
      | Lprologue | Lend | Lreloadretaddr | Lreturn | Lentertrap | Lpoptrap _
      | Lop _ | Llabel _ | Lbranch _
      | Lcondbranch (_, _)
      | Lcondbranch3 (_, _, _)
      | Lswitch _ | Ladjust_stack_offset _ | Lpushtrap _ | Lraise _
      | Lstackcheck _ ->
        assert false
    in
    let args =
      Array.fold_right (fun arg acc -> stap_arg arg :: acc) p.probe_insn.arg []
      |> String.concat " "
    in
    let semsym =
      find_or_add_semaphore probe_name (Some enabled_at_init) p.probe_insn.dbg
    in
    let semaphore_label = S.create semsym in
    let emit_desc () =
      let lbl = label_to_asm_label ~section:Stapsdt_note p.probe_label in
      ND.label lbl;
      (match Target_system.is_macos () with
      | false -> ND.symbol (S.create ~already_encoded:true "_.stapsdt.base")
      | true -> ND.int64 0L);
      ND.symbol semaphore_label;
      ND.string "ocaml_1\000";
      ND.string (probe_name ^ "\000");
      ND.string (args ^ "\000")
    in
    emit_elf_note ~section:Stapsdt_note ~owner:"stapsdt" ~typ:3l ~emit_desc
  in
  List.iter describe_one_probe !probes;
  (match Target_system.is_macos () with
  | false ->
    emit_stapsdt_base_section ();
    ND.switch_to_section Probes
  | true -> ND.switch_to_section Probes);
  ND.align ~data_section:true ~bytes:2;
  String.Map.iter
    (fun _ (label, enabled_at_init) ->
      (* Unresolved weak symbols have a zero value regardless of the following
         initialization. *)
      let enabled_at_init = Option.value enabled_at_init ~default:false in
      let label_sym = S.create ~already_encoded:true label in
      ND.weak label_sym;
      ND.hidden label_sym;
      ND.define_symbol_label ~section:Probes label_sym;
      ND.int16 (Numbers.Int16.of_int_exn 0);
      (* for systemtap probes *)
      ND.int16 (Numbers.Int16.of_int_exn (Bool.to_int enabled_at_init));
      (* for ocaml probes *)
      add_def_symbol label)
    !probe_semaphores

let emit_probe_notes () =
  match !probes with [] -> () | _ -> emit_probe_notes0 ()

let emit_trap_notes () =
  (* Don't emit trap notes on windows and macos systems *)
  let is_system_supported =
    match system with
    | S_macosx -> false (* can be supported with a symbol *)
    | S_gnu | S_solaris | S_linux_elf | S_bsd_elf | S_beos | S_linux -> true
    | S_cygwin | S_mingw | S_mingw64 | S_win64 | S_win32 | S_unknown -> false
    | S_freebsd | S_netbsd | S_openbsd ->
      (* Probably works, as these are ELF-based, but untested. *)
      false
  in
  let emit_labels list =
    List.iter (fun l -> ND.label l) list;
    ND.int64 0L
  in
  let emit_desc () =
    (* CR sspies: This symbol could be pre-defined in [Asm_symbol].
       We could then avoid exposing the `already_encoded` flag. *)
    ND.symbol (S.create ~already_encoded:true "_.stapsdt.base");
    emit_labels (L.Set.elements traps.enter_traps);
    emit_labels traps.push_traps;
    emit_labels traps.pop_traps
  in
  if is_system_supported && !Arch.trap_notes
     && not (L.Set.is_empty traps.enter_traps)
  then (
    ND.switch_to_section Note_ocaml_eh;
    emit_elf_note ~section:Note_ocaml_eh ~owner:"OCaml" ~typ:1l ~emit_desc;
    (* Reuse stapsdt base section for calcluating addresses after pre-link *)
    emit_stapsdt_base_section ();
    (* Switch back to Data section *)
    ND.data ())

let end_assembly () =
  if not (Misc.Stdlib.List.is_empty !float_constants)
  then (
    ND.switch_to_section Eight_byte_literals;
    ND.align ~data_section:true ~bytes:8;
    List.iter (fun (cst, lbl) -> emit_float_constant cst lbl) !float_constants);
  if not (Misc.Stdlib.List.is_empty !vec128_constants)
  then (
    ND.switch_to_section Sixteen_byte_literals;
    ND.align ~data_section:true ~bytes:16;
    List.iter (fun (cst, lbl) -> emit_vec128_constant cst lbl) !vec128_constants);
  (* Emit probe handler wrappers *)
  List.iter emit_probe_handler_wrapper !probes;
  emit_named_text_section (Cmm_helpers.make_symbol "jump_tables");
  emit_jump_tables ();
  let code_end = Cmm_helpers.make_symbol "code_end" in
  emit_named_text_section code_end;
  if is_macosx system then I.nop ();
  (* suppress "ld warning: atom sorting error" *)
  emit_global_label_for_symbol ~section:Text code_end;
  emit_imp_table ~section:Text ();
  ND.data ();
  ND.int64 0L;
  (* PR#6329 *)
  emit_global_label ~section:Data "data_end";
  ND.int64 0L;
  ND.text ();
  ND.align ~data_section:true ~bytes:8;
  (* PR#7591 *)
  emit_global_label ~section:Text "frametable";
  (* CR sspies: Share the [emit_frames] code with the Arm backend. *)
  emit_frames
    { efa_code_label =
        (fun l ->
          let l = label_to_asm_label ~section:Text l in
          ND.label l);
      efa_data_label =
        (fun l ->
          let l = label_to_asm_label ~section:Data l in
          ND.label l);
      efa_i8 = (fun n -> ND.int8 n);
      efa_i16 = (fun n -> ND.int16 n);
      efa_i32 = (fun n -> ND.int32 n);
      efa_u8 = (fun n -> ND.uint8 n);
      efa_u16 = (fun n -> ND.uint16 n);
      efa_u32 = (fun n -> ND.uint32 n);
      efa_word = (fun n -> ND.targetint (Targetint.of_int_exn n));
      efa_align = (fun n -> ND.align ~data_section:true ~bytes:n);
      efa_label_rel =
        (fun lbl ofs ->
          let lbl = label_to_asm_label ~section:Text lbl in
          let ofs = Targetint.of_int32 ofs in
          ND.between_this_and_label_offset_32bit_expr ~upper:lbl
            ~offset_upper:ofs);
      efa_def_label =
        (fun l ->
          let lbl = label_to_asm_label ~section:Text l in
          ND.define_label lbl);
      efa_string = (fun s -> ND.string (s ^ "\000"))
    };
  let frametable_sym = S.create (Cmm_helpers.make_symbol "frametable") in
  ND.size frametable_sym;
  ND.data ();
  emit_probe_notes ();
  emit_trap_notes ();
  ND.mark_stack_non_executable ();
  (* Note that [mark_stack_non_executable] switches the section on Linux. *)
  if is_win64 system
  then (
    ND.comment "External functions";
    String.Set.iter
      (fun s ->
        if not (String.Set.mem s !symbols_defined) then ND.extrn (S.create s))
      !symbols_used;
    symbols_used := String.Set.empty;
    symbols_defined := String.Set.empty);
  let asm =
    if !X86_proc.create_asm_file
    then
      Some
        ((if X86_proc.masm then X86_masm.generate_asm else X86_gas.generate_asm)
           !Emitaux.output_channel)
    else None
  in
  if not !Flambda_backend_flags.internal_assembler
  then Emitaux.Dwarf_helpers.emit_dwarf ();
  X86_proc.generate_code asm;
  (* The internal assembler does not work if reset_all is called here *)
  if not !Flambda_backend_flags.internal_assembler then reset_all ()
