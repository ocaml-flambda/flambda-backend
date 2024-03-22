(***********************************************************************)
(*                                                                     *)
(*                              OCaml                                  *)
(*                                                                     *)
(*  Copyright 2014, OCamlPro. All rights reserved.                     *)
(*  All rights reserved. This file is distributed under the terms of   *)
(*  the GNU Lesser General Public License version 2.1                  *)
(*                                                                     *)
(***********************************************************************)
(*
  Contributors:
  * Fabrice LE FESSANT (INRIA/OCamlPro)
*)

[@@@ocaml.warning "+A-4-9-69"]

open X86_ast
open X86_proc
module String = Misc.Stdlib.String

type section = {
  sec_name : string;
  mutable sec_instrs : asm_line array;
}

type data_size = B8 | B16 | B32 | B64

module IntSet = Set.Make (Int)

module StringMap = Map.Make (String)

let print_old_arg ppf = function
  | Imm _ -> Format.fprintf ppf "Imm"
  | Reg8L _ -> Format.fprintf ppf "Reg8L"
  | Reg8H _ -> Format.fprintf ppf "Reg8H"
  | Reg16 _ -> Format.fprintf ppf "Reg16"
  | Reg32 _ -> Format.fprintf ppf "Reg32"
  | Reg64 _ -> Format.fprintf ppf "Reg64"
  | Regf _ -> Format.fprintf ppf "Regf"
  | Mem _ -> Format.fprintf ppf "Mem"
  | Mem64_RIP _ -> Format.fprintf ppf "Mem64_RIP"
  | Sym _ -> Format.fprintf ppf "Sym"

(*
TODO:

If a or-pattern contains both "Reg64 ... | Reg32 ... ", it means that
we didn't discriminate between 32 bit and 64 bit modes for that
instruction. It also means that using this instruction on a 32-bit
register in 64 bit mode will not generate the 32-bit version of the
instruction, but the 64-bit version...

*)

module Relocation = struct
  module Kind = struct
    type t =
      (* 32 bits offset usually in data section *)
      | REL32 of string * int64
      | DIR32 of string * int64
      | DIR64 of string * int64
  end

  type t = { offset_from_section_beginning : int; kind : Kind.t }
end

type symbol_binding = Sy_local | Sy_global | Sy_weak

type symbol = {
  sy_name : string;
  mutable sy_type : string option;
  mutable sy_size : int option;
  mutable sy_binding : symbol_binding;
  mutable sy_protected : bool;
  mutable sy_sec : section;
  mutable sy_pos : int option;
  mutable sy_num : int option; (* position in .symtab *)
}

type buffer = {
  sec : section;
  buf : Buffer.t;
  labels : symbol String.Tbl.t;
  mutable patches : (int * data_size * int64) list;
  mutable relocations : Relocation.t list;
}

type local_reloc =
  | RelocCall of string
  | RelocShortJump of string * int (* loc *)
  | RelocLongJump of string
  | RelocConstant of constant * data_size

type result =
  | Rint of int64
  | Rabs of string * int64 (* absolute label + offset *)
  | Rrel of string * int64

(* relative label + offset *)

(*
let string_of_result = function
  Rint n -> Printf.sprintf "Rint %Ld" n
  | Rabs (s, n) -> Printf.sprintf "Rabs (%S, %Ld)" s n
  | Rrel (s, n) -> Printf.sprintf "Rrel (%S, %Ld)" s n
*)

let get_symbol b s =
  try String.Tbl.find b.labels s
  with Not_found ->
    let sy =
      {
        sy_name = s;
        sy_type = None;
        sy_size = None;
        sy_pos = None;
        sy_binding = Sy_local;
        sy_protected = false;
        sy_num = None;
        sy_sec = b.sec;
      }
    in
    String.Tbl.add b.labels s sy ;
    sy

let buf_int8 b i = Buffer.add_char b.buf (char_of_int (i land 0xff))

let buf_int8L b iL = buf_int8 b (Int64.to_int iL)

let buf_int16L b iL =
  buf_int8L b iL;
  buf_int8L b (Int64.shift_right iL 8)

let buf_int32L b iL =
  buf_int16L b iL;
  buf_int16L b (Int64.shift_right iL 16)

let buf_int64L b iL =
  buf_int32L b iL;
  buf_int32L b (Int64.shift_right iL 32)

let str_int8L s pos v = Bytes.set s pos (char_of_int (Int64.to_int v land 0xff))

let str_int16L s pos v =
  str_int8L s pos v;
  str_int8L s (pos + 1) (Int64.shift_right_logical v 8)

let str_int32L s pos v =
  str_int16L s pos v;
  str_int16L s (pos + 2) (Int64.shift_right_logical v 16)

let str_int64L s pos v =
  str_int32L s pos v;
  str_int32L s (pos + 4) (Int64.shift_right_logical v 32)

(* When a jump has to be generated, we compare the offset between the
   source instruction and the target instruction, in number of
   instructions.

   If the offset is less than [short_jump_threshold] instructions,
   we generate a short jump during the first pass. 16 is a "safe"
   value, as most instructions are shorter than 8 bytes: [REX] +
   [OPCODE] + [MODRM] + [SIB] + [IMM32] *)

let local_relocs = ref []

let local_labels = String.Tbl.create 100

let forced_long_jumps = ref IntSet.empty

let instr_size = ref 4

let new_buffer sec =
  {
    sec;
    buf = Buffer.create 10000;
    labels = String.Tbl.create 100;
    relocations = [];
    patches = [];
  }

let label_pos b lbl =
  match (String.Tbl.find b.labels lbl).sy_pos with
  | None -> raise Not_found
  | Some pos -> pos

(* Try to compute some statically computable arithmetic expressions
   in labels, or to simplify them to a form that is encodable by
   relocations. *)
let eval_const b current_pos cst =
  let rec eval = function
    | Const n -> Rint n
    | ConstThis -> Rabs ("", 0L)
    | ConstLabel lbl -> Rabs (lbl, 0L)
    | ConstLabelOffset (lbl, o) -> Rabs (lbl, Int64.of_int o)
    | ConstSub (c1, c2) -> (
        let c1 = eval c1 and c2 = eval c2 in
        match (c1, c2) with
        | Rint n1, Rint n2 -> Rint (Int64.sub n1 n2)
        | Rabs (s, n1), Rint n2 -> Rabs (s, Int64.sub n1 n2)
        | Rrel (s, n1), Rint n2 -> Rrel (s, Int64.sub n1 n2)
        | Rabs ("", n1), Rabs ("", n2) -> Rint (Int64.sub n1 n2)
        | Rabs ("", n1), Rabs (s2, n2) -> (
            try
              let sy2 = String.Tbl.find b.labels s2 in
              match sy2.sy_pos with
              | Some pos2 ->
                  let pos2 = Int64.of_int pos2 in
                  Rint
                    (Int64.sub
                       (Int64.add n1 (Int64.of_int current_pos))
                       (Int64.add pos2 n2))
              | _ -> assert false
            with Not_found -> assert false)
        | Rabs (s, n1), Rabs ("", n2) -> (
            try
              let sy = String.Tbl.find b.labels s in
              match sy.sy_pos with
              | Some pos ->
                  let pos = Int64.of_int pos in
                  Rint
                    (Int64.sub (Int64.add pos n1)
                       (Int64.add n2 (Int64.of_int current_pos)))
              | _ -> assert false
            with Not_found -> Rrel (s, Int64.sub n1 n2))
        | Rabs (s1, n1), Rabs (s2, n2) -> (
            try
              let sy2 = String.Tbl.find b.labels s2 in
              try
                let sy1 = String.Tbl.find b.labels s1 in
                assert (sy1.sy_sec == sy2.sy_sec);
                match (sy1.sy_pos, sy2.sy_pos) with
                | Some pos1, Some pos2 ->
                    let pos1 = Int64.of_int pos1 in
                    let pos2 = Int64.of_int pos2 in
                    Rint (Int64.sub (Int64.add pos1 n1) (Int64.add pos2 n2))
                | _ -> assert false
              with Not_found -> (
                match sy2.sy_pos with
                | Some pos2 ->
                    let pos2 = Int64.of_int pos2 in
                    Rrel
                      ( s1,
                        Int64.sub
                          (Int64.add n1 (Int64.of_int current_pos))
                          (Int64.add pos2 n2) )
                | _ -> assert false)
            with Not_found -> assert false)
        | _ -> assert false)
    | ConstAdd (c1, c2) -> (
        let c1 = eval c1 and c2 = eval c2 in
        match (c1, c2) with
        | Rint n1, Rint n2 -> Rint (Int64.add n1 n2)
        | Rabs (s, n1), Rint n2 | Rint n2, Rabs (s, n1) ->
            Rabs (s, Int64.add n1 n2)
        | Rrel (s, n1), Rint n2 | Rint n2, Rrel (s, n1) ->
            Rrel (s, Int64.add n1 n2)
        (* TODO: we could add another case, easy to solve: adding a
           Rrel to a Rabs where the symbol is local, in which case it
           can be computed. *)
        | Rrel (s, n1), Rabs ("", n2) -> Rabs (s, Int64.add n1 n2)
        | _ -> assert false)
  in
  try
    let r = eval cst in
    (*
    if debug then
      Printf.eprintf "eval_const (%s) = %s at @%d\n%!"
        (X86_gas.string_of_constant cst)
        (string_of_result r) current_pos;
*)
    r
  with e ->
    Printf.eprintf "Error in eval_const: exception %S\n%!"
      (*(X86_gas.string_of_constant cst)*) (Printexc.to_string e);
    raise e

let is_imm32L n = n < 0x8000_0000L && n >= -0x8000_0000L

let is_imm8L x = x < 128L && x >= -128L

let is_imm16L n = n < 32768L && n >= -32768L

let rd_of_regf regf =
  match regf with
  | XMM n -> n

let rd_of_reg64 = function
  | RAX -> 0
  | RCX -> 1
  | RDX -> 2
  | RBX -> 3
  | RSP -> 4
  | RBP -> 5
  | RSI -> 6
  | RDI -> 7
  | R8 -> 8
  | R9 -> 9
  | R10 -> 10
  | R11 -> 11
  | R12 -> 12
  | R13 -> 13
  | R14 -> 14
  | R15 -> 15

let rd_of_reg8 = function
  | Reg8L r -> rd_of_reg64 r
  | Reg8H AH -> 4
  | Reg8H CH -> 5
  | Reg8H DH -> 6
  | Reg8H BH -> 7
  | _ -> assert false

let cd_of_condition condition =
  match condition with
  | O -> 0
  | NO -> 1
  | B -> 2
  | AE -> 3
  | E -> 4
  | NE -> 5
  | BE -> 6
  | A -> 7
  | S -> 8
  | NS -> 9
  | P -> 10
  | NP -> 11
  | L -> 12
  | GE -> 13
  | LE -> 14
  | G -> 15

(* We should precompute a position for each label depending on
   the number of instructions: heuristics = offset_in_instrs x 7
*)

let no_rex = 0

let rex = 0b01000000

let rexr = 0b00000100 (* extension of r *)

let rexr_reg reg = if reg > 7 then rexr else 0

let rexw = rex lor 0b00001000

let rexx = 0b00000010

let rexx_index reg = if reg > 7 then rexx else 0

let rexb = 0b00000001

let rexb_opcode reg = if reg > 7 then rexb else 0

let rexb_rm reg = if reg > 7 then rexb else 0

let rexb_base reg = if reg > 7 then rexb else 0

let reg7 reg = reg land 0x07

let rex_of_reg8 = function Reg8L (RSP | RBP | RSI | RDI) -> rex | _ -> 0

(* TODO: we should check conformance with page 3-2, vol 2A of Intel Spec ? *)

let rex_of_reg16 = function
  | RAX | RCX | RDX | RBX | RSP | RBP | RSI | RDI -> 0
  | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 -> rex

let mod_rm_reg m rm reg = (m lsl 6) + reg7 rm + (reg7 reg lsl 3)

let sib scale index base =
  let scale =
    match scale with 1 -> 0 | 2 -> 1 | 4 -> 2 | 8 -> 3 | _ -> assert false
  in
  (scale lsl 6) lor (reg7 index lsl 3) lor reg7 base

let record_reloc b offset_from_section_beginning kind =
  b.relocations <-
    { Relocation.offset_from_section_beginning; kind } :: b.relocations

let declare_label b s =
  let sy = get_symbol b s in
  assert (sy.sy_pos = None);
  let pos = Buffer.length b.buf in
  sy.sy_pos <- Some pos

let buf_opcodes b opcodes =
  ListLabels.iter ~f:(fun opcode -> buf_int8 b opcode) opcodes

let arch64 = Config.architecture = "amd64"

let emit_rex b rexcode =
  if arch64 && rexcode <> 0 then buf_int8 b (rexcode lor rex)

let buf_int16_imm b = function
  | Imm n ->
      assert (is_imm16L n);
      buf_int16L b n
  | _ -> assert false

let buf_int32_imm b = function
  | Imm n ->
      assert (is_imm32L n);
      buf_int32L b n
  | Sym symbol ->
      record_reloc b (Buffer.length b.buf) (Relocation.Kind.DIR32 (symbol, 0L));
      buf_int32L b 0L
  | _ -> assert false

type offset_exp = OImm8 of int64 | OImm32 of string option * int64

let sym32 b sym =
  record_reloc b (Buffer.length b.buf) (Relocation.Kind.DIR32 (sym, 0L));
  buf_int32L b 0L

let sym64 b sym =
  record_reloc b (Buffer.length b.buf) (Relocation.Kind.DIR64 (sym, 0L));
  buf_int64L b 0L

let buf_sym b sym offset =
  match sym with
  | None -> buf_int32L b offset
  | Some lbl ->
      (* TODO: assert we are in 32 bits ? *)
      record_reloc b (Buffer.length b.buf) (Relocation.Kind.DIR32 (lbl, offset));
      buf_int32L b 0L

let emit_prefix_modrm b opcodes rm reg ~prefix =
  (* When required for a particular instruction, the REX / REXW flag is added in
     [emit_mod_rm_reg]. This function otherwise assumes [~rex:0] for Reg32,
     Reg64, Regf, and addressing modes. *)
  match rm with
  | Reg32 rm ->
      let rm = rd_of_reg64 rm in
      prefix b ~rex:0 ~rexr:(rexr_reg reg) ~rexb:(rexb_rm rm) ~rexx:0;
      buf_opcodes b opcodes;
      buf_int8 b (mod_rm_reg 0b11 rm reg)
  | Reg64 rm ->
      let rm = rd_of_reg64 rm in
      prefix b ~rex:0 ~rexr:(rexr_reg reg) ~rexb:(rexb_rm rm) ~rexx:0;
      buf_opcodes b opcodes;
      buf_int8 b (mod_rm_reg 0b11 rm reg)
  | (Reg8L _ | Reg8H _) as reg8 ->
      let rm = rd_of_reg8 reg8 in
      prefix b ~rex:(rex_of_reg8 reg8) ~rexr:(rexr_reg reg)
               ~rexb:(rexb_rm rm) ~rexx:0;
      buf_opcodes b opcodes;
      buf_int8 b (mod_rm_reg 0b11 rm reg)
  | Reg16 reg16 ->
      let rm = rd_of_reg64 reg16 in
      prefix b ~rex:(rex_of_reg16 reg16) ~rexr:(rexr_reg reg)
               ~rexb:(rexb_rm rm) ~rexx:0;
      buf_opcodes b opcodes;
      buf_int8 b (mod_rm_reg 0b11 rm reg)
  | Regf regf ->
      let rm = rd_of_regf regf in
      prefix b ~rex:0 ~rexr:(rexr_reg reg) ~rexb:(rexb_rm rm) ~rexx:0;
      buf_opcodes b opcodes;
      buf_int8 b (mod_rm_reg 0b11 rm reg)
  (* 64 bits memory access *)
  | Mem64_RIP (_, symbol, offset) ->
      prefix b ~rex:0 ~rexr:(rexr_reg reg) ~rexb:0 ~rexx:0;
      buf_opcodes b opcodes;
      buf_int8 b (mod_rm_reg 0b00 0b101 reg);
      record_reloc b (Buffer.length b.buf)
        (Relocation.Kind.REL32 (symbol, Int64.of_int offset));
      buf_int32L b 0L
  | Mem { arch; typ = _; idx; scale; base; sym; displ } -> (
      let offset =
        let displ = Int64.of_int displ in
        match sym with
        | None ->
            if is_imm8L displ then OImm8 displ
            else if is_imm32L displ then OImm32 (None, displ)
            else assert false
        | Some s -> OImm32 (Some s, displ)
      in
      let idx_reg = idx in
      let idx = rd_of_reg64 idx in
      if scale = 0 then (
        assert (base = None && arch = X86);
        match offset with
        | OImm8 _ -> assert false
        | OImm32 (sym, offset) ->
            (* No prefix; 32-bit mode. *)
            buf_opcodes b opcodes;
            buf_int8 b (mod_rm_reg 0b00 0b101 reg);
            buf_sym b sym offset)
      else
        match base with
        | None -> (
            match (idx_reg, scale, offset) with
            | (RSP | R12), 1, OImm8 0L ->
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_base idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b00 idx reg);
                buf_int8 b (sib 1 0b100 idx)
            | (RSP | R12), 1, OImm8 offset8 ->
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_base idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b01 0b100 reg);
                buf_int8 b (sib 1 0b100 idx);
                buf_int8L b offset8
            | (RSP | R12), 1, OImm32 (sym, offset) ->
                (* to 0x??(%rsp) *)
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_base idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b10 0b100 reg);
                buf_int8 b (sib 1 0b100 idx);
                buf_sym b sym offset
            | (RBP | R13), 1, OImm8 _ -> (
                (* to 0x??(%rbp) *)
                (* TODO check if offset8 = 0 is enough *)
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_base idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b01 idx reg);
                match offset with
                | OImm8 offset8 -> buf_int8L b offset8
                | _ -> assert false)
            | _, 1, OImm8 0L ->
                (* to 0x00(%r??) except %rsp and %rbp *)
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_rm idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b00 idx reg)
            | _, 1, OImm8 offset8 ->
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_rm idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b01 idx reg);
                buf_int8L b offset8
            | _, 1, OImm32 (sym, offset) ->
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:(rexb_rm idx) ~rexx:0;
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b10 idx reg);
                buf_sym b sym offset
            | _, _, _ -> (
                prefix b ~rex:0 ~rexr:(rexr_reg reg)
                         ~rexb:0 ~rexx:(rexx_index idx);
                buf_opcodes b opcodes;
                buf_int8 b (mod_rm_reg 0b00 0b100 reg);
                buf_int8 b (sib scale idx 0b101);
                match offset with
                | OImm8 offset8 -> buf_int32L b offset8
                | OImm32 (sym, offset) -> buf_sym b sym offset))
        | Some base_reg -> (
            assert (scale = 1 || scale = 2 || scale = 4 || scale = 8);
            let base = rd_of_reg64 base_reg in
            prefix b ~rex:0 ~rexr:(rexr_reg reg)
                     ~rexb:(rexb_base base) ~rexx:(rexx_index idx);
            buf_opcodes b opcodes;
            match (base_reg, offset) with
            | (RBP | R13), OImm8 0L ->
                (* to 0x00(%rbp+reg) *)
                buf_int8 b (mod_rm_reg 0b01 0b100 reg);
                buf_int8 b (sib scale idx base);
                buf_int8 b 0
            | _, OImm8 0L ->
                buf_int8 b (mod_rm_reg 0b00 0b100 reg);
                buf_int8 b (sib scale idx base)
            | _, OImm8 offset ->
                buf_int8 b (mod_rm_reg 0b01 0b100 reg);
                buf_int8 b (sib scale idx base);
                buf_int8L b offset
            | _, OImm32 (sym, offset) ->
                buf_int8 b (mod_rm_reg 0b10 0b100 reg);
                buf_int8 b (sib scale idx base);
                buf_sym b sym offset))
  | Imm _ | Sym _ -> assert false

let emit_mod_rm_reg b rex_always opcodes rm reg =
  emit_prefix_modrm b opcodes rm reg ~prefix:(fun b ~rex ~rexr ~rexb ~rexx ->
    emit_rex b (rex_always lor rex lor rexr lor rexb lor rexx))

let emit_movlpd b dst src =
  match (dst, src) with
  | Regf reg, ((Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x12 ] rm (rd_of_regf reg)
  | ((Mem _ | Mem64_RIP _) as rm), Regf reg ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x13 ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_movapd b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x28 ] rm (rd_of_regf reg)
  | ((Mem _ | Mem64_RIP _) as rm), Regf reg ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x29 ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_movupd b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x10 ] rm (rd_of_regf reg)
  | ((Mem _ | Mem64_RIP _) as rm), Regf reg ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x11 ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_movd b ~dst ~src =
  match (dst, src) with
  | Regf reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b no_rex [ 0x0F; 0x6E ] rm (rd_of_regf reg)
  | ((Reg32 _ | Mem _ | Mem64_RIP _) as rm), Regf reg ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b no_rex [ 0x0F; 0x7E ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_movq b ~dst ~src =
  (* It seems there is a choice to make on how we encode instructions here
     as there are different encoding possible for the same operation.
     See https://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-instruction-set-reference-manual-325383.pdf,
     pages 707 and 755.  *)
  match (dst, src) with
  | Regf reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm) ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b rexw [ 0x0F; 0x6E ] rm (rd_of_regf reg)
  | ((Reg64 _ | Mem _ | Mem64_RIP _) as rm), Regf reg ->
    buf_int8 b 0x66;
    emit_mod_rm_reg b rexw [ 0x0F; 0x7E ] rm (rd_of_regf reg)
  | Regf reg, ((Regf _) as rm) ->
    buf_int8 b 0xF3;
    emit_mod_rm_reg b no_rex [ 0x0F; 0x7E ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_movsd b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b 0 [ 0x0f; 0x10 ] rm (rd_of_regf reg)
  | ((Mem _ | Mem64_RIP _) as rm), Regf reg ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b 0 [ 0x0f; 0x11 ] rm (rd_of_regf reg)
  | _ ->
      Format.eprintf "src=%a dst=%a@." print_old_arg src print_old_arg dst;
      assert false

let emit_movss b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF3;
      emit_mod_rm_reg b 0 [ 0x0f; 0x10 ] rm (rd_of_regf reg)
  | ((Mem _ | Mem64_RIP _) as rm), Regf reg ->
      buf_int8 b 0xF3;
      emit_mod_rm_reg b 0 [ 0x0f; 0x11 ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_andpd b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x54 ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_bsf b ~dst ~src =
  match (dst, src) with
  | Reg16 reg, ((Reg16 _ | Mem _ | Mem64_RIP _) as rm)
  | Reg32 reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
    (* BSF r16, r/m16 and BSF r32, r/m32 *)
    emit_mod_rm_reg b 0 [ 0x0F; 0xBC ] rm (rd_of_reg64 reg)
  | Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm) ->
    (* BSF r64, r/m64 *)
    emit_mod_rm_reg b rexw [ 0x0F; 0xBC ] rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_bsr b ~dst ~src =
  match (dst, src) with
  | Reg16 reg, ((Reg16 _ | Mem _ | Mem64_RIP _) as rm)
  | Reg32 reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
    (* BSR r16, r/m16 and BSR r32, r/m32 *)
    emit_mod_rm_reg b 0 [ 0x0F; 0xBD ] rm (rd_of_reg64 reg)
  | Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm) ->
    (* BSR r64, r/m64 *)
    emit_mod_rm_reg b rexw [ 0x0F; 0xBD ] rm (rd_of_reg64 reg)
  | _ -> assert false

let imm8_of_rounding rounding =
  (* bits are:
     - 3: Precision Mask (0 = Normal, 1 = Inexact)
     - 2: Rounding Select (0 = Use bits 1 and 0 for Rounding mode, 1 = MXCSR.RC)
     - 1 and 0: Rounding mode *)
  match rounding with
  | RoundNearest -> 0b0000
  | RoundDown -> 0b0001
  | RoundUp -> 0b0010
  | RoundTruncate -> 0b0011
  | RoundCurrent -> 0b0100

let emit_roundsd b dst rounding src =
  let rounding = imm8_of_rounding rounding in
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x3A; 0x0B ] rm (rd_of_regf reg);
      buf_int8 b rounding
  | _ -> assert false

let emit_addsd b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b 0 [ 0x0f; 0x58 ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_sqrtsd b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b 0 [ 0x0f; 0x51 ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_mulsd b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b 0 [ 0x0f; 0x59 ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_divsd b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b 0 [ 0x0f; 0x5E ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_subsd b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b 0 [ 0x0f; 0x5C ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_xorpd b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x57 ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_CVTSI2SS b dst src =
  match (dst, src) with
  | Regf reg, ((Reg64 _ | Mem { typ = QWORD }) as rm) ->
      buf_int8 b 0xF3;
      emit_mod_rm_reg b rexw [ 0x0f; 0x2A ] rm (rd_of_regf reg)
  | Regf reg, ((Reg32 _ | Mem { typ = DWORD }) as rm) ->
      buf_int8 b 0xF3;
      emit_mod_rm_reg b 0 [ 0x0f; 0x2A ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_CVTSI2SD b dst src =
  match (dst, src) with
  | Regf reg, ((Reg64 _ | Mem { typ = QWORD }) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b rexw [ 0x0f; 0x2A ] rm (rd_of_regf reg)
  | Regf reg, ((Reg32 _ | Mem { typ = DWORD }) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b 0 [ 0x0f; 0x2A ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_CVTSS2SI b dst src =
  match (dst, src) with
  | Reg64 reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF3;
      emit_mod_rm_reg b rexw [ 0x0f; 0x2D ] rm (rd_of_reg64 reg)
  | Reg32 reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF3;
      emit_mod_rm_reg b 0 [ 0x0f; 0x2D ] rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_CVTSD2SI b dst src =
  match (dst, src) with
  | Reg64 reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b rexw [ 0x0f; 0x2D ] rm (rd_of_reg64 reg)
  | Reg32 reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b 0 [ 0x0f; 0x2D ] rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_CVTTSS2SI b dst src =
  match (dst, src) with
  | Reg64 reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF3;
      emit_mod_rm_reg b rexw [ 0x0f; 0x2C ] rm (rd_of_reg64 reg)
  | Reg32 reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF3;
      emit_mod_rm_reg b 0 [ 0x0f; 0x2C ] rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_CVTTSD2SI b dst src =
  match (dst, src) with
  | Reg64 reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b rexw [ 0x0f; 0x2C ] rm (rd_of_reg64 reg)
  | Reg32 reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b 0 [ 0x0f; 0x2C ] rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_CVTSD2SS b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF2;
      emit_mod_rm_reg b 0 [ 0x0f; 0x5A ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_CVTSS2SD b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0xF3;
      emit_mod_rm_reg b 0 [ 0x0f; 0x5A ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_comisd b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x2F ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_ucomisd b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x2E ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_MOV b dst src =
  match (dst, src) with
  (* movb *)
  | ((Reg8L (RAX | RCX | RDX | RBX) | Reg8H _) as r8), Imm n ->
      assert (is_imm8L n);
      buf_opcodes b [ 0xB0 + reg7 (rd_of_reg8 r8) ];
      buf_int8L b n
  | ((Mem _ | Mem64_RIP _) as rm), ((Reg8L _ | Reg8H _) as reg) ->
      emit_mod_rm_reg b (rex_of_reg8 reg) [ 0x88 ] rm (rd_of_reg8 reg)
  (* no REX.W *)
  (* movw *)
  | ((Mem _ | Mem64_RIP _) as rm), Reg16 reg ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b rex [ 0x89 ] rm (rd_of_reg64 reg) (* no REX.W *)
  | Reg16 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b rex [ 0x8B ] rm (rd_of_reg64 reg) (* no REX.W *)
  (* movl *)
  | Reg32 reg32, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
      let reg = rd_of_reg64 reg32 in
      emit_mod_rm_reg b 0 [ 0x8B ] rm reg
  | ((Mem _ | Mem64_RIP _) as rm), Reg32 reg32 ->
      let reg = rd_of_reg64 reg32 in
      emit_mod_rm_reg b 0 [ 0x89 ] rm reg
  | (Mem { typ = DWORD } as rm), ((Imm _ | Sym _) as n) ->
      emit_mod_rm_reg b 0 [ 0xC7 ] rm 0;
      buf_int32_imm b n
  | (Mem { typ = NONE; arch = X86 } as rm), ((Imm _ | Sym _) as n) ->
      let reg = 0 in
      emit_mod_rm_reg b 0 [ 0xC7 ] rm reg;
      buf_int32_imm b n
  | Reg32 r32, ((Imm _ | Sym _) as n) ->
      let n =
        match n with
        | Imm n ->
            (* "Shift" [n] from [0, 0xFFFF_FFFF] to [-0x8000_0000, 0x7FFF_FFFF] *)
            Imm (Int64.of_int32 (Int64.to_int32 n))
        | _ as n -> n
      in
      let reg = rd_of_reg64 r32 in
      emit_rex b (rexb_opcode reg);
      buf_int8 b (0xB8 lor reg7 reg);
      buf_int32_imm b n
  (* movq *)
  | Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm) ->
      emit_mod_rm_reg b rexw [ 0x8B ] rm (rd_of_reg64 reg)
  | ((Mem _ | Mem64_RIP _) as rm), Reg64 reg ->
      emit_mod_rm_reg b rexw [ 0x89 ] rm (rd_of_reg64 reg)
  | Reg64 r64, Imm n when not (is_imm32L n) ->
      (* MOVNoneQ *)
      let reg = rd_of_reg64 r64 in
      emit_rex b (rexw lor rexb_opcode reg);
      buf_int8 b (0xB8 lor reg7 reg);
      buf_int64L b n
  | Reg64 r64, Sym symbol when windows ->
      let reg = rd_of_reg64 r64 in
      emit_rex b (rexw lor rexb_opcode reg);
      buf_int8 b (0xB8 lor reg7 reg);
      sym64 b symbol
  | ((Mem { arch = X64 } | Reg64 _) as rm), ((Imm _ | Sym _) as n) ->
      emit_mod_rm_reg b rexw [ 0xC7 ] rm 0;
      buf_int32_imm b n
  | _ ->
      Format.printf "dst = %a@." print_old_arg dst;
      Format.printf "src = %a@." print_old_arg src;
      assert false

let check_rf_rfm ops b dst src =
  match (dst, src) with
  | Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm) ->
      emit_mod_rm_reg b 0 ops rm (rd_of_regf reg)
  | _ -> assert false

let check_rf_rf ops b dst src =
  match (dst, src) with
  | Regf reg, (Regf _ as rm) ->
      emit_mod_rm_reg b 0 ops rm (rd_of_regf reg)
  | _ -> assert false

let suffix f op b suf dst src =
  f op b dst src;
  buf_int8 b suf

let prefix pref f op b dst src =
  buf_int8 b pref;
  f op b dst src

let emit_rf_rf op b dst src = check_rf_rf [ 0x0f; op ] b dst src
let emit_rf_rfm op b dst src = check_rf_rfm [ 0x0f; op ] b dst src
let emit_rep_rf_rfm = prefix 0xF3 emit_rf_rfm
let emit_repne_rf_rfm = prefix 0xF2 emit_rf_rfm
let emit_osize_rf_rfm = prefix 0x66 emit_rf_rfm
let emit_osize_rf_rfm_38 =
  let emit op b dst src = check_rf_rfm [ 0x0f; 0x38; op ] b dst src in
  prefix 0x66 emit
let emit_osize_rf_rfm_3A =
  let emit op b dst src = check_rf_rfm [ 0x0f; 0x3A; op ] b dst src in
  prefix 0x66 emit

let emit_cmpps = suffix emit_rf_rfm 0xC2
let emit_shufps = suffix emit_rf_rfm 0xC6
let emit_addps = emit_rf_rfm 0x58
let emit_subps = emit_rf_rfm 0x5C
let emit_mulps = emit_rf_rfm 0x59
let emit_divps = emit_rf_rfm 0x5E
let emit_minps = emit_rf_rfm 0x5D
let emit_maxps = emit_rf_rfm 0x5F
let emit_rcpps = emit_rf_rfm 0x53
let emit_sqrtps = emit_rf_rfm 0x51
let emit_rsqrtps = emit_rf_rfm 0x52
let emit_unpcklps = emit_rf_rfm 0x14
let emit_unpckhps = emit_rf_rfm 0x15
let emit_movhlps = emit_rf_rf 0x12
let emit_movlhps = emit_rf_rf 0x16
let emit_paddb = emit_osize_rf_rfm 0xFC
let emit_paddw = emit_osize_rf_rfm 0xFD
let emit_paddd = emit_osize_rf_rfm 0xFE
let emit_paddq = emit_osize_rf_rfm 0xD4
let emit_addpd = emit_osize_rf_rfm 0x58
let emit_paddsb = emit_osize_rf_rfm 0xEC
let emit_paddsw = emit_osize_rf_rfm 0xED
let emit_paddusb = emit_osize_rf_rfm 0xDC
let emit_paddusw = emit_osize_rf_rfm 0xDD
let emit_psubb = emit_osize_rf_rfm 0xF8
let emit_psubw = emit_osize_rf_rfm 0xF9
let emit_psubd = emit_osize_rf_rfm 0xFA
let emit_psubq = emit_osize_rf_rfm 0xFB
let emit_subpd = emit_osize_rf_rfm 0x5C
let emit_psubsb = emit_osize_rf_rfm 0xE8
let emit_psubsw = emit_osize_rf_rfm 0xE9
let emit_psubusb = emit_osize_rf_rfm 0xD8
let emit_psubusw = emit_osize_rf_rfm 0xD9
let emit_pmaxub = emit_osize_rf_rfm 0xDE
let emit_pmaxsw = emit_osize_rf_rfm 0xEE
let emit_maxpd = emit_osize_rf_rfm 0x5F
let emit_pminub = emit_osize_rf_rfm 0xDA
let emit_pminsw = emit_osize_rf_rfm 0xEA
let emit_minpd = emit_osize_rf_rfm 0x5D
let emit_mulpd = emit_osize_rf_rfm 0x59
let emit_divpd = emit_osize_rf_rfm 0x5E
let emit_sqrtpd = emit_osize_rf_rfm 0x51
let emit_pand = emit_osize_rf_rfm 0xDB
let emit_pandnot = emit_osize_rf_rfm 0xDF
let emit_por = emit_osize_rf_rfm 0xEB
let emit_pxor = emit_osize_rf_rfm 0xEF
let emit_pcmpeqb = emit_osize_rf_rfm 0x74
let emit_pcmpeqw = emit_osize_rf_rfm 0x75
let emit_pcmpeqd = emit_osize_rf_rfm 0x76
let emit_pcmpgtb = emit_osize_rf_rfm 0x64
let emit_pcmpgtw = emit_osize_rf_rfm 0x65
let emit_pcmpgtd = emit_osize_rf_rfm 0x66
let emit_cvtdq2pd = emit_rep_rf_rfm 0xE6
let emit_cvtdq2ps = emit_rf_rfm 0x5B
let emit_cvtpd2dq = emit_repne_rf_rfm 0xE6
let emit_cvtpd2ps = emit_osize_rf_rfm 0x5A
let emit_cvtps2dq = emit_osize_rf_rfm 0x5B
let emit_cvtps2pd = emit_rf_rfm 0x5A
let emit_psllw = emit_osize_rf_rfm 0xF1
let emit_pslld = emit_osize_rf_rfm 0xF2
let emit_psllq = emit_osize_rf_rfm 0xF3
let emit_psrlw = emit_osize_rf_rfm 0xD1
let emit_psrld = emit_osize_rf_rfm 0xD2
let emit_psrlq = emit_osize_rf_rfm 0xD3
let emit_psraw = emit_osize_rf_rfm 0xE1
let emit_psrad = emit_osize_rf_rfm 0xE2
let emit_punpckhbw = emit_osize_rf_rfm 0x68
let emit_punpckhwd = emit_osize_rf_rfm 0x69
let emit_punpckhqdq = emit_osize_rf_rfm 0x6D
let emit_punpcklbw = emit_osize_rf_rfm 0x60
let emit_punpcklwd = emit_osize_rf_rfm 0x61
let emit_punpcklqdq = emit_osize_rf_rfm 0x6C
let emit_addsubps = emit_repne_rf_rfm 0xD0
let emit_addsubpd = emit_osize_rf_rfm 0xD0
let emit_haddps = emit_repne_rf_rfm 0x7C
let emit_haddpd = emit_osize_rf_rfm 0x7C
let emit_hsubps = emit_repne_rf_rfm 0x7D
let emit_hsubpd = emit_osize_rf_rfm 0x7D
let emit_movddup = emit_repne_rf_rfm 0x12
let emit_movshdup = emit_rep_rf_rfm 0x16
let emit_movsldup = emit_rep_rf_rfm 0x12
let emit_pabsb = emit_osize_rf_rfm_38 0x1C
let emit_pabsw = emit_osize_rf_rfm_38 0x1D
let emit_pabsd = emit_osize_rf_rfm_38 0x1E
let emit_phaddw = emit_osize_rf_rfm_38 0x01
let emit_phaddd = emit_osize_rf_rfm_38 0x02
let emit_phaddsw = emit_osize_rf_rfm_38 0x03
let emit_phsubw = emit_osize_rf_rfm_38 0x05
let emit_phsubd = emit_osize_rf_rfm_38 0x06
let emit_phsubsw = emit_osize_rf_rfm_38 0x07
let emit_psignb = emit_osize_rf_rfm_38 0x08
let emit_psignw = emit_osize_rf_rfm_38 0x09
let emit_psignd = emit_osize_rf_rfm_38 0x0A
let emit_pshufb = emit_osize_rf_rfm_38 0x00
let emit_pblendvb = emit_osize_rf_rfm_38 0x10
let emit_blendvps = emit_osize_rf_rfm_38 0x14
let emit_blendvpd = emit_osize_rf_rfm_38 0x15
let emit_pcmpeqq = emit_osize_rf_rfm_38 0x29
let emit_pmovsxbw = emit_osize_rf_rfm_38 0x20
let emit_pmovsxbd = emit_osize_rf_rfm_38 0x21
let emit_pmovsxbq = emit_osize_rf_rfm_38 0x22
let emit_pmovsxwd = emit_osize_rf_rfm_38 0x23
let emit_pmovsxwq = emit_osize_rf_rfm_38 0x24
let emit_pmovsxdq = emit_osize_rf_rfm_38 0x25
let emit_pmovzxbw = emit_osize_rf_rfm_38 0x30
let emit_pmovzxbd = emit_osize_rf_rfm_38 0x31
let emit_pmovzxbq = emit_osize_rf_rfm_38 0x32
let emit_pmovzxwd = emit_osize_rf_rfm_38 0x33
let emit_pmovzxwq = emit_osize_rf_rfm_38 0x34
let emit_pmovzxdq = emit_osize_rf_rfm_38 0x35
let emit_pmaxsb = emit_osize_rf_rfm_38 0x3C
let emit_pmaxsd = emit_osize_rf_rfm_38 0x3D
let emit_pmaxuw = emit_osize_rf_rfm_38 0x3E
let emit_pmaxud = emit_osize_rf_rfm_38 0x3F
let emit_pminsb = emit_osize_rf_rfm_38 0x38
let emit_pminsd = emit_osize_rf_rfm_38 0x39
let emit_pminuw = emit_osize_rf_rfm_38 0x3A
let emit_pminud = emit_osize_rf_rfm_38 0x3B
let emit_pcmpgtq = emit_osize_rf_rfm_38 0x37
let emit_pcmpestrm = suffix emit_osize_rf_rfm_3A 0x60
let emit_pcmpestri = suffix emit_osize_rf_rfm_3A 0x61
let emit_pcmpistrm = suffix emit_osize_rf_rfm_3A 0x62
let emit_pcmpistri = suffix emit_osize_rf_rfm_3A 0x63

let emit_pavgb = emit_osize_rf_rfm 0xE0
let emit_pavgw = emit_osize_rf_rfm 0xE3
let emit_psadbw = emit_osize_rf_rfm 0xF6
let emit_packsswb = emit_osize_rf_rfm 0x63
let emit_packssdw = emit_osize_rf_rfm 0x6B
let emit_packuswb = emit_osize_rf_rfm 0x67
let emit_packusdw = emit_osize_rf_rfm_38 0x2B
let emit_palignr = suffix emit_osize_rf_rfm_3A 0x0F
let emit_mpsadbw = suffix emit_osize_rf_rfm_3A 0x42
let emit_phminposuw = emit_osize_rf_rfm_38 0x41

let emit_cmppd = suffix emit_osize_rf_rfm 0xC2
let emit_shufpd = suffix emit_osize_rf_rfm 0xC6
let emit_pshufhw = suffix emit_rep_rf_rfm 0x70
let emit_pshuflw = suffix emit_repne_rf_rfm 0x70
let emit_pblendw = suffix emit_osize_rf_rfm_3A 0x0E
let emit_blendps = suffix emit_osize_rf_rfm_3A 0x0C
let emit_blendpd = suffix emit_osize_rf_rfm_3A 0x0D
let emit_dpps = suffix emit_osize_rf_rfm_3A 0x40
let emit_dppd = suffix emit_osize_rf_rfm_3A 0x41
let emit_roundps = suffix emit_osize_rf_rfm_3A 0x08
let emit_roundpd = suffix emit_osize_rf_rfm_3A 0x09

let emit_pmulhw = emit_osize_rf_rfm 0xE5
let emit_pmulhuw = emit_osize_rf_rfm 0xE4
let emit_pmullw = emit_osize_rf_rfm 0xD5
let emit_pmaddwd = emit_osize_rf_rfm 0xF5
let emit_pmaddubsw = emit_osize_rf_rfm_38 0x04
let emit_pmulld = emit_osize_rf_rfm_38 0x40

let emit_pclmulqdq = suffix emit_osize_rf_rfm_3A 0x44

let emit_osize_rf op rmod b dst =
  match dst with
  | Regf reg ->
      buf_int8 b 0x66;
      let rm = rd_of_regf reg in
      emit_rex b (rex lor rexb_rm rm);
      buf_opcodes b [ 0x0F; op ];
      buf_int8 b (mod_rm_reg 0b11 rm rmod)
  | _ -> assert false

let emit_psllwi b n dst = emit_osize_rf 0x71 0x06 b dst; buf_int8 b n
let emit_pslldi b n dst = emit_osize_rf 0x72 0x06 b dst; buf_int8 b n
let emit_psllqi b n dst = emit_osize_rf 0x73 0x06 b dst; buf_int8 b n
let emit_psrlwi b n dst = emit_osize_rf 0x71 0x02 b dst; buf_int8 b n
let emit_psrldi b n dst = emit_osize_rf 0x72 0x02 b dst; buf_int8 b n
let emit_psrlqi b n dst = emit_osize_rf 0x73 0x02 b dst; buf_int8 b n
let emit_psrawi b n dst = emit_osize_rf 0x71 0x04 b dst; buf_int8 b n
let emit_psradi b n dst = emit_osize_rf 0x72 0x04 b dst; buf_int8 b n
let emit_pslldq b n dst = emit_osize_rf 0x73 0x07 b dst; buf_int8 b n
let emit_psrldq b n dst = emit_osize_rf 0x73 0x03 b dst; buf_int8 b n

let emit_pextrb b n dst src =
  match (dst, src) with
  | ((Reg64 _ | Mem _ | Mem64_RIP _) as rm), Regf reg ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x3A; 0x14 ] rm (rd_of_regf reg);
      buf_int8 b n
  | _ -> assert false

let emit_pextrw b n dst src =
  match (dst, src) with
  | ((Reg64 _ | Mem _ | Mem64_RIP _) as rm), Regf reg ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x3A; 0x15 ] rm (rd_of_regf reg);
      buf_int8 b n
  | _ -> assert false

let emit_pextrd b n dst src =
  match (dst, src) with
  | ((Reg32 _ | Mem _ | Mem64_RIP _) as rm), Regf reg ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x3A; 0x16 ] rm (rd_of_regf reg);
      buf_int8 b n
  | _ -> assert false

let emit_pextrq b n dst src =
  match (dst, src) with
  | ((Reg64 _ | Mem _ | Mem64_RIP _) as rm), Regf reg ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b rexw [ 0x0f; 0x3A; 0x16 ] rm (rd_of_regf reg);
      buf_int8 b n
  | _ -> assert false

let emit_pinsrb b n dst src =
  match (dst, src) with
  | Regf reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x3A; 0x20 ] rm (rd_of_regf reg);
      buf_int8 b n
  | _ -> assert false

let emit_pinsrw b n dst src =
  match (dst, src) with
  | Regf reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0xC4 ] rm (rd_of_regf reg);
      buf_int8 b n
  | _ -> assert false

let emit_pinsrd b n dst src  =
  match (dst, src) with
  | Regf reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x3A; 0x22 ] rm (rd_of_regf reg);
      buf_int8 b n
  | _ -> assert false

let emit_pinsrq b n dst src  =
  match (dst, src) with
  | Regf reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b rexw [ 0x0f; 0x3A; 0x22 ] rm (rd_of_regf reg);
      buf_int8 b n
  | _ -> assert false

let emit_movmskps b dst src =
  match (dst, src) with
  | Reg64 reg, (Regf _ as rm) ->
      emit_mod_rm_reg b 0 [ 0x0f; 0x50 ] rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_pmovmskb b dst src =
  match (dst, src) with
  | Reg64 reg, (Regf _ as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0xD7 ] rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_movmskpd b dst src =
  match (dst, src) with
  | Reg64 reg, (Regf _ as rm) ->
      buf_int8 b 0x66;
      emit_mod_rm_reg b 0 [ 0x0f; 0x50 ] rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_vex3 buf ~rexr ~rexx ~rexb ~vexm ~vexw ~vexv ~vexl ~vexp =
  buf_int8 buf 0xC4;
  buf_int8 buf (((lnot rexr) lsl 7) lor
                ((lnot rexx) lsl 6) lor
                ((lnot rexb) lsl 5) lor
                vexm);
  buf_int8 buf ((vexw lsl 7) lor
                ((lnot vexv) lsl 3) lor
                (vexl lsl 2) lor
                vexp)

let vex_prefix_adaptor f =
  fun b ~rex:_ ~rexr ~rexb ~rexx ->
    let rexr = if rexr <> 0 then 1 else 0 in
    let rexb = if rexb <> 0 then 1 else 0 in
    let rexx = if rexx <> 0 then 1 else 0 in
    f b ~rexr ~rexx ~rexb

let emit_pext b dst src0 src1 =
  match (dst, src0, src1) with
  | Reg64 dreg, Reg64 s0reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as s1rm) ->
    emit_prefix_modrm b [ 0xf5 ] s1rm (rd_of_reg64 dreg)
      ~prefix:(vex_prefix_adaptor
        (emit_vex3 ~vexm:2 ~vexw:1 ~vexv:(rd_of_reg64 s0reg) ~vexl:0 ~vexp:2));
  | _ -> assert false

let emit_pdep b dst src0 src1 =
  match (dst, src0, src1) with
  | Reg64 dreg, Reg64 s0reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as s1rm) ->
    emit_prefix_modrm b [ 0xf5 ] s1rm (rd_of_reg64 dreg)
      ~prefix:(vex_prefix_adaptor
        (emit_vex3 ~vexm:2 ~vexw:1 ~vexv:(rd_of_reg64 s0reg) ~vexl:0 ~vexp:3));
  | _ -> assert false

type simple_encoding = {
  rm8_r8 : int list;
  rm64_r64 : int list;
  r8_rm8 : int list;
  r64_rm64 : int list;
  al_imm8 : int list;
  rax_imm32 : int list;
  rm8_imm8 : int list;
  rm16_imm16 : int list;
  rm64_imm32 : int list;
  rm64_imm8 : int list;
  reg : int;
}

let emit_simple_encoding enc b dst src =
  match (enc, dst, src) with
  (* 64 bits encodings *)
  | { rm64_r64 = opcodes }, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm), Reg64 reg
    ->
      emit_mod_rm_reg b rexw opcodes rm (rd_of_reg64 reg)
  | { rm64_r64 = opcodes }, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm), Reg32 reg
    ->
      emit_mod_rm_reg b 0 opcodes rm (rd_of_reg64 reg)
  | { r64_rm64 = opcodes }, Reg64 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      emit_mod_rm_reg b rexw opcodes rm (rd_of_reg64 reg)
  | { r64_rm64 = opcodes }, Reg32 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      emit_mod_rm_reg b 0 opcodes rm (rd_of_reg64 reg)
  | ( { rm64_imm8 = opcodes; reg },
      ((Reg64 _ | Mem { typ = NONE | QWORD | REAL8; arch = X64 }) as rm),
      Imm n )
    when is_imm8L n ->
      emit_mod_rm_reg b rexw opcodes rm reg;
      buf_int8L b n
  | ( { rm8_imm8 = opcodes; reg },
      ((Reg8L _ | Reg8H _ | Mem { typ = BYTE; arch = X64 }) as rm),
      Imm n ) ->
      assert (is_imm8L n);
      emit_mod_rm_reg b rexw opcodes rm reg;
      buf_int8L b n
  | ( { rm64_imm8 = opcodes; reg },
      ((Reg32 _ | Mem { typ = DWORD | REAL4 } | Mem { typ = NONE; arch = X86 })
      as rm),
      Imm n )
    when is_imm8L n ->
      emit_mod_rm_reg b 0 opcodes rm reg;
      buf_int8L b n
  | { rax_imm32 = opcodes }, Reg64 RAX, ((Imm _ | Sym _) as n) ->
      emit_rex b rexw;
      buf_opcodes b opcodes;
      buf_int32_imm b n
  | { rax_imm32 = opcodes }, Reg32 RAX, ((Imm _ | Sym _) as n) ->
      buf_opcodes b opcodes;
      buf_int32_imm b n
  | ( { rm16_imm16 = opcodes; reg },
      ((Reg16 _ | Mem { typ = WORD })
      as rm),
      (Imm _ as n) ) ->
      emit_mod_rm_reg b 0 opcodes rm reg;
      buf_int16_imm b n
  | ( { rm64_imm32 = opcodes; reg },
      ((Reg32 _ | Mem { typ = NONE; arch = X86 } | Mem { typ = DWORD | REAL4 })
      as rm),
      ((Imm _ | Sym _) as n) ) ->
      emit_mod_rm_reg b 0 opcodes rm reg;
      buf_int32_imm b n
  | ( { rm64_imm32 = opcodes; reg },
      ((Reg64 _ | Mem _ | Mem64_RIP _) as rm),
      ((Imm _ | Sym _) as n) ) ->
      emit_mod_rm_reg b rexw opcodes rm reg;
      buf_int32_imm b n
  | _ ->
      Format.eprintf "src=%a dst=%a@." print_old_arg src print_old_arg dst;
      assert false

let emit_simple_encoding base reg =
  emit_simple_encoding
    {
      rm8_r8 = [ base ];
      rm64_r64 = [ base + 1 ];
      r8_rm8 = [ base + 2 ];
      r64_rm64 = [ base + 3 ];
      al_imm8 = [ base + 4 ];
      rax_imm32 = [ base + 5 ];
      rm8_imm8 = [ 0x80 ];
      rm16_imm16 = [ 0x81 ];
      rm64_imm32 = [ 0x81 ];
      rm64_imm8 = [ 0x83 ];
      reg;
    }

let emit_ADD = emit_simple_encoding 0x00 0

let emit_OR = emit_simple_encoding 0x08 1

let emit_AND = emit_simple_encoding 0x20 4

let emit_SUB = emit_simple_encoding 0x28 5

let emit_XOR = emit_simple_encoding 0x30 6

let emit_CMP = emit_simple_encoding 0x38 7

let emit_test b dst src =
  match (dst, src) with
  | ((Reg32 _ | Mem _ | Mem64_RIP _) as rm), Reg32 reg ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b 0 [ 0x85 ] rm reg
  | ((Reg64 _ | Mem _ | Mem64_RIP _) as rm), Reg64 reg ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x85 ] rm reg
  | Reg64 RAX, ((Imm _ | Sym _) as n) ->
      emit_rex b rexw;
      buf_opcodes b [ 0xA9 ];
      buf_int32_imm b n
  | Reg32 RAX, ((Imm _ | Sym _) as n) ->
      buf_opcodes b [ 0xA9 ];
      buf_int32_imm b n
  | ((Reg32 _ | Reg64 _ | Mem _ | Mem64_RIP _) as rm), ((Imm _ | Sym _) as n) ->
      emit_mod_rm_reg b rexw [ 0xF7 ] rm 0;
      buf_int32_imm b n
  | Reg8L RAX, Imm n ->
      assert (is_imm8L n);
      buf_opcodes b [ 0xA8 ];
      buf_int8L b n
  | ((Reg8L _ | Reg8H _) as rm), Imm n ->
      assert (is_imm8L n);
      emit_mod_rm_reg b 0 [ 0xF6 ] rm 0;
      buf_int8L b n
  | _ -> assert false

(* 3-390 -> 452 *)
let emit_imul b dst src =
  match (dst, src) with
  | Some (Reg32 reg), ((Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b 0 [ 0x0F; 0xAF ] rm reg
  | Some (Reg64 reg), ((Reg64 _ | Mem _ | Mem64_RIP _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x0F; 0xAF ] rm reg
  | Some ((Reg64 reg | Reg32 reg) as rm), Imm n when is_imm8L n ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x6B ] rm reg;
      buf_int8L b n
  | Some ((Reg64 reg | Reg32 reg) as rm), ((Imm _ | Sym _) as n) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x69 ] rm reg;
      buf_int32_imm b n
  | None, ((Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm) ->
      let reg = 5 in
      emit_mod_rm_reg b rexw [ 0xF7 ] rm reg
  | _ -> assert false

let emit_mul b ~src =
  let opcode_extension = 4 in
  match src with
  | ((Reg8H _ | Reg8L _ | Mem {typ = BYTE; _} | Mem64_RIP (BYTE, _, _)) as rm) ->
    emit_mod_rm_reg b rex [ 0xF6 ] rm opcode_extension
  | ((Reg16 _ | Mem {typ = WORD; _} | Mem64_RIP (WORD, _, _)) as rm)
  | ((Reg32 _ | Mem {typ = DWORD; _} | Mem64_RIP (DWORD, _, _)) as rm) ->
    emit_mod_rm_reg b no_rex [ 0xF7 ] rm opcode_extension
  | ((Reg64 _ | Mem {typ = QWORD; _} | Mem64_RIP (QWORD, _, _)) as rm) ->
    emit_mod_rm_reg b rexw [ 0xF7 ] rm opcode_extension
  | _ -> assert false

let emit_idiv b dst =
  let reg = 7 in
  match dst with
  | (Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm ->
      emit_mod_rm_reg b rexw [ 0xF7 ] rm reg
  | _ -> assert false

let emit_shift reg b dst src =
  match (dst, src) with
  | ((Reg64 _ | Reg32 _ | Mem _) as rm), Imm 1L ->
      emit_mod_rm_reg b rexw [ 0xD1 ] rm reg
  | ((Reg64 _ | Reg32 _ | Mem _) as rm), Imm n ->
      assert (is_imm8L n);
      emit_mod_rm_reg b rexw [ 0xC1 ] rm reg;
      buf_int8L b n
  | ((Reg64 _ | Reg32 _) as rm), Reg8L RCX ->
      emit_mod_rm_reg b rexw [ 0xD3 ] rm reg
  | _ ->
      Format.eprintf "emit_shift: src=%a dst=%a@." print_old_arg src
        print_old_arg dst;
      assert false

let emit_SAL b dst src = emit_shift 4 b dst src

let emit_SHR b dst src = emit_shift 5 b dst src

let emit_SAR b dst src = emit_shift 7 b dst src

let record_local_reloc b ?(offset=0) local_reloc =
  local_relocs := (Buffer.length b.buf + offset, local_reloc) :: !local_relocs

let emit_reloc_jump near_opcodes far_opcodes b loc symbol =
  if String.Tbl.mem local_labels symbol then
    (* local_reloc *)
    let target_loc = String.Tbl.find local_labels symbol in
    if target_loc < loc then (
      (* backward *)
      (* The target position is known, and so is the actual offset.  We can
         thus decide locally if a short jump can be used. *)
      let target_pos =
        try label_pos b symbol with Not_found -> assert false
      in
      let source_pos = Buffer.length b.buf in
      assert (target_pos < source_pos);
      let togo = Int64.of_int (target_pos - source_pos) in
      let togo_short =
        Int64.sub togo (Int64.of_int (1 + List.length near_opcodes))
      in

      (*      Printf.printf "%s/%i: backward  togo_short=%Ld\n%!" symbol loc togo_short; *)
      if togo_short >= -128L && togo_short < 128L then (
        buf_opcodes b near_opcodes;
        buf_int8L b togo_short)
      else (
        buf_opcodes b far_opcodes;
        buf_int32L b
          (Int64.sub togo (Int64.of_int (4 + List.length far_opcodes)))))
    else
      (* forward *)
      (* Is the target too far forward (in term of instruction count)
         or have we detected previously that this jump instruction needs
         to be a long one?

         The str_size constant (see below) is chosen to avoid a second
         pass most oftenm while not being overly pessimistic. *)

      (*
      if Int64.of_int ((target_loc - loc) * !instr_size) >= 120L then
        Printf.printf "%s/%i: probably too far (%i)\n%!" symbol loc target_loc
      else if IntSet.mem loc !forced_long_jumps then
        Printf.printf "%s/%i: forced long jump\n%!" symbol loc
      else
        Printf.printf "%s/%i: short\n%!" symbol loc;
*)
      let force_far =
        Int64.of_int ((target_loc - loc) * !instr_size) >= 120L
        || IntSet.mem loc !forced_long_jumps
      in
      if force_far then (
        buf_opcodes b far_opcodes;
        record_local_reloc b (RelocLongJump symbol);
        buf_int32L b 0L)
      else (
        buf_opcodes b near_opcodes;
        record_local_reloc b (RelocShortJump (symbol, loc));
        buf_int8L b 0L)
  else (
    (* external symbol, must reloc *)

    (*    Printf.printf "%s/%i: non local\n%!" symbol loc; *)
    buf_opcodes b far_opcodes;
    record_reloc b (Buffer.length b.buf) (Relocation.Kind.REL32 (symbol, 0L));
    buf_int32L b 0L)

let emit_jmp b loc dst =
  match dst with
  | Sym symbol -> emit_reloc_jump [ 0xEB ] [ 0xE9 ] b loc symbol
  | (Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm ->
      let reg = 4 in
      emit_mod_rm_reg b 0 [ 0xFF ] rm reg
      (* no REX *)
  | _ -> assert false

let emit_call b dst =
  match dst with
  | Sym symbol ->
      buf_int8 b 0xE8;
      if String.Tbl.mem local_labels symbol then
        record_local_reloc b (RelocCall symbol)
      else
        (* external symbol, must reloc *)
        record_reloc b (Buffer.length b.buf)
          (Relocation.Kind.REL32 (symbol, 0L));
      buf_int32L b 0L
  | (Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm ->
      emit_mod_rm_reg b no_rex [ 0xFF ] rm 2
  | _ -> assert false

let emit_j b loc condition dst =
  match dst with
  | Sym symbol ->
      let opcode_offset = cd_of_condition condition in
      emit_reloc_jump [ 0x70 + opcode_offset ]
        [ 0x0F; 0x80 + opcode_offset ]
        b loc symbol
  | _ -> assert false

let imm8_of_float_condition = function
  | EQf -> 0x00
  | LTf -> 0x01
  | LEf -> 0x02
  | UNORDf -> 0x03
  | NEQf -> 0x04
  | NLTf -> 0x05
  | NLEf -> 0x06
  | ORDf -> 0x07

let emit_cmpsd b ~condition ~dst ~src =
  match (dst, src) with
  | (Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* CMPSD xmm1, xmm2/m64, imm8 *)
    let condition = imm8_of_float_condition condition in
    buf_int8 b 0xF2;
    emit_mod_rm_reg b no_rex [ 0x0F; 0xC2 ] rm (rd_of_regf reg);
    buf_int8 b condition
  | _ -> assert false

let emit_cmov b condition dst src =
  match (dst, src) with
  | (Reg64 reg | Reg32 reg), ((Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm)
    ->
      emit_mod_rm_reg b rexw
        [ 0x0F; 0x40 + cd_of_condition condition ]
        rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_set b condition dst =
  match dst with
  | (Reg8L _ | Reg8H _) as rm ->
      emit_mod_rm_reg b 0 [ 0x0F; 0x90 + cd_of_condition condition ] rm 0
  | _ -> assert false

let emit_movsx b dst src =
  match (dst, src) with
  | (Reg64 reg | Reg32 reg), ((Mem { typ = BYTE } | Reg8L _ | Reg8H _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rex [ 0x0F; 0xBE ] rm reg
      (* no REX.W *)
  | (Reg64 reg | Reg32 reg), ((Mem { typ = WORD } | Reg16 _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x0F; 0xBF ] rm reg
  | _ -> assert false

let emit_movsxd b dst src =
  match (dst, src) with
  | (Reg64 reg | Reg32 reg), ((Mem _ | Mem64_RIP _ | Reg32 _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x63 ] rm reg
  | _ -> assert false

let emit_MOVZX b dst src =
  match (dst, src) with
  | (Reg64 reg | Reg32 reg), ((Mem { typ = BYTE } | Reg8L _ | Reg8H _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x0F; 0xB6 ] rm reg
  | Reg64 reg, ((Mem { typ = WORD } | Reg16 _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x0F; 0xB7 ] rm reg
  | Reg32 reg, ((Mem { typ = WORD } | Reg16 _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b 0 [ 0x0F; 0xB7 ] rm reg
  | _ -> assert false

let emit_neg b dst =
  match dst with
  | (Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm ->
      emit_mod_rm_reg b rexw [ 0xF7 ] rm 3
  | _ -> assert false

let emit_LEA b dst src =
  match (dst, src) with
  | Reg64 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b rexw [ 0x8D ] rm reg
  | Reg32 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      let reg = rd_of_reg64 reg in
      emit_mod_rm_reg b 0 [ 0x8D ] rm reg
  (*
        | Reg16 reg, (Mem _ | Mem64_RIP _ as rm) ->
        let reg = rd_of_reg64 reg in
        emit_mod_rm_reg b 0 [ 0x8D ] rm reg
    *)
  | _ ->
      Format.eprintf "lea src=%a dst=%a@." print_old_arg src print_old_arg dst;
      assert false

let emit_lock_cmpxchg b dst src =
  let rex, rm, reg = match (dst, src) with
  | ((Mem _ | Mem64_RIP _) as rm), Reg64 reg ->
    rexw, rm, rd_of_reg64 reg
  | ((Mem _ | Mem64_RIP _) as rm), Reg32 reg ->
    no_rex, rm, rd_of_reg64 reg
  | _ ->
    Misc.fatal_errorf "lock cmpxchg src=%a dst=%a@." print_old_arg src print_old_arg dst
  in
  buf_int8 b 0xF0;
  emit_mod_rm_reg b rex [ 0x0F; 0xB1 ] rm reg

let emit_lock_xadd b dst src =
  let rex, rm, reg = match (dst, src) with
  | ((Mem _ | Mem64_RIP _) as rm), Reg64 reg ->
    rexw, rm, rd_of_reg64 reg
  | ((Mem _ | Mem64_RIP _) as rm), Reg32 reg ->
    no_rex, rm, rd_of_reg64 reg
  | _ ->
    Misc.fatal_errorf "lock cmpxchg src=%a dst=%a@." print_old_arg src print_old_arg dst
  in
  buf_int8 b 0xF0;
  emit_mod_rm_reg b rex [ 0x0F; 0xC1 ] rm reg

let emit_stack_reg b opcode dst =
  match dst with
  | Reg64 reg ->
      let reg = rd_of_reg64 reg in
      if reg > 7 then emit_rex b (rex lor rexb_opcode reg);
      buf_int8 b (opcode + reg7 reg)
  | Reg32 reg ->
      let reg = rd_of_reg64 reg in
      buf_int8 b (opcode + reg7 reg)
  | _ -> assert false

let emit_push b dst =
  match dst with
  | Reg32 _ | Reg64 _ -> emit_stack_reg b 0x50 dst
  | (Mem _ | Mem64_RIP _) as rm -> emit_mod_rm_reg b no_rex [ 0xFF ] rm 6
  | Imm n ->
      if is_imm8L n then (
        buf_int8 b 0x6A;
        buf_int8L b n)
      else (
        assert (is_imm32L n);
        buf_int8 b 0x68;
        buf_int32L b n)
  | Sym sym ->
      buf_int8 b 0x68;
      sym32 b sym
  | _ -> assert false

let emit_pop b dst =
  match dst with
  | Reg32 _ | Reg64 _ -> emit_stack_reg b 0x58 dst
  | (Mem _ | Mem64_RIP _) as rm -> emit_mod_rm_reg b no_rex [ 0x8F ] rm 0
  | _ -> assert false

let emit_popcnt b ~dst ~src =
  match (dst, src) with
  | (Reg16 reg, ((Reg16 _ | Mem _ | Mem64_RIP _) as rm))
  | (Reg32 reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* POPCNT r16, r/m16 and POPCNT r32, r/m32 *)
    buf_int8 b 0xF3;
    emit_mod_rm_reg b no_rex [ 0x0F; 0xB8 ] rm (rd_of_reg64 reg);
  | (Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* POPCNT r64, r/m64 *)
    buf_int8 b 0xF3;
    emit_mod_rm_reg b rexw [ 0x0F; 0xB8 ] rm (rd_of_reg64 reg);
  | _ -> assert false

let emit_tzcnt b ~dst ~src =
  match (dst, src) with
  | (Reg16 reg, ((Reg16 _ | Mem _ | Mem64_RIP _) as rm))
  | (Reg32 reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* TZCNT r16, r/m16 and TZCNT r32, r/m32 *)
    buf_int8 b 0xF3;
    emit_mod_rm_reg b no_rex [ 0x0F; 0xBC ] rm (rd_of_reg64 reg);
  | (Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* TZCNT r64, r/m64 *)
    buf_int8 b 0xF3;
    emit_mod_rm_reg b rexw [ 0x0F; 0xBC ] rm (rd_of_reg64 reg);
  | _ -> assert false

let emit_lzcnt b ~dst ~src =
  match (dst, src) with
  | (Reg16 reg, ((Reg16 _ | Mem _ | Mem64_RIP _) as rm))
  | (Reg32 reg, ((Reg32 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* LZCNT r16, r/m16 and LZCNT r32, r/m32 *)
    buf_int8 b 0xF3;
    emit_mod_rm_reg b no_rex [ 0x0F; 0xBD ] rm (rd_of_reg64 reg);
  | (Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* LZCNT r64, r/m64 *)
    buf_int8 b 0xF3;
    emit_mod_rm_reg b rexw [ 0x0F; 0xBD ] rm (rd_of_reg64 reg);
  | _ -> assert false

let rd_of_prefetch_hint = function
  | Nta -> 0
  | T0 -> 1
  | T1 -> 2
  | T2 -> 3

let emit_prefetch b ~is_write ~hint rm =
  match (is_write, hint, rm) with
  | (false, _, (Mem _ | Mem64_RIP _)) ->
    (* PREFETCHT0 m8, PREFETCHT1 m8, PREFETCHT1 m8 and PREFETCHNTA m8 *)
    emit_mod_rm_reg b no_rex [ 0x0F; 0x18 ] rm (rd_of_prefetch_hint hint)
  | (true, T0, (Mem _ | Mem64_RIP _)) ->
    (* PREFETCHW m8 *)
    emit_mod_rm_reg b no_rex [ 0x0F; 0x0D ] rm (rd_of_prefetch_hint hint)
  | (true, (T1 | T2 | Nta), (Mem _ | Mem64_RIP _)) ->
    (* PREFETCHWT1 m8 *)
    (* This sticks to X86_gas' behaviour which emit prefetchwt1 if hint is
       [T2 | Nta] *)
    emit_mod_rm_reg b no_rex [ 0x0F; 0x0D ] rm (rd_of_prefetch_hint T1)
  | _ -> assert false

let emit_pause b = buf_opcodes b [ 0xF3; 0x90 ]

let emit_rdtsc b = buf_opcodes b [ 0x0F; 0x31 ]

let emit_rdpmc b = buf_opcodes b [ 0x0F; 0x33 ]

let emit_lfence b = buf_opcodes b [ 0x0F; 0xAE; 0xE8 ]

let emit_sfence b = buf_opcodes b [ 0x0F; 0xAE; 0xF8 ]

let emit_mfence b = buf_opcodes b [ 0x0F; 0xAE; 0xF0 ]

let emit_leave b = buf_int8 b 0xC9

let emit_maxsd b ~dst ~src =
  match (dst, src) with
  | (Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm)) ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b no_rex [ 0x0F; 0x5F ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_minsd b ~dst ~src =
  match (dst, src) with
  | (Regf reg, ((Regf _ | Mem _ | Mem64_RIP _) as rm)) ->
    buf_int8 b 0xF2;
    emit_mod_rm_reg b no_rex [ 0x0F; 0x5D ] rm (rd_of_regf reg)
  | _ -> assert false

let emit_inc b = function
  | (Reg64 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm ->
      emit_mod_rm_reg b rexw [ 0xFF ] rm 0
  | _ -> assert false

let emit_DEC b = function
  (* FE /1 DEC r/m8 M Valid Valid *)
  | [ ((Reg8L _ | Reg8H _ | Mem { typ = BYTE }) as rm) ] ->
      emit_mod_rm_reg b no_rex [ 0xFE ] rm 1
  (* FF /1 DEC r/m16 M Valid Valid *)
  | [ ((Reg16 _ | Mem { typ = WORD }) as rm) ] ->
      emit_mod_rm_reg b no_rex [ 0x66; 0xFF ] rm 1
  (* FF /1 DEC r/m32 M Valid Valid *)
  | [ ((Reg32 _ | Mem { typ = DWORD }) as rm) ] ->
      emit_mod_rm_reg b no_rex [ 0xFF ] rm 1
  (* REX.W + FF /1 DEC r/m64 M Valid N.E. *)
  | [ ((Reg64 _ | Mem { typ = QWORD }) as rm) ] ->
      emit_mod_rm_reg b rexw [ 0xFF ] rm 1
  | _ -> assert false

let emit_ret b = buf_int8 b 0xC3

let emit_cqto b =
  emit_rex b rexw;
  buf_int8 b 0x99

let emit_crc32 b ~dst ~src =
  match (dst, src) with
  | (Reg32 reg, ((Reg8L _ | Reg8H _ | Mem {typ = BYTE; _} | Mem64_RIP (BYTE, _, _)) as rm)) ->
    (* CRC32 r32, r/m8 *)
    buf_int8 b 0xF2;
    emit_mod_rm_reg b rex [ 0x0F; 0x38; 0xF0 ] rm (rd_of_reg64 reg)
  | (Reg64 reg, ((Reg8L _ | Reg8H _ | Mem {typ = BYTE; _} | Mem64_RIP (BYTE, _, _)) as rm)) ->
    (* CRC32 r64, r/m8 *)
    buf_int8 b 0xF2;
    emit_mod_rm_reg b rexw [ 0x0F; 0x38; 0xF0 ] rm (rd_of_reg64 reg)
  | (Reg32 reg, ((Reg16 _ | Reg32 _ | Mem _ | Mem64_RIP _) as rm)) ->
    (* CRC32 r32, r/m16 and CRC32 r32, r/m32 *)
    buf_int8 b 0xF2;
    emit_mod_rm_reg b no_rex [ 0x0F; 0x38; 0xF1 ] rm (rd_of_reg64 reg)
  | (Reg64 reg, ((Reg64 _ | Mem _ | Mem64_RIP _) as rm)) ->
     (* CRC32 r64, r/m64 *)
    buf_int8 b 0xF2;
    emit_mod_rm_reg b rexw [ 0x0F; 0x38; 0xF1 ] rm (rd_of_reg64 reg)
  | _ -> assert false

let emit_BSWAP b = function
  | Reg32 reg -> buf_opcodes b [ 0x0F; 0xC8 + reg7 (rd_of_reg64 reg) ]
  | Reg64 reg ->
      let reg = rd_of_reg64 reg in
      emit_rex b (rexw lor rexb_opcode reg);
      buf_opcodes b [ 0x0F; 0xC8 + reg7 reg ]
  | _ -> assert false

let emit_XCHG b src dst =
  (* TODO: test ! *)
  match (dst, src) with
  | ((Reg64 _ | Mem _ | Mem64_RIP _) as rm), Reg64 reg
  | Reg64 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      (* r64, r/m64 *)
      emit_mod_rm_reg b rexw [ 0x87 ] rm (rd_of_reg64 reg)
  | ((Reg32 _ | Mem _ | Mem64_RIP _) as rm), Reg32 reg
  | Reg32 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      (* r32, r/m32 *)
      emit_mod_rm_reg b no_rex [ 0x87 ] rm (rd_of_reg64 reg)
  | ((Reg16 _ | Mem _ | Mem64_RIP _) as rm), Reg16 reg
  | Reg16 reg, ((Mem _ | Mem64_RIP _) as rm) ->
      (* r16, r/m16 *)
      emit_mod_rm_reg b rex [ 0x66; 0x87 ] rm (rd_of_reg64 reg)
  | ( ((Reg8L _ | Reg8H _ | Mem _ | Mem64_RIP _) as rm),
      ((Reg8L _ | Reg8H _) as reg) )
  | ((Reg8L _ | Reg8H _) as reg), ((Mem _ | Mem64_RIP _) as rm) ->
      (* r8, r/m8 *)
      emit_mod_rm_reg b no_rex [ 0x86 ] rm (rd_of_reg8 reg)
  | _ -> assert false

let imm arg = match arg with Imm n -> Int64.to_int n | _ -> assert false

let assemble_instr b loc = function
  | ADD (src, dst) -> emit_ADD b dst src
  | ADDSD (src, dst) -> emit_addsd b dst src
  | AND (src, dst) -> emit_AND b dst src
  | ANDPD (src, dst) -> emit_andpd b dst src
  | BSF (src, dst) -> emit_bsf b ~dst ~src
  | BSR (src, dst) -> emit_bsr b ~dst ~src
  | BSWAP arg -> emit_BSWAP b arg
  | CALL dst -> emit_call b dst
  | CVTSI2SS (src, dst) -> emit_CVTSI2SS b dst src
  | CVTSI2SD (src, dst) -> emit_CVTSI2SD b dst src
  | CVTSD2SI (src, dst) -> emit_CVTSD2SI b dst src
  | CVTSS2SI (src, dst) -> emit_CVTSS2SI b dst src
  | CVTTSS2SI (src, dst) -> emit_CVTTSS2SI b dst src
  | CVTTSD2SI (src, dst) -> emit_CVTTSD2SI b dst src
  | CVTSD2SS (src, dst) -> emit_CVTSD2SS b dst src
  | CVTSS2SD (src, dst) -> emit_CVTSS2SD b dst src
  | COMISD (src, dst) -> emit_comisd b dst src
  | CQO -> emit_cqto b
  | CMP (src, dst) -> emit_CMP b dst src
  | CMPSD (condition, src, dst) -> emit_cmpsd b ~condition ~dst ~src
  | CMOV (condition, src, dst) -> emit_cmov b condition dst src
  | CDQ -> buf_int8 b 0x99
  | DIVSD (src, dst) -> emit_divsd b dst src
  | DEC dst -> emit_DEC b [ dst ]
  | HLT -> buf_int8 b 0xF4
  | INC dst -> emit_inc b dst
  | IMUL (src, dst) -> emit_imul b dst src
  | MUL src -> emit_mul b ~src
  | IDIV dst -> emit_idiv b dst
  | J (condition, dst) -> emit_j b !loc condition dst
  | JMP dst -> emit_jmp b !loc dst
  | LEAVE -> emit_leave b
  | LEA (src, dst) -> emit_LEA b dst src
  | LOCK_CMPXCHG (src, dst) -> emit_lock_cmpxchg b dst src
  | LOCK_XADD (src, dst) -> emit_lock_xadd b dst src
  | MAXSD (src, dst) -> emit_maxsd b ~dst ~src
  | MINSD (src, dst) -> emit_minsd b ~dst ~src
  | MOV (src, dst) -> emit_MOV b dst src
  | MOVAPD (src, dst) -> emit_movapd b dst src
  | MOVUPD (src, dst) -> emit_movupd b dst src
  | MOVD (src, dst) -> emit_movd b ~dst ~src
  | MOVQ (src, dst) -> emit_movq b ~dst ~src
  | MOVLPD (src, dst) -> emit_movlpd b dst src
  | MOVSD (src, dst) -> emit_movsd b dst src
  | MOVSS (src, dst) -> emit_movss b dst src
  | MULSD (src, dst) -> emit_mulsd b dst src
  | MOVSX (src, dst) -> emit_movsx b dst src
  | MOVZX (src, dst) -> emit_MOVZX b dst src
  | MOVSXD (src, dst) -> emit_movsxd b dst src
  | NEG dst -> emit_neg b dst
  | NOP -> buf_int8 b 0x90
  | OR (src, dst) -> emit_OR b dst src
  | PAUSE -> emit_pause b
  | PUSH dst -> emit_push b dst
  | POP dst -> emit_pop b dst
  | POPCNT (src, dst) -> emit_popcnt b ~dst ~src
  | PREFETCH (is_write, hint, rm) -> emit_prefetch b ~is_write ~hint rm
  | RDTSC -> emit_rdtsc b
  | RDPMC -> emit_rdpmc b
  | LFENCE -> emit_lfence b
  | SFENCE -> emit_sfence b
  | MFENCE -> emit_mfence b
  | RET -> emit_ret b
  | ROUNDSD (rounding, src, dst) -> emit_roundsd b dst rounding src
  | SAL (src, dst) -> emit_SAL b dst src
  | SAR (src, dst) -> emit_SAR b dst src
  | SHR (src, dst) -> emit_SHR b dst src
  | SUBSD (src, dst) -> emit_subsd b dst src
  | SQRTSD (src, dst) -> emit_sqrtsd b dst src
  | SUB (src, dst) -> emit_SUB b dst src
  | SET (condition, dst) -> emit_set b condition dst
  | TEST (src, dst) -> emit_test b dst src
  | UCOMISD (src, dst) -> emit_ucomisd b dst src
  | XCHG (src, dst) -> emit_XCHG b dst src
  | XOR (src, dst) -> emit_XOR b dst src
  | XORPD (src, dst) -> emit_xorpd b dst src
  | SSE CMPPS (cmp, src, dst) -> emit_cmpps b (imm8_of_float_condition cmp) dst src
  | SSE ADDPS (src, dst) -> emit_addps b dst src
  | SSE SUBPS (src, dst) -> emit_subps b dst src
  | SSE MULPS (src, dst) -> emit_mulps b dst src
  | SSE DIVPS (src, dst) -> emit_divps b dst src
  | SSE MAXPS (src, dst) -> emit_maxps b dst src
  | SSE MINPS (src, dst) -> emit_minps b dst src
  | SSE RCPPS (src, dst) -> emit_rcpps b dst src
  | SSE SQRTPS (src, dst) -> emit_sqrtps b dst src
  | SSE RSQRTPS (src, dst) -> emit_rsqrtps b dst src
  | SSE MOVHLPS (src, dst) -> emit_movhlps b dst src
  | SSE MOVLHPS (src, dst) -> emit_movlhps b dst src
  | SSE UNPCKHPS (src, dst) -> emit_unpckhps b dst src
  | SSE UNPCKLPS (src, dst) -> emit_unpcklps b dst src
  | SSE MOVMSKPS (src, dst) -> emit_movmskps b dst src
  | SSE SHUFPS (shuf, src, dst) -> emit_shufps b (imm shuf) dst src
  | SSE2 PADDB (src, dst) -> emit_paddb b dst src
  | SSE2 PADDW (src, dst) -> emit_paddw b dst src
  | SSE2 PADDD (src, dst) -> emit_paddd b dst src
  | SSE2 PADDQ (src, dst) -> emit_paddq b dst src
  | SSE2 ADDPD (src, dst) -> emit_addpd b dst src
  | SSE2 PADDSB (src, dst) -> emit_paddsb b dst src
  | SSE2 PADDSW (src, dst) -> emit_paddsw b dst src
  | SSE2 PADDUSB (src, dst) -> emit_paddusb b dst src
  | SSE2 PADDUSW (src, dst) -> emit_paddusw b dst src
  | SSE2 PSUBB (src, dst) -> emit_psubb b dst src
  | SSE2 PSUBW (src, dst) -> emit_psubw b dst src
  | SSE2 PSUBD (src, dst) -> emit_psubd b dst src
  | SSE2 PSUBQ (src, dst) -> emit_psubq b dst src
  | SSE2 SUBPD (src, dst) -> emit_subpd b dst src
  | SSE2 PSUBSB (src, dst) -> emit_psubsb b dst src
  | SSE2 PSUBSW (src, dst) -> emit_psubsw b dst src
  | SSE2 PSUBUSB (src, dst) -> emit_psubusb b dst src
  | SSE2 PSUBUSW (src, dst) -> emit_psubusw b dst src
  | SSE2 PMAXUB (src, dst) -> emit_pmaxub b dst src
  | SSE2 PMAXSW (src, dst) -> emit_pmaxsw b dst src
  | SSE2 MAXPD (src, dst) -> emit_maxpd b dst src
  | SSE2 PMINUB (src, dst) -> emit_pminub b dst src
  | SSE2 PMINSW (src, dst) -> emit_pminsw b dst src
  | SSE2 MINPD (src, dst) -> emit_minpd b dst src
  | SSE2 MULPD (src, dst) -> emit_mulpd b dst src
  | SSE2 DIVPD (src, dst) -> emit_divpd b dst src
  | SSE2 SQRTPD (src, dst) -> emit_sqrtpd b dst src
  | SSE2 PAND (src, dst) -> emit_pand b dst src
  | SSE2 PANDNOT (src, dst) -> emit_pandnot b dst src
  | SSE2 POR (src, dst) -> emit_por b dst src
  | SSE2 PXOR (src, dst) -> emit_pxor b dst src
  | SSE2 PMOVMSKB (src, dst) -> emit_pmovmskb b dst src
  | SSE2 MOVMSKPD (src, dst) -> emit_movmskpd b dst src
  | SSE2 PSLLDQ (n, dst) -> emit_pslldq b (imm n) dst
  | SSE2 PSRLDQ (n, dst) -> emit_psrldq b (imm n) dst
  | SSE2 PCMPEQB (src, dst) -> emit_pcmpeqb b dst src
  | SSE2 PCMPEQW (src, dst) -> emit_pcmpeqw b dst src
  | SSE2 PCMPEQD (src, dst) -> emit_pcmpeqd b dst src
  | SSE2 PCMPGTB (src, dst) -> emit_pcmpgtb b dst src
  | SSE2 PCMPGTW (src, dst) -> emit_pcmpgtw b dst src
  | SSE2 PCMPGTD (src, dst) -> emit_pcmpgtd b dst src
  | SSE2 CMPPD (n, src, dst) -> emit_cmppd b (imm8_of_float_condition n) dst src
  | SSE2 CVTDQ2PD (src, dst) -> emit_cvtdq2pd b dst src
  | SSE2 CVTDQ2PS (src, dst) -> emit_cvtdq2ps b dst src
  | SSE2 CVTPD2DQ (src, dst) -> emit_cvtpd2dq b dst src
  | SSE2 CVTPD2PS (src, dst) -> emit_cvtpd2ps b dst src
  | SSE2 CVTPS2DQ (src, dst) -> emit_cvtps2dq b dst src
  | SSE2 CVTPS2PD (src, dst) -> emit_cvtps2pd b dst src
  | SSE2 PSLLW (src, dst) -> emit_psllw b dst src
  | SSE2 PSLLD (src, dst) -> emit_pslld b dst src
  | SSE2 PSLLQ (src, dst) -> emit_psllq b dst src
  | SSE2 PSRLW (src, dst) -> emit_psrlw b dst src
  | SSE2 PSRLD (src, dst) -> emit_psrld b dst src
  | SSE2 PSRLQ (src, dst) -> emit_psrlq b dst src
  | SSE2 PSRAW (src, dst) -> emit_psraw b dst src
  | SSE2 PSRAD (src, dst) -> emit_psrad b dst src
  | SSE2 PSLLWI (n, dst) -> emit_psllwi b (imm n) dst
  | SSE2 PSLLDI (n, dst) -> emit_pslldi b (imm n) dst
  | SSE2 PSLLQI (n, dst) -> emit_psllqi b (imm n) dst
  | SSE2 PSRLWI (n, dst) -> emit_psrlwi b (imm n) dst
  | SSE2 PSRLDI (n, dst) -> emit_psrldi b (imm n) dst
  | SSE2 PSRLQI (n, dst) -> emit_psrlqi b (imm n) dst
  | SSE2 PSRAWI (n, dst) -> emit_psrawi b (imm n) dst
  | SSE2 PSRADI (n, dst) -> emit_psradi b (imm n) dst
  | SSE2 SHUFPD (n, src, dst) -> emit_shufpd b (imm n) dst src
  | SSE2 PSHUFHW (n, src, dst) -> emit_pshufhw b (imm n) dst src
  | SSE2 PSHUFLW (n, src, dst) -> emit_pshuflw b (imm n) dst src
  | SSE2 PUNPCKHBW (src, dst) -> emit_punpckhbw b dst src
  | SSE2 PUNPCKHWD (src, dst) -> emit_punpckhwd b dst src
  | SSE2 PUNPCKHQDQ (src, dst) -> emit_punpckhqdq b dst src
  | SSE2 PUNPCKLBW (src, dst) -> emit_punpcklbw b dst src
  | SSE2 PUNPCKLWD (src, dst) -> emit_punpcklwd b dst src
  | SSE2 PUNPCKLQDQ (src, dst) -> emit_punpcklqdq b dst src
  | SSE2 PAVGB (src, dst) -> emit_pavgb b dst src
  | SSE2 PAVGW (src, dst) -> emit_pavgw b dst src
  | SSE2 PSADBW (src, dst) -> emit_psadbw b dst src
  | SSE2 PACKSSWB (src, dst) -> emit_packsswb b dst src
  | SSE2 PACKSSDW (src, dst) -> emit_packssdw b dst src
  | SSE2 PACKUSWB (src, dst) -> emit_packuswb b dst src
  | SSE2 PACKUSDW (src, dst) -> emit_packusdw b dst src
  | SSE2 PMULHW (src, dst) -> emit_pmulhw b dst src
  | SSE2 PMULHUW (src, dst) -> emit_pmulhuw b dst src
  | SSE2 PMULLW (src, dst) -> emit_pmullw b dst src
  | SSE2 PMADDWD (src, dst) -> emit_pmaddwd b dst src
  | SSE3 ADDSUBPS (src, dst) -> emit_addsubps b dst src
  | SSE3 ADDSUBPD (src, dst) -> emit_addsubpd b dst src
  | SSE3 HADDPS (src, dst) -> emit_haddps b dst src
  | SSE3 HADDPD (src, dst) -> emit_haddpd b dst src
  | SSE3 HSUBPS (src, dst) -> emit_hsubps b dst src
  | SSE3 HSUBPD (src, dst) -> emit_hsubpd b dst src
  | SSE3 MOVDDUP (src, dst) -> emit_movddup b dst src
  | SSE3 MOVSHDUP (src, dst) -> emit_movshdup b dst src
  | SSE3 MOVSLDUP (src, dst) -> emit_movsldup b dst src
  | SSSE3 PABSB (src, dst) -> emit_pabsb b dst src
  | SSSE3 PABSW (src, dst) -> emit_pabsw b dst src
  | SSSE3 PABSD (src, dst) -> emit_pabsd b dst src
  | SSSE3 PHADDW (src, dst) -> emit_phaddw b dst src
  | SSSE3 PHADDD (src, dst) -> emit_phaddd b dst src
  | SSSE3 PHADDSW (src, dst) -> emit_phaddsw b dst src
  | SSSE3 PHSUBW (src, dst) -> emit_phsubw b dst src
  | SSSE3 PHSUBD (src, dst) -> emit_phsubd b dst src
  | SSSE3 PHSUBSW (src, dst) -> emit_phsubsw b dst src
  | SSSE3 PSIGNB (src, dst) -> emit_psignb b dst src
  | SSSE3 PSIGNW (src, dst) -> emit_psignw b dst src
  | SSSE3 PSIGND (src, dst) -> emit_psignd b dst src
  | SSSE3 PSHUFB (src, dst) -> emit_pshufb b dst src
  | SSSE3 PALIGNR (n, src, dst) -> emit_palignr b (imm n) dst src
  | SSE41 PBLENDW (n, src, dst) -> emit_pblendw b (imm n) dst src
  | SSE41 BLENDPS (n, src, dst) -> emit_blendps b (imm n) dst src
  | SSE41 BLENDPD (n, src, dst) -> emit_blendpd b (imm n) dst src
  | SSE41 PBLENDVB (src, dst) -> emit_pblendvb b dst src
  | SSE41 BLENDVPS (src, dst) -> emit_blendvps b dst src
  | SSE41 BLENDVPD (src, dst) -> emit_blendvpd b dst src
  | SSE41 PCMPEQQ (src, dst) -> emit_pcmpeqq b dst src
  | SSE41 PMOVSXBW (src, dst) -> emit_pmovsxbw b dst src
  | SSE41 PMOVSXBD (src, dst) -> emit_pmovsxbd b dst src
  | SSE41 PMOVSXBQ (src, dst) -> emit_pmovsxbq b dst src
  | SSE41 PMOVSXWD (src, dst) -> emit_pmovsxwd b dst src
  | SSE41 PMOVSXWQ (src, dst) -> emit_pmovsxwq b dst src
  | SSE41 PMOVSXDQ (src, dst) -> emit_pmovsxdq b dst src
  | SSE41 PMOVZXBW (src, dst) -> emit_pmovzxbw b dst src
  | SSE41 PMOVZXBD (src, dst) -> emit_pmovzxbd b dst src
  | SSE41 PMOVZXBQ (src, dst) -> emit_pmovzxbq b dst src
  | SSE41 PMOVZXWD (src, dst) -> emit_pmovzxwd b dst src
  | SSE41 PMOVZXWQ (src, dst) -> emit_pmovzxwq b dst src
  | SSE41 PMOVZXDQ (src, dst) -> emit_pmovzxdq b dst src
  | SSE41 DPPS (n, src, dst) -> emit_dpps b (imm n) dst src
  | SSE41 DPPD (n, src, dst) -> emit_dppd b (imm n) dst src
  | SSE41 PEXTRB (n, src, dst) -> emit_pextrb b (imm n) dst src
  | SSE41 PEXTRW (n, src, dst) -> emit_pextrw b (imm n) dst src
  | SSE41 PEXTRD (n, src, dst) -> emit_pextrd b (imm n) dst src
  | SSE41 PEXTRQ (n, src, dst) -> emit_pextrq b (imm n) dst src
  | SSE41 PINSRB (n, src, dst) -> emit_pinsrb b (imm n) dst src
  | SSE41 PINSRW (n, src, dst) -> emit_pinsrw b (imm n) dst src
  | SSE41 PINSRD (n, src, dst) -> emit_pinsrd b (imm n) dst src
  | SSE41 PINSRQ (n, src, dst) -> emit_pinsrq b (imm n) dst src
  | SSE41 PMAXSB (src, dst) -> emit_pmaxsb b dst src
  | SSE41 PMAXSD (src, dst) -> emit_pmaxsd b dst src
  | SSE41 PMAXUW (src, dst) -> emit_pmaxuw b dst src
  | SSE41 PMAXUD (src, dst) -> emit_pmaxud b dst src
  | SSE41 PMINSB (src, dst) -> emit_pminsb b dst src
  | SSE41 PMINSD (src, dst) -> emit_pminsd b dst src
  | SSE41 PMINUW (src, dst) -> emit_pminuw b dst src
  | SSE41 PMINUD (src, dst) -> emit_pminud b dst src
  | SSE41 ROUNDPD (n, src, dst) -> emit_roundpd b (imm8_of_rounding n) dst src
  | SSE41 ROUNDPS (n, src, dst) -> emit_roundps b (imm8_of_rounding n) dst src
  | SSE41 PHMINPOSUW (src, dst) -> emit_phminposuw b dst src
  | SSE41 PMULLD (src, dst) -> emit_pmulld b dst src
  | SSE41 MPSADBW (n, src, dst) -> emit_mpsadbw b (imm n) dst src
  | SSE42 PCMPGTQ (src, dst) -> emit_pcmpgtq b dst src
  | SSE42 PCMPESTRI (n, src, dst) -> emit_pcmpestri b (imm n) dst src
  | SSE42 PCMPESTRM (n, src, dst) -> emit_pcmpestrm b (imm n) dst src
  | SSE42 PCMPISTRI (n, src, dst) -> emit_pcmpistri b (imm n) dst src
  | SSE42 PCMPISTRM (n, src, dst) -> emit_pcmpistrm b (imm n) dst src
  | SSE42 CRC32 (src, dst) -> emit_crc32 b ~dst ~src
  | PCLMULQDQ (n, src, dst) -> emit_pclmulqdq b (imm n) dst src
  | SSSE3 PMADDUBSW (src, dst) -> emit_pmaddubsw b dst src
  | PEXT (src1, src0, dst) -> emit_pext b dst src0 src1
  | PDEP (src1, src0, dst) -> emit_pdep b dst src0 src1
  | TZCNT (src, dst) -> emit_tzcnt b ~dst ~src
  | LZCNT (src, dst) -> emit_lzcnt b ~dst ~src

let assemble_line b loc ins =
  try
    match ins with
    | Ins instr ->
        assemble_instr b loc instr;
        incr loc
    | Comment _ -> ()
    | Global sym -> (get_symbol b sym).sy_binding <- Sy_global
    | Weak sym -> (get_symbol b sym).sy_binding <- Sy_weak
    | Protected sym -> (get_symbol b sym).sy_protected <- true
    | Quad (Const n) -> buf_int64L b n
    | Quad cst ->
        record_local_reloc b (RelocConstant (cst, B64));
        buf_int64L b 0L
    | Long (Const n) -> buf_int32L b n
    | Long cst ->
        record_local_reloc b (RelocConstant (cst, B32));
        buf_int32L b 0L
    | Word (Const n) -> buf_int16L b n
    | Word cst ->
        record_local_reloc b (RelocConstant (cst, B16));
        buf_int16L b 0L
    | Byte (Const n) -> buf_int8L b n
    | Byte cst ->
        record_local_reloc b (RelocConstant (cst, B8));
        buf_int8L b 0L
    | NewLabel (s, _) -> declare_label b s
    | Bytes s -> Buffer.add_string b.buf s
    | External (_, _) -> ()
    | Set (_, _) -> assert false
    | Section _ -> assert false
    | Mode386 -> assert (system = S_win32)
    | Model _ -> assert (system = S_win32)
    | Cfi_startproc -> ()
    | Cfi_endproc -> ()
    | Cfi_adjust_cfa_offset _ -> ()
    | Cfi_remember_state -> ()
    | Cfi_restore_state -> ()
    | Cfi_def_cfa_register _ -> ()
    | Cfi_def_cfa_offset _ -> ()
    | File _ -> ()
    | Loc _ -> ()
    | Private_extern _ -> assert false
    | Indirect_symbol _ -> assert false
    | Type (lbl, kind) -> (get_symbol b lbl).sy_type <- Some kind
    | Size (lbl, cst) -> (
        match eval_const b (Buffer.length b.buf) cst with
        | Rint n -> (get_symbol b lbl).sy_size <- Some (Int64.to_int n)
        | _ -> assert false)
    | Align (data, n) -> (
        (* TODO: Buffer.length = 0 => set section align *)
        let pos = Buffer.length b.buf in
        let current = pos mod n in
        if current > 0 then
          let n = n - current in
          if data then
            for _ = 1 to n do
              buf_int8 b 0x00
            done
          else
            match n with
            | 0 -> ()
            | 1 -> buf_int8 b 0x90
            | 2 -> buf_opcodes b [ 0x66; 0x90 ]
            | 3 -> buf_opcodes b [ 0x0f; 0x1f; 0x00 ]
            | 4 -> buf_opcodes b [ 0x0f; 0x1f; 0x40; 0x00 ]
            | 5 -> buf_opcodes b [ 0x0f; 0x1f; 0x44; 0x00; 0x00 ]
            | 6 ->
                buf_opcodes b [ 0x66; 0x0f; 0x1f; 0x44 ];
                buf_int16L b 0L
            | 7 ->
                buf_opcodes b [ 0x0f; 0x1f; 0x80 ];
                buf_int32L b 0L
            | _ ->
                for _ = 9 to n do
                  buf_int8 b 0x66
                done;
                buf_opcodes b [ 0x0f; 0x1f; 0x84; 0x00 ];
                buf_int32L b 0L)
    | Space n ->
        (* TODO: in text section, should be NOP *)
        for _ = 1 to n do
          buf_int8 b 0
        done
    | Hidden _ | NewLine -> ()
    | Reloc { name = R_X86_64_PLT32;
              expr = ConstSub (ConstLabel wrap_label, Const 4L);
              offset = ConstSub (ConstThis, Const 4L);
            }  when String.Tbl.mem local_labels wrap_label ->
      record_local_reloc b ~offset:(-4) (RelocCall wrap_label)
    | Reloc _ | Sleb128 _ | Uleb128 _ | Direct_assignment _ ->
      X86_gas.generate_asm Out_channel.stderr [ins];
      Misc.fatal_errorf "x86_binary_emitter: unsupported instruction"
  with e ->
    Printf.eprintf "Exception %s:\n%!" (Printexc.to_string e);
    (*
    Printf.eprintf "   masm: %s%!"
      (string_of_buffer X86_masm.bprint_instr !arch64 ins);
    Printf.eprintf "   gas : %s%!"
      (string_of_buffer X86_gas.bprint_instr !arch64 ins);
*)
    raise e

let add_patch b pos size v = b.patches <- (pos, size, v) :: b.patches

let assemble_section arch section =
  (match arch with X86 -> instr_size := 5 | X64 -> instr_size := 6);
  forced_long_jumps := IntSet.empty;
  String.Tbl.clear local_labels;

  let icount = ref 0 in
  ArrayLabels.iter section.sec_instrs ~f:(function
    | NewLabel (lbl, _) ->
        String.Tbl.add local_labels lbl !icount
    | Ins _ -> incr icount
    | _ -> ());

  let passes = ref 0 in

  let rec iter_assemble () =
    incr passes;

    (*     if !passes >= 2 then Printf.eprintf "[binary backend] pass %i\n%!" !passes; *)
    let b = new_buffer section in
    local_relocs := [];

    let loc = ref 0 in
    ArrayLabels.iter ~f:(assemble_line b loc) section.sec_instrs;

    let retry = ref false in

    let do_local_reloc pos = function
      | RelocShortJump (label, loc) ->
          let source_pos = pos + 1 in
          let target_pos = label_pos b label in
          let n = target_pos - source_pos in
          if n >= -128 && n < 128 then add_patch b pos B8 (Int64.of_int n)
          else (
            (* We thought this could be a short jump, but actually, this is
               not the case.  Force another pass and remember to use
               a long jump for this instruction. *)
            forced_long_jumps := IntSet.add loc !forced_long_jumps;
            retry := true)
      | RelocCall label | RelocLongJump label ->
          let source_pos = pos + 4 in
          let target_pos = label_pos b label in
          let n = target_pos - source_pos in
          add_patch b pos B32 (Int64.of_int n)
      (* TODO: here, we resolve all computations in each section, i.e. we can only
         allow one external symbol per expression. We could tolerate more complex
         expressions if we delay resolution later, i.e. after all sections have
         been generated and all symbol positions are known. *)
      | RelocConstant (cst, data_size) -> (
          (* Printf.eprintf "RelocConstant (%s, %s)\n%!"
             (X86_gas.string_of_constant cst)
             (string_of_data_size data_size); *)
          let v = eval_const b pos cst in
          match (v, data_size) with
          | Rint n, _ -> add_patch b pos data_size n
          | Rabs (lbl, offset), B32 ->
              record_reloc b pos (Relocation.Kind.DIR32 (lbl, offset))
          | Rabs (lbl, offset), B64 ->
              record_reloc b pos (Relocation.Kind.DIR64 (lbl, offset))
          (* Relative relocation in data segment. We add an offset of 4 because
              REL32 relocations are computed with a PC at the end, while here, it
              is at the beginning. *)
          | Rrel (lbl, offset), B32 ->
              record_reloc b pos
                (Relocation.Kind.REL32 (lbl, Int64.add offset 4L))
          | Rrel _, _ -> assert false
          | Rabs _, _ -> assert false)
    in

    ListLabels.iter !local_relocs ~f:(fun (pos, local_reloc) ->
        do_local_reloc pos local_reloc);

    if !retry then iter_assemble () else b
  in
  iter_assemble ()

(* Relocations: we should compute all non-local relocations completely at the
   end. We should keep the last string/bytes couple to avoid duplication.
   All external labels should be absolute (ConstLabelAbs), while internal
   labels should be replaced by a relative computation. The goal is to make
   all computations either absolute, or relative to the current offset.
*)

let size b = Buffer.length b.buf

let add_patch ~offset ~size ~data t = add_patch t offset size data

let contents_mut b =
  let buf = Buffer.to_bytes b.buf in
  ListLabels.iter b.patches ~f:(fun (pos, nbits, v) ->
      (*    Printf.eprintf "Apply patch %s @%d\n%!" (string_of_data_size nbits) pos; *)
      match nbits with
      | B64 -> str_int64L buf pos v
      | B32 -> str_int32L buf pos v
      | B16 -> str_int16L buf pos v
      | B8 -> str_int8L buf pos v);
  buf

let contents b =
  Bytes.to_string (contents_mut b)

let relocations b = b.relocations

let labels b = b.labels
