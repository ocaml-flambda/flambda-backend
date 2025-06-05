(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

open! Int_replace_polymorphic_compare
open X86_ast
open X86_proc
open Amd64_simd_instrs

let bprintf = Printf.bprintf

let print_reg b f r =
  Buffer.add_char b '%';
  Buffer.add_string b (f r)

let opt_displ b displ =
  if displ = 0
  then ()
  else if displ > 0
  then bprintf b "+%d" displ
  else bprintf b "%d" displ

let arg_mem b { arch; typ = _; idx; scale; base; sym; displ } =
  let string_of_register =
    match arch with X86 -> string_of_reg32 | X64 -> string_of_reg64
  in
  (match sym with
  | None ->
    if displ <> 0 || scale = 0 then Buffer.add_string b (Int.to_string displ)
  | Some s ->
    Buffer.add_string b s;
    opt_displ b displ);
  if scale <> 0
  then (
    Buffer.add_char b '(';
    (match base with
    | None -> ()
    | Some base -> print_reg b string_of_register base);
    if base != None || scale <> 1 then Buffer.add_char b ',';
    print_reg b string_of_register idx;
    if scale <> 1 then bprintf b ",%s" (Int.to_string scale);
    Buffer.add_char b ')')

let arg b = function
  | Sym x ->
    Buffer.add_char b '$';
    Buffer.add_string b x
  | Imm x -> bprintf b "$%Ld" x
  | Reg8L x -> print_reg b string_of_reg8l x
  | Reg8H x -> print_reg b string_of_reg8h x
  | Reg16 x -> print_reg b string_of_reg16 x
  | Reg32 x -> print_reg b string_of_reg32 x
  | Reg64 x -> print_reg b string_of_reg64 x
  | Regf x -> print_reg b string_of_regf x
  | Mem addr -> arg_mem b addr
  | Mem64_RIP (_, s, displ) -> bprintf b "%s%a(%%rip)" s opt_displ displ

let typeof = function
  | Mem { typ; _ } | Mem64_RIP (typ, _, _) -> typ
  | Reg8L _ | Reg8H _ -> BYTE
  | Reg16 _ -> WORD
  | Reg32 _ -> DWORD
  | Reg64 _ -> QWORD
  | Imm _ | Sym _ -> NONE
  | Regf _ -> assert false

let suf arg =
  match typeof arg with
  | BYTE -> "b"
  | WORD -> "w"
  | DWORD | REAL8 -> "l"
  | QWORD -> "q"
  | REAL4 -> "s"
  | VEC128 | VEC256 | VEC512 | NONE -> ""
  | NEAR | PROC -> assert false

let i0 b s = bprintf b "\t%s" s

let i1 b s x = bprintf b "\t%s\t%a" s arg x

let i1_s b s x = bprintf b "\t%s%s\t%a" s (suf x) arg x

let i2 b s x y = bprintf b "\t%s\t%a, %a" s arg x arg y

let i2_s b s x y = bprintf b "\t%s%s\t%a, %a" s (suf y) arg x arg y

let i2_sx b s x y = bprintf b "\t%s%s\t%a, %a" s (suf x) arg x arg y

let i2_ss b s x y = bprintf b "\t%s%s%s\t%a, %a" s (suf x) (suf y) arg x arg y

let i3 b s x y z = bprintf b "\t%s\t%a, %a, %a" s arg x arg y arg z

let i1_call_jmp b s = function
  (* this is the encoding of jump labels: don't use * *)
  | Mem { arch = X86; idx = _; scale = 0; base = None; sym = Some _; _ } as x ->
    i1 b s x
  | (Reg32 _ | Reg64 _ | Mem { arch = X64 | X86; _ } | Mem64_RIP _) as x ->
    bprintf b "\t%s\t*%a" s arg x
  | Sym x -> bprintf b "\t%s\t%s" s x
  | Imm _ | Reg8L _ | Reg8H _ | Reg16 _ | Regf _ -> assert false

let print_instr b = function
  | ADD (arg1, arg2) -> i2_s b "add" arg1 arg2
  | ADDSD (arg1, arg2) -> i2 b "addsd" arg1 arg2
  | AND (arg1, arg2) -> i2_s b "and" arg1 arg2
  | ANDPD (arg1, arg2) -> i2 b "andpd" arg1 arg2
  | BSF (arg1, arg2) -> i2_s b "bsf" arg1 arg2
  | BSR (arg1, arg2) -> i2_s b "bsr" arg1 arg2
  | BSWAP arg -> i1 b "bswap" arg
  | CALL arg -> i1_call_jmp b "call" arg
  | CDQ -> i0 b "cltd"
  | CLDEMOTE arg -> i1 b "cldemote" arg
  | CMOV (c, arg1, arg2) -> i2 b ("cmov" ^ string_of_condition c) arg1 arg2
  | CMP (arg1, arg2) -> i2_s b "cmp" arg1 arg2
  | CMPSD (c, arg1, arg2) ->
    i2 b ("cmp" ^ string_of_float_condition c ^ "sd") arg1 arg2
  | COMISD (arg1, arg2) -> i2 b "comisd" arg1 arg2
  | CQO -> i0 b "cqto"
  | CVTSI2SS (arg1, arg2) -> i2 b ("cvtsi2ss" ^ suf arg1) arg1 arg2
  | CVTSD2SS (arg1, arg2) -> i2 b "cvtsd2ss" arg1 arg2
  | CVTSI2SD (arg1, arg2) -> i2 b ("cvtsi2sd" ^ suf arg1) arg1 arg2
  | CVTSS2SD (arg1, arg2) -> i2 b "cvtss2sd" arg1 arg2
  | CVTTSS2SI (arg1, arg2) -> i2_s b "cvttss2si" arg1 arg2
  | CVTTSD2SI (arg1, arg2) -> i2_s b "cvttsd2si" arg1 arg2
  | DEC arg -> i1_s b "dec" arg
  | DIVSD (arg1, arg2) -> i2 b "divsd" arg1 arg2
  | HLT -> i0 b "hlt"
  | IDIV arg -> i1_s b "idiv" arg
  | IMUL (arg, None) -> i1_s b "imul" arg
  | IMUL (arg1, Some arg2) -> i2_s b "imul" arg1 arg2
  | MUL arg -> i1_s b "mul" arg
  | INC arg -> i1_s b "inc" arg
  | J (c, arg) -> i1_call_jmp b ("j" ^ string_of_condition c) arg
  | JMP arg -> i1_call_jmp b "jmp" arg
  | LEA (arg1, arg2) -> i2_s b "lea" arg1 arg2
  | LOCK_CMPXCHG (arg1, arg2) -> i2_sx b "lock cmpxchg" arg1 arg2
  | LOCK_XADD (arg1, arg2) -> i2_sx b "lock xadd" arg1 arg2
  | LOCK_ADD (arg1, arg2) -> i2_sx b "lock add" arg1 arg2
  | LOCK_SUB (arg1, arg2) -> i2_sx b "lock sub" arg1 arg2
  | LOCK_AND (arg1, arg2) -> i2_sx b "lock and" arg1 arg2
  | LOCK_OR (arg1, arg2) -> i2_sx b "lock or" arg1 arg2
  | LOCK_XOR (arg1, arg2) -> i2_sx b "lock xor" arg1 arg2
  | LEAVE -> i0 b "leave"
  | MOV ((Imm n as arg1), (Reg64 _ as arg2))
    when not
           (Int64.compare n 0x7FFF_FFFFL <= 0
           && Int64.compare n (-0x8000_0000L) >= 0) ->
    i2 b "movabsq" arg1 arg2
  | MOV ((Sym _ as arg1), (Reg64 _ as arg2)) when windows ->
    i2 b "movabsq" arg1 arg2
  | MOV
      ( (( Reg8L _ | Imm _ | Sym _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _
         | Regf _ | Mem _
         | Mem64_RIP (_, _, _) ) as arg1),
        (( Reg8L _ | Imm _ | Sym _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _
         | Regf _ | Mem _
         | Mem64_RIP (_, _, _) ) as arg2) ) ->
    i2_s b "mov" arg1 arg2
  | MOVAPD (arg1, arg2) -> i2 b "movapd" arg1 arg2
  | MOVUPD (arg1, arg2) -> i2 b "movupd" arg1 arg2
  | MOVD (arg1, arg2) -> i2 b "movd" arg1 arg2
  | MOVQ (arg1, arg2) -> i2 b "movq" arg1 arg2
  | MOVLPD (arg1, arg2) -> i2 b "movlpd" arg1 arg2
  | MOVSD (arg1, arg2) -> i2 b "movsd" arg1 arg2
  | MOVSS (arg1, arg2) -> i2 b "movss" arg1 arg2
  | MOVSX (arg1, arg2) -> i2_ss b "movs" arg1 arg2
  | MOVSXD (arg1, arg2) -> i2 b "movslq" arg1 arg2
  | MOVZX (arg1, arg2) -> i2_ss b "movz" arg1 arg2
  | MULSD (arg1, arg2) -> i2 b "mulsd" arg1 arg2
  | NEG arg -> i1 b "neg" arg
  | NOP -> i0 b "nop"
  | OR (arg1, arg2) -> i2_s b "or" arg1 arg2
  | PAUSE -> i0 b "pause"
  | POP arg -> i1_s b "pop" arg
  | POPCNT (arg1, arg2) -> i2_s b "popcnt" arg1 arg2
  | PREFETCH (is_write, hint, arg1) -> (
    match is_write, hint with
    | true, T0 -> i1 b "prefetchw" arg1
    | true, (T1 | T2 | Nta) -> i1 b "prefetchwt1" arg1
    | false, (T0 | T1 | T2 | Nta) ->
      i1 b ("prefetch" ^ string_of_prefetch_temporal_locality_hint hint) arg1)
  | PUSH arg -> i1_s b "push" arg
  | RDTSC -> i0 b "rdtsc"
  | RDPMC -> i0 b "rdpmc"
  | LFENCE -> i0 b "lfence"
  | SFENCE -> i0 b "sfence"
  | MFENCE -> i0 b "mfence"
  | RET -> i0 b "ret"
  | SAL (arg1, arg2) -> i2_s b "sal" arg1 arg2
  | SAR (arg1, arg2) -> i2_s b "sar" arg1 arg2
  | SET (c, arg) -> i1 b ("set" ^ string_of_condition c) arg
  | SHR (arg1, arg2) -> i2_s b "shr" arg1 arg2
  | SUB (arg1, arg2) -> i2_s b "sub" arg1 arg2
  | SUBSD (arg1, arg2) -> i2 b "subsd" arg1 arg2
  | TEST (arg1, arg2) -> i2_s b "test" arg1 arg2
  | UCOMISD (arg1, arg2) -> i2 b "ucomisd" arg1 arg2
  | XCHG (arg1, arg2) -> i2 b "xchg" arg1 arg2
  | XOR (arg1, arg2) -> i2_s b "xor" arg1 arg2
  | XORPD (arg1, arg2) -> i2 b "xorpd" arg1 arg2
  | ADDSS (arg1, arg2) -> i2 b "addss" arg1 arg2
  | SUBSS (arg1, arg2) -> i2 b "subss" arg1 arg2
  | MULSS (arg1, arg2) -> i2 b "mulss" arg1 arg2
  | DIVSS (arg1, arg2) -> i2 b "divss" arg1 arg2
  | COMISS (arg1, arg2) -> i2 b "comiss" arg1 arg2
  | UCOMISS (arg1, arg2) -> i2 b "ucomiss" arg1 arg2
  | XORPS (arg1, arg2) -> i2 b "xorps" arg1 arg2
  | ANDPS (arg1, arg2) -> i2 b "andps" arg1 arg2
  | CMPSS (cmp, arg1, arg2) ->
    i2 b ("cmp" ^ string_of_float_condition cmp ^ "ss") arg1 arg2
  | LZCNT (arg1, arg2) -> i2_s b "lzcnt" arg1 arg2
  | TZCNT (arg1, arg2) -> i2_s b "tzcnt" arg1 arg2
  | SIMD (instr, args) -> (
    match[@warning "-4"] instr.id, args with
    (* The assembler won't accept these mnemonics directly. *)
    | Cmpps, [| imm; arg1; arg2 |] ->
      i2 b ("cmp" ^ string_of_float_condition_imm imm ^ "ps") arg1 arg2
    | Cmppd, [| imm; arg1; arg2 |] ->
      i2 b ("cmp" ^ string_of_float_condition_imm imm ^ "pd") arg1 arg2
    | Crc32_r64_r64m64, [| arg1; arg2 |] -> i2 b "crc32q" arg1 arg2
    (* All other simd instructions. *)
    | _, [| arg1; arg2 |] -> i2 b instr.mnemonic arg1 arg2
    | _, [| arg1; arg2; arg3 |] -> i3 b instr.mnemonic arg1 arg2 arg3
    | _, _ ->
      Misc.fatal_errorf "unexpected instruction layout for %s (%d args)"
        instr.mnemonic (Array.length args))

let print_line b i =
  match i with
  | Ins i -> print_instr b i
  | Directive d -> Asm_targets.Asm_directives.Directive.print b d

let generate_asm oc lines =
  let b = Buffer.create 10000 in
  output_string oc "\t.file \"\"\n";
  (* PR#7037 *)
  List.iter
    (fun i ->
      Buffer.clear b;
      print_line b i;
      Buffer.add_char b '\n';
      Buffer.output_buffer oc b)
    lines
