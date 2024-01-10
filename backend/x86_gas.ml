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

open X86_ast
open X86_proc

let bprintf = Printf.bprintf

let print_reg b f r =
  Buffer.add_char b '%';
  Buffer.add_string b (f r)

let opt_displ b displ =
  if displ = 0 then ()
  else if displ > 0 then bprintf b "+%d" displ
  else bprintf b "%d" displ

let arg_mem b {arch; typ=_; idx; scale; base; sym; displ} =
  let string_of_register =
    match arch with
    | X86 -> string_of_reg32
    | X64 -> string_of_reg64
  in
  begin match sym with
  | None ->
      if displ <> 0 || scale = 0 then
        Buffer.add_string b (Int.to_string displ)
  | Some s ->
      Buffer.add_string b s;
      opt_displ b displ
  end;
  if scale <> 0 then begin
    Buffer.add_char b '(';
    begin match base with
    | None -> ()
    | Some base -> print_reg b string_of_register base
    end;
    if base != None || scale <> 1 then Buffer.add_char b ',';
    print_reg b string_of_register idx;
    if scale <> 1 then bprintf b ",%s" (Int.to_string scale);
    Buffer.add_char b ')'
  end

let reloc_type_to_string = function
  | R_X86_64_PLT32 -> "R_X86_64_PLT32"

let arg b = function
  | Sym x -> Buffer.add_char b '$'; Buffer.add_string b x
  | Imm x -> bprintf b "$%Ld" x
  | Reg8L x -> print_reg b string_of_reg8l x
  | Reg8H x -> print_reg b string_of_reg8h x
  | Reg16 x -> print_reg b string_of_reg16 x
  | Reg32 x -> print_reg b string_of_reg32 x
  | Reg64 x -> print_reg b string_of_reg64 x
  | Regf x  -> print_reg b string_of_regf x
  | Mem addr -> arg_mem b addr
  | Mem64_RIP (_, s, displ) -> bprintf b "%s%a(%%rip)" s opt_displ displ

let rec cst b = function
  | ConstLabel _ | Const _ | ConstThis as c -> scst b c
  | ConstAdd (c1, c2) -> bprintf b "%a + %a" scst c1 scst c2
  | ConstSub (c1, c2) -> bprintf b "%a - %a" scst c1 scst c2

and scst b = function
  | ConstThis -> Buffer.add_string b "."
  | ConstLabel l -> Buffer.add_string b l
  | Const n when n <= 0x7FFF_FFFFL && n >= -0x8000_0000L ->
      Buffer.add_string b (Int64.to_string n)
  | Const n -> bprintf b "0x%Lx" n
  | ConstAdd (c1, c2) -> bprintf b "(%a + %a)" scst c1 scst c2
  | ConstSub (c1, c2) -> bprintf b "(%a - %a)" scst c1 scst c2

let typeof = function
  | Mem {typ; _} | Mem64_RIP (typ, _, _) -> typ
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
  | VEC128 | NONE -> ""
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
  | Mem {arch=X86; idx=_;   scale=0; base=None; sym=Some _; _} as x ->
      i1 b s x
  | Reg32 _ | Reg64 _ | Mem _  | Mem64_RIP _ as x ->
      bprintf b "\t%s\t*%a" s arg x
  | Sym x -> bprintf b "\t%s\t%s" s x
  | _ -> assert false

let print_instr b = function
  | ADD (arg1, arg2) -> i2_s b "add" arg1 arg2
  | ADDSD (arg1, arg2) -> i2 b "addsd" arg1 arg2
  | AND (arg1, arg2) -> i2_s b "and" arg1 arg2
  | ANDPD (arg1, arg2) -> i2 b "andpd" arg1 arg2
  | BSF (arg1, arg2) -> i2_s b "bsf" arg1 arg2
  | BSR (arg1, arg2) -> i2_s b "bsr" arg1 arg2
  | BSWAP arg -> i1 b "bswap" arg
  | CALL arg  -> i1_call_jmp b "call" arg
  | CDQ -> i0 b "cltd"
  | CMOV (c, arg1, arg2) -> i2 b ("cmov" ^ string_of_condition c) arg1 arg2
  | CMP (arg1, arg2) -> i2_s b "cmp" arg1 arg2
  | CMPSD (c, arg1, arg2) ->
      i2 b ("cmp" ^ string_of_float_condition c ^ "sd") arg1 arg2
  | COMISD (arg1, arg2) -> i2 b "comisd" arg1 arg2
  | CQO ->  i0 b "cqto"
  | CRC32 (arg1, arg2) -> i2_s b "crc32" arg1 arg2
  | CVTSD2SI (arg1, arg2) -> i2 b "cvtsd2si" arg1 arg2
  | CVTSD2SS (arg1, arg2) -> i2 b "cvtsd2ss" arg1 arg2
  | CVTSI2SD (arg1, arg2) -> i2 b ("cvtsi2sd" ^ suf arg1) arg1 arg2
  | CVTSS2SD (arg1, arg2) -> i2 b "cvtss2sd" arg1 arg2
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
  | LEAVE -> i0 b "leave"
  | MAXSD (arg1, arg2) -> i2 b "maxsd" arg1 arg2
  | MINSD (arg1, arg2) -> i2 b "minsd" arg1 arg2
  | MOV ((Imm n as arg1), (Reg64 _ as arg2))
    when not (n <= 0x7FFF_FFFFL && n >= -0x8000_0000L) ->
      i2 b "movabsq" arg1 arg2
  | MOV ((Sym _ as arg1), (Reg64 _ as arg2)) when windows ->
      i2 b "movabsq" arg1 arg2
  | MOV (arg1, arg2) -> i2_s b "mov" arg1 arg2
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
  | POP  arg -> i1_s b "pop" arg
  | POPCNT (arg1, arg2) -> i2_s b "popcnt" arg1 arg2
  | PREFETCH (is_write, hint, arg1) ->
    (match is_write, hint with
     | true, T0 -> i1 b "prefetchw" arg1
     | true, (T1|T2|Nta) -> i1 b "prefetchwt1" arg1
     | false, (T0|T1|T2|Nta) ->
       i1 b ("prefetch" ^ string_of_prefetch_temporal_locality_hint hint) arg1)
  | PUSH arg -> i1_s b "push" arg
  | RDTSC -> i0 b "rdtsc"
  | RDPMC -> i0 b "rdpmc"
  | LFENCE -> i0 b "lfence"
  | SFENCE -> i0 b "sfence"
  | MFENCE -> i0 b "mfence"
  | RET ->  i0 b "ret"
  | ROUNDSD (r, arg1, arg2) -> i3 b "roundsd" (imm_of_rounding r) arg1 arg2
  | SAL (arg1, arg2) -> i2_s b "sal" arg1 arg2
  | SAR (arg1, arg2) -> i2_s b "sar" arg1 arg2
  | SET (c, arg) -> i1 b ("set" ^ string_of_condition c) arg
  | SHR (arg1, arg2) -> i2_s b "shr" arg1 arg2
  | SQRTSD (arg1, arg2) -> i2 b "sqrtsd" arg1 arg2
  | SUB (arg1, arg2) -> i2_s b "sub" arg1 arg2
  | SUBSD (arg1, arg2) -> i2 b "subsd" arg1 arg2
  | TEST (arg1, arg2) -> i2_s b "test" arg1 arg2
  | UCOMISD (arg1, arg2) -> i2 b "ucomisd" arg1 arg2
  | XCHG (arg1, arg2) -> i2 b "xchg" arg1 arg2
  | XOR (arg1, arg2) -> i2_s b "xor" arg1 arg2
  | XORPD (arg1, arg2) -> i2 b "xorpd" arg1 arg2
  | CMPPS (cmp, arg1, arg2) -> i2 b ("cmp" ^ string_of_float_condition cmp ^ "ps") arg1 arg2
  | SHUFPS (shuf, arg1, arg2) -> i3 b "shufps" shuf arg1 arg2
  | ADDPS (arg1, arg2) -> i2 b "addps" arg1 arg2
  | SUBPS (arg1, arg2) -> i2 b "subps" arg1 arg2
  | MULPS (arg1, arg2) -> i2 b "mulps" arg1 arg2
  | DIVPS (arg1, arg2) -> i2 b "divps" arg1 arg2
  | MAXPS (arg1, arg2) -> i2 b "maxps" arg1 arg2
  | MINPS (arg1, arg2) -> i2 b "minps" arg1 arg2
  | RCPPS (arg1, arg2) -> i2 b "rcpps" arg1 arg2
  | SQRTPS (arg1, arg2) -> i2 b "sqrtps" arg1 arg2
  | RSQRTPS (arg1, arg2) -> i2 b "rsqrtps" arg1 arg2
  | MOVHLPS (arg1, arg2) -> i2 b "movhlps" arg1 arg2
  | MOVLHPS (arg1, arg2) -> i2 b "movlhps" arg1 arg2
  | UNPCKHPS (arg1, arg2) -> i2 b "unpckhps" arg1 arg2
  | UNPCKLPS (arg1, arg2) -> i2 b "unpcklps" arg1 arg2
  | MOVMSKPS (arg1, arg2) -> i2 b "movmskps" arg1 arg2
  | PADDB (arg1, arg2) -> i2 b "paddb" arg1 arg2
  | PADDW (arg1, arg2) -> i2 b "paddw" arg1 arg2
  | PADDD (arg1, arg2) -> i2 b "paddd" arg1 arg2
  | PADDQ (arg1, arg2) -> i2 b "paddq" arg1 arg2
  | ADDPD (arg1, arg2) -> i2 b "addpd" arg1 arg2
  | PADDSB (arg1, arg2) -> i2 b "paddsb" arg1 arg2
  | PADDSW (arg1, arg2) -> i2 b "paddsw" arg1 arg2
  | PADDUSB (arg1, arg2) -> i2 b "paddusb" arg1 arg2
  | PADDUSW (arg1, arg2) -> i2 b "paddusw" arg1 arg2
  | PSUBB (arg1, arg2) -> i2 b "psubb" arg1 arg2
  | PSUBW (arg1, arg2) -> i2 b "psubw" arg1 arg2
  | PSUBD (arg1, arg2) -> i2 b "psubd" arg1 arg2
  | PSUBQ (arg1, arg2) -> i2 b "psubq" arg1 arg2
  | SUBPD (arg1, arg2) -> i2 b "subpd" arg1 arg2
  | PSUBSB (arg1, arg2) -> i2 b "psubsb" arg1 arg2
  | PSUBSW (arg1, arg2) -> i2 b "psubsw" arg1 arg2
  | PSUBUSB (arg1, arg2) -> i2 b "psubusb" arg1 arg2
  | PSUBUSW (arg1, arg2) -> i2 b "psubusw" arg1 arg2
  | PMAXUB (arg1, arg2) -> i2 b "pmaxub" arg1 arg2
  | PMAXSW (arg1, arg2) -> i2 b "pmaxsw" arg1 arg2
  | MAXPD (arg1, arg2) -> i2 b "maxpd" arg1 arg2
  | PMINUB (arg1, arg2) -> i2 b "pminub" arg1 arg2
  | PMINSW (arg1, arg2) -> i2 b "pminsw" arg1 arg2
  | MINPD (arg1, arg2) -> i2 b "minpd" arg1 arg2
  | MULPD (arg1, arg2) -> i2 b "mulpd" arg1 arg2
  | DIVPD (arg1, arg2) -> i2 b "divpd" arg1 arg2
  | SQRTPD (arg1, arg2) -> i2 b "sqrtpd" arg1 arg2
  | PAND (arg1, arg2) -> i2 b "pand" arg1 arg2
  | PANDNOT (arg1, arg2) -> i2 b "pandn" arg1 arg2
  | POR (arg1, arg2) -> i2 b "por" arg1 arg2
  | PXOR (arg1, arg2) -> i2 b "pxor" arg1 arg2
  | PMOVMSKB (arg1, arg2) -> i2 b "pmovmskb" arg1 arg2
  | MOVMSKPD (arg1, arg2) -> i2 b "movmskpd" arg1 arg2
  | PSLLDQ (bytes, arg1) -> i2 b "pslldq" bytes arg1
  | PSRLDQ (bytes, arg1) -> i2 b "psrldq" bytes arg1
  | PCMPEQB (arg1, arg2) -> i2 b "pcmpeqb" arg1 arg2
  | PCMPEQW (arg1, arg2) -> i2 b "pcmpeqw" arg1 arg2
  | PCMPEQD (arg1, arg2) -> i2 b "pcmpeqd" arg1 arg2
  | PCMPGTB (arg1, arg2) -> i2 b "pcmpgtb" arg1 arg2
  | PCMPGTW (arg1, arg2) -> i2 b "pcmpgtw" arg1 arg2
  | PCMPGTD (arg1, arg2) -> i2 b "pcmpgtd" arg1 arg2
  | CMPPD (cmp, arg1, arg2) -> i2 b ("cmp" ^ string_of_float_condition cmp ^ "pd") arg1 arg2
  | CVTDQ2PD (arg1, arg2) -> i2 b "cvtdq2pd" arg1 arg2
  | CVTDQ2PS (arg1, arg2) -> i2 b "cvtdq2ps" arg1 arg2
  | CVTPD2DQ (arg1, arg2) -> i2 b "cvtpd2dq" arg1 arg2
  | CVTPD2PS (arg1, arg2) -> i2 b "cvtpd2ps" arg1 arg2
  | CVTPS2DQ (arg1, arg2) -> i2 b "cvtps2dq" arg1 arg2
  | CVTPS2PD (arg1, arg2) -> i2 b "cvtps2pd" arg1 arg2
  | PSLLW (arg1, arg2) -> i2 b "psllw" arg1 arg2
  | PSLLD (arg1, arg2) -> i2 b "pslld" arg1 arg2
  | PSLLQ (arg1, arg2) -> i2 b "psllq" arg1 arg2
  | PSRLW (arg1, arg2) -> i2 b "psrlw" arg1 arg2
  | PSRLD (arg1, arg2) -> i2 b "psrld" arg1 arg2
  | PSRLQ (arg1, arg2) -> i2 b "psrlq" arg1 arg2
  | PSRAW (arg1, arg2) -> i2 b "psraw" arg1 arg2
  | PSRAD (arg1, arg2) -> i2 b "psrad" arg1 arg2
  | PSLLWI (bits, arg1) -> i2 b "psllw" bits arg1
  | PSLLDI (bits, arg1) -> i2 b "pslld" bits arg1
  | PSLLQI (bits, arg1) -> i2 b "psllq" bits arg1
  | PSRLWI (bits, arg1) -> i2 b "psrlw" bits arg1
  | PSRLDI (bits, arg1) -> i2 b "psrld" bits arg1
  | PSRLQI (bits, arg1) -> i2 b "psrlq" bits arg1
  | PSRAWI (bits, arg1) -> i2 b "psraw" bits arg1
  | PSRADI (bits, arg1) -> i2 b "psrad" bits arg1
  | SHUFPD (shuf, arg1, arg2) -> i3 b "shufpd" shuf arg1 arg2
  | PSHUFHW (shuf, arg1, arg2) -> i3 b "pshufhw" shuf arg1 arg2
  | PSHUFLW (shuf, arg1, arg2) -> i3 b "pshuflw" shuf arg1 arg2
  | PUNPCKHBW (arg1, arg2) -> i2 b "punpckhbw" arg1 arg2
  | PUNPCKHWD (arg1, arg2) -> i2 b "punpckhwd" arg1 arg2
  | PUNPCKHQDQ (arg1, arg2) -> i2 b "punpckhqdq" arg1 arg2
  | PUNPCKLBW (arg1, arg2) -> i2 b "punpcklbw" arg1 arg2
  | PUNPCKLWD (arg1, arg2) -> i2 b "punpcklwd" arg1 arg2
  | PUNPCKLQDQ (arg1, arg2) -> i2 b "punpcklqdq" arg1 arg2
  | ADDSUBPS (arg1, arg2) -> i2 b "addsubps" arg1 arg2
  | ADDSUBPD (arg1, arg2) -> i2 b "addsubpd" arg1 arg2
  | HADDPS (arg1, arg2) -> i2 b "haddps" arg1 arg2
  | HADDPD (arg1, arg2) -> i2 b "haddpd" arg1 arg2
  | HSUBPS (arg1, arg2) -> i2 b "hsubps" arg1 arg2
  | HSUBPD (arg1, arg2) -> i2 b "hsubpd" arg1 arg2
  | MOVDDUP (arg1, arg2) -> i2 b "movddup" arg1 arg2
  | MOVSHDUP (arg1, arg2) -> i2 b "movshdup" arg1 arg2
  | MOVSLDUP (arg1, arg2) -> i2 b "movsldup" arg1 arg2
  | PABSB (arg1, arg2) -> i2 b "pabsb" arg1 arg2
  | PABSW (arg1, arg2) -> i2 b "pabsw" arg1 arg2
  | PABSD (arg1, arg2) -> i2 b "pabsd" arg1 arg2
  | PHADDW (arg1, arg2) -> i2 b "phaddw" arg1 arg2
  | PHADDD (arg1, arg2) -> i2 b "phaddd" arg1 arg2
  | PHADDSW (arg1, arg2) -> i2 b "phaddsw" arg1 arg2
  | PHSUBW (arg1, arg2) -> i2 b "phsubw" arg1 arg2
  | PHSUBD (arg1, arg2) -> i2 b "phsubd" arg1 arg2
  | PHSUBSW (arg1, arg2) -> i2 b "phsubsw" arg1 arg2
  | PSIGNB (arg1, arg2) -> i2 b "psignb" arg1 arg2
  | PSIGNW (arg1, arg2) -> i2 b "psignw" arg1 arg2
  | PSIGND (arg1, arg2) -> i2 b "psignd" arg1 arg2
  | PSHUFB (arg1, arg2) -> i2 b "pshufb" arg1 arg2
  | PBLENDW (lanes, arg1, arg2) -> i3 b "pblendw" lanes arg1 arg2
  | BLENDPS (lanes, arg1, arg2) -> i3 b "blendps" lanes arg1 arg2
  | BLENDPD (lanes, arg1, arg2) -> i3 b "blendpd" lanes arg1 arg2
  | PBLENDVB (arg1, arg2) -> i2 b "pblendvb" arg1 arg2
  | BLENDVPS (arg1, arg2) -> i2 b "blendvps" arg1 arg2
  | BLENDVPD (arg1, arg2) -> i2 b "blendvpd" arg1 arg2
  | PCMPEQQ (arg1, arg2) -> i2 b "pcmpeqq" arg1 arg2
  | PMOVSXBW (arg1, arg2) -> i2 b "pmovsxbw" arg1 arg2
  | PMOVSXBD (arg1, arg2) -> i2 b "pmovsxbd" arg1 arg2
  | PMOVSXBQ (arg1, arg2) -> i2 b "pmovsxbq" arg1 arg2
  | PMOVSXWD (arg1, arg2) -> i2 b "pmovsxwd" arg1 arg2
  | PMOVSXWQ (arg1, arg2) -> i2 b "pmovsxwq" arg1 arg2
  | PMOVSXDQ (arg1, arg2) -> i2 b "pmovsxdq" arg1 arg2
  | PMOVZXBW (arg1, arg2) -> i2 b "pmovzxbw" arg1 arg2
  | PMOVZXBD (arg1, arg2) -> i2 b "pmovzxbd" arg1 arg2
  | PMOVZXBQ (arg1, arg2) -> i2 b "pmovzxbq" arg1 arg2
  | PMOVZXWD (arg1, arg2) -> i2 b "pmovzxwd" arg1 arg2
  | PMOVZXWQ (arg1, arg2) -> i2 b "pmovzxwq" arg1 arg2
  | PMOVZXDQ (arg1, arg2) -> i2 b "pmovzxdq" arg1 arg2
  | DPPS (sel, arg1, arg2) -> i3 b "dpps" sel arg1 arg2
  | DPPD (sel, arg1, arg2) -> i3 b "dppd" sel arg1 arg2
  | PEXTRB (n, arg1, arg2) -> i3 b "pextrb" n arg1 arg2
  | PEXTRW (n, arg1, arg2) -> i3 b "pextrw" n arg1 arg2
  | PEXTRD (n, arg1, arg2) -> i3 b "pextrd" n arg1 arg2
  | PEXTRQ (n, arg1, arg2) -> i3 b "pextrq" n arg1 arg2
  | PINSRB (n, arg1, arg2) -> i3 b "pinsrb" n arg1 arg2
  | PINSRW (n, arg1, arg2) -> i3 b "pinsrw" n arg1 arg2
  | PINSRD (n, arg1, arg2) -> i3 b "pinsrd" n arg1 arg2
  | PINSRQ (n, arg1, arg2) -> i3 b "pinsrq" n arg1 arg2
  | PMAXSB (arg1, arg2) -> i2 b "pmaxsb" arg1 arg2
  | PMAXSD (arg1, arg2) -> i2 b "pmaxsd" arg1 arg2
  | PMAXUW (arg1, arg2) -> i2 b "pmaxuw" arg1 arg2
  | PMAXUD (arg1, arg2) -> i2 b "pmaxud" arg1 arg2
  | PMINSB (arg1, arg2) -> i2 b "pminsb" arg1 arg2
  | PMINSD (arg1, arg2) -> i2 b "pminsd" arg1 arg2
  | PMINUW (arg1, arg2) -> i2 b "pminuw" arg1 arg2
  | PMINUD (arg1, arg2) -> i2 b "pminud" arg1 arg2
  | ROUNDPD (rd, arg1, arg2) -> i3 b "roundpd" (imm_of_rounding rd) arg1 arg2
  | ROUNDPS (rd, arg1, arg2) -> i3 b "roundps" (imm_of_rounding rd) arg1 arg2
  | PCMPGTQ (arg1, arg2) -> i2 b "pcmpgtq" arg1 arg2
  | PCMPESTRI (n, arg1, arg2) -> i3 b "pcmpestri" n arg1 arg2
  | PCMPESTRM (n, arg1, arg2) -> i3 b "pcmpestrm" n arg1 arg2
  | PCMPISTRI (n, arg1, arg2) -> i3 b "pcmpistri" n arg1 arg2
  | PCMPISTRM (n, arg1, arg2) -> i3 b "pcmpistrm" n arg1 arg2
  | PAVGB (arg1, arg2) -> i2 b "pavgb" arg1 arg2
  | PAVGW (arg1, arg2) -> i2 b "pavgw" arg1 arg2
  | PSADBW (arg1, arg2) -> i2 b "psadbw" arg1 arg2
  | PACKSSWB (arg1, arg2) -> i2 b "packsswb" arg1 arg2
  | PACKSSDW (arg1, arg2) -> i2 b "packssdw" arg1 arg2
  | PACKUSWB (arg1, arg2) -> i2 b "packuswb" arg1 arg2
  | PACKUSDW (arg1, arg2) -> i2 b "packusdw" arg1 arg2
  | PALIGNR (n, arg1, arg2) -> i3 b "palignr" n arg1 arg2
  | MPSADBW (n, arg1, arg2) -> i3 b "mpsadbw" n arg1 arg2
  | PHMINPOSUW (arg1, arg2) -> i2 b "phminposuw" arg1 arg2
  | PCLMULQDQ (n, arg1, arg2) -> i3 b "pclmulqdq" n arg1 arg2
  | PMULHW (arg1, arg2) -> i2 b "pmulhw" arg1 arg2
  | PMULHUW (arg1, arg2) -> i2 b "pmulhuw" arg1 arg2
  | PMULLW (arg1, arg2) -> i2 b "pmullw" arg1 arg2
  | PMADDWD (arg1, arg2) -> i2 b "pmaddwd" arg1 arg2
  | PMADDUBSW (arg1, arg2) -> i2 b "pmaddubsw" arg1 arg2
  | PMULLD (arg1, arg2) -> i2 b "pmulld" arg1 arg2
  | PEXT (arg1, arg2, arg3) -> i3 b "pext" arg1 arg2 arg3
  | PDEP (arg1, arg2, arg3) -> i3 b "pdep" arg1 arg2 arg3

(* bug:
   https://sourceware.org/binutils/docs-2.22/as/i386_002dBugs.html#i386_002dBugs

   The AT&T syntax has a bug for fsub/fdiv/fsubr/fdivr instructions when
   the source register is %st and the destination is %st(i).  In those
   case, AT&T use fsub (resp. fsubr) in place of fsubr (resp. fsub),
   and idem for fdiv/fdivr.

   Concretely, AT&T syntax interpretation of:

      fsub  %st, %st(3)

   should normally be:

      %st(3) := %st(3) - %st

   but it should actually be interpreted as:

      %st(3) := %st - %st(3)

   which means the FSUBR instruction should be used.
*)


let print_line b = function
  | Ins instr -> print_instr b instr

  | Align (_data,n) ->
      (* MacOSX assembler interprets the integer n as a 2^n alignment *)
      let n = if system = S_macosx then Misc.log2 n else n in
      bprintf b "\t.align\t%d" n
  | Byte n -> bprintf b "\t.byte\t%a" cst n
  | Bytes s ->
      if system = S_solaris then buf_bytes_directive b ".byte" s
      else
        (* Very long lines can cause gas to be extremely slow so split up large
          string literals. It turns out that gas reads files in 32kb chunks
          so splitting the string into blocks of 25k characters should be close
          to the sweet spot even with a lot of escapes. *)
        let chunk_size = 25000 in
        let rec chunk i =
          if String.length s - i > chunk_size
            then (
              bprintf b "\t.ascii\t\"%s\"\n"
                (string_of_substring_literal i chunk_size s);
              chunk (i + chunk_size))
            else i
        in
        let i = chunk 0 in
        bprintf b "\t.ascii\t\"%s\""
          (string_of_substring_literal i (String.length s - i) s)
  | Comment s -> bprintf b "\t\t\t\t/* %s */" s
  | Global { protected; sym } ->
    bprintf b "\t.globl\t%s" sym;
    if protected then bprintf b "\n\t.protected\t%s" sym;
  | Hidden s -> bprintf b "\t.hidden\t%s" s;
  | Weak s -> bprintf b "\t.weak\t%s" s;
  | Long n -> bprintf b "\t.long\t%a" cst n
  | NewLabel (s, _) -> bprintf b "%s:" s
  | NewLine -> ()
  | Quad n -> bprintf b "\t.quad\t%a" cst n
  | Section ([".data" ], _, _, _) -> bprintf b "\t.data"
  | Section ([".text" ], _, _, _) -> bprintf b "\t.text"
  | Section (name, flags, args, _delayed) ->
      bprintf b "\t.section %s" (String.concat "," name);
      begin match flags with
      | None -> ()
      | Some flags -> bprintf b ",%S" flags
      end;
      begin match args with
      | [] -> ()
      | _ -> bprintf b ",%s" (String.concat "," args)
      end
  | Space n ->
      if system = S_solaris then bprintf b "\t.zero\t%d" n
      else bprintf b "\t.space\t%d" n
  | Word n ->
      if system = S_solaris then bprintf b "\t.value\t%a" cst n
      else bprintf b "\t.word\t%a" cst n

  | Uleb128 n -> bprintf b "\t.uleb128\t%a" cst n
  | Sleb128 n -> bprintf b "\t.sleb128\t%a" cst n

  (* gas only *)
  | Cfi_adjust_cfa_offset n -> bprintf b "\t.cfi_adjust_cfa_offset %d" n
  | Cfi_endproc -> bprintf b "\t.cfi_endproc"
  | Cfi_startproc -> bprintf b "\t.cfi_startproc"
  | Cfi_remember_state -> bprintf b "\t.cfi_remember_state"
  | Cfi_restore_state -> bprintf b "\t.cfi_restore_state"
  | Cfi_def_cfa_register reg -> bprintf b "\t.cfi_def_cfa_register %%%s" reg
  | Cfi_def_cfa_offset n -> bprintf b "\t.cfi_def_cfa_offset %d" n
  | File (file_num, file_name) ->
      bprintf b "\t.file\t%d\t\"%s\""
        file_num (X86_proc.string_of_string_literal file_name)
  | Indirect_symbol s -> bprintf b "\t.indirect_symbol %s" s
  | Loc { file_num; line; col; discriminator } ->
      (* PR#7726: Location.none uses column -1, breaks LLVM assembler *)
      (* If we don't set the optional column field, debug_line program
         gets the column value from the previous .loc directive. *)
      if col >= 0 then bprintf b "\t.loc\t%d\t%d\t%d" file_num line col
      else bprintf b "\t.loc\t%d\t%d\t0" file_num line;
      begin match discriminator with
      | None -> ()
      | Some k -> bprintf b "\tdiscriminator %d" k
      end
  | Private_extern s -> bprintf b "\t.private_extern %s" s
  | Set (arg1, arg2) -> bprintf b "\t.set %s, %a" arg1 cst arg2
  | Size (s, c) -> bprintf b "\t.size %s,%a" s cst c
  | Type (s, typ) -> bprintf b "\t.type %s,%s" s typ
  | Reloc {offset; name; expr} ->
      bprintf b "\t.reloc %a,%s,%a"
        cst offset
        (reloc_type_to_string name)
        cst expr

  (* masm only *)
  | External _
  | Mode386
  | Model _
    -> assert false

  (* MacOS only *)
  | Direct_assignment (var, c) ->
    assert (List.mem Config.system [ "macosx"; "darwin" ]);
    bprintf b "\t%s = %a" var cst c

let generate_asm oc lines =
  let b = Buffer.create 10000 in
  output_string oc "\t.file \"\"\n"; (* PR#7037 *)
  List.iter
    (fun i ->
       Buffer.clear b;
       print_line b i;
       Buffer.add_char b '\n';
       Buffer.output_buffer oc b;
    )
    lines
