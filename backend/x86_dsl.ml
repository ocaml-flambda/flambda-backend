(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*         Fabrice Le Fessant, projet Gallium, INRIA Rocquencourt         *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Helpers for Intel code generators *)

(* The DSL* modules expose functions to emit x86/x86_64 instructions
   using a syntax close to AT&T (in particular, arguments are reversed compared
   to the official Intel syntax).

   Some notes:

     - Unary floating point instructions such as fadd/fmul/fstp/fld/etc.
       come with a single version supporting both the single and double
       precision instructions.  (As with Intel syntax.)

     - A legacy bug in GAS:
   https://sourceware.org/binutils/docs-2.22/as/i386_002dBugs.html#i386_002dBugs
       is not replicated here.  It is managed by X86_gas.
*)


open X86_ast
open X86_proc

let sym s = Sym s

let nat n = Imm (Int64.of_nativeint n)
let int n = Imm (Int64.of_int n)

let const_32 n = Const (Int64.of_int32 n)
let const_nat n = Const (Int64.of_nativeint n)
let const n = Const (Int64.of_int n)

let al  = Reg8L RAX
let ah  = Reg8H AH
let cl  = Reg8L RCX
let ax  = Reg16 RAX
let rax = Reg64 RAX
let rbx = Reg64 RBX
let rdx = Reg64 RDX
let r10 = Reg64 R10
let r11 = Reg64 R11
let r12 = Reg64 R12
let r13 = Reg64 R13
let r14 = Reg64 R14
let r15 = Reg64 R15
let rsp = Reg64 RSP
let rbp = Reg64 RBP
let xmm15 = Regf (XMM 15)
let eax = Reg32 RAX
let ebx = Reg32 RBX
let ecx = Reg32 RCX
let edx = Reg32 RDX
let ebp = Reg32 RBP
let esp = Reg32 RSP

let mem32 typ ?(scale = 1) ?base ?sym displ idx =
  assert(scale >= 0);
  Mem {arch = X86; typ; idx; scale; base; sym; displ}

let mem64 typ ?(scale = 1) ?base ?sym displ idx =
  assert(scale > 0);
  Mem {arch = X64; typ; idx; scale; base; sym; displ}

let mem64_rip typ ?(ofs = 0) s =
  Mem64_RIP (typ, s, ofs)

module D = struct
  let section ?(delayed=false) segment flags args = directive (Section (segment, flags, args, delayed))
  let align ~data n = directive (Align (data, n))
  let byte n = directive (Byte n)
  let bytes s = directive (Bytes s)
  let cfi_adjust_cfa_offset n = directive (Cfi_adjust_cfa_offset n)
  let cfi_endproc () = directive Cfi_endproc
  let cfi_startproc () = directive Cfi_startproc
  let cfi_remember_state () = directive Cfi_remember_state
  let cfi_restore_state () = directive Cfi_restore_state
  let cfi_def_cfa_register reg = directive (Cfi_def_cfa_register reg)
  let cfi_def_cfa_offset n = directive (Cfi_def_cfa_offset n)
  let comment s = directive (Comment s)
  let data () = section [ ".data" ] None []
  let direct_assignment var const = directive (Direct_assignment (var, const))
  let extrn s ptr = directive (External (s, ptr))
  let file ~file_num ~file_name = directive (File (file_num, file_name))
  let global s = directive (Global s)
  let protected s = directive (Protected s)

  let hidden s = directive (Hidden s)
  let weak s = directive (Weak s)
  let indirect_symbol s = directive (Indirect_symbol s)
  let label ?(typ = NONE) s = directive (NewLabel (s, typ))
  let loc ~file_num ~line ~col ?discriminator () =
    directive (Loc { file_num; line; col; discriminator })
  let long cst = directive (Long cst)
  let mode386 () = directive Mode386
  let model name = directive (Model name)
  let new_line () = directive NewLine
  let private_extern s = directive (Private_extern s)
  let qword cst = directive (Quad cst)
  let reloc ~offset ~name ~expr = directive (Reloc { offset; name; expr })
  let setvar (x, y) = directive (Set (x, y))
  let size name cst = directive (Size (name, cst))
  let sleb128 n = directive (Sleb128 n)
  let space n = directive (Space n)
  let text () = section [ ".text" ] None []
  let type_ name typ = directive (Type (name, typ))
  let uleb128 n = directive (Uleb128 n)
  let word cst = directive (Word cst)
end

module I = struct
  let add x y = emit (ADD (x, y))
  let addsd x y = emit (ADDSD (x, y))
  let and_ x y= emit (AND (x, y))
  let andpd x y = emit (ANDPD (x, y))
  let bsf x y = emit (BSF (x, y))
  let bsr x y = emit (BSR (x, y))
  let bswap x = emit (BSWAP x)
  let call x = emit (CALL x)
  let cdq () = emit CDQ
  let cmov cond x y = emit (CMOV (cond, x, y))
  let cmp x y = emit (CMP (x, y))
  let cmpsd cond x y = emit (CMPSD (cond, x, y))
  let comisd x y = emit (COMISD (x, y))
  let cqo () = emit CQO
  let cvtsd2si x y = emit (CVTSD2SI (x, y))
  let cvtsd2ss x y = emit (CVTSD2SS (x, y))
  let cvtsi2sd x y = emit (CVTSI2SD (x, y))
  let cvtss2sd x y = emit (CVTSS2SD (x, y))
  let cvttsd2si x y = emit (CVTTSD2SI (x, y))
  let dec x = emit (DEC x)
  let divsd x y = emit (DIVSD (x, y))
  let hlt () = emit HLT
  let idiv x = emit (IDIV x)
  let imul x y = emit (IMUL (x, y))
  let mul x = emit (MUL x)
  let inc x = emit (INC x)
  let j cond x = emit (J (cond, x))
  let ja = j A
  let jae = j AE
  let jb = j B
  let jbe = j BE
  let je = j E
  let jg = j G
  let jmp x = emit (JMP x)
  let jne = j NE
  let jp = j P
  let lea x y = emit (LEA (x, y))
  let lock_cmpxchg x y = emit (LOCK_CMPXCHG (x, y))
  let lock_xadd x y = emit (LOCK_XADD (x, y))
  let maxsd x y = emit (MAXSD (x,y))
  let minsd x y = emit (MINSD (x,y))
  let mov x y = emit (MOV (x, y))
  let movapd x y = emit (MOVAPD (x, y))
  let movupd x y = emit (MOVUPD (x, y))
  let movd x y = emit (MOVD (x, y))
  let movq x y = emit (MOVQ (x, y))
  let movsd x y = emit (MOVSD (x, y))
  let movss x y = emit (MOVSS (x, y))
  let movsx x y = emit (MOVSX (x, y))
  let movsxd x y = emit (MOVSXD  (x, y))
  let movzx x y = emit (MOVZX (x, y))
  let mulsd x y = emit (MULSD (x, y))
  let neg x = emit (NEG x)
  let nop () = emit NOP
  let or_ x y = emit (OR (x, y))
  let pause () = emit (PAUSE)
  let pop x = emit (POP x)
  let popcnt x y = emit (POPCNT (x, y))
  let prefetch is_write locality x = emit (PREFETCH (is_write, locality, x))
  let push x = emit (PUSH x)
  let rdtsc () = emit (RDTSC)
  let rdpmc () = emit (RDPMC)
  let lfence () = emit LFENCE
  let sfence () = emit SFENCE
  let mfence () = emit MFENCE
  let ret () = emit RET
  let roundsd r x y = emit (ROUNDSD (r, x, y))
  let sal x y = emit (SAL (x, y))
  let sar x y = emit (SAR (x, y))
  let set cond x = emit (SET (cond, x))
  let shr x y = emit (SHR (x, y))
  let sqrtsd x y = emit (SQRTSD (x, y))
  let sub x y = emit (SUB (x, y))
  let subsd  x y = emit (SUBSD (x, y))
  let test x y = emit (TEST (x, y))
  let ucomisd x y = emit (UCOMISD (x, y))
  let xchg x y = emit (XCHG (x, y))
  let xor x y = emit (XOR (x, y))
  let xorpd x y = emit (XORPD (x, y))

  let cmpps i x y = emit (CMPPS (i, x, y))
  let shufps i x y = emit (SHUFPS (i, x, y))
  let addps x y = emit (ADDPS (x, y))
  let subps x y = emit (SUBPS (x, y))
  let mulps x y = emit (MULPS (x, y))
  let divps x y = emit (DIVPS (x, y))
  let maxps x y = emit (MAXPS (x, y))
  let minps x y = emit (MINPS (x, y))
  let rcpps x y = emit (RCPPS (x, y))
  let sqrtps x y = emit (SQRTPS (x, y))
  let rsqrtps x y = emit (RSQRTPS (x, y))
  let movhlps x y = emit (MOVHLPS (x, y))
  let movlhps x y = emit (MOVLHPS (x, y))
  let unpckhps x y = emit (UNPCKHPS (x, y))
  let unpcklps x y = emit (UNPCKLPS (x, y))
  let movmskps x y = emit (MOVMSKPS (x, y))

  let paddb x y = emit (PADDB (x, y))
  let paddw x y = emit (PADDW (x, y))
  let paddd x y = emit (PADDD (x, y))
  let paddq x y = emit (PADDQ (x, y))
  let addpd x y = emit (ADDPD (x, y))
  let paddsb x y = emit (PADDSB (x, y))
  let paddsw x y = emit (PADDSW (x, y))
  let paddusb x y = emit (PADDUSB (x, y))
  let paddusw x y = emit (PADDUSW (x, y))
  let psubb x y = emit (PSUBB (x, y))
  let psubw x y = emit (PSUBW (x, y))
  let psubd x y = emit (PSUBD (x, y))
  let psubq x y = emit (PSUBQ (x, y))
  let subpd x y = emit (SUBPD (x, y))
  let psubsb x y = emit (PSUBSB (x, y))
  let psubsw x y = emit (PSUBSW (x, y))
  let psubusb x y = emit (PSUBUSB (x, y))
  let psubusw x y = emit (PSUBUSW (x, y))
  let pmaxub x y = emit (PMAXUB (x, y))
  let pmaxsw x y = emit (PMAXSW (x, y))
  let maxpd x y = emit (MAXPD (x, y))
  let pminub x y = emit (PMINUB (x, y))
  let pminsw x y = emit (PMINSW (x, y))
  let minpd x y = emit (MINPD (x, y))
  let mulpd x y = emit (MULPD (x, y))
  let divpd x y = emit (DIVPD (x, y))
  let sqrtpd x y = emit (SQRTPD (x, y))
  let pand x y = emit (PAND (x, y))
  let pandnot x y = emit (PANDNOT (x, y))
  let por x y = emit (POR (x, y))
  let pxor x y = emit (PXOR (x, y))
  let pmovmskb x y = emit (PMOVMSKB (x, y))
  let movmskpd x y = emit (MOVMSKPD (x, y))
  let pslldq i x = emit (PSLLDQ (i, x))
  let psrldq i x = emit (PSRLDQ (i, x))
  let pcmpeqb x y = emit (PCMPEQB (x, y))
  let pcmpeqw x y = emit (PCMPEQW (x, y))
  let pcmpeqd x y = emit (PCMPEQD (x, y))
  let pcmpgtb x y = emit (PCMPGTB (x, y))
  let pcmpgtw x y = emit (PCMPGTW (x, y))
  let pcmpgtd x y = emit (PCMPGTD (x, y))
  let cmppd i x y = emit (CMPPD (i, x, y))
  let cvtdq2pd x y = emit (CVTDQ2PD (x, y))
  let cvtdq2ps x y = emit (CVTDQ2PS (x, y))
  let cvtpd2dq x y = emit (CVTPD2DQ (x, y))
  let cvtpd2ps x y = emit (CVTPD2PS (x, y))
  let cvtps2dq x y = emit (CVTPS2DQ (x, y))
  let cvtps2pd x y = emit (CVTPS2PD (x, y))
  let psllw x y = emit (PSLLW (x, y))
  let pslld x y = emit (PSLLD (x, y))
  let psllq x y = emit (PSLLQ (x, y))
  let psrlw x y = emit (PSRLW (x, y))
  let psrld x y = emit (PSRLD (x, y))
  let psrlq x y = emit (PSRLQ (x, y))
  let psraw x y = emit (PSRAW (x, y))
  let psrad x y = emit (PSRAD (x, y))
  let psllwi i x = emit (PSLLWI (i, x))
  let pslldi i x = emit (PSLLDI (i, x))
  let psllqi i x = emit (PSLLQI (i, x))
  let psrlwi i x = emit (PSRLWI (i, x))
  let psrldi i x = emit (PSRLDI (i, x))
  let psrlqi i x = emit (PSRLQI (i, x))
  let psrawi i x = emit (PSRAWI (i, x))
  let psradi i x = emit (PSRADI (i, x))
  let shufpd i x y = emit (SHUFPD (i, x, y))
  let pshufhw i x y = emit (PSHUFHW (i, x, y))
  let pshuflw i x y = emit (PSHUFLW (i, x, y))
  let punpckhbw x y = emit (PUNPCKHBW (x, y))
  let punpckhwd x y = emit (PUNPCKHWD (x, y))
  let punpckhqdq x y = emit (PUNPCKHQDQ (x, y))
  let punpcklbw x y = emit (PUNPCKLBW (x, y))
  let punpcklwd x y = emit (PUNPCKLWD (x, y))
  let punpcklqdq x y = emit (PUNPCKLQDQ (x, y))
  let pmulhw x y = emit (PMULHW (x, y))
  let pmulhuw x y = emit (PMULHUW (x, y))
  let pmullw x y = emit (PMULLW (x, y))
  let pmaddwd x y = emit (PMADDWD (x, y))

  let addsubps x y = emit (ADDSUBPS (x, y))
  let addsubpd x y = emit (ADDSUBPD (x, y))
  let haddps x y = emit (HADDPS (x, y))
  let haddpd x y = emit (HADDPD (x, y))
  let hsubps x y = emit (HSUBPS (x, y))
  let hsubpd x y = emit (HSUBPD (x, y))
  let movddup x y = emit (MOVDDUP (x, y))
  let movshdup x y = emit (MOVSHDUP (x, y))
  let movsldup x y = emit (MOVSLDUP (x, y))
  let pavgb x y = emit (PAVGB (x, y))
  let pavgw x y = emit (PAVGW (x, y))
  let psadbw x y = emit (PSADBW (x, y))
  let packsswb x y = emit (PACKSSWB (x, y))
  let packssdw x y = emit (PACKSSDW (x, y))
  let packuswb x y = emit (PACKUSWB (x, y))
  let packusdw x y = emit (PACKUSDW (x, y))

  let pabsb x y = emit (PABSB (x, y))
  let pabsw x y = emit (PABSW (x, y))
  let pabsd x y = emit (PABSD (x, y))
  let phaddw x y = emit (PHADDW (x, y))
  let phaddd x y = emit (PHADDD (x, y))
  let phaddsw x y = emit (PHADDSW (x, y))
  let phsubw x y = emit (PHSUBW (x, y))
  let phsubd x y = emit (PHSUBD (x, y))
  let phsubsw x y = emit (PHSUBSW (x, y))
  let psignb x y = emit (PSIGNB (x, y))
  let psignw x y = emit (PSIGNW (x, y))
  let psignd x y = emit (PSIGND (x, y))
  let pshufb x y = emit (PSHUFB (x, y))
  let palignr i x y = emit (PALIGNR (i, x, y))
  let pmaddubsw x y = emit (PMADDUBSW (x, y))

  let pblendw i x y = emit (PBLENDW (i, x, y))
  let blendps i x y = emit (BLENDPS (i, x, y))
  let blendpd i x y = emit (BLENDPD (i, x, y))
  let pblendvb x y = emit (PBLENDVB (x, y))
  let blendvps x y = emit (BLENDVPS (x, y))
  let blendvpd x y = emit (BLENDVPD (x, y))
  let pcmpeqq x y = emit (PCMPEQQ (x, y))
  let pmovsxbw x y = emit (PMOVSXBW (x, y))
  let pmovsxbd x y = emit (PMOVSXBD (x, y))
  let pmovsxbq x y = emit (PMOVSXBQ (x, y))
  let pmovsxwd x y = emit (PMOVSXWD (x, y))
  let pmovsxwq x y = emit (PMOVSXWQ (x, y))
  let pmovsxdq x y = emit (PMOVSXDQ (x, y))
  let pmovzxbw x y = emit (PMOVZXBW (x, y))
  let pmovzxbd x y = emit (PMOVZXBD (x, y))
  let pmovzxbq x y = emit (PMOVZXBQ (x, y))
  let pmovzxwd x y = emit (PMOVZXWD (x, y))
  let pmovzxwq x y = emit (PMOVZXWQ (x, y))
  let pmovzxdq x y = emit (PMOVZXDQ (x, y))
  let dpps i x y = emit (DPPS (i, x, y))
  let dppd i x y = emit (DPPD (i, x, y))
  let pextrb i x y = emit (PEXTRB (i, x, y))
  let pextrw i x y = emit (PEXTRW (i, x, y))
  let pextrd i x y = emit (PEXTRD (i, x, y))
  let pextrq i x y = emit (PEXTRQ (i, x, y))
  let pinsrb i x y = emit (PINSRB (i, x, y))
  let pinsrw i x y = emit (PINSRW (i, x, y))
  let pinsrd i x y = emit (PINSRD (i, x, y))
  let pinsrq i x y = emit (PINSRQ (i, x, y))
  let pmaxsb x y = emit (PMAXSB (x, y))
  let pmaxsd x y = emit (PMAXSD (x, y))
  let pmaxuw x y = emit (PMAXUW (x, y))
  let pmaxud x y = emit (PMAXUD (x, y))
  let pminsb x y = emit (PMINSB (x, y))
  let pminsd x y = emit (PMINSD (x, y))
  let pminuw x y = emit (PMINUW (x, y))
  let pminud x y = emit (PMINUD (x, y))
  let roundpd i x y = emit (ROUNDPD (i, x, y))
  let roundps i x y = emit (ROUNDPS (i, x, y))
  let mpsadbw i x y = emit (MPSADBW (i, x, y))
  let phminposuw x y = emit (PHMINPOSUW (x, y))
  let pmulld x y = emit (PMULLD (x, y))

  let pcmpgtq x y = emit (PCMPGTQ (x, y))
  let pcmpestri i x y = emit (PCMPESTRI (i, x, y))
  let pcmpestrm i x y = emit (PCMPESTRM (i, x, y))
  let pcmpistri i x y = emit (PCMPISTRI (i, x, y))
  let pcmpistrm i x y = emit (PCMPISTRM (i, x, y))
  let crc32 x y = emit (CRC32 (x, y))

  let pclmulqdq i x y = emit (PCLMULQDQ (i, x, y))

  let lzcnt x y = emit (LZCNT (x, y))
  let tzcnt x y = emit (TZCNT (x, y))

  let pext x y z = emit (PEXT (x, y, z))
  let pdep x y z = emit (PDEP (x, y, z))
end
