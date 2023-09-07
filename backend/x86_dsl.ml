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
let r10 = Reg64 R10
let r11 = Reg64 R11
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
  let comment s = directive (Comment s)
  let data () = section [ ".data" ] None []
  let direct_assignment var const = directive (Direct_assignment (var, const))
  let extrn s ptr = directive (External (s, ptr))
  let file ~file_num ~file_name = directive (File (file_num, file_name))
  let global s = directive (Global s)
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
  let crc32 x y = emit (CRC32 (x, y))
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
  let test x y= emit (TEST (x, y))
  let ucomisd x y = emit (UCOMISD (x, y))
  let xchg x y = emit (XCHG (x, y))
  let xor x y= emit (XOR (x, y))
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
end
