[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Int_replace_polymorphic_compare[@@ocaml.warning "-66"]
open X86_ast

let equal_data_type left right =
  match left, right with
  | NONE, NONE
  | REAL4, REAL4
  | REAL8, REAL8
  | BYTE, BYTE
  | WORD, WORD
  | DWORD, DWORD
  | QWORD, QWORD
  | VEC128, VEC128
  | NEAR, NEAR
  | PROC, PROC ->
    true
  | (NONE | REAL4 | REAL8 | BYTE | WORD | DWORD | QWORD | VEC128 | NEAR | PROC), _ ->
    false

let equal_reg64 left right =
  match left, right with
  | RAX, RAX
  | RBX, RBX
  | RCX, RCX
  | RDX, RDX
  | RSP, RSP
  | RBP, RBP
  | RSI, RSI
  | RDI, RDI
  | R8, R8
  | R9, R9
  | R10, R10
  | R11, R11
  | R12, R12
  | R13, R13
  | R14, R14
  | R15, R15 ->
    true
  | (RAX | RBX | RCX | RDX | RSP | RBP | RSI | RDI
    | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15), _ -> false

let equal_reg8h left right =
  match left, right with
  | AH, AH
  | BH, BH
  | CH, CH
  | DH, DH ->
    true
  | (AH | BH | CH | DH), _ -> false

let equal_regf left right =
  match left, right with
  | XMM l, XMM r -> Int.equal l r

let equal_arch left right =
  match left, right with
  | X64, X64
  | X86, X86 ->
    true
  | (X64 | X86), _ -> false

let equal_addr left right =
  equal_arch left.arch right.arch
  && equal_data_type left.typ right.typ
  && equal_reg64 left.idx right.idx
  && Int.equal left.scale right.scale
  && Option.equal equal_reg64 left.base right.base
  && Option.equal String.equal left.sym right.sym
  && Int.equal left.displ right.displ

let equal_arg left right =
  match left, right with
  | Imm l, Imm r -> Int64.equal l r
  | Sym l, Sym r -> String.equal l r
  | Reg8L l, Reg8L r -> equal_reg64 l r
  | Reg8H l, Reg8H r -> equal_reg8h l r
  | Reg16 l, Reg16 r -> equal_reg64 l r
  | Reg32 l, Reg32 r -> equal_reg64 l r
  | Reg64 l, Reg64 r -> equal_reg64 l r
  | Regf l, Regf r -> equal_regf l r
  | Mem l, Mem r -> equal_addr l r
  | Mem64_RIP (l_dt, l_s, l_i), Mem64_RIP (r_dt, r_s, r_i) ->
    equal_data_type l_dt r_dt && String.equal l_s r_s && Int.equal l_i r_i
  | (Imm _ | Sym _ | Reg8L _ | Reg8H _ | Reg16 _ | Reg32 _ | Reg64 _ | Regf _ | Mem _ | Mem64_RIP _), _ -> false
