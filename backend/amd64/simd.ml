(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2025 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-42"]

open! Int_replace_polymorphic_compare [@@warning "-66"]
open Format
include Amd64_simd_defs

type instr = Amd64_simd_instrs.instr

module Seq = struct
  type id =
    | Sqrtss
    | Sqrtsd
    | Roundss
    | Roundsd
    | Pcmpestra
    | Pcmpestrc
    | Pcmpestro
    | Pcmpestrs
    | Pcmpestrz
    | Pcmpistra
    | Pcmpistrc
    | Pcmpistro
    | Pcmpistrs
    | Pcmpistrz

  type nonrec instr =
    { id : id;
      instr : instr
    }

  let mnemonic ({ id; _ } : instr) =
    match id with
    | Sqrtss -> "sqrtss"
    | Sqrtsd -> "sqrtsd"
    | Roundss -> "roundss"
    | Roundsd -> "roundsd"
    | Pcmpestra -> "pcmpestra"
    | Pcmpestrc -> "pcmpestrc"
    | Pcmpestro -> "pcmpestro"
    | Pcmpestrs -> "pcmpestrs"
    | Pcmpestrz -> "pcmpestrz"
    | Pcmpistra -> "pcmpistra"
    | Pcmpistrc -> "pcmpistrc"
    | Pcmpistro -> "pcmpistro"
    | Pcmpistrs -> "pcmpistrs"
    | Pcmpistrz -> "pcmpistrz"

  let equal { id = id0; instr = instr0 } { id = id1; instr = instr1 } =
    match id0, id1 with
    | Sqrtss, Sqrtss
    | Sqrtsd, Sqrtsd
    | Roundss, Roundss
    | Roundsd, Roundsd
    | Pcmpestra, Pcmpestra
    | Pcmpestrc, Pcmpestrc
    | Pcmpestro, Pcmpestro
    | Pcmpestrs, Pcmpestrs
    | Pcmpestrz, Pcmpestrz
    | Pcmpistra, Pcmpistra
    | Pcmpistrc, Pcmpistrc
    | Pcmpistro, Pcmpistro
    | Pcmpistrs, Pcmpistrs
    | Pcmpistrz, Pcmpistrz ->
      assert (Stdlib.( == ) instr0 instr1);
      true
    | ( ( Sqrtss | Sqrtsd | Roundss | Roundsd | Pcmpestra | Pcmpestrc
        | Pcmpestro | Pcmpestrs | Pcmpestrz | Pcmpistra | Pcmpistrc | Pcmpistro
        | Pcmpistrs | Pcmpistrz ),
        _ ) ->
      false
end

type operation =
  | Instruction of
      { instr : instr;
        imm : int option
      }
  | Sequence of
      { seq : Seq.instr;
        imm : int option
      }

type operation_class =
  | Pure
  | Load of { is_mutable : bool }

let is_pure_operation _op = true

let class_of_operation _op = Pure

let equal_operation op0 op1 =
  match op0, op1 with
  | ( Instruction { instr = instr0; imm = imm0 },
      Instruction { instr = instr1; imm = imm1 } ) ->
    Stdlib.( == ) instr0 instr1 && Option.equal Int.equal imm0 imm1
  | Sequence { seq = seq0; imm = imm0 }, Sequence { seq = seq1; imm = imm1 } ->
    Seq.equal seq0 seq1 && Option.equal Int.equal imm0 imm1
  | (Instruction _ | Sequence _), _ -> false

let print_operation printreg (op : operation) ppf regs =
  let imm =
    match op with
    | Instruction { instr; imm } ->
      fprintf ppf "%s" instr.mnemonic;
      imm
    | Sequence { seq; imm } ->
      fprintf ppf "[seq] %s" (Seq.mnemonic seq);
      imm
  in
  Option.iter (fun imm -> fprintf ppf " %d" imm) imm;
  Array.iter (fun reg -> fprintf ppf " %a" printreg reg) regs

module Mem = struct
  (** Initial support for some operations with memory arguments.
      Requires 16-byte aligned memory. *)

  type sse_operation =
    | Add_f32
    | Sub_f32
    | Mul_f32
    | Div_f32

  type sse2_operation =
    | Add_f64
    | Sub_f64
    | Mul_f64
    | Div_f64

  type operation =
    | SSE of sse_operation
    | SSE2 of sse2_operation

  let class_of_operation_sse (op : sse_operation) =
    match op with
    | Add_f32 | Sub_f32 | Mul_f32 | Div_f32 -> Load { is_mutable = true }

  let class_of_operation_sse2 (op : sse2_operation) =
    match op with
    | Add_f64 | Sub_f64 | Mul_f64 | Div_f64 -> Load { is_mutable = true }

  let class_of_operation (op : operation) =
    match op with
    | SSE op -> class_of_operation_sse op
    | SSE2 op -> class_of_operation_sse2 op

  let op_name_sse (op : sse_operation) =
    match op with
    | Add_f32 -> "add_f32"
    | Sub_f32 -> "sub_f32"
    | Mul_f32 -> "mul_f32"
    | Div_f32 -> "div_f32"

  let op_name_sse2 (op : sse2_operation) =
    match op with
    | Add_f64 -> "add_f64"
    | Sub_f64 -> "sub_f64"
    | Mul_f64 -> "mul_f64"
    | Div_f64 -> "div_f64"

  let print_operation printreg printaddr (op : operation) ppf arg =
    let addr_args = Array.sub arg 1 (Array.length arg - 1) in
    let op_name =
      match op with SSE op -> op_name_sse op | SSE2 op -> op_name_sse2 op
    in
    fprintf ppf "%s %a [%a]" op_name printreg arg.(0) printaddr addr_args

  let is_pure_operation op =
    match class_of_operation op with Pure -> true | Load _ -> true

  let equal_operation_sse2 (l : sse2_operation) (r : sse2_operation) =
    match l, r with
    | Add_f64, Add_f64 | Sub_f64, Sub_f64 | Mul_f64, Mul_f64 | Div_f64, Div_f64
      ->
      true
    | (Add_f64 | Sub_f64 | Mul_f64 | Div_f64), _ -> false

  let equal_operation_sse (l : sse_operation) (r : sse_operation) =
    match l, r with
    | Add_f32, Add_f32 | Sub_f32, Sub_f32 | Mul_f32, Mul_f32 | Div_f32, Div_f32
      ->
      true
    | (Add_f32 | Sub_f32 | Mul_f32 | Div_f32), _ -> false

  let equal_operation (l : operation) (r : operation) =
    match l, r with
    | SSE l, SSE r -> equal_operation_sse l r
    | SSE2 l, SSE2 r -> equal_operation_sse2 l r
    | (SSE _ | SSE2 _), _ -> false
end
