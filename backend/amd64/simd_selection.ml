(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Max Slater, Jane Street                           *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Arch
open Simd

(* This will need to be expanded with the addition of three and four argument
   operations in AVX2 and AVX512. *)
type register_behavior =
  | R_to_R
  | RM_to_R
  | R_R_to_fst
  | R_RM_to_fst

let arg i args = List.nth args i

(* Assumes untagged int *)
let extract_constant args low high name =
  match args with
  | Cmm.Cconst_int (i, _) :: args ->
    if i < low || i > high
    then
      Misc.fatal_errorf "Immediate for %s must be in range [%d,%d] (got %d)"
        name low high i;
    i, args
  | _ -> Misc.fatal_errorf "Did not get integer immediate for %s" name

let float_condition_of_int = function
  | 0 -> EQf
  | 1 -> LTf
  | 2 -> LEf
  | 3 -> UNORDf
  | 4 -> NEQf
  | 5 -> NLTf
  | 6 -> NLEf
  | 7 -> ORDf
  | i -> Misc.fatal_errorf "Invalid float condition immediate: %d" i

let select_operation_sse op args =
  match op with
  | "caml_sse_float32x4_cmp" ->
    let i, args = extract_constant args 0 7 "caml_sse_float32x4_cmp" in
    Some (Cmp_f32 (float_condition_of_int i), args)
  | "caml_sse_float32x4_add" -> Some (Add_f32, args)
  | "caml_sse_float32x4_sub" -> Some (Sub_f32, args)
  | "caml_sse_float32x4_mul" -> Some (Mul_f32, args)
  | "caml_sse_float32x4_div" -> Some (Div_f32, args)
  | "caml_sse_float32x4_max" -> Some (Max_f32, args)
  | "caml_sse_float32x4_min" -> Some (Min_f32, args)
  | "caml_sse_float32x4_rcp" -> Some (Rcp_f32, args)
  | "caml_sse_float32x4_rsqrt" -> Some (Rsqrt_f32, args)
  | "caml_sse_float32x4_sqrt" -> Some (Sqrt_f32, args)
  | "caml_sse_vec128_high_64_to_low_64" -> Some (High_64_to_low_64, args)
  | "caml_sse_vec128_low_64_to_high_64" -> Some (Low_64_to_high_64, args)
  | "caml_sse_vec128_interleave_high_32" -> Some (Interleave_high_32, args)
  | "caml_sse_vec128_interleave_low_32" -> Some (Interleave_low_32, args)
  | "caml_sse_vec128_movemask_32" -> Some (Movemask_32, args)
  | "caml_sse_vec128_shuffle_32" ->
    let i, args = extract_constant args 0 0xff "caml_sse_vec128_shuffle_32" in
    Some (Shuffle_32 i, args)
  | _ -> None

let select_operation_sse2 _op _args = None

let select_operation_sse3 op _args =
  if not !Arch.sse3_support then None else match op with _ -> None

let select_operation_ssse3 op _args =
  if not !Arch.ssse3_support then None else match op with _ -> None

let select_operation_sse41 op _args =
  if not !Arch.sse41_support then None else match op with _ -> None

let select_operation_sse42 op args =
  if not !Arch.sse42_support
  then None
  else
    match op with
    | "caml_int64_crc_unboxed" | "caml_int_crc_untagged" -> Some (Crc32_64, args)
    | _ -> None

let select_simd_instr op args =
  let or_else try_ ctr opt =
    match opt with Some x -> Some x | None -> Option.map ctr (try_ op args)
  in
  None
  |> or_else select_operation_sse (fun (op, args) -> SSE op, args)
  |> or_else select_operation_sse2 (fun (op, args) -> SSE2 op, args)
  |> or_else select_operation_sse3 (fun (op, args) -> SSE3 op, args)
  |> or_else select_operation_ssse3 (fun (op, args) -> SSSE3 op, args)
  |> or_else select_operation_sse41 (fun (op, args) -> SSE41 op, args)
  |> or_else select_operation_sse42 (fun (op, args) -> SSE42 op, args)

let select_operation op args =
  select_simd_instr op args
  |> Option.map (fun (op, args) -> Mach.(Ispecific (Isimd op), args))

let register_behavior op =
  match op with
  | SSE (Cmp_f32 _ | Add_f32 | Sub_f32 | Mul_f32 | Div_f32 | Max_f32 | Min_f32)
  | SSE (Interleave_low_32 | Interleave_high_32 | Shuffle_32 _) ->
    R_RM_to_fst
  | SSE (Rcp_f32 | Sqrt_f32 | Rsqrt_f32) -> RM_to_R
  | SSE (High_64_to_low_64 | Low_64_to_high_64) -> R_R_to_fst
  | SSE Movemask_32 -> R_to_R
  | SSE42 Crc32_64 -> R_RM_to_fst
  | _ -> .
