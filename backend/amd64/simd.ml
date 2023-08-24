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

(* SIMD instructions for AMD64 *)

open Format

type operation_class = Pure

type float_condition = X86_ast.float_condition =
  | EQf
  | LTf
  | LEf
  | UNORDf
  | NEQf
  | NLTf
  | NLEf
  | ORDf

type sse_operation =
  | Cmp_f32 of float_condition
  | Add_f32
  | Sub_f32
  | Mul_f32
  | Div_f32
  | Max_f32
  | Min_f32
  | Rcp_f32
  | Sqrt_f32
  | Rsqrt_f32
  | High_64_to_low_64
  | Low_64_to_high_64
  | Interleave_high_32
  | Interleave_low_32
  | Movemask_32
  | Shuffle_32 of int

type sse2_operation = |

type sse3_operation = |

type ssse3_operation = |

type sse41_operation = |

type sse42_operation = Crc32_64

type operation =
  | SSE of sse_operation
  | SSE2 of sse2_operation
  | SSE3 of sse3_operation
  | SSSE3 of ssse3_operation
  | SSE41 of sse41_operation
  | SSE42 of sse42_operation

let equal_operation_sse l r =
  match l, r with
  | Add_f32, Add_f32
  | Sub_f32, Sub_f32
  | Mul_f32, Mul_f32
  | Div_f32, Div_f32
  | Max_f32, Max_f32
  | Min_f32, Min_f32
  | Rcp_f32, Rcp_f32
  | Sqrt_f32, Sqrt_f32
  | Rsqrt_f32, Rsqrt_f32
  | High_64_to_low_64, High_64_to_low_64
  | Low_64_to_high_64, Low_64_to_high_64
  | Interleave_high_32, Interleave_high_32
  | Interleave_low_32, Interleave_low_32
  | Movemask_32, Movemask_32 ->
    true
  | Cmp_f32 l, Cmp_f32 r when l = r -> true
  | Shuffle_32 l, Shuffle_32 r when l = r -> true
  | ( ( Add_f32 | Sub_f32 | Mul_f32 | Div_f32 | Max_f32 | Min_f32 | Rcp_f32
      | Sqrt_f32 | Rsqrt_f32 | High_64_to_low_64 | Low_64_to_high_64
      | Interleave_high_32 | Interleave_low_32 | Movemask_32 | Cmp_f32 _
      | Shuffle_32 _ ),
      _ ) ->
    false

let equal_operation_sse42 l r = match l, r with Crc32_64, Crc32_64 -> true

let equal_operation l r =
  match l, r with
  | SSE l, SSE r -> equal_operation_sse l r
  | SSE2 _, SSE2 _ -> .
  | SSE3 _, SSE3 _ -> .
  | SSSE3 _, SSSE3 _ -> .
  | SSE41 _, SSE41 _ -> .
  | SSE42 l, SSE42 r -> equal_operation_sse42 l r
  | (SSE _ | SSE2 _ | SSE3 _ | SSSE3 _ | SSE41 _ | SSE42 _), _ -> false

let print_float_condition ppf = function
  | EQf -> pp_print_string ppf "eq"
  | LTf -> pp_print_string ppf "lt"
  | LEf -> pp_print_string ppf "le"
  | UNORDf -> pp_print_string ppf "unord"
  | NEQf -> pp_print_string ppf "neq"
  | NLTf -> pp_print_string ppf "nlt"
  | NLEf -> pp_print_string ppf "nle"
  | ORDf -> pp_print_string ppf "ord"

let print_operation_sse printreg op ppf arg =
  match op with
  | Cmp_f32 i ->
    fprintf ppf "cmp_f32[%a] %a %a" print_float_condition i printreg arg.(0)
      printreg arg.(1)
  | Add_f32 -> fprintf ppf "add_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Sub_f32 -> fprintf ppf "sub_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Mul_f32 -> fprintf ppf "mul_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Div_f32 -> fprintf ppf "div_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Max_f32 -> fprintf ppf "max_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Min_f32 -> fprintf ppf "min_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Rcp_f32 -> fprintf ppf "rcp_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Sqrt_f32 -> fprintf ppf "sqrt_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Rsqrt_f32 -> fprintf ppf "rsqrt_f32 %a %a" printreg arg.(0) printreg arg.(1)
  | Movemask_32 -> fprintf ppf "movemask_32 %a" printreg arg.(0)
  | Shuffle_32 i ->
    fprintf ppf "shuffle_32[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | High_64_to_low_64 ->
    fprintf ppf "high_64_to_low_64 %a %a" printreg arg.(0) printreg arg.(1)
  | Low_64_to_high_64 ->
    fprintf ppf "low_64_to_high_64 %a %a" printreg arg.(0) printreg arg.(1)
  | Interleave_high_32 ->
    fprintf ppf "interleave_high_32 %a %a" printreg arg.(0) printreg arg.(1)
  | Interleave_low_32 ->
    fprintf ppf "interleave_low_32 %a %a" printreg arg.(0) printreg arg.(1)

let print_operation_sse42 printreg op ppf arg =
  match op with
  | Crc32_64 -> fprintf ppf "crc32_64 %a %a" printreg arg.(0) printreg arg.(1)

let print_operation printreg op ppf arg =
  match op with
  | SSE op -> print_operation_sse printreg op ppf arg
  | SSE42 op -> print_operation_sse42 printreg op ppf arg
  | _ -> .

let class_of_operation_sse = function
  | Cmp_f32 _ | Add_f32 | Sub_f32 | Mul_f32 | Div_f32 | Max_f32 | Min_f32
  | Rcp_f32 | Sqrt_f32 | Rsqrt_f32 | High_64_to_low_64 | Low_64_to_high_64
  | Interleave_high_32 | Interleave_low_32 | Movemask_32 | Shuffle_32 _ ->
    Pure

let class_of_operation_sse42 = function Crc32_64 -> Pure

let class_of_operation op =
  match op with
  | SSE op -> class_of_operation_sse op
  | SSE42 op -> class_of_operation_sse42 op
  | _ -> .

let is_pure op = match class_of_operation op with Pure -> true
