(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2000 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* SIMD instructions for AMD64 *)

open Format

type sse_operation =
  | Cmp_ps of int
  | Add_ps
  | Sub_ps
  | Mul_ps
  | Div_ps
  | Max_ps
  | Min_ps
  | Rcp_ps
  | Sqrt_ps
  | Rsqrt_ps
  | Move_high_to_low
  | Move_low_to_high
  | Interleave_high
  | Interleave_low
  | Shuffle of int

type sse2_operation = |

type sse3_operation = |

type ssse3_operation = |

type sse41_operation = |

type sse42_operation = Crc32q

type operation =
  | SSE of sse_operation
  | SSE2 of sse2_operation
  | SSE3 of sse3_operation
  | SSSE3 of ssse3_operation
  | SSE41 of sse41_operation
  | SSE42 of sse42_operation

let equal_operation_sse l r =
  match l, r with
  | Cmp_ps l, Cmp_ps r when l = r -> true
  | Add_ps, Add_ps -> true
  | Sub_ps, Sub_ps -> true
  | Mul_ps, Mul_ps -> true
  | Div_ps, Div_ps -> true
  | Max_ps, Max_ps -> true
  | Min_ps, Min_ps -> true
  | Rcp_ps, Rcp_ps -> true
  | Sqrt_ps, Sqrt_ps -> true
  | Rsqrt_ps, Rsqrt_ps -> true
  | Shuffle l, Shuffle r when l = r -> true
  | Move_high_to_low, Move_high_to_low -> true
  | Move_low_to_high, Move_low_to_high -> true
  | Interleave_high, Interleave_high -> true
  | Interleave_low, Interleave_low -> true
  | _ -> false

let equal_operation_sse42 l r = match l, r with Crc32q, Crc32q -> true

let equal_operation l r =
  match l, r with
  | SSE l, SSE r -> equal_operation_sse l r
  | SSE42 l, SSE42 r -> equal_operation_sse42 l r
  | _ -> false

let print_operation_sse printreg op ppf arg =
  match op with
  | Cmp_ps i ->
    fprintf ppf "cmp_ps[%d] %a %a" i printreg arg.(0) printreg arg.(1)
  | Add_ps -> fprintf ppf "add_ps %a %a" printreg arg.(0) printreg arg.(1)
  | Sub_ps -> fprintf ppf "sub_ps %a %a" printreg arg.(0) printreg arg.(1)
  | Mul_ps -> fprintf ppf "mul_ps %a %a" printreg arg.(0) printreg arg.(1)
  | Div_ps -> fprintf ppf "div_ps %a %a" printreg arg.(0) printreg arg.(1)
  | Max_ps -> fprintf ppf "max_ps %a %a" printreg arg.(0) printreg arg.(1)
  | Min_ps -> fprintf ppf "min_ps %a %a" printreg arg.(0) printreg arg.(1)
  | Rcp_ps -> fprintf ppf "rcp_ps %a %a" printreg arg.(0) printreg arg.(1)
  | Sqrt_ps -> fprintf ppf "sqrt_ps %a %a" printreg arg.(0) printreg arg.(1)
  | Rsqrt_ps -> fprintf ppf "rsqrt_ps %a %a" printreg arg.(0) printreg arg.(1)
  | Shuffle i ->
    fprintf ppf "shuf[%d] %a %a %a" i printreg arg.(0) printreg arg.(1) printreg
      arg.(2)
  | Move_high_to_low ->
    fprintf ppf "move_high_to_low %a %a" printreg arg.(0) printreg arg.(1)
  | Move_low_to_high ->
    fprintf ppf "move_low_to_high %a %a" printreg arg.(0) printreg arg.(1)
  | Interleave_high ->
    fprintf ppf "interleave_high %a %a" printreg arg.(0) printreg arg.(1)
  | Interleave_low ->
    fprintf ppf "interleave_low %a %a" printreg arg.(0) printreg arg.(1)

let print_operation_sse42 printreg op ppf arg =
  match op with
  | Crc32q -> fprintf ppf "crc32 %a %a" printreg arg.(0) printreg arg.(1)

let print_operation printreg op ppf arg =
  match op with
  | SSE op -> print_operation_sse printreg op ppf arg
  | SSE42 op -> print_operation_sse42 printreg op ppf arg
  | _ -> .
