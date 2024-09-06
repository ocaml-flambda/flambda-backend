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

[@@@ocaml.warning "+a-40-42"]

(* SIMD instruction selection for ARM64 *)

let select_operation _ = None

let pseudoregs_for_operation _ arg res = arg, res

(* See `amd64/simd_selection.ml`. *)

type register =
  | New of int
  | Argument of int
  | Result of int
  | Original of int

type vectorized_instruction =
  { operation : Cfg.operation;
    arguments : register array;
    results : register array
  }

let vector_width_in_bits = 128

let vectorize_operation ~width_in_bits:_ ~arg_count:_ ~res_count:_
    (_ : Cfg.operation list) : vectorized_instruction list option =
  None
