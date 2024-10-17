(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                 Benedikt Meurer, University of Siegen                  *)
(*                                                                        *)
(*   Copyright 2013 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2012 Benedikt Meurer.                                      *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Instruction selection for the ARM processor *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Arch

let is_offset chunk n =
  (n >= -256 && n <= 255) (* 9 bits signed unscaled *)
  || n >= 0
     &&
     match (chunk : Cmm.memory_chunk) with
     (* 12 bits unsigned, scaled by chunk size *)
     | Byte_unsigned | Byte_signed -> n < 0x1000
     | Sixteen_unsigned | Sixteen_signed -> n land 1 = 0 && n lsr 1 < 0x1000
     | Thirtytwo_unsigned | Thirtytwo_signed | Single { reg = Float64 } ->
       n land 3 = 0 && n lsr 2 < 0x1000
     | Word_int | Word_val | Double -> n land 7 = 0 && n lsr 3 < 0x1000
     | Onetwentyeight_aligned | Onetwentyeight_unaligned ->
       (* CR mslater: (SIMD) arm64 *)
       Misc.fatal_error "arm64: got 128 bit memory chunk"
     | Single { reg = Float32 } ->
       (* CR mslater: (float32) arm64 *)
       Misc.fatal_error "arm64: got float32 memory chunk"

let is_logical_immediate_int n = Arch.is_logical_immediate (Nativeint.of_int n)

(* Signed immediates are simpler *)

let is_immediate n =
  let mn = -n in
  n land 0xFFF = n
  || n land 0xFFF_000 = n
  || mn land 0xFFF = mn
  || mn land 0xFFF_000 = mn

(* If you update [inline_ops], you may need to update [is_simple_expr] and/or
   [effects_of], below. *)
let inline_ops = ["sqrt"]

let use_direct_addressing _symb = (not !Clflags.dlcode) && not Arch.macosx

let is_stack_slot rv =
  Reg.(match rv with [| { loc = Stack _ } |] -> true | _ -> false)

let select_bitwidth : Cmm.bswap_bitwidth -> Arch.bswap_bitwidth = function
  | Sixteen -> Sixteen
  | Thirtytwo -> Thirtytwo
  | Sixtyfour -> Sixtyfour
