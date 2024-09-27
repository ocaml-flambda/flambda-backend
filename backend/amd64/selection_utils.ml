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

(* Instruction selection for the AMD64 *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Arch
open Proc

(* Auxiliary for recognizing addressing modes *)

type addressing_expr =
  | Asymbol of Cmm.symbol
  | Alinear of Cmm.expression
  | Aadd of Cmm.expression * Cmm.expression
  | Ascale of Cmm.expression * int
  | Ascaledadd of Cmm.expression * Cmm.expression * int

let rec select_addr exp =
  let default = Alinear exp, 0 in
  match exp with
  | Cmm.Cconst_symbol (s, _) when not !Clflags.dlcode -> Asymbol s, 0
  | Cmm.Cop ((Caddi | Caddv | Cadda), [arg; Cconst_int (m, _)], _)
  | Cmm.Cop ((Caddi | Caddv | Cadda), [Cconst_int (m, _); arg], _) ->
    let a, n = select_addr arg in
    if Misc.no_overflow_add n m then a, n + m else default
  | Cmm.Cop (Csubi, [arg; Cconst_int (m, _)], _) ->
    let a, n = select_addr arg in
    if Misc.no_overflow_sub n m then a, n - m else default
  | Cmm.Cop (Clsl, [arg; Cconst_int (((1 | 2 | 3) as shift), _)], _) -> (
    let default = Ascale (arg, 1 lsl shift), 0 in
    match select_addr arg with
    | Alinear e, n ->
      if Misc.no_overflow_lsl n shift
      then Ascale (e, 1 lsl shift), n lsl shift
      else default
    | (Asymbol _ | Aadd (_, _) | Ascale (_, _) | Ascaledadd (_, _, _)), _ ->
      default)
  | Cmm.Cop (Cmuli, [arg; Cconst_int (((2 | 4 | 8) as mult), _)], _)
  | Cmm.Cop (Cmuli, [Cconst_int (((2 | 4 | 8) as mult), _); arg], _) -> (
    let default = Ascale (arg, mult), 0 in
    match select_addr arg with
    | Alinear e, n ->
      if Misc.no_overflow_mul n mult
      then Ascale (e, mult), n * mult
      else default
    | (Asymbol _ | Aadd (_, _) | Ascale (_, _) | Ascaledadd (_, _, _)), _ ->
      default)
  | Cmm.Cop ((Caddi | Caddv | Cadda), [arg1; arg2], _) -> (
    match select_addr arg1, select_addr arg2 with
    | (Alinear e1, n1), (Alinear e2, n2) when Misc.no_overflow_add n1 n2 ->
      Aadd (e1, e2), n1 + n2
    | (Alinear e1, n1), (Ascale (e2, scale), n2)
    | (Ascale (e2, scale), n2), (Alinear e1, n1)
      when Misc.no_overflow_add n1 n2 ->
      Ascaledadd (e1, e2, scale), n1 + n2
    | _, (Ascale (e2, scale), n2) -> Ascaledadd (arg1, e2, scale), n2
    | (Ascale (e1, scale), n1), _ -> Ascaledadd (arg2, e1, scale), n1
    | ( (Alinear _, _),
        ((Alinear _ | Asymbol _ | Aadd (_, _) | Ascaledadd (_, _, _)), _) )
    | ( ((Asymbol _ | Aadd (_, _) | Ascaledadd (_, _, _)), _),
        ((Asymbol _ | Alinear _ | Aadd (_, _) | Ascaledadd (_, _, _)), _) ) ->
      Aadd (arg1, arg2), 0)
  | _ -> default

(* Special constraints on operand and result registers *)

exception Use_default

let rax = phys_reg Int 0

let rcx = phys_reg Int 5

let rdx = phys_reg Int 4

let _xmm0v () = phys_reg Vec128 100

let select_locality (l : Cmm.prefetch_temporal_locality_hint) :
    Arch.prefetch_temporal_locality_hint =
  match l with
  | Nonlocal -> Nonlocal
  | Low -> Low
  | Moderate -> Moderate
  | High -> High

let select_bitwidth : Cmm.bswap_bitwidth -> Arch.bswap_bitwidth = function
  | Sixteen -> Sixteen
  | Thirtytwo -> Thirtytwo
  | Sixtyfour -> Sixtyfour

let one_arg name args =
  match args with
  | [arg] -> arg
  | _ -> Misc.fatal_errorf "Selection: expected exactly 1 argument for %s" name

(* If you update [inline_ops], you may need to update [is_simple_expr] and/or
   [effects_of], below. *)
let inline_ops = ["sqrt"]

let is_immediate n = n <= 0x7FFF_FFFF && n >= -0x8000_0000

let is_immediate_natint n = n <= 0x7FFF_FFFFn && n >= -0x8000_0000n
