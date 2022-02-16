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

(* Specific operations for the ARM processor, 64-bit mode *)

open Format

let macosx = (Config.system = "macosx")

(* Machine-specific command-line options *)

let command_line_options = []

(* Addressing modes *)

type addressing_mode =
  | Iindexed of int                     (* reg + displ *)
  | Ibased of string * int              (* global var + displ *)

(* We do not support the reg + shifted reg addressing mode, because
   what we really need is reg + shifted reg + displ,
   and this is decomposed in two instructions (reg + shifted reg -> tmp,
   then addressing tmp + displ). *)

(* Specific operations *)

type cmm_label = int
  (* Do not introduce a dependency to Cmm *)

type bswap_bitwidth = Sixteen | Thirtytwo | Sixtyfour

type specific_operation =
  | Ifar_alloc of { bytes : int; dbginfo : Debuginfo.alloc_dbginfo }
  | Ifar_intop_checkbound
  | Ifar_intop_imm_checkbound of { bound : int; }
  | Ishiftarith of arith_operation * int
  | Ishiftcheckbound of { shift : int; }
  | Ifar_shiftcheckbound of { shift : int; }
  | Imuladd       (* multiply and add *)
  | Imulsub       (* multiply and subtract *)
  | Inegmulf      (* floating-point negate and multiply *)
  | Imuladdf      (* floating-point multiply and add *)
  | Inegmuladdf   (* floating-point negate, multiply and add *)
  | Imulsubf      (* floating-point multiply and subtract *)
  | Inegmulsubf   (* floating-point negate, multiply and subtract *)
  | Isqrtf        (* floating-point square root *)
  | Ibswap of { bitwidth: bswap_bitwidth; } (* endianness conversion *)
  | Imove32       (* 32-bit integer move *)

and arith_operation =
    Ishiftadd
  | Ishiftsub

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

let allow_unaligned_access = false

(* Behavior of division *)

let division_crashes_on_overflow = false

(* Operations on addressing modes *)

let identity_addressing = Iindexed 0

let offset_addressing addr delta =
  match addr with
  | Iindexed n -> Iindexed(n + delta)
  | Ibased(s, n) -> Ibased(s, n + delta)

let num_args_addressing = function
  | Iindexed _ -> 1
  | Ibased _ -> 0

(* Printing operations and addressing modes *)

let print_addressing printreg addr ppf arg =
  match addr with
  | Iindexed n ->
      printreg ppf arg.(0);
      if n <> 0 then fprintf ppf " + %i" n
  | Ibased(s, 0) ->
      fprintf ppf "\"%s\"" s
  | Ibased(s, n) ->
      fprintf ppf "\"%s\" + %i" s n

let int_of_bswap_bitwidth = function
  | Sixteen -> 16
  | Thirtytwo -> 32
  | Sixtyfour -> 64

let print_specific_operation printreg op ppf arg =
  match op with
  | Ifar_alloc { bytes; } ->
    fprintf ppf "(far) alloc %i" bytes
  | Ifar_intop_checkbound ->
    fprintf ppf "%a (far) check > %a" printreg arg.(0) printreg arg.(1)
  | Ifar_intop_imm_checkbound { bound; } ->
    fprintf ppf "%a (far) check > %i" printreg arg.(0) bound
  | Ishiftarith(op, shift) ->
      let op_name = function
      | Ishiftadd -> "+"
      | Ishiftsub -> "-" in
      let shift_mark =
       if shift >= 0
       then sprintf "<< %i" shift
       else sprintf ">> %i" (-shift) in
      fprintf ppf "%a %s %a %s"
       printreg arg.(0) (op_name op) printreg arg.(1) shift_mark
  | Ishiftcheckbound { shift; } ->
      fprintf ppf "check %a >> %i > %a" printreg arg.(0) shift
        printreg arg.(1)
  | Ifar_shiftcheckbound { shift; } ->
      fprintf ppf
        "(far) check %a >> %i > %a" printreg arg.(0) shift printreg arg.(1)
  | Imuladd ->
      fprintf ppf "(%a * %a) + %a"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Imulsub ->
      fprintf ppf "-(%a * %a) + %a"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Inegmulf ->
      fprintf ppf "-f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
  | Imuladdf ->
      fprintf ppf "%a +f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Inegmuladdf ->
      fprintf ppf "(-f %a) -f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Imulsubf ->
      fprintf ppf "%a -f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Inegmulsubf ->
      fprintf ppf "(-f %a) +f (%a *f %a)"
        printreg arg.(0)
        printreg arg.(1)
        printreg arg.(2)
  | Isqrtf ->
      fprintf ppf "sqrtf %a"
        printreg arg.(0)
  | Ibswap { bitwidth } ->
      let n = int_of_bswap_bitwidth bitwidth in
      fprintf ppf "bswap%i %a" n
        printreg arg.(0)
  | Imove32 ->
      fprintf ppf "move32 %a"
        printreg arg.(0)

let equal_addressing_mode left right =
  match left, right with
  | Iindexed left_int, Iindexed right_int ->
    Int.equal left_int right_int
  | Ibased (left_string, left_int), Ibased (right_string, right_int) ->
    String.equal left_string right_string
    && Int.equal left_int right_int
  | (Iindexed _ | Ibased _), _ -> false

let equal_arith_operation left right =
  match left, right with
  | Ishiftadd, Ishiftadd -> true
  | Ishiftsub, Ishiftsub -> true
  | (Ishiftadd | Ishiftsub), _ -> false

let equal_specific_operation left right =
  match left, right with
  | Ifar_alloc { bytes = left_bytes; dbginfo = _; },
    Ifar_alloc { bytes = right_bytes; dbginfo = _; } ->
    Int.equal left_bytes right_bytes
  | Ifar_intop_checkbound, Ifar_intop_checkbound -> true
  | Ifar_intop_imm_checkbound { bound = left_bound; },
    Ifar_intop_imm_checkbound { bound = right_bound; } ->
    Int.equal left_bound right_bound
  | Ishiftarith (left_arith_operation, left_int),
    Ishiftarith (right_arith_operation, right_int) ->
    equal_arith_operation left_arith_operation right_arith_operation
    && Int.equal left_int right_int
  | Ishiftcheckbound { shift = left_shift; },
    Ishiftcheckbound { shift = right_shift; } ->
    Int.equal left_shift right_shift
  | Ifar_shiftcheckbound { shift = left_shift; },
    Ifar_shiftcheckbound { shift = right_shift; } ->
    Int.equal left_shift right_shift
  | Imuladd, Imuladd -> true
  | Imulsub, Imulsub -> true
  | Inegmulf, Inegmulf -> true
  | Imuladdf, Imuladdf -> true
  | Inegmuladdf, Inegmuladdf -> true
  | Imulsubf, Imulsubf -> true
  | Inegmulsubf, Inegmulsubf -> true
  | Isqrtf, Isqrtf -> true
  | Ibswap { bitwidth = left }, Ibswap { bitwidth = right } ->
    Int.equal (int_of_bswap_bitwidth left) (int_of_bswap_bitwidth right)
  | Imove32, Imove32 -> true
  | (Ifar_alloc _  | Ifar_intop_checkbound | Ifar_intop_imm_checkbound _
    | Ishiftarith _ | Ishiftcheckbound _ | Ifar_shiftcheckbound _
    | Imuladd | Imulsub | Inegmulf | Imuladdf | Inegmuladdf | Imulsubf
    | Inegmulsubf | Isqrtf | Ibswap _ | Imove32), _ -> false

