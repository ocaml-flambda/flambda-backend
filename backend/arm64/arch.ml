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
  | Ifar_poll of { return_label: cmm_label option }
  | Ifar_alloc of { bytes : int; dbginfo : Debuginfo.alloc_dbginfo }
  | Ishiftarith of arith_operation * int
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
  | Isignext of int (* sign extension *)

and arith_operation =
    Ishiftadd
  | Ishiftsub

(* Sizes, endianness *)

let big_endian = false

let size_addr = 8
let size_int = 8
let size_float = 8

let size_vec128 = 16

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
  | Ifar_poll _ ->
    fprintf ppf "(far) poll"
  | Ifar_alloc { bytes; } ->
    fprintf ppf "(far) alloc %i" bytes
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
  | Isignext n ->
      fprintf ppf "signext%d %a"
        n printreg arg.(0)

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
  | Ishiftarith (left_arith_operation, left_int),
    Ishiftarith (right_arith_operation, right_int) ->
    equal_arith_operation left_arith_operation right_arith_operation
    && Int.equal left_int right_int
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
  | Isignext left, Isignext right -> Int.equal left right
  | (Ifar_alloc _  | Ifar_poll _  | Ishiftarith _
    | Imuladd | Imulsub | Inegmulf | Imuladdf | Inegmuladdf | Imulsubf
    | Inegmulsubf | Isqrtf | Ibswap _ | Imove32 | Isignext _), _ -> false

(* Recognition of logical immediate arguments *)

(* An automaton to recognize ( 0+1+0* | 1+0+1* )

               0          1          0
              / \        / \        / \
              \ /        \ /        \ /
        -0--> [1] --1--> [2] --0--> [3]
       /
     [0]
       \
        -1--> [4] --0--> [5] --1--> [6]
              / \        / \        / \
              \ /        \ /        \ /
               1          0          1

The accepting states are 2, 3, 5 and 6. *)

let auto_table = [|   (* accepting?, next on 0, next on 1 *)
  (* state 0 *) (false, 1, 4);
  (* state 1 *) (false, 1, 2);
  (* state 2 *) (true,  3, 2);
  (* state 3 *) (true,  3, 7);
  (* state 4 *) (false, 5, 4);
  (* state 5 *) (true,  5, 6);
  (* state 6 *) (true,  7, 6);
  (* state 7 *) (false, 7, 7)   (* error state *)
|]

let rec run_automata nbits state input =
  let (acc, next0, next1) = auto_table.(state) in
  if nbits <= 0
  then acc
  else run_automata (nbits - 1)
                    (if Nativeint.logand input 1n = 0n then next0 else next1)
                    (Nativeint.shift_right_logical input 1)

(* The following function determines a length [e]
   such that [x] is a repetition [BB...B] of a bit pattern [B] of length [e].
   [e] ranges over 64, 32, 16, 8, 4, 2.  The smaller [e] the better. *)

let logical_imm_length x =
  (* [test n] checks that the low [2n] bits of [x] are of the
     form [BB], that is, two occurrences of the same [n] bits *)
  let test n =
    let mask = Nativeint.(sub (shift_left 1n n) 1n) in
    let low_n_bits = Nativeint.(logand x mask) in
    let next_n_bits = Nativeint.(logand (shift_right_logical x n) mask) in
    low_n_bits = next_n_bits in
  (* If [test n] fails, we know that the length [e] is
     at least [2n].  Hence we test with decreasing values of [n]:
     32, 16, 8, 4, 2. *)
  if not (test 32) then 64
  else if not (test 16) then 32
  else if not (test 8) then 16
  else if not (test 4) then 8
  else if not (test 2) then 4
  else 2

(* A valid logical immediate is
- neither [0] nor [-1];
- composed of a repetition [BBBBB] of a bit-pattern [B] of length [e]
- the low [e] bits of the number, that is, [B], match [0+1+0*] or [1+0+1*].
*)

let is_logical_immediate x =
  x <> 0n && x <> -1n && run_automata (logical_imm_length x) 0 x

(* Specific operations that are pure *)

let operation_is_pure : specific_operation -> bool = function
  | Ifar_alloc _ | Ifar_poll _ -> false
  | Ishiftarith _ -> true
  | Imuladd -> true
  | Imulsub -> true
  | Inegmulf -> true
  | Imuladdf -> true
  | Inegmuladdf -> true
  | Imulsubf -> true
  | Inegmulsubf -> true
  | Isqrtf -> true
  | Ibswap _ -> true
  | Imove32 -> true
  | Isignext _ -> true

(* Specific operations that can raise *)

let operation_can_raise = function
  | Ifar_alloc _
  | Ifar_poll _ -> true
  | Imuladd
  | Imulsub
  | Inegmulf
  | Imuladdf
  | Inegmuladdf
  | Imulsubf
  | Inegmulsubf
  | Isqrtf
  | Imove32
  | Ishiftarith (_, _)
  | Isignext _
  | Ibswap _ -> false

let operation_allocates = function
  | Ifar_alloc _ -> true
  | Ifar_poll _
  | Imuladd
  | Imulsub
  | Inegmulf
  | Imuladdf
  | Inegmuladdf
  | Imulsubf
  | Inegmulsubf
  | Isqrtf
  | Imove32
  | Ishiftarith (_, _)
  | Isignext _
  | Ibswap _ -> false

(* addressing mode functions, for avoiding compilation errors when using them in amd64 *)

let compare_addressing_mode_without_displ (addressing_mode_1: addressing_mode) (addressing_mode_2 : addressing_mode) =
  match addressing_mode_1, addressing_mode_2 with
  | Iindexed _, Iindexed _ -> 0
  | Iindexed _ , _ -> -1
  | _, Iindexed _ -> 1
  | Ibased (var1, _), Ibased (var2, _) -> String.compare var1 var2

let compare_addressing_mode_displ (addressing_mode_1: addressing_mode) (addressing_mode_2 : addressing_mode) =
  match addressing_mode_1, addressing_mode_2 with
  | Iindexed n1, Iindexed n2 -> Some (Int.compare n1 n2)
  | Ibased (var1, n1), Ibased (var2, n2) ->
    if String.compare var1 var2 = 0 then Some (Int.compare n1 n2) else None
  | Iindexed _ , _ -> None
  | Ibased _ , _ -> None

let addressing_offset (addressing_mode_1: addressing_mode) (addressing_mode_2 : addressing_mode) = None

let can_cross_loads_or_stores (specific_operation : specific_operation) =
  match specific_operation with
  | Ifar_poll _ | Ifar_alloc _ | Ishiftarith _ | Imuladd | Imulsub | Inegmulf | Imuladdf
  | Inegmuladdf | Imulsubf | Inegmulsubf | Isqrtf | Ibswap _ | Imove32 | Isignext _ ->
    true

let preserves_alloc_freshness (op : specific_operation) =
  match op with
  | Ifar_poll _ | Ifar_alloc _ | Ishiftarith _ | Imuladd | Imulsub | Inegmulf | Imuladdf
  | Inegmuladdf | Imulsubf | Inegmulsubf | Isqrtf | Ibswap _ | Imove32 | Isignext _ ->
    true
