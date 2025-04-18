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

(* Fixed machine register location *)
type reg =
  | RAX
  | RCX
  | RDX
  | XMM0

(* Flexible register or memory location *)
type temp =
  | R8
  | R16
  | R32
  | R64
  | M8
  | M16
  | M32
  | M64
  | M128
  | MM
  | XMM

(* Possible argument location *)
type loc =
  | Pin of reg
  | Temp of temp array (* All allowed locations *)

(* Possible argument encoding within an instruction *)
type loc_enc =
  | RM_r
  | RM_rm
  | Vex_v
  | Implicit

type arg =
  { loc : loc;
    enc : loc_enc
  }

type res =
  | First_arg (* Result is returned in the first argument operand. *)
  | Res of arg (* Separate operand for result. *)

type legacy_prefix =
  | Prx_none
  | Prx_66
  | Prx_F2
  | Prx_F3

type legacy_rex =
  | Rex_none
  | Rex
  | Rex_w

type legacy_escape =
  | Esc_none
  | Esc_0F
  | Esc_0F38
  | Esc_0F3A

type vex_map =
  | Vexm_0F
  | Vexm_0F38
  | Vexm_0F3A

type prefix =
  | Legacy of
      { prefix : legacy_prefix;
        rex : legacy_rex;
        escape : legacy_escape
      }
  | Vex of
      { vex_m : vex_map;
        vex_w : bool;
        vex_l : bool;
        vex_p : legacy_prefix
      }

type rm_reg =
  | Reg
  | Spec of int

type enc =
  { prefix : prefix;
    rm_reg : rm_reg;
    opcode : int
  }

type 'id instr =
  { id : 'id;
    args : arg array;
    res : res;
    imm : bool;
    mnemonic : string;
    enc : enc
  }

let equal_reg reg0 reg1 =
  match reg0, reg1 with
  | RAX, RAX | RCX, RCX | RDX, RDX | XMM0, XMM0 -> true
  | (RAX | RCX | RDX | XMM0), _ -> false

let equal_temp temp0 temp1 =
  match temp0, temp1 with
  | R8, R8
  | R16, R16
  | R32, R32
  | R64, R64
  | M8, M8
  | M16, M16
  | M32, M32
  | M64, M64
  | M128, M128
  | MM, MM
  | XMM, XMM ->
    true
  | (R8 | R16 | R32 | R64 | M8 | M16 | M32 | M64 | M128 | MM | XMM), _ -> false

let equal_loc loc0 loc1 =
  match loc0, loc1 with
  | Pin reg0, Pin reg1 -> equal_reg reg0 reg1
  | Temp temp0, Temp temp1 -> Array.for_all2 equal_temp temp0 temp1
  | (Pin _ | Temp _), _ -> false

let temp_is_reg = function
  | R8 | R16 | R32 | R64 | MM | XMM -> true
  | M8 | M16 | M32 | M64 | M128 -> false

let loc_allows_reg = function
  | Pin _ -> true
  | Temp temps -> Array.exists temp_is_reg temps

let loc_allows_mem = function
  | Pin _ -> false
  | Temp temps -> Array.exists (fun temp -> not (temp_is_reg temp)) temps

let loc_is_pinned = function Pin reg -> Some reg | Temp _ -> None

let arg_is_implicit ({ enc; _ } : arg) =
  match enc with Implicit -> true | RM_r | RM_rm | Vex_v -> false

type bit_width =
  | Eight
  | Sixteen
  | Thirtytwo
  | Sixtyfour

let loc_requires_width = function
  | Pin _ -> None
  | Temp temps ->
    let width = ref None in
    let set w =
      assert (Option.is_none !width);
      width := Some w
    in
    Array.iter
      (function
        | R8 -> set Eight
        | R16 -> set Sixteen
        | R32 -> set Thirtytwo
        | R64 -> set Sixtyfour
        | M8 | M16 | M32 | M64 | M128 | MM | XMM -> ())
      temps;
    !width
