[@@@ocaml.warning "+a-40-41-42"]

type t =
  | GPR
    (* 64-bit integer registers. The same name is used regardless of the width
       of the value stored in the register. *)
  | SIMD
(* 128/256/512-bit SIMD registers. Different names are used when storing
   different sized values. *)

include Reg_class_utils.T with type t := t

module Tbl : Reg_class_utils.Tbl with type reg_class = t

type save_simd_regs =
  | Save_xmm
  | Save_ymm
  | Save_zmm

val gc_regs_offset : simd:save_simd_regs -> Cmm.machtype_component -> int -> int
