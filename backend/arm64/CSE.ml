(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2014 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-40-41-42"]

(* CSE for ARM64 *)

open! Int_replace_polymorphic_compare

let of_simd_class (cl : Simd.operation_class) : Cfg_cse_target_intf.op_class =
  match cl with
  | Pure -> Op_pure

let class_of_operation (op : Operation.t)
    : Cfg_cse_target_intf.class_of_operation_result =
  match op with
  | Specific spec ->
    let op_class : Cfg_cse_target_intf.op_class =
      match spec with
      | Ifar_poll
      | Ifar_alloc _
      | Ishiftarith _
      | Imuladd
      | Imulsub
      | Inegmulf
      | Imuladdf
      | Inegmuladdf
      | Imulsubf
      | Inegmulsubf
      | Isqrtf
      | Ibswap _
      | Imove32
      | Isignext _ -> Op_pure
      | Isimd op -> of_simd_class (Simd.class_of_operation op)
    in
    Class op_class
  | Move | Spill | Reload
  | Floatop _
  | Csel _
  | Reinterpret_cast _ | Static_cast _
  | Const_int _ | Const_float32 _ | Const_float _
  | Const_symbol _ | Const_vec128 _
  | Stackoffset _ | Load _ | Store _ | Alloc _
  | Intop _ | Intop_imm _ | Intop_atomic _
  | Name_for_debugger _ | Probe_is_enabled _ | Opaque
  | Begin_region | End_region | Poll | Dls_get | Extcall _
    -> Use_default

let is_cheap_operation (op : Operation.t)
    : Cfg_cse_target_intf.is_cheap_operation_result =
  match op with
  | Const_int n ->
    Cheap (Nativeint.compare n 65535n <= 0 && Nativeint.compare n 0n >= 0)
  | Specific _
  | Move | Spill | Reload
  | Floatop _
  | Csel _
  | Reinterpret_cast _ | Static_cast _
  | Const_float32 _ | Const_float _
  | Const_symbol _ | Const_vec128 _
  | Stackoffset _ | Load _ | Store _ | Alloc _
  | Intop _ | Intop_imm _ | Intop_atomic _
  | Name_for_debugger _ | Probe_is_enabled _ | Opaque
  | Begin_region | End_region | Poll | Dls_get | Extcall _
    -> Cheap false
