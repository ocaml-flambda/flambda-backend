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

open! Int_replace_polymorphic_compare [@@ocaml.warning "-66"]

(* CSE for the AMD64 *)

open Arch

let of_simd_class (cl : Simd.operation_class) : Cfg_cse_target_intf.op_class =
  match cl with
  | Pure -> Op_pure
  | Load { is_mutable = true } -> Op_load Mutable
  | Load { is_mutable = false } -> Op_load Immutable

let class_of_operation (op : Operation.t)
    : Cfg_cse_target_intf.class_of_operation_result =
  match op with
  | Specific spec ->
    begin match spec with
    | Ilea _ | Isextend32 | Izextend32 -> Class Op_pure
    | Istore_int(_, _, is_asg) -> Class (Op_store is_asg)
    | Ioffset_loc(_, _) -> Class (Op_store true)
    | Ifloatarithmem _ -> Class (Op_load Mutable)
    | Ibswap _ -> Use_default
    | Irdtsc | Irdpmc
    | Ilfence | Isfence | Imfence -> Class Op_other
    | Ipackf32 -> Class Op_pure
    | Isimd op ->
      Class (of_simd_class (Simd.class_of_operation op))
    | Isimd_mem (op,_addr) ->
      Class (of_simd_class (Simd.Mem.class_of_operation op))
    | Ipause
    | Icldemote _
    | Iprefetch _ -> Class Op_other
    end
  | Move | Spill | Reload
  | Floatop _
  | Csel _
  | Reinterpret_cast _ | Static_cast _
  | Const_int _ | Const_float32 _ | Const_float _
  | Const_symbol _ | Const_vec128 _
  | Stackoffset _ | Load _ | Store _ | Alloc _
  | Intop _ | Intop_imm _ | Intop_atomic _
  | Name_for_debugger _ | Probe_is_enabled _ | Opaque
  | Begin_region | End_region | Poll | Dls_get
    -> Use_default

let is_cheap_operation _op
    : Cfg_cse_target_intf.is_cheap_operation_result =
  Use_default
