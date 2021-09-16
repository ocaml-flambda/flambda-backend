# 2 "backend/amd64/CSE.ml"
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
[@@@ocaml.warning "+4"]

(* CSE for the AMD64 *)

open Arch
open Mach
open CSEgen

class cse = object

inherit cse_generic as super

method! class_of_operation op =
  match op with
  | Ispecific spec ->
    begin match spec with
    | Ilea _ | Isextend32 | Izextend32 -> Op_pure
    | Istore_int(_, _, is_asg) -> Op_store is_asg
    | Ioffset_loc(_, _) -> Op_store true
    | Ifloatarithmem _ | Ifloatsqrtf _ -> Op_load
    | Ibswap _ | Isqrtf -> super#class_of_operation op
    | Irdtsc | Irdpmc -> Op_other
    | Icrc32q -> Op_pure
    | Iprefetch _ -> Op_other
    end
  | Imove | Ispill | Ireload | Inegf | Iabsf | Iaddf | Isubf | Imulf | Idivf
  | Icompf _
  | Ifloatofint | Iintoffloat | Iconst_int _ | Iconst_float _ | Iconst_symbol _
  | Icall_ind | Icall_imm _ | Itailcall_ind | Itailcall_imm _ | Iextcall _
  | Istackoffset _ | Iload _ | Istore _ | Ialloc _
  | Iintop _ | Iintop_imm _
  | Iname_for_debugger _ | Iprobe _ | Iprobe_is_enabled _ | Iopaque
    -> super#class_of_operation op

end

let fundecl f =
  (new cse)#fundecl f
