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
open CSE_utils

class cfg_cse = object

  inherit Cfg_cse.cse_generic as super

  method! class_of_operation op =
    match op with
    | Specific spec ->
      (match spec with
       | Ifar_poll _
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
       | Isignext _ -> Op_pure)
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
      -> super#class_of_operation op

  method! is_cheap_operation op =
    match op with
    | Const_int n -> Nativeint.compare n 65535n <= 0 && Nativeint.compare n 0n >= 0
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
    | Begin_region | End_region | Poll | Dls_get
      -> false

end

let cfg_with_layout cfg_with_layout =
  (new cfg_cse)#cfg_with_layout cfg_with_layout
