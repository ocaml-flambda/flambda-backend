(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

include Int_ids.Const

let of_descr (descr : Descr.t) =
  match descr with
  | Naked_immediate i -> naked_immediate i
  | Tagged_immediate i -> tagged_immediate i
  | Naked_float f -> naked_float f
  | Naked_int32 i -> naked_int32 i
  | Naked_int64 i -> naked_int64 i
  | Naked_nativeint i -> naked_nativeint i
  | Naked_vec128 v -> naked_vec128 v

let is_naked_immediate t =
  match descr t with
  | Naked_immediate i -> Some i
  | Tagged_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Naked_vec128 _ ->
    None

let is_tagged_immediate t =
  match descr t with
  | Tagged_immediate i -> Some i
  | Naked_immediate _ | Naked_float _ | Naked_int32 _ | Naked_int64 _
  | Naked_nativeint _ | Naked_vec128 _ ->
    None

let dummy_from_kind (kind : Flambda_kind.t) =
  match kind with
  | Value ->
    tagged_immediate Targetint_31_63.zero
  | Naked_number Naked_immediate ->
    naked_immediate Targetint_31_63.zero
  | Naked_number Naked_float ->
    naked_float Numeric_types.Float_by_bit_pattern.zero
  | Naked_number Naked_int32 ->
    naked_int32 Int32.zero
  | Naked_number Naked_int64 ->
    naked_int64 Int64.zero
  | Naked_number Naked_nativeint ->
    naked_nativeint Targetint_32_64.zero
  | Naked_number Naked_vec128 ->
    naked_vec128 Vector_types.Vec128.Bit_pattern.zero
  | Region | Rec_info ->
    Misc.fatal_errorf "No dummy constant for kind %a" Flambda_kind.print kind
