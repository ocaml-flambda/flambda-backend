(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Mark Shinwell and Xavier Clerc, Jane Street Europe          *)
(*                       Guillaume Bury, OCamlPro SAS                     *)
(*                                                                        *)
(*   Copyright 2017--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Values_or_immediates_or_naked_floats
  | Naked_float32s
  | Naked_int32s
  | Naked_int64s
  | Naked_nativeints

let print ppf t =
  match t with
  | Values_or_immediates_or_naked_floats -> Format.pp_print_string ppf "regular"
  | Naked_float32s -> Format.pp_print_string ppf "Naked_float32s"
  | Naked_int32s -> Format.pp_print_string ppf "Naked_int32s"
  | Naked_int64s -> Format.pp_print_string ppf "Naked_int64s"
  | Naked_nativeints -> Format.pp_print_string ppf "Naked_nativeints"

let compare = Stdlib.compare

let of_element_kind t =
  match (t : Flambda_kind.t) with
  | Value | Nullable_value | Naked_number Naked_float ->
    Values_or_immediates_or_naked_floats
  | Naked_number Naked_immediate ->
    Misc.fatal_errorf
      "Arrays cannot yet contain elements of kind naked immediate"
  | Naked_number Naked_float32 -> Naked_float32s
  | Naked_number Naked_int32 -> Naked_int32s
  | Naked_number Naked_int64 -> Naked_int64s
  | Naked_number Naked_nativeint -> Naked_nativeints
  | Naked_number Naked_vec128 ->
    Misc.fatal_errorf
      "Arrays cannot yet contain elements of kind unboxed vec128"
  | Region | Rec_info ->
    Misc.fatal_errorf
      "Arrays cannot contain elements of kind region or rec_info"

let of_lambda array_kind =
  match (array_kind : Lambda.array_kind) with
  | Pnullablearray _ | Pgenarray | Paddrarray | Pintarray | Pfloatarray
  | Punboxedfloatarray Pfloat64 ->
    Values_or_immediates_or_naked_floats
  | Punboxedfloatarray Pfloat32 -> Naked_float32s
  | Punboxedintarray Pint32 -> Naked_int32s
  | Punboxedintarray Pint64 -> Naked_int64s
  | Punboxedintarray Pnativeint -> Naked_nativeints
