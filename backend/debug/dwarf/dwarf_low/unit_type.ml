(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t =
  | Compile
  | Type
  | Partial
  | Skeleton
  | Split_compile
  | Split_type

let code t =
  match t with
  | Compile -> 0x01
  | Type -> 0x02
  | Partial -> 0x03
  | Skeleton -> 0x04
  | Split_compile -> 0x05
  | Split_type -> 0x06

let encode t =
  Dwarf_value.uint8 ~comment:"Unit header unit type"
    (Numbers_extra.Uint8.of_nonnegative_int_exn (code t))

let size t = Dwarf_value.size (encode t)

let emit ~params t = Dwarf_value.emit ~params (encode t)
