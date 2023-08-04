(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Bytes_relative_to_cfa of int
  | Bytes_relative_to_domainstate_pointer of int

let print ppf t =
  match t with
  | Bytes_relative_to_cfa i ->
    Format.fprintf ppf "@[<hov 1>(Bytes_relative_to_cfa@ %d)@]" i
  | Bytes_relative_to_domainstate_pointer i ->
    Format.fprintf ppf "@[<hov 1>(Bytes_relative_to_domainstate_pointer@ %d)@]"
      i
