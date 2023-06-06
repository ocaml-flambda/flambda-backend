(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                      Mark Shinwell, Jane Street                        *)
(*                                                                        *)
(*   Copyright 2023 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | In_current_dir
  | Not_in_current_dir
  | Unknown

let print ppf t =
  match t with
  | In_current_dir -> Format.fprintf ppf "In_current_dir"
  | Not_in_current_dir -> Format.fprintf ppf "Not_in_current_dir"
  | Unknown -> Format.fprintf ppf "Unknown"
