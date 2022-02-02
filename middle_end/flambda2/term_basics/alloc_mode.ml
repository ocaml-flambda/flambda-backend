(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Heap
  | Local

let print ppf t =
  match t with
  | Heap -> Format.pp_print_string ppf "Heap"
  | Local -> Format.pp_print_string ppf "Local"

let compare t1 t2 =
  match t1, t2 with
  | Heap, Heap | Local, Local -> 0
  | Heap, Local -> -1
  | Local, Heap -> 1
