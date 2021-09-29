(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2021 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type 'a t =
  | Finite of 'a
  | Infinity

let equal ~f t1 t2 =
  match t1, t2 with
  | Finite a1, Finite a2 -> f a1 a2
  | Infinity, Infinity -> true
  | (Finite _ | Infinity), _ -> false

let compare ~f t1 t2 =
  match t1, t2 with
  | Finite a1, Finite a2 -> f a1 a2
  | Infinity, Infinity -> 0
  | Finite _, Infinity -> -1
  | Infinity, Finite _ -> 1

let hash ~f = function
  | Finite a -> Hashtbl.hash (0, f a)
  | Infinity -> Hashtbl.hash 1

let [@ocamlformat "disable"] print ~f ppf = function
  | Finite a -> f ppf a
  | Infinity -> Format.pp_print_string ppf "\u{221e}"
