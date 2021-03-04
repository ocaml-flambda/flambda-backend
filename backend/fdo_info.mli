(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Greta Yorsh, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)
(** Currently, [Fdo_info] holds low-level location information that
    identifies instructions in the intermediate representation,
    for the purpose of mapping execution profiles directly back
    to them, instead of a source location. *)
type t = private int option
val none : t
val is_none : t -> bool
val create : int -> t
val get : t  -> int
