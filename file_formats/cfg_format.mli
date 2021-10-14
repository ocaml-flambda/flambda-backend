(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                    Greta Yorsh, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Format of .cmir-cfg files *)

(* Compiler can optionally save Cfg representation of a compilation unit,
   along with other information required to emit assembly. *)
type cfg_item_info =
  | Cfg of Cfg_with_layout.t
  | Data of Cmm.data_item list

type cfg_unit_info =
  {
    mutable unit_name : string;
    mutable items : cfg_item_info list;
    mutable for_pack : string option
  }

(* Marshal and unmarshal a compilation unit in Cfg format.
   It includes saving and restoring global state required for Emit,
   that currently consists of Cmm.label_counter.
*)
val save : string -> cfg_unit_info -> unit
val restore : string -> cfg_unit_info * Digest.t
