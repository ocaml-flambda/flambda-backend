(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Construction of DWARF "compile_unit" DIEs and associated entities. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open Dwarf_high
open Dwarf_low

val compile_unit_proto_die
   : sourcefile:string
  -> cmt_file_digest:Digest.t option
  -> objfiles:string list
  -> unit_name:Ident.t
  -> Address_table.t
  -> Location_list_table.t
  -> Range_list_table.t
  -> Proto_die.t