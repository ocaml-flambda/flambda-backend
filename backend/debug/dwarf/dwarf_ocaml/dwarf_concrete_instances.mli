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

type fundecl =
  { fun_name : string;
    fun_dbg : Debuginfo.t
  }

val for_fundecl :
  get_file_id:(string -> int) -> Dwarf_state.t -> fundecl -> unit

(** End symbol name given start symbol name for a function block *)
val end_symbol_name : start_symbol:string -> string
