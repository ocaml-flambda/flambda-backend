(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*            Mark Shinwell and Thomas Refis, Jane Street Europe          *)
(*                                                                        *)
(*   Copyright 2013--2023 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open! Asm_targets

(** The name laundry: where names get (de)mangled.
    (This functionality is used by the debugger support library as well as
    the compiler.) *)

(** The name of the DWARF debugging information entry corresponding to the
    type of some identifier. *)
val base_type_die_name_for_var :
  Compilation_unit.t -> Backend_var.t -> Is_parameter.t -> string
