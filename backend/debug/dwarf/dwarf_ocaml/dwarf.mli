(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2013--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Generation and emission of DWARF debugging information for OCaml
    compilation units. *)

type t

(** Create a value of type [t], which holds all state necessary to emit
    DWARF debugging information for a single compilation unit.
    The names of certain parameters line up with the code in [Asmgen].
    The current [Compilation_unit] must have been set before calling this
    function. *)
val create
   : sourcefile:string
  -> unit_name:Ident.t
  -> t

(** Write the DWARF information to the assembly file.  This should only be
    called once all (in)constants and function declarations have been passed
    to the above functions. *)
val emit : params:(module Dwarf_params.S) -> t -> unit
