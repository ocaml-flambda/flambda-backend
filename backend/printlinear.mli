(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Pretty-printing of linearized machine code *)

[@@@ocaml.warning "+a-40-41-42"]

open Format
open Linear

val call_operation :
  ?print_reg:(formatter -> Reg.t -> unit) ->
  formatter ->
  Linear.call_operation ->
  Reg.t array ->
  unit

val instr' :
  ?print_reg:(formatter -> Reg.t -> unit) -> formatter -> instruction -> unit

val instr : formatter -> instruction -> unit

val fundecl : formatter -> fundecl -> unit
