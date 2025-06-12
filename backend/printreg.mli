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

(* Pretty-printing of registers *)

[@@@ocaml.warning "+a-40-41-42"]

val loc :
  ?wrap_out:(Format.formatter -> (Format.formatter -> unit) -> unit) ->
  unknown:(Format.formatter -> unit) ->
  Format.formatter ->
  Reg.location ->
  Cmm.machtype_component ->
  unit

val reg : Format.formatter -> Reg.t -> unit

val regs' :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  Reg.t array ->
  unit

val regs : Format.formatter -> Reg.t array -> unit

val regset : Format.formatter -> Reg.Set.t -> unit

val reglist : Format.formatter -> Reg.t list -> unit

val regsetaddr' :
  ?print_reg:(Format.formatter -> Reg.t -> unit) ->
  Format.formatter ->
  Reg.Set.t ->
  unit

val regsetaddr : Format.formatter -> Reg.Set.t -> unit
