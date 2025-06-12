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

(* Generation of bytecode from lambda terms *)

open Instruct

val compile_implementation :
  Compilation_unit.t -> Blambda.blambda -> instruction list

val compile_phrase : Blambda.blambda -> instruction list * bool

val merge_events :
  Instruct.debug_event -> Instruct.debug_event -> Instruct.debug_event
