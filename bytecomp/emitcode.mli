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

(* Generation of bytecode for .cmo files *)

open Cmo_format
open Instruct

val to_file: out_channel -> Compilation_unit.t -> Unit_info.Artifact.t ->
  required_globals:Compilation_unit.Set.t ->
  main_module_block_format:Lambda.main_module_block_format ->
  arg_descr:Lambda.arg_descr option -> instruction list -> unit
        (* Arguments:
             channel on output file
             name of compilation unit implemented
             path of cmo file being written
             required_globals: list of compilation units that must be
               evaluated before this one
             list of instructions to emit *)
val to_memory:
  instruction list ->
    Misc.LongString.t * (reloc_info * int) list * debug_event list
        (* Arguments:
             initialization code (terminated by STOP)
             function code
           Results:
             block of relocatable bytecode
             relocation information
             debug events *)
val to_packed_file:
  out_channel -> instruction list ->
    int * (reloc_info * int) list * debug_event list * Misc.Stdlib.String.Set.t
        (* Arguments:
             channel on output file
             list of instructions to emit
           Result:
             size of the emitted code
             relocation information
             debug events
             debug directory
             *)

val marshal_to_channel_with_possibly_32bit_compat :
  filename:string -> kind:string -> out_channel -> 'a -> unit
