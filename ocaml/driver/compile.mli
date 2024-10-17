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

(** Bytecode compilation for .ml and .mli files. *)

val interface:
  source_file:string -> output_prefix:string -> unit
val implementation:
  start_from:Clflags.Compiler_pass.t ->
  source_file:string -> output_prefix:string -> keep_symbol_tables:bool -> unit
val instance:
  source_file:string -> output_prefix:string ->
  compilation_unit:Compilation_unit.t ->
  runtime_args:Translmod.runtime_arg list ->
  main_module_block_size:int ->
  arg_descr:Lambda.arg_descr option ->
  keep_symbol_tables:bool -> unit

(** {2 Internal functions} **)

val to_bytecode :
  Compile_common.info ->
  Typedtree.implementation ->
  as_arg_for:Global_module.Name.t option ->
  Instruct.instruction list * Compilation_unit.Set.t *
    Lambda.module_block_format *
    Lambda.arg_descr option
(** [to_bytecode info typed] takes a typechecked implementation
    and returns its bytecode.
*)

val emit_bytecode :
  Compile_common.info ->
  Instruct.instruction list * Compilation_unit.Set.t *
    Lambda.module_block_format *
    Lambda.arg_descr option ->
    unit
(** [emit_bytecode bytecode] output the bytecode executable. *)
