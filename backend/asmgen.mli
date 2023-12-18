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

(** From Lambda to assembly code *)

(** The type of converters straight from Lambda to Cmm. This is how Flambda 2
    operates. *)
type direct_to_cmm =
  ppf_dump:Format.formatter
  -> prefixname:string
  -> filename:string
  -> Lambda.program
  -> Cmm.phrase list

(** The one true way to get from Lambda to Cmm. *)
type pipeline =
  | Direct_to_cmm of direct_to_cmm

(** Compile an implementation from Lambda using the given middle end. *)
val compile_implementation
   : (module Compiler_owee.Unix_intf.S)
  -> ?toplevel:(string -> bool)
  -> pipeline:pipeline
  -> filename:string
  -> prefixname:string
  -> ppf_dump:Format.formatter
  -> Lambda.program
  -> unit

val compile_implementation_linear
  : (module Compiler_owee.Unix_intf.S)
  -> string
  -> progname:string
  -> ppf_dump:Format.formatter
  -> unit

val compile_phrase
  : ppf_dump:Format.formatter
  -> Cmm.phrase
  -> unit

type error =
  | Assembler_error of string
  | Mismatched_for_pack of Compilation_unit.Prefix.t
  | Asm_generation of string * Emitaux.error

exception Error of error
val report_error: Format.formatter -> error -> unit

val compile_unit
   : output_prefix:string
   -> asm_filename:string
   -> keep_asm:bool
   -> obj_filename:string
   -> may_reduce_heap:bool
   -> ppf_dump:Format.formatter
   -> (unit -> unit)
   -> unit
