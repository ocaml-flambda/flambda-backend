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

(** The type of converters from Lambda to Clambda. *)
type middle_end =
     backend:(module Backend_intf.S)
  -> filename:string
  -> prefixname:string
  -> ppf_dump:Format.formatter
  -> Lambda.program
  -> Clambda.with_constants

(** Compile an implementation from Lambda using the given middle end. *)
val compile_implementation
   : ?toplevel:(string -> bool)
  -> backend:(module Backend_intf.S)
  -> filename:string
  -> prefixname:string
  -> middle_end:middle_end
  -> ppf_dump:Format.formatter
  -> Lambda.program
  -> unit

(** Compile an implementation from Lambda using Flambda 2.
    The Flambda 2 middle end neither uses the Clambda language nor the
    Cmmgen pass.  Instead it emits Cmm directly. *)
val compile_implementation_flambda2
   : ?toplevel:(string -> bool)
  -> ?keep_symbol_tables:bool
  -> filename:string
  -> prefixname:string
  -> size:int
  -> module_ident:Ident.t
  -> module_initializer:Lambda.lambda
  -> flambda2:(
    ppf_dump:Format.formatter ->
    prefixname:string ->
    filename:string ->
    module_ident:Ident.t ->
    module_block_size_in_words:int ->
    module_initializer:Lambda.lambda ->
    keep_symbol_tables:bool ->
    Cmm.phrase list)
  -> ppf_dump:Format.formatter
  -> required_globals:Ident.Set.t
  -> unit
  -> unit

val compile_implementation_linear :
    string -> progname:string -> unit

val compile_phrase :
    ppf_dump:Format.formatter -> Cmm.phrase -> unit

type error =
  | Assembler_error of string
  | Mismatched_for_pack of string option

exception Error of error
val report_error: Format.formatter -> error -> unit

val compile_unit
   : output_prefix:string
   -> asm_filename:string
   -> keep_asm:bool
   -> obj_filename:string
   -> (unit -> unit)
   -> unit
