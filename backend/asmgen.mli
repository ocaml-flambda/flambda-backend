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

(** The type of converters from Lambda to Flambda 2 programs. *)
type middle_end_flambda2 =
     ppf_dump:Format.formatter
  -> prefixname:string
  -> backend:(module Flambda2__Flambda_backend_intf.S)
  -> filename:string
  -> module_ident:Ident.t
  -> module_block_size_in_words:int
  -> module_initializer:Lambda.lambda
  -> Flambda2__Flambda_middle_end.middle_end_result

(** Compile an implementation from Lambda using Flambda 2.
    The Flambda 2 middle end does not use the Clambda language nor the
    Cmmgen pass.  Instead it emits Cmm directly. *)
val compile_implementation_flambda2
   : ?toplevel:(string -> bool)
  -> backend:(module Flambda2__Flambda_backend_intf.S)
  -> filename:string
  -> prefixname:string
  -> size:int
  -> module_ident:Ident.t
  -> module_initializer:Lambda.lambda
  -> middle_end:middle_end_flambda2
  -> flambda2_to_cmm:(
         Flambda2__Flambda_middle_end.middle_end_result
      -> Cmm.phrase list)
  -> ppf_dump:Format.formatter
  -> required_globals:Ident.Set.t
  -> unit
  -> unit

val compile_implementation_linear :
    string -> progname:string -> unit

val compile_phrase
  : ?dwarf:Dwarf_ocaml.Dwarf.t 
  -> ppf_dump:Format.formatter
  -> Cmm.phrase
  -> unit

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
