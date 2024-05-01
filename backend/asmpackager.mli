(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Package" a set of .cmx/.o files into one .cmx/.o file having the
   original compilation units as sub-modules. *)

val package_files
   : (module Compiler_owee.Unix_intf.S)
  -> ppf_dump:Format.formatter
  -> Env.t
  -> string list
  -> string
  -> flambda2:(
    ppf_dump:Format.formatter ->
    prefixname:string ->
    filename:string ->
    keep_symbol_tables:bool ->
    Lambda.program ->
    Cmm.phrase list)
  -> unit

type error =
    Illegal_renaming of Compilation_unit.Name.t * string * Compilation_unit.Name.t
  | Forward_reference of string * Compilation_unit.Name.t
  | Wrong_for_pack of string * Compilation_unit.t
  | Linking_error
  | Assembler_error of string
  | File_not_found of string

exception Error of error

val report_error: Format.formatter -> error -> unit
