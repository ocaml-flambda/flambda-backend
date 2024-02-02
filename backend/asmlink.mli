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

(* Link a set of .cmx/.o files and produce an executable or a plugin *)

open Misc
open Format

val link: (module Compiler_owee.Unix_intf.S) -> ppf_dump:formatter ->
  string list -> string -> unit

val link_shared: (module Compiler_owee.Unix_intf.S) ->
  ppf_dump:formatter -> string list -> string -> unit

val call_linker_shared: ?native_toplevel:bool -> string list -> string -> unit

val reset : unit -> unit
val check_consistency: filepath -> Cmx_format.unit_infos -> Digest.t -> unit
val extract_crc_interfaces: unit -> Import_info.Intf.t list
val extract_crc_implementations: unit -> Import_info.Impl.t list

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (Compilation_unit.t * string list) list
  | Inconsistent_interface of Compilation_unit.Name.t * filepath * filepath
  | Inconsistent_implementation of Compilation_unit.t * filepath * filepath
  | Assembler_error of filepath
  | Linking_error of int
  | Multiple_definition of Compilation_unit.Name.t * filepath * filepath
  | Missing_cmx of filepath * Compilation_unit.t

exception Error of error

val report_error: formatter -> error -> unit
