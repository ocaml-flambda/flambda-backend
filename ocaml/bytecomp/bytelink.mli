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

open Misc

(* Link .cmo files and produce a bytecode executable. *)

(* CR mshinwell: seems like this should use [CU.Name.t] *)
module Dep : Set.OrderedType with type t = string * string
module DepSet : Set.S with type elt = Dep.t

val link : filepath list -> filepath -> unit
val reset : unit -> unit

val check_consistency: filepath -> Cmo_format.compilation_unit_descr -> unit

val extract_crc_interfaces: unit -> Import_info.Intf.t list

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Wrong_object_name of filepath
  | Symbol_error of filepath * Symtable.error
  | Inconsistent_import of Compilation_unit.Name.t * filepath * filepath
  | Custom_runtime
  | File_exists of filepath
  | Cannot_open_dll of filepath
  | Required_module_unavailable of string * Compilation_unit.t
  | Camlheader of string * filepath
  | Wrong_link_order of DepSet.t
  (* CR mshinwell: seems like [Multiple_definition] should use [CU.t] *)
  | Multiple_definition of string * filepath * filepath

exception Error of error

open Format

val report_error: formatter -> error -> unit
