(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Gallium, INRIA Rocquencourt           *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2010 Institut National de Recherche en Informatique et     *)
(*     en Automatique                                                     *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Compilation environments for compilation units *)

open Cmx_format

(* CR-soon mshinwell: this is a bit ugly
   mshinwell: deferred CR, this has been addressed in the export info
   improvement feature.
*)
val imported_sets_of_closures_table
  : Simple_value_approx.function_declarations option Set_of_closures_id.Tbl.t
        (* flambda-only *)

val reset : Compilation_unit.t -> unit
        (* Reset the environment and record the name of the unit being
           compiled (including any associated -for-pack prefix). *)

val current_unit_infos: unit -> unit_infos
        (* Return the infos for the unit being compiled *)

val global_approx: Compilation_unit.t -> Clambda.value_approximation
        (* Return the approximation for the given global identifier
           clambda-only *)
val set_global_approx: Clambda.value_approximation -> unit
        (* Record the approximation of the unit being compiled
           clambda-only *)
val record_global_approx_toplevel: unit -> unit
        (* Record the current approximation for the current toplevel phrase
           clambda-only *)

val set_export_info: Export_info.t -> unit
        (* Record the information of the unit being compiled
           flambda-only *)
val approx_env: unit -> Export_info.t
        (* Returns all the information loaded from external compilation units
           flambda-only *)
val approx_for_global: Compilation_unit.t -> Export_info.t option
        (* Loads the exported information declaring the compilation_unit
           flambda-only *)

val need_curry_fun:
  Lambda.function_kind ->
  Cmx_format.machtype list ->
  Cmx_format.machtype ->
  unit
val need_apply_fun:
  Cmx_format.machtype list -> Cmx_format.machtype -> Lambda.alloc_mode -> unit
val need_send_fun:
  Cmx_format.machtype list -> Cmx_format.machtype -> Lambda.alloc_mode -> unit
        (* Record the need of a currying (resp. application,
           message sending) function with the given arity *)

val new_const_symbol : unit -> string

val new_structured_constant:
  Clambda.ustructured_constant ->
  shared:bool -> (* can be shared with another structurally equal constant *)
  string
val structured_constants:
  unit -> Clambda.preallocated_constant list
val clear_structured_constants: unit -> unit

val structured_constant_of_symbol:
  string -> Clambda.ustructured_constant option

val add_exported_constant: string -> unit
        (* clambda-only *)
type structured_constants
        (* clambda-only *)
val snapshot: unit -> structured_constants
        (* clambda-only *)
val backtrack: structured_constants -> unit
        (* clambda-only *)

val read_unit_info: string -> unit_infos * Digest.t
        (* Read infos and MD5 from a [.cmx] file. *)
val write_unit_info: unit_infos -> string -> unit
        (* Save the given infos in the given file *)
val save_unit_info: string -> unit
        (* Save the infos for the current unit in the given file *)
val cache_unit_info: unit_infos -> unit
        (* Enter the given infos in the cache.  The infos will be
           honored by [symbol_for_global] and [global_approx]
           without looking at the corresponding .cmx file. *)

val require_global: Compilation_unit.t -> unit
        (* Enforce a link dependency of the current compilation
           unit to the required module *)

val read_library_info: string -> library_infos

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of Compilation_unit.t * Compilation_unit.t * string
  | Mismatching_for_pack of
      string * Compilation_unit.Prefix.t * Compilation_unit.Name.t
      * Compilation_unit.Prefix.t option

exception Error of error

val report_error: Format.formatter -> error -> unit
