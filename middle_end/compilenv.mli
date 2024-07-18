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

val reset : Compilation_unit.t -> unit
        (* Reset the environment and record the name of the unit being
           compiled (including any associated -for-pack prefix). *)

val reset_info_tables: unit -> unit

val current_unit_infos: unit -> unit_infos
        (* Return the infos for the unit being compiled *)

val get_global_export_info : Compilation_unit.t
  -> Flambda2_cmx.Flambda_cmx_format.t option
        (* Means of getting the export info found in the
           .cmx file of the given unit. *)

val get_unit_export_info
  : Compilation_unit.t -> Flambda2_cmx.Flambda_cmx_format.t option

val set_export_info : Flambda2_cmx.Flambda_cmx_format.t -> unit
        (* Set the export information for the current unit. *)

val need_curry_fun:
  Lambda.function_kind -> Cmm.machtype list -> Cmm.machtype -> unit
val need_apply_fun:
  Cmm.machtype list -> Cmm.machtype -> Lambda.alloc_mode -> unit
val need_send_fun:
  Cmm.machtype list -> Cmm.machtype -> Lambda.alloc_mode -> unit
        (* Record the need of a currying (resp. application,
           message sending) function with the given arity *)

val cached_zero_alloc_info : Zero_alloc_info.t
        (* Return cached information about functions
           (from other complication units) that satisfy certain properties. *)

val cache_zero_alloc_info : Zero_alloc_info.t -> unit
        (* [cache_zero_alloc_info c] adds [c] to [cached_zero_alloc_info] *)

val new_const_symbol : unit -> string

val read_unit_info: string -> unit_infos * Digest.t
        (* Read infos and MD5 from a [.cmx] file. *)
val write_unit_info: unit_infos -> string -> unit
        (* Save the given infos in the given file *)
val save_unit_info:
  string -> module_block_format:Lambda.module_block_format ->
  arg_descr:Lambda.arg_descr option ->
  unit
        (* Save the infos for the current unit in the given file *)
val cache_unit_info: unit_infos -> unit
        (* Enter the given infos in the cache.  The infos will be
           honored by [symbol_for_global] and [global_approx]
           without looking at the corresponding .cmx file. *)

val require_global: Compilation_unit.t -> unit
        (* Enforce a link dependency of the current compilation
           unit to the required module *)

val read_library_info: string -> library_infos

val record_external_symbols : unit -> unit

(* CR mshinwell: see comment in .ml
val ensure_sharing_between_cmi_and_cmx_imports :
  (_ * (Compilation_unit.t * _) option) list ->
  (Compilation_unit.t * 'a) list ->
  (Compilation_unit.t * 'a) list
*)

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of Compilation_unit.t * Compilation_unit.t * string

exception Error of error

val report_error: Format.formatter -> error -> unit
