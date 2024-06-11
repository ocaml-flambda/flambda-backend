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

(* Format of .cmx, .cmxa and .cmxs files *)

open Misc

type error =
    Not_a_unit_info of filepath
  | Corrupted_unit_info of filepath

exception Error of error

(* Each .o file has a matching .cmx file that provides the following infos
   on the compilation unit:
     - list of other units imported, with MD5s of their .cmx files
     - approximation of the structure implemented
       (includes descriptions of known functions: arity and direct entry
        points)
     - list of currying functions and application functions needed
   The .cmx file contains these infos (as an externed record) plus a MD5
   of these infos *)

(* Declare machtype here to avoid depending on [Cmm]. *)
type machtype_component = Val | Addr | Int | Float | Vec128 | Float32
type machtype = machtype_component array

type apply_fn := machtype list * machtype * Lambda.alloc_mode

(* Curry/apply/send functions *)
type generic_fns =
  { curry_fun: (Lambda.function_kind * machtype list * machtype) list;
    apply_fun: apply_fn list;
    send_fun: apply_fn list }

type unit_infos =
  { mutable ui_unit: Compilation_unit.t;  (* Compilation unit implemented *)
    mutable ui_defines: Compilation_unit.t list;
                                          (* All compilation units in the
                                             .cmx file (i.e. [ui_unit] and
                                             any produced via [Asmpackager]) *)
    mutable ui_imports_cmi: Import_info.t list;
                                          (* Interfaces imported *)
    mutable ui_imports_cmx: Import_info.t list;
                                          (* Infos imported *)
    mutable ui_generic_fns: generic_fns;  (* Generic functions needed *)
    mutable ui_export_info: Flambda2_cmx.Flambda_cmx_format.t option;
    mutable ui_zero_alloc_info: Zero_alloc_info.t;
    mutable ui_force_link: bool;          (* Always linked *)
    mutable ui_external_symbols: string list; (* Set of external symbols *)
  }

(* Each .a library has a matching .cmxa file that provides the following
   infos on the library: *)

type library_infos =
  { lib_imports_cmi: Import_info.t list;
    lib_imports_cmx: Import_info.t list;
    lib_units: (unit_infos * Digest.t) list;
    lib_generic_fns: generic_fns;
    (* In the following fields the lists are reversed with respect to
       how they end up being used on the command line. *)
    lib_ccobjs: string list;            (* C object files needed *)
    lib_ccopts: string list }           (* Extra opts to C compiler *)

val read_unit_info : filename:filepath -> unit_infos * Digest.t

val write_unit_info : filename:filepath -> unit_infos -> unit

val read_library_info : filename:filepath -> library_infos

val write_library_info : filename:filepath -> library_infos -> unit
