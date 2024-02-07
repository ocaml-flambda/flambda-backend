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

(* Each .o file has a matching .cmx file that provides the following infos
   on the compilation unit:
     - list of other units imported, with MD5s of their .cmx files
     - approximation of the structure implemented
       (includes descriptions of known functions: arity and direct entry
        points)
     - list of currying functions and application functions needed
   The .cmx file contains these infos (as an externed record) plus a MD5
   of these infos *)

type export_info =
  | Clambda of Clambda.value_approximation
  | Flambda1 of Export_info.t
  | Flambda2 of Flambda2_cmx.Flambda_cmx_format.t option

type export_info_raw =
  | Clambda_raw of Clambda.value_approximation
  | Flambda1_raw of Export_info.t
  | Flambda2_raw of Flambda2_cmx.Flambda_cmx_format.raw option

(* Declare machtype here to avoid depending on [Cmm]. *)
type machtype_component = Val | Addr | Int | Float | Vec128
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
    mutable ui_runtime_params: Global.Name.t list;
                                          (* Implementation imports which are
                                             bound as parameters at runtime,
                                             including source-level parameters
                                             as well as implementation imports
                                             with unbound parameters *)
    mutable ui_generic_fns: generic_fns;  (* Generic functions needed *)
    mutable ui_export_info: export_info;
    mutable ui_checks: Checks.t;
    mutable ui_force_link: bool }         (* Always linked *)

type unit_infos_raw =
  { uir_unit: Compilation_unit.t;
    uir_defines: Compilation_unit.t list;
    uir_imports_cmi: Import_info.t array;
    uir_imports_cmx: Import_info.t array;
    uir_runtime_params: Global.Name.t array;
    uir_generic_fns: generic_fns;
    uir_export_info: export_info_raw;
    uir_checks: Checks.Raw.t;
    uir_force_link: bool;
    uir_section_toc: int array;    (* Byte offsets of sections in .cmx
                                      relative to byte immediately after
                                      this record *)
    uir_sections_length: int;      (* Byte length of all sections *)
  }

(* Each .a library has a matching .cmxa file that provides the following
   infos on the library: *)

type lib_unit_info =
  { li_name: Compilation_unit.t;
    li_crc: Digest.t;
    li_defines: Compilation_unit.t list;
    li_force_link: bool;
    li_imports_cmi : Bitmap.t;  (* subset of lib_imports_cmi *)
    li_imports_cmx : Bitmap.t } (* subset of lib_imports_cmx *)

type library_infos =
  { lib_imports_cmi: Import_info.t array;
    lib_imports_cmx: Import_info.t array;
    lib_units: lib_unit_info list;
    lib_generic_fns: generic_fns;
    (* In the following fields the lists are reversed with respect to
       how they end up being used on the command line. *)
    lib_ccobjs: string list;            (* C object files needed *)
    lib_ccopts: string list }           (* Extra opts to C compiler *)
