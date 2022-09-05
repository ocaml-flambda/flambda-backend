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

type apply_fn := int * Lambda.alloc_mode

(* Curry/apply/send functions *)
type generic_fns =
  { curry_fun: Clambda.arity list;
    apply_fun: apply_fn list;
    send_fun: apply_fn list }

type crcs = (Compilation_unit.t * Digest.t option) list

type unit_infos =
  { mutable ui_unit: Compilation_unit.t;  (* Compilation unit implemented *)
    mutable ui_defines: Compilation_unit.t list;
                                          (* All compilation units in the
                                             .cmx file (i.e. [ui_unit] and
                                             any produced via [Asmpackager]) *)
    mutable ui_imports_cmi: (Compilation_unit.Name.t * Digest.t option) list;
                                          (* Interfaces imported *)
    mutable ui_imports_cmx: (Compilation_unit.Name.t * Digest.t option) list;
                                          (* Infos imported *)

    mutable ui_generic_fns: generic_fns;  (* Generic functions needed *)
    mutable ui_export_info: export_info;
    mutable ui_force_link: bool }         (* Always linked *)

(* Each .a library has a matching .cmxa file that provides the following
   infos on the library: *)

type lib_unit_info =
  { li_unit: Compilation_unit.t;
    li_crc: Digest.t;
    li_defines: Compilation_unit.t list;
    li_force_link: bool;
    li_imports_cmi : Bitmap.t;  (* subset of lib_imports_cmi *)
    li_imports_cmx : Bitmap.t } (* subset of lib_imports_cmx *)

type library_infos =
  (* CR lmaurer: Should document why this is an array rather than a list *)
  { lib_imports_cmi: (Compilation_unit.Name.t * Digest.t option) array;
    lib_imports_cmx: (Compilation_unit.Name.t * Digest.t option) array;
    lib_units: lib_unit_info list;
    lib_generic_fns: generic_fns;
    (* In the following fields the lists are reversed with respect to
       how they end up being used on the command line. *)
    lib_ccobjs: string list;            (* C object files needed *)
    lib_ccopts: string list }           (* Extra opts to C compiler *)
