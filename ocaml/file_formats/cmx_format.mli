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
  | Flambda of Export_info.t

(* Declare machtype here to avoid depending on [Cmm]. *)
type machtype_component = Val | Addr | Int | Float
type machtype = machtype_component array

type apply_fn := machtype list * machtype * Lambda.alloc_mode

type unit_infos =
  { mutable ui_unit: Compilation_unit.t;  (* Compilation unit implemented *)
    mutable ui_defines: Compilation_unit.t list;
                                          (* All compilation units in the
                                             .cmx file (i.e. [ui_name] and
                                             any produced via [Asmpackager]) *)
    mutable ui_arg_descr: Lambda.arg_descr option;
                                          (* If this is an argument unit, the
                                             parameter it implements *)
    mutable ui_imports_cmi: Import_info.t array;
                                          (* Interfaces imported *)
    mutable ui_imports_cmx: Import_info.t array;
                                          (* Infos imported *)
    mutable ui_runtime_params: Global.Name.t list;
                                          (* Implementation imports which are
                                             bound as parameters at runtime,
                                             including source-level parameters
                                             as well as implementation imports
                                             with unbound parameters *)
    mutable ui_curry_fun:
      (Lambda.function_kind * machtype list * machtype) list;
                                          (* Currying functions needed *)
    mutable ui_apply_fun: apply_fn list;  (* Apply functions needed *)
    mutable ui_send_fun: apply_fn list;   (* Send functions needed *)
    mutable ui_export_info: export_info;
    mutable ui_force_link: bool;          (* Always linked *)
    mutable ui_for_pack: string option }  (* Part of a pack *)

(* Each .a library has a matching .cmxa file that provides the following
   infos on the library: *)

type library_infos =
  { lib_units: (unit_infos * Digest.t) list;  (* List of unit infos w/ MD5s *)
    (* In the following fields the lists are reversed with respect to
       how they end up being used on the command line. *)
    lib_ccobjs: string list;            (* C object files needed *)
    lib_ccopts: string list }           (* Extra opts to C compiler *)
