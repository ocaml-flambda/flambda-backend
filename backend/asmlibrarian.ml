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

(* Build libraries of .cmx files *)

open Misc
open Config
open Cmx_format
module String = Misc.Stdlib.String

type error =
    File_not_found of string
  | Archiver_error of string

exception Error of error

let default_ui_export_info =
  if Config.flambda then
    Cmx_format.Flambda1 Export_info.empty
  else if Config.flambda2 then
    Cmx_format.Flambda2 None
  else
    Cmx_format.Clambda Clambda.Value_unknown

let read_info name =
  let filename =
    try
      Load_path.find name
    with Not_found ->
      raise(Error(File_not_found name)) in
  let (info, crc) = Compilenv.read_unit_info filename in
  info.ui_force_link <- info.ui_force_link || !Clflags.link_everything;
  (* There is no need to keep the approximation in the .cmxa file,
     since the compiler will go looking directly for .cmx files.
     The linker, which is the only one that reads .cmxa files, does not
     need the approximation. *)
  info.ui_export_info <- default_ui_export_info;
  (Filename.chop_suffix filename ".cmx" ^ ext_obj, (info, crc))

let create_archive file_list lib_name =
  let archive_name = Filename.remove_extension lib_name ^ ext_lib in
  let outchan = open_out_bin lib_name in
  Misc.try_finally
    ~always:(fun () -> close_out outchan)
    ~exceptionally:(fun () -> remove_file lib_name; remove_file archive_name)
    (fun () ->
       output_string outchan cmxa_magic_number;
       let (objfile_list, descr_list) =
         List.split (List.map read_info file_list) in
       List.iter2
         (fun file_name (unit, crc) ->
            Asmlink.check_consistency file_name unit crc)
         file_list descr_list;
       let cmis = Asmlink.extract_crc_interfaces () |> Array.of_list in
       let cmxs = Asmlink.extract_crc_implementations () |> Array.of_list in
       let cmi_index = String.Tbl.create 42 in
       Array.iteri (fun i (name, _crc) -> String.Tbl.add cmi_index name i) cmis;
       let cmx_index = String.Tbl.create 42 in
       Array.iteri (fun i (name, _crc) -> String.Tbl.add cmx_index name i) cmxs;
       let genfns = Cmm_helpers.Generic_fns_tbl.make () in
       let mk_bitmap arr ix entries =
         let module B = Misc.Bitmap in
         let b = B.make (Array.length arr) in
         entries |> List.iter (fun (name, _crc) -> B.set b (String.Tbl.find ix name));
         b
       in
       let units =
         List.map (fun (unit, crc) ->
           Cmm_helpers.Generic_fns_tbl.add genfns unit.ui_generic_fns;
           { li_name = unit.ui_name;
             li_symbol = unit.ui_symbol;
             li_crc = crc;
             li_defines = unit.ui_defines;
             li_force_link = unit.ui_force_link;
             li_imports_cmi = mk_bitmap cmis cmi_index unit.ui_imports_cmi;
             li_imports_cmx = mk_bitmap cmxs cmx_index unit.ui_imports_cmx })
         descr_list
       in
       let infos =
         { lib_units = units;
           lib_imports_cmi = cmis;
           lib_imports_cmx = cmxs;
           lib_generic_fns = Cmm_helpers.Generic_fns_tbl.entries genfns;
           lib_ccobjs = !Clflags.ccobjs;
           lib_ccopts = !Clflags.all_ccopts } in
       output_value outchan infos;
       if Ccomp.create_archive archive_name objfile_list <> 0
       then raise(Error(Archiver_error archive_name));
    )

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Archiver_error name ->
      fprintf ppf "Error while creating the library %s" name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
