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

(* Link a set of .cmx/.o files and produce an executable *)

open Misc
open Config
open Cmx_format
open Compilenv

module String = Misc.Stdlib.String

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (modname * string list) list
  | Inconsistent_interface of modname * filepath * filepath
  | Inconsistent_implementation of modname * filepath * filepath
  | Assembler_error of filepath
  | Linking_error of int
  | Multiple_definition of modname * filepath * filepath
  | Missing_cmx of filepath * modname

exception Error of error

type unit_link_info = {
  name: modname;
  symbol: string;
  defines: string list;
  file_name: string;
  crc: Digest.t;
  (* for shared libs *)
  dynunit : Cmxs_format.dynunit option;
}

(* Consistency check between interfaces and implementations *)

module Cmi_consistbl = Consistbl.Make (String)
let crc_interfaces = Cmi_consistbl.create ()
let interfaces = String.Tbl.create 100

module Cmx_consistbl = Consistbl.Make (String)
let crc_implementations = Cmx_consistbl.create ()
let implementations = ref ([] : string list)
let implementations_defined = String.Tbl.create 100
let cmx_required = ref ([] : string list)

let check_cmi_consistency file_name cmis =
  try
    Array.iter
      (fun (name, crco) ->
        String.Tbl.replace interfaces name ();
        match crco with
          None -> ()
        | Some crc ->
            Cmi_consistbl.check crc_interfaces name crc file_name)
      cmis
  with Cmi_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_interface(name, user, auth)))

let check_cmx_consistency file_name cmxs =
  try
    Array.iter
      (fun (name, crco) ->
        implementations := name :: !implementations;
        match crco with
            None ->
              if List.mem name !cmx_required then
                raise(Error(Missing_cmx(file_name, name)))
          | Some crc ->
              Cmx_consistbl.check crc_implementations name crc file_name)
      cmxs
  with Cmx_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_implementation(name, user, auth)))

let check_consistency ~unit cmis cmxs =
  check_cmi_consistency unit.file_name cmis;
  check_cmx_consistency unit.file_name cmxs;
  begin try
    let source = String.Tbl.find implementations_defined unit.name in
    raise (Error(Multiple_definition(unit.name, unit.file_name, source)))
  with Not_found -> ()
  end;
  implementations := unit.name :: !implementations;
  Cmx_consistbl.check crc_implementations unit.name unit.crc unit.file_name;
  String.Tbl.replace implementations_defined unit.name unit.file_name;
  if unit.symbol <> unit.name then
    cmx_required := unit.name :: !cmx_required

let extract_crc_interfaces () =
  String.Tbl.fold (fun name () crcs ->
      (name, Cmi_consistbl.find crc_interfaces name) :: crcs)
    interfaces
    []

let extract_crc_implementations () =
  Cmx_consistbl.extract !implementations crc_implementations

(* Add C objects and options and "custom" info from a library descriptor.
   See bytecomp/bytelink.ml for comments on the order of C objects. *)

let lib_ccobjs = ref []
let lib_ccopts = ref []

let add_ccobjs origin l =
  if not !Clflags.no_auto_link then begin
    lib_ccobjs := l.lib_ccobjs @ !lib_ccobjs;
    let replace_origin =
      Misc.replace_substring ~before:"$CAMLORIGIN" ~after:origin
    in
    lib_ccopts := List.map replace_origin l.lib_ccopts @ !lib_ccopts
  end

let runtime_lib () =
  let libname = "libasmrun" ^ !Clflags.runtime_variant ^ ext_lib in
  try
    if !Clflags.nopervasives || not !Clflags.with_runtime then []
    else [ Load_path.find libname ]
  with Not_found ->
    raise(Error(File_not_found libname))

(* First pass: determine which units are needed *)

let missing_globals =
  (Hashtbl.create 17 : (string, (string * string option) list ref) Hashtbl.t)

let is_required name =
  try ignore (Hashtbl.find missing_globals name); true
  with Not_found -> false

let add_required by (name, _crc) =
  try
    let rq = Hashtbl.find missing_globals name in
    rq := by :: !rq
  with Not_found ->
    Hashtbl.add missing_globals name (ref [by])

let remove_required name =
  Hashtbl.remove missing_globals name

let extract_missing_globals () =
  let mg = ref [] in
  let fmt = function
    | file, None -> file
    | file, Some part -> Printf.sprintf "%s(%s)" file part
  in
  Hashtbl.iter (fun md rq -> mg := (md, List.map fmt !rq) :: !mg) missing_globals;
  !mg

type file =
  | Unit of string * unit_infos * Digest.t
  | Library of string * library_infos

let read_file obj_name =
  let file_name =
    try
      Load_path.find obj_name
    with Not_found ->
      raise(Error(File_not_found obj_name)) in
  if Filename.check_suffix file_name ".cmx" then begin
    (* This is a .cmx file. It must be linked in any case.
       Read the infos to see which modules it requires. *)
    let (info, crc) = read_unit_info file_name in
    Unit (file_name,info,crc)
  end
  else if Filename.check_suffix file_name ".cmxa" then begin
    let infos =
      try read_library_info file_name
      with Compilenv.Error(Not_a_unit_info _) ->
        raise(Error(Not_an_object_file file_name))
    in
    Library (file_name,infos)
  end
  else raise(Error(Not_an_object_file file_name))

let scan_file ~shared genfns file (objfiles, tolink) =
  match read_file file with
  | Unit (file_name,info,crc) ->
      (* This is a .cmx file. It must be linked in any case. *)
      remove_required info.ui_name;
      List.iter (add_required (file_name, None)) info.ui_imports_cmx;
      let dynunit : Cmxs_format.dynunit option =
        if not shared then None else
          Some { dynu_name = info.ui_name;
                 dynu_crc = crc;
                 dynu_defines = info.ui_defines;
                 dynu_imports_cmi = info.ui_imports_cmi;
                 dynu_imports_cmx = info.ui_imports_cmx }
      in
      let unit =
        { name = info.ui_name;
          symbol = info.ui_symbol;
          crc;
          defines = info.ui_defines;
          file_name;
          dynunit }
      in
      let object_file_name =
        Filename.chop_suffix file_name ".cmx" ^ ext_obj in
      check_consistency ~unit
        (Array.of_list info.ui_imports_cmi)
        (Array.of_list info.ui_imports_cmx);
      Cmm_helpers.Generic_fns_tbl.add genfns info.ui_generic_fns;
      object_file_name :: objfiles, unit :: tolink
  | Library (file_name,infos) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      add_ccobjs (Filename.dirname file_name) infos;
      Cmm_helpers.Generic_fns_tbl.add genfns infos.lib_generic_fns;
      check_cmi_consistency file_name infos.lib_imports_cmi;
      check_cmx_consistency file_name infos.lib_imports_cmx;
      let objfiles =
        let obj_file =
          Filename.chop_suffix file_name ".cmxa" ^ ext_lib in
        (* MSVC doesn't support empty .lib files, and macOS struggles to
           make them (#6550), so there shouldn't be one if the .cmxa
           contains no units. The file_exists check is added to be
           ultra-defensive for the case where a user has manually added
           things to the .a/.lib file *)
        if infos.lib_units = [] && not (Sys.file_exists obj_file)
        then objfiles
        else obj_file :: objfiles
      in
      objfiles,
      List.fold_right
        (fun info reqd ->
           if info.li_force_link
           || !Clflags.link_everything
           || is_required info.li_name
           then begin
             remove_required info.li_name;
             let req_by = (file_name, Some info.li_name) in
             info.li_imports_cmx |> Misc.Bitmap.iter (fun i ->
               add_required req_by infos.lib_imports_cmx.(i));
             let imports_list tbl bits =
               List.init (Array.length tbl) (fun i ->
                 if Misc.Bitmap.get bits i then Some tbl.(i) else None)
               |> List.filter_map Fun.id
             in
             let dynunit : Cmxs_format.dynunit option =
               if not shared then None else
                 Some {
                   dynu_name = info.li_name;
                   dynu_crc = info.li_crc;
                   dynu_defines = info.li_defines;
                   dynu_imports_cmi =
                     imports_list infos.lib_imports_cmi info.li_imports_cmi;
                   dynu_imports_cmx =
                     imports_list infos.lib_imports_cmx info.li_imports_cmx }
             in
             let unit =
               { name = info.li_name;
                 symbol = info.li_symbol;
                 crc = info.li_crc;
                 defines = info.li_defines;
                 file_name;
                 dynunit }
             in
             check_consistency ~unit [| |] [| |];
             unit :: reqd
           end else
           reqd)
        infos.lib_units tolink

(* Second pass: generate the startup file and link it with everything else *)

let force_linking_of_startup ~ppf_dump =
  Asmgen.compile_phrase ~ppf_dump
    (Cmm.Cdata ([Cmm.Csymbol_address "caml_startup"]))

let make_globals_map units_list =
  (* The order in which entries appear in the globals map does not matter
     (see the natdynlink code).
     We can corrupt [interfaces] since it won't be used again until the next
     compilation. *)
  let defined =
    List.map (fun unit ->
        let intf_crc = Cmi_consistbl.find crc_interfaces unit.name in
        String.Tbl.remove interfaces unit.name;
        (unit.name, intf_crc, Some unit.crc, unit.defines))
      units_list
  in
  String.Tbl.fold (fun name () globals_map ->
      let intf_crc = Cmi_consistbl.find crc_interfaces name in
      (name, intf_crc, None, []) :: globals_map)
    interfaces
    defined

let make_startup_file ~ppf_dump ~filename genfns units =
  Location.input_name := "caml_startup"; (* set name of "current" input *)
  Compilenv.reset "_startup"; (* set the name of the "current" compunit *)
  let dwarf =
    Asmgen.emit_begin_assembly_with_dwarf
      ~emit_begin_assembly:Emit.begin_assembly
      ~sourcefile:filename
      ()
  in
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump ?dwarf p in
  let name_list =
    List.flatten (List.map (fun u -> u.defines) units) in
  compile_phrase (Cmm_helpers.entry_point name_list);
  List.iter compile_phrase (Cmm_helpers.generic_functions false genfns);
  Array.iteri
    (fun i name -> compile_phrase (Cmm_helpers.predef_exception i name))
    Runtimedef.builtin_exceptions;
  compile_phrase (Cmm_helpers.global_table name_list);
  let globals_map = make_globals_map units in
  compile_phrase (Cmm_helpers.globals_map globals_map);
  compile_phrase(Cmm_helpers.data_segment_table ("_startup" :: name_list));
  if !Clflags.function_sections then
    compile_phrase
      (Cmm_helpers.code_segment_table("_hot" :: "_startup" :: name_list))
  else
    compile_phrase(Cmm_helpers.code_segment_table("_startup" :: name_list));
  let all_names = "_startup" :: "_system" :: name_list in
  compile_phrase (Cmm_helpers.frame_table all_names);
  if !Clflags.output_complete_object then
    force_linking_of_startup ~ppf_dump;
  Emit.end_assembly dwarf

let make_shared_startup_file ~ppf_dump genfns units =
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  Location.input_name := "caml_startup";
  Compilenv.reset "_shared_startup";
  Emit.begin_assembly ~init_dwarf:(fun () -> ());
  List.iter compile_phrase
    (Cmm_helpers.generic_functions true genfns);
  let dynunits = List.map (fun u -> Option.get u.dynunit) units in
  compile_phrase (Cmm_helpers.plugin_header dynunits);
  compile_phrase
    (Cmm_helpers.global_table
       (List.map (fun u -> u.symbol) units));
  if !Clflags.output_complete_object then
    force_linking_of_startup ~ppf_dump;
  (* this is to force a reference to all units, otherwise the linker
     might drop some of them (in case of libraries) *)
  Emit.end_assembly None

let call_linker_shared file_list output_name =
  let exitcode = Ccomp.call_linker Ccomp.Dll output_name file_list "" in
  if not (exitcode = 0)
  then raise(Error(Linking_error exitcode))

let link_shared ~ppf_dump objfiles output_name =
  Profile.record_call output_name (fun () ->
    let genfns = Cmm_helpers.Generic_fns_tbl.make () in
    let ml_objfiles, units_tolink =
      List.fold_right (scan_file ~shared:true genfns) objfiles ([],[]) in
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
    let objfiles = List.rev ml_objfiles @ List.rev !Clflags.ccobjs in
    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = output_name ^ ".startup" ^ ext_obj in
    Asmgen.compile_unit ~output_prefix:output_name
      ~asm_filename:startup ~keep_asm:!Clflags.keep_startup_file
      ~obj_filename:startup_obj
      ~may_reduce_heap:true
      (fun () ->
         make_shared_startup_file ~ppf_dump genfns units_tolink
      );
    call_linker_shared (startup_obj :: objfiles) output_name;
    remove_file startup_obj
  )

let call_linker file_list_rev startup_file output_name =
  let main_dll = !Clflags.output_c_object
                 && Filename.check_suffix output_name Config.ext_dll
  and main_obj_runtime = !Clflags.output_complete_object
  in
  let files = startup_file :: (List.rev file_list_rev) in
  let files, c_lib =
    if (not !Clflags.output_c_object) || main_dll || main_obj_runtime then
      files @ (List.rev !Clflags.ccobjs) @ runtime_lib (),
      (if !Clflags.nopervasives || (main_obj_runtime && not main_dll)
       then "" else Config.native_c_libraries)
    else
      files, ""
  in
  let mode =
    if main_dll then Ccomp.MainDll
    else if !Clflags.output_c_object then Ccomp.Partial
    else Ccomp.Exe
  in
  let exitcode = Ccomp.call_linker mode output_name files c_lib in
  if not (exitcode = 0)
  then raise(Error(Linking_error exitcode))

let reset () =
  Cmi_consistbl.clear crc_interfaces;
  Cmx_consistbl.clear crc_implementations;
  String.Tbl.reset implementations_defined;
  cmx_required := [];
  String.Tbl.reset interfaces;
  implementations := [];
  lib_ccobjs := [];
  lib_ccopts := []

(* Main entry point *)

let link ~ppf_dump objfiles output_name =
  Profile.record_call output_name (fun () ->
    let stdlib = "stdlib.cmxa" in
    let stdexit = "std_exit.cmx" in
    let objfiles =
      if !Clflags.nopervasives then objfiles
      else if !Clflags.output_c_object then stdlib :: objfiles
      else stdlib :: (objfiles @ [stdexit]) in
    let genfns = Cmm_helpers.Generic_fns_tbl.make () in
    let ml_objfiles, units_tolink =
      List.fold_right (scan_file ~shared:false genfns) objfiles ([],[]) in
    Array.iter remove_required Runtimedef.builtin_exceptions;
    begin match extract_missing_globals() with
      [] -> ()
    | mg -> raise(Error(Missing_implementations mg))
    end;
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
                                                 (* put user's opts first *)
    let startup =
      if !Clflags.keep_startup_file || !Emitaux.binary_backend_available
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = Filename.temp_file "camlstartup" ext_obj in
    Asmgen.compile_unit ~output_prefix:output_name
      ~asm_filename:startup ~keep_asm:!Clflags.keep_startup_file
      ~obj_filename:startup_obj
      ~may_reduce_heap:true
      (fun () -> make_startup_file ~ppf_dump ~filename:startup genfns units_tolink);
    Emitaux.reduce_heap_size ~reset:(fun () -> reset ());
    Misc.try_finally
      (fun () -> call_linker ml_objfiles startup_obj output_name)
      ~always:(fun () -> remove_file startup_obj)
  )

(* Exported version for Asmlibrarian / Asmpackager *)
let check_consistency file_name u crc =
  let unit =
    { file_name;
      name = u.ui_name;
      symbol = u.ui_symbol;
      defines = u.ui_defines;
      crc;
      dynunit = None }
  in
  check_consistency ~unit
    (Array.of_list u.ui_imports_cmi) (Array.of_list u.ui_imports_cmx)

(* Error report *)

open Format

let report_error ppf = function
  | File_not_found name ->
      fprintf ppf "Cannot find file %s" name
  | Not_an_object_file name ->
      fprintf ppf "The file %a is not a compilation unit description"
        Location.print_filename name
  | Missing_implementations l ->
     let print_references ppf = function
       | [] -> ()
       | r1 :: rl ->
           fprintf ppf "%s" r1;
           List.iter (fun r -> fprintf ppf ",@ %s" r) rl in
      let print_modules ppf =
        List.iter
         (fun (md, rq) ->
            fprintf ppf "@ @[<hov 2>%s referenced from %a@]" md
            print_references rq) in
      fprintf ppf
       "@[<v 2>No implementations provided for the following modules:%a@]"
       print_modules l
  | Inconsistent_interface(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over interface %s@]"
       Location.print_filename file1
       Location.print_filename file2
       intf
  | Inconsistent_implementation(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over implementation %s@]"
       Location.print_filename file1
       Location.print_filename file2
       intf
  | Assembler_error file ->
      fprintf ppf "Error while assembling %a" Location.print_filename file
  | Linking_error exitcode ->
      fprintf ppf "Error during linking (exit code %d)" exitcode
  | Multiple_definition(modname, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ both define a module named %s@]"
        Location.print_filename file1
        Location.print_filename file2
        modname
  | Missing_cmx(filename, name) ->
      fprintf ppf
        "@[<hov>File %a@ was compiled without access@ \
         to the .cmx file@ for module %s,@ \
         which was produced by `ocamlopt -for-pack'.@ \
         Please recompile %a@ with the correct `-I' option@ \
         so that %s.cmx@ is found.@]"
        Location.print_filename filename name
        Location.print_filename  filename
        name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
