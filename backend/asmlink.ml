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
module CU = Compilation_unit

type error =
  | File_not_found of filepath
  | Not_an_object_file of filepath
  | Missing_implementations of (CU.t * string list) list
  | Inconsistent_interface of CU.Name.t * filepath * filepath
  | Inconsistent_implementation of CU.t * filepath * filepath
  | Assembler_error of filepath
  | Linking_error of int
  | Multiple_definition of CU.Name.t * filepath * filepath
  | Missing_cmx of filepath * CU.t

exception Error of error

type unit_link_info = {
  name: Compilation_unit.t;
  defines: Compilation_unit.t list;
  file_name: string;
  crc: Digest.t;
  (* for shared libs *)
  dynunit : Cmxs_format.dynunit option;
}

(* Consistency check between interfaces and implementations *)

module Cmi_consistbl = Consistbl.Make (CU.Name) (Import_info.Intf.Nonalias.Kind)
let crc_interfaces = Cmi_consistbl.create ()
let interfaces = CU.Name.Tbl.create 100

module Cmx_consistbl = Consistbl.Make (CU) (Unit)
let crc_implementations = Cmx_consistbl.create ()
let implementations = ref ([] : CU.t list)
let implementations_defined = CU.Tbl.create 100
let cmx_required = ref ([] : CU.t list)

let check_cmi_consistency file_name cmis =
  try
    List.iter
      (fun import ->
        let name = Import_info.name import in
        let info = Import_info.Intf.info import in
        CU.Name.Tbl.replace interfaces name ();
        match info with
          None -> ()
        | Some (kind, crc) ->
            Cmi_consistbl.check crc_interfaces name kind crc file_name)
      cmis
  with Cmi_consistbl.Inconsistency {
      unit_name = name;
      inconsistent_source = user;
      original_source = auth;
    } ->
    raise(Error(Inconsistent_interface(name, user, auth)))

let check_cmx_consistency file_name cmxs =
  try
    List.iter
      (fun import ->
        let name = Import_info.cu import in
        let crco = Import_info.crc import in
        implementations := name :: !implementations;
        match crco with
            None ->
              if List.mem name !cmx_required then
                raise(Error(Missing_cmx(file_name, name)))
          | Some crc ->
              Cmx_consistbl.check crc_implementations name () crc file_name)
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
  let ui_unit = CU.name unit.name in
  begin try
    let source = CU.Tbl.find implementations_defined unit.name in
    raise (Error(Multiple_definition(ui_unit, unit.file_name, source)))
  with Not_found -> ()
  end;
  implementations := unit.name :: !implementations;
  Cmx_consistbl.check crc_implementations unit.name () unit.crc unit.file_name;
  CU.Tbl.replace implementations_defined unit.name unit.file_name;
  if CU.is_packed unit.name then
    cmx_required := unit.name :: !cmx_required

let extract_crc_interfaces () =
  CU.Name.Tbl.fold (fun name () crcs ->
      let crc_with_unit = Cmi_consistbl.find crc_interfaces name in
      Import_info.Intf.create name crc_with_unit :: crcs)
    interfaces
    []

let extract_crc_implementations () =
  Cmx_consistbl.extract !implementations crc_implementations
  |> List.map (fun (cu, crc) ->
       let crc = Option.map (fun ((), crc) -> crc) crc in
       Import_info.create_normal cu ~crc)

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
  let variant =
    if Config.runtime5 && !Clflags.runtime_variant = "nnp" then ""
    else !Clflags.runtime_variant
  in
  let libname = "libasmrun" ^ variant ^ ext_lib in
  try
    if !Clflags.nopervasives || not !Clflags.with_runtime then []
    else [ Load_path.find libname ]
  with Not_found ->
    raise(Error(File_not_found libname))

(* First pass: determine which units are needed *)

let missing_globals =
  (Hashtbl.create 17 :
     (CU.t, (string * CU.Name.t option) list ref) Hashtbl.t)

let is_required name =
  try ignore (Hashtbl.find missing_globals name); true
  with Not_found -> false

let add_required by import =
  let name = Import_info.cu import in
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
    | file, Some part -> Format.asprintf "%s(%a)" file CU.Name.print part
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
    let (info, crc) = read_unit_info ~filename:file_name in
    Unit (file_name,info,crc)
  end
  else if Filename.check_suffix file_name ".cmxa" then begin
    let infos =
      try read_library_info ~filename:file_name
      with Cmx_format.Error(Not_a_unit_info _) ->
        raise(Error(Not_an_object_file file_name))
    in
    Library (file_name,infos)
  end
  else raise(Error(Not_an_object_file file_name))

let assume_no_prefix modname =
  (* We're the linker, so we assume that everything's already been packed, so
     no module needs its prefix considered. *)
  CU.create CU.Prefix.empty modname

let scan_file ~shared genfns file (objfiles, tolink, cached_genfns_imports) =
  match read_file file with
  | Unit (file_name,info,crc) ->
      (* This is a .cmx file. It must be linked in any case. *)
      remove_required info.ui_unit;
      List.iter (fun import ->
          add_required (file_name, None) import)
        info.ui_imports_cmx;
      let dynunit : Cmxs_format.dynunit option =
        if not shared then None else
          Some { dynu_name = info.ui_unit;
                 dynu_crc = crc;
                 dynu_defines = info.ui_defines;
                 dynu_imports_cmi = info.ui_imports_cmi |> Array.of_list;
                 dynu_imports_cmx = info.ui_imports_cmx |> Array.of_list }
      in
      let unit =
        { name = info.ui_unit;
          crc;
          defines = info.ui_defines;
          file_name;
          dynunit }
      in
      let object_file_name =
        Filename.chop_suffix file_name ".cmx" ^ ext_obj in
      check_consistency ~unit info.ui_imports_cmi info.ui_imports_cmx;
      let cached_genfns_imports =
        Generic_fns.Tbl.add ~imports:cached_genfns_imports genfns info.ui_generic_fns
      in
      object_file_name :: objfiles, unit :: tolink, cached_genfns_imports
  | Library (file_name,infos) ->
      (* This is an archive file. Each unit contained in it will be linked
         in only if needed. *)
      add_ccobjs (Filename.dirname file_name) infos;
      let cached_genfns_imports =
        Generic_fns.Tbl.add ~imports:cached_genfns_imports  genfns infos.lib_generic_fns
      in
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
        (fun (info, crc) reqd ->
           let li_name = CU.name info.ui_unit in
           if info.ui_force_link
           || !Clflags.link_everything
           || is_required info.ui_unit
           then begin
             remove_required info.ui_unit;
             let req_by = (file_name, Some li_name) in
             info.ui_imports_cmx |> List.iter (fun import ->
               add_required req_by import);
             let dynunit : Cmxs_format.dynunit option =
               if not shared then None else
                 Some {
                   dynu_name = info.ui_unit;
                   dynu_crc = crc;
                   dynu_defines = info.ui_defines;
                   dynu_imports_cmi = info.ui_imports_cmi |> Array.of_list;
                   dynu_imports_cmx = info.ui_imports_cmx |> Array.of_list }
             in
             let unit =
               { name = info.ui_unit;
                 crc;
                 defines = info.ui_defines;
                 file_name;
                 dynunit }
             in
             check_consistency ~unit [] [];
             unit :: reqd
           end else
           reqd)
        infos.lib_units tolink, cached_genfns_imports

(* Second pass: generate the startup file and link it with everything else *)

let named_startup_file () =
  !Clflags.keep_startup_file || !Emitaux.binary_backend_available

let force_linking_of_startup ~ppf_dump =
  Asmgen.compile_phrase ~ppf_dump
    (Cmm.Cdata ([Cmm.Csymbol_address (Cmm.global_symbol "caml_startup")]))

let make_globals_map units_list =
  (* The order in which entries appear in the globals map does not matter
     (see the natdynlink code).
     We can corrupt [interfaces] since it won't be used again until the next
     compilation. *)
  let find_crc name =
    Cmi_consistbl.find crc_interfaces name
    |> Option.map (fun (_unit, crc) -> crc)
  in
  let defined =
    List.map (fun unit ->
        let name = CU.name unit.name in
        let intf_crc = find_crc name in
        CU.Name.Tbl.remove interfaces name;
        let syms = List.map Symbol.for_compilation_unit unit.defines in
        (unit.name, intf_crc, Some unit.crc, syms))
      units_list
  in
  CU.Name.Tbl.fold (fun name () globals_map ->
      let intf_crc = find_crc name in
      (assume_no_prefix name, intf_crc, None, []) :: globals_map)
    interfaces
    defined

let sourcefile_for_dwarf ~named_startup_file filename =
  (* Ensure the name emitted into the DWARF is stable, for build
     reproducibility purposes. *)
  if named_startup_file then filename
  else ".startup"

let make_startup_file unix ~ppf_dump ~sourcefile_for_dwarf genfns units cached_gen =
  Location.input_name := "caml_startup"; (* set name of "current" input *)
  let startup_comp_unit =
    CU.create CU.Prefix.empty (CU.Name.of_string "_startup")
  in
  Compilenv.reset startup_comp_unit;
  Emitaux.Dwarf_helpers.init ~disable_dwarf:(not !Dwarf_flags.dwarf_for_startup_file)
    sourcefile_for_dwarf;
  Emit.begin_assembly unix;
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  let name_list =
    List.flatten (List.map (fun u -> u.defines) units) in
  List.iter compile_phrase (Cmm_helpers.entry_point name_list);
  List.iter compile_phrase
    (* Emit the GC roots table, for dynlink. *)
    (Cmm_helpers.emit_gc_roots_table ~symbols:[]
      (Generic_fns.compile ~shared:false genfns));
  Array.iteri
    (fun i name -> compile_phrase (Cmm_helpers.predef_exception i name))
    Runtimedef.builtin_exceptions;
  compile_phrase (Cmm_helpers.global_table name_list);
  let globals_map = make_globals_map units in
  compile_phrase (Cmm_helpers.globals_map globals_map);
  compile_phrase
    (Cmm_helpers.data_segment_table (startup_comp_unit :: name_list));
  (* CR mshinwell: We should have a separate notion of "backend compilation
     unit" really, since the units here don't correspond to .ml source
     files. *)
  let hot_comp_unit = CU.create CU.Prefix.empty (CU.Name.of_string "_hot") in
  let system_comp_unit =
    CU.create CU.Prefix.empty (CU.Name.of_string "_system") in
  let code_comp_units =
    if !Clflags.function_sections then
      hot_comp_unit :: startup_comp_unit :: name_list
    else
      startup_comp_unit :: name_list
  in
  let code_comp_units =
    if !Flambda_backend_flags.use_cached_generic_functions then
      Generic_fns.imported_units cached_gen @ code_comp_units
    else code_comp_units
  in
  compile_phrase (Cmm_helpers.code_segment_table code_comp_units);
  let all_comp_units = startup_comp_unit :: system_comp_unit :: name_list in
  let all_comp_units =
    if !Flambda_backend_flags.use_cached_generic_functions then
      Generic_fns.imported_units cached_gen @ all_comp_units
    else
      all_comp_units
  in
  compile_phrase (Cmm_helpers.frame_table all_comp_units);
  if !Clflags.output_complete_object then
    force_linking_of_startup ~ppf_dump;
  Emit.end_assembly ()

let make_shared_startup_file unix ~ppf_dump ~sourcefile_for_dwarf genfns units =
  let compile_phrase p = Asmgen.compile_phrase ~ppf_dump p in
  Location.input_name := "caml_startup";
  let shared_startup_comp_unit =
    CU.create CU.Prefix.empty (CU.Name.of_string "_shared_startup")
  in
  Compilenv.reset shared_startup_comp_unit;
  Emitaux.Dwarf_helpers.init ~disable_dwarf:(not !Dwarf_flags.dwarf_for_startup_file)
    sourcefile_for_dwarf;
  Emit.begin_assembly unix;
  List.iter compile_phrase
    (Cmm_helpers.emit_gc_roots_table ~symbols:[]
      (Generic_fns.compile ~shared:true genfns));
  let dynunits = List.map (fun u -> Option.get u.dynunit) units in
  compile_phrase (Cmm_helpers.plugin_header dynunits);
  compile_phrase
    (Cmm_helpers.global_table (List.map (fun unit -> unit.name) units));
  if !Clflags.output_complete_object then
    force_linking_of_startup ~ppf_dump;
  (* this is to force a reference to all units, otherwise the linker
     might drop some of them (in case of libraries) *)
  Emit.end_assembly ()

let call_linker_shared ?(native_toplevel = false) file_list output_name =
  let exitcode =
    Ccomp.call_linker ~native_toplevel Ccomp.Dll output_name file_list ""
  in
  if not (exitcode = 0)
  then raise(Error(Linking_error exitcode))

let link_shared unix ~ppf_dump objfiles output_name =
  Profile.record_call output_name (fun () ->
    if !Flambda_backend_flags.use_cached_generic_functions then
      (* When doing shared linking do not use the shared generated startup file.
         Frametables for the imported functions needs to be initialized, which is a bit
         tricky to do in the context of shared libraries as the frametables are
         initialized at runtime. *)
      Flambda_backend_flags.use_cached_generic_functions := false;
    if !Flambda_backend_flags.internal_assembler then
      (* CR-soon gyorsh: workaround to turn off internal assembler temporarily,
         until it is properly tested for shared library linking. *)
      Emitaux.binary_backend_available := false;
    let genfns = Generic_fns.Tbl.make () in
    let ml_objfiles, units_tolink, _ =
      List.fold_right
        (scan_file ~shared:true genfns)
        objfiles
        ([],[], Generic_fns.Partition.Set.empty)
    in
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
    let objfiles = List.rev ml_objfiles @ List.rev !Clflags.ccobjs in
    let named_startup_file = named_startup_file () in
    let startup =
      if named_startup_file
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let startup_obj = output_name ^ ".startup" ^ ext_obj in
    let sourcefile_for_dwarf = sourcefile_for_dwarf ~named_startup_file startup in
    Asmgen.compile_unit ~output_prefix:output_name
      ~asm_filename:startup ~keep_asm:!Clflags.keep_startup_file
      ~obj_filename:startup_obj
      ~may_reduce_heap:true
      ~ppf_dump
      (fun () ->
         make_shared_startup_file unix ~ppf_dump ~sourcefile_for_dwarf
           genfns units_tolink
      );
    call_linker_shared (startup_obj :: objfiles) output_name;
    if !Flambda_backend_flags.internal_assembler then
      (* CR gyorsh: restore after workaround. *)
      Emitaux.binary_backend_available := true;
    remove_file startup_obj
  )

let call_linker file_list_rev startup_file output_name =
  let main_dll = !Clflags.output_c_object
                 && Filename.check_suffix output_name Config.ext_dll
  and main_obj_runtime = !Clflags.output_complete_object
  in
  let file_list_rev =
    if !Flambda_backend_flags.use_cached_generic_functions then
      !Flambda_backend_flags.cached_generic_functions_path :: file_list_rev
    else file_list_rev
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
  CU.Tbl.reset implementations_defined;
  cmx_required := [];
  CU.Name.Tbl.reset interfaces;
  implementations := [];
  lib_ccobjs := [];
  lib_ccopts := []

(* Main entry point *)

let link unix ~ppf_dump objfiles output_name =
  if !Flambda_backend_flags.internal_assembler then
      Emitaux.binary_backend_available := true;
  Profile.record_call output_name (fun () ->
    let stdlib = "stdlib.cmxa" in
    let stdexit = "std_exit.cmx" in
    let objfiles =
      if !Clflags.nopervasives then objfiles
      else if !Clflags.output_c_object then stdlib :: objfiles
      else stdlib :: (objfiles @ [stdexit]) in
    let genfns = Generic_fns.Tbl.make () in
    let ml_objfiles, units_tolink, cached_genfns_imports =
      List.fold_right
        (scan_file ~shared:false genfns)
        objfiles
        ([],[], Generic_fns.Partition.Set.empty)
    in
    begin match extract_missing_globals() with
      [] -> ()
    | mg -> raise(Error(Missing_implementations mg))
    end;
    Clflags.ccobjs := !Clflags.ccobjs @ !lib_ccobjs;
    Clflags.all_ccopts := !lib_ccopts @ !Clflags.all_ccopts;
                                                 (* put user's opts first *)
    let named_startup_file = named_startup_file () in
    let startup =
      if named_startup_file
      then output_name ^ ".startup" ^ ext_asm
      else Filename.temp_file "camlstartup" ext_asm in
    let sourcefile_for_dwarf = sourcefile_for_dwarf ~named_startup_file startup in
    let startup_obj = Filename.temp_file "camlstartup" ext_obj in
    Asmgen.compile_unit ~output_prefix:output_name
      ~asm_filename:startup ~keep_asm:!Clflags.keep_startup_file
      ~obj_filename:startup_obj
      ~may_reduce_heap:true
      ~ppf_dump
      (fun () -> make_startup_file unix ~ppf_dump
                   ~sourcefile_for_dwarf genfns units_tolink cached_genfns_imports);
    Emitaux.reduce_heap_size ~reset:(fun () -> reset ());
    Misc.try_finally
      (fun () -> call_linker ml_objfiles startup_obj output_name)
      ~always:(fun () -> remove_file startup_obj)
  )

(* Exported version for Asmlibrarian / Asmpackager *)
let check_consistency file_name u crc =
  let unit =
    { file_name;
      name = u.ui_unit;
      defines = u.ui_defines;
      crc;
      dynunit = None }
  in
  check_consistency ~unit u.ui_imports_cmi u.ui_imports_cmx

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
            fprintf ppf "@ @[<hov 2>%a referenced from %a@]"
            CU.print md
            print_references rq) in
      fprintf ppf
       "@[<v 2>No implementations provided for the following modules:%a@]"
       print_modules l
  | Inconsistent_interface(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over interface %a@]"
       Location.print_filename file1
       Location.print_filename file2
       CU.Name.print intf
  | Inconsistent_implementation(intf, file1, file2) ->
      fprintf ppf
       "@[<hov>Files %a@ and %a@ make inconsistent assumptions \
              over implementation %a@]"
       Location.print_filename file1
       Location.print_filename file2
       CU.print intf
  | Assembler_error file ->
      fprintf ppf "Error while assembling %a" Location.print_filename file
  | Linking_error exitcode ->
      fprintf ppf "Error during linking (exit code %d)" exitcode
  | Multiple_definition(modname, file1, file2) ->
      fprintf ppf
        "@[<hov>Files %a@ and %a@ both define a module named %a@]"
        Location.print_filename file1
        Location.print_filename file2
        CU.Name.print modname
  | Missing_cmx(filename, name) ->
      fprintf ppf
        "@[<hov>File %a@ was compiled without access@ \
         to the .cmx file@ for module %a,@ \
         which was produced by `ocamlopt -for-pack'.@ \
         Please recompile %a@ with the correct `-I' option@ \
         so that %a.cmx@ is found.@]"
        Location.print_filename filename
        CU.print name
        Location.print_filename filename
        CU.print name

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
