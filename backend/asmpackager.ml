(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* "Package" a set of .cmx/.o files into one .cmx/.o file having the
   original compilation units as sub-modules. *)

open Misc
open Cmx_format

module CU = Compilation_unit

type error =
    Illegal_renaming of CU.Name.t * string * CU.Name.t
  | Forward_reference of string * CU.Name.t
  | Wrong_for_pack of string * CU.t
  | Linking_error
  | Assembler_error of string
  | File_not_found of string


exception Error of error

(* Read the unit information from a .cmx file. *)

type pack_member_kind = PM_intf | PM_impl of unit_infos

type pack_member =
  { pm_file: string;
    pm_name: CU.Name.t;
    pm_kind: pack_member_kind }

let read_member_info pack_path file = (
  let name =
    String.capitalize_ascii(Filename.basename(chop_extensions file))
    |> CU.Name.of_string in
  let kind =
    if Filename.check_suffix file ".cmi" then
      PM_intf
    else begin
      let (info, crc) = Compilenv.read_unit_info file in
      if not (CU.Name.equal (CU.name info.ui_unit) name)
      then raise(Error(Illegal_renaming(name, file, (CU.name info.ui_unit))));
      if not (CU.is_parent pack_path ~child:info.ui_unit)
      then raise(Error(Wrong_for_pack(file, pack_path)));
      Asmlink.check_consistency file info crc;
      Compilenv.cache_unit_info info;
      PM_impl info
    end in
  { pm_file = file; pm_name = name; pm_kind = kind }
)

(* Check absence of forward references *)

let check_units members =
  let rec check forbidden = function
    [] -> ()
  | mb :: tl ->
      begin match mb.pm_kind with
      | PM_intf -> ()
      | PM_impl infos ->
          List.iter
            (fun import ->
              let unit = Import_info.cu import in
              let name = CU.name unit in
              if List.mem name forbidden
              then raise(Error(Forward_reference(mb.pm_file, name))))
            infos.ui_imports_cmx
      end;
      check (list_remove mb.pm_name forbidden) tl in
  check (List.map (fun mb -> mb.pm_name) members) members

(* Make the .o file for the package *)

type flambda2 =
  ppf_dump:Format.formatter ->
  prefixname:string ->
  filename:string ->
  keep_symbol_tables:bool ->
  Lambda.program ->
  Cmm.phrase list

let make_package_object unix ~ppf_dump members targetobj targetname coercion
      ~(flambda2 : flambda2) =
  Profile.record_call (Printf.sprintf "pack(%s)" targetname) (fun () ->
    let objtemp =
      if !Clflags.keep_asm_file
      then Filename.remove_extension targetobj ^ ".pack" ^ Config.ext_obj
      else
        (* Put the full name of the module in the temporary file name
           to avoid collisions with MSVC's link /lib in case of successive
           packs *)
        let name =
          Symbol.for_current_unit ()
          |> Symbol.linkage_name
          |> Linkage_name.to_string
        in
        Filename.temp_file name Config.ext_obj in
    let components =
      List.map
        (fun m ->
          match m.pm_kind with
          | PM_intf -> None
          | PM_impl _ -> Some(CU.create_child (CU.get_current_exn ()) m.pm_name))
        members in
    let for_pack_prefix = CU.Prefix.from_clflags () in
    let modname = CU.Name.of_string targetname in
    let compilation_unit = CU.create for_pack_prefix modname in
    let prefixname = Filename.remove_extension objtemp in
    let required_globals = Compilation_unit.Set.empty in
    let transl_style : Translmod.compilation_unit_style =
      if Config.flambda || Config.flambda2 then Plain_block
      else Set_individual_fields
    in
    let main_module_block_size, code =
      Translmod.transl_package components compilation_unit coercion
        ~style:transl_style
    in
    let code = Simplif.simplify_lambda code in
    let program =
      { Lambda.
        code;
        main_module_block_size;
        compilation_unit;
        required_globals;
      }
    in
    let pipeline : Asmgen.pipeline =
      Direct_to_cmm (flambda2 ~keep_symbol_tables:true)
    in
    Asmgen.compile_implementation ~pipeline unix
      ~filename:targetname
      ~prefixname
      ~ppf_dump
      program;
    let objfiles =
      List.map
        (fun m -> Filename.remove_extension m.pm_file ^ Config.ext_obj)
        (List.filter (fun m -> m.pm_kind <> PM_intf) members) in
    let exitcode =
      Ccomp.call_linker Ccomp.Partial targetobj (objtemp :: objfiles) ""
    in
    remove_file objtemp;
    if not (exitcode = 0) then raise(Error Linking_error)
  )

(* Make the .cmx file for the package *)

let build_package_cmx members cmxfile =
  let unit_names =
    List.map (fun m -> m.pm_name) members in
  let filter lst =
    List.filter (fun import ->
      not (List.mem (Import_info.name import) unit_names)) lst in
  let union lst =
    List.fold_left
      (List.fold_left
          (fun accu n -> if List.mem n accu then accu else n :: accu))
      [] lst in
  let units =
    List.fold_right
      (fun m accu ->
        match m.pm_kind with PM_intf -> accu | PM_impl info -> info :: accu)
      members [] in
  let ui = Compilenv.current_unit_infos() in
  let ui_export_info =
    List.fold_left (fun acc info ->
        Flambda2_cmx.Flambda_cmx_format.merge info.ui_export_info acc)
      ui.ui_export_info
      units
  in
  let ui_zero_alloc_info = Zero_alloc_info.create () in
  List.iter (fun info -> Zero_alloc_info.merge info.ui_zero_alloc_info
                           ~into:ui_zero_alloc_info) units;
  let modname = Compilation_unit.name ui.ui_unit in
  let pkg_infos =
    { ui_unit = ui.ui_unit;
      ui_defines =
          List.flatten (List.map (fun info -> info.ui_defines) units) @
          [ui.ui_unit];
      ui_imports_cmi =
          (Import_info.create modname
            ~crc_with_unit:(Some (ui.ui_unit, Env.crc_of_unit modname))) ::
            filter (Asmlink.extract_crc_interfaces ());
      ui_imports_cmx =
          filter(Asmlink.extract_crc_implementations());
      ui_generic_fns =
        { curry_fun =
            union(List.map (fun info -> info.ui_generic_fns.curry_fun) units);
          apply_fun =
            union(List.map (fun info -> info.ui_generic_fns.apply_fun) units);
          send_fun =
            union(List.map (fun info -> info.ui_generic_fns.send_fun) units) };
      ui_force_link =
          List.exists (fun info -> info.ui_force_link) units;
      ui_export_info;
      ui_zero_alloc_info;
      ui_external_symbols = union (List.map (fun info -> info.ui_external_symbols) units);
    } in
  Compilenv.write_unit_info pkg_infos cmxfile

(* Make the .cmx and the .o for the package *)

let package_object_files unix ~ppf_dump files targetcmx
                         targetobj targetname coercion ~flambda2 =
  let pack_path =
    let for_pack_prefix = CU.Prefix.from_clflags () in
    let name = targetname |> CU.Name.of_string in
    CU.create for_pack_prefix name
  in
  let members = map_left_right (read_member_info pack_path) files in
  check_units members;
  make_package_object unix ~ppf_dump members targetobj targetname coercion
    ~flambda2;
  build_package_cmx members targetcmx

(* The entry point *)

let package_files unix ~ppf_dump initial_env files targetcmx ~flambda2 =
  let files =
    List.map
      (fun f ->
        try Load_path.find f
        with Not_found -> raise(Error(File_not_found f)))
      files in
  let prefix = chop_extensions targetcmx in
  let targetcmi = prefix ^ ".cmi" in
  let targetobj = Filename.remove_extension targetcmx ^ Config.ext_obj in
  let targetname = String.capitalize_ascii(Filename.basename prefix) in
  (* Set the name of the current "input" *)
  Location.input_name := targetcmx;
  (* Set the name of the current compunit *)
  let comp_unit =
    let for_pack_prefix = CU.Prefix.from_clflags () in
    CU.create for_pack_prefix (CU.Name.of_string targetname)
  in
  Compilenv.reset comp_unit;
  Misc.try_finally (fun () ->
      let coercion =
        Typemod.package_units initial_env files targetcmi comp_unit in
      package_object_files unix ~ppf_dump files targetcmx targetobj targetname
        coercion ~flambda2
    )
    ~exceptionally:(fun () -> remove_file targetcmx; remove_file targetobj)

(* Error report *)

open Format

let report_error ppf = function
    Illegal_renaming(name, file, id) ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %a when %a was expected"
        Location.print_filename file CU.Name.print name CU.Name.print id
  | Forward_reference(file, ident) ->
      fprintf ppf "Forward reference to %a in file %a" CU.Name.print ident
        Location.print_filename file
  | Wrong_for_pack(file, path) ->
      fprintf ppf "File %a@ was not compiled with the `-for-pack %a' option"
        Location.print_filename file Compilation_unit.print path
  | File_not_found file ->
      fprintf ppf "File %s not found" file
  | Assembler_error file ->
      fprintf ppf "Error while assembling %s" file
  | Linking_error ->
      fprintf ppf "Error during partial linking"

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
