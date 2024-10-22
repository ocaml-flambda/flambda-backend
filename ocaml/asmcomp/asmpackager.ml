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
<<<<<<< HEAD
  let name =
    String.capitalize_ascii(Filename.basename(chop_extensions file))
    |> CU.Name.of_string in
||||||| 121bedcfd2
  let name =
    String.capitalize_ascii(Filename.basename(chop_extensions file)) in
=======
  let unit_info = Unit_info.Artifact.from_filename file in
  let name = Unit_info.Artifact.modname unit_info in
>>>>>>> 5.2.0
  let kind =
    if Unit_info.is_cmi unit_info  then
      PM_intf
    else begin
      let (info, crc) = Compilenv.read_unit_info file in
<<<<<<< HEAD
      if not (CU.Name.equal (CU.name info.ui_unit) name)
      then raise(Error(Illegal_renaming(name, file, (CU.name info.ui_unit))));
      if not (CU.is_parent pack_path ~child:info.ui_unit)
||||||| 121bedcfd2
      if info.ui_name <> name
      then raise(Error(Illegal_renaming(name, file, info.ui_name)));
      if info.ui_symbol <>
         (Compilenv.current_unit_infos()).ui_symbol ^ "." ^ info.ui_name
=======
      if info.ui_name <> name
      then raise(Error(Illegal_renaming(name, file, info.ui_name)));
      let expected_symbol =
        Printf.sprintf "%s%c%s"
          (Compilenv.current_unit_infos()).ui_symbol Compilenv.symbol_separator
          info.ui_name
      in
      if info.ui_symbol <> expected_symbol
>>>>>>> 5.2.0
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
          Array.iter
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

let make_package_object ~ppf_dump members target coercion ~backend =
  let pack_name =
    Printf.sprintf "pack(%s)" (Unit_info.Artifact.modname target) in
  Profile.record_call pack_name (fun () ->
    let objtemp =
      if !Clflags.keep_asm_file
      then Unit_info.Artifact.prefix target ^ ".pack" ^ Config.ext_obj
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
<<<<<<< HEAD
    let for_pack_prefix = CU.Prefix.from_clflags () in
    let modname = targetname |> CU.Name.of_string in
    let compilation_unit = CU.create for_pack_prefix modname in
||||||| 121bedcfd2
    let module_ident = Ident.create_persistent targetname in
=======
    let module_ident =
      Ident.create_persistent @@ Unit_info.Artifact.modname target in
>>>>>>> 5.2.0
    let prefixname = Filename.remove_extension objtemp in
<<<<<<< HEAD
    let required_globals = CU.Set.empty in
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
    let middle_end =
      if Config.flambda then Flambda_middle_end.lambda_to_clambda
      else Closure_middle_end.lambda_to_clambda
||||||| 121bedcfd2
    let required_globals = Ident.Set.empty in
    let program, middle_end =
      if Config.flambda then
        let main_module_block_size, code =
          Translmod.transl_package_flambda components coercion
        in
        let code = Simplif.simplify_lambda code in
        let program =
          { Lambda.
            code;
            main_module_block_size;
            module_ident;
            required_globals;
          }
        in
        program, Flambda_middle_end.lambda_to_clambda
      else
        let main_module_block_size, code =
          Translmod.transl_store_package components
            (Ident.create_persistent targetname) coercion
        in
        let code = Simplif.simplify_lambda code in
        let program =
          { Lambda.
            code;
            main_module_block_size;
            module_ident;
            required_globals;
          }
        in
        program, Closure_middle_end.lambda_to_clambda
=======
    let required_globals = Ident.Set.empty in
    let program, middle_end =
      if Config.flambda then
        let main_module_block_size, code =
          Translmod.transl_package_flambda components coercion
        in
        let code = Simplif.simplify_lambda code in
        let program =
          { Lambda.
            code;
            main_module_block_size;
            module_ident;
            required_globals;
          }
        in
        program, Flambda_middle_end.lambda_to_clambda
      else
        let main_module_block_size, code =
          Translmod.transl_store_package components module_ident coercion
        in
        let code = Simplif.simplify_lambda code in
        let program =
          { Lambda.
            code;
            main_module_block_size;
            module_ident;
            required_globals;
          }
        in
        program, Closure_middle_end.lambda_to_clambda
>>>>>>> 5.2.0
    in
    Asmgen.compile_implementation ~backend
      ~prefixname
      ~middle_end
      ~ppf_dump
      program;
    let objfiles =
      List.map
        (fun m -> Filename.remove_extension m.pm_file ^ Config.ext_obj)
        (List.filter (fun m -> m.pm_kind <> PM_intf) members) in
    let exitcode =
      Ccomp.call_linker Ccomp.Partial (Unit_info.Artifact.filename target)
        (objtemp :: objfiles) ""
    in
    remove_file objtemp;
    if not (exitcode = 0) then raise(Error Linking_error)
  )

(* Make the .cmx file for the package *)

let get_export_info ui =
  assert(Config.flambda);
  match ui.ui_export_info with
  | Clambda _ -> assert false
  | Flambda info -> info

let get_approx ui =
  assert(not Config.flambda);
  match ui.ui_export_info with
  | Flambda _ -> assert false
  | Clambda info -> info

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
    if Config.flambda then
      let ui_export_info =
        List.fold_left (fun acc info ->
            Export_info.merge acc (get_export_info info))
          (get_export_info ui)
          units
      in
      Flambda ui_export_info
    else
      Clambda (get_approx ui)
  in
  let modname = Compilation_unit.name ui.ui_unit in
  let pkg_infos =
    { ui_unit = ui.ui_unit;
      ui_defines =
          List.flatten (List.map (fun info -> info.ui_defines) units) @
          [ui.ui_unit];
      ui_imports_cmi =
          (Import_info.create modname
            ~crc_with_unit:(Some (ui.ui_unit, Env.crc_of_unit modname))) ::
            filter (Asmlink.extract_crc_interfaces ())
          |> Array.of_list;
      ui_imports_cmx =
          (filter(Asmlink.extract_crc_implementations()))
          |> Array.of_list;
      ui_curry_fun =
          union(List.map (fun info -> info.ui_curry_fun) units);
      ui_apply_fun =
          union(List.map (fun info -> info.ui_apply_fun) units);
      ui_send_fun =
          union(List.map (fun info -> info.ui_send_fun) units);
      ui_force_link =
          List.exists (fun info -> info.ui_force_link) units;
      ui_export_info;
      ui_for_pack = None;
    } in
  Compilenv.write_unit_info pkg_infos cmxfile

(* Make the .cmx and the .o for the package *)

let package_object_files ~ppf_dump files target targetcmx coercion ~backend =
  let pack_path =
<<<<<<< HEAD
    let for_pack_prefix = CU.Prefix.from_clflags () in
    let name = targetname |> CU.Name.of_string in
    CU.create for_pack_prefix name
  in
||||||| 121bedcfd2
    match !Clflags.for_package with
    | None -> targetname
    | Some p -> p ^ "." ^ targetname in
=======
    match !Clflags.for_package with
    | None -> Unit_info.Artifact.modname target
    | Some p -> p ^ "." ^ Unit_info.Artifact.modname target in
>>>>>>> 5.2.0
  let members = map_left_right (read_member_info pack_path) files in
  check_units members;
  make_package_object ~ppf_dump members target coercion ~backend;
  build_package_cmx members targetcmx

(* The entry point *)

let package_files ~ppf_dump initial_env files targetcmx ~backend =
  let files =
    List.map
      (fun f ->
        try Load_path.find f
        with Not_found -> raise(Error(File_not_found f)))
      files in
  let cmx = Unit_info.Artifact.from_filename targetcmx in
  let cmi = Unit_info.companion_cmi cmx in
  let obj = Unit_info.companion_obj cmx in
  (* Set the name of the current "input" *)
  Location.input_name := targetcmx;
  (* Set the name of the current compunit *)
<<<<<<< HEAD
  let comp_unit =
    let for_pack_prefix = CU.Prefix.from_clflags () in
    CU.create for_pack_prefix (CU.Name.of_string targetname)
  in
  Compilenv.reset comp_unit;
||||||| 121bedcfd2
  Compilenv.reset ?packname:!Clflags.for_package targetname;
=======
  Compilenv.reset ?packname:!Clflags.for_package
    (Unit_info.Artifact.modname cmi);
>>>>>>> 5.2.0
  Misc.try_finally (fun () ->
<<<<<<< HEAD
      let coercion =
        Typemod.package_units initial_env files targetcmi comp_unit in
      package_object_files ~ppf_dump files targetcmx targetobj targetname
        coercion ~backend
||||||| 121bedcfd2
      let coercion =
        Typemod.package_units initial_env files targetcmi targetname in
      package_object_files ~ppf_dump files targetcmx targetobj targetname
        coercion ~backend
=======
      let coercion = Typemod.package_units initial_env files cmi in
      package_object_files ~ppf_dump files obj targetcmx coercion ~backend
>>>>>>> 5.2.0
    )
    ~exceptionally:(fun () ->
        remove_file targetcmx; remove_file (Unit_info.Artifact.filename obj)
      )

(* Error report *)

open Format
module Style = Misc.Style

let report_error ppf = function
    Illegal_renaming(name, file, id) ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
<<<<<<< HEAD
                   @ %a when %a was expected"
        Location.print_filename file CU.Name.print name CU.Name.print id
||||||| 121bedcfd2
                   @ %s when %s was expected"
        Location.print_filename file name id
=======
                   @ %a when %a was expected"
        (Style.as_inline_code Location.print_filename) file
        Style.inline_code name Style.inline_code id
>>>>>>> 5.2.0
  | Forward_reference(file, ident) ->
<<<<<<< HEAD
      fprintf ppf "Forward reference to %a in file %a" CU.Name.print ident
        Location.print_filename file
||||||| 121bedcfd2
      fprintf ppf "Forward reference to %s in file %a" ident
        Location.print_filename file
=======
      fprintf ppf "Forward reference to %a in file %a" Style.inline_code ident
        (Style.as_inline_code Location.print_filename) file
>>>>>>> 5.2.0
  | Wrong_for_pack(file, path) ->
<<<<<<< HEAD
      fprintf ppf "File %a@ was not compiled with the `-for-pack %a' option"
        Location.print_filename file Compilation_unit.print path
||||||| 121bedcfd2
      fprintf ppf "File %a@ was not compiled with the `-for-pack %s' option"
              Location.print_filename file path
=======
      fprintf ppf "File %a@ was not compiled with the %a option"
        (Style.as_inline_code Location.print_filename) file
        Style.inline_code ("-for-pack " ^ path)
>>>>>>> 5.2.0
  | File_not_found file ->
      fprintf ppf "File %a not found" Style.inline_code file
  | Assembler_error file ->
      fprintf ppf "Error while assembling %a" Style.inline_code file
  | Linking_error ->
      fprintf ppf "Error during partial linking"

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
