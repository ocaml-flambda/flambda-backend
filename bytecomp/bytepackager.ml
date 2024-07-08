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

(* "Package" a set of .cmo files into one .cmo file having the
   original compilation units as sub-modules. *)

open Misc
open Instruct
open Cmo_format

module CU = Compilation_unit
module String = Misc.Stdlib.String
module Compunit = Symtable.Compunit

let rec rev_append_map f l rest =
  match l with
  | [] -> rest
  | x :: xs -> rev_append_map f xs (f x :: rest)

type error =
    Forward_reference of string * compunit
  | Multiple_definition of string * compunit
  | Not_an_object_file of string
<<<<<<< HEAD
  | Illegal_renaming of Compilation_unit.Name.t * string * string
||||||| 121bedcfd2
  | Illegal_renaming of string * string * string
=======
  | Illegal_renaming of compunit * string * compunit
>>>>>>> 5.2.0
  | File_not_found of string

exception Error of error

type mapped_compunit = {
  packed_modname : compunit; (** qualified name of the compilation unit *)
  processed : bool
}

let record_as_processed mapping id =
  let update_processed = function
    | Some ({ processed = false; _} as r) -> Some {r with processed=true}
    | Some {processed = true;_} | None -> assert false
  in
  Compunit.Map.update id update_processed mapping

type state = {
  relocs : (reloc_info * int) list; (** accumulated reloc info *)
  events : debug_event list;        (** accumulated debug events *)
  debug_dirs : String.Set.t;        (** accumulated debug_dirs *)
  primitives : string list;         (** accumulated primitives *)
  offset : int;                     (** offset of the current unit *)
  subst : Subst.t;                  (** Substitution for debug event *)
<<<<<<< HEAD
||||||| 121bedcfd2
  mapping : (Ident.t * bool) Ident.Map.t;
  (** Mapping from module to packed-module idents.
      The boolean tells whether we've processed the compilation unit already. *)
=======
  mapping : mapped_compunit Compunit.Map.t;
  (** Mapping from module to packed-module idents. *)
>>>>>>> 5.2.0
}

let empty_state = {
  relocs = [];
  events = [];
  debug_dirs = String.Set.empty;
  primitives = [];
  offset = 0;
<<<<<<< HEAD
||||||| 121bedcfd2
  mapping = Ident.Map.empty;
=======
  mapping = Compunit.Map.empty;
>>>>>>> 5.2.0
  subst = Subst.identity;
}

(* Record a relocation, updating its offset. *)

<<<<<<< HEAD
let rename_relocation base (rel, ofs) =
  (* Nothing to do here following the symbols patches *)
  rel, base + ofs
||||||| 121bedcfd2
let rename_relocation packagename objfile mapping base (rel, ofs) =
  let rel' =
    match rel with
      Reloc_getglobal id ->
        begin try
          let id', defined = Ident.Map.find id mapping in
          if defined
          then Reloc_getglobal id'
          else raise(Error(Forward_reference(objfile, id)))
        with Not_found ->
          (* PR#5276: unique-ize dotted global names, which appear
             if one of the units being consolidated is itself a packed
             module. *)
          let name = Ident.name id in
          if String.contains name '.' then
            Reloc_getglobal (Ident.create_persistent (packagename ^ "." ^ name))
          else
            rel
        end
    | Reloc_setglobal id ->
        begin try
          let id', defined = Ident.Map.find id mapping in
          if defined
          then raise(Error(Multiple_definition(objfile, id)))
          else Reloc_setglobal id'
        with Not_found ->
          (* PR#5276, as above *)
          let name = Ident.name id in
          if String.contains name '.' then
            Reloc_setglobal (Ident.create_persistent (packagename ^ "." ^ name))
          else
            rel
        end
    | _ ->
        rel in
  (rel', base + ofs)
=======
let rename_relocation packagename objfile mapping base (rel, ofs) =
  (* PR#5276: unique-ize dotted global names, which appear if one of
    the units being consolidated is itself a packed module. *)
  let make_compunit_name_unique cu =
    if Compunit.is_packed cu
    then Compunit (packagename ^ "." ^ (Compunit.name cu))
    else cu
  in
  let rel' =
    match rel with
      | Reloc_getcompunit cu ->
        begin try
          let mapped_modname = Compunit.Map.find cu mapping in
          if mapped_modname.processed
          then Reloc_getcompunit mapped_modname.packed_modname
          else raise(Error(Forward_reference(objfile, cu)))
        with Not_found -> Reloc_getcompunit (make_compunit_name_unique cu)
      end
    | Reloc_setcompunit cu ->
      begin try
          let mapped_modname = Compunit.Map.find cu mapping in
          if mapped_modname.processed
          then raise(Error(Multiple_definition(objfile, cu)))
          else Reloc_setcompunit mapped_modname.packed_modname
        with Not_found -> Reloc_setcompunit (make_compunit_name_unique cu)
      end
    | Reloc_literal _ | Reloc_getpredef _ | Reloc_primitive _ ->
        rel in
  (rel', base + ofs)
>>>>>>> 5.2.0

(* Record and update a debugging event *)

let relocate_debug base subst ev =
  { ev with ev_pos = base + ev.ev_pos;
            ev_typsubst = Subst.compose ev.ev_typsubst subst }

(* Read the unit information from a .cmo file. *)

type pack_member_kind = PM_intf | PM_impl of compilation_unit_descr

type pack_member =
  { pm_file: string;
<<<<<<< HEAD
    pm_name: Compilation_unit.Name.t;
||||||| 121bedcfd2
    pm_name: string;
    pm_ident: Ident.t;
    pm_packed_ident: Ident.t;
=======
    pm_name: string;
    pm_ident: compunit;
    pm_packed_ident: compunit;
>>>>>>> 5.2.0
    pm_kind: pack_member_kind }

<<<<<<< HEAD
let read_member_info file =
  let name = String.capitalize_ascii(Filename.basename(chop_extensions file)) in
  let name = Compilation_unit.Name.of_string name in
||||||| 121bedcfd2
let read_member_info targetname file =
  let name = String.capitalize_ascii(Filename.basename(chop_extensions file)) in
=======
let read_member_info targetname file =
  let member = Unit_info.Artifact.from_filename file in
  let member_name = Unit_info.Artifact.modname member in
  let member_compunit = Compunit member_name in
>>>>>>> 5.2.0
  let kind =
    (* PR#7479: make sure it is either a .cmi or a .cmo *)
    if Unit_info.is_cmi member then
      PM_intf
    else begin
      let ic = open_in_bin file in
      Fun.protect ~finally:(fun () -> close_in ic) (fun () ->
        let buffer =
          really_input_string ic (String.length Config.cmo_magic_number)
        in
        if buffer <> Config.cmo_magic_number then
          raise(Error(Not_an_object_file file));
        let compunit_pos = input_binary_int ic in
        seek_in ic compunit_pos;
<<<<<<< HEAD
        let compunit = (input_value ic : compilation_unit_descr) in
        if not (CU.Name.equal (CU.name compunit.cu_name) name)
        then raise(Error(Illegal_renaming(name, file,
          CU.name_as_string compunit.cu_name)));
||||||| 121bedcfd2
        let compunit = (input_value ic : compilation_unit) in
        if compunit.cu_name <> name
        then raise(Error(Illegal_renaming(name, file, compunit.cu_name)));
=======
        let compunit = (input_value ic : compilation_unit) in
        if compunit.cu_name <> member_compunit
        then begin
          raise(Error(Illegal_renaming
            (member_compunit, file, compunit.cu_name)))
        end;
>>>>>>> 5.2.0
        PM_impl compunit)
    end in
<<<<<<< HEAD
  { pm_file = file; pm_name = name; pm_kind = kind }
||||||| 121bedcfd2
  let pm_ident = Ident.create_persistent name in
  let pm_packed_ident = Ident.create_persistent(targetname ^ "." ^ name) in
  { pm_file = file; pm_name = name; pm_kind = kind; pm_ident; pm_packed_ident }
=======
  let pm_packed_ident = Compunit (targetname ^ "." ^ member_name) in
  { pm_file = file; pm_name = member_name; pm_kind = kind;
    pm_ident = member_compunit; pm_packed_ident }
>>>>>>> 5.2.0

(* Read the bytecode from a .cmo file.
   Write bytecode to channel [oc].
<<<<<<< HEAD
||||||| 121bedcfd2
   Rename globals as indicated by [mapping] in reloc info.
=======
   Rename compunits as indicated by [mapping] in reloc info.
>>>>>>> 5.2.0
   Accumulate relocs, debug info, etc.
   Return the accumulated state. *)

let process_append_bytecode oc state objfile compunit =
  let ic = open_in_bin objfile in
  try
    Bytelink.check_consistency objfile compunit;
    let relocs =
      rev_append_map
        (rename_relocation state.offset)
        compunit.cu_reloc
        state.relocs in
    let primitives = List.rev_append compunit.cu_primitives state.primitives in
    seek_in ic compunit.cu_pos;
    Misc.copy_file_chunk ic oc compunit.cu_codesize;
    let events, debug_dirs =
      if !Clflags.debug && compunit.cu_debug > 0 then begin
        seek_in ic compunit.cu_debug;
        let unit_events = (Compression.input_value ic : debug_event list) in
        let events =
          rev_append_map
            (relocate_debug state.offset state.subst)
            unit_events
            state.events in
        let unit_debug_dirs = (Compression.input_value ic : string list) in
        let debug_dirs =
          String.Set.union
            state.debug_dirs
            (String.Set.of_list unit_debug_dirs) in
        events, debug_dirs
      end
      else state.events, state.debug_dirs
    in
    close_in ic;
    { state with
      relocs; primitives; events; debug_dirs;
      offset = state.offset + compunit.cu_codesize;
    }
  with x ->
    close_in ic;
    raise x

(* Same, for a list of .cmo and .cmi files.
   Return the accumulated state. *)
let process_append_pack_member packagename oc state m =
  match m.pm_kind with
  | PM_intf -> state
  | PM_impl compunit ->
      let state =
        process_append_bytecode oc state m.pm_file compunit in
      let id =
        Ident.create_persistent
          (m.pm_name |> Compilation_unit.Name.to_string)
      in
      let root = Path.Pident (Ident.create_persistent packagename) in
<<<<<<< HEAD
||||||| 121bedcfd2
      let mapping = Ident.Map.update id (function
          | Some (p,false) -> Some (p,true)
          | Some (_, true) | None -> assert false) state.mapping in
=======
      let mapping = record_as_processed state.mapping id in
>>>>>>> 5.2.0
      let subst =
<<<<<<< HEAD
        Subst.add_module id (Path.Pdot (root, Ident.name id)) state.subst in
      { state with subst }
||||||| 121bedcfd2
        Subst.add_module id (Path.Pdot (root, Ident.name id)) state.subst in
      { state with subst; mapping }
=======
        let id' = Compunit.to_ident id in
        Subst.add_module id' (Path.Pdot (root, Ident.name id')) state.subst
      in
      { state with subst; mapping }
>>>>>>> 5.2.0

(* Generate the code that builds the tuple representing the package module *)

<<<<<<< HEAD
let build_global_target ~ppf_dump oc target_name state members coercion =
  let for_pack_prefix = Compilation_unit.Prefix.from_clflags () in
  let compilation_unit =
    Compilation_unit.create for_pack_prefix
      (target_name |> Compilation_unit.Name.of_string)
  in
  let unit_of_name name = Compilation_unit.create_child compilation_unit name in
  let components =
    List.map
      (fun m ->
        match m.pm_kind with
        | PM_intf -> None
        | PM_impl _ -> Some (m.pm_name |> unit_of_name))
      members in
  let _size, lam =
    Translmod.transl_package components compilation_unit coercion
      ~style:Set_global_to_block
  in
  if !Clflags.dump_rawlambda then
    Format.fprintf ppf_dump "%a@." Printlambda.lambda lam;
||||||| 121bedcfd2
let build_global_target ~ppf_dump oc target_name state components coercion =
  let lam =
    Translmod.transl_package
      components (Ident.create_persistent target_name) coercion in
=======
let build_global_target ~ppf_dump oc target_name state components coercion =
  let components =
    List.map (Option.map Compunit.to_ident) components
  in
  let lam =
    Translmod.transl_package
      components (Ident.create_persistent target_name) coercion in
>>>>>>> 5.2.0
  let lam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then
    Format.fprintf ppf_dump "%a@." Printlambda.lambda lam;
  let instrs =
    Bytegen.compile_implementation target_name lam in
  let size, pack_relocs, pack_events, pack_debug_dirs =
    Emitcode.to_packed_file oc instrs in
  let events = List.rev_append pack_events state.events in
  let debug_dirs = String.Set.union pack_debug_dirs state.debug_dirs in
  let relocs =
    rev_append_map
      (fun (r, ofs) -> (r, state.offset + ofs))
      pack_relocs state.relocs in
  { state with events; debug_dirs; relocs; offset = state.offset + size}

(* Build the .cmo file obtained by packaging the given .cmo files. *)

<<<<<<< HEAD
let package_object_files ~ppf_dump files targetfile targetname coercion =
  let members = map_left_right read_member_info files in
  let required_globals =
    List.fold_right (fun compunit required_globals -> match compunit with
||||||| 121bedcfd2
let package_object_files ~ppf_dump files targetfile targetname coercion =
  let members = map_left_right (read_member_info targetname) files in
  let required_globals =
    List.fold_right (fun compunit required_globals -> match compunit with
=======
let package_object_files ~ppf_dump files target coercion =
  let targetfile = Unit_info.Artifact.filename target in
  let targetname = Unit_info.Artifact.modname target in
  let members = map_left_right (read_member_info targetname) files in
  let required_compunits =
    List.fold_right (fun compunit required_compunits -> match compunit with
>>>>>>> 5.2.0
        | { pm_kind = PM_intf } ->
<<<<<<< HEAD
            required_globals
        | { pm_kind = PM_impl { cu_required_globals; cu_reloc } } ->
            let ids_to_remove (rel, _pos) =
||||||| 121bedcfd2
            required_globals
        | { pm_kind = PM_impl { cu_required_globals; cu_reloc } } ->
            let remove_required (rel, _pos) required_globals =
=======
            required_compunits
        | { pm_kind = PM_impl { cu_required_compunits; cu_reloc } } ->
            let remove_required (rel, _pos) required_compunits =
>>>>>>> 5.2.0
              match rel with
<<<<<<< HEAD
                Reloc_setglobal id -> [id]
              | _ -> []
            in
            let ids_to_remove =
              List.concat_map ids_to_remove cu_reloc
              |> Ident.Set.of_list
||||||| 121bedcfd2
                Reloc_setglobal id ->
                  Ident.Set.remove id required_globals
              | _ ->
                  required_globals
=======
                Reloc_setcompunit cu ->
                  Compunit.Set.remove cu required_compunits
              | Reloc_literal _ | Reloc_getcompunit _ | Reloc_getpredef _
              | Reloc_primitive _ ->
                  required_compunits
>>>>>>> 5.2.0
            in
<<<<<<< HEAD
            let required_globals =
              let keep cu =
                not (Ident.Set.mem (cu |> CU.to_global_ident_for_bytecode)
                       ids_to_remove)
              in
              Compilation_unit.Set.filter keep required_globals
||||||| 121bedcfd2
            let required_globals =
              List.fold_right remove_required cu_reloc required_globals
=======
            let required_compunits =
              List.fold_right remove_required cu_reloc required_compunits
>>>>>>> 5.2.0
            in
<<<<<<< HEAD
            List.fold_right Compilation_unit.Set.add cu_required_globals
              required_globals)
      members Compilation_unit.Set.empty
||||||| 121bedcfd2
            List.fold_right Ident.Set.add cu_required_globals required_globals)
      members Ident.Set.empty
=======
            List.fold_right
              Compunit.Set.add cu_required_compunits required_compunits)
      members Compunit.Set.empty
>>>>>>> 5.2.0
  in
  let oc = open_out_bin targetfile in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc Config.cmo_magic_number;
    let pos_depl = pos_out oc in
    output_binary_int oc 0;
    let pos_code = pos_out oc in
    let state = empty_state in
    let state =
<<<<<<< HEAD
      List.fold_left (process_append_pack_member targetname oc) state members in
||||||| 121bedcfd2
      let mapping =
        List.map
          (fun m -> m.pm_ident, (m.pm_packed_ident, false))
          members
        |> Ident.Map.of_list in
      { empty_state with mapping } in
=======
      let mapping =
        List.map
          (fun m -> m.pm_ident,
            { packed_modname = m.pm_packed_ident; processed = false }
          )
          members
        |> Compunit.Map.of_list in
      { empty_state with mapping } in
>>>>>>> 5.2.0
    let state =
      build_global_target ~ppf_dump oc targetname state members coercion in
    let pos_debug = pos_out oc in
    (* CR mshinwell: Compression not supported in the OCaml 4 runtime
    if !Clflags.debug && state.events <> [] then begin
      Compression.output_value oc (List.rev state.events);
      Compression.output_value oc (String.Set.elements state.debug_dirs)
    end;
    *)
    let force_link =
      List.exists (function
          | {pm_kind = PM_impl {cu_force_link}} -> cu_force_link
          | _ -> false) members in
    let pos_final = pos_out oc in
    let imports =
      let unit_names =
        List.map (fun m -> m.pm_name) members in
      List.filter
        (fun import -> not (List.mem (Import_info.name import) unit_names))
        (Bytelink.extract_crc_interfaces()) in
    let for_pack_prefix = CU.Prefix.from_clflags () in
    let modname = targetname |> CU.Name.of_string in
    let cu_name = CU.create for_pack_prefix modname in
    let compunit =
<<<<<<< HEAD
      { cu_name;
||||||| 121bedcfd2
      { cu_name = targetname;
=======
      { cu_name = Compunit targetname;
>>>>>>> 5.2.0
        cu_pos = pos_code;
        cu_codesize = pos_debug - pos_code;
        cu_reloc = List.rev state.relocs;
        cu_imports =
          Array.of_list
            ((Import_info.create modname
               ~crc_with_unit:(Some (cu_name, Env.crc_of_unit modname)))
              :: imports);
        cu_primitives = List.rev state.primitives;
<<<<<<< HEAD
        cu_required_globals = Compilation_unit.Set.elements required_globals;
||||||| 121bedcfd2
        cu_required_globals = Ident.Set.elements required_globals;
=======
        cu_required_compunits =
          (Compunit.Set.elements required_compunits);
>>>>>>> 5.2.0
        cu_force_link = force_link;
        cu_debug = if pos_final > pos_debug then pos_debug else 0;
        cu_debugsize = pos_final - pos_debug } in
    Emitcode.marshal_to_channel_with_possibly_32bit_compat
      ~filename:targetfile ~kind:"bytecode unit"
      oc compunit;
    seek_out oc pos_depl;
    output_binary_int oc pos_final)

(* The entry point *)

let package_files ~ppf_dump initial_env files targetfile =
  let files =
    List.map
      (fun f ->
         try Load_path.find f
         with Not_found -> raise(Error(File_not_found f)))
      files in
<<<<<<< HEAD
  let prefix = chop_extensions targetfile in
  let targetcmi = prefix ^ ".cmi" in
  let targetname = String.capitalize_ascii(Filename.basename prefix) in
    let comp_unit =
      Compilation_unit.create (Compilation_unit.Prefix.from_clflags ())
        (targetname |> Compilation_unit.Name.of_string)
    in
    Compilation_unit.set_current (Some comp_unit);
||||||| 121bedcfd2
  let prefix = chop_extensions targetfile in
  let targetcmi = prefix ^ ".cmi" in
  let targetname = String.capitalize_ascii(Filename.basename prefix) in
=======
  let target = Unit_info.Artifact.from_filename targetfile in
>>>>>>> 5.2.0
  Misc.try_finally (fun () ->
      let coercion =
<<<<<<< HEAD
        Typemod.package_units initial_env files targetcmi comp_unit in
      package_object_files ~ppf_dump files targetfile targetname coercion
||||||| 121bedcfd2
        Typemod.package_units initial_env files targetcmi targetname in
      package_object_files ~ppf_dump files targetfile targetname coercion
=======
        Typemod.package_units initial_env files (Unit_info.companion_cmi target)
      in
      package_object_files ~ppf_dump files target coercion
>>>>>>> 5.2.0
    )
    ~exceptionally:(fun () -> remove_file targetfile)

(* Error report *)

open Format
module Style = Misc.Style

let report_error ppf = function
    Forward_reference(file, compunit) ->
      fprintf ppf "Forward reference to %a in file %a"
        Style.inline_code (Compunit.name compunit)
        (Style.as_inline_code Location.print_filename) file
  | Multiple_definition(file, compunit) ->
      fprintf ppf "File %a redefines %a"
        (Style.as_inline_code Location.print_filename) file
        Style.inline_code (Compunit.name compunit)
  | Not_an_object_file file ->
      fprintf ppf "%a is not a bytecode object file"
        (Style.as_inline_code Location.print_filename) file
  | Illegal_renaming(name, file, id) ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
<<<<<<< HEAD
                   @ %a when %s was expected"
        Location.print_filename file
        Compilation_unit.Name.print name
        id
||||||| 121bedcfd2
                   @ %s when %s was expected"
        Location.print_filename file name id
=======
                   @ %a when %a was expected"
        (Style.as_inline_code Location.print_filename) file
        Style.inline_code (Compunit.name name)
        Style.inline_code (Compunit.name id)
>>>>>>> 5.2.0
  | File_not_found file ->
      fprintf ppf "File %a not found"
        Style.inline_code file

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
