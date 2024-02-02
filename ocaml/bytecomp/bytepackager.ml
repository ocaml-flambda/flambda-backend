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

let rec rev_append_map f l rest =
  match l with
  | [] -> rest
  | x :: xs -> rev_append_map f xs (f x :: rest)

type error =
    Forward_reference of string * Ident.t
  | Multiple_definition of string * Ident.t
  | Not_an_object_file of string
  | Illegal_renaming of Compilation_unit.Name.t * string * string
  | File_not_found of string

exception Error of error

type state = {
  relocs : (reloc_info * int) list; (** accumulated reloc info *)
  events : debug_event list;        (** accumulated debug events *)
  debug_dirs : String.Set.t;        (** accumulated debug_dirs *)
  primitives : string list;         (** accumulated primitives *)
  offset : int;                     (** offset of the current unit *)
  subst : Subst.t;                  (** Substitution for debug event *)
}

let empty_state = {
  relocs = [];
  events = [];
  debug_dirs = String.Set.empty;
  primitives = [];
  offset = 0;
  subst = Subst.identity;
}

(* Record a relocation, updating its offset. *)

let rename_relocation base (rel, ofs) =
  (* Nothing to do here following the symbols patches *)
  rel, base + ofs

(* Record and update a debugging event *)

let relocate_debug base subst ev =
  { ev with ev_pos = base + ev.ev_pos;
            ev_typsubst = Subst.compose ev.ev_typsubst subst }

(* Read the unit information from a .cmo file. *)

type pack_member_kind = PM_intf | PM_impl of compilation_unit_descr

type pack_member =
  { pm_file: string;
    pm_name: Compilation_unit.Name.t;
    pm_kind: pack_member_kind }

let read_member_info file =
  let name = String.capitalize_ascii(Filename.basename(chop_extensions file)) in
  let name = Compilation_unit.Name.of_string name in
  let kind =
    (* PR#7479: make sure it is either a .cmi or a .cmo *)
    if Filename.check_suffix file ".cmi" then
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
        let compunit = (input_value ic : compilation_unit_descr) in
        if not (CU.Name.equal (CU.name compunit.cu_name) name)
        then raise(Error(Illegal_renaming(name, file,
          CU.name_as_string compunit.cu_name)));
        PM_impl compunit)
    end in
  { pm_file = file; pm_name = name; pm_kind = kind }

(* Read the bytecode from a .cmo file.
   Write bytecode to channel [oc].
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
        let unit_events = (input_value ic : debug_event list) in
        let events =
          rev_append_map
            (relocate_debug state.offset state.subst)
            unit_events
            state.events in
        let unit_debug_dirs = (input_value ic : string list) in
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
      let subst =
        Subst.add_module id (Path.Pdot (root, Ident.name id)) state.subst in
      { state with subst }

(* Generate the code that builds the tuple representing the package module *)

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

let package_object_files ~ppf_dump files targetfile targetname coercion =
  let members = map_left_right read_member_info files in
  let required_globals =
    List.fold_right (fun compunit required_globals -> match compunit with
        | { pm_kind = PM_intf } ->
            required_globals
        | { pm_kind = PM_impl { cu_required_globals; cu_reloc } } ->
            let ids_to_remove (rel, _pos) =
              match rel with
                Reloc_setglobal id -> [id]
              | _ -> []
            in
            let ids_to_remove =
              List.concat_map ids_to_remove cu_reloc
              |> Ident.Set.of_list
            in
            let required_globals =
              let keep cu =
                not (Ident.Set.mem (cu |> CU.to_global_ident_for_bytecode)
                       ids_to_remove)
              in
              Compilation_unit.Set.filter keep required_globals
            in
            List.fold_right Compilation_unit.Set.add cu_required_globals
              required_globals)
      members Compilation_unit.Set.empty
  in
  let oc = open_out_bin targetfile in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc Config.cmo_magic_number;
    let pos_depl = pos_out oc in
    output_binary_int oc 0;
    let pos_code = pos_out oc in
    let state = empty_state in
    let state =
      List.fold_left (process_append_pack_member targetname oc) state members in
    let state =
      build_global_target ~ppf_dump oc targetname state members coercion in
    let pos_debug = pos_out oc in
    (* CR mshinwell: Compression not supported in the OCaml 4 runtime
    if !Clflags.debug && state.events <> [] then begin
      Marshal.(to_channel oc (List.rev state.events) [Compression]);
      Marshal.(to_channel oc (String.Set.elements state.debug_dirs)
                             [Compression]);
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
        (fun import -> not (List.mem (Import_info.Intf.name import) unit_names))
        (Bytelink.extract_crc_interfaces()) in
    let for_pack_prefix = CU.Prefix.from_clflags () in
    let modname = targetname |> CU.Name.of_string in
    let cu_name = CU.create for_pack_prefix modname in
    let compunit =
      { cu_name;
        cu_pos = pos_code;
        cu_codesize = pos_debug - pos_code;
        cu_reloc = List.rev state.relocs;
        cu_imports =
          Array.of_list
            ((Import_info.Intf.create modname
               ~crc_with_unit:(Some (cu_name, Env.crc_of_unit modname)))
              :: imports);
        cu_primitives = List.rev state.primitives;
        cu_required_globals = Compilation_unit.Set.elements required_globals;
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
  let prefix = chop_extensions targetfile in
  let targetcmi = prefix ^ ".cmi" in
  let targetname = String.capitalize_ascii(Filename.basename prefix) in
    let comp_unit =
      Compilation_unit.create (Compilation_unit.Prefix.from_clflags ())
        (targetname |> Compilation_unit.Name.of_string)
    in
    Compilation_unit.set_current (Some comp_unit);
  Misc.try_finally (fun () ->
      let coercion =
        Typemod.package_units initial_env files targetcmi comp_unit in
      package_object_files ~ppf_dump files targetfile targetname coercion
    )
    ~exceptionally:(fun () -> remove_file targetfile)

(* Error report *)

open Format

let report_error ppf = function
    Forward_reference(file, ident) ->
      fprintf ppf "Forward reference to %s in file %a" (Ident.name ident)
        Location.print_filename file
  | Multiple_definition(file, ident) ->
      fprintf ppf "File %a redefines %s"
        Location.print_filename file
        (Ident.name ident)
  | Not_an_object_file file ->
      fprintf ppf "%a is not a bytecode object file"
        Location.print_filename file
  | Illegal_renaming(name, file, id) ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %a when %s was expected"
        Location.print_filename file
        Compilation_unit.Name.print name
        id
  | File_not_found file ->
      fprintf ppf "File %s not found" file

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
