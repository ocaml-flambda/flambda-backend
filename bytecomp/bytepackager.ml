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
    Forward_reference of string * CU.t
  | Multiple_definition of string * CU.t
  | Not_an_object_file of string
  | Illegal_renaming of CU.t * string * CU.t
  | File_not_found of string

exception Error of error

type mapped_compunit = {
  processed : bool
}

let record_as_processed mapping cu =
  let update_processed = function
    | Some { processed = false } -> Some { processed = true }
    | Some { processed = true } | None -> assert false
  in
  CU.Name.Map.update (CU.name cu) update_processed mapping

type state = {
  relocs : (reloc_info * int) list; (** accumulated reloc info *)
  events : debug_event list;        (** accumulated debug events *)
  debug_dirs : String.Set.t;        (** accumulated debug_dirs *)
  primitives : string list;         (** accumulated primitives *)
  offset : int;                     (** offset of the current unit *)
  subst : Subst.t;                  (** Substitution for debug event *)
  mapping : mapped_compunit CU.Name.Map.t;
  (** Says whether a [CU.Name.t] has already been seen during packing *)
}

let empty_state = {
  relocs = [];
  events = [];
  debug_dirs = String.Set.empty;
  primitives = [];
  offset = 0;
  mapping = CU.Name.Map.empty;
  subst = Subst.identity;
}

(* Record a relocation, updating its offset. *)

let rename_relocation base (rel, ofs) =
  (* Nothing to do here following the symbols patches *)
  rel, base + ofs

(* XXX mshinwell: the above function in runtime5 has:

  (* PR#5276: unique-ize dotted global names, which appear if one of
    the units being consolidated is itself a packed module. *)
  let make_compunit_name_unique cu =
    if CU.is_packed cu
    then CU (packagename ^ "." ^ (CU.name cu))
    else cu
  in

*)

(* Record and update a debugging event *)

let relocate_debug base subst ev =
  { ev with ev_pos = base + ev.ev_pos;
            ev_typsubst = Subst.compose ev.ev_typsubst subst }

(* Read the unit information from a .cmo file. *)

type pack_member_kind = PM_intf | PM_impl of compilation_unit_descr

type pack_member =
  { pm_file: string;
    pm_packed_name : CU.t;
    pm_kind: pack_member_kind }

let read_member_info ~packed_compilation_unit file =
  let member_artifact = Unit_info.Artifact.from_filename file in
  let member_name =
    Unit_info.Artifact.modname member_artifact |> CU.Name.of_string
  in
  let packed_name = CU.create_child packed_compilation_unit member_name in
  let kind =
    (* PR#7479: make sure it is either a .cmi or a .cmo *)
    if Unit_info.is_cmi member_artifact then
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
        if not (CU.Name.equal (CU.name compunit.cu_name) member_name)
        then begin
          raise(Error(Illegal_renaming (packed_name, file, compunit.cu_name)))
        end;
        PM_impl compunit)
    end
  in
  { pm_file = file;
    pm_packed_name = packed_name;
    pm_kind = kind;
  }

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
        (* CR ocaml 5 compressed-marshal:
        let unit_events = (Compression.input_value ic : debug_event list) in
        *)
        let unit_events = (Marshal.from_channel ic : debug_event list) in
        let events =
          rev_append_map
            (relocate_debug state.offset state.subst)
            unit_events
            state.events in
        let unit_debug_dirs =
          (* CR ocaml 5 compressed-marshal:
          (Compression.input_value ic : string list)
          *)
          (Marshal.from_channel ic : string list)
        in
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
      let root = Path.Pident (Ident.create_persistent packagename) in
      let mapping = record_as_processed state.mapping m.pm_packed_name in
      let subst =
        let name = CU.name m.pm_packed_name |> CU.Name.to_string in
        Subst.add_module (Ident.create_persistent name)
          (Path.Pdot (root, name)) state.subst
      in
      { state with subst; mapping }

(* Generate the code that builds the tuple representing the package module *)

let build_global_target ~ppf_dump oc ~packed_compilation_unit state members
      coercion =
   let components =
    List.map
      (fun m ->
        match m.pm_kind with
        | PM_intf -> None
        | PM_impl _ -> Some m.pm_packed_name)
      members
  in
  let _size, lam =
    Translmod.transl_package components packed_compilation_unit coercion
      ~style:Set_global_to_block
  in
  if !Clflags.dump_rawlambda then
    Format.fprintf ppf_dump "%a@." Printlambda.lambda lam;
  let lam = Simplif.simplify_lambda lam in
  if !Clflags.dump_lambda then
    Format.fprintf ppf_dump "%a@." Printlambda.lambda lam;
  let instrs = Bytegen.compile_implementation packed_compilation_unit lam in
  let size, pack_relocs, pack_events, pack_debug_dirs =
    Emitcode.to_packed_file oc instrs in
  let events = List.rev_append pack_events state.events in
  let debug_dirs = String.Set.union pack_debug_dirs state.debug_dirs in
  let relocs =
    rev_append_map
      (fun (r, ofs) -> (r, state.offset + ofs))
      pack_relocs state.relocs in
  { state with events; debug_dirs; relocs; offset = state.offset + size }

(* Build the .cmo file obtained by packaging the given .cmo files. *)

let package_object_files ~ppf_dump files target coercion =
  let targetfile = Unit_info.Artifact.filename target in
  let packed_compilation_unit_name =
    CU.Name.of_string (Unit_info.Artifact.modname target)
  in
  let packed_compilation_unit =
    let prefix = CU.Prefix.from_clflags () in
    CU.create prefix packed_compilation_unit_name
  in
  let members =
    map_left_right (read_member_info ~packed_compilation_unit) files
  in
  let required_compunits =
    List.fold_right (fun compunit required_compunits ->
        match compunit with
        | { pm_kind = PM_intf } ->
            required_compunits
        | { pm_kind = PM_impl { cu_required_compunits; cu_reloc } } ->
            let cus_to_remove (rel, _pos) =
              match rel with
              | Reloc_setcompunit cu -> [cu]
              | Reloc_literal _ | Reloc_getcompunit _ | Reloc_getpredef _
              | Reloc_primitive _ -> []
            in
            let cus_to_remove =
              List.concat_map cus_to_remove cu_reloc
              |> CU.Set.of_list
            in
            let required_compunits =
              let keep cu = not (CU.Set.mem cu cus_to_remove) in
              CU.Set.filter keep required_compunits
            in
            List.fold_right CU.Set.add cu_required_compunits required_compunits)
      members CU.Set.empty
  in
  let oc = open_out_bin targetfile in
  Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
    output_string oc Config.cmo_magic_number;
    let pos_depl = pos_out oc in
    output_binary_int oc 0;
    let pos_code = pos_out oc in
    let state =
      let mapping =
        List.map
          (fun m -> CU.name m.pm_packed_name, { processed = false })
          members
        |> CU.Name.Map.of_list in
      { empty_state with mapping } in
    let state =
      List.fold_left (process_append_pack_member targetfile oc) state members
    in
    let state =
      build_global_target ~ppf_dump oc ~packed_compilation_unit state
        members coercion
    in
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
          | _ -> false) members
    in
    let pos_final = pos_out oc in
    let imports =
      let unit_names = List.map (fun m -> CU.name m.pm_packed_name) members in
      List.filter
        (fun import -> not (List.mem (Import_info.name import) unit_names))
        (Bytelink.extract_crc_interfaces())
    in
    let import_info_for_the_pack_itself =
      let crc = Env.crc_of_unit packed_compilation_unit_name in
      Import_info.create packed_compilation_unit_name
        ~crc_with_unit:(Some (packed_compilation_unit, crc))
    in
    let compunit =
      { cu_name = packed_compilation_unit;
        cu_pos = pos_code;
        cu_codesize = pos_debug - pos_code;
        cu_reloc = List.rev state.relocs;
        cu_imports = Array.of_list (import_info_for_the_pack_itself :: imports);
        cu_primitives = List.rev state.primitives;
        cu_required_compunits = CU.Set.elements required_compunits;
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
  let target = Unit_info.Artifact.from_filename targetfile in
  let comp_unit =
    CU.create (CU.Prefix.from_clflags ())
      (Unit_info.Artifact.modname target |> CU.Name.of_string)
  in
  CU.set_current (Some comp_unit);
  Misc.try_finally (fun () ->
      let coercion =
        Typemod.package_units initial_env files (Unit_info.companion_cmi target)
          comp_unit
      in
      package_object_files ~ppf_dump files target coercion
    )
    ~exceptionally:(fun () -> remove_file targetfile)

(* Error report *)

open Format
module Style = Misc.Style

let report_error ppf = function
    Forward_reference(file, compunit) ->
      fprintf ppf "Forward reference to %a in file %a"
        (Style.as_inline_code CU.print) compunit
        (Style.as_inline_code Location.print_filename) file
  | Multiple_definition(file, compunit) ->
      fprintf ppf "File %a redefines %a"
        (Style.as_inline_code Location.print_filename) file
        (Style.as_inline_code CU.print) compunit
  | Not_an_object_file file ->
      fprintf ppf "%a is not a bytecode object file"
        (Style.as_inline_code Location.print_filename) file
  | Illegal_renaming(name, file, compunit) ->
      fprintf ppf "Wrong file naming: %a@ contains the code for\
                   @ %a when %a was expected"
        (Style.as_inline_code Location.print_filename) file
        (Style.as_inline_code CU.print) name
        (Style.as_inline_code CU.print) compunit
  | File_not_found file ->
      fprintf ppf "File %a not found"
        Style.inline_code file

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
