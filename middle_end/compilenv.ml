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

(* Compilation environments for compilation units *)

[@@@ocaml.warning "+a-4-9-40-41-42"]

open Config
open Cmx_format

module File_sections = Flambda_backend_utils.File_sections

module CU = Compilation_unit

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of CU.t * CU.t * string

exception Error of error

let global_infos_table =
  (CU.Name.Tbl.create 17 : unit_infos option CU.Name.Tbl.t)
let export_infos_table =
  (CU.Name.Tbl.create 10 : Export_info.t CU.Name.Tbl.t)

let reset_info_tables () =
  CU.Name.Tbl.reset global_infos_table;
  CU.Name.Tbl.reset export_infos_table

let imported_sets_of_closures_table =
  (Set_of_closures_id.Tbl.create 10
   : Simple_value_approx.function_declarations option
       Set_of_closures_id.Tbl.t)

module CstMap =
  Map.Make(struct
    type t = Clambda.ustructured_constant
    let compare = Clambda.compare_structured_constants
    (* PR#6442: it is incorrect to use Stdlib.compare on values of type t
       because it compares "0.0" and "-0.0" equal. *)
  end)

module SymMap = Misc.Stdlib.String.Map
module String = Misc.Stdlib.String

type structured_constants =
  {
    strcst_shared: string CstMap.t;
    strcst_all: Clambda.ustructured_constant SymMap.t;
  }

let structured_constants_empty  =
  {
    strcst_shared = CstMap.empty;
    strcst_all = SymMap.empty;
  }

let structured_constants = ref structured_constants_empty


let exported_constants = Hashtbl.create 17

let merged_environment = ref Export_info.empty

let cached_checks = Checks.create ()

let cache_checks c = Checks.merge c ~into:cached_checks

let default_ui_export_info =
  if Config.flambda then
    Cmx_format.Flambda1 Export_info.empty
  else if Config.flambda2 then
    Cmx_format.Flambda2 None
  else
    Cmx_format.Clambda Value_unknown

let current_unit =
  { ui_unit = CU.dummy;
    ui_defines = [];
    ui_imports_cmi = [];
    ui_imports_cmx = [];
    ui_runtime_params = [];
    ui_generic_fns = { curry_fun = []; apply_fun = []; send_fun = [] };
    ui_force_link = false;
    ui_checks = Checks.create ();
    ui_export_info = default_ui_export_info }

let reset compilation_unit =
  CU.Name.Tbl.clear global_infos_table;
  Set_of_closures_id.Tbl.clear imported_sets_of_closures_table;
  Checks.reset cached_checks;
  CU.set_current (Some compilation_unit);
  current_unit.ui_unit <- compilation_unit;
  current_unit.ui_defines <- [compilation_unit];
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmx <- [];
  current_unit.ui_generic_fns <-
    { curry_fun = []; apply_fun = []; send_fun = [] };
  current_unit.ui_force_link <- !Clflags.link_everything;
  Checks.reset current_unit.ui_checks;
  Hashtbl.clear exported_constants;
  structured_constants := structured_constants_empty;
  current_unit.ui_export_info <- default_ui_export_info;
  merged_environment := Export_info.empty;
  CU.Name.Tbl.clear export_infos_table

let current_unit_infos () =
  current_unit

let read_unit_info filename =
  let ic = open_in_bin filename in
  try
    let buffer = really_input_string ic (String.length cmx_magic_number) in
    if buffer <> cmx_magic_number then begin
      close_in ic;
      raise(Error(Not_a_unit_info filename))
    end;
    let uir = (input_value ic : unit_infos_raw) in
    let first_section_offset = pos_in ic in
    seek_in ic (first_section_offset + uir.uir_sections_length);
    let crc = Digest.input ic in
    (* This consumes the channel *)
    let sections = File_sections.create uir.uir_section_toc filename ic ~first_section_offset in
    let export_info =
      match uir.uir_export_info with
      | Clambda_raw info -> Clambda info
      | Flambda1_raw info -> Flambda1 info
      | Flambda2_raw None -> Flambda2 None
      | Flambda2_raw (Some info) ->
        Flambda2 (Some (Flambda2_cmx.Flambda_cmx_format.from_raw ~sections info))
    in
    let ui = {
      ui_unit = uir.uir_unit;
      ui_defines = uir.uir_defines;
      ui_imports_cmi = uir.uir_imports_cmi |> Array.to_list;
      ui_imports_cmx = uir.uir_imports_cmx |> Array.to_list;
      ui_runtime_params = uir.uir_runtime_params |> Array.to_list;
      ui_generic_fns = uir.uir_generic_fns;
      ui_export_info = export_info;
      ui_checks = Checks.of_raw uir.uir_checks;
      ui_force_link = uir.uir_force_link
    }
    in
    (ui, crc)
  with End_of_file | Failure _ ->
    close_in ic;
    raise(Error(Corrupted_unit_info(filename)))

let read_library_info filename =
  let ic = open_in_bin filename in
  let buffer = really_input_string ic (String.length cmxa_magic_number) in
  if buffer <> cmxa_magic_number then
    raise(Error(Not_a_unit_info filename));
  let infos = (input_value ic : library_infos) in
  close_in ic;
  infos


(* Read and cache info on global identifiers *)

let get_unit_info comp_unit =
  (* If this fails, it likely means that someone didn't call
     [CU.which_cmx_file]. *)
  assert (CU.can_access_cmx_file comp_unit ~accessed_by:current_unit.ui_unit);
  (* CR lmaurer: Surely this should just compare [comp_unit] to
     [current_unit.ui_unit], but doing so seems to break Closure. We should fix
     that. *)
  if CU.Name.equal (CU.name comp_unit) (CU.name current_unit.ui_unit)
  then
    Some current_unit
  else begin
    let cmx_name = CU.name comp_unit in
    try
      CU.Name.Tbl.find global_infos_table cmx_name
    with Not_found ->
      let (infos, crc) =
        if Env.is_imported_opaque cmx_name then (None, None)
        else begin
          try
            let filename =
              Load_path.find_uncap ((cmx_name |> CU.Name.to_string) ^ ".cmx") in
            let (ui, crc) = read_unit_info filename in
            if not (CU.equal ui.ui_unit comp_unit) then
              raise(Error(Illegal_renaming(comp_unit, ui.ui_unit, filename)));
            cache_checks ui.ui_checks;
            (Some ui, Some crc)
          with Not_found ->
            let warn = Warnings.No_cmx_file (cmx_name |> CU.Name.to_string) in
              Location.prerr_warning Location.none warn;
              (None, None)
          end
      in
      let import = Import_info.create_normal comp_unit ~crc in
      current_unit.ui_imports_cmx <- import :: current_unit.ui_imports_cmx;
      CU.Name.Tbl.add global_infos_table cmx_name infos;
      infos
  end

let which_cmx_file comp_unit =
  CU.which_cmx_file comp_unit ~accessed_by:(CU.get_current_exn ())

let get_unit_export_info comp_unit =
  match get_unit_info comp_unit with
  | None -> None
  | Some ui -> Some ui.ui_export_info

let get_global_info comp_unit =
  get_unit_info (which_cmx_file comp_unit)

let get_global_export_info id =
  match get_global_info id with
  | None -> None
  | Some ui -> Some ui.ui_export_info

let cache_unit_info ui =
  cache_checks ui.ui_checks;
  CU.Name.Tbl.add global_infos_table (CU.name ui.ui_unit) (Some ui)

(* Return the approximation of a global identifier *)

let get_clambda_approx ui =
  assert(not Config.flambda);
  match ui.ui_export_info with
  | Flambda1 _ | Flambda2 _ -> assert false
  | Clambda approx -> approx

let toplevel_approx :
  (CU.t, Clambda.value_approximation) Hashtbl.t = Hashtbl.create 16

let record_global_approx_toplevel () =
  Hashtbl.add toplevel_approx
    current_unit.ui_unit
    (get_clambda_approx current_unit)

let global_approx comp_unit =
  try Hashtbl.find toplevel_approx comp_unit
  with Not_found ->
    match get_global_info comp_unit with
      | None -> Clambda.Value_unknown
      | Some ui -> get_clambda_approx ui

(* Register the approximation of the module being compiled *)

let set_global_approx approx =
  assert(not Config.flambda);
  current_unit.ui_export_info <- Clambda approx

(* Exporting and importing cross module information (Flambda only) *)

let get_flambda_export_info ui =
  assert(Config.flambda);
  match ui.ui_export_info with
  | Clambda _ | Flambda2 _ -> assert false
  | Flambda1 ei -> ei

let set_export_info export_info =
  assert(Config.flambda);
  current_unit.ui_export_info <- Flambda1 export_info

let flambda2_set_export_info export_info =
  assert(Config.flambda2);
  current_unit.ui_export_info <- Flambda2 (Some export_info)

let approx_for_global comp_unit =
  if CU.equal comp_unit CU.predef_exn
  then invalid_arg "approx_for_global with predef_exn compilation unit";
  let accessible_comp_unit = which_cmx_file comp_unit in
  let cmx_name = CU.name accessible_comp_unit in
  match CU.Name.Tbl.find export_infos_table cmx_name with
  | otherwise -> Some otherwise
  | exception Not_found ->
    match get_unit_info accessible_comp_unit with
    | None -> None
    | Some ui ->
      let exported = get_flambda_export_info ui in
      CU.Name.Tbl.add export_infos_table cmx_name exported;
      merged_environment := Export_info.merge !merged_environment exported;
      Some exported

let approx_env () = !merged_environment

(* Record that a currying function or application function is needed *)

let need_curry_fun kind arity result =
  let fns = current_unit.ui_generic_fns in
  if not (List.mem (kind, arity, result) fns.curry_fun) then
    current_unit.ui_generic_fns <-
      { fns with curry_fun = (kind, arity, result) :: fns.curry_fun }

let need_apply_fun arity result mode =
  assert(List.compare_length_with arity 0 > 0);
  let fns = current_unit.ui_generic_fns in
  if not (List.mem (arity, result, mode) fns.apply_fun) then
    current_unit.ui_generic_fns <-
      { fns with apply_fun = (arity, result, mode) :: fns.apply_fun }

let need_send_fun arity result mode =
  let fns = current_unit.ui_generic_fns in
  if not (List.mem (arity, result, mode) fns.send_fun) then
    current_unit.ui_generic_fns <-
      { fns with send_fun = (arity, result, mode) :: fns.send_fun }

(* Write the description of the current unit *)

(* CR mshinwell: let's think about this later, quadratic algorithm

let ensure_sharing_between_cmi_and_cmx_imports cmi_imports cmx_imports =
  (* If a [CU.t] in the .cmx imports also occurs in the .cmi imports, use
     the one in the .cmi imports, to increase sharing.  (Such a [CU.t] in
     the .cmi imports may already have part of its value shared with the
     first [CU.Name.t] component in the .cmi imports, c.f.
     [Persistent_env.ensure_crc_sharing], so it's best to pick this [CU.t].) *)
  List.map (fun ((comp_unit, crc) as import) ->
      match
        List.find_map (function
            | _, None -> None
            | _, Some (comp_unit', _) ->
              if CU.equal comp_unit comp_unit' then Some comp_unit'
              else None)
          cmi_imports
      with
      | None -> import
      | Some comp_unit -> comp_unit, crc)
    cmx_imports
*)

let write_unit_info info filename =
  let raw_export_info, sections =
    match info.ui_export_info with
    | Clambda info -> Clambda_raw info, File_sections.empty
    | Flambda1 info -> Flambda1_raw info, File_sections.empty
    | Flambda2 None -> Flambda2_raw None, File_sections.empty
    | Flambda2 (Some info) ->
      let info, sections = Flambda2_cmx.Flambda_cmx_format.to_raw info in
      Flambda2_raw (Some info), sections
  in
  let serialized_sections, toc, total_length = File_sections.serialize sections in
  let raw_info = {
    uir_unit = info.ui_unit;
    uir_defines = info.ui_defines;
    uir_imports_cmi = Array.of_list info.ui_imports_cmi;
    uir_imports_cmx = Array.of_list info.ui_imports_cmx;
    uir_runtime_params = Array.of_list info.ui_runtime_params;
    uir_generic_fns = info.ui_generic_fns;
    uir_export_info = raw_export_info;
    uir_checks = Checks.to_raw info.ui_checks;
    uir_force_link = info.ui_force_link;
    uir_section_toc = toc;
    uir_sections_length = total_length;
  } in
  let oc = open_out_bin filename in
  output_string oc cmx_magic_number;
  output_value oc raw_info;
  Array.iter (output_string oc) serialized_sections;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc

let save_unit_info filename =
  current_unit.ui_imports_cmi <- Env.imports();
  write_unit_info current_unit filename

let snapshot () = !structured_constants
let backtrack s = structured_constants := s

let new_const_symbol () =
  Symbol.for_new_const_in_current_unit ()
  |> Symbol.linkage_name
  |> Linkage_name.to_string

let new_structured_constant cst ~shared =
  let {strcst_shared; strcst_all} = !structured_constants in
  if shared then
    try
      CstMap.find cst strcst_shared
    with Not_found ->
      let lbl = new_const_symbol() in
      structured_constants :=
        {
          strcst_shared = CstMap.add cst lbl strcst_shared;
          strcst_all = SymMap.add lbl cst strcst_all;
        };
      lbl
  else
    let lbl = new_const_symbol() in
    structured_constants :=
      {
        strcst_shared;
        strcst_all = SymMap.add lbl cst strcst_all;
      };
    lbl

let add_exported_constant s =
  Hashtbl.replace exported_constants s ()

let clear_structured_constants () =
  structured_constants := structured_constants_empty

let structured_constant_of_symbol s =
  SymMap.find_opt s (!structured_constants).strcst_all

let structured_constants () =
  let provenance : Clambda.usymbol_provenance =
    { original_idents = [];
      module_path =
        (* CR-someday lmaurer: Properly construct a [Path.t] from the module name
           with its pack prefix. *)
        Path.Pident (Ident.create_persistent (Compilation_unit.Name.to_string (
          Compilation_unit.name (Compilation_unit.get_current_exn ()))));
    }
  in
  SymMap.bindings (!structured_constants).strcst_all
  |> List.map
    (fun (symbol, definition) ->
       {
         Clambda.symbol;
         exported = Hashtbl.mem exported_constants symbol;
         definition;
         provenance = Some provenance;
        })

let require_global global_ident =
  ignore (get_global_info global_ident : Cmx_format.unit_infos option)

(* Error report *)

open Format

let report_error ppf = function
  | Not_a_unit_info filename ->
      fprintf ppf "%a@ is not a compilation unit description."
        Location.print_filename filename
  | Corrupted_unit_info filename ->
      fprintf ppf "Corrupted compilation unit description@ %a"
        Location.print_filename filename
  | Illegal_renaming(name, modname, filename) ->
      fprintf ppf "%a@ contains the description for unit\
                   @ %a when %a was expected"
        Location.print_filename filename
        CU.print name
        CU.print modname

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
