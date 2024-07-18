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

module CU = Compilation_unit

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of CU.t * CU.t * string
  | Mismatching_for_pack of
      string * CU.Prefix.t * CU.Name.t * CU.Prefix.t option

exception Error of error

module Infos_table = Global.Name.Tbl

let global_infos_table =
  (Infos_table.create 17 : unit_infos option Infos_table.t)
let export_infos_table =
  (Infos_table.create 10 : Export_info.t Infos_table.t)

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

let default_ui_export_info =
  if Config.flambda then
    Cmx_format.Flambda Export_info.empty
  else
    Cmx_format.Clambda Value_unknown

let current_unit =
  { ui_unit = CU.dummy;
    ui_defines = [];
    ui_arg_descr = None;
    ui_imports_cmi = [| |];
    ui_imports_cmx = [| |];
    ui_runtime_params = [];
    ui_curry_fun = [];
    ui_apply_fun = [];
    ui_send_fun = [];
    ui_force_link = false;
    ui_export_info = default_ui_export_info;
    ui_for_pack = None }

let reset compilation_unit =
  Infos_table.clear global_infos_table;
  Set_of_closures_id.Tbl.clear imported_sets_of_closures_table;
  CU.set_current (Some compilation_unit);
  current_unit.ui_unit <- compilation_unit;
  current_unit.ui_defines <- [compilation_unit];
  current_unit.ui_arg_descr <- None;
  current_unit.ui_imports_cmi <- [| |];
  current_unit.ui_imports_cmx <- [| |];
  current_unit.ui_runtime_params <- [];
  current_unit.ui_curry_fun <- [];
  current_unit.ui_apply_fun <- [];
  current_unit.ui_send_fun <- [];
  current_unit.ui_force_link <- !Clflags.link_everything;
  Hashtbl.clear exported_constants;
  structured_constants := structured_constants_empty;
  current_unit.ui_export_info <- default_ui_export_info;
  merged_environment := Export_info.empty;
  Infos_table.clear export_infos_table

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
    let ui = (input_value ic : unit_infos) in
    let crc = Digest.input ic in
    close_in ic;
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

let equal_args (name1, value1) (name2, value2) =
  CU.equal name1 name2 && CU.equal value1 value2

let equal_up_to_pack_prefix cu1 cu2 =
  CU.Name.equal (CU.name cu1) (CU.name cu2)
  && List.equal equal_args (CU.instance_arguments cu1) (CU.instance_arguments cu2)

let get_unit_info comp_unit =
  (* If this fails, it likely means that someone didn't call
     [CU.which_cmx_file]. *)
  assert (CU.can_access_cmx_file comp_unit ~accessed_by:current_unit.ui_unit);
  (* CR lmaurer: Surely this should just compare [comp_unit] to
     [current_unit.ui_unit], but doing so seems to break Closure. We should fix
     that. *)
  if equal_up_to_pack_prefix comp_unit current_unit.ui_unit
  then
    Some current_unit
  else begin
    let name = CU.to_global_name_without_prefix comp_unit in
    try
      Infos_table.find global_infos_table name
    with Not_found ->
      let (infos, crc) =
        if Env.is_imported_opaque (CU.name comp_unit) then (None, None)
        else begin
          try
            let filename =
              Load_path.find_uncap (CU.base_filename comp_unit ^ ".cmx") in
            let (ui, crc) = read_unit_info filename in
            if not (CU.equal ui.ui_unit comp_unit) then
              raise(Error(Illegal_renaming(comp_unit, ui.ui_unit, filename)));
            (* Linking to a compilation unit expected to go into a
               pack is possible only from
               inside the same pack, but it is perfectly ok to link to
               an unit outside of the pack. *)
            let[@inline] for_pack_prefix unit =
              let prefix = CU.for_pack_prefix unit in
              if CU.Prefix.is_empty prefix then None else Some prefix
            in
            (match for_pack_prefix ui.ui_unit,
                   for_pack_prefix current_unit.ui_unit
             with
             | None, _ -> ()
             | Some p1, Some p2 when CU.Prefix.equal p1 p2 -> ()
             | Some p1, p2 ->
               raise (Error (Mismatching_for_pack
                        (filename, p1, CU.name current_unit.ui_unit, p2))));
            (Some ui, Some crc)
          with Not_found ->
            let warn = Warnings.No_cmx_file (Global.Name.to_string name) in
              Location.prerr_warning Location.none warn;
              (None, None)
          end
      in
      let import = Import_info.create_normal comp_unit ~crc in
      current_unit.ui_imports_cmx <-
        Array.append [| import |] current_unit.ui_imports_cmx;
      Infos_table.add global_infos_table name infos;
      infos
  end

let which_cmx_file desired_comp_unit =
  CU.which_cmx_file desired_comp_unit ~accessed_by:(CU.get_current_exn ())

let get_global_info global_ident =
  get_unit_info (which_cmx_file global_ident)

let cache_unit_info ui =
  Infos_table.add global_infos_table
    (ui.ui_unit |> CU.to_global_name_without_prefix) (Some ui)

(* Return the approximation of a global identifier *)

let get_clambda_approx ui =
  assert(not Config.flambda);
  match ui.ui_export_info with
  | Flambda _ -> assert false
  | Clambda approx -> approx

let toplevel_approx :
  (CU.t, Clambda.value_approximation) Hashtbl.t = Hashtbl.create 16

let record_global_approx_toplevel () =
  Hashtbl.add toplevel_approx
    current_unit.ui_unit
    (get_clambda_approx current_unit)

let global_approx id =
  try Hashtbl.find toplevel_approx id
  with Not_found ->
    match get_global_info id with
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
  | Clambda _ -> assert false
  | Flambda ei -> ei

let set_export_info export_info =
  assert(Config.flambda);
  current_unit.ui_export_info <- Flambda export_info

let approx_for_global comp_unit =
  if CU.equal comp_unit CU.predef_exn
  then invalid_arg "approx_for_global with predef_exn compilation unit";
  let accessible_comp_unit = which_cmx_file comp_unit in
  let name = accessible_comp_unit |> CU.to_global_name_without_prefix in
  match Infos_table.find export_infos_table name with
  | otherwise -> Some otherwise
  | exception Not_found ->
    match get_unit_info accessible_comp_unit with
    | None -> None
    | Some ui ->
      let exported = get_flambda_export_info ui in
      Infos_table.add export_infos_table name exported;
      merged_environment := Export_info.merge !merged_environment exported;
      Some exported

let approx_env () = !merged_environment

(* Record that a currying function or application function is needed *)

let need_curry_fun kind arity result =
  if not (List.mem (kind, arity, result) current_unit.ui_curry_fun) then
    current_unit.ui_curry_fun <-
      (kind, arity, result) :: current_unit.ui_curry_fun

let need_apply_fun arity result mode =
  assert(List.compare_length_with arity 0 > 0);
  if not (List.mem (arity, result, mode) current_unit.ui_apply_fun) then
    current_unit.ui_apply_fun <-
      (arity, result, mode) :: current_unit.ui_apply_fun

let need_send_fun arity result mode =
  if not (List.mem (arity, result, mode) current_unit.ui_send_fun) then
    current_unit.ui_send_fun <-
      (arity, result, mode) :: current_unit.ui_send_fun

(* Write the description of the current unit *)

let write_unit_info info filename =
  let oc = open_out_bin filename in
  output_string oc cmx_magic_number;
  output_value oc info;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc

let save_unit_info filename ~arg_block_field =
  current_unit.ui_imports_cmi <- Array.of_list (Env.imports());
  current_unit.ui_arg_descr <-
    begin match !Clflags.as_argument_for, arg_block_field with
    | Some arg_param, Some arg_block_field ->
      (* Currently, parameters don't have parameters, so we assume the argument
          list is empty *)
      let arg_param = Global.Name.create arg_param [] in
      Some { arg_param; arg_block_field }
    | None, None -> None
    | Some _, None -> Misc.fatal_error "No argument block"
    | None, Some _ -> Misc.fatal_error "Unexpected argument block"
  end;
  current_unit.ui_runtime_params <-
    Env.locally_bound_imports () |> List.map fst;
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
  | Mismatching_for_pack(filename, pack_1, current_unit, None) ->
      fprintf ppf "%a@ was built with -for-pack %a, but the \
                   @ current unit %a is not"
        Location.print_filename filename CU.Prefix.print pack_1
          CU.Name.print current_unit
  | Mismatching_for_pack(filename, pack_1, current_unit, Some pack_2) ->
      fprintf ppf "%a@ was built with -for-pack %a, but the \
                   @ current unit %a is built with -for-pack %a"
        Location.print_filename filename CU.Prefix.print pack_1
          CU.Name.print current_unit CU.Prefix.print pack_2

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
