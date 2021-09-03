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

type error =
    Not_a_unit_info of string
  | Corrupted_unit_info of string
  | Illegal_renaming of string * string * string

exception Error of error

let global_infos_table =
  (Hashtbl.create 17 : (string, unit_infos option) Hashtbl.t)
let export_infos_table =
  (Hashtbl.create 10 : (string, Export_info.t) Hashtbl.t)

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
    Cmx_format.Flambda1 Export_info.empty
  else if Config.flambda2 then
    Cmx_format.Flambda2 None
  else
    Cmx_format.Clambda Value_unknown

let current_unit =
  { ui_name = "";
    ui_symbol = "";
    ui_defines = [];
    ui_imports_cmi = [];
    ui_imports_cmx = [];
    ui_curry_fun = [];
    ui_apply_fun = [];
    ui_send_fun = [];
    ui_force_link = false;
    ui_export_info = default_ui_export_info }

let symbolname_for_pack pack name =
  match pack with
  | None -> name
  | Some p ->
      let b = Buffer.create 64 in
      for i = 0 to String.length p - 1 do
        match p.[i] with
        | '.' -> Buffer.add_string b "__"
        |  c  -> Buffer.add_char b c
      done;
      Buffer.add_string b "__";
      Buffer.add_string b name;
      Buffer.contents b

let unit_id_from_name name = Ident.create_persistent name

let concat_symbol unitname id =
  unitname ^ "__" ^ id

let make_symbol ?(unitname = current_unit.ui_symbol) idopt =
  let prefix = "caml" ^ unitname in
  match idopt with
  | None -> prefix
  | Some id -> concat_symbol prefix id

let begins_with ?(from = 0) str prefix = 
  let rec helper idx =
    if idx < 0 then true
    else 
      String.get str (from + idx) = String.get prefix idx && helper (idx-1)
  in
  let n = String.length str in
  let m = String.length prefix in
  if n >= from + m then helper (m-1) else false

let split_on_string str split = 
  let n = String.length str in
  let m = String.length split in
  let rec helper acc last_idx idx = 
    if idx = n then
      let cur = String.sub str last_idx (idx - last_idx) in
      List.rev (cur :: acc)
    else if begins_with ~from:idx str split then
      let cur = String.sub str last_idx (idx - last_idx) in
      helper (cur :: acc) (idx + m) (idx + m)
    else
      helper acc last_idx (idx + 1)
  in
  helper [] 0 0

let split_on_chars str chars =
  let rec helper chars_left s acc = 
    match chars_left with
    | [] -> s :: acc
    | c :: cs -> 
      List.fold_right (helper cs) (String.split_on_char c s) acc
  in 
  helper chars str []

let split_last_exn str c = 
  let n = String.length str in
  let ridx = String.rindex str c in
  (String.sub str 0 ridx, String.sub str (ridx + 1) (n - ridx - 1))

let escape_symbols part = 
  let buf = Buffer.create 16 in
  let was_hex_last = ref false in
  let handle_char = function 
    | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') as c ->
      if !was_hex_last then Buffer.add_string buf "__";
      Buffer.add_char buf c;
      was_hex_last := false
    | c ->
      Printf.bprintf buf "%sX%02x"
        (if !was_hex_last then "_" else "__")
        (Char.code c);
      was_hex_last := true
  in
  String.iter handle_char part;
  Buffer.contents buf

[@@@ocaml.warning "-37"]

type binop =
  | Slash
  | Minus

type expression = 
  | String of string
  | Integer of int
  | Binop of binop * expression * expression
  | Dot of expression * string

type cpp_name =
  | Simple of string
  | Scoped of cpp_name list
  | Templated of string * template_arg list

and template_arg =
  | Cpp_name of cpp_name
  | Expression of expression

let mangle_cpp name =
  let with_length s =
    let s = escape_symbols s in
    Printf.sprintf "%d%s" (String.length s) s in
  let binop_name = function
    | Slash -> "dv"
    | Minus -> "mi"
  in
  let rec mangle_expression = function
    | String s -> with_length s
    | Integer n -> Printf.sprintf "Li%dE" n
    | Dot (e, name) -> Printf.sprintf "dt%s%s" (mangle_expression e) (with_length name)
    | Binop (op, e1, e2) ->
      Printf.sprintf "%s%s%s" (binop_name op) (mangle_expression e1) (mangle_expression e2) 
  in 
  let rec mangle_name = function
    | Simple s -> with_length s
    | Scoped names ->
      let s = List.map mangle_name names |> String.concat "" in
      Printf.sprintf "N%sE" s
    | Templated (str, parts) ->
      let s = List.map mangle_arg parts |> String.concat "" in
      Printf.sprintf "%sI%sE" (with_length str) s
  and mangle_arg = function
    | Cpp_name name -> mangle_name name
    | Expression expression ->
      Printf.sprintf "X%sE" (mangle_expression expression)
  in
  "_Z" ^ mangle_name name

(* Split module names where '__' should represent a nested scope *)
let namespace_parts name =
  split_on_string name "__" |> List.map (fun part -> Simple part)

let file_template_arg file =
  (* Take the file name only *)
  let filename =
    if String.contains file '/' then snd (split_last_exn file '/')
    else file
  in
  match String.split_on_char '.' filename with
  | [] -> Misc.fatal_error "Empty split"
  | hd :: tl ->
    let expr = List.fold_left (fun e x -> Dot (e, x)) (String hd) tl in
    Expression expr

let name_op = function
  | "+" -> "PLUS"
  | "++" -> "PLUSPLUS"
  | "+." -> "PLUSDOT"
  | "+=" -> "PLUSEQ"
  | "-" -> "MINUS"
  | "-." -> "MINUSDOT"
  | "*" -> "STAR"
  | "%" -> "PERCENT"
  | "=" -> "EQUAL"
  | "<" -> "LESS"
  | ">" -> "GREATER"
  | "<>" -> "NOTEQUAL"
  | "||" -> "BARBAR"
  | "&" -> "AMPERSAND"
  | "&&" -> "AMPERAMPER"
  | ":=" -> "COLONEQUAL"
  | "^" -> "CARET"
  | "^^" -> "CARETCARET"
  | "@" -> "AT"
  | "<<" -> "LSHIFT"
  | ">>" -> "RSHIFT"
  | op -> op
  
let build_location_info loc =
  let loc = Debuginfo.Scoped_location.to_location loc in
  let (file, line, startchar) = Location.get_pos_info loc.loc_start in
  let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_bol in
  let line_str = Printf.sprintf "ln_%d" line in
  let info = [ file_template_arg file; Cpp_name (Simple line_str) ] in
  if startchar >= 0 then
    let char_str = Printf.sprintf "ch_%d_to_%d" startchar endchar in
    info @ [ Cpp_name (Simple char_str) ]
  else info

(* OCaml names can contain single quotes but need to be escaped
   for C++ identifiers. *)
let convert_identifier str =
  match String.split_on_char '\'' str with
  | [] -> Misc.fatal_error "empty split"
  | [ s ] -> Simple s
  | parts -> 
    let s = String.concat "_Q" parts in
    Templated (s, [ Cpp_name (Simple "quoted")] )

let convert_closure_id id loc = 
  if begins_with id "anon_fn[" then
    (* Keep the unique integer stamp *)
    let (_init, stamp) = split_last_exn id '_' in
    (* Put the location inside C++ template args *)
    Templated ("anon_fn_" ^ stamp, build_location_info loc)
  else
    match id.[0] with
    (* A regular identifier *)
    | ('A'..'Z' | 'a'..'z' | '0'..'9' | '_') -> convert_identifier id
    (* An operator *)
    | _op -> 
      let (op, stamp) = split_last_exn id '_' in
      Templated ("op_" ^ stamp, [Cpp_name (Simple (name_op op))])
  
let convert_scope scope =
  let n = String.length scope in
  (* anonymous function *)
  if String.equal scope "(fun)" then Templated ("anon_fn", [])
  (* operators *)
  else if n > 2 && String.get scope 0 = '(' && String.get scope (n - 1) = ')' then
    let op = String.sub scope 1 (n - 2) in
    Templated ("op", [ Cpp_name (Simple (name_op op)) ])
  (* regular identifiers *)
  else convert_identifier scope

let list_of_scopes scopes = 
  (* Works for now since the only separators are '.' and '#' *)
  let scope_str = Debuginfo.Scoped_location.string_of_scopes scopes in
  split_on_chars scope_str [ '.'; '#' ]

let scope_matches_closure_id scope closure_id = 
  (* If the `id` is an anonymous function this corresponds to that,
      and, even if not, then the function has likely been given
      a name via some aliasing (e.g. `let f = fun x -> ...`) *)
  String.equal scope "(fun)" ||
  (* Normal case where closure id and scope match directly *)
  begins_with closure_id scope ||
  (* For operators, the scope is wrapped in parens *)
  ( String.length scope >= 3 &&
    begins_with closure_id (String.sub scope 1 (String.length scope - 2)))

let symbol_parts ~unitname loc id =
  match (loc : Debuginfo.Scoped_location.t) with
  | Loc_known { loc = _; scopes } ->
    let scopes = list_of_scopes scopes in
    (* Remove last scope if it matches closure id *)
    let scopes = 
      match List.rev scopes with
      | [] -> Misc.fatal_errorf "No location - %s %s" unitname id
      | last_scope :: rest when scope_matches_closure_id last_scope id -> 
        List.rev rest
      | _ -> scopes
    in
    (* If the scope is now empty, use the unitname as the top-level module *)
    let top_level_module, sub_scopes = 
      match scopes with
      | [] -> unitname, []
      | hd :: tl -> hd, tl
    in
    let parts =
      List.concat [
        namespace_parts top_level_module;
        List.map convert_scope sub_scopes;
        [ convert_closure_id id loc ]
      ]
    in
    if String.equal top_level_module unitname then
      parts 
    else
      parts @ [ Templated ("inlined_in", [ Cpp_name (Scoped (namespace_parts unitname)) ])]
  | Loc_unknown ->
    namespace_parts unitname @ [ convert_closure_id id loc ]

let make_fun_symbol ?(unitname = current_unit.ui_symbol) loc id =
  (* Remove caml *)
  if not (begins_with unitname "caml") then
    Misc.fatal_error "unitname expected to begin with caml";
  let unitname = String.sub unitname 4 (String.length unitname - 4) in
  let parts = symbol_parts ~unitname loc id in
  mangle_cpp (Scoped parts)

let current_unit_linkage_name () =
  Linkage_name.create (make_symbol ~unitname:current_unit.ui_symbol None)

let reset ?packname name =
  Hashtbl.clear global_infos_table;
  Set_of_closures_id.Tbl.clear imported_sets_of_closures_table;
  let symbol = symbolname_for_pack packname name in
  current_unit.ui_name <- name;
  current_unit.ui_symbol <- symbol;
  current_unit.ui_defines <- [symbol];
  current_unit.ui_imports_cmi <- [];
  current_unit.ui_imports_cmx <- [];
  current_unit.ui_curry_fun <- [];
  current_unit.ui_apply_fun <- [];
  current_unit.ui_send_fun <- [];
  current_unit.ui_force_link <- !Clflags.link_everything;
  Hashtbl.clear exported_constants;
  structured_constants := structured_constants_empty;
  current_unit.ui_export_info <- default_ui_export_info;
  merged_environment := Export_info.empty;
  Hashtbl.clear export_infos_table;
  let compilation_unit =
    Compilation_unit.create
      (Ident.create_persistent name)
      (current_unit_linkage_name ())
  in
  Compilation_unit.set_current compilation_unit;
  (* The Flambda 2 current compilation unit must be set separately
     since a different set of types are used. *)
  let module Compilation_unit = Flambda2_compilenv_deps.Compilation_unit in
  let module Linkage_name = Flambda2_compilenv_deps.Linkage_name in
  let compilation_unit =
    Compilation_unit.create
      (Ident.create_persistent name)
      (Linkage_name.create (make_symbol ~unitname:current_unit.ui_symbol None))
  in
  Compilation_unit.set_current compilation_unit

let current_unit_infos () =
  current_unit

let current_unit_name () =
  current_unit.ui_name

let symbol_in_current_unit name =
  let prefix = "caml" ^ current_unit.ui_symbol in
  name = prefix ||
  (let lp = String.length prefix in
   String.length name >= 2 + lp
   && String.sub name 0 lp = prefix
   && name.[lp] = '_'
   && name.[lp + 1] = '_')

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

let get_global_info global_ident = (
  let modname = Ident.name global_ident in
  if modname = current_unit.ui_name then
    Some current_unit
  else begin
    try
      Hashtbl.find global_infos_table modname
    with Not_found ->
      let (infos, crc) =
        if Env.is_imported_opaque modname then (None, None)
        else begin
          try
            let filename =
              Load_path.find_uncap (modname ^ ".cmx") in
            let (ui, crc) = read_unit_info filename in
            if ui.ui_name <> modname then
              raise(Error(Illegal_renaming(modname, ui.ui_name, filename)));
            (Some ui, Some crc)
          with Not_found ->
            let warn = Warnings.No_cmx_file modname in
              Location.prerr_warning Location.none warn;
              (None, None)
          end
      in
      current_unit.ui_imports_cmx <-
        (modname, crc) :: current_unit.ui_imports_cmx;
      Hashtbl.add global_infos_table modname infos;
      infos
  end
)

let get_global_info' id =
  match get_global_info id with
  | None -> None
  | Some ui -> Some ui.ui_export_info

let cache_unit_info ui =
  Hashtbl.add global_infos_table ui.ui_name (Some ui)

(* Return the approximation of a global identifier *)

let get_clambda_approx ui =
  assert(not Config.flambda);
  match ui.ui_export_info with
  | Flambda1 _ | Flambda2 _ -> assert false
  | Clambda approx -> approx

let toplevel_approx :
  (string, Clambda.value_approximation) Hashtbl.t = Hashtbl.create 16

let record_global_approx_toplevel () =
  Hashtbl.add toplevel_approx current_unit.ui_name
    (get_clambda_approx current_unit)

let global_approx id =
  if Ident.is_predef id then Clambda.Value_unknown
  else try Hashtbl.find toplevel_approx (Ident.name id)
  with Not_found ->
    match get_global_info id with
      | None -> Clambda.Value_unknown
      | Some ui -> get_clambda_approx ui

(* Return the symbol used to refer to a global identifier *)

let symbol_for_global id =
  if Ident.is_predef id then
    "caml_exn_" ^ Ident.name id
  else begin
    let unitname = Ident.name id in
    match
      try ignore (Hashtbl.find toplevel_approx unitname); None
      with Not_found -> get_global_info id
    with
    | None -> make_symbol ~unitname:(Ident.name id) None
    | Some ui -> make_symbol ~unitname:ui.ui_symbol None
  end

(* Register the approximation of the module being compiled *)

let unit_for_global id =
  let sym_label = Linkage_name.create (symbol_for_global id) in
  Compilation_unit.create id sym_label

let predefined_exception_compilation_unit =
  Compilation_unit.create (Ident.create_persistent "__dummy__")
    (Linkage_name.create "__dummy__")

let is_predefined_exception sym =
  Compilation_unit.equal
    predefined_exception_compilation_unit
    (Symbol.compilation_unit sym)

let symbol_for_global' id =
  let sym_label = Linkage_name.create (symbol_for_global id) in
  if Ident.is_predef id then
    Symbol.of_global_linkage predefined_exception_compilation_unit sym_label
  else
    Symbol.of_global_linkage (unit_for_global id) sym_label

let set_global_approx approx =
  assert(not Config.flambda);
  current_unit.ui_export_info <- Clambda approx

(* Exporting and importing cross module information *)

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
  let id = Compilation_unit.get_persistent_ident comp_unit in
  if (Compilation_unit.equal
      predefined_exception_compilation_unit
      comp_unit)
     || Ident.is_predef id
     || not (Ident.global id)
  then invalid_arg (Format.asprintf "approx_for_global %a" Ident.print id);
  let modname = Ident.name id in
  match Hashtbl.find export_infos_table modname with
  | otherwise -> Some otherwise
  | exception Not_found ->
    match get_global_info id with
    | None -> None
    | Some ui ->
      let exported = get_flambda_export_info ui in
      Hashtbl.add export_infos_table modname exported;
      merged_environment := Export_info.merge !merged_environment exported;
      Some exported

let approx_env () = !merged_environment

(* Record that a currying function or application function is needed *)

let need_curry_fun n =
  if not (List.mem n current_unit.ui_curry_fun) then
    current_unit.ui_curry_fun <- n :: current_unit.ui_curry_fun

let need_apply_fun n =
  assert(n > 0);
  if not (List.mem n current_unit.ui_apply_fun) then
    current_unit.ui_apply_fun <- n :: current_unit.ui_apply_fun

let need_send_fun n =
  if not (List.mem n current_unit.ui_send_fun) then
    current_unit.ui_send_fun <- n :: current_unit.ui_send_fun

(* Write the description of the current unit *)

let write_unit_info info filename =
  let oc = open_out_bin filename in
  output_string oc cmx_magic_number;
  output_value oc info;
  flush oc;
  let crc = Digest.file filename in
  Digest.output oc crc;
  close_out oc

let save_unit_info filename =
  current_unit.ui_imports_cmi <- Env.imports();
  write_unit_info current_unit filename

let current_unit () =
  match Compilation_unit.get_current () with
  | Some current_unit -> current_unit
  | None -> Misc.fatal_error "Compilenv.current_unit"

let current_unit_symbol () =
  Symbol.of_global_linkage (current_unit ()) (current_unit_linkage_name ())

let const_label = ref 0

let new_const_symbol () =
  incr const_label;
  make_symbol (Some (Int.to_string !const_label))

let snapshot () = !structured_constants
let backtrack s = structured_constants := s

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
        Path.Pident (Ident.create_persistent (current_unit_name ()));
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

let function_label closure_id =
  let unitname =
    Closure_id.get_compilation_unit closure_id
    |> Compilation_unit.get_linkage_name
    |> Linkage_name.to_string
  in
  let name = Closure_id.unique_name closure_id in
  match Closure_id.debug_info closure_id with
  | None | Some [] ->
    (* concat_symbol unitname name *)
    let scoped_loc = Debuginfo.Scoped_location.Loc_unknown in
    make_fun_symbol ~unitname scoped_loc name
  | Some ((item :: _items) as debug_info) ->
    let scoped_loc =
      Debuginfo.Scoped_location.Loc_known
        { loc = Debuginfo.to_location debug_info
        ; scopes = item.dinfo_scopes
        }
    in
    make_fun_symbol ~unitname scoped_loc name

let closure_symbol closure_id =
  let compilation_unit = Closure_id.get_compilation_unit closure_id in
  let linkage_name = (function_label closure_id) ^ "_closure" in
  Symbol.of_global_linkage compilation_unit (Linkage_name.create linkage_name)

let require_global global_ident =
  if not (Ident.is_predef global_ident) then
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
                   @ %s when %s was expected"
        Location.print_filename filename name modname

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
