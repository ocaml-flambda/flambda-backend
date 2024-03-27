(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Guillaume Bury, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2019--2019 OCamlPro SAS                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module C = Cmm_helpers
module String = Misc.Stdlib.String

type t =
  { gc_roots : Symbol.t list;
    data_list : Cmm.phrase list;
    functions : Cmm.fundecl list;
    current_data : Cmm.data_item list;
    reachable_names : Name_occurrences.t;
    symbols : Cmm.symbol String.Map.t;
    (* This map is only used for symbols not directly translated from
       [Symbol.t], e.g. module entry point names. *)
    module_symbol : Symbol.t;
    module_symbol_defined : bool;
    invalid_message_symbols : Symbol.t String.Map.t
  }

let create ~module_symbol ~reachable_names =
  { gc_roots = [];
    data_list = [];
    functions = [];
    current_data = [];
    reachable_names;
    symbols = String.Map.empty;
    module_symbol;
    module_symbol_defined = false;
    invalid_message_symbols = String.Map.empty
  }

(* Symbol handling

   These functions are there to ensure that a given symbol is: 1) given an
   appropriate locality, and 2) **always** given the same locality *)
let raw_symbol res ~global:sym_global sym_name : t * Cmm.symbol =
  match String.Map.find_opt sym_name res.symbols with
  | None ->
    let sym : Cmm.symbol = { sym_name; sym_global } in
    let symbols = String.Map.add sym_name sym res.symbols in
    { res with symbols }, sym
  | Some sym ->
    if Cmm.equal_is_global sym_global sym.sym_global
    then res, sym
    else
      Misc.fatal_errorf "The symbol %s is declared as both local and global"
        sym_name

let symbol res sym =
  let sym_name = Linkage_name.to_string (Symbol.linkage_name sym) in
  let sym_global =
    if Compilation_unit.is_current (Symbol.compilation_unit sym)
       && not (Name_occurrences.mem_symbol res.reachable_names sym)
    then Cmm.Local
    else Cmm.Global
  in
  let s : Cmm.symbol = { sym_name; sym_global } in
  s

let symbol_of_code_id res code_id : Cmm.symbol =
  let sym_name = Linkage_name.to_string (Code_id.linkage_name code_id) in
  let sym_global =
    if Compilation_unit.is_current (Code_id.get_compilation_unit code_id)
       && not (Name_occurrences.mem_code_id res.reachable_names code_id)
    then Cmm.Local
    else Cmm.Global
  in
  { sym_name; sym_global }

(* *)

let check_for_module_symbol t symbol =
  if Symbol.equal symbol t.module_symbol
  then (
    if t.module_symbol_defined
    then
      Misc.fatal_errorf
        "check_for_module_symbol %a: Module block symbol (%a) already defined"
        Symbol.print symbol Symbol.print t.module_symbol;
    { t with module_symbol_defined = true })
  else t

let defines_a_symbol data =
  match (data : Cmm.data_item) with
  | Cdefine_symbol _ -> true
  | Cint8 _ | Cint16 _ | Cint32 _ | Cint _ | Csingle _ | Cdouble _ | Cvec128 _
  | Csymbol_address _ | Csymbol_offset _ | Cstring _ | Cskip _ | Calign _ ->
    false

let add_to_data_list x l =
  match x with
  | [] -> l
  | _ :: _ ->
    if not (List.exists defines_a_symbol x)
    then
      Misc.fatal_errorf
        "data list does not define any symbol, its elements will be unusable: \
         %a"
        Printcmm.data x;
    C.cdata x :: l

let archive_data r =
  { r with
    current_data = [];
    data_list = add_to_data_list r.current_data r.data_list
  }

let update_data r f = { r with current_data = f r.current_data }

let set_data r l =
  update_data r (function
    | [] -> l
    | _ ->
      Misc.fatal_errorf "To_cmm_result.set_data: %s"
        "about to lose some translated static data items")

let add_archive_data_items r l =
  { r with data_list = add_to_data_list l r.data_list }

let add_gc_roots r l = { r with gc_roots = l @ r.gc_roots }

let add_function r f = { r with functions = f :: r.functions }

type result =
  { data_items : Cmm.phrase list;
    gc_roots : Cmm.symbol list;
    functions : Cmm.phrase list
  }

let define_module_symbol_if_missing r =
  if r.module_symbol_defined
  then r
  else
    let linkage_name =
      Linkage_name.to_string (Symbol.linkage_name r.module_symbol)
    in
    let sym : Cmm.symbol = { sym_name = linkage_name; sym_global = Global } in
    let l = C.emit_block sym (C.black_block_header 0 0) [] in
    set_data r l

let add_invalid_message_symbol t symbol ~message =
  { t with
    invalid_message_symbols =
      String.Map.add message symbol t.invalid_message_symbols
  }

let invalid_message_symbol t ~message =
  String.Map.find_opt message t.invalid_message_symbols

let to_cmm r =
  (* Make sure the module symbol is defined *)
  let r = define_module_symbol_if_missing r in
  (* Make sure we do not forget any current data *)
  let r = archive_data r in
  let sorted_functions =
    match !Flambda_backend_flags.function_layout with
    | Topological -> List.rev r.functions
    | Source ->
      (* Sort functions according to debuginfo, to get a stable ordering *)
      List.sort
        (fun (f1 : Cmm.fundecl) (f2 : Cmm.fundecl) ->
          Debuginfo.compare f1.fun_dbg f2.fun_dbg)
        r.functions
  in
  let function_phrases = List.map (fun f -> C.cfunction f) sorted_functions in
  (* Translate roots to Cmm symbols *)
  let roots = List.map (symbol r) r.gc_roots in
  (* Return the data list, gc roots and function declarations *)
  { data_items = r.data_list; gc_roots = roots; functions = function_phrases }
