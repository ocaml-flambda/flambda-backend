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

[@@@ocaml.warning "+a-30-40-41-42"]

module C = Cmm_helpers

type t =
  { gc_roots : Symbol.t list;
    data_list : Cmm.phrase list;
    offset_data_list : Cmm.data_item list;
    functions : Cmm.fundecl list;
    current_data : Cmm.data_item list;
    module_symbol : Symbol.t;
    module_symbol_defined : bool;
    offsets : Exported_offsets.t;
    next_symbol_offset : Targetint.t;
    data_symbol : Symbol.t
  }

let create ~module_symbol ~data_symbol offsets =
  { gc_roots = [];
    data_list = [];
    offset_data_list = [];
    functions = [];
    current_data = [];
    module_symbol;
    module_symbol_defined = false;
    offsets;
    next_symbol_offset = Targetint.of_int 0;
    data_symbol
  }

let record_symbol_offset t symbol ~size_in_words_excluding_header =
  let offsets =
    Exported_offsets.add_symbol_offset t.offsets symbol
      ~bytes:t.next_symbol_offset
  in
  Format.eprintf "OFFSET for %a = %a bytes\n%!" Symbol.print symbol
    Targetint.print t.next_symbol_offset;
  let next_symbol_offset =
    Targetint.add t.next_symbol_offset
      (Targetint.of_int (8 * (size_in_words_excluding_header + 1)))
  in
  { t with offsets; next_symbol_offset }

let increment_symbol_offset t ~size_in_words =
  let next_symbol_offset =
    Targetint.add t.next_symbol_offset (Targetint.of_int (8 * size_in_words))
  in
  Format.eprintf "increment_symbol_offset: %a -> %a\n%!" Targetint.print
    t.next_symbol_offset Targetint.print next_symbol_offset;
  { t with next_symbol_offset }

let is_module_symbol t symbol = Symbol.equal t.module_symbol symbol

let check_for_module_symbol t symbol =
  if Symbol.equal symbol t.module_symbol
  then begin
    if t.module_symbol_defined
    then
      Misc.fatal_errorf
        "check_for_module_symbol %a: Module block symbol (%a) already defined"
        Symbol.print symbol Symbol.print t.module_symbol;
    { t with module_symbol_defined = true }, true
  end
  else t, false

let defines_a_symbol data =
  match (data : Cmm.data_item) with
  | Cdefine_symbol _ -> true
  | Cglobal_symbol _ | Cint8 _ | Cint16 _ | Cint32 _ | Cint _ | Csingle _
  | Cdouble _ | Csymbol_address _ | Coffset_symbol_address _ | Cstring _
  | Cskip _ | Calign _ | Ccomment _ ->
    false

let add_to_data_list x l =
  match x with
  | [] -> l
  | _ :: _ ->
    if not (List.exists defines_a_symbol x)
    then
      Misc.fatal_errorf
        "data list does not define any symbol, its elements will be unusable: \
         %a\n\n\
         Backtrace:\n\
         %s\n"
        Printcmm.data x
        (Printexc.raw_backtrace_to_string (Printexc.get_callstack 100));
    C.cdata x :: l

let archive_data r =
  { r with
    current_data = [];
    data_list = add_to_data_list r.current_data r.data_list
  }

let filter_offset_data data =
  List.filter_map
    (fun (data_item : Cmm.data_item) : Cmm.data_item option ->
      match data_item with
      | Cdefine_symbol symbol -> Some (Ccomment symbol (* ^ ":" *))
      | Cglobal_symbol _ -> None
      | Cint8 _ | Cint16 _ | Cint32 _ | Cint _ | Csingle _ | Cdouble _
      | Csymbol_address _ | Coffset_symbol_address _ | Cstring _ | Cskip _
      | Calign _ | Ccomment _ ->
        Some data_item)
    data

let archive_offset_data r =
  (* CR mshinwell: Add [Ccomment] to [Cmm.data_item] for debugging. *)
  { r with
    current_data = [];
    offset_data_list = r.offset_data_list @ filter_offset_data r.current_data
  }

let update_data r f = { r with current_data = f r.current_data }

let set_data r l =
  update_data r (function
    | [] -> l
    | _ ->
      Misc.fatal_errorf "To_cmm_result.set_data: %s"
        "about to lose some translated static data items")

let add_gc_roots r l = { r with gc_roots = l @ r.gc_roots }

let add_function r f = { r with functions = f :: r.functions }

let is_module_symbol' symbol =
  let comp_unit = Compilation_unit.name (Symbol.compilation_unit symbol) in
  let linkage_name = Symbol.linkage_name_as_string symbol in
  String.equal ("caml" ^ comp_unit) linkage_name

let symbol_offset_in_bytes t symbol =
  match Exported_offsets.symbol_offset_in_bytes t.offsets symbol with
  | Some bytes ->
    (* Format.eprintf "LOCAL Symbol %a can be reached via offset %a\n"
       Symbol.print symbol Targetint.print bytes; *)
    Some bytes
  | None -> (
    match
      Exported_offsets.symbol_offset_in_bytes
        (Exported_offsets.imported_offsets ())
        symbol
    with
    | Some bytes ->
      (* CR mshinwell: To cope with missing .cmx files, this offset now needs to
         go in [t.offsets]. *)
      (* Format.eprintf "IMPORTED Symbol %a can be reached via offset %a\n"
         Symbol.print symbol Targetint.print bytes; *)
      Some bytes
    | None ->
      if (not (Symbol.is_predefined_exception symbol))
         && not (is_module_symbol' symbol)
      then
        Misc.fatal_errorf "Cannot find offset for symbol %a, backtrace:@ \n%s"
          Symbol.print symbol
          (Printexc.raw_backtrace_to_string (Printexc.get_callstack 20));
      None)

let data_symbol_for_unit comp_unit =
  Symbol.create comp_unit (Linkage_name.create "data_symbol")

let static_symbol_address t symbol : Cmm.data_item =
  match symbol_offset_in_bytes t symbol with
  | None -> C.symbol_address (Symbol.linkage_name_as_string symbol)
  | Some bytes ->
    let data_symbol = data_symbol_for_unit (Symbol.compilation_unit symbol) in
    if Targetint.equal bytes Targetint.zero
    then C.symbol_address (Symbol.linkage_name_as_string data_symbol)
    else
      C.offset_symbol_address (Symbol.linkage_name_as_string data_symbol) ~bytes

let expr_symbol_address t symbol dbg : Cmm.expression =
  match symbol_offset_in_bytes t symbol with
  | None -> C.symbol_from_string ~dbg (Symbol.linkage_name_as_string symbol)
  | Some bytes ->
    let sym_expr =
      C.symbol_from_string ~dbg
        (Symbol.linkage_name_as_string
           (data_symbol_for_unit (Symbol.compilation_unit symbol)))
    in
    if Targetint.equal bytes Targetint.zero
    then sym_expr
    else
      let bytes = Int64.to_nativeint (Targetint.to_int64 bytes) in
      (* Beware: this must be all in machine integers, not tagged integers. *)
      Cop (Caddi, [sym_expr; Cconst_natint (bytes, dbg)], dbg)

type result =
  { data_items : Cmm.phrase list;
    gc_roots : Symbol.t list;
    functions : Cmm.phrase list;
    offsets : Exported_offsets.t
  }

let define_module_symbol_if_missing r =
  if r.module_symbol_defined
  then r
  else
    let linkage_name =
      Linkage_name.to_string (Symbol.linkage_name r.module_symbol)
    in
    let l = C.emit_block (linkage_name, Global) (C.black_block_header 0 0) [] in
    set_data r l

let to_cmm r =
  (* Make sure the module symbol is defined *)
  let r = define_module_symbol_if_missing r in
  (* Make sure we do not forget any current data *)
  let r = archive_data r in
  (* Sort functions according to debuginfo, to get a stable ordering *)
  let sorted_functions =
    List.sort
      (fun (f1 : Cmm.fundecl) (f2 : Cmm.fundecl) ->
        Debuginfo.compare f1.fun_dbg f2.fun_dbg)
      r.functions
  in
  let function_phrases = List.map (fun f -> C.cfunction f) sorted_functions in
  let data_items =
    match r.offset_data_list with
    | [] -> r.data_list
    | header :: rest -> (
      (* Move the first block header prior to the data symbol definition. *)
      match header with
      | Cint _ ->
        C.cdata
          (header
           :: C.define_symbol ~global:true
                (Symbol.linkage_name_as_string r.data_symbol)
          @ rest)
        :: r.data_list
      | Cdefine_symbol _ | Cglobal_symbol _ | Cint8 _ | Cint16 _ | Cint32 _
      | Csingle _ | Cdouble _ | Csymbol_address _ | Coffset_symbol_address _
      | Cstring _ | Cskip _ | Calign _ | Ccomment _ ->
        Misc.fatal_errorf
          "Malformed [offset_data_list], expected block header to begin:@ %a"
          Printcmm.data r.offset_data_list)
  in
  (match Sys.getenv "OFFSETS" with
  | exception Not_found -> ()
  | _ ->
    Format.eprintf "Offsets:@ \n%a\n\n%!" Exported_offsets.print r.offsets;
    Format.eprintf "Data items:@ \n";
    let _offset =
      List.fold_left
        (fun offset (phrase : Cmm.phrase) ->
          match phrase with
          | Cfunction _ -> assert false
          | Cdata items ->
            List.fold_left
              (fun offset (item : Cmm.data_item) ->
                let next_offset =
                  match item with
                  | Cdefine_symbol _ | Cglobal_symbol _ | Ccomment _ -> offset
                  | Cint8 _ -> offset + 1
                  | Cint16 _ -> offset + 2
                  | Cint32 _ -> offset + 4
                  | Cint _ -> offset + 8
                  | Csingle _ -> offset + 4
                  | Cdouble _ -> offset + 8
                  | Csymbol_address _ | Coffset_symbol_address _ -> offset + 8
                  | Cstring s -> String.length s + offset
                  | Cskip n -> offset + n
                  | Calign _ -> Misc.fatal_error "Calign unsupported"
                in
                Format.eprintf "Offset dec. %06d: %a\n" offset Printcmm.data
                  [item];
                next_offset)
              offset items)
        (-8) data_items
    in
    Format.eprintf "\n\n");
  let module String = Misc.Stdlib.String in
  let _offset, cmm_offsets =
    List.fold_left
      (fun (offset, cmm_offsets) (phrase : Cmm.phrase) ->
        match phrase with
        | Cfunction _ -> assert false
        | Cdata items ->
          List.fold_left
            (fun (offset, cmm_offsets) (item : Cmm.data_item) ->
              let next_offset, cmm_offsets =
                match item with
                | Cdefine_symbol _ | Cglobal_symbol _ -> offset, cmm_offsets
                | Ccomment sym_name ->
                  let cmm_offsets =
                    String.Map.add sym_name offset cmm_offsets
                  in
                  offset, cmm_offsets
                | Cint8 _ -> offset + 1, cmm_offsets
                | Cint16 _ -> offset + 2, cmm_offsets
                | Cint32 _ -> offset + 4, cmm_offsets
                | Cint _ -> offset + 8, cmm_offsets
                | Csingle _ -> offset + 4, cmm_offsets
                | Cdouble _ -> offset + 8, cmm_offsets
                | Csymbol_address _ | Coffset_symbol_address _ ->
                  offset + 8, cmm_offsets
                | Cstring s -> String.length s + offset, cmm_offsets
                | Cskip n -> offset + n, cmm_offsets
                | Calign _ -> Misc.fatal_error "Calign unsupported"
              in
              next_offset, cmm_offsets)
            (offset, cmm_offsets) items)
      (-8, String.Map.empty) data_items
  in
  let exported_sym_offsets =
    Exported_offsets.symbol_offsets r.offsets
    |> Symbol.Map.bindings
    |> List.sort (fun (_sym1, offset1) (_sym2, offset2) ->
           Targetint.compare offset1 offset2)
  in
  List.iter
    (fun (sym, offset) ->
      let offset = Targetint.to_int offset in
      let sym_name = Symbol.linkage_name_as_string sym in
      match String.Map.find sym_name cmm_offsets with
      | exception Not_found ->
        Format.eprintf "Can't find: %a\n%!" Symbol.print sym
      | offset' ->
        if offset <> offset'
        then
          Misc.fatal_errorf
            "Offset for symbol %a is %d in exported offsets but %d in Cmm data \
             list"
            Symbol.print sym offset offset')
    exported_sym_offsets;
  { data_items;
    gc_roots = r.gc_roots;
    functions = function_phrases;
    offsets = r.offsets
  }
