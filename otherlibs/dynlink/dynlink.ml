#3 "otherlibs/dynlink/dynlink.ml"
(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*              Mark Shinwell and Leo White, Jane Street Europe           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*   Copyright 2017--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Dynlink_compilerlibs

module DC = Dynlink_common
module DT = Dynlink_types

module Bytecode = struct
  type filename = string

  module Unit_header = struct
    type t = Cmo_format.compilation_unit

    let name (t : t) = t.cu_name
    let crc _t = None

    let interface_imports (t : t) = t.cu_imports
    let implementation_imports (t : t) =
      let required =
        t.cu_required_globals
        @ Symtable.required_globals t.cu_reloc
      in
      let required =
        List.filter
          (fun id ->
             not (Ident.is_predef id)
             && not (String.contains (Ident.name id) '.'))
          required
      in
      List.map
        (fun ident -> Ident.name ident, None)
        required

    let defined_symbols (t : t) =
      List.map (fun ident -> Ident.name ident)
        (Symtable.defined_globals t.cu_reloc)

    let unsafe_module (t : t) = t.cu_primitives <> []
  end

  type handle = Stdlib.in_channel * filename * Digest.t

  let default_crcs = ref []
  let default_global_map = ref Symtable.empty_global_map

  let init () =
    if !Sys.interactive then begin (* PR#6802 *)
      invalid_arg "The dynlink.cma library cannot be used \
        inside the OCaml toplevel"
    end;
    default_crcs := Symtable.init_toplevel ();
    default_global_map := Symtable.current_state ()

  let is_native = false
  let adapt_filename f = f

  let num_globals_inited () =
    Misc.fatal_error "Should never be called for bytecode dynlink"

  let fold_initial_units ~init ~f =
    List.fold_left (fun acc (comp_unit, interface) ->
        let id = Ident.create_persistent comp_unit in
        let defined =
          Symtable.is_defined_in_global_map !default_global_map id
        in
        let implementation =
          if defined then Some (None, DT.Loaded)
          else None
        in
        let defined_symbols =
          if defined then [comp_unit]
          else []
        in
        f acc ~comp_unit ~interface ~implementation ~defined_symbols)
      init
      !default_crcs

  let run_shared_startup _ = ()

  let run (ic, file_name, file_digest) ~unit_header ~priv =
    let open Misc in
    let old_state = Symtable.current_state () in
    let compunit : Cmo_format.compilation_unit = unit_header in
    seek_in ic compunit.cu_pos;
    let code_size = compunit.cu_codesize + 8 in
    let code = LongString.create code_size in
    LongString.input_bytes_into code ic compunit.cu_codesize;
    LongString.set code compunit.cu_codesize (Char.chr Opcodes.opRETURN);
    LongString.blit_string "\000\000\000\001\000\000\000" 0
      code (compunit.cu_codesize + 1) 7;
    begin try
      Symtable.patch_object code compunit.cu_reloc;
      Symtable.check_global_initialized compunit.cu_reloc;
      Symtable.update_global_table ()
    with Symtable.Error error ->
      let new_error : DT.linking_error =
        match error with
        | Symtable.Undefined_global s -> Undefined_global s
        | Symtable.Unavailable_primitive s -> Unavailable_primitive s
        | Symtable.Uninitialized_global s -> Uninitialized_global s
        | Symtable.Wrong_vm _ -> assert false
      in
      raise (DT.Error (Linking_error (file_name, new_error)))
    end;
    (* PR#5215: identify this code fragment by
       digest of file contents + unit name.
       Unit name is needed for .cma files, which produce several code
       fragments. *)
    let digest = Digest.string (file_digest ^ compunit.cu_name) in
    let events =
      if compunit.cu_debug = 0 then [| |]
      else begin
        seek_in ic compunit.cu_debug;
        [| input_value ic |]
      end in
    if priv then Symtable.hide_additions old_state;
    let _, clos = Meta.reify_bytecode code events (Some digest) in
    try ignore ((clos ()) : Obj.t)
    with exn ->
      Printexc.raise_with_backtrace
        (DT.Error (Library's_module_initializers_failed exn))
        (Printexc.get_raw_backtrace ())

  let load ~filename:file_name ~priv:_ =
    let ic = open_in_bin file_name in
    let file_digest = Digest.channel ic (-1) in
    seek_in ic 0;
    try
      let buffer =
        try really_input_string ic (String.length Config.cmo_magic_number)
        with End_of_file -> raise (DT.Error (Not_a_bytecode_file file_name))
      in
      let handle = ic, file_name, file_digest in
      if buffer = Config.cmo_magic_number then begin
        let compunit_pos = input_binary_int ic in  (* Go to descriptor *)
        seek_in ic compunit_pos;
        let cu = (input_value ic : Cmo_format.compilation_unit) in
        handle, [cu]
      end else
      if buffer = Config.cma_magic_number then begin
        let toc_pos = input_binary_int ic in  (* Go to table of contents *)
        seek_in ic toc_pos;
        let lib = (input_value ic : Cmo_format.library) in
        begin try
          Dll.open_dlls Dll.For_execution
            (List.map Dll.extract_dll_name lib.lib_dllibs)
        with exn ->
          raise (DT.Error (Cannot_open_dynamic_library exn))
        end;
        handle, lib.lib_units
      end else begin
        raise (DT.Error (Not_a_bytecode_file file_name))
      end
    with exc ->
      close_in ic;
      raise exc

  let unsafe_get_global_value ~bytecode_or_asm_symbol =
    let id = Ident.create_persistent bytecode_or_asm_symbol in
    match Symtable.get_global_value id with
    | exception _ -> None
    | obj -> Some obj

  let finish (ic, _filename, _digest) =
    close_in ic
end

module B = DC.Make (Bytecode)

type global_map = {
  name : string;
  crc_intf : Digest.t option;
  crc_impl : Digest.t option;
  syms : string list
}

module Native = struct
  type handle

  (* mshinwell: We need something better than caml_sys_exit *)
  external ndl_open : string -> bool -> handle * Cmxs_format.dynheader
    = "caml_sys_exit" "caml_natdynlink_open"
  external ndl_run : handle -> string -> unit
    = "caml_sys_exit" "caml_natdynlink_run"
  external ndl_getmap : unit -> global_map list
    = "caml_sys_exit" "caml_natdynlink_getmap"
  external ndl_globals_inited : unit -> int
    = "caml_sys_exit" "caml_natdynlink_globals_inited"
  external ndl_loadsym : string -> Obj.t
    = "caml_sys_exit" "caml_natdynlink_loadsym"

  module Unit_header = struct
    type t = Cmxs_format.dynunit

    let name (t : t) = t.dynu_name
    let crc (t : t) = Some t.dynu_crc

    let interface_imports (t : t) = t.dynu_imports_cmi
    let implementation_imports (t : t) = t.dynu_imports_cmx

    let defined_symbols (t : t) = t.dynu_defines
    let unsafe_module _t = false
  end

  let init () = ()

  let is_native = true
  let adapt_filename f = Filename.chop_extension f ^ ".cmxs"

  let num_globals_inited () = ndl_globals_inited ()

  let fold_initial_units ~init ~f =
    let rank = ref 0 in
    List.fold_left (fun acc { name; crc_intf; crc_impl; syms; } ->
        rank := !rank + List.length syms;
        let implementation =
          match crc_impl with
          | None -> None
          | Some _ as crco -> Some (crco, DT.Check_inited !rank)
        in
        f acc ~comp_unit:name ~interface:crc_intf
            ~implementation ~defined_symbols:syms)
      init
      (ndl_getmap ())

  let run_shared_startup handle =
    ndl_run handle "_shared_startup"

  let run handle ~unit_header ~priv:_ =
    List.iter (fun cu ->
        try ndl_run handle cu
        with exn ->
          Printexc.raise_with_backtrace
            (DT.Error (Library's_module_initializers_failed exn))
            (Printexc.get_raw_backtrace ()))
      (Unit_header.defined_symbols unit_header)

  let load ~filename ~priv =
    let handle, header =
      try ndl_open filename (not priv)
      with exn -> raise (DT.Error (Cannot_open_dynamic_library exn))
    in
    if header.dynu_magic <> Config.cmxs_magic_number then begin
      raise (DT.Error (Not_a_bytecode_file filename))
    end;
    handle, header.dynu_units

  let unsafe_get_global_value ~bytecode_or_asm_symbol =
    match ndl_loadsym bytecode_or_asm_symbol with
    | exception _ -> None
    | obj -> Some obj

  let finish _handle = ()
end

module N = DC.Make (Native)

type linking_error = DT.linking_error =
  | Undefined_global of string
  | Unavailable_primitive of string
  | Uninitialized_global of string

type error = DT.error =
  | Not_a_bytecode_file of string
  | Inconsistent_import of string
  | Unavailable_unit of string
  | Unsafe_file
  | Linking_error of string * linking_error
  | Corrupted_interface of string
  | Cannot_open_dynamic_library of exn
  | Library's_module_initializers_failed of exn
  | Inconsistent_implementation of string
  | Module_already_loaded of string
  | Private_library_cannot_implement_interface of string

exception Error = DT.Error
let error_message = DT.error_message

let is_native =
  match Sys.backend_type with
  | Native -> true
  | Bytecode | Other _ -> false

let loadfile file =
  if is_native then N.loadfile file
  else B.loadfile file

let loadfile_private file =
  if is_native then N.loadfile_private file
  else B.loadfile_private file

let unsafe_get_global_value ~bytecode_or_asm_symbol =
  if is_native then N.unsafe_get_global_value ~bytecode_or_asm_symbol
  else B.unsafe_get_global_value ~bytecode_or_asm_symbol

let adapt_filename file =
  if is_native then N.adapt_filename file
  else B.adapt_filename file

let set_allowed_units units =
  if is_native then N.set_allowed_units units
  else B.set_allowed_units units

let allow_only units =
  if is_native then N.allow_only units
  else B.allow_only units

let prohibit units =
  if is_native then N.prohibit units
  else B.prohibit units

let main_program_units units =
  if is_native then N.main_program_units units
  else B.main_program_units units

let public_dynamically_loaded_units units =
  if is_native then N.public_dynamically_loaded_units units
  else B.public_dynamically_loaded_units units

let all_units () =
  if is_native then N.all_units ()
  else B.all_units ()

let allow_unsafe_modules allow =
  if is_native then N.allow_unsafe_modules allow
  else B.allow_unsafe_modules allow