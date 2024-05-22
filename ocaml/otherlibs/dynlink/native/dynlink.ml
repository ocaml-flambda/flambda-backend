#2 "otherlibs/dynlink/dynlink.ml"
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

(* Dynamic loading of .cmx files *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Dynlink_compilerlibs

module DC = Dynlink_common
module DT = Dynlink_types

let convert_cmi_import import =
  let name = Import_info.name import |> Compilation_unit.Name.to_string in
  let crc = Import_info.crc import in
  name, crc

type global_map = {
  name : Compilation_unit.t;
  crc_intf : Digest.t option;
  crc_impl : Digest.t option;
  syms : Symbol.t list;
}

module Native = struct
  type handle

  (* mshinwell: We need something better than caml_sys_exit *)
  external ndl_open : string -> bool -> handle * Cmxs_format.dynheader
    = "caml_sys_exit" "caml_natdynlink_open"
  external ndl_register : handle -> string array -> unit
    = "caml_sys_exit" "caml_natdynlink_register"
  external ndl_run : handle -> string -> unit
    = "caml_sys_exit" "caml_natdynlink_run"
  external ndl_getmap : unit -> global_map list
    = "caml_sys_exit" "caml_natdynlink_getmap"
  external ndl_globals_inited : unit -> int
    = "caml_sys_exit" "caml_natdynlink_globals_inited"
  external ndl_loadsym : string -> Obj.t
    = "caml_sys_exit" "caml_natdynlink_loadsym"
  external ndl_existssym : string -> bool
    = "caml_sys_exit" "caml_natdynlink_existssym"
    [@@noalloc]

  module Unit_header = struct
    type t = Cmxs_format.dynunit

    let name (t : t) = t.dynu_name |> Compilation_unit.name_as_string
    let crc (t : t) = Some t.dynu_crc

    let convert_cmx_import import =
      let cu = Import_info.cu import |> Compilation_unit.name_as_string in
      let crc = Import_info.crc import in
      cu, crc

    let interface_imports (t : t) =
      List.map convert_cmi_import (Array.to_list t.dynu_imports_cmi)
    let implementation_imports (t : t) =
      List.map convert_cmx_import (Array.to_list t.dynu_imports_cmx)

    let defined_symbols (t : t) =
      List.map (fun comp_unit ->
          Symbol.for_compilation_unit comp_unit
          |> Symbol.linkage_name
          |> Linkage_name.to_string)
        t.dynu_defines

    let unsafe_module _t = false
  end

  let init () = ()

  let is_native = true
  let adapt_filename f = Filename.chop_extension f ^ ".cmxs"

  let num_globals_inited () = ndl_globals_inited ()

  let fold_initial_units ~init ~f =
    let rank = ref 0 in
    List.fold_left (fun acc { name; crc_intf; crc_impl; syms; } ->
        let name = Compilation_unit.full_path_as_string name in
        let syms =
          List.map
            (fun sym -> Symbol.linkage_name sym |> Linkage_name.to_string)
            syms
        in
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
    ndl_run handle "caml_shared_startup"

  let run _lock handle ~unit_header ~priv:_ =
    List.iter (fun cu ->
        try ndl_run handle cu
        with exn ->
          Printexc.raise_with_backtrace
            (DT.Error (Library's_module_initializers_failed exn))
            (Printexc.get_raw_backtrace ()))
      (Unit_header.defined_symbols unit_header)

  exception Register_dyn_global_duplicate
  let () =
    Callback.register "Register_dyn_global_duplicate"
      Register_dyn_global_duplicate

  let load ~filename ~priv =
    let handle, header =
      try ndl_open filename (not priv)
      with exn -> raise (DT.Error (Cannot_open_dynamic_library exn))
    in
    if header.dynu_magic <> Config.cmxs_magic_number then begin
      raise (DT.Error (Not_a_bytecode_file filename))
    end;
    handle, header.dynu_units

  let register handle dynu_units ~priv ~filename =
    let syms =
      "caml_shared_startup" ::
      List.concat_map Unit_header.defined_symbols dynu_units
    in
    try
      ndl_register handle (Array.of_list syms)
    with
    | Register_dyn_global_duplicate ->
      if not priv then
        failwith (Printf.sprintf "Attempt to register duplicate dynamic \
          GC roots for non-privately-loaded library `%s'; this is a bug in \
          [Dynlink]" filename)
      else
        Printexc.raise_with_backtrace
          (DT.Error (Library_file_already_loaded_privately { filename }))
          (Printexc.get_raw_backtrace ())
    | exn ->
      Printexc.raise_with_backtrace
        (DT.Error (Library's_module_initializers_failed exn))
        (Printexc.get_raw_backtrace ())

  let unsafe_get_global_value ~bytecode_or_asm_symbol =
    match ndl_loadsym bytecode_or_asm_symbol with
    | exception _ -> None
    | obj -> Some obj

  let does_symbol_exist ~bytecode_or_asm_symbol =
    ndl_existssym bytecode_or_asm_symbol

  let finish _handle = ()
end

include DC.Make (Native)

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
  | Library_file_already_loaded_privately of { filename : string; }

exception Error = DT.Error
let error_message = DT.error_message
