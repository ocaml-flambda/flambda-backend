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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module DT = Dynlink_types

module B = Dynlink_internal_byte
module N = Dynlink_internal_native

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

let does_symbol_exist ~bytecode_or_asm_symbol =
  if is_native then N.does_symbol_exist ~bytecode_or_asm_symbol
  else B.does_symbol_exist ~bytecode_or_asm_symbol

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
