(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Fabrice Le Fessant, INRIA Saclay                     *)
(*                                                                        *)
(*   Copyright 2012 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc

type pers_flags =
  | Rectypes
  | Alerts of alerts
  | Opaque
  | Unsafe_string

type kind =
  | Normal of { cmi_impl : Compilation_unit.t }
  | Parameter

type error =
  | Not_an_interface of filepath
  | Wrong_version_interface of filepath * string
  | Corrupted_interface of filepath

exception Error of error

(* these type abbreviations are not exported;
   they are used to provide consistency across
   input_value and output_value usage. *)
type signature = Types.signature_item list

type crcs = Import_info.Intf.t array  (* smaller on disk than using a list *)
type flags = pers_flags list
type header = {
    header_name : Compilation_unit.Name.t;
    header_kind : kind;
    header_globals : (Global.Name.t * Global.t) array;
    header_sign : signature;
    header_secondary_sign : signature option;
    header_params : Global.Name.t list;
    header_arg_for : Global.Name.t option;
}

type cmi_infos = {
    cmi_name : Compilation_unit.Name.t;
    cmi_kind : kind;
    cmi_globals : (Global.Name.t * Global.t) array;
    cmi_sign : signature;
    cmi_secondary_sign : signature option;
    cmi_params : Global.Name.t list;
    cmi_arg_for : Global.Name.t option;
    cmi_crcs : crcs;
    cmi_flags : flags;
}

let input_cmi ic =
  let {
      header_name = name;
      header_kind = kind;
      header_globals = globals;
      header_sign = sign;
      header_secondary_sign = secondary_sign;
      header_params = params;
      header_arg_for = arg_for;
    } = (input_value ic : header) in
  let crcs = (input_value ic : crcs) in
  let flags = (input_value ic : flags) in
  {
      cmi_name = name;
      cmi_kind = kind;
      cmi_globals = globals;
      cmi_sign = sign;
      cmi_secondary_sign = secondary_sign;
      cmi_params = params;
      cmi_arg_for = arg_for;
      cmi_crcs = crcs;
      cmi_flags = flags;
    }

let read_cmi filename =
  let ic = open_in_bin filename in
  try
    let buffer =
      really_input_string ic (String.length Config.cmi_magic_number)
    in
    if buffer <> Config.cmi_magic_number then begin
      close_in ic;
      let pre_len = String.length Config.cmi_magic_number - 3 in
      if String.sub buffer 0 pre_len
          = String.sub Config.cmi_magic_number 0 pre_len then
      begin
        let msg =
          if buffer < Config.cmi_magic_number then "an older" else "a newer" in
        raise (Error (Wrong_version_interface (filename, msg)))
      end else begin
        raise(Error(Not_an_interface filename))
      end
    end;
    let cmi = input_cmi ic in
    close_in ic;
    cmi
  with End_of_file | Failure _ ->
      close_in ic;
      raise(Error(Corrupted_interface(filename)))
    | Error e ->
      close_in ic;
      raise (Error e)

let output_cmi filename oc cmi =
(* beware: the provided signature must have been substituted for saving *)
  output_string oc Config.cmi_magic_number;
  output_value oc
    {
      header_name = cmi.cmi_name;
      header_kind = cmi.cmi_kind;
      header_globals = cmi.cmi_globals;
      header_sign = cmi.cmi_sign;
      header_secondary_sign = cmi.cmi_secondary_sign;
      header_params = cmi.cmi_params;
      header_arg_for = cmi.cmi_arg_for;
    };
  flush oc;
  let crc = Digest.file filename in
  let unit =
    match cmi.cmi_kind with
    | Normal { cmi_impl } -> Some cmi_impl
    | Parameter -> None
  in
  let my_info =
    Import_info.Intf.create cmi.cmi_name unit ~crc:(Some crc)
  in
  let crcs = Array.append [| my_info |] cmi.cmi_crcs in
  output_value oc (crcs : crcs);
  output_value oc (cmi.cmi_flags : flags);
  crc

(* Error report *)

open Format

let report_error ppf = function
  | Not_an_interface filename ->
      fprintf ppf "%a@ is not a compiled interface"
        Location.print_filename filename
  | Wrong_version_interface (filename, older_newer) ->
      fprintf ppf
        "%a@ is not a compiled interface for this version of OCaml.@.\
         It seems to be for %s version of OCaml."
        Location.print_filename filename older_newer
  | Corrupted_interface filename ->
      fprintf ppf "Corrupted compiled interface@ %a"
        Location.print_filename filename

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
