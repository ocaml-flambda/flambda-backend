(**********************************************************************************
 *                             MIT License                                        *
 *                                                                                *
 *                                                                                *
 * Copyright (c) 2019-2024 Jane Street Group LLC                                  *
 *                                                                                *
 * Permission is hereby granted, free of charge, to any person obtaining a copy   *
 * of this software and associated documentation files (the "Software"), to deal  *
 * in the Software without restriction, including without limitation the rights   *
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell      *
 * copies of the Software, and to permit persons to whom the Software is          *
 * furnished to do so, subject to the following conditions:                       *
 *                                                                                *
 * The above copyright notice and this permission notice shall be included in all *
 * copies or substantial portions of the Software.                                *
 *                                                                                *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR     *
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,       *
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE    *
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER         *
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,  *
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE  *
 * SOFTWARE.                                                                      *
 *                                                                                *
 **********************************************************************************)

type error =
  | Not_an_object_file of Misc.filepath

exception Error of error

let read_cmo file =
  let open Cmo_format in
  (* FIXME This low-level binary I/O logic dearly needs to be refactored from
     the five or so places it's been replicated. *)
  let ic = open_in_bin file in
  try
    let buffer =
      really_input_string ic (String.length Config.cmo_magic_number)
    in
    if buffer <> Config.cmo_magic_number then
      raise(Error(Not_an_object_file file));
    let compunit_pos = input_binary_int ic in
    seek_in ic compunit_pos;
    let compunit = (input_value ic : compilation_unit_descr) in
    close_in ic;
    compunit
  with x ->
    close_in ic;
    raise x

let read_unit_info file : Instantiator.unit_info =
  let cmo = read_cmo file in
  { ui_unit = cmo.cu_name;
    ui_arg_descr = cmo.cu_arg_descr;
    ui_format = cmo.cu_format;
  }

let instantiate ~src ~args targetcmo =
  Instantiator.instantiate ~src ~args targetcmo
    ~expected_extension:".cmo"
    ~read_unit_info
    ~compile:(Compile.instance ~keep_symbol_tables:false)

(* Error report *)

open Format

let report_error ppf = function
  | Not_an_object_file file ->
    fprintf ppf "%a is not a bytecode object file"
      Location.print_filename file

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
