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
