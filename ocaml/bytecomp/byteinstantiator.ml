module CU = Compilation_unit

type error =
  | Not_compiled_as_parameter of { cmo_path : Misc.filepath }
  | Incorrect_target_filename of {
      expected_basename : Misc.filepath;
      actual_basename : Misc.filepath;
      compilation_unit : CU.t;
    }
  | Not_an_object_file of Misc.filepath


exception Error of error

let error e = raise (Error e)

let read_unit_info file =
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


let instantiate ~src ~args targetcmo =
  let unit_infos = read_unit_info src in
  let base_compilation_unit = unit_infos.cu_name in
  let param_and_comp_unit_of_cmo_path cmo_path =
    let unit_infos = read_unit_info cmo_path in
    match unit_infos.cu_implements_param with
    | None -> error (Not_compiled_as_parameter { cmo_path })
    | Some param -> CU.of_global_name param, unit_infos.cu_name
  in
  let compilation_unit =
    CU.create_instance base_compilation_unit
      (args |> List.map param_and_comp_unit_of_cmo_path)
  in
  let expected_output_prefix = CU.base_filename compilation_unit in
  let output_prefix = Filename.remove_extension targetcmo in
  let output_prefix_basename = Filename.basename output_prefix in
  if not (String.equal output_prefix_basename expected_output_prefix)
  then begin
    (* The module will only work if given the correct filename *)
    error (Incorrect_target_filename
             { expected_basename = expected_output_prefix;
               actual_basename = output_prefix_basename;
               compilation_unit })
  end;
  let runtime_params = unit_infos.cu_runtime_params |> Array.to_list in
  Compile.instance
    ~source_file:src ~output_prefix ~compilation_unit ~runtime_params
    ~keep_symbol_tables:false;
  ()

(* Error report *)

open Format

let report_error ppf = function
  | Not_compiled_as_parameter { cmo_path } ->
    fprintf ppf
      "Argument .cmo must be compiled with -as-parameter-for: %a"
      Location.print_filename cmo_path
  | Incorrect_target_filename
      { expected_basename; actual_basename = _; compilation_unit } ->
    fprintf ppf
      "Filename given by -o must have basename %s.cmo@ \
       to produce the desired instance %a"
      expected_basename
      CU.print compilation_unit
  | Not_an_object_file file ->
    fprintf ppf "%a is not a bytecode object file"
      Location.print_filename file

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
