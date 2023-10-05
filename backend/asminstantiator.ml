type error =
  | Not_compiled_as_parameter of { cmx_path : Misc.filepath }
  | Incorrect_target_filename of {
      expected_basename : Misc.filepath;
      actual_basename : Misc.filepath;
      compilation_unit : Compilation_unit.t;
    }


exception Error of error

let error e = raise (Error e)

let instantiate unix ~src ~args targetcmx ~backend ~flambda2 =
  let unit_infos, _ = Compilenv.read_unit_info src in
  let base_compilation_unit = unit_infos.ui_unit in
  let param_and_comp_unit_of_cmx_path cmx_path =
    let unit_infos, _ = Compilenv.read_unit_info cmx_path in
    match unit_infos.ui_implements_param with
    | None -> error (Not_compiled_as_parameter { cmx_path })
    | Some param ->
      Compilation_unit.of_global_name param, unit_infos.ui_unit
  in
  let compilation_unit =
    Compilation_unit.create_instance
      base_compilation_unit
      (args |> List.map param_and_comp_unit_of_cmx_path)
  in
  let expected_output_prefix =
    Compilation_unit.base_filename compilation_unit
  in
  let output_prefix = Filename.remove_extension targetcmx in
  let output_prefix_basename = Filename.basename output_prefix in
  if not (String.equal output_prefix_basename expected_output_prefix)
  then begin
    (* The module will only work if given the correct filename *)
    error (Incorrect_target_filename
             { expected_basename = expected_output_prefix;
               actual_basename = output_prefix_basename;
               compilation_unit })
  end;
  let runtime_params = unit_infos.ui_runtime_params in
  Optcompile.instance unix ~backend ~flambda2
    ~source_file:src ~output_prefix ~compilation_unit ~runtime_params
    ~keep_symbol_tables:false;
  ()

(* Error report *)

open Format

let report_error ppf = function
  | Not_compiled_as_parameter { cmx_path } ->
    Format.fprintf ppf
      "Argument .cmx must be compiled with -as-parameter-for: %s"
      cmx_path
  | Incorrect_target_filename
      { expected_basename; actual_basename = _; compilation_unit } ->
    Format.fprintf ppf
      "Filename given by -o must have basename %s.cmx@
       to produce the desired instance %a"
      expected_basename
      Compilation_unit.print compilation_unit

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
