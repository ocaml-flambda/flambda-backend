module CU = Compilation_unit

type error =
  | Not_compiled_as_argument of CU.t
  | Incorrect_target_filename of {
      expected_basename : Misc.filepath;
      expected_extension : string;
      actual_basename : Misc.filepath;
      compilation_unit : CU.t;
    }
  | Not_parameterised of CU.t
  | Missing_argument of { param : Global_module.Name.t }
  | No_such_parameter of {
      base_unit : CU.t;
      param : Global_module.Name.t;
      arg : Global_module.Name.t
    }
  | Repeated_parameter of {
      param : Global_module.Name.t;
      arg1 : CU.t;
      arg2 : CU.t;
    }


exception Error of error

let error e = raise (Error e)

type unit_info = {
  ui_unit : CU.t;
  ui_arg_descr : Lambda.arg_descr option;
  ui_format : Lambda.main_module_block_format;
}

let instantiate
      ~src ~args targetcm ~expected_extension ~read_unit_info ~compile =
  let unit_infos = read_unit_info src in
  let base_compilation_unit = unit_infos.ui_unit in
  let arg_info_of_cm_path cm_path =
    let unit_infos = read_unit_info cm_path in
    match unit_infos.ui_arg_descr with
    | None -> error (Not_compiled_as_argument unit_infos.ui_unit)
    | Some { arg_param; arg_block_field } ->
      arg_param, (unit_infos.ui_unit, arg_block_field)
  in
  let arg_info = List.map arg_info_of_cm_path args in
  let arg_pairs : CU.argument list =
    List.map
      (fun (param, (value, _)) : CU.argument ->
         { param = CU.of_global_name param; value })
      arg_info
  in
  let arg_map : (CU.t * int) Global_module.Name.Map.t =
    match Global_module.Name.Map.of_list_checked arg_info with
    | Ok map -> map
    | Error (Duplicate { key; value1 = (arg1, _); value2 = (arg2, _) }) ->
      error (Repeated_parameter { param = key; arg1; arg2 })
  in
  let compilation_unit = CU.create_instance base_compilation_unit arg_pairs in
  let expected_output_prefix = CU.base_filename compilation_unit in
  let output_prefix = Filename.remove_extension targetcm in
  let output_prefix_basename = Filename.basename output_prefix in
  if not (String.equal output_prefix_basename expected_output_prefix)
  then begin
    (* The module will only work if given the correct filename *)
    error (Incorrect_target_filename
             { expected_basename = expected_output_prefix;
               expected_extension;
               actual_basename = output_prefix_basename;
               compilation_unit })
  end;
  let global =
    (* This checks that we have all the arguments we need and that the CRCs all
       match up *)
    try
      Env.global_of_instance_compilation_unit compilation_unit
    with
    | Persistent_env.Error (Imported_module_has_unset_parameter e) ->
      raise (Error (Missing_argument { param = e.parameter }))
    | Persistent_env.Error (Imported_module_has_no_such_parameter e) ->
      begin
        let base_unit = unit_infos.ui_unit in
        match e.valid_parameters with
        | [] -> raise (Error (Not_parameterised base_unit))
        | _ ->
          let param = e.parameter in
          let arg = e.value in
          raise (Error (No_such_parameter { base_unit; param; arg }))
      end
  in
  let arg_subst : Global_module.subst =
    global.visible_args
    |> List.map (fun ({ param; value } : Global_module.argument) -> param, value)
    |> Global_module.Name.Map.of_list
  in
  let runtime_params, main_module_block_size =
    match unit_infos.ui_format with
    | Mb_record _ ->
      (* Should have raised [Not_parameterised] above *)
      Misc.fatal_errorf "No runtime parameters for %a" CU.print unit_infos.ui_unit
    | Mb_wrapped_function { mb_runtime_params; mb_returned_size } ->
      mb_runtime_params, mb_returned_size
  in
  let runtime_args =
    runtime_params
    |> List.map (fun runtime_param : Translmod.runtime_arg ->
         match (runtime_param : Lambda.runtime_param) with
           | Rp_argument_block global ->
             let global_name = Global_module.to_name global in
             begin
               match
                 Global_module.Name.Map.find_opt global_name arg_map
               with
               | Some (ra_unit, ra_field) ->
                 Argument_block { ra_unit; ra_field }
               | None ->
                 (* This should have been caught by
                    [Env.global_of_instance_compilation_unit] earlier *)
                 Misc.fatal_errorf "Can't find value for %a"
                   Global_module.Name.print global_name
             end
           | Rp_dependency global ->
             (* This is a dependency that will be passed in. We need to
                substitute the arguments into the name of the dependency to find
                the particular instance to pass. *)
             let instance =
               Global_module.subst_inside global arg_subst
               |> Compilation_unit.of_complete_global_exn
             in
             Dependency instance
           | Rp_unit ->
             Unit)
  in
  let arg_descr = unit_infos.ui_arg_descr in
  compile
    ~source_file:src ~output_prefix ~compilation_unit ~runtime_args
    ~main_module_block_size ~arg_descr;
  ()

(* Error report *)

open Format

let report_error ppf = function
  | Not_compiled_as_argument compilation_unit ->
    fprintf ppf
      "Argument must be compiled with -as-argument-for: %a"
      CU.print compilation_unit
  | Incorrect_target_filename
      { expected_basename; expected_extension = _; actual_basename = _;
        compilation_unit } ->
    fprintf ppf
      "Filename given by -o must have basename %s@ \
       to produce the desired instance %a"
      expected_basename
      CU.print compilation_unit
  | Not_parameterised compilation_unit ->
    fprintf ppf "%a must be compiled with at least one -parameter"
      CU.print compilation_unit
  | Missing_argument { param } ->
    fprintf ppf "No argument given for parameter %a"
      Global_module.Name.print param
  | No_such_parameter { base_unit; param; arg } ->
    fprintf ppf
      "Mismatched argument: %a@ was compiled with -as-argument-for %a@ \
       but %a@ was not compiled with -parameter %a"
      Global_module.Name.print arg
      Global_module.Name.print param
      CU.print base_unit
      Global_module.Name.print param
  | Repeated_parameter { param; arg1; arg2 } ->
    fprintf ppf
      "%a@ and %a@ were both compiled with -as-argument-for %a.@ \
       Only one argument may be given for each parameter."
      CU.print arg1
      CU.print arg2
      Global_module.Name.print param

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
