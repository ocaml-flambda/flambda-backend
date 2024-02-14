module CU = Compilation_unit

type error =
  | Not_compiled_as_parameter of { cmx_path : Misc.filepath }
  | Incorrect_target_filename of {
      expected_basename : Misc.filepath;
      actual_basename : Misc.filepath;
      compilation_unit : CU.t;
    }
  | Not_parameterised of { cmx_path : Misc.filepath }
  | Missing_argument of { param : Global.Name.t }


exception Error of error

let error e = raise (Error e)

let instantiate unix ~src ~args targetcmx ~backend ~flambda2 =
  let unit_infos, _ = Compilenv.read_unit_info src in
  let base_compilation_unit = unit_infos.ui_unit in
  let arg_info_of_cmx_path cmx_path =
    let unit_infos, _ = Compilenv.read_unit_info cmx_path in
    match unit_infos.ui_arg_descr with
    | None -> error (Not_compiled_as_parameter { cmx_path })
    | Some { arg_param; arg_block_field } ->
      arg_param, (unit_infos.ui_unit, arg_block_field)
  in
  let arg_info = List.map arg_info_of_cmx_path args in
  let arg_pairs : (CU.t * CU.t) list =
    List.map (fun (param, (value, _)) -> CU.of_global_name param, value)
      arg_info
  in
  let arg_map : (CU.t * int) Global.Name.Map.t =
    Global.Name.Map.of_list arg_info (* FIXME: check for dupes *)
  in
  let compilation_unit = CU.create_instance base_compilation_unit arg_pairs in
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
  let global =
    (* This checks that we have all the arguments we need and that the CRCs all
       match up *)
    Env.global_of_instance_compilation_unit compilation_unit
  in
  let arg_subst : Global.subst = Global.Name.Map.of_list global.visible_args in
  let runtime_params, main_module_block_size =
    match unit_infos.ui_format with
    | Mb_record _ ->
      error (Not_parameterised { cmx_path = targetcmx })
    | Mb_wrapped_function { mb_runtime_params; mb_returned_size } ->
      mb_runtime_params, mb_returned_size
  in
  let runtime_args =
    runtime_params
    |> List.map (fun runtime_param : Translmod.runtime_arg ->
         match (runtime_param : Lambda.runtime_param_descr) with
           | Rp_argument_block global ->
             let global_name = Global.to_name global in
             begin
               match
                 Global.Name.Map.find_opt global_name arg_map
               with
               | Some (ra_unit, ra_field) ->
                 Argument_block { ra_unit; ra_field }
               | None ->
                 error (Missing_argument { param = global_name })
             end
           | Rp_dependency global ->
             (* This is a dependency that will be passed in. We need to
                substitute the arguments into the name of the dependency to find
                the particular instance to pass. *)
             let instance =
               Global.subst_inside global arg_subst
               |> Compilation_unit.of_complete_global_exn
             in
             Dependency instance
           | Rp_unit ->
             Unit)
  in
  let main_module_block_size = main_module_block_size in
  let arg_descr = unit_infos.ui_arg_descr in
  Optcompile.instance unix ~backend ~flambda2
    ~source_file:src ~output_prefix ~compilation_unit ~runtime_args
    ~main_module_block_size ~arg_descr
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
  | Not_parameterised { cmx_path } ->
    Format.fprintf ppf "%a must be compiled with at least one -parameter"
      Location.print_filename cmx_path
  | Missing_argument { param } ->
    Format.fprintf ppf "No argument given for parameter %a"
      Global.Name.print param

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
