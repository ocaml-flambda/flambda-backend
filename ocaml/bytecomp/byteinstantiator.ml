module CU = Compilation_unit

type error =
  | Not_compiled_as_parameter of { cmo_path : Misc.filepath }
  | Incorrect_target_filename of {
      expected_basename : Misc.filepath;
      actual_basename : Misc.filepath;
      compilation_unit : CU.t;
    }
  | Not_an_object_file of Misc.filepath
  | Not_parameterised of { cmo_path : Misc.filepath }
  | Missing_argument of { param : Global.Name.t }

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
  let arg_info_of_cmo_path cmo_path =
    let unit_infos = read_unit_info cmo_path in
    match unit_infos.cu_arg_descr with
    | None -> error (Not_compiled_as_parameter { cmo_path })
    | Some { arg_param; arg_block_field } ->
      arg_param, (unit_infos.cu_name, arg_block_field)
  in
  let arg_info = List.map arg_info_of_cmo_path args in
  let arg_pairs : (CU.t * CU.t) list =
    List.map (fun (param, (value, _)) -> CU.of_global_name param, value)
      arg_info
  in
  let arg_map : (CU.t * int) Global.Name.Map.t =
    Global.Name.Map.of_list arg_info (* FIXME: check for dupes *)
  in
  let compilation_unit = CU.create_instance base_compilation_unit arg_pairs in
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
  let global =
    (* This checks that we have all the arguments we need and that the CRCs all
       match up *)
    Env.global_of_instance_compilation_unit compilation_unit
  in
  let arg_subst : Global.subst = Global.Name.Map.of_list global.visible_args in
  let runtime_params, main_module_block_size =
    match unit_infos.cu_format with
    | Mb_record _ ->
      error (Not_parameterised { cmo_path = targetcmo })
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
  let arg_descr = unit_infos.cu_arg_descr in
  Compile.instance
    ~source_file:src ~output_prefix ~compilation_unit ~runtime_args
    ~main_module_block_size ~arg_descr ~keep_symbol_tables:false;
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
  | Not_parameterised { cmo_path } ->
    fprintf ppf "%a must be compiled with at least one -parameter"
      Location.print_filename cmo_path
  | Missing_argument { param } ->
    fprintf ppf "No argument given for parameter %a"
      Global.Name.print param

let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
