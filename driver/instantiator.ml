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

module CU = Compilation_unit

type error =
  | Not_compiled_as_argument of {
      compilation_unit : CU.t;
      filename : Misc.filepath;
      base_unit : CU.t;
    }
  | Incorrect_target_filename of {
      expected_basename : Misc.filepath;
      expected_extension : string;
      actual_basename : Misc.filepath;
      compilation_unit : CU.t;
    }
  | Not_parameterised of {
      compilation_unit : CU.t;
      filename : Misc.filepath;
    }
  | Missing_argument of { param : Global_module.Parameter_name.t }
  | No_such_parameter of {
      base_unit : CU.t;
      available_params : Global_module.Parameter_name.t list;
      param : Global_module.Parameter_name.t;
      arg : Global_module.Name.t;
    }
  | Repeated_parameter of {
      param : Global_module.Parameter_name.t;
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
  let base_unit_info = read_unit_info src in
  let base_compilation_unit = base_unit_info.ui_unit in
  let arg_info_of_cm_path cm_path =
    let unit_info = read_unit_info cm_path in
    match unit_info.ui_arg_descr with
    | None ->
      error (Not_compiled_as_argument
               { compilation_unit = unit_info.ui_unit;
                 filename = cm_path;
                 base_unit = base_unit_info.ui_unit; })
    | Some { arg_param; arg_block_idx } ->
      arg_param, (unit_info.ui_unit, arg_block_idx)
  in
  let arg_infos = List.map arg_info_of_cm_path args in
  let arg_pairs : CU.argument list =
    List.map
      (fun (param, (value, _)) : CU.argument ->
         { param = CU.Name.of_parameter_name param; value })
      arg_infos
  in
  let arg_map : (CU.t * int) Global_module.Parameter_name.Map.t =
    match Global_module.Parameter_name.Map.of_list_checked arg_infos with
    | Ok map -> map
    | Error (Duplicate { key; value1 = (arg1, _); value2 = (arg2, _) }) ->
      error (Repeated_parameter { param = key; arg1; arg2 })
  in
  let compilation_unit = CU.create_instance base_compilation_unit arg_pairs in
  let expected_output_basename_without_extension =
    CU.base_filename compilation_unit
  in
  let output_filename_without_extension = Filename.remove_extension targetcm in
  let output_basename_without_extension =
    Filename.basename output_filename_without_extension
  in
  if
    not (String.equal output_basename_without_extension
           expected_output_basename_without_extension)
  then begin
    (* The module will only work if given the correct filename *)
    error (Incorrect_target_filename
             { expected_basename = expected_output_basename_without_extension;
               expected_extension;
               actual_basename = output_basename_without_extension;
               compilation_unit })
  end;
  let global =
    (* This checks that we have all the arguments we need and that the CRCs all
       match up *)
    try
      Env.global_of_instance_compilation_unit compilation_unit
    with
    | Persistent_env.Error (Imported_module_has_unset_parameter e) ->
      error (Missing_argument { param = e.parameter })
    | Persistent_env.Error (Imported_module_has_no_such_parameter e) ->
      begin
        match e.valid_parameters with
        | [] ->
          let compilation_unit = base_compilation_unit in
          let filename = src in
          error (Not_parameterised { compilation_unit; filename })
        | available_params ->
          let base_unit = base_compilation_unit in
          let param = e.parameter in
          let arg = e.value in
          error (No_such_parameter { base_unit; available_params; param; arg })
      end
  in
  let arg_subst : Global_module.subst =
    global.visible_args
    |> List.map (fun ({ param; value } : Global_module.argument) -> param, value)
    |> Global_module.Parameter_name.Map.of_list
  in
  let runtime_params, main_module_block_size =
    match base_unit_info.ui_format with
    | Mb_struct _ ->
      (* Should have raised [Not_parameterised] above *)
      Misc.fatal_errorf "No runtime parameters for %a"
        CU.print base_compilation_unit
    | Mb_instantiating_functor { mb_runtime_params; mb_returned_size } ->
      mb_runtime_params, mb_returned_size
  in
  let runtime_args =
    runtime_params
    |> List.map (fun runtime_param : Translmod.runtime_arg ->
         match (runtime_param : Lambda.runtime_param) with
           | Rp_argument_block global ->
             begin
               match
                 Global_module.find_in_parameter_map global arg_map
               with
               | Some (ra_unit, ra_field_idx) ->
                 Argument_block { ra_unit; ra_field_idx }
               | None ->
                 (* This should have been caught by
                    [Env.global_of_instance_compilation_unit] earlier *)
                 Misc.fatal_errorf "Can't find value for %a"
                   Global_module.print global
             end
           | Rp_main_module_block global ->
             (* Substitute away any references to parameters in [global] *)
             let instance =
               Global_module.subst_inside global arg_subst
               |> Compilation_unit.of_complete_global_exn
             in
             Main_module_block instance
           | Rp_unit ->
             Unit)
  in
  let output_prefix = output_filename_without_extension in
  let arg_descr = base_unit_info.ui_arg_descr in
  compile
    ~source_file:src ~output_prefix ~compilation_unit ~runtime_args
    ~main_module_block_size ~arg_descr;
  ()

(* Error report *)

open Format
module Style = Misc.Style

let pp_parameters ppf params =
  fprintf ppf "@[<hov>%a@]"
    (pp_print_list ~pp_sep:pp_print_space
       (Style.as_inline_code Global_module.Parameter_name.print))
    params

let report_error ppf = function
  | Not_compiled_as_argument
      { base_unit; compilation_unit; filename } ->
    (* CR lmaurer: Would be nice to list out the parameters of the base unit
       here but that turns out to be very awkward (this gets raised before we've
       gotten [Persistent_env] involved and that's what knows the parameters).
       Worth revisiting if the situation changes or we really want a more
       convenient error message. *)
    fprintf ppf
      "@[<hov>Module %a@ cannot be used as an argument.@]@.\
       @[<hov>@{<hint>Hint@}: \
         @[<hov>Compile %a@ with @{<inline_code>-as-argument-for Foo@}@ where \
           @{<inline_code>Foo@} is a parameter of %a.@]@]"
      (Style.as_inline_code CU.print) compilation_unit
      (Style.as_inline_code Location.print_filename) filename
      (Style.as_inline_code CU.print) base_unit
  | Incorrect_target_filename
      { expected_basename; expected_extension; actual_basename;
        compilation_unit } ->
    let expected_filename = expected_basename ^ expected_extension in
    fprintf ppf
      "@[<hov>Incorrect output basename %a@ for instance %a.@]@.\
       @[<hov>@{<hint>Hint@}: @[<hov>Compile with %a@ or omit \
         @{<inline_code>-o@} entirely.@]@]"
      Style.inline_code actual_basename
      (Style.as_inline_code CU.print) compilation_unit
      (Style.as_clflag "-o" pp_print_string) expected_filename
  | Not_parameterised { compilation_unit; filename } ->
    fprintf ppf
      "@[<hov>Cannot instantiate %a@ because it has no parameters.@]@.\
       @[<hov>@{<hint>Hint@}: \
         @[<hov>Compile %a@ with @{<inline_code>-parameter@}.@]@]"
      (Style.as_inline_code CU.print) compilation_unit
      (Style.as_inline_code Location.print_filename) filename
  | Missing_argument { param } ->
    fprintf ppf "No argument given for parameter %a"
      (Style.as_inline_code Global_module.Parameter_name.print) param
  | No_such_parameter { base_unit; available_params; param; arg } ->
    fprintf ppf
      "@[<hov>Module %a@ is an argument for parameter %a,@ \
         which is not a parameter of %a.@]@.\
       @[<hov>@{<hint>Hint@}: @[<hov>%a@ was compiled with %a.@]@]@.\
       @[<hov>@{<hint>Hint@}: @[<hov>Parameters of %a:@ %a@]@]"
      (Style.as_inline_code Global_module.Name.print) arg
      (Style.as_inline_code Global_module.Parameter_name.print) param
      (Style.as_inline_code CU.print) base_unit
      (Style.as_inline_code Global_module.Name.print) arg
      (Style.as_clflag "-as-argument-for" Global_module.Parameter_name.print)
        param
      (Style.as_inline_code CU.print) base_unit
      pp_parameters available_params
  | Repeated_parameter { param; arg1; arg2 } ->
    fprintf ppf
      "@[<hov>Cannot use both %a@ and %a@ as arguments, since they are both \
         arguments for %a.@ Only one argument may be given for each \
         parameter.@]@.\
       @[<hov>@{<hint>Hint@}: @[<hov>Both %a@ and %a@ were compiled \
         with %a.@]@]"
      (Style.as_inline_code CU.print) arg1
      (Style.as_inline_code CU.print) arg2
      (Style.as_inline_code Global_module.Parameter_name.print) param
      (Style.as_inline_code CU.print) arg1
      (Style.as_inline_code CU.print) arg2
      (Style.as_clflag "-as-argument-for" Global_module.Parameter_name.print)
        param
let () =
  Location.register_error_of_exn
    (function
      | Error err -> Some (Location.error_of_printer_file report_error err)
      | _ -> None
    )
