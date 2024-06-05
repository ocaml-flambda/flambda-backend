module CU := Compilation_unit

type unit_info = {
  ui_unit : CU.t;
  ui_arg_descr : Lambda.arg_descr option;
  ui_format : Lambda.module_block_format;
}

val instantiate
   : src:Misc.filepath
  -> args:Misc.filepath list
  -> Misc.filepath
  -> expected_extension:string
  -> read_unit_info:(Misc.filepath -> unit_info)
  -> compile:(
      source_file:Misc.filepath ->
      output_prefix:string ->
      compilation_unit:CU.t ->
      runtime_args:Translmod.runtime_arg list ->
      main_module_block_size:int ->
      arg_descr:Lambda.arg_descr option -> unit)
  -> unit

type error =
  | Not_compiled_as_parameter of { cm_path : Misc.filepath }
  | Incorrect_target_filename of {
      expected_basename : Misc.filepath;
      expected_extension : string;
      actual_basename : Misc.filepath;
      compilation_unit : CU.t;
    }
  | Not_parameterised of { cm_path : Misc.filepath }
  | Missing_argument of { param : Global_module.Name.t }

exception Error of error

val report_error: Format.formatter -> error -> unit
