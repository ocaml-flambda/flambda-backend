val instantiate
  : (module Compiler_owee.Unix_intf.S)
  -> src:string
  -> args:string list
  -> string
  -> backend:(module Backend_intf.S)
  -> flambda2:(
      ppf_dump:Format.formatter ->
      prefixname:string ->
      filename:string ->
      keep_symbol_tables:bool ->
      Lambda.program ->
      Cmm.phrase list)
  -> unit

type error =
  | Not_compiled_as_parameter of { cmx_path : Misc.filepath }
  | Incorrect_target_filename of {
      expected_basename : Misc.filepath;
      actual_basename : Misc.filepath;
      compilation_unit : Compilation_unit.t;
    }
  | Not_parameterised of { cmx_path : Misc.filepath }
  | Missing_argument of { param : Global.Name.t }

exception Error of error

val report_error: Format.formatter -> error -> unit
