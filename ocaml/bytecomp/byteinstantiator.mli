module CU := Compilation_unit

val instantiate : src:string -> args:string list -> string -> unit

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

val report_error : Format.formatter -> error -> unit
