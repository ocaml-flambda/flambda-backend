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

module CU := Compilation_unit

type unit_info = {
  ui_unit : CU.t;
  ui_arg_descr : Lambda.arg_descr option;
  ui_format : Lambda.main_module_block_format;
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
      arg : Global_module.Name.t
    }
  | Repeated_parameter of {
      param : Global_module.Parameter_name.t;
      arg1 : CU.t;
      arg2 : CU.t;
    }


exception Error of error

val report_error: Format.formatter -> error -> unit
