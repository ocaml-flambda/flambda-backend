(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** The batch compiler *)

open Misc
open Compile_common

let tool_name = "ocamlopt"

let with_info =
  Compile_common.with_info ~native:true ~tool_name

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface info

let (|>>) (x, y) f = (x, f y)

(** Native compilation backend for .ml files. *)

let flambda_and_flambda2 i typed ~compile_implementation =
  typed
  |> Profile.(record transl)
      (Translmod.transl_implementation_flambda i.module_name)
  |> Profile.(record generate)
    (fun {Lambda.module_ident; main_module_block_size;
          required_globals; code } ->
    ((module_ident, main_module_block_size), code)
    |>> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
    |>> Simplif.simplify_lambda
    |>> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
    |> (fun ((module_ident, main_module_block_size), code) ->
      compile_implementation ~module_ident ~main_module_block_size ~code
        ~required_globals;
      Compilenv.save_unit_info (cmx i)))

let flambda2_ i ~flambda2 typed =
  flambda_and_flambda2 i typed
    ~compile_implementation:(fun ~module_ident ~main_module_block_size ~code
          ~required_globals ->
      Asmgen.compile_implementation_flambda2
        ~filename:i.source_file
        ~prefixname:i.output_prefix
        ~size:main_module_block_size
        ~module_ident
        ~module_initializer:code
        ~flambda2
        ~ppf_dump:i.ppf_dump
        ~required_globals
        ())

let flambda i backend typed =
  flambda_and_flambda2 i typed
    ~compile_implementation:(fun ~module_ident ~main_module_block_size ~code
          ~required_globals ->
      let program : Lambda.program =
        { Lambda.
          module_ident;
          main_module_block_size;
          required_globals;
          code;
        }
      in
      Asmgen.compile_implementation
        ~backend
        ~filename:i.source_file
        ~prefixname:i.output_prefix
        ~middle_end:Flambda_middle_end.lambda_to_clambda
        ~ppf_dump:i.ppf_dump
        program)

let clambda i backend typed =
  Clflags.set_oclassic ();
  typed
  |> Profile.(record transl)
    (Translmod.transl_store_implementation i.module_name)
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Profile.(record generate)
    (fun program ->
       let code = Simplif.simplify_lambda program.Lambda.code in
       { program with Lambda.code }
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
       |> Asmgen.compile_implementation
            ~backend
            ~filename:i.source_file
            ~prefixname:i.output_prefix
            ~middle_end:Closure_middle_end.lambda_to_clambda
            ~ppf_dump:i.ppf_dump;
       Compilenv.save_unit_info (cmx i))

(* Emit assembly directly from Linear IR *)
let emit i =
  Compilenv.reset ?packname:!Clflags.for_package i.module_name;
  Asmgen.compile_implementation_linear i.output_prefix ~progname:i.source_file

let implementation ~backend ~flambda2 ~start_from ~source_file ~output_prefix =
  let backend info typed =
    Compilenv.reset ?packname:!Clflags.for_package info.module_name;
    if Config.flambda
    then flambda info backend typed
    else if Config.flambda2
    then flambda2_ info ~flambda2 typed
    else clambda info backend typed
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmx" @@ fun info ->
  match (start_from:Clflags.Compiler_pass.t) with
  | Parsing -> Compile_common.implementation info ~backend
  | Emit -> emit info
  | _ -> Misc.fatal_errorf "Cannot start from %s"
           (Clflags.Compiler_pass.to_string start_from)
