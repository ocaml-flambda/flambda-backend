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
  Compile_common.interface
    ~hook_parse_tree:(fun _ -> ())
    ~hook_typed_tree:(fun _ -> ())
    info

(** Native compilation backend for .ml files. *)

let compile i ~backend ~middle_end ~transl_style
      Typedtree.{structure; coercion; _} =
  (structure, coercion)
  |> Profile.(record transl)
    (Translmod.transl_implementation i.module_name ~style:transl_style)
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Profile.(record generate)
    (fun program ->
       Builtin_attributes.warn_unused ();
       let code = Simplif.simplify_lambda program.Lambda.code in
       { program with Lambda.code }
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
       |>(fun lambda ->
          if Clflags.(should_stop_after Compiler_pass.Lambda) then () else
          Asmgen.compile_implementation
            ~backend
            ~prefixname:(Unit_info.prefix i.target)
            ~middle_end
            ~ppf_dump:i.ppf_dump
            lambda;
           Compilenv.save_unit_info
             Unit_info.(Artifact.filename @@ cmx i.target)))

let flambda i backend typed =
  compile i typed ~backend ~transl_style:Plain_block
    ~middle_end:Flambda_middle_end.lambda_to_clambda

let clambda i backend typed =
  Clflags.set_oclassic ();
  compile i typed ~backend ~transl_style:Set_individual_fields
    ~middle_end:Closure_middle_end.lambda_to_clambda

(* Emit assembly directly from Linear IR *)
let emit i =
  Compilenv.reset i.module_name;
  Asmgen.compile_implementation_linear i.output_prefix ~progname:i.source_file

let implementation ~backend ~start_from ~source_file
    ~output_prefix ~keep_symbol_tables:_ =
  let backend info typed =
    Compilenv.reset info.module_name;
    if Config.flambda
    then flambda info backend typed
    else clambda info backend typed
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmx" @@ fun info ->
  match (start_from:Clflags.Compiler_pass.t) with
  | Parsing ->
    Compile_common.implementation
      ~hook_parse_tree:(fun _ -> ())
      ~hook_typed_tree:(fun _ -> ())
      info ~backend
  | Emit -> emit info
  | _ -> Misc.fatal_errorf "Cannot start from %s"
           (Clflags.Compiler_pass.to_string start_from)
