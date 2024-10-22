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

let with_info = Compile_common.with_info ~native:true ~tool_name

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface
  ~hook_parse_tree:(Compiler_hooks.execute Compiler_hooks.Parse_tree_intf)
  ~hook_typed_tree:(Compiler_hooks.execute Compiler_hooks.Typed_tree_intf)
    info

(** Native compilation backend for .ml files. *)

let compile i typed ~transl_style ~unix ~pipeline =
  typed
  |> Profile.(record transl)
    (Translmod.transl_implementation i.module_name ~style:transl_style)
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Compiler_hooks.execute_and_pipe Compiler_hooks.Raw_lambda
  |> Profile.(record generate)
   (fun program ->
      Builtin_attributes.warn_unused ();
      let code = Simplif.simplify_lambda program.Lambda.code in
      { program with Lambda.code }
      |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
      |> Compiler_hooks.execute_and_pipe Compiler_hooks.Lambda
      |> (fun program ->
           if Clflags.(should_stop_after Compiler_pass.Lambda) then () else
           Asmgen.compile_implementation
             unix
             ~pipeline
             ~sourcefile:(Some (Unit_info.source_file i.target))
             ~prefixname:(Unit_info.prefix i.target)
             ~ppf_dump:i.ppf_dump
             program);
           Compilenv.save_unit_info
             (Unit_info.Artifact.filename (Unit_info.cmx i.target)))

type flambda2 =
  ppf_dump:Format.formatter ->
  prefixname:string ->
  keep_symbol_tables:bool ->
  Lambda.program ->
  Cmm.phrase list

(* Emit assembly directly from Linear IR *)
let emit unix i =
  Compilenv.reset i.module_name;
  Asmgen.compile_implementation_linear unix
    (Unit_info.prefix i.target)
    ~progname:(Unit_info.source_file i.target)

let implementation unix ~(flambda2 : flambda2) ~start_from ~source_file
    ~output_prefix ~keep_symbol_tables =
  let backend info ({ structure; coercion; _ } : Typedtree.implementation) =
    Compilenv.reset info.module_name;
    let typed = structure, coercion in
    let transl_style : Translmod.compilation_unit_style =
      if Config.flambda || Config.flambda2 then Plain_block
      else Set_individual_fields
    in
    let pipeline : Asmgen.pipeline =
      Direct_to_cmm (flambda2 ~keep_symbol_tables)
    in
    if not (Config.flambda || Config.flambda2) then Clflags.set_oclassic ();
    compile info typed ~unix ~transl_style ~pipeline
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmx" @@ fun info ->
  if !Flambda_backend_flags.internal_assembler then
      Emitaux.binary_backend_available := true;
  match (start_from:Clflags.Compiler_pass.t) with
  | Parsing ->
    Compile_common.implementation
      ~hook_parse_tree:(Compiler_hooks.execute Compiler_hooks.Parse_tree_impl)
      ~hook_typed_tree:(fun (impl : Typedtree.implementation) ->
        Compiler_hooks.execute Compiler_hooks.Typed_tree_impl impl)
      info ~backend
  | Emit -> emit unix info ~ppf_dump:info.ppf_dump
  | _ -> Misc.fatal_errorf "Cannot start from %s"
           (Clflags.Compiler_pass.to_string start_from)
