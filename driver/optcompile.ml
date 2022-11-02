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

let (|>>) (x, y) f = (x, f y)

(** Native compilation backend for .ml files. *)

let flambda_and_flambda2 i typed ~compile_implementation =
  typed
  |> Profile.(record transl)
    (Translmod.transl_implementation_flambda i.module_name)
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Compiler_hooks.execute_and_pipe Compiler_hooks.Raw_lambda
  |> Profile.(record generate)
   (fun program ->
      let code = Simplif.simplify_lambda program.Lambda.code in
      { program with Lambda.code }
      |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
      |> Compiler_hooks.execute_and_pipe Compiler_hooks.Lambda
      |> (fun ({ Lambda.module_ident; main_module_block_size;
                 required_globals; code }) ->
           compile_implementation ~module_ident ~main_module_block_size ~code
             ~required_globals;
           Compilenv.save_unit_info (cmx i)))

let flambda2_ unix i ~flambda2 ~keep_symbol_tables typed =
  flambda_and_flambda2 i typed
    ~compile_implementation:(fun ~module_ident ~main_module_block_size ~code
          ~required_globals ->
      Asmgen.compile_implementation_flambda2
        unix
        ~filename:i.source_file
        ~prefixname:i.output_prefix
        ~size:main_module_block_size
        ~module_ident
        ~module_initializer:code
        ~flambda2
        ~ppf_dump:i.ppf_dump
        ~required_globals
        ~keep_symbol_tables
        ())

let flambda unix i backend typed =
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
        unix
        ~backend
        ~filename:i.source_file
        ~prefixname:i.output_prefix
        ~middle_end:Flambda_middle_end.lambda_to_clambda
        ~ppf_dump:i.ppf_dump
        program)

let clambda unix i backend typed =
  Clflags.set_oclassic ();
  typed
  |> Profile.(record transl)
    (Translmod.transl_store_implementation i.module_name)
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Compiler_hooks.execute_and_pipe Compiler_hooks.Raw_lambda
  |> Profile.(record generate)
    (fun program ->
       let code = Simplif.simplify_lambda program.Lambda.code in
       { program with Lambda.code }
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
       |> Compiler_hooks.execute_and_pipe Compiler_hooks.Lambda
       |> Asmgen.compile_implementation
            unix
            ~backend
            ~filename:i.source_file
            ~prefixname:i.output_prefix
            ~middle_end:Closure_middle_end.lambda_to_clambda
            ~ppf_dump:i.ppf_dump;
       Compilenv.save_unit_info (cmx i))

let reset_compilenv ~module_name =
  let comp_unit =
    Compilation_unit.create (Compilation_unit.Prefix.from_clflags ())
      (Compilation_unit.Name.of_string module_name)
  in
  Compilenv.reset comp_unit

(* Emit assembly directly from Linear IR *)
let emit unix i =
  reset_compilenv ~module_name:i.module_name;
  Asmgen.compile_implementation_linear unix
    i.output_prefix ~progname:i.source_file

let implementation unix ~backend ~flambda2 ~start_from ~source_file
    ~output_prefix ~keep_symbol_tables =
  let backend info ({ structure; coercion; _ } : Typedtree.implementation) =
    reset_compilenv ~module_name:info.module_name;
    let typed = structure, coercion in
    if Config.flambda
    then flambda unix info backend typed
    else if Config.flambda2
    then flambda2_ unix info ~flambda2 ~keep_symbol_tables typed
    else clambda unix info backend typed
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmx" @@ fun info ->
  match (start_from:Clflags.Compiler_pass.t) with
  | Parsing ->
    Compile_common.implementation
      ~hook_parse_tree:(Compiler_hooks.execute Compiler_hooks.Parse_tree_impl)
      ~hook_typed_tree:(fun (impl : Typedtree.implementation) ->
        Compiler_hooks.execute Compiler_hooks.Typed_tree_impl
          (impl.structure, impl.coercion))
      info ~backend
  | Emit -> emit unix info ~ppf_dump:info.ppf_dump
  | _ -> Misc.fatal_errorf "Cannot start from %s"
           (Clflags.Compiler_pass.to_string start_from)
