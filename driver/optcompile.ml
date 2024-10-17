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
  with_info ~source_file ~output_prefix ~dump_ext:"cmi"
    ~compilation_unit:Inferred_from_output_prefix
  @@ fun info ->
  Compile_common.interface
  ~hook_parse_tree:(Compiler_hooks.execute Compiler_hooks.Parse_tree_intf)
  ~hook_typed_tree:(Compiler_hooks.execute Compiler_hooks.Typed_tree_intf)
    info

(** Native compilation backend for .ml files. *)

let make_arg_descr ~param ~arg_block_field : Lambda.arg_descr option =
  match param, arg_block_field with
  | Some arg_param, Some arg_block_field -> Some { arg_param; arg_block_field }
  | None, None -> None
  | Some _, None -> Misc.fatal_error "No argument field"
  | None, Some _ -> Misc.fatal_error "Unexpected argument field"

let compile_from_raw_lambda i raw_lambda ~unix ~pipeline ~as_arg_for =
  raw_lambda
  |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.program
  |> Compiler_hooks.execute_and_pipe Compiler_hooks.Raw_lambda
  |> Profile.(record generate)
   (fun (program : Lambda.program) ->
      Builtin_attributes.warn_unused ();
      let code = Simplif.simplify_lambda program.Lambda.code in
      { program with Lambda.code }
      |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.program
      |> Compiler_hooks.execute_and_pipe Compiler_hooks.Lambda
      |> (fun (program : Lambda.program) ->
           Asmgen.compile_implementation
             unix
             ~pipeline
             ~sourcefile:(Some (Unit_info.source_file i.target))
             ~prefixname:(Unit_info.prefix i.target)
             ~ppf_dump:i.ppf_dump
             program);
           let arg_descr =
             make_arg_descr ~param:as_arg_for
               ~arg_block_field:program.arg_block_field
           in
           Compilenv.save_unit_info
             (Unit_info.Artifact.filename (Unit_info.cmx i.target))
             ~module_block_format:program.module_block_format
             ~arg_descr)

let compile_from_typed i typed ~transl_style ~unix ~pipeline ~as_arg_for =
  typed
  |> Profile.(record transl)
    (Translmod.transl_implementation i.module_name ~style:transl_style)
  |> compile_from_raw_lambda i ~unix ~pipeline ~as_arg_for

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

type starting_point =
  | Parsing
  | Emit
  | Instantiation of {
      runtime_args : Translmod.runtime_arg list;
      main_module_block_size : int;
      arg_descr : Lambda.arg_descr option;
  }

let starting_point_of_compiler_pass start_from  =
  match (start_from:Clflags.Compiler_pass.t) with
  | Parsing -> Parsing
  | Emit -> Emit
  | _ -> Misc.fatal_errorf "Cannot start from %s"
           (Clflags.Compiler_pass.to_string start_from)

let implementation0 unix ~(flambda2 : flambda2) ~start_from
      ~source_file ~output_prefix ~keep_symbol_tables
      ~(compilation_unit : Compile_common.compilation_unit_or_inferred) =
  let transl_style : Translmod.compilation_unit_style =
    if Config.flambda || Config.flambda2 then Plain_block
    else Set_individual_fields
  in
  let pipeline : Asmgen.pipeline =
    Direct_to_cmm (flambda2 ~keep_symbol_tables)
  in
  let backend info ({ structure; coercion; argument_interface; _ }
                    : Typedtree.implementation) =
    Compilenv.reset info.module_name;
    let argument_coercion =
      match argument_interface with
      | Some { ai_coercion_from_primary; ai_signature = _ } ->
        Some ai_coercion_from_primary
      | None -> None
    in
    let typed = structure, coercion, argument_coercion in
    let as_arg_for =
      !Clflags.as_argument_for
      |> Option.map (fun param ->
           (* Currently, parameters don't have parameters, so we assume the argument
              list is empty *)
           Global_module.Name.create_exn param [])
    in
    if not (Config.flambda || Config.flambda2) then Clflags.set_oclassic ();
    compile_from_typed info typed ~unix ~transl_style ~pipeline ~as_arg_for
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmx" ~compilation_unit
  @@ fun info ->
  if !Flambda_backend_flags.internal_assembler then
      Emitaux.binary_backend_available := true;
  match start_from with
  | Parsing ->
    Compile_common.implementation
      ~hook_parse_tree:(Compiler_hooks.execute Compiler_hooks.Parse_tree_impl)
      ~hook_typed_tree:(fun (impl : Typedtree.implementation) ->
        Compiler_hooks.execute Compiler_hooks.Typed_tree_impl impl)
      info ~backend
  | Emit -> emit unix info ~ppf_dump:info.ppf_dump
  | Instantiation { runtime_args; main_module_block_size; arg_descr } ->
    Compilenv.reset info.module_name;
    (* FIXME delete {[
    let global_name =
      Compilation_unit.to_global_name_exn info.module_name
    in
    (* Consider the names of arguments to be parameters for the purposes of the
       subset rule - that is, a module we import can refer to our arguments as
       parameters. *)
    List.iter
      (fun (param, _value) ->
        let import = Compilation_unit.Name.of_head_of_global_name param in
        Env.register_parameter_import import)
      global_name.args;
    ]} *)
    let as_arg_for, arg_block_field =
      match (arg_descr : Lambda.arg_descr option) with
      | Some { arg_param; arg_block_field } ->
        Some arg_param, Some arg_block_field
      | None -> None, None
    in
    let impl =
      Translmod.transl_instance info.module_name ~runtime_args
        ~main_module_block_size ~arg_block_field
        ~style:transl_style
    in
    if not (Config.flambda || Config.flambda2) then Clflags.set_oclassic ();
    compile_from_raw_lambda info impl ~unix ~pipeline ~as_arg_for

let implementation unix ~flambda2 ~start_from ~source_file
      ~output_prefix ~keep_symbol_tables =
  let start_from = start_from |> starting_point_of_compiler_pass in
  implementation0 unix ~flambda2 ~start_from ~source_file
    ~output_prefix ~keep_symbol_tables
    ~compilation_unit:Inferred_from_output_prefix

let instance unix ~flambda2 ~source_file
      ~output_prefix ~compilation_unit ~runtime_args ~main_module_block_size
      ~arg_descr ~keep_symbol_tables =
  let start_from =
    Instantiation { runtime_args; main_module_block_size; arg_descr }
  in
  implementation0 unix ~flambda2 ~start_from ~source_file
    ~output_prefix ~keep_symbol_tables
    ~compilation_unit:(Exactly compilation_unit)
