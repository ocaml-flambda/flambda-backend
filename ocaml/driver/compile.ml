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

open Misc
open Compile_common

let tool_name = "ocamlc"

let with_info =
  Compile_common.with_info ~native:false ~tool_name

let interface ~source_file ~output_prefix =
  with_info ~source_file ~output_prefix ~dump_ext:"cmi"
    ~compilation_unit:Inferred_from_output_prefix
  @@ fun info ->
  Compile_common.interface
    ~hook_parse_tree:(fun _ -> ())
    ~hook_typed_tree:(fun _ -> ())
    info

(** Bytecode compilation backend for .ml files. *)

let raw_lambda_to_bytecode i raw_lambda =
  raw_lambda
  |> Profile.(record ~accumulate:true generate)
    (fun { Lambda.code = lambda; required_globals; arg_block_field } ->
       lambda
       |> print_if i.ppf_dump Clflags.dump_rawlambda Printlambda.lambda
       |> Simplif.simplify_lambda
       |> print_if i.ppf_dump Clflags.dump_lambda Printlambda.lambda
       |> Bytegen.compile_implementation
            (i.module_name |> Compilation_unit.name_as_string)
       |> print_if i.ppf_dump Clflags.dump_instr Printinstr.instrlist
       |> fun bytecode -> bytecode, required_globals, arg_block_field
    )

let to_bytecode i Typedtree.{structure; coercion; secondary_iface; _} =
  let secondary_coercion =
    match secondary_iface with
    | Some { si_coercion_from_primary; si_signature = _ } ->
        Some si_coercion_from_primary
    | None -> None
  in
  (structure, coercion, secondary_coercion)
  |> Profile.(record transl)
    (Translmod.transl_implementation i.module_name ~style:Set_global_to_block)
  |> raw_lambda_to_bytecode i

let emit_bytecode i (bytecode, required_globals, arg_block_field) =
  let cmofile = cmo i in
  let oc = open_out_bin cmofile in
  Misc.try_finally
    ~always:(fun () -> close_out oc)
    ~exceptionally:(fun () -> Misc.remove_file cmofile)
    (fun () ->
       bytecode
       |> Profile.(record ~accumulate:true generate)
         (Emitcode.to_file oc i.module_name cmofile ~required_globals
            ~arg_block_field);
    )

type starting_point =
  | Parsing
  | Instantiation of { runtime_params : Global.t list }

let starting_point_of_compiler_pass start_from =
  match (start_from:Clflags.Compiler_pass.t) with
  | Parsing -> Parsing
  | _ -> Misc.fatal_errorf "Cannot start from %s"
           (Clflags.Compiler_pass.to_string start_from)

let implementation0 ~start_from ~source_file ~output_prefix
    ~keep_symbol_tables:_
    ~(compilation_unit : Compile_common.compilation_unit_or_inferred) =
  let backend info typed =
    let bytecode = to_bytecode info typed in
    emit_bytecode info bytecode
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmo" ~compilation_unit
  @@ fun info ->
  match start_from with
  | Parsing ->
    Compile_common.implementation
      ~hook_parse_tree:(fun _ -> ())
      ~hook_typed_tree:(fun _ -> ())
      info ~backend
  | Instantiation { runtime_params } ->
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
    let impl =
      Translmod.transl_instance info.module_name ~runtime_params
        ~style:Set_global_to_block
    in
    let bytecode = raw_lambda_to_bytecode info impl in
    emit_bytecode info bytecode

let implementation ~start_from ~source_file ~output_prefix ~keep_symbol_tables =
  let start_from = start_from |> starting_point_of_compiler_pass in
  implementation0 ~start_from ~source_file ~output_prefix ~keep_symbol_tables
    ~compilation_unit:Inferred_from_output_prefix

let instance ~source_file ~output_prefix ~compilation_unit ~runtime_params
    ~keep_symbol_tables =
  let start_from = Instantiation { runtime_params } in
  implementation0 ~start_from ~source_file ~output_prefix ~keep_symbol_tables
    ~compilation_unit:(Exactly compilation_unit)
