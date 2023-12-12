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
  with_info ~source_file ~output_prefix ~dump_ext:"cmi" @@ fun info ->
  Compile_common.interface
    ~hook_parse_tree:(fun _ -> ())
    ~hook_typed_tree:(fun _ -> ())
    info

(** Bytecode compilation backend for .ml files. *)

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

let implementation ~start_from ~source_file ~output_prefix
    ~keep_symbol_tables:_ =
  let backend info typed =
    let bytecode = to_bytecode info typed in
    emit_bytecode info bytecode
  in
  with_info ~source_file ~output_prefix ~dump_ext:"cmo" @@ fun info ->
  match (start_from : Clflags.Compiler_pass.t) with
  | Parsing ->
    Compile_common.implementation
      ~hook_parse_tree:(fun _ -> ())
      ~hook_typed_tree:(fun _ -> ())
      info ~backend
  | _ -> Misc.fatal_errorf "Cannot start from %s"
           (Clflags.Compiler_pass.to_string start_from)
