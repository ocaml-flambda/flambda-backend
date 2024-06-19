(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Misc

type info = {
  source_file : string;
  module_name : Compilation_unit.t;
  output_prefix : string;
  env : Env.t;
  ppf_dump : Format.formatter;
  tool_name : string;
  native : bool;
}

let cmx i = i.output_prefix ^ ".cmx"
let obj i = i.output_prefix ^ Config.ext_obj
let cmo i = i.output_prefix ^ ".cmo"
let annot i = i.output_prefix ^ ".annot"

let with_info ~native ~tool_name ~source_file ~output_prefix ~dump_ext k =
  Compmisc.init_path ();
  let module_name = Compenv.module_of_filename source_file output_prefix in
  let for_pack_prefix = Compilation_unit.Prefix.from_clflags () in
  let compilation_unit =
    Compilation_unit.create for_pack_prefix
      (module_name |> Compilation_unit.Name.of_string)
  in
  Compilation_unit.set_current (Some compilation_unit);
  let env = Compmisc.initial_env() in
  let dump_file = String.concat "." [output_prefix; dump_ext] in
  Compmisc.with_ppf_dump ~file_prefix:dump_file (fun ppf_dump ->
  k {
    module_name = compilation_unit;
    output_prefix;
    env;
    source_file;
    ppf_dump;
    tool_name;
    native;
  })

(** Compile a .mli file *)

let parse_intf i =
  Pparse.parse_interface ~tool_name:i.tool_name i.source_file
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.interface
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.signature

let typecheck_intf info ast =
  Profile.(record_call typing) @@ fun () ->
  let tsg =
    ast
    |> Typemod.type_interface
         ~sourcefile:info.source_file info.module_name info.env
    |> print_if info.ppf_dump Clflags.dump_typedtree Printtyped.interface
  in
  let sg = tsg.Typedtree.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env ~error:false info.env (fun () ->
        Format.(fprintf std_formatter) "%a@."
          (Printtyp.printed_signature info.source_file)
          sg);
  ignore (Includemod.signatures info.env ~mark:Mark_both sg sg);
  Typecore.force_delayed_checks ();
  Builtin_attributes.warn_unused ();
  Warnings.check_fatal ();
  tsg

let emit_signature info ast tsg =
  let sg =
    let name = Compilation_unit.name info.module_name in
    let kind : Cmi_format.kind =
      if !Clflags.as_parameter then
        Parameter
      else begin
        let cmi_arg_for =
          match !Clflags.as_argument_for with
          | Some arg_type -> Some (Compilation_unit.Name.of_string arg_type)
          | None -> None
        in
        Normal { cmi_impl = info.module_name; cmi_arg_for }
      end
    in
    let alerts = Builtin_attributes.alerts_of_sig ast in
    Env.save_signature ~alerts tsg.Typedtree.sig_type
      name kind (info.output_prefix ^ ".cmi")
  in
  Typemod.save_signature info.module_name tsg
    info.output_prefix info.source_file info.env sg

let interface ~hook_parse_tree ~hook_typed_tree info =
  Profile.record_call info.source_file @@ fun () ->
  let ast = parse_intf info in
  hook_parse_tree ast;
  if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
    let tsg = typecheck_intf info ast in
    hook_typed_tree tsg;
    if not !Clflags.print_types then begin
      emit_signature info ast tsg
    end
  end


(** Frontend for a .ml file *)

let parse_impl i =
  Pparse.parse_implementation ~tool_name:i.tool_name i.source_file
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.implementation
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.structure

let typecheck_impl i parsetree =
  parsetree
  |> Profile.(record typing)
    (Typemod.type_implementation
       ~sourcefile:i.source_file i.output_prefix i.module_name i.env)
  |> print_if i.ppf_dump Clflags.dump_typedtree
    Printtyped.implementation_with_coercion
  |> print_if i.ppf_dump Clflags.dump_shape
    (fun fmt {Typedtree.shape; _} -> Shape.print fmt shape)

let implementation ~hook_parse_tree ~hook_typed_tree info ~backend =
  Profile.record_call info.source_file @@ fun () ->
  let exceptionally () =
    let sufs = if info.native then [ cmx; obj ] else [ cmo ] in
    List.iter (fun suf -> remove_file (suf info)) sufs;
  in
  Misc.try_finally ?always:None ~exceptionally (fun () ->
    let parsed = parse_impl info in
    hook_parse_tree parsed;
    if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
      let typed = typecheck_impl info parsed in
      hook_typed_tree typed;
      if Clflags.(should_stop_after Compiler_pass.Typing) then () else begin
        backend info typed;
      end;
    end;
    if not (Clflags.(should_stop_after Compiler_pass.Selection)) then
      Builtin_attributes.warn_unchecked_zero_alloc_attribute ();
    Warnings.check_fatal ();
  )
