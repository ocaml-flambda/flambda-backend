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
  target: Unit_info.t;
  module_name : Compilation_unit.t;
  env : Env.t;
  ppf_dump : Format.formatter;
  tool_name : string;
  native : bool;
}

let with_info ~native ~tool_name ~source_file ~output_prefix ~dump_ext k =
  Compmisc.init_path ();
  let target = Unit_info.make ~source_file output_prefix in
  let module_name = Unit_info.modname target in
  let for_pack_prefix = Compilation_unit.Prefix.from_clflags () in
  let compilation_unit =
    Compilation_unit.create for_pack_prefix
      (module_name |> Compilation_unit.Name.of_string)
  in
  Compilation_unit.set_current (Some compilation_unit);
  Env.set_unit_name (Some compilation_unit);
  let env = Compmisc.initial_env() in
  let dump_file = String.concat "." [output_prefix; dump_ext] in
  Compmisc.with_ppf_dump ~file_prefix:dump_file (fun ppf_dump ->
  k {
    target;
    module_name = compilation_unit;
    env;
    ppf_dump;
    tool_name;
    native;
  })

(** Compile a .mli file *)

let parse_intf i =
  Pparse.parse_interface ~tool_name:i.tool_name (Unit_info.source_file i.target)
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.interface
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.signature

let typecheck_intf info ast =
  Profile.(
    record_call_with_counters
      ~counter_f:(fun (_alerts, signature) ->
        Profile_counters_functions.(
          count_language_extensions (Typedtree_signature_output signature)))
      typing)
  @@ fun () ->
  let tsg =
    ast
    |> Typemod.type_interface
         ~sourcefile:(Unit_info.source_file info.target)
         info.module_name info.env
    |> print_if info.ppf_dump Clflags.dump_typedtree Printtyped.interface
  in
  let alerts = Builtin_attributes.alerts_of_sig ~mark:true ast in
  let sg = tsg.Typedtree.sig_type in
  if !Clflags.print_types then
    Printtyp.wrap_printing_env ~error:false info.env (fun () ->
        Format.(fprintf std_formatter) "%a@."
          (Printtyp.printed_signature (Unit_info.source_file info.target))
          sg);
  ignore (Includemod.signatures info.env ~mark:Mark_both sg sg);
  Typecore.force_delayed_checks ();
  Builtin_attributes.warn_unused ();
  Warnings.check_fatal ();
  alerts, tsg

let emit_signature info alerts tsg =
  let sg =
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
    Env.save_signature ~alerts tsg.Typedtree.sig_type
      (Compilation_unit.name info.module_name) kind
      (Unit_info.cmi info.target)
  in
  Typemod.save_signature info.target info.module_name tsg info.env sg

let interface ~hook_parse_tree ~hook_typed_tree info =
  Profile.(record_call (annotate_file_name (
    Unit_info.source_file info.target))) @@ fun () ->
  let ast = parse_intf info in
  hook_parse_tree ast;
  if Clflags.(should_stop_after Compiler_pass.Parsing) then () else begin
    let alerts, tsg = typecheck_intf info ast in
    hook_typed_tree tsg;
    if not !Clflags.print_types then begin
      emit_signature info alerts tsg
    end
  end


(** Frontend for a .ml file *)

let parse_impl i =
  let sourcefile = Unit_info.source_file i.target in
  Pparse.parse_implementation ~tool_name:i.tool_name sourcefile
  |> print_if i.ppf_dump Clflags.dump_parsetree Printast.implementation
  |> print_if i.ppf_dump Clflags.dump_source Pprintast.structure

let typecheck_impl i parsetree =
  parsetree
  |> Profile.(
    record_with_counters
      ~counter_f:(fun (typed_tree : Typedtree.implementation) ->
        Profile_counters_functions.(
          count_language_extensions
            (Typedtree_implementation_output typed_tree)))
      typing)
    (Typemod.type_implementation i.target i.module_name i.env)
  |> print_if i.ppf_dump Clflags.dump_typedtree
    Printtyped.implementation_with_coercion
  |> print_if i.ppf_dump Clflags.dump_shape
    (fun fmt {Typedtree.shape; _} -> Shape.print fmt shape)

let implementation ~hook_parse_tree ~hook_typed_tree info ~backend =
  Profile.(record_call (annotate_file_name (
    Unit_info.source_file info.target))) @@ fun () ->
  let exceptionally () =
    let sufs =
      if info.native then Unit_info.[ cmx; obj ]
      else Unit_info.[ cmo ] in
    List.iter
      (fun suf -> remove_file (Unit_info.Artifact.filename @@ suf info.target))
      sufs;
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
    Builtin_attributes.warn_unused ();
    if not (Clflags.(should_stop_after Compiler_pass.Selection)) then
      Builtin_attributes.warn_unchecked_zero_alloc_attribute ();
    Warnings.check_fatal ();
  )
