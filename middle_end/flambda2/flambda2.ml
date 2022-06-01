(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

(* Unlike the rest of Flambda 2, this file depends on ocamloptcomp, meaning it
   can call [Compilenv]. *)

module Flambda1_compilation_unit = Compilation_unit
module Flambda1_linkage_name = Linkage_name

module Compilation_unit = struct
  include Flambda2_identifiers.Compilation_unit

  let of_flambda1_compilation_unit comp_unit =
    let ident = Flambda1_compilation_unit.get_persistent_ident comp_unit in
    let linkage_name =
      comp_unit |> Flambda1_compilation_unit.get_linkage_name
      |> Flambda1_linkage_name.to_string
      |> Flambda2_identifiers.Linkage_name.create
    in
    create ~name:(Ident.name ident) linkage_name
end

module Linkage_name = Flambda2_identifiers.Linkage_name
module Symbol = Flambda2_identifiers.Symbol

let symbol_for_module_block id =
  assert (Ident.global id);
  assert (not (Ident.is_predef id));
  let comp_unit =
    Compilenv.unit_for_global id
    |> Compilation_unit.of_flambda1_compilation_unit
  in
  Symbol.unsafe_create comp_unit
    (Linkage_name.create (Compilenv.symbol_for_global id))

let symbol_for_global ?comp_unit id =
  if Ident.global id && not (Ident.is_predef id)
  then symbol_for_module_block id
  else
    let comp_unit =
      match comp_unit with
      | Some comp_unit -> comp_unit
      | None ->
        if Ident.is_predef id
        then Compilation_unit.predefined_exception ()
        else Compilation_unit.get_current_exn ()
    in
    Symbol.unsafe_create comp_unit
      (Linkage_name.create (Compilenv.symbol_for_global id))

let get_global_info comp_unit =
  (* Typing information for predefined exceptions should be populated directly
     by the callee. *)
  if Compilation_unit.is_predefined_exception comp_unit
  then
    Misc.fatal_error
      "get_global_info is not for use with predefined exception compilation \
       units";
  if Compilation_unit.is_external_symbols comp_unit
  then None
  else
    let id = Compilation_unit.get_persistent_ident comp_unit in
    match Compilenv.get_global_info' id with
    | None | Some (Flambda2 None) -> None
    | Some (Flambda2 (Some info)) -> Some info
    | Some (Clambda _) ->
      (* CR mshinwell: This should be a user error, not a fatal error. Same
         below. *)
      Misc.fatal_errorf
        "The .cmx file for unit %a was compiled with the Closure middle-end, \
         not Flambda 2, and cannot be loaded"
        Compilation_unit.print comp_unit
    | Some (Flambda1 _) ->
      Misc.fatal_errorf
        "The .cmx file for unit %a was compiled with the Flambda 1 middle-end, \
         not Flambda 2, and cannot be loaded"
        Compilation_unit.print comp_unit

let print_rawflambda ppf unit =
  if Flambda_features.dump_rawflambda ()
  then
    Format.fprintf ppf "\n%sAfter CPS conversion:%s@ %a@."
      (Flambda_colours.each_file ())
      (Flambda_colours.normal ())
      Flambda_unit.print unit;
  if Flambda_features.dump_rawfexpr ()
  then
    Format.fprintf ppf "\n%sAfter CPS conversion:%s@ %a@."
      (Flambda_colours.each_file ())
      (Flambda_colours.normal ())
      Print_fexpr.flambda_unit
      (unit |> Flambda_to_fexpr.conv)

let print_flambda name ppf unit =
  if Flambda_features.dump_flambda ()
  then
    Format.fprintf ppf "\n%sAfter %s:%s@ %a@."
      (Flambda_colours.each_file ())
      name
      (Flambda_colours.normal ())
      Flambda_unit.print unit;
  if Flambda_features.dump_fexpr ()
  then
    Format.fprintf ppf "\n%sAfter %s:%s@ %a@."
      (Flambda_colours.each_file ())
      name
      (Flambda_colours.normal ())
      Print_fexpr.flambda_unit
      (unit |> Flambda_to_fexpr.conv)

let output_flexpect ~ml_filename ~raw_flambda:old_unit new_unit =
  if Flambda_features.dump_flexpect ()
  then
    let basename = Filename.chop_suffix ml_filename ".ml" in
    let filename = basename ^ ".flt" in
    let before = old_unit |> Flambda_to_fexpr.conv in
    let after = new_unit |> Flambda_to_fexpr.conv in
    let test : Fexpr.expect_test_spec = { before; after } in
    let out = open_out filename in
    Misc.try_finally
      ~always:(fun () -> close_out out)
      (fun () ->
        let ppf = out |> Format.formatter_of_out_channel in
        Print_fexpr.expect_test_spec ppf test;
        Format.pp_print_flush ppf ())

let lambda_to_cmm ~ppf_dump:ppf ~prefixname ~filename ~module_ident
    ~module_block_size_in_words ~module_initializer ~keep_symbol_tables =
  (* Make sure -linscan is enabled in classic mode. Doing this here to be sure
     it happens exactly when -Oclassic is in effect, which we don't know at CLI
     processing time because there may be an [@@@flambda_oclassic] or
     [@@@flambda_o3] attribute. *)
  if Flambda_features.classic_mode () then Clflags.use_linscan := true;
  Misc.Color.setup (Flambda_features.colour ());
  (* When the float array optimisation is enabled, the length of an array needs
     to be computed differently according to the array kind, in the case where
     the width of a float is not equal to the machine word width (at present,
     this happens only on 32-bit targets). *)
  if Cmm_helpers.wordsize_shift <> Cmm_helpers.numfloat_shift
     && Flambda_features.flat_float_array ()
  then
    Misc.fatal_error
      "Cannot compile on targets where floats are not word-width when the \
       float array optimisation is enabled";
  let run () =
    let cmx_loader =
      Flambda_cmx.create_loader ~get_global_info ~symbol_for_global
    in
    let raw_flambda, code, cmx, offsets =
      Profile.record_call "lambda_to_flambda" (fun () ->
          Lambda_to_flambda.lambda_to_flambda ~symbol_for_global
            ~big_endian:Arch.big_endian ~cmx_loader ~module_ident
            ~module_block_size_in_words module_initializer)
    in
    Compiler_hooks.execute Raw_flambda2 raw_flambda;
    print_rawflambda ppf raw_flambda;
    let flambda, offsets, cmx, all_code =
      if Flambda_features.classic_mode ()
      then begin
        (if Flambda_features.inlining_report ()
        then
          let output_prefix = prefixname ^ ".cps_conv" in
          let inlining_tree =
            Inlining_report.output_then_forget_decisions ~output_prefix
          in
          Compiler_hooks.execute Inlining_tree inlining_tree);
        raw_flambda, offsets, cmx, code
      end
      else
        let round = 0 in
        let { Simplify.unit = flambda; exported_offsets; cmx; all_code } =
          Profile.record_call ~accumulate:true "simplify" (fun () ->
              Simplify.run ~cmx_loader ~round raw_flambda)
        in
        (if Flambda_features.inlining_report ()
        then
          let output_prefix = Printf.sprintf "%s.%d" prefixname round in
          let inlining_tree =
            Inlining_report.output_then_forget_decisions ~output_prefix
          in
          Compiler_hooks.execute Inlining_tree inlining_tree);
        Compiler_hooks.execute Flambda2 flambda;
        print_flambda "simplify" ppf flambda;
        output_flexpect ~ml_filename:filename ~raw_flambda flambda;
        flambda, exported_offsets, cmx, all_code
    in
    begin
      match Sys.getenv "PRINT_SIZES" with
      | exception Not_found -> ()
      | _ ->
        Exported_code.iter_code all_code ~f:(fun code ->
            let size = Code.cost_metrics code in
            Format.fprintf Format.std_formatter "%a %a\n"
              Flambda2_identifiers.Code_id.print (Code.code_id code)
              Cost_metrics.print size)
    end;
    let cmm, offsets =
      Flambda2_to_cmm.To_cmm.unit ~make_symbol:Compilenv.make_symbol flambda
        ~all_code ~offsets
    in
    begin
      match cmx with
      | None -> assert !Clflags.opaque
      | Some cmx ->
        let cmx =
          Flambda_cmx_format.with_exported_offsets cmx offsets
          |> Flambda_cmx_format.prepare_for_serialization
        in
        Compilenv.flambda2_set_export_info cmx
    end;
    if not keep_symbol_tables
    then begin
      Compilenv.reset_info_tables ();
      Flambda2_identifiers.Continuation.reset ();
      Flambda2_identifiers.Int_ids.reset ()
    end;
    cmm
  in
  Profile.record_call "flambda2" run
