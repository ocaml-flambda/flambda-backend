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

(* Unlike most of the rest of Flambda 2, this file depends on ocamloptcomp,
   meaning it can call [Compilenv]. *)

let get_module_info comp_unit =
  let cmx_name = Compilation_unit.name comp_unit in
  (* Typing information for predefined exceptions should be populated directly
     by the callee. *)
  if Compilation_unit.Name.equal cmx_name Compilation_unit.Name.predef_exn
  then
    Misc.fatal_error
      "get_global_info is not for use with predefined exception compilation \
       units";
  if Compilation_unit.Name.equal cmx_name
       (Flambda2_identifiers.Symbol.external_symbols_compilation_unit ()
       |> Compilation_unit.name)
  then None
  else Compilenv.get_unit_export_info comp_unit

let dump_to_target_if_any main_dump_ppf target ~header ~f a =
  match (target : Flambda_features.dump_target) with
  | Nowhere -> ()
  | Main_dump_stream ->
    Format.fprintf main_dump_ppf "\n%t%s:%t@ %a@." Flambda_colours.each_file
      header Flambda_colours.pop f a
  | File filename ->
    Misc.protect_writing_to_file ~filename ~f:(fun out ->
        let ppf = Format.formatter_of_out_channel out in
        f ppf a;
        Format.pp_print_flush ppf ())

let dump_if_enabled ppf enabled ~header ~f a =
  let target : Flambda_features.dump_target =
    if enabled then Main_dump_stream else Nowhere
  in
  dump_to_target_if_any ppf target ~header ~f a

let pp_flambda_as_fexpr ppf unit =
  Print_fexpr.flambda_unit ppf (unit |> Flambda_to_fexpr.conv)

let print_rawflambda ppf unit =
  dump_if_enabled ppf
    (Flambda_features.dump_rawflambda ())
    ~header:"After CPS conversion" ~f:Flambda_unit.print unit;
  dump_to_target_if_any ppf
    (Flambda_features.dump_rawfexpr ())
    ~header:"After CPS conversion" ~f:pp_flambda_as_fexpr unit

let print_flambda name ppf unit =
  let header = "After " ^ name in
  dump_if_enabled ppf
    (Flambda_features.dump_flambda ())
    ~header ~f:Flambda_unit.print unit;
  dump_to_target_if_any ppf
    (Flambda_features.dump_fexpr ())
    ~header ~f:pp_flambda_as_fexpr unit

let pp_flambda_as_flexpect ppf (old_unit, new_unit) =
  let before = old_unit |> Flambda_to_fexpr.conv in
  let after = new_unit |> Flambda_to_fexpr.conv in
  let test : Fexpr.expect_test_spec = { before; after } in
  Print_fexpr.expect_test_spec ppf test

let print_flexpect name main_dump_ppf ~raw_flambda:old_unit new_unit =
  dump_to_target_if_any main_dump_ppf
    (Flambda_features.dump_flexpect ())
    ~header:("Before and after " ^ name)
    ~f:pp_flambda_as_flexpect (old_unit, new_unit)

let lambda_to_cmm ~ppf_dump:ppf ~prefixname ~filename:_ ~keep_symbol_tables
    (program : Lambda.program) =
  let compilation_unit = program.compilation_unit in
  let module_block_size_in_words = program.main_module_block_size in
  let module_initializer = program.code in
  (* Make sure -linscan is enabled in classic mode. Doing this here to be sure
     it happens exactly when -Oclassic is in effect, which we don't know at CLI
     processing time because there may be an [@@@flambda_oclassic] or
     [@@@flambda_o3] attribute. *)
  if Flambda_features.classic_mode () then Clflags.use_linscan := true;
  Misc.Color.setup (Flambda_features.colour ());
  (* CR-someday mshinwell: Note for future WebAssembly work: this thing about
     the length of arrays will need fixing, I don't think it only applies to the
     Cmm translation.

     This is partially fixed now, but the float array optimization case for
     array length in the Cmm translation assumes the floats are word width. *)
  (* The Flambda 2 code won't currently operate on 32-bit hosts; see
     [Name_occurrences]. *)
  if Sys.word_size <> 64
  then Misc.fatal_error "Flambda 2 can only run on 64-bit hosts at present";
  (* At least one place in the Cmm translation code (for unboxed arrays) cannot
     cope with big-endian systems, and it seems unlikely any such systems will
     have to be supported in the future anyway. *)
  if Arch.big_endian
  then Misc.fatal_error "Flambda2 only supports little-endian hosts";
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
    let cmx_loader = Flambda_cmx.create_loader ~get_module_info in
    let (Mode mode) = Flambda_features.mode () in
    let Closure_conversion.
          { unit = raw_flambda;
            code_slot_offsets;
            metadata = close_program_metadata
          } =
      Profile.record_call "lambda_to_flambda" (fun () ->
          Lambda_to_flambda.lambda_to_flambda ~mode ~big_endian:Arch.big_endian
            ~cmx_loader ~compilation_unit ~module_block_size_in_words
            module_initializer)
    in
    Compiler_hooks.execute Raw_flambda2 raw_flambda;
    print_rawflambda ppf raw_flambda;
    let flambda, offsets, reachable_names, cmx, all_code =
      match mode, close_program_metadata with
      | Classic, Classic (code, reachable_names, cmx, offsets) ->
        (if Flambda_features.inlining_report ()
        then
          let output_prefix = prefixname ^ ".cps_conv" in
          let inlining_tree =
            Inlining_report.output_then_forget_decisions ~output_prefix
          in
          Compiler_hooks.execute Inlining_tree inlining_tree);
        raw_flambda, offsets, reachable_names, cmx, code
      | Normal, Normal ->
        let round = 0 in
        let { Simplify.unit = flambda;
              exported_offsets;
              cmx;
              all_code;
              reachable_names
            } =
          Profile.record_call ~accumulate:true "simplify" (fun () ->
              Simplify.run ~cmx_loader ~round ~code_slot_offsets raw_flambda)
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
        print_flexpect "simplify" ppf ~raw_flambda flambda;
        flambda, exported_offsets, reachable_names, cmx, all_code
    in
    (match cmx with
    | None ->
      () (* Either opaque was passed, or there is no need to export offsets *)
    | Some cmx -> Compilenv.set_export_info cmx);
    let cmm =
      Flambda2_to_cmm.To_cmm.unit flambda ~all_code ~offsets ~reachable_names
    in
    if not keep_symbol_tables
    then (
      Compilenv.reset_info_tables ();
      Flambda2_identifiers.Continuation.reset ();
      Flambda2_identifiers.Int_ids.reset ());
    cmm
  in
  Profile.record_call "flambda2" run
