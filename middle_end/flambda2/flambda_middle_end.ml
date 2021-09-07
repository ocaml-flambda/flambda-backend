(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type middle_end_result =
  { (* CR mshinwell: This next field is redundant *)
    cmx : Flambda_cmx_format.t option;
    unit : Flambda_unit.t;
    all_code : Exported_code.t
  }

let check_invariants unit =
  try Flambda_unit.invariant unit
  with exn ->
    Format.eprintf "Unit which failed invariant check:@ %a\n%!"
      Flambda_unit.print unit;
    raise exn

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
  then Format.fprintf ppf "\n%sAfter %s:%s@ %a@."
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

let output_flexpect ~ml_filename old_unit new_unit =
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

let middle_end0 ppf ~prefixname ~backend ~filename ~module_ident
    ~module_block_size_in_words ~module_initializer =
  Misc.Color.setup (Flambda_features.colour ());
  Profile.record_call "flambda.0" (fun () ->
      let flambda =
        Profile.record_call "lambda_to_flambda" (fun () ->
            Lambda_to_flambda.lambda_to_flambda ~backend ~module_ident
              ~module_block_size_in_words module_initializer)
      in
      print_rawflambda ppf flambda;
      check_invariants flambda;
      let flambda =
        if Flambda_features.Debug.permute_every_name ()
        then Flambda_unit.permute_everything flambda
        else flambda
      in
      let round = 0 in
      let new_flambda =
        Profile.record_call ~accumulate:true "simplify" (fun () ->
            Simplify.run ~backend ~round flambda)
      in
      (if Flambda_features.inlining_report ()
      then
        let output_prefix = Printf.sprintf "%s.%d" prefixname round in
        Inlining_report.output_then_forget_decisions ~output_prefix);
      print_flambda "simplify" ppf new_flambda.unit;
      output_flexpect ~ml_filename:filename flambda new_flambda.unit;
      new_flambda)

let middle_end ~ppf_dump:ppf ~prefixname ~backend ~filename ~module_ident
    ~module_block_size_in_words ~module_initializer : middle_end_result =
  let simplify_result =
    middle_end0 ppf ~prefixname ~backend ~filename ~module_ident
      ~module_block_size_in_words ~module_initializer
  in
  begin
    match Sys.getenv "PRINT_SIZES" with
    | exception Not_found -> ()
    | _ ->
      Exported_code.iter simplify_result.all_code (fun id code ->
          let size = Flambda.Code.cost_metrics code in
          Format.fprintf Format.std_formatter "%a %a\n" Code_id.print id
            Flambda.Cost_metrics.print size)
  end;
  { cmx = simplify_result.cmx;
    unit = simplify_result.unit;
    all_code = simplify_result.all_code
  }
