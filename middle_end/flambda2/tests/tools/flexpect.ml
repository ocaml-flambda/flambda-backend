open Import

(* Dune expects that on failure we [exit 0] but write a corrected file. *)
(* CR lmaurer: Make this an argument. *)
let exit_normally_on_failure = true

let get_module_info = Flambda2.get_module_info

let check_invariants program =
  try () (* Flambda_unit.invariant program *)
  with exn ->
    Format.eprintf "Program which failed invariant check:@ %a\n%!"
      Flambda_unit.print program;
    raise exn

module Outcome = struct
  type t =
    | Success
    | Failure
    | Error

  let to_exit_code = function
    | Success -> 0
    | Failure when exit_normally_on_failure -> 0
    | Failure -> 1
    | Error -> 2
end

module Test_outcome = struct
  type t =
    | Pass
    | Fail of { corrected : Fexpr.expect_test_spec }
end

let run_expect_test ~get_module_info ~extension ~filename
    ({ before; after = expected } : Fexpr.expect_test_spec) : Test_outcome.t =
  let comp_unit = Parse_flambda.make_compilation_unit ~extension ~filename () in
  Compilation_unit.set_current (Some comp_unit);
  let before_fl = Fexpr_to_flambda.conv comp_unit before in
  check_invariants before_fl;
  let cmx_loader = Flambda_cmx.create_loader ~get_module_info in
  let { Simplify.unit = actual_fl; _ } =
    Simplify.run ~cmx_loader ~round:0 before_fl
  in
  let expected_fl = Fexpr_to_flambda.conv comp_unit expected in
  match Compare.flambda_units actual_fl expected_fl with
  | Equivalent -> Pass
  | Different { approximant = actual' } ->
    let actual_fexpr = Flambda_to_fexpr.conv actual' in
    Fail { corrected = { before; after = actual_fexpr } }

let show_diff a b =
  let command_line = Filename.quote_command "diff" ["-u"; a; b] in
  let _exit_code = Sys.command command_line in
  ()

let save_corrected ~desc ~print ~orig_filename corrected =
  let corrected_filename = orig_filename ^ ".corrected" in
  Format.eprintf "Saving corrected %s as %s@." desc corrected_filename;
  let corrected_out = open_out corrected_filename in
  Misc.try_finally
    ~always:(fun () -> close_out corrected_out)
    (fun () ->
      let ppf = corrected_out |> Format.formatter_of_out_channel in
      print ppf corrected;
      Format.pp_print_flush ppf ());
  if false then show_diff orig_filename corrected_filename

let run_flt_file filename : Outcome.t =
  match Parse_flambda.parse_expect_test_spec filename with
  | Ok test_spec -> (
    match
      run_expect_test ~get_module_info ~extension:".flt" ~filename test_spec
    with
    | Pass ->
      Format.eprintf "%s: PASS@." filename;
      Success
    | Fail { corrected } ->
      Format.eprintf "%s: FAIL@." filename;
      save_corrected corrected ~desc:"test" ~print:Print_fexpr.expect_test_spec
        ~orig_filename:filename;
      Failure)
  | Error e ->
    Test_utils.dump_error e;
    Error

let run_mdflx_file filename : Outcome.t =
  match Parse_flambda.parse_markdown_doc filename with
  | Ok doc ->
    let all_passed = ref true in
    let corrected_doc =
      List.map
        (fun (node : Fexpr.markdown_node) : Fexpr.markdown_node ->
          match node with
          | Text _ -> node
          | Expect test_spec -> (
            match
              run_expect_test test_spec ~get_module_info ~extension:".mdflx"
                ~filename
            with
            | Pass ->
              Format.eprintf "PASS@.";
              node
            | Fail { corrected } ->
              all_passed := false;
              Format.eprintf "FAIL@.";
              Expect corrected))
        doc
    in
    if !all_passed
    then Outcome.Success
    else (
      save_corrected corrected_doc ~desc:"document"
        ~print:Print_fexpr.markdown_doc ~orig_filename:filename;
      Failure)
  | Error e ->
    Test_utils.dump_error e;
    Error

let _ =
  if not Config.stack_allocation
  then (
    Printf.printf "flexpect not supported when stack allocation disabled";
    exit 0);
  let file = Sys.argv.(1) in
  let ext = Filename.extension file in
  let outcome =
    match ext with
    | ".flt" -> run_flt_file file
    | ".mdflx" -> run_mdflx_file file
    | _ ->
      Misc.fatal_errorf "Unrecognized extension %s; expected .flt or .mdflx" ext
  in
  exit (outcome |> Outcome.to_exit_code)
