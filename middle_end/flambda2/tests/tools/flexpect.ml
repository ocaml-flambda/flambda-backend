open Import

(* Dune expects that on failure we [exit 0] but write a corrected file. *)
(* CR lmaurer: Make this an argument. *)
let exit_normally_on_failure = true

let symbol_for_global = Flambda2.symbol_for_global

let get_global_info = Flambda2.get_global_info

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

let dump_error (e : Parse_flambda.error) =
  match e with
  | Parsing_error (msg, loc) ->
    Format.eprintf "%a:@.Syntax error: %s@." Location.print_loc loc msg
  | Lexing_error (error, loc) ->
    Format.eprintf "%a:@.Lex error: %a@." Location.print_loc loc
      Flambda_lex.pp_error error

let run_expect_test ~symbol_for_global ~get_global_info ~extension ~filename
    ({ before; after = expected } : Fexpr.expect_test_spec) : Test_outcome.t =
  let comp_unit = Parse_flambda.make_compilation_unit ~extension ~filename () in
  Compilation_unit.set_current comp_unit;
  let module_ident = Compilation_unit.get_persistent_ident comp_unit in
  let before_fl =
    Fexpr_to_flambda.conv ~symbol_for_global ~module_ident before
  in
  check_invariants before_fl;
  let { Simplify.unit = actual_fl; _ } =
    Simplify.run ~symbol_for_global ~get_global_info ~round:0 before_fl
  in
  let expected_fl =
    Fexpr_to_flambda.conv ~symbol_for_global ~module_ident expected
  in
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
  | Ok test_spec -> begin
    match
      run_expect_test ~symbol_for_global ~get_global_info ~extension:".flt"
        ~filename test_spec
    with
    | Pass ->
      Format.eprintf "PASS@.";
      Success
    | Fail { corrected } ->
      Format.eprintf "FAIL@.";
      save_corrected corrected ~desc:"test" ~print:Print_fexpr.expect_test_spec
        ~orig_filename:filename;
      Failure
  end
  | Error e ->
    dump_error e;
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
              run_expect_test test_spec ~symbol_for_global ~get_global_info
                ~extension:".mdflx" ~filename
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
    else begin
      save_corrected corrected_doc ~desc:"document"
        ~print:Print_fexpr.markdown_doc ~orig_filename:filename;
      Failure
    end
  | Error e ->
    dump_error e;
    Error

let _ =
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
