open Import

let parse_flambda file =
  match Parse_flambda.parse file with
  | Ok unit -> unit
  | Error e ->
    (match e with
    | Parsing_error (msg, loc) ->
      Format.eprintf "%a:@.Syntax error: %s@." Location.print_loc loc msg
    | Lexing_error (error, loc) ->
      Format.eprintf "%a:@.Lex error: %a@." Location.print_loc loc
        Flambda_lex.pp_error error);
    exit 1

let _ =
  let file1 = Sys.argv.(1) in
  let file2 = Sys.argv.(2) in
  let unit1 = parse_flambda file1 in
  let unit2 = parse_flambda file2 in
  Format.printf "%a@."
    (Compare.Comparison.print Flambda_unit.print)
    (Compare.flambda_units unit1 unit2)
