open Import

exception Failure

let dump_error (e : Parse_flambda.error) =
  match e with
  | Parsing_error (msg, loc) ->
    Format.eprintf "%a:@.Syntax error: %s@." Location.print_loc loc msg
  | Lexing_error (error, loc) ->
    Format.eprintf "%a:@.Lex error: %a@." Location.print_loc loc
      Flambda_lex.pp_error error

let parse_flambda file =
  match Parse_flambda.parse file with
  | Ok unit -> unit
  | Error e ->
    dump_error e;
    raise Failure
