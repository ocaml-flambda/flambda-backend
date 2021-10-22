open Import

let symbol_for_global = Flambda2.symbol_for_global

let get_global_info = Flambda2.get_global_info

let check_invariants program =
  try () (* Flambda_unit.invariant program *)
  with exn ->
    Format.eprintf "Program which failed invariant check:@ %a\n%!"
      Flambda_unit.print program;
    raise exn

let parse_flambda filename =
  match Parse_flambda.parse_fexpr filename with
  | Ok unit ->
    let comp_unit =
      Parse_flambda.make_compilation_unit ~extension:".fl" ~filename ()
    in
    Compilation_unit.set_current comp_unit;
    Format.printf "%a@.@." Print_fexpr.flambda_unit unit;
    let module_ident = Compilation_unit.get_persistent_ident comp_unit in
    let fl2 = Fexpr_to_flambda.conv ~symbol_for_global ~module_ident unit in
    Format.printf "flambda:@.%a@.@." Flambda_unit.print fl2;
    check_invariants fl2;
    let { Simplify.unit = fl2'; _ } =
      Simplify.run ~symbol_for_global ~get_global_info ~round:1 fl2
    in
    Format.printf "simplify:@.%a@." Flambda_unit.print fl2';
    let fl3 = Flambda_to_fexpr.conv fl2' in
    Format.printf "back to fexpr:@.%a@." Print_fexpr.flambda_unit fl3;
    fl3
  | Error e ->
    begin
      match e with
      | Parsing_error (msg, loc) ->
        Format.eprintf "%a:@.Syntax error: %s@." Location.print_loc loc msg
      | Lexing_error (error, loc) ->
        Format.eprintf "%a:@.Lex error: %a@." Location.print_loc loc
          Flambda_lex.pp_error error
    end;
    exit 1

let _ =
  let file = Sys.argv.(1) in
  let ext = Filename.extension file in
  match ext with
  | ".fl" -> parse_flambda file
  | _ -> Misc.fatal_errorf "Unrecognized extension %s" ext
