let print = Flambda2_core.print

(** Parsing **)
let parse_flambda file : Flambda_unit.t =
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

let run_validator filename : Outcome.t =
  let comp_unit =
    Parse_flambda.make_compilation_unit ~extension ~filename:file () in
  Compilation_unit.set_current (Some comp_unit);
  let fl_output :Flambda_unit.t = parse_flambda (cwd ^ test_dir ^ file) in
  let cmx_loader = Flambda_cmx.create_loader ~get_module_info in

  (* IY: What is [round]? *)
  let {Simplify.unit = simplify_result ; _ } =
    Simplify.run ~cmx_loader ~round:0 fl_output in

  let src_core = flambda_unit_to_core fl_output in
  let tgt_core = flambda_unit_to_core simplify_result in

  let src_core = src_core |> normalize in
  let tgt_core = tgt_core |> normalize in

  try (if Equiv.core_eq src_core tgt_core then Success else Failure) with
  | _ -> Error

let _ =
  let file = Sys.argv.(1) in
  let ext = Filename.extension file in
  let outcome =
    match ext with
    | ".fl" -> run_validator file
    | _ -> Misc.fatal_errorf "Unrecognized extension %s; expected .fl" ext
  in
  exit (outcome |> Outcome.to_exit_code)
