open Import

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
    let fl2 = Fexpr_to_flambda.conv comp_unit unit in
    Format.printf "flambda:@.%a@.@." Flambda_unit.print fl2;
    check_invariants fl2;
    let cmx_loader = Flambda_cmx.create_loader ~get_global_info in
    let { Simplify.unit = fl2'; _ } = Simplify.run ~cmx_loader ~round:1 fl2 in
    Format.printf "simplify:@.%a@." Flambda_unit.print fl2';
    let fl3 = Flambda_to_fexpr.conv fl2' in
    Format.printf "back to fexpr:@.%a@." Print_fexpr.flambda_unit fl3;
    fl3
  | Error e ->
    Test_utils.dump_error e;
    exit 1

let _ =
  let file = Sys.argv.(1) in
  let ext = Filename.extension file in
  match ext with
  | ".fl" -> parse_flambda file
  | _ -> Misc.fatal_errorf "Unrecognized extension %s" ext
