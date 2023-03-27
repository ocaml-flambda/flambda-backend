open Import

let _ =
  try
    let file1 = Sys.argv.(1) in
    let file2 = Sys.argv.(2) in
    let unit1 = Test_utils.parse_flambda file1 in
    let unit2 = Test_utils.parse_flambda file2 in
    let modname1 =
      Parse_flambda.make_compilation_unit ~filename:file1 ~extension:".fl" ()
    in
    Compilation_unit.set_current (Some modname1);
    Format.printf "%a@."
      (Compare.Comparison.print Flambda_unit.print)
      (Compare.flambda_units unit1 unit2)
  with Test_utils.Failure -> exit 1
