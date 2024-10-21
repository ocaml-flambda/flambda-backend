let () =
  try
    failwith "SUCCESS"
  with
  | e ->
    let c = Printexc.get_callstack 10 in
    let bt = Printexc.raw_backtrace_to_string c in
    let bt_list = String.split_on_char '\n' bt in
    if List.length bt_list > 5 then (
      print_endline "Backtrace sufficiently long (in plugin)";
      raise e
    )
    else (
      print_endline "Failure: Backtrace too short (in plugin):";
      print_string bt;
      raise e
    )
