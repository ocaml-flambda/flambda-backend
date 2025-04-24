open Import

let save unit filename =
  let out = open_out filename in
  Misc.try_finally
    ~always:(fun () -> close_out out)
    ~exceptionally:(fun () -> Misc.remove_file filename)
    (fun () ->
      let ppf = out |> Format.formatter_of_out_channel in
      let fexpr = unit |> Flambda_to_fexpr.conv in
      Print_fexpr.flambda_unit ppf fexpr;
      Format.pp_print_flush ppf ())

let dump_text filename =
  In_channel.with_open_text filename (fun in_ ->
      let rec loop () =
        match In_channel.input_line in_ with
        | Some line ->
          Printf.eprintf "%s\n" line;
          loop ()
        | None -> ()
      in
      loop ())

let () =
  try
    let file = Sys.argv.(1) in
    let modname =
      Parse_flambda.make_compilation_unit ~filename:file ~extension:".fl" ()
    in
    let unit_info = Unit_info.make_dummy ~input_name:file modname in
    (* Need to get this right or the conversion will complain about binding
       non-local symbols *)
    Env.set_unit_name (Some unit_info);
    let unit =
      match Parse_flambda.parse file with
      | Ok unit -> unit
      | Error e ->
        Test_utils.dump_error e;
        dump_text file;
        raise Test_utils.Failure
    in
    let temp_file =
      Filename.temp_file ("roundtrip_" ^ Filename.remove_extension file) ".fl"
    in
    save unit temp_file;
    let unit' =
      Misc.try_finally
        ~always:(fun () -> Sys.remove temp_file)
        (fun () ->
          match Parse_flambda.parse temp_file with
          | Ok unit' -> unit'
          | Error e ->
            Test_utils.dump_error e;
            dump_text temp_file;
            Out_channel.flush stderr;
            raise Test_utils.Failure)
    in
    match Compare.flambda_units unit unit' with
    | Equivalent -> Printf.eprintf "Roundtrip test: %s: PASS\n" file
    | Different { approximant } ->
      let corrected_file = file ^ ".corrected" in
      Printf.eprintf "Roundtrip test: %s: FAIL\nSaving corrected file as %s\n"
        file corrected_file;
      (* For Dune, return 0 on failure with corrections *)
      save approximant corrected_file
  with Test_utils.Failure -> exit 1
