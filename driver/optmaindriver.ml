(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Clflags

let usage = "Usage: ocamlopt <options> <files>\nOptions are:"

module Options = Flambda_backend_args.Make_optcomp_options
        (Flambda_backend_args.Default.Optmain)

let main unix argv ppf ~flambda2 =
  native_code := true;
  let columns =
    match Sys.getenv "COLUMNS" with
    | exception Not_found -> None
    | columns ->
      try Some (int_of_string columns)
      with _ -> None
  in
  (match columns with
  | None -> ()
  | Some columns ->
    (* Avoid getting too close to the edge just in case we've mismeasured
       the boxes for some reason. *)
    let columns = columns - 5 in
    let set_geometry ppf =
      Format.pp_set_margin ppf columns;
      (* Make sure the max indent is at least 3/4 of the total width. Without
         this, output can be unreadable no matter how wide your screen is. Note
         that [Format.pp_set_margin] already messes with the max indent
         sometimes, so we want to check [Format.pp_get_max_indent] rather than
         make assumptions. *)
      let desired_max_indent = columns * 3 / 4 in
      if Format.pp_get_max_indent ppf () < desired_max_indent then
        Format.pp_set_max_indent ppf desired_max_indent
    in
    set_geometry Format.std_formatter;
    set_geometry Format.err_formatter);
  match
    Compenv.warnings_for_discarded_params := true;
    Compenv.set_extra_params
      (Some Flambda_backend_args.Extra_params.read_param);
    Compenv.readenv ppf Before_args;
    Clflags.add_arguments __LOC__ (Arch.command_line_options @ Options.list);
    Clflags.add_arguments __LOC__
      ["-depend", Arg.Unit Makedepend.main_from_option,
       "<options> Compute dependencies \
        (use 'ocamlopt -depend -help' for details)"];
    Clflags.Opt_flag_handler.set Flambda_backend_flags.opt_flag_handler;
    Compenv.parse_arguments (ref argv) Compenv.anonymous "ocamlopt";
    Compmisc.read_clflags_from_env ();
    if !Flambda_backend_flags.gc_timings then Gc_timings.start_collection ();
    if !Clflags.plugin then
      Compenv.fatal "-plugin is only supported up to OCaml 4.08.0";
    begin try
      Compenv.process_deferred_actions
        (ppf,
         Optcompile.implementation unix ~flambda2,
         Optcompile.interface,
         ".cmx",
         ".cmxa");
    with Arg.Bad msg ->
      begin
        prerr_endline msg;
        Clflags.print_arguments usage;
        exit 2
      end
    end;
    Compenv.readenv ppf Before_link;
    if
      List.length (List.filter (fun x -> !x)
                     [make_package; make_archive; shared;
                      Compenv.stop_early; output_c_object]) > 1
    then
    begin
      let module P = Clflags.Compiler_pass in
      match !stop_after with
      | None ->
          Compenv.fatal "Please specify at most one of -pack, -a, -shared, -c, \
                         -output-obj";
      | Some ((P.Parsing | P.Typing | P.Lambda | P.Middle_end | P.Linearization
              | P.Simplify_cfg | P.Emit | P.Selection) as p) ->
        assert (P.is_compilation_pass p);
        Printf.ksprintf Compenv.fatal
          "Options -i and -stop-after (%s) \
           are  incompatible with -pack, -a, -shared, -output-obj"
          (String.concat "|"
             (P.available_pass_names ~filter:(fun _ -> true) ~native:true))
    end;
    if !make_archive then begin
      Compmisc.init_path ();
      let target = Compenv.extract_output !output_name in
      Asmlibrarian.create_archive
        (Compenv.get_objfiles ~with_ocamlparam:false) target;
      Warnings.check_fatal ();
    end
    else if !make_package then begin
      Compmisc.init_path ();
      let target = Compenv.extract_output !output_name in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
        Asmpackager.package_files unix
          ~ppf_dump (Compmisc.initial_env ())
          (Compenv.get_objfiles ~with_ocamlparam:false) target
          ~flambda2);
      Warnings.check_fatal ();
    end
    else if !shared then begin
      Compmisc.init_path ();
      let target = Compenv.extract_output !output_name in
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
        Asmlink.link_shared unix ~ppf_dump
          (Compenv.get_objfiles ~with_ocamlparam:false) target);
      Warnings.check_fatal ();
    end
    else if not !Compenv.stop_early && !objfiles <> [] then begin
      let target =
        if !output_c_object then
          let s = Compenv.extract_output !output_name in
          if (Filename.check_suffix s Config.ext_obj
            || Filename.check_suffix s Config.ext_dll)
          then s
          else
            Compenv.fatal
              (Printf.sprintf
                 "The extension of the output file must be %s or %s"
                 Config.ext_obj Config.ext_dll
              )
        else
          Compenv.default_output !output_name
      in
      Compmisc.init_path ();
      Compmisc.with_ppf_dump ~file_prefix:target (fun ppf_dump ->
          let objs = Compenv.get_objfiles ~with_ocamlparam:true in
          Asmlink.link unix
            ~ppf_dump objs target);
      Warnings.check_fatal ();
    end;
  with
  | exception (Compenv.Exit_with_status n) ->
    n
  | exception x ->
    Location.report_exception ppf x;
    2
  | () ->
    let output_profile_csv ppf_file = Profile.output_to_csv
      ppf_file !Clflags.profile_columns ~timings_precision:!Clflags.timings_precision
    in
    let output_profile_standard ppf =
      if !Flambda_backend_flags.gc_timings then begin
        let minor = Gc_timings.gc_minor_ns () in
        let major = Gc_timings.gc_major_ns () in
        let stats = Gc.quick_stat () in
        let secs x = x *. 1e-9 in
        let precision = !Clflags.timings_precision in
        let w2b n = n * (Sys.word_size / 8) in
        let fw2b x = w2b (Float.to_int x) in
        Format.fprintf ppf "%0.*fs gc\n" precision (secs (minor +. major));
        Format.fprintf ppf "  %0.*fs minor\n" precision (secs minor);
        Format.fprintf ppf "  %0.*fs major\n" precision (secs major);
        Format.fprintf ppf "- heap\n";
        (* Having minor + major + promoted = total alloc make more sense for
          hierarchical stats. *)
        Format.fprintf ppf "  %ib alloc\n"
          (fw2b stats.minor_words + (fw2b stats.major_words - fw2b stats.promoted_words));
        Format.fprintf ppf "    %ib minor\n"
          (fw2b stats.minor_words - fw2b stats.promoted_words);
        Format.fprintf ppf "    %ib major\n"
          (fw2b stats.major_words - fw2b stats.promoted_words);
        Format.fprintf ppf "    %ib promoted\n"
          (fw2b stats.promoted_words);
        Format.fprintf ppf "  %ib top\n" (w2b stats.top_heap_words);
        Format.fprintf ppf "  %i collections\n"
          (stats.minor_collections + stats.major_collections);
        Format.fprintf ppf "    %i minor\n" stats.minor_collections;
        Format.fprintf ppf "    %i major\n" stats.major_collections;
      end;
      Profile.print ppf !Clflags.profile_columns ~timings_precision:!Clflags.timings_precision
    in
    if !Clflags.dump_into_csv then
      Compmisc.with_ppf_file ~file_prefix:"profile" ~file_extension:".csv" output_profile_csv
    else if !Flambda_backend_flags.gc_timings || !Clflags.profile_columns <> [] then
      Compmisc.with_ppf_dump ~stdout:() ~file_prefix:"profile" output_profile_standard;
    0
