(**************************************************************************
 *                                                                        *
 *                                 OCaml                                  *
 *                                                                        *
 *                    Mark Shinwell, Jane Street Europe                   *
 *                                                                        *
 *   Copyright 2021 Jane Street Group LLC                                 *
 *                                                                        *
 *   All rights reserved.  This file is distributed under the terms of    *
 *   the GNU Lesser General Public License version 2.1, with the          *
 *   special exception on linking described in the file LICENSE.          *
 *                                                                        *
 **************************************************************************)

module String_set = Set.Make (String)

let merge_cma ~target ~archives =
  (* This is rather tightly tied to the internals of [Bytelibrarian]. *)
  Clflags.link_everything := false;
  Clflags.custom_runtime := false;
  Clflags.no_auto_link := false;
  Clflags.ccobjs := [];
  Clflags.all_ccopts := [];
  Clflags.dllibs := [];
  List.iter
    (fun archive -> Load_path.add_dir ~hidden:false (Filename.dirname archive))
    archives;
  let error reporter err =
    Format.eprintf "Error whilst merging .cma files:@ %a\n%!" reporter err;
    exit 1
  in
  try
    Bytelibrarian.create_archive archives target;
    Warnings.check_fatal ()
  with
  | Bytelibrarian.Error err -> error Bytelibrarian.report_error err
  | Bytelink.Error err -> error Bytelink.report_error err
  | Warnings.Errors ->
    (* Warnings should already have been printed to stderr. *)
    exit 1

let read_magic filename =
  let chan = open_in_bin filename in
  let magic =
    really_input_string chan (String.length Config.cmxa_magic_number)
  in
  close_in chan;
  magic

let read_cmxa filename =
  Cmx_format.read_library_info ~filename

let merge_cmxa0 ~archives =
  let magics = List.map read_magic archives in
  let () =
    match String_set.elements (String_set.of_list magics) with
    | [magic] ->
        if not (String.equal magic Config.cmxa_magic_number) then
          failwith "Archives have the wrong .cmxa magic number"
    | _ :: _ -> failwith "Archives do not agree on the .cmxa magic number"
    | [] -> assert false
  in
  let cmxa_list = List.map read_cmxa archives in
  let cmi_table = Hashtbl.create 42 in
  let cmx_table = Hashtbl.create 42 in
  cmxa_list
  |> List.iter (fun (lib : Cmx_format.library_infos) ->
         lib.lib_imports_cmi
         |> List.iter (fun import ->
                let name = Import_info.name import in
                if not (Hashtbl.mem cmi_table name)
                then Hashtbl.add cmi_table name import);
         lib.lib_imports_cmx
         |> List.iter (fun import ->
                let cu = Import_info.cu import in
                if not (Hashtbl.mem cmx_table cu)
                then Hashtbl.add cmx_table cu import;));
  let cmis = Hashtbl.to_seq_values cmi_table |> List.of_seq in
  let cmxs = Hashtbl.to_seq_values cmx_table |> List.of_seq in
  let genfns = Generic_fns.Tbl.make () in
  let _, lib_units, lib_ccobjs, lib_ccopts =
    List.fold_left
      (fun (lib_names, lib_units, lib_ccobjs, lib_ccopts)
           (cmxa : Cmx_format.library_infos) ->
        let new_lib_names =
          List.map
            (fun ((cmx : Cmx_format.unit_infos), _crc) -> cmx.ui_unit)
            cmxa.lib_units
          |> Compilation_unit.Set.of_list
        in
        let already_defined =
          Compilation_unit.Set.inter new_lib_names lib_names
        in
        if not (Compilation_unit.Set.is_empty already_defined)
        then failwith "Archives contain multiply-defined units";
        ignore(Generic_fns.Tbl.add
                ~imports:Generic_fns.Partition.Set.empty
                genfns cmxa.lib_generic_fns);
        let lib_names = Compilation_unit.Set.union new_lib_names lib_names in
        let lib_units = lib_units @ cmxa.lib_units in
        let cmxa_lib_ccobjs = String_set.of_list cmxa.lib_ccobjs in
        let lib_ccobjs = String_set.union cmxa_lib_ccobjs lib_ccobjs in
        let lib_ccopts = lib_ccopts @ cmxa.lib_ccopts in
        lib_names, lib_units, lib_ccobjs, lib_ccopts)
      (Compilation_unit.Set.empty, [], String_set.empty, [])
      cmxa_list
  in
  let cmxa : Cmx_format.library_infos =
    { lib_units;
      lib_ccobjs = String_set.elements lib_ccobjs;
      lib_ccopts;
      lib_imports_cmi = cmis;
      lib_imports_cmx = cmxs;
      lib_generic_fns = Generic_fns.Tbl.entries genfns
    }
  in
  cmxa

let merge_cmxa ~target ~archives =
  let cmxa = merge_cmxa0 ~archives in
  Cmx_format.write_library_info ~filename:target cmxa

let has_extension archive ~ext = Filename.check_suffix archive ("." ^ ext)

let syntax () =
  Printf.eprintf
    "syntax: %s OCAMLOPT-BINARY TARGET-CMA-OR-CMXA-FILE CMA-OR-CMXA-FILES\n"
    Sys.argv.(0);
  Printf.eprintf "Please provide only .cma files or only .cmxa files.";
  exit 1

let () =
  if Array.length Sys.argv < 3 then syntax ();
  let target = Sys.argv.(1) in
  let archives =
    Array.sub Sys.argv 2 (Array.length Sys.argv - 2) |> Array.to_list
  in
  let all_cma =
    List.for_all (fun archive -> has_extension archive ~ext:"cma") archives
  in
  let all_cmxa =
    List.for_all (fun archive -> has_extension archive ~ext:"cmxa") archives
  in
  match all_cma, all_cmxa with
  | true, false -> merge_cma ~target ~archives
  | false, true -> merge_cmxa ~target ~archives
  | true, true | false, false -> syntax ()
