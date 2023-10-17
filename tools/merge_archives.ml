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
    (fun archive -> Load_path.add_dir (Filename.dirname archive))
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

let read_cmxa filename =
  let chan = open_in_bin filename in
  let magic =
    really_input_string chan (String.length Config.cmxa_magic_number)
  in
  let (cmxa : Cmx_format.library_infos) = input_value chan in
  close_in chan;
  magic, cmxa

let merge_cmxa0 ~archives =
  let magic_and_cmxa_list = List.map read_cmxa archives in
  let magics = List.map fst magic_and_cmxa_list in
  let cmxa_list = List.map snd magic_and_cmxa_list in
  let magic =
    match String_set.elements (String_set.of_list magics) with
    | [magic] -> magic
    | _ :: _ -> failwith "Archives do not agree on the .cmxa magic number"
    | [] -> assert false
  in
  let ncmxs = ref 0 and ncmis = ref 0 in
  let cmi_table = Hashtbl.create 42 in
  let cmx_table = Hashtbl.create 42 in
  cmxa_list
  |> List.iter (fun (lib : Cmx_format.library_infos) ->
         lib.lib_imports_cmi
         |> Array.iter (fun import ->
                let name = Import_info.name import in
                if not (Hashtbl.mem cmi_table name)
                then begin
                  Hashtbl.add cmi_table name (import, !ncmis);
                  incr ncmis
                end);
         lib.lib_imports_cmx
         |> Array.iter (fun import ->
                let cu = Import_info.cu import in
                if not (Hashtbl.mem cmx_table cu)
                then begin
                  Hashtbl.add cmx_table cu (import, !ncmxs);
                  incr ncmxs
                end));
  let cmis = Array.make !ncmis Import_info.dummy in
  Hashtbl.iter (fun name (import, i) -> cmis.(i) <- import) cmi_table;
  let cmxs = Array.make !ncmxs Import_info.dummy in
  Hashtbl.iter (fun name (import, i) -> cmxs.(i) <- import) cmx_table;
  let genfns = Generic_fns.Tbl.make () in
  let _, lib_units, lib_ccobjs, lib_ccopts =
    List.fold_left
      (fun (lib_names, lib_units, lib_ccobjs, lib_ccopts)
           (cmxa : Cmx_format.library_infos) ->
        let new_lib_names =
          List.map
            (fun (cmx : Cmx_format.lib_unit_info) -> cmx.li_name)
            cmxa.lib_units
          |> Compilation_unit.Set.of_list
        in
        let already_defined =
          Compilation_unit.Set.inter new_lib_names lib_names
        in
        if not (Compilation_unit.Set.is_empty already_defined)
        then failwith "Archives contain multiply-defined units";
        Generic_fns.Tbl.add genfns cmxa.lib_generic_fns;
        let lib_names = Compilation_unit.Set.union new_lib_names lib_names in
        let remap oldarr newarr tbl oldb ~get_key =
          let module B = Misc.Bitmap in
          let b = B.make (Array.length newarr) in
          oldb
          |> B.iter (fun i ->
                 B.set b (snd (Hashtbl.find tbl (get_key oldarr.(i)))));
          b
        in
        let new_units =
          List.map
            (fun (li : Cmx_format.lib_unit_info) ->
              { li with
                li_imports_cmi =
                  remap cmxa.lib_imports_cmi cmis cmi_table li.li_imports_cmi
                    ~get_key:Import_info.name;
                li_imports_cmx =
                  remap cmxa.lib_imports_cmx cmxs cmx_table li.li_imports_cmx
                    ~get_key:Import_info.cu
              })
            cmxa.lib_units
        in
        let lib_units = lib_units @ new_units in
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
  magic, cmxa

let merge_cmxa ~target ~archives =
  let magic, cmxa = merge_cmxa0 ~archives in
  let chan = open_out_bin target in
  output_string chan magic;
  output_value chan cmxa;
  close_out chan

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
