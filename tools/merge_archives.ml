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

let merge_cma ~target:_ ~archives:_ =
  failwith "not yet implemented"

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
    | _::_ -> failwith "Archives do not agree on the .cmxa magic number"
    | [] -> assert false
  in
  let _, lib_units, lib_ccobjs, lib_ccopts =
    List.fold_left
      (fun (lib_names, lib_units, lib_ccobjs, lib_ccopts)
           (cmxa : Cmx_format.library_infos) ->
        let new_lib_names =
          List.map (fun ((cmx : Cmx_format.unit_infos), _) -> cmx.ui_name)
            cmxa.lib_units
          |>
          String_set.of_list
        in
        let already_defined = String_set.inter new_lib_names lib_names in
        if not (String_set.is_empty already_defined) then begin
          failwith "Archives contain multiply-defined units"
        end;
        let lib_names = String_set.union new_lib_names lib_names in
        let lib_units = lib_units @ cmxa.lib_units in
        let cmxa_lib_ccobjs = String_set.of_list cmxa.lib_ccobjs in
        let lib_ccobjs = String_set.union cmxa_lib_ccobjs lib_ccobjs in
        let lib_ccopts = lib_ccopts @ cmxa.lib_ccopts in
        lib_names, lib_units, lib_ccobjs, lib_ccopts)
      (String_set.empty, [], String_set.empty, [])
      cmxa_list
  in
  let cmxa : Cmx_format.library_infos =
    { lib_units;
      lib_ccobjs = String_set.elements lib_ccobjs;
      lib_ccopts;
    }
  in
  magic, cmxa

let merge_cmxa ~target ~archives =
  let magic, cmxa = merge_cmxa0 ~archives in
  let chan = open_out_bin target in
  output_string chan magic;
  output_value chan cmxa;
  close_out chan

let has_extension archive ~ext =
  Filename.check_suffix archive ("." ^ ext)

let syntax () =
  Printf.eprintf "syntax: %s TARGET-CMX-OR-CMXA-FILE CMA-OR-CMXA-FILES\n"
    Sys.argv.(0);
  Printf.eprintf "Please provide only .cma files or only .cmxa files.";
  exit 1

let () =
  if Array.length Sys.argv < 3 then syntax ();
  let target = Sys.argv.(1) in
  let archives =
    Array.sub Sys.argv 2 (Array.length Sys.argv - 2)
    |> Array.to_list
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
