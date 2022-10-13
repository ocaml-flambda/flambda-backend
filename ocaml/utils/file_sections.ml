(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2022 OCamlPro SAS                                          *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type section =
  | Loaded of Obj.t
  | Pending of { byte_offset_in_file : int }

type t =
  { channel : in_channel;
    sections : section array
  }

let section_cache : (Compilation_unit.t, t) Hashtbl.t = Hashtbl.create 10

let add_unit unit section_toc channel ~first_section_offset =
  (* Format.eprintf "Adding unit %a with %i sections.@." Compilation_unit.print unit (Array.length section_toc); *)
  let sections =
    Array.map
      (fun offset ->
        Pending { byte_offset_in_file = offset + first_section_offset })
      section_toc
  in
  if Hashtbl.mem section_cache unit
  then Misc.fatal_errorf "Unit loaded multiple time %a" Compilation_unit.print unit;
  Hashtbl.add section_cache unit { channel; sections }

let read_section_from_file ~unit ~index =
  (* Format.eprintf "Reading section %i from unit %a@." index Compilation_unit.print unit; *)
  match Hashtbl.find_opt section_cache unit with
  | None ->
    Misc.fatal_errorf "Read section %i from an unopened unit %a" index Compilation_unit.print unit
  | Some { sections; channel } -> (
    let num_sections = Array.length sections in
    if index < 0 || index >= num_sections
    then
      Misc.fatal_errorf
        ".cmx file for unit %a only has %d sections, but the section at index \
         %d was requested"
        Compilation_unit.print unit num_sections index
    else
      match sections.(index) with
      | Loaded section_contents -> section_contents
      | Pending { byte_offset_in_file } ->
        (* Printf.eprintf "--> seeking to %d\n%!" byte_offset_in_cmx; *)
        seek_in channel byte_offset_in_file;
        let section_contents : Obj.t = input_value channel in
        sections.(index) <- Loaded section_contents;
        section_contents)

let close_all () =
  Hashtbl.iter
    (fun _ sections ->
      try close_in sections.channel
      with Sys_error _ ->
        (* In cmxa files, the channel is shared, we might close it multiple
           times *)
        ())
    section_cache;
  Hashtbl.reset section_cache

let compute_toc serialized_sections =
  let toc = Array.make (Array.length serialized_sections) 0 in
  let length = ref 0 in
  for i = 0 to Array.length serialized_sections - 1 do
    toc.(i) <- !length;
    length := !length + String.length (serialized_sections.(i))
  done;
  toc, !length

let serialize sections =
  let serialized_sections = Array.map (fun section ->
      Marshal.to_string section []
    ) sections in
  let toc, total_length = compute_toc serialized_sections in
  serialized_sections, toc, total_length
