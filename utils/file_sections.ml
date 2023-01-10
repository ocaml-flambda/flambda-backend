(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Pierre Chambart, Nathanaëlle Courant, OCamlPro             *)
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

module File_lru_cache = Lru.Make (struct
  type cached = in_channel

  type uncached = string

  let load = open_in_bin

  let unload _ ic = close_in ic
end)

let file_lru = File_lru_cache.create ~capacity:128

let () = at_exit (fun () -> File_lru_cache.unload_all file_lru)

type t =
  | From_file of
      { channel : File_lru_cache.slot;
        sections : section array
      }
  | In_memory of Obj.t array
  | Cat of int * t * t
(* For efficient concatenation *)

let create section_toc file channel ~first_section_offset =
  if Array.length section_toc = 0
  then (
    close_in channel;
    In_memory [||])
  else
    let channel = File_lru_cache.add_slot file channel file_lru in
    let sections =
      Array.map
        (fun offset ->
          Pending { byte_offset_in_file = offset + first_section_offset })
        section_toc
    in
    From_file { channel; sections }

let empty = In_memory [||]

let length = function
  | From_file { sections; _ } -> Array.length sections
  | In_memory sections -> Array.length sections
  | Cat (length, _, _) -> length

let read_section sections channel index =
  match sections.(index) with
  | Loaded section_contents -> section_contents
  | Pending { byte_offset_in_file } ->
    let channel = File_lru_cache.load_slot channel file_lru in
    seek_in channel byte_offset_in_file;
    let section_contents : Obj.t = input_value channel in
    sections.(index) <- Loaded section_contents;
    section_contents

let rec unsafe_get t index =
  match t with
  | From_file { sections; channel } -> read_section sections channel index
  | In_memory sections -> sections.(index)
  | Cat (_, t1, t2) ->
    let n = length t1 in
    if index < n then unsafe_get t1 index else unsafe_get t2 (index - n)

let get t index =
  let len = length t in
  if index < 0 || index >= len
  then
    Misc.fatal_errorf
      "File_sections.get index out of bounds: index is %d, but length is %d"
      index len;
  unsafe_get t index

let rec unsafe_blit_to_array t dest start_index =
  match t with
  | From_file { sections; channel } ->
    for i = 0 to Array.length sections - 1 do
      dest.(start_index + i) <- read_section sections channel i
    done
  | In_memory sections ->
    Array.blit sections 0 dest start_index (Array.length sections)
  | Cat (_, t1, t2) ->
    unsafe_blit_to_array t1 dest start_index;
    unsafe_blit_to_array t2 dest (start_index + length t1)

let to_array t =
  let dest = Array.make (length t) (Obj.repr 0) in
  unsafe_blit_to_array t dest 0;
  dest

let from_array t = In_memory (Array.copy t)

let concat t1 t2 = Cat (length t1 + length t2, t1, t2)

let compute_toc serialized_sections =
  let toc = Array.make (Array.length serialized_sections) 0 in
  let length = ref 0 in
  for i = 0 to Array.length serialized_sections - 1 do
    toc.(i) <- !length;
    length := !length + String.length serialized_sections.(i)
  done;
  toc, !length

let serialize t =
  let sections = to_array t in
  let serialized_sections =
    Array.map (fun section -> Marshal.to_string section []) sections
  in
  let toc, total_length = compute_toc serialized_sections in
  serialized_sections, toc, total_length
