(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                  Mark Shinwell, Jane Street Europe                     *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Heap
  | Local

type without_region = t

let print ppf t =
  match t with
  | Heap -> Format.pp_print_string ppf "Heap"
  | Local -> Format.pp_print_string ppf "Local"

let compare t1 t2 =
  match t1, t2 with
  | Heap, Heap | Local, Local -> 0
  | Heap, Local -> -1
  | Local, Heap -> 1

let from_lambda (mode : Lambda.alloc_mode) =
  match mode with Alloc_heap -> Heap | Alloc_local -> Local

let to_lambda t =
  match t with Heap -> Lambda.alloc_heap | Local -> Lambda.alloc_local

module With_region = struct
  type t =
    | Heap
    | Local of { region : Variable.t }

  let print ppf t =
    match t with
    | Heap -> Format.pp_print_string ppf "Heap"
    | Local { region } ->
      Format.fprintf ppf "@[<hov 1>(Local (region@ %a))@]" Variable.print region

  let compare t1 t2 =
    match t1, t2 with
    | Heap, Heap -> 0
    | Local { region = region1 }, Local { region = region2 } ->
      Variable.compare region1 region2
    | Heap, Local _ -> -1
    | Local _, Heap -> 1

  let without_region t : without_region =
    match t with Heap -> Heap | Local _ -> Local

  let from_lambda (mode : Lambda.alloc_mode) ~current_region =
    match mode with
    | Alloc_heap -> Heap
    | Alloc_local -> Local { region = current_region }

  let to_lambda t =
    match t with Heap -> Lambda.alloc_heap | Local _ -> Lambda.alloc_local

  let free_names t =
    match t with
    | Heap -> Name_occurrences.empty
    | Local { region } ->
      Name_occurrences.singleton_variable region Name_mode.normal

  let apply_renaming t renaming =
    match t with
    | Heap -> Heap
    | Local { region } ->
      let region' = Renaming.apply_variable renaming region in
      if region == region' then t else Local { region = region' }

  let ids_for_export t =
    match t with
    | Heap -> Ids_for_export.empty
    | Local { region } -> Ids_for_export.singleton_variable region
end
