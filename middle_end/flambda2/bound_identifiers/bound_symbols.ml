(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module Pattern = struct
  type t =
    | Code of Code_id.t
    | Set_of_closures of Symbol.t Closure_id.Lmap.t
    | Block_like of Symbol.t

  let code code_id = Code code_id

  let set_of_closures closure_symbols = Set_of_closures closure_symbols

  let block_like symbol = Block_like symbol

  let [@ocamlformat "disable"] print ppf t =
    match t with
    | Code code_id ->
      Format.fprintf ppf "@[<hov 1>(Code@ %a)@]" Code_id.print code_id
    | Set_of_closures closure_symbols ->
      Format.fprintf ppf "@[<hov 1>(Set_of_closures@ %a)@]"
        (Closure_id.Lmap.print Symbol.print) closure_symbols
    | Block_like symbol ->
      Format.fprintf ppf "@[<hov 1>(Block_like@ %a)@]" Symbol.print symbol

  let apply_renaming t perm =
    match t with
    | Code code_id -> Code (Renaming.apply_code_id perm code_id)
    | Set_of_closures map ->
      Set_of_closures (Closure_id.Lmap.map (Renaming.apply_symbol perm) map)
    | Block_like symbol -> Block_like (Renaming.apply_symbol perm symbol)

  let free_names t =
    match t with
    | Code code_id ->
      Name_occurrences.singleton_code_id code_id Name_mode.normal
    | Set_of_closures closure_symbols ->
      Closure_id.Lmap.fold
        (fun _ symbol free_names ->
          Name_occurrences.add_symbol free_names symbol Name_mode.normal)
        closure_symbols Name_occurrences.empty
    | Block_like symbol ->
      Name_occurrences.singleton_symbol symbol Name_mode.normal

  let being_defined t =
    match t with
    | Code _ -> Symbol.Set.empty
    | Set_of_closures closure_symbols ->
      closure_symbols |> Closure_id.Lmap.data |> Symbol.Set.of_list
    | Block_like symbol -> Symbol.Set.singleton symbol

  let code_being_defined t =
    match t with
    | Code code_id -> Code_id.Set.singleton code_id
    | Set_of_closures _ | Block_like _ -> Code_id.Set.empty

  let binds_code t =
    match t with Code _ -> true | Set_of_closures _ | Block_like _ -> false

  let binds_symbols t =
    match t with Code _ -> false | Set_of_closures _ | Block_like _ -> true

  let closure_symbols_being_defined t =
    match t with
    | Code _ | Block_like _ -> Symbol.Set.empty
    | Set_of_closures closure_symbols ->
      closure_symbols |> Closure_id.Lmap.data |> Symbol.Set.of_list

  let everything_being_defined t =
    match t with
    | Code code_id ->
      Code_id_or_symbol.Set.singleton (Code_id_or_symbol.create_code_id code_id)
    | Set_of_closures closure_symbols ->
      closure_symbols |> Closure_id.Lmap.data |> Symbol.Set.of_list
      |> Code_id_or_symbol.set_of_symbol_set
    | Block_like symbol ->
      Code_id_or_symbol.Set.singleton (Code_id_or_symbol.create_symbol symbol)

  let everything_being_defined_as_list t =
    match t with
    | Code code_id -> [Code_id_or_symbol.create_code_id code_id]
    | Set_of_closures closure_symbols ->
      closure_symbols |> Closure_id.Lmap.data
      |> List.map Code_id_or_symbol.create_symbol
    | Block_like symbol -> [Code_id_or_symbol.create_symbol symbol]

  let for_all_everything_being_defined t ~f =
    match t with
    | Code code_id -> f (Code_id_or_symbol.create_code_id code_id)
    | Set_of_closures closure_symbols ->
      Closure_id.Lmap.for_all_with_fixed_arg
        (fun _closure_id symbol f -> f (Code_id_or_symbol.create_symbol symbol))
        closure_symbols f
    | Block_like symbol -> f (Code_id_or_symbol.create_symbol symbol)

  let all_ids_for_export t =
    match t with
    | Code code_id -> Ids_for_export.singleton_code_id code_id
    | Set_of_closures closure_symbols ->
      let symbols =
        closure_symbols |> Closure_id.Lmap.data |> Symbol.Set.of_list
      in
      Ids_for_export.create ~symbols ()
    | Block_like symbol -> Ids_for_export.singleton_symbol symbol

  let gc_roots t =
    match t with
    | Code _ -> []
    | Set_of_closures closure_symbols ->
      [List.hd (Closure_id.Lmap.data closure_symbols)]
    | Block_like s -> [s]
end

type t = Pattern.t list

let empty = []

let check_pattern_list_invariant pattern_list =
  (* Check that there are no repeated bindings of symbols or code IDs. *)
  let everything_being_defined =
    List.map Pattern.everything_being_defined_as_list pattern_list
    |> List.concat
  in
  let everything_being_defined_as_set =
    Code_id_or_symbol.Set.of_list everything_being_defined
  in
  if List.compare_length_with everything_being_defined
       (Code_id_or_symbol.Set.cardinal everything_being_defined_as_set)
     <> 0
  then
    Misc.fatal_errorf
      "Illegal pattern list (duplicate code IDs or symbols):@ %a"
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Pattern.print)
      pattern_list

let create pattern_list =
  if Flambda_features.check_invariants ()
  then check_pattern_list_invariant pattern_list;
  pattern_list

let singleton pattern = [pattern]

let to_list t = t

let [@ocamlformat "disable"] print ppf t =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Pattern.print) t

let being_defined t = List.map Pattern.being_defined t |> Symbol.Set.union_list

let closure_symbols_being_defined t =
  List.map Pattern.closure_symbols_being_defined t |> Symbol.Set.union_list

let non_closure_symbols_being_defined t =
  Symbol.Set.diff (being_defined t) (closure_symbols_being_defined t)

let code_being_defined t =
  List.fold_left
    (fun code_ids pattern ->
      Code_id.Set.union code_ids (Pattern.code_being_defined pattern))
    Code_id.Set.empty t

let binds_code t = List.exists Pattern.binds_code t

let binds_symbols t = List.exists Pattern.binds_symbols t

let everything_being_defined t =
  List.map Pattern.everything_being_defined t
  |> Code_id_or_symbol.Set.union_list

module List = struct
  include List

  let rec for_all_with_fixed_arg f t fixed_arg =
    match t with
    | [] -> true
    | x :: t -> f x fixed_arg && for_all_with_fixed_arg f t fixed_arg
end

let for_all_everything_being_defined t ~f =
  List.for_all_with_fixed_arg
    (fun pattern f -> Pattern.for_all_everything_being_defined pattern ~f)
    t f

let apply_renaming t perm =
  List.map (fun pattern -> Pattern.apply_renaming pattern perm) t

let free_names t = List.map Pattern.free_names t |> Name_occurrences.union_list

let all_ids_for_export t =
  List.map Pattern.all_ids_for_export t |> Ids_for_export.union_list

let concat t1 t2 = t1 @ t2

let gc_roots t = List.map Pattern.gc_roots t |> List.concat
