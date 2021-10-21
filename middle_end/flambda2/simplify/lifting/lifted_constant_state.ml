(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module DE = Downwards_env
module LC = Lifted_constant
module T = Flambda2_types
module TE = T.Typing_env

type t =
  | Empty
  | Leaf of LC.t
  | Leaf_array of { innermost_first : LC.t array }
  | Union of
      { outer : t;
        inner : t
      }

let to_list_outermost_first t =
  let rec to_list t acc =
    match t with
    | Empty -> acc
    | Leaf const -> const :: acc
    | Leaf_array { innermost_first } ->
      List.rev (Array.to_list innermost_first) @ acc
    | Union { inner; outer } -> to_list outer (to_list inner acc)
  in
  to_list t []

let [@ocamlformat "disable"] print ppf t =
  Format.fprintf ppf "@[<hov 1>(outermost_first@ %a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space LC.print)
    (to_list_outermost_first t)

let empty = Empty

let is_empty t =
  match t with Empty -> true | Leaf _ | Leaf_array _ | Union _ -> false

let singleton const = Leaf const

let singleton_sorted_array_of_constants ~innermost_first =
  if Array.length innermost_first < 1
  then empty
  else Leaf_array { innermost_first }

let union_ordered ~innermost ~outermost =
  match innermost, outermost with
  | Empty, _ -> outermost
  | _, Empty -> innermost
  | inner, outer -> Union { inner; outer }

let union t1 t2 = union_ordered ~innermost:t1 ~outermost:t2

let add_innermost t const =
  if is_empty t then Leaf const else Union { inner = Leaf const; outer = t }

let add_outermost t const =
  if is_empty t then Leaf const else Union { outer = Leaf const; inner = t }

let add = add_innermost

let rec fold_outermost_first t ~init ~f =
  match t with
  | Empty -> init
  | Leaf const -> f init const
  | Leaf_array { innermost_first } ->
    (* Avoid [Array.fold_right] as it would require a closure allocation. *)
    let acc = ref init in
    for i = Array.length innermost_first - 1 downto 0 do
      acc := f !acc innermost_first.(i)
    done;
    !acc
  | Union { inner; outer } ->
    let init = fold_outermost_first outer ~init ~f in
    fold_outermost_first inner ~init ~f

let rec fold_innermost_first t ~init ~f =
  match t with
  | Empty -> init
  | Leaf const -> f init const
  | Leaf_array { innermost_first } ->
    ArrayLabels.fold_left innermost_first ~init ~f
  | Union { inner; outer } ->
    let init = fold_innermost_first inner ~init ~f in
    fold_innermost_first outer ~init ~f

let fold = fold_innermost_first

let all_defined_symbols t =
  fold t ~init:Symbol.Set.empty ~f:(fun symbols const ->
      LC.all_defined_symbols const |> Symbol.Set.union symbols)

let add_to_denv ?maybe_already_defined denv lifted =
  let maybe_already_defined =
    match maybe_already_defined with None -> false | Some () -> true
  in
  let denv =
    fold lifted ~init:denv ~f:(fun denv lifted_constant ->
        let types_of_symbols = LC.types_of_symbols lifted_constant in
        Symbol.Map.fold
          (fun sym (_denv, typ) denv ->
            if maybe_already_defined && DE.mem_symbol denv sym
            then denv
            else DE.define_symbol denv sym (T.kind typ))
          types_of_symbols denv)
  in
  let typing_env =
    let typing_env = DE.typing_env denv in
    fold lifted ~init:typing_env ~f:(fun typing_env lifted_constant ->
        let types_of_symbols = LC.types_of_symbols lifted_constant in
        Symbol.Map.fold
          (fun sym (denv_at_definition, typ) typing_env ->
            if maybe_already_defined && DE.mem_symbol denv sym
            then typing_env
            else
              let sym = Name.symbol sym in
              let env_extension =
                (* CR-someday mshinwell: Sometimes we might already have the
                   types "made suitable" in the [closure_env] field of the
                   typing environment, perhaps? For example when lifted
                   constants' types are coming out of a closure into the
                   enclosing scope. *)
                T.make_suitable_for_environment
                  (DE.typing_env denv_at_definition)
                  typ ~suitable_for:typing_env ~bind_to:sym
              in
              TE.add_env_extension_with_extra_variables typing_env env_extension)
          types_of_symbols typing_env)
  in
  fold lifted ~init:(DE.with_typing_env denv typing_env)
    ~f:(fun denv lifted_constant ->
      let pieces_of_code =
        LC.defining_exprs lifted_constant
        |> Rebuilt_static_const.Group.pieces_of_code_including_those_not_rebuilt
      in
      Code_id.Map.fold
        (fun code_id code denv ->
          if maybe_already_defined && DE.mem_code denv code_id
          then denv
          else DE.define_code denv ~code_id ~code)
        pieces_of_code denv)

let add_singleton_to_denv t const = add_to_denv t (singleton const)

let add_list_to_denv t consts =
  ListLabels.fold_left consts ~init:t ~f:add_singleton_to_denv

module CIS = Code_id_or_symbol
module SCC_lifted_constants = Strongly_connected_components_flambda2.Make (CIS)

let build_dep_graph t =
  fold t ~init:(CIS.Map.empty, CIS.Map.empty)
    ~f:(fun (dep_graph, code_id_or_symbol_to_const) lifted_constant ->
      ListLabels.fold_left (LC.definitions lifted_constant)
        ~init:(dep_graph, code_id_or_symbol_to_const)
        ~f:(fun (dep_graph, code_id_or_symbol_to_const) definition ->
          let module D = LC.Definition in
          let free_names =
            let free_names = D.free_names definition in
            match D.descr definition with
            | Code _ | Block_like _ -> free_names
            | Set_of_closures { closure_symbols_with_types; _ } ->
              (* To avoid existing sets of closures (with or without associated
                 code) being pulled apart, we add a dependency from each of the
                 closure symbols (in the current set) to all of the others (in
                 the current set). *)
              ListLabels.fold_left
                (Closure_id.Lmap.data closure_symbols_with_types)
                ~init:free_names ~f:(fun free_names (symbol, _) ->
                  Name_occurrences.add_symbol free_names symbol Name_mode.normal)
          in
          let free_syms = Name_occurrences.symbols free_names in
          let free_code_ids =
            free_names
            |> Name_occurrences.code_ids_and_newer_version_of_code_ids
          in
          let deps =
            CIS.Set.union
              (CIS.set_of_symbol_set free_syms)
              (CIS.set_of_code_id_set free_code_ids)
          in
          let being_defined =
            D.bound_symbols definition |> Bound_symbols.everything_being_defined
          in
          CIS.Set.fold
            (fun being_defined (dep_graph, code_id_or_symbol_to_const) ->
              let dep_graph = CIS.Map.add being_defined deps dep_graph in
              let code_id_or_symbol_to_const =
                CIS.Map.add being_defined
                  (Lifted_constant.create_definition definition)
                  code_id_or_symbol_to_const
              in
              dep_graph, code_id_or_symbol_to_const)
            being_defined
            (dep_graph, code_id_or_symbol_to_const)))

let sort0 t =
  (* The various lifted constants may exhibit recursion between themselves
     (specifically between closures and/or code). We use SCC to obtain a
     topological sort of groups that must be coalesced into single
     code-and-set-of-closures definitions. *)
  let lifted_constants_dep_graph, code_id_or_symbol_to_const =
    build_dep_graph t
  in
  let innermost_first =
    lifted_constants_dep_graph
    |> SCC_lifted_constants.connected_components_sorted_from_roots_to_leaf
    |> ArrayLabels.map ~f:(fun (group : SCC_lifted_constants.component) ->
           let code_id_or_symbols =
             match group with
             | No_loop code_id_or_symbol -> [code_id_or_symbol]
             | Has_loop code_id_or_symbols -> code_id_or_symbols
           in
           let _, lifted_constants =
             ListLabels.fold_left code_id_or_symbols ~init:(CIS.Set.empty, [])
               ~f:(fun ((already_seen, definitions) as acc) code_id_or_symbol ->
                 if CIS.Set.mem code_id_or_symbol already_seen
                 then acc
                 else
                   let lifted_constant =
                     CIS.Map.find code_id_or_symbol code_id_or_symbol_to_const
                   in
                   let already_seen =
                     (* We may encounter the same defining expression more than
                        once, in the case of sets of closures, which may bind
                        more than one symbol. We must avoid duplicates in the
                        resulting [LC.t]. *)
                     let bound_symbols = LC.bound_symbols lifted_constant in
                     CIS.Set.union
                       (Bound_symbols.everything_being_defined bound_symbols)
                       already_seen
                   in
                   already_seen, lifted_constant :: definitions)
           in
           LC.concat lifted_constants)
  in
  (* We may wish to traverse the array of constants in either direction.
   * This can be done by virtue of the following property:
   *   Let the list/array L be a topological sort of a directed graph G.
   *   Then the reverse of L is a topological sort of the transpose of G.
   *)
  singleton_sorted_array_of_constants ~innermost_first

let sort t = if is_empty t then empty else sort0 t
