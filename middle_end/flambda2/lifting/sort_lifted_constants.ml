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

open! Simplify_import
module SCC_lifted_constants = Strongly_connected_components_flambda2.Make (CIS)

let build_dep_graph lifted_constants =
  (* Format.eprintf "SORTING:\n%!"; *)
  LCS.fold lifted_constants ~init:(CIS.Map.empty, CIS.Map.empty)
    ~f:(fun (dep_graph, code_id_or_symbol_to_const) lifted_constant ->
      (* Format.eprintf "One constant: %a\n%!" LC.print lifted_constant; *)
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
                  Name_occurrences.add_symbol free_names symbol NM.normal)
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
                CIS.Map.add being_defined lifted_constant
                  code_id_or_symbol_to_const
              in
              dep_graph, code_id_or_symbol_to_const)
            being_defined
            (dep_graph, code_id_or_symbol_to_const)))

let sort0 lifted_constants =
  (* The various lifted constants may exhibit recursion between themselves
     (specifically between closures and/or code). We use SCC to obtain a
     topological sort of groups that must be coalesced into single
     code-and-set-of-closures definitions. *)
  let lifted_constants_dep_graph, code_id_or_symbol_to_const =
    build_dep_graph lifted_constants
  in
  (* Format.eprintf "SCC graph is:@ %a\n%!" (CIS.Map.print CIS.Set.print)
     lifted_constants_dep_graph; *)
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
  LCS.singleton_sorted_array_of_constants ~innermost_first

let sort lifted_constants =
  if LCS.is_empty lifted_constants then LCS.empty else sort0 lifted_constants
