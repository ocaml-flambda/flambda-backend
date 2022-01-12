(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2022 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module T = Flambda2_types

module Bound = struct
  type t =
    { params : Bound_parameter.List.t;
      results : Bound_parameter.List.t;
      other_vars : Variable.t list
    }

  let free_names { params; results; other_vars } =
    let free_names =
      Name_occurrences.union
        (Bound_parameter.List.free_names params)
        (Bound_parameter.List.free_names results)
    in
    List.fold_left
      (fun free_names var ->
        Name_occurrences.add_variable free_names var Name_mode.in_types)
      free_names other_vars

  let apply_renaming { params; results; other_vars } renaming =
    let params = Bound_parameter.List.apply_renaming params renaming in
    let results = Bound_parameter.List.apply_renaming results renaming in
    let other_vars =
      List.map (fun var -> Renaming.apply_variable renaming var) other_vars
    in
    { params; results; other_vars }

  let all_ids_for_export { params; results; other_vars } =
    let ids =
      Ids_for_export.union
        (Bound_parameter.List.all_ids_for_export params)
        (Bound_parameter.List.all_ids_for_export results)
    in
    List.fold_left
      (fun ids var -> Ids_for_export.add_variable ids var)
      ids other_vars

  let rename { params; results; other_vars } =
    let params = Bound_parameter.List.rename params in
    let results = Bound_parameter.List.rename results in
    let other_vars = List.map (fun var -> Variable.rename var) other_vars in
    { params; results; other_vars }

  let print ppf { params; results; other_vars } =
    Format.fprintf ppf
      "@[<hov 1>(@[<hov 1>(params@ (%a))@]@ @[<hov 1>(results@ (%a))@]@ @[<hov \
       1>(other_vars@ (%a))@])@]"
      Bound_parameter.List.print params Bound_parameter.List.print results
      (Format.pp_print_list ~pp_sep:Format.pp_print_space Variable.print)
      other_vars

  let name_permutation { params; results; other_vars }
      ~guaranteed_fresh:
        { params = fresh_params;
          results = fresh_results;
          other_vars = fresh_other_vars
        } =
    if List.compare_lengths params fresh_params <> 0
       || List.compare_lengths results fresh_results <> 0
       || List.compare_lengths other_vars fresh_other_vars <> 0
    then Misc.fatal_error "Mismatching params/results/other-vars list lengths";
    let renaming =
      List.fold_left2
        (fun renaming param fresh_param ->
          Renaming.add_fresh_variable renaming
            (Bound_parameter.var param)
            ~guaranteed_fresh:(Bound_parameter.var fresh_param))
        Renaming.empty params fresh_params
    in
    let renaming =
      List.fold_left2
        (fun renaming result fresh_result ->
          Renaming.add_fresh_variable renaming
            (Bound_parameter.var result)
            ~guaranteed_fresh:(Bound_parameter.var fresh_result))
        renaming results fresh_results
    in
    List.fold_left2
      (fun renaming other_var fresh_other_var ->
        Renaming.add_fresh_variable renaming other_var
          ~guaranteed_fresh:fresh_other_var)
      renaming other_vars fresh_other_vars
end

module Result_env_extensions = struct
  type t = T.Typing_env_extension.With_extra_variables.t list

  let apply_renaming t renaming =
    List.map
      (fun env_extension ->
        T.Typing_env_extension.With_extra_variables.apply_renaming env_extension
          renaming)
      t

  let free_names t =
    List.map T.Typing_env_extension.With_extra_variables.free_names t
    |> Name_occurrences.union_list

  let all_ids_for_export t =
    List.map T.Typing_env_extension.With_extra_variables.all_ids_for_export t
    |> Ids_for_export.union_list
end

module A = Name_abstraction.Make (Bound) (Result_env_extensions)
module FN = Name_abstraction.Make_free_names (Bound) (Result_env_extensions)

type t = A.t

let print ppf t =
  let { Bound.params; results; other_vars }, env_extensions =
    Name_abstraction.peek_for_printing t
  in
  let results = List.combine results env_extensions in
  Format.fprintf ppf
    "@[<hov 1>(@[<hov 1>(params@ (%a))@]@ @[<hov 1>(other_vars@ (%a))@]@ \
     @[<hov 1>(results@ (%a))@])@]"
    Bound_parameter.List.print params
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Variable.print)
    other_vars
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
       (fun ppf (result, env_extension) ->
         Format.fprintf ppf
           "@[<hov 1>(@[<hov 1>(result@ %a)@]@ @[<hov 1>(env_extension@ \
            %a)@])@]"
           Bound_parameter.print result
           T.Typing_env_extension.With_extra_variables.print env_extension))
    results

let create ~params ~results =
  let results, env_extensions = List.split results in
  let other_vars =
    List.fold_left
      (fun other_vars env_extension ->
        (T.Typing_env_extension.With_extra_variables.existential_vars
           env_extension
        |> Variable.Set.elements)
        @ other_vars)
      [] env_extensions
  in
  let bound = { Bound.params; results; other_vars } in
  A.create bound env_extensions

let create_trivial ~params ~result_arity create_type =
  let results =
    List.mapi
      (fun i kind_with_subkind ->
        let bound_parameter =
          Bound_parameter.create
            (Variable.create ("result" ^ string_of_int i))
            kind_with_subkind
        in
        let env_extension =
          T.Typing_env_extension.With_extra_variables.add_or_replace_equation
            T.Typing_env_extension.With_extra_variables.empty
            (Bound_parameter.name bound_parameter)
            (create_type kind_with_subkind)
        in
        bound_parameter, env_extension)
      result_arity
  in
  create ~params ~results

let create_unknown ~params ~result_arity =
  create_trivial ~params ~result_arity T.unknown_with_subkind

let create_bottom ~params ~result_arity =
  create_trivial ~params ~result_arity (fun kind_with_subkind ->
      T.bottom (Flambda_kind.With_subkind.kind kind_with_subkind))

let pattern_match t ~f =
  A.pattern_match t
    ~f:(fun { Bound.params; results; other_vars = _ } env_extensions ->
      let results = List.combine results env_extensions in
      f ~params ~results)

let free_names = FN.free_names

let apply_renaming = A.apply_renaming

let all_ids_for_export = A.all_ids_for_export

let map_result_types t ~f =
  A.pattern_match t
    ~f:(fun { Bound.params; results; other_vars = _ } env_extensions ->
      let env_extensions =
        List.map
          (T.Typing_env_extension.With_extra_variables.map_types ~f)
          env_extensions
      in
      create ~params ~results:(List.combine results env_extensions))
