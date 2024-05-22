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

module BP = Bound_parameter

type t = BP.t list

let print ppf t =
  Format.fprintf ppf "@[<hov 0>%a@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space BP.print)
    t

let empty = []

let create params =
  (if Flambda_features.check_invariants ()
  then
    let params_set = BP.Set.of_list params in
    if List.length params <> BP.Set.cardinal params_set
    then
      Misc.fatal_errorf
        "Names provided to [Bound_parameters.create] must be disjoint:@ %a"
        print params);
  params

let cons param t = create (param :: t)

let append t1 t2 = create (t1 @ t2)

let to_list t = t

let is_empty t = match t with [] -> true | _ :: _ -> false

let same_number t1 t2 = List.compare_lengths t1 t2 = 0

let cardinal t = List.length t

let vars t = List.map BP.var t

let simples t = List.map BP.simple t

let to_set t = Bound_parameter.Set.of_list t

let var_set t = Variable.Set.of_list (vars t)

let rename t = List.map (fun t -> BP.rename t) t

let arity t =
  List.map
    (fun t -> Flambda_arity.Component_for_creation.Singleton (BP.kind t))
    t
  |> Flambda_arity.create

let free_names t =
  List.fold_left
    (fun result param -> Name_occurrences.union result (BP.free_names param))
    Name_occurrences.empty t

let apply_renaming t renaming =
  Misc.Stdlib.List.map_sharing (fun param -> BP.apply_renaming param renaming) t

let ids_for_export t = Ids_for_export.union_list (List.map BP.ids_for_export t)

let check_no_duplicates t =
  if not (Flambda_features.check_invariants ())
  then ()
  else
    let t_set = BP.Set.of_list t in
    if BP.Set.cardinal t_set <> List.length t
    then Misc.fatal_errorf "Duplicates in bound parameter list:@ %a" print t

let renaming t1 ~guaranteed_fresh:t2 =
  try
    List.fold_left2
      (fun renaming param1 param2 ->
        Renaming.add_variable renaming (BP.var param1) (BP.var param2))
      Renaming.empty t1 t2
  with Invalid_argument _ ->
    assert (List.compare_lengths t1 t2 <> 0);
    Misc.fatal_errorf "Parameter lists are of differing lengths:@ %a@ and@ %a"
      print t1 print t2

let filter f t = List.filter f t

let exists f t = List.exists f t
