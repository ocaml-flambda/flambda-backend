(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

module type Term = sig
  include Contains_names.S
  include Contains_ids.S with type t := t
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
end

type printing_style =
  | Normal
  | Brackets
  | Existential

let before_binding_position style =
  match style with
  | Normal -> "\u{0418}"
  | Brackets -> "["
  | Existential -> "\u{2203} "

let after_binding_position style =
  match style with
  | Normal -> "."
  | Brackets -> "]"
  | Existential -> "."

let printing_style = ref Brackets

let set_printing_style new_style =
  printing_style := new_style

let with_printing_style new_style ~f =
  let old_style = !printing_style in
  printing_style := new_style;
  f ();
  printing_style := old_style

module type Common = sig
  type t
  val print : Format.formatter -> t -> unit
  val print_with_cache : cache:Printing_cache.t -> Format.formatter -> t -> unit
end

module Make (Bindable : Bindable.S) (Term : Term) = struct
  type t = Bindable.t * Term.t

  let create name term = name, term

  let [@inline always] pattern_match (name, term) ~f =
    let fresh_name = Bindable.rename name in
    let perm = Bindable.name_permutation name ~guaranteed_fresh:fresh_name in
    let fresh_term = Term.apply_renaming term perm in
    f fresh_name fresh_term

  let [@ocamlformat "disable"] print ppf t =
    let style = !printing_style in
    pattern_match t ~f:(fun name term ->
      Format.fprintf ppf "@[<hov 1>%s@<1>%s%s%a%s@<1>%s%s@ %a@]"
        (Flambda_colours.name_abstraction ())
        (before_binding_position style)
        (Flambda_colours.normal ())
        Bindable.print name
        (Flambda_colours.name_abstraction ())
        (after_binding_position style)
        (Flambda_colours.normal ())
        Term.print term)

  let [@ocamlformat "disable"] print_with_cache ~cache ppf t =
    let style = !printing_style in
    pattern_match t ~f:(fun name term ->
      Format.fprintf ppf "@[<hov 1>%s@<1>%s%s%a%s@<1>%s%s@ %a@]"
        (Flambda_colours.name_abstraction ())
        (before_binding_position style)
        (Flambda_colours.normal ())
        Bindable.print name
        (Flambda_colours.name_abstraction ())
        (after_binding_position style)
        (Flambda_colours.normal ())
        (Term.print_with_cache ~cache) term)

  let [@inline always] pattern_match_mapi t ~f =
    pattern_match t ~f:(fun fresh_name fresh_term ->
      let new_term = f fresh_name fresh_term in
      fresh_name, new_term)

  let [@inline always] pattern_match_map t ~f =
    pattern_match_mapi t ~f:(fun _fresh_name fresh_term -> f fresh_term)

  let [@inline always] pattern_match_pair (name0, term0) (name1, term1) ~f =
    let fresh_name = Bindable.rename name0 in
    let perm0 = Bindable.name_permutation name0 ~guaranteed_fresh:fresh_name in
    let perm1 = Bindable.name_permutation name1 ~guaranteed_fresh:fresh_name in
    let fresh_term0 = Term.apply_renaming term0 perm0 in
    let fresh_term1 = Term.apply_renaming term1 perm1 in
    f fresh_name fresh_term0 fresh_term1

  let apply_renaming ((name, term) as t) perm =
    if Renaming.is_empty perm then t
    else
      let name = Bindable.apply_renaming name perm in
      let term = Term.apply_renaming term perm in
      name, term

  let free_names (name, term) =
    let in_binding_position = Bindable.singleton_occurrence_in_terms name in
    let free_in_term = Term.free_names term in
    Name_occurrences.diff free_in_term in_binding_position

  let all_ids_for_export (name, term) =
    Ids_for_export.union (Bindable.all_ids_for_export name)
      (Term.all_ids_for_export term)
end [@@inline always]

module Make_list (Bindable : Bindable.S) (Term : Term) = struct
  type t = Bindable.t list * Term.t

  let create names term =
    let names_set = Bindable.Set.of_list names in
    if List.length names <> Bindable.Set.cardinal names_set then begin
      Misc.fatal_errorf "Cannot create generalised abstraction value with \
          non-disjoint names in binding position: %a"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          Bindable.print) names
    end;
    names, term

  let [@inline always] pattern_match (names, term) ~f =
    match names with
    | [] -> f [] term
    | _ ->
      let fresh_names_rev, perm =
        List.fold_left (fun (fresh_names_rev, perm) stale_name ->
            let fresh_name = Bindable.rename stale_name in
            let perm =
              Bindable.add_to_name_permutation stale_name
                ~guaranteed_fresh:fresh_name perm
            in
            fresh_name :: fresh_names_rev, perm)
          ([], Renaming.empty)
          names
      in
      let fresh_names = List.rev fresh_names_rev in
      let fresh_term = Term.apply_renaming term perm in
      f fresh_names fresh_term

  let print_bindable_name_list ppf bns =
    let style = !printing_style in
    match bns with
    | [] -> ()
    | _ ->
      Format.fprintf ppf "@<1>%s%s@<1>%s%a@<1>%s%s@<1>%s"
        (Flambda_colours.name_abstraction ())
        (before_binding_position style)
        (Flambda_colours.normal ())
        (Format.pp_print_list ~pp_sep:Format.pp_print_space
          Bindable.print) bns
        (Flambda_colours.name_abstraction ())
        (after_binding_position style)
        (Flambda_colours.normal ())

  let [@ocamlformat "disable"] print ppf t =
    pattern_match t ~f:(fun names term ->
      Format.fprintf ppf "@[<hov 1>%a@ %a@]"
        print_bindable_name_list names
        Term.print term)

  let [@ocamlformat "disable"] print_with_cache ~cache ppf t =
    pattern_match t ~f:(fun names term ->
      Format.fprintf ppf "@[<hov 1>%a@ %a@]"
        print_bindable_name_list names
        (Term.print_with_cache ~cache) term)

  let [@inline always] pattern_match_mapi t ~f =
    pattern_match t ~f:(fun fresh_names fresh_term ->
      let new_term = f fresh_names fresh_term in
      fresh_names, new_term)

  let [@inline always] pattern_match_map t ~f =
    pattern_match_mapi t ~f:(fun _name term -> f term)

  let [@inline always] pattern_match_pair
        ((names0, term0) as t1) ((names1, term1) as t2) ~f =
    if List.compare_lengths names0 names1 <> 0 then begin
      let print ppf t : unit = print ppf t in
      Misc.fatal_errorf "Cannot concrete a pair of generalised abstractions \
          unless they have the same number of names in binding position:@ \
          %a@ and@ %a"
        print t1
        print t2
    end;
    let fresh_names_rev, perm0, perm1 =
      List.fold_left2
        (fun (fresh_names_rev, perm0, perm1) stale_name0 stale_name1 ->
          let fresh_name = Bindable.rename stale_name0 in
          let perm0 =
            Bindable.add_to_name_permutation stale_name0
              ~guaranteed_fresh:fresh_name perm0
          in
          let perm1 =
            Bindable.add_to_name_permutation stale_name1
              ~guaranteed_fresh:fresh_name perm1
          in
          fresh_name :: fresh_names_rev, perm0, perm1)
        ([], Renaming.empty, Renaming.empty)
        names0 names1
    in
    let fresh_names = List.rev fresh_names_rev in
    let fresh_term0 = Term.apply_renaming term0 perm0 in
    let fresh_term1 = Term.apply_renaming term1 perm1 in
    f fresh_names fresh_term0 fresh_term1

  let apply_renaming (names, term) perm =
    let names =
      List.map (fun name -> Bindable.apply_renaming name perm) names
    in
    let term = Term.apply_renaming term perm in
    names, term

  let free_names (names, term) =
    let in_binding_position =
      List.fold_left (fun in_binding_position name ->
          Bindable.add_occurrence_in_terms name in_binding_position)
        (Name_occurrences.empty)
        names
    in
    let free_in_term = Term.free_names term in
    Name_occurrences.diff free_in_term in_binding_position

  let all_ids_for_export (names, term) =
    let term_ids = Term.all_ids_for_export term in
    List.fold_left (fun ids name ->
        Ids_for_export.union ids (Bindable.all_ids_for_export name))
      term_ids
      names
end [@@inline always]

module Make_map (Bindable : Bindable.S) (Term : Term) = struct
  type t = E : _ Bindable.Map.t * Term.t -> t

  let create map term = E (map, term)

  let [@inline always] pattern_match t ~f =
    match t with
    | E (names, term) ->
      if Bindable.Map.is_empty names then f term
      else
        let perm =
          Bindable.Map.fold (fun stale_name _ perm ->
              let fresh_name = Bindable.rename stale_name in
              Bindable.add_to_name_permutation stale_name
                ~guaranteed_fresh:fresh_name perm)
            names
            Renaming.empty
        in
        f (Term.apply_renaming term perm)

  let pattern_match' t ~f =
    match t with
    | E (names, term) ->
      if Bindable.Map.is_empty names then f [] term
      else
        let fresh_names, perm =
          Bindable.Map.fold (fun stale_name _ (fresh_names, perm) ->
              let fresh_name = Bindable.rename stale_name in
              let fresh_names = Bindable.Set.add fresh_name fresh_names in
              let perm =
                Bindable.add_to_name_permutation stale_name
                  ~guaranteed_fresh:fresh_name perm
              in
              fresh_names, perm)
            names
            (Bindable.Set.empty, Renaming.empty)
        in
        f (Bindable.Set.elements fresh_names)
          (Term.apply_renaming term perm)

  let print_bindable_name_list ppf bns =
    let style = !printing_style in
    match bns with
    | [] -> ()
    | _ ->
      Format.fprintf ppf "@<1>%s%s@<1>%s%a@<1>%s%s@<1>%s"
        (Flambda_colours.name_abstraction ())
        (before_binding_position style)
        (Flambda_colours.normal ())
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Bindable.print) bns
        (Flambda_colours.name_abstraction ())
        (after_binding_position style)
        (Flambda_colours.normal ())

  let [@ocamlformat "disable"] print ppf t =
    pattern_match' t ~f:(fun names term ->
      Format.fprintf ppf "@[<hov 1>%a@ %a@]"
        print_bindable_name_list names
        Term.print term)

  let [@ocamlformat "disable"] print_with_cache ~cache ppf t =
    pattern_match' t ~f:(fun names term ->
      Format.fprintf ppf "@[<hov 1>%a@ %a@]"
        print_bindable_name_list names
        (Term.print_with_cache ~cache) term)

  let apply_renaming t perm =
    match t with
    | E (names, term) ->
      let names =
        Bindable.Map.fold (fun name datum names ->
            let name = Bindable.apply_renaming name perm in
            Bindable.Map.add name datum names)
          names
          Bindable.Map.empty
      in
      let term = Term.apply_renaming term perm in
      E (names, term)

  let free_names t =
    match t with
    | E (names, term) ->
      let in_binding_position =
        Bindable.Map.fold (fun name _ in_binding_position ->
            Bindable.add_occurrence_in_terms name in_binding_position)
          names
          Name_occurrences.empty
      in
      let free_in_term = Term.free_names term in
      Name_occurrences.diff free_in_term in_binding_position
end [@@inline always]
