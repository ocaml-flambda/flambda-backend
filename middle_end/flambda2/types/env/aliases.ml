(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type coercion_to_canonical = Coercion.t

let compose_map_values_exn map ~then_:coercion =
  if Coercion.is_id coercion
  then map
  else
    Name.Map.map_sharing
      (fun old_coercion -> Coercion.compose_exn old_coercion ~then_:coercion)
      map

module Map_to_canonical = struct
  type t = coercion_to_canonical Name.Map.t

  let fatal_inconsistent ~func_name elt coercion1 coercion2 =
    Misc.fatal_errorf
      "[%s] maps with inconsistent  element/coercion couples; %a has coercions \
       %a and %a"
      func_name Name.print elt Coercion.print coercion1 Coercion.print coercion2

  let inter map1 map2 =
    Name.Map.merge
      (fun _elt coercion1 coercion2 ->
        match coercion1, coercion2 with
        | None, None | Some _, None | None, Some _ -> None
        | Some coercion1, Some coercion2 ->
          (* See documentation of [Alias_set.inter] *)
          if Coercion.equal coercion1 coercion2 then Some coercion1 else None)
      map1 map2

  let union map1 map2 =
    Name.Map.union
      (fun elt coercion1 coercion2 ->
        match coercion1, coercion2 with
        | coercion1, coercion2 ->
          if Coercion.equal coercion1 coercion2
          then Some coercion1
          else
            fatal_inconsistent ~func_name:"Aliases.Map_to_canonical.union" elt
              coercion1 coercion2)
      map1 map2

  let ids_for_export t =
    Name.Map.fold
      (fun name coercion ids ->
        Ids_for_export.union ids
          (Ids_for_export.add_name (Coercion.ids_for_export coercion) name))
      t Ids_for_export.empty

  let apply_renaming t renaming =
    Name.Map.fold
      (fun name coercion t ->
        Name.Map.add
          (Renaming.apply_name renaming name)
          (Coercion.apply_renaming coercion renaming)
          t)
      t Name.Map.empty
end

module Aliases_of_canonical_element : sig
  type t

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val empty : t

  val is_empty : t -> bool

  val add :
    t ->
    Name.t ->
    coercion_to_canonical:coercion_to_canonical ->
    Name_mode.t ->
    t

  val find_earliest_candidates :
    t ->
    filter_by_scope:(Name_mode.t -> Map_to_canonical.t -> Map_to_canonical.t) ->
    min_name_mode:Name_mode.t ->
    Map_to_canonical.t option

  val all : t -> Map_to_canonical.t

  val mem : t -> Name.t -> bool

  val union : t -> t -> t

  val inter : t -> t -> t

  val compose : t -> then_:Coercion.t -> t

  include Contains_ids.S with type t := t

  val apply_renaming : t -> Renaming.t -> t
end = struct
  type t =
    { aliases : Map_to_canonical.t Name_mode.Map.t;
      all : Map_to_canonical.t
    }

  let invariant { aliases; all } =
    (* The elements in [aliases] have disjoint set of keys. *)
    let aliases_union : Map_to_canonical.t =
      Name_mode.Map.fold
        (fun _name_mode map acc ->
          Name.Map.union
            (fun elt _coercion1 _coercion2 ->
              Misc.fatal_errorf
                "[Aliases_of_canonical_element.invariant]: element %a appears \
                 in several modes"
                Name.print elt)
            map acc)
        aliases Name.Map.empty
    in
    (* [all] is the union of all elements in [aliases] *)
    if Name.Map.equal Coercion.equal all aliases_union
    then ()
    else
      Misc.fatal_errorf
        "[Aliases_of_canonical_element.invariant]: [aliases] and [all] are not \
         consistent"

  let [@ocamlformat "disable"] print ppf { aliases; all = _; } =
    Name_mode.Map.print (Name.Map.print Coercion.print) ppf aliases

  let empty = { aliases = Name_mode.Map.empty; all = Name.Map.empty }

  let is_empty t = Name.Map.is_empty t.all

  let add t elt ~coercion_to_canonical name_mode =
    if Name.Map.mem elt t.all
    then
      Misc.fatal_errorf "%a already added to [Aliases_of_canonical_element]: %a"
        Name.print elt print t;
    let aliases =
      Name_mode.Map.update name_mode
        (function
          | None -> Some (Name.Map.singleton elt coercion_to_canonical)
          | Some elts ->
            if Flambda_features.check_invariants ()
            then assert (not (Name.Map.mem elt elts));
            Some (Name.Map.add elt coercion_to_canonical elts))
        t.aliases
    in
    let all = Name.Map.add elt coercion_to_canonical t.all in
    { aliases; all }

  let find_earliest_candidates t ~filter_by_scope ~min_name_mode =
    Name_mode.Map.fold
      (fun order aliases res_opt ->
        match res_opt with
        | Some _ -> res_opt
        | None -> (
          match Name_mode.compare_partial_order order min_name_mode with
          | None -> None
          | Some result ->
            if result >= 0
            then
              let aliases = filter_by_scope order aliases in
              if Name.Map.is_empty aliases then None else Some aliases
            else None))
      t.aliases None

  let mem t elt = Name.Map.mem elt t.all

  let all t = t.all

  let union t1 t2 =
    let aliases : Map_to_canonical.t Name_mode.Map.t =
      Name_mode.Map.union
        (fun _order elts1 elts2 -> Some (Map_to_canonical.union elts1 elts2))
        t1.aliases t2.aliases
    in
    let t = { aliases; all = Map_to_canonical.union t1.all t2.all } in
    (* Should pass the invariant if the arguments did, since we assume that the
       maps are consistent (any keys in common map to the same coercion). *)
    invariant t;
    t

  let inter t1 t2 =
    let aliases =
      Name_mode.Map.merge
        (fun _order elts1 elts2 ->
          match elts1, elts2 with
          | None, None | Some _, None | None, Some _ -> None
          | Some elts1, Some elts2 -> Some (Map_to_canonical.inter elts1 elts2))
        t1.aliases t2.aliases
    in
    let all =
      (* This assumes that there aren't any names that occur on both sides but in
       * different modes, which indeed shouldn't happen *)
      Map_to_canonical.inter t1.all t2.all
    in
    let t = { aliases; all } in
    invariant t;
    t

  let compose { aliases; all } ~then_ =
    let f m = Name.Map.map_sharing (Coercion.compose_exn ~then_) m in
    let aliases = Name_mode.Map.map_sharing f aliases in
    let all = f all in
    { aliases; all }

  let ids_for_export { aliases; all } =
    let aliases =
      Name_mode.Map.fold
        (fun _mode map_to_canonical ids ->
          Ids_for_export.union ids
            (Map_to_canonical.ids_for_export map_to_canonical))
        aliases Ids_for_export.empty
    in
    let all = Map_to_canonical.ids_for_export all in
    Ids_for_export.union aliases all

  let apply_renaming { aliases; all } renaming =
    let aliases =
      Name_mode.Map.map_sharing
        (fun map_to_canonical ->
          Map_to_canonical.apply_renaming map_to_canonical renaming)
        aliases
    in
    let all = Map_to_canonical.apply_renaming all renaming in
    { aliases; all }
end

module Alias_set = struct
  type t =
    { const : Reg_width_const.t option;
      names : Coercion.t Name.Map.t
    }

  let empty = { const = None; names = Name.Map.empty }

  let create_aliases_of_element ~element:_ ~canonical_element
      ~coercion_from_canonical_to_element ~alias_names_with_coercions_to_element
      =
    Simple.pattern_match canonical_element
      ~const:(fun canonical_const ->
        let const = Some canonical_const in
        let names = alias_names_with_coercions_to_element in
        { const; names })
      ~name:(fun canonical_name ~coercion ->
        assert (Coercion.is_id coercion);
        let const = None in
        let names =
          Name.Map.add canonical_name coercion_from_canonical_to_element
            alias_names_with_coercions_to_element
        in
        { const; names })

  let singleton simple =
    Simple.pattern_match simple
      ~const:(fun const -> { const = Some const; names = Name.Map.empty })
      ~name:(fun name ~coercion ->
        { const = None; names = Name.Map.singleton name coercion })

  let choose_opt { const; names } =
    match const with
    | Some const ->
      if Name.Map.is_empty names then Some (Simple.const const) else None
    | None ->
      Name.Map.choose_opt names
      |> Option.map (fun (name, coercion) ->
             Simple.with_coercion (Simple.name name) coercion)

  let get_singleton { const; names } =
    match const with
    | Some const ->
      if Name.Map.is_empty names then Some (Simple.const const) else None
    | None ->
      Name.Map.get_singleton names
      |> Option.map (fun (name, coercion) ->
             Simple.with_coercion (Simple.name name) coercion)

  let [@ocamlformat "disable"] print ppf { const; names; } =
    let none ppf () =
      Format.fprintf ppf "%t()%t" Flambda_colours.elide Flambda_colours.pop
    in
    Format.fprintf ppf
      "@[<hov 1>(\
           @[<hov 1>(const@ %a)@]@ \
           @[<hov 1>(names@ %a)@]\
       @]"
       (Format.pp_print_option Reg_width_const.print ~none) const
       (Name.Map.print Coercion.print) names

  let inter { const = const1; names = names1 }
      { const = const2; names = names2 } =
    let const =
      match const1, const2 with
      | Some const1, Some const2 when Reg_width_const.equal const1 const2 ->
        Some const1
      | _, _ -> None
    in
    let names = Map_to_canonical.inter names1 names2 in
    { const; names }

  let filter { const; names } ~f =
    let const =
      match const with
      | Some const when f (Simple.const const) -> Some const
      | _ -> None
    in
    let names =
      Name.Map.filter
        (fun name coercion ->
          let simple = Simple.with_coercion (Simple.name name) coercion in
          f simple)
        names
    in
    { const; names }

  let find_best { const; names } =
    match const with
    | Some const -> Some (Simple.const const)
    | None -> (
      let key_is_symbol key _data = Name.is_symbol key in
      let symbols, vars = Name.Map.partition key_is_symbol names in
      match Name.Map.min_binding_opt symbols with
      | Some (symbol, coercion) ->
        Some (Simple.with_coercion (Simple.name symbol) coercion)
      | None -> (
        match Name.Map.min_binding_opt vars with
        | Some (var, coercion) ->
          Some (Simple.with_coercion (Simple.name var) coercion)
        | None -> None))
end

type t =
  { canonical_elements : (Simple.t * coercion_to_canonical) Name.Map.t;
    (* Canonical elements that have no known aliases are not included in
       [canonical_elements]. *)
    aliases_of_canonical_names : Aliases_of_canonical_element.t Name.Map.t;
    (* For [elt |-> aliases] in [aliases_of_canonical_names], then [aliases]
       never includes [elt]. *)
    (* CR mshinwell: check this always holds *)
    aliases_of_consts : Aliases_of_canonical_element.t Reg_width_const.Map.t
  }

(* Canonical elements can be seen as a collection of star graphs:
 *
 * canon_i <--[coercion_i_0]-- elem_i_0
 *     ^ ^--[...]-- ...
 *      \--[coercion_i_m]-- elem_i_m
 *
 * ...
 *
 * canon_j <--[coercion_j_0]-- elem_j_0
 *     ^ ^--[...]-- ...
 *      \--[coercion_j_n]-- elem_j_n
 *
 *
 * stored as a map:
 *
 * canonical_elements[elem_i_0] = (canon_i, coercion_i_0)
 * ...
 * canonical_elements[elem_i_m] = (canon_i, coercion_i_m)
 *
 * ...
 *
 * canonical_elements[elem_j_0] = (canon_j, coercion_j_0)
 * ...
 * canonical_elements[elem_j_n] = (canon_j, coercion_j_n)
 *)

let [@ocamlformat "disable"] print ppf
    { canonical_elements; aliases_of_canonical_names;
      aliases_of_consts }=
  let print_element_and_coercion ppf (elt, coercion) =
    Format.fprintf ppf "@[<hov 1>(\
                        %a@ \
                        @[<hov 1>%t(coercion@ %a)%t@]\
                        )@]"
      Simple.print elt
      (if Coercion.is_id coercion
      then Flambda_colours.elide
      else Flambda_colours.none)
      Coercion.print coercion
      Flambda_colours.pop
  in
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(canonical_elements@ %a)@]@ \
      @[<hov 1>(aliases_of_canonical_names@ %a)@]@ \
      @[<hov 1>(aliases_of_consts@ %a)@]\
      )@]"
    (Name.Map.print print_element_and_coercion) canonical_elements
    (Name.Map.print Aliases_of_canonical_element.print)
    aliases_of_canonical_names
    (Reg_width_const.Map.print Aliases_of_canonical_element.print)
    aliases_of_consts

let name_defined_earlier ~binding_time_resolver ~binding_times_and_modes alias
    ~than =
  if Name.equal alias than || (Name.is_symbol alias && Name.is_symbol than)
  then false
  else if Name.is_symbol alias && not (Name.is_symbol than)
  then true
  else if Name.is_symbol than && not (Name.is_symbol alias)
  then false
  else
    (* Names from imported units may be mapped by [binding_times_and_modes].
       However if this situation occurs they should have binding time
       [Binding_time.imported_variables] (see [Typing_env.add_equation0]). *)
    let alias_binding_time =
      match Name.Map.find alias binding_times_and_modes with
      | exception Not_found -> Binding_time.imported_variables
      | _, binding_time_and_mode ->
        Binding_time.With_name_mode.binding_time binding_time_and_mode
    in
    let than_binding_time =
      match Name.Map.find than binding_times_and_modes with
      | exception Not_found -> Binding_time.imported_variables
      | _, binding_time_and_mode ->
        Binding_time.With_name_mode.binding_time binding_time_and_mode
    in
    if Binding_time.strictly_earlier alias_binding_time ~than:than_binding_time
    then true
    else if not (Binding_time.equal alias_binding_time than_binding_time)
    then false
    else
      let alias_comp_unit = Name.compilation_unit alias in
      let than_comp_unit = Name.compilation_unit than in
      (* The compilation unit ordering is arbitrary, but total. *)
      let c = Compilation_unit.compare alias_comp_unit than_comp_unit in
      if c < 0
      then true
      else if c > 0
      then false
      else
        let alias_binding_time =
          binding_time_resolver alias
          |> Binding_time.With_name_mode.binding_time
        in
        let than_binding_time =
          binding_time_resolver than |> Binding_time.With_name_mode.binding_time
        in
        Binding_time.strictly_earlier alias_binding_time ~than:than_binding_time

let defined_earlier ~binding_time_resolver ~binding_times_and_modes alias ~than
    =
  Simple.pattern_match than
    ~const:(fun _ -> false)
    ~name:(fun than ~coercion:_ ->
      Simple.pattern_match alias
        ~const:(fun _ -> true)
        ~name:(fun alias ~coercion:_ ->
          name_defined_earlier ~binding_time_resolver ~binding_times_and_modes
            alias ~than))

let binding_time_and_name_mode ~binding_times_and_modes elt =
  Simple.pattern_match' elt
    ~const:(fun _ -> Binding_time.With_name_mode.consts)
    ~var:(fun var ~coercion:_ ->
      let name = Name.var var in
      match Name.Map.find name binding_times_and_modes with
      | _, binding_time_and_mode -> binding_time_and_mode
      | exception Not_found ->
        (* This variable must be in another compilation unit. *)
        Binding_time.With_name_mode.imported_variables)
    ~symbol:(fun _ ~coercion:_ -> Binding_time.With_name_mode.symbols)

let compute_name_mode_unscoped ~binding_times_and_modes elt =
  Binding_time.With_name_mode.name_mode
    (binding_time_and_name_mode ~binding_times_and_modes elt)

let compute_name_mode ~binding_times_and_modes elt ~min_binding_time =
  Binding_time.With_name_mode.scoped_name_mode
    (binding_time_and_name_mode ~binding_times_and_modes elt)
    ~min_binding_time

let invariant ~binding_time_resolver ~binding_times_and_modes t =
  if Flambda_features.check_invariants ()
  then
    let all_aliases_of_names : Map_to_canonical.t =
      Name.Map.fold
        (fun canonical_element aliases all_aliases ->
          Aliases_of_canonical_element.invariant aliases;
          let aliases = Aliases_of_canonical_element.all aliases in
          if not
               (Name.Map.for_all
                  (fun elt _coercion ->
                    name_defined_earlier ~binding_time_resolver
                      ~binding_times_and_modes canonical_element ~than:elt)
                  aliases)
          then
            Misc.fatal_errorf
              "Canonical element %a is not earlier than all of its aliases:@ %a"
              Name.print canonical_element print t;
          if Name.Map.mem canonical_element aliases
          then
            Misc.fatal_errorf "Canonical element %a occurs in alias set:@ %a"
              Name.print canonical_element
              (Name.Map.print Coercion.print)
              aliases;
          if Name.Map.inter_domain_is_non_empty aliases all_aliases
          then Misc.fatal_errorf "Overlapping alias sets:@ %a" print t;
          Map_to_canonical.union aliases all_aliases)
        t.aliases_of_canonical_names Name.Map.empty
    in
    let _all_aliases : Map_to_canonical.t =
      Reg_width_const.Map.fold
        (fun _const aliases all_aliases ->
          Aliases_of_canonical_element.invariant aliases;
          let aliases = Aliases_of_canonical_element.all aliases in
          if Name.Map.inter_domain_is_non_empty aliases all_aliases
          then Misc.fatal_errorf "Overlapping alias sets:@ %a" print t;
          Name.Map.disjoint_union aliases all_aliases)
        t.aliases_of_consts all_aliases_of_names
    in
    ()

let empty =
  { (* CR mshinwell: Rename canonical_elements, maybe to
       aliases_to_canonical_elements. *)
    canonical_elements = Name.Map.empty;
    aliases_of_canonical_names = Name.Map.empty;
    aliases_of_consts = Reg_width_const.Map.empty
  }

let is_empty
    { canonical_elements; aliases_of_canonical_names; aliases_of_consts } =
  Name.Map.is_empty canonical_elements
  && Name.Map.is_empty aliases_of_canonical_names
  && Reg_width_const.Map.is_empty aliases_of_consts

type canonical =
  | Is_canonical
  | Alias_of_canonical of
      { canonical_element : Simple.t;
        coercion_to_canonical : coercion_to_canonical
      }

let canonical t element : canonical =
  Simple.pattern_match element
    ~const:(fun _ -> Is_canonical)
    ~name:(fun name ~coercion:coercion_from_bare_element_to_element ->
      match Name.Map.find name t.canonical_elements with
      | exception Not_found -> Is_canonical
      | canonical_element, coercion_from_bare_element_to_canonical ->
        let coercion_from_element_to_bare_element =
          Coercion.inverse coercion_from_bare_element_to_element
        in
        let coercion_from_element_to_canonical =
          Coercion.compose_exn coercion_from_element_to_bare_element
            ~then_:coercion_from_bare_element_to_canonical
        in
        if Flambda_features.check_invariants ()
        then assert (not (Simple.equal element canonical_element));
        Alias_of_canonical
          { canonical_element;
            coercion_to_canonical = coercion_from_element_to_canonical
          })

let get_aliases_of_canonical_element t ~canonical_element =
  assert (not (Simple.has_coercion canonical_element));
  let name name ~coercion:_ = Name.Map.find name t.aliases_of_canonical_names in
  let const const = Reg_width_const.Map.find const t.aliases_of_consts in
  match Simple.pattern_match canonical_element ~name ~const with
  | exception Not_found -> Aliases_of_canonical_element.empty
  | aliases -> aliases

(*
 * before
 * ~~~~~~
 * canonical_element <--[coercion_ce_0]-- ce_0
 *   ^ ^--[...]-- ...
 *    \--[coercion_ce_m]-- ce_m
 * to_be_demoted <--[coercion_tbd_0]-- tbd_0
 *   ^ ^--[...]-- ...
 *    \--[coercion_tbd_n]-- tbd_n
 *
 * i.e.
 *
 * canonical_elements[ce_0] = (canonical_element, coercion_ce_0)
 * ...
 * canonical_elements[ce_m] = (canonical_element, coercion_ce_m)
 * canonical_elements[tbd_0] = (to_be_demoted, coercion_tbd_0)
 * ...
 * canonical_elements[tbd_n] = (to_be_demoted, coercion_tbd_n)
 *
 *
 * after
 * ~~~~~
 * canonical_element <--[coercion_ce_0]-- ce_0
 *   ^ ^ ^ ^ ^ ^--[...]-- ...
 *   | | | |  \--[coercion_ce_m]-- ce_m
 *   | | | \--[coercion_to_canonical]-- to_be_demoted
 *   | | \--[compose(coercion_tbd_0, coercion_to_canonical)]-- tbd_0
 *   | \--[...]-- ...
 *   \--[compose(coercion_tbd_n, coercion_to_canonical)]-- tbd_n
 *
 * i.e.
 *
 * canonical_elements[ce_0] = (canonical_element, coercion_ce_0)
 * ...
 * canonical_elements[ce_m] = (canonical_element, coercion_ce_m)
 * canonical_elements[to_be_demoted] =
 *   (canonical_element, coercion_to_canonical)
 * canonical_elements[tbd_0] =
 *   (canonical_element, compose(coercion_tbd_0, coercion_to_canonical))
 * ...
 * canonical_elements[tbd_n] =
 *   (canonical_element, compose(coercion_tbd_n, coercion_to_canonical))
 *)
let add_alias_between_canonical_elements ~binding_time_resolver
    ~binding_times_and_modes t ~canonical_element ~coercion_to_canonical
    ~to_be_demoted =
  if Simple.equal canonical_element to_be_demoted
  then
    if Coercion.is_id coercion_to_canonical
    then Or_bottom.Ok t
    else
      Misc.fatal_errorf
        "Cannot add an alias of %a@ to itself with a non-identity coercion@ %a"
        Simple.print canonical_element Coercion.print coercion_to_canonical
  else
    let open Or_bottom.Let_syntax in
    let<+ name_to_be_demoted =
      assert (not (Simple.has_coercion to_be_demoted));
      Simple.pattern_match to_be_demoted
        ~const:(fun _ ->
          (* We're adding aliases between different constants; this is a Bottom case. *)
            Or_bottom.Bottom)
        ~name:(fun name ~coercion:_ -> Or_bottom.Ok name)
    in
    let aliases_of_to_be_demoted =
      get_aliases_of_canonical_element t ~canonical_element:to_be_demoted
    in
    if Flambda_features.check_invariants ()
    then
      Simple.pattern_match canonical_element
        ~const:(fun _ -> ())
        ~name:(fun canonical_element ~coercion ->
          assert (Coercion.is_id coercion);
          assert (
            not
              (Aliases_of_canonical_element.mem aliases_of_to_be_demoted
                 canonical_element)));
    let canonical_elements =
      t.canonical_elements
      |> Name.Map.fold
           (fun alias coercion_to_to_be_demoted canonical_elements ->
             let coercion_to_canonical =
               Coercion.compose_exn coercion_to_to_be_demoted
                 ~then_:coercion_to_canonical
             in
             Name.Map.add alias
               (canonical_element, coercion_to_canonical)
               canonical_elements)
           (Aliases_of_canonical_element.all aliases_of_to_be_demoted)
      |> Name.Map.add name_to_be_demoted
           (canonical_element, coercion_to_canonical)
    in
    let aliases_of_canonical_element =
      get_aliases_of_canonical_element t ~canonical_element
    in
    if Flambda_features.check_invariants ()
    then (
      assert (
        not
          (Aliases_of_canonical_element.mem aliases_of_canonical_element
             name_to_be_demoted));
      assert (
        Aliases_of_canonical_element.is_empty
          (Aliases_of_canonical_element.inter aliases_of_canonical_element
             aliases_of_to_be_demoted)));
    let aliases =
      (* CR lmaurer: Consider adding a combination [union] and [compose] to
         [Aliases_of_canonical_element] to save a map traversal here (a single
         single call to [Name.Map.merge] could implement both operations). *)
      Aliases_of_canonical_element.add
        (Aliases_of_canonical_element.union
           (Aliases_of_canonical_element.compose aliases_of_to_be_demoted
              ~then_:coercion_to_canonical)
           aliases_of_canonical_element)
        name_to_be_demoted ~coercion_to_canonical
        (compute_name_mode_unscoped ~binding_times_and_modes to_be_demoted)
    in
    let aliases_of_canonical_names =
      Name.Map.remove name_to_be_demoted t.aliases_of_canonical_names
    in
    let aliases_of_canonical_names, aliases_of_consts =
      assert (not (Simple.has_coercion canonical_element));
      Simple.pattern_match canonical_element
        ~name:(fun name ~coercion:_ ->
          ( Name.Map.add (* replace *) name aliases aliases_of_canonical_names,
            t.aliases_of_consts ))
        ~const:(fun const ->
          ( aliases_of_canonical_names,
            Reg_width_const.Map.add (* replace *) const aliases
              t.aliases_of_consts ))
    in
    let res =
      { canonical_elements; aliases_of_canonical_names; aliases_of_consts }
    in
    invariant ~binding_time_resolver ~binding_times_and_modes res;
    res

type to_be_demoted =
  | Demote_canonical_element1
  | Demote_canonical_element2

let choose_canonical_element_to_be_demoted ~binding_time_resolver
    ~binding_times_and_modes ~canonical_element1 ~canonical_element2 =
  if defined_earlier ~binding_time_resolver ~binding_times_and_modes
       canonical_element1 ~than:canonical_element2
  then Demote_canonical_element2
  else Demote_canonical_element1

type add_result =
  { t : t;
    canonical_element : Simple.t;
    alias_of_demoted_element : Simple.t
  }

let invariant_add_result ~binding_time_resolver ~binding_times_and_modes
    ~original_t { canonical_element; alias_of_demoted_element; t } =
  if Flambda_features.check_invariants ()
  then (
    invariant ~binding_time_resolver ~binding_times_and_modes t;
    if not
         (defined_earlier ~binding_time_resolver ~binding_times_and_modes
            canonical_element ~than:alias_of_demoted_element)
    then
      Misc.fatal_errorf
        "Canonical element %a should be defined earlier than %a after alias \
         addition.@ Original alias tracker:@ %a@ Resulting alias tracker:@ %a"
        Simple.print canonical_element Simple.print alias_of_demoted_element
        print original_t print t;
    match canonical t alias_of_demoted_element with
    | Is_canonical ->
      Misc.fatal_errorf
        "Alias %a must not be must not be canonical anymore.@ Original alias \
         tracker:@ %a@ Resulting alias tracker:@ %a"
        Simple.print alias_of_demoted_element print original_t print t
    | Alias_of_canonical _ -> ())

let add_alias ~binding_time_resolver ~binding_times_and_modes t
    ~canonical_element1 ~coercion_from_canonical_element2_to_canonical_element1
    ~canonical_element2 =
  assert (not (Simple.has_coercion canonical_element1));
  assert (not (Simple.has_coercion canonical_element2));
  let ( canonical_element,
        demoted_canonical,
        coercion_from_demoted_canonical_to_canonical ) =
    let which_element =
      choose_canonical_element_to_be_demoted ~binding_time_resolver
        ~binding_times_and_modes ~canonical_element1 ~canonical_element2
    in
    match which_element with
    | Demote_canonical_element1 ->
      let coercion_from_canonical_element1_to_canonical_element2 =
        Coercion.inverse coercion_from_canonical_element2_to_canonical_element1
      in
      ( canonical_element2,
        canonical_element1,
        coercion_from_canonical_element1_to_canonical_element2 )
    | Demote_canonical_element2 ->
      ( canonical_element1,
        canonical_element2,
        coercion_from_canonical_element2_to_canonical_element1 )
  in
  let open Or_bottom.Let_syntax in
  let<+ t =
    add_alias_between_canonical_elements ~binding_time_resolver
      ~binding_times_and_modes t ~canonical_element
      ~coercion_to_canonical:coercion_from_demoted_canonical_to_canonical
      ~to_be_demoted:demoted_canonical
  in
  let alias_of_demoted_element =
    Simple.with_coercion demoted_canonical
      coercion_from_demoted_canonical_to_canonical
  in
  { t; canonical_element; alias_of_demoted_element }

let add ~binding_time_resolver ~binding_times_and_modes t
    ~canonical_element1:element1_with_coercion
    ~canonical_element2:element2_with_coercion =
  let original_t = t in
  (* element1_with_coercion <--[c1]-- element1
   * +
   * element2_with_coercion <--[c2]-- element2
   * ~
   * element1 <--[c1^-1]-- element1_with_coercion
   * ~
   * element1 <--[c1^-1 << c2]-- element2
   *)
  let canonical_element1 = element1_with_coercion |> Simple.without_coercion in
  let canonical_element2 = element2_with_coercion |> Simple.without_coercion in
  let coercion_from_canonical_element2_to_canonical_element1 =
    Coercion.compose_exn
      (Simple.coercion element2_with_coercion)
      ~then_:(Coercion.inverse (Simple.coercion element1_with_coercion))
  in
  if Flambda_features.check_invariants ()
  then (
    if Simple.equal canonical_element1 canonical_element2
    then
      Misc.fatal_errorf "Cannot alias an element to itself: %a" Simple.print
        canonical_element1;
    Simple.pattern_match canonical_element1
      ~name:(fun _ ~coercion:_ -> ())
      ~const:(fun const1 ->
        Simple.pattern_match canonical_element2
          ~name:(fun _ ~coercion:_ -> ())
          ~const:(fun const2 ->
            Misc.fatal_errorf "Cannot add alias between two consts: %a, %a"
              Reg_width_const.print const1 Reg_width_const.print const2)));
  let add_result =
    add_alias ~binding_time_resolver ~binding_times_and_modes t
      ~canonical_element1
      ~coercion_from_canonical_element2_to_canonical_element1
      ~canonical_element2
  in
  if Flambda_features.check_invariants ()
  then
    begin match add_result with
    | Ok add_result ->
      invariant_add_result ~binding_time_resolver ~binding_times_and_modes
        ~original_t add_result
    | Bottom -> () end;
  add_result

(* CR-someday mshinwell: For the moment we allow relations between canonical
   elements that are actually incomparable under the name mode ordering, and
   check in [get_canonical_element_exn] accordingly. However maybe we should
   never allow these situations to arise. *)

let find_earliest_alias t ~canonical_element ~binding_times_and_modes
    ~min_binding_time ~min_name_mode ~binding_time_resolver
    ~coercion_from_canonical_to_element =
  (* There used to be a shortcut that avoided consulting the aliases in the
     common case that [element] is itself canonical and has no aliases, since
     then it does not appear in [canonical_elements]. However, this shortcut was
     broken: a canonical element *with* known aliases may still not appear in
     [canonical_elements]. See tests/flambda2-aliases for a test that gave
     incorrect output (saying x/39 had no aliases). It may be worth restoring
     the shortcut, perhaps by returning more information from [canonical]. *)
  (* CR mshinwell for lmaurer: please import the tests from
     https://github.com/ocaml-flambda/ocaml/tree/flambda2.0-stable/testsuite/tests/flambda2-aliases *)
  let aliases = get_aliases_of_canonical_element t ~canonical_element in
  let filter_by_scope name_mode names =
    if Name_mode.equal name_mode Name_mode.in_types
    then names
    else
      Name.Map.filter
        (fun name _coercion ->
          let scoped_name_mode =
            compute_name_mode ~binding_times_and_modes (Simple.name name)
              ~min_binding_time
          in
          Name_mode.equal name_mode scoped_name_mode)
        names
  in
  match
    Aliases_of_canonical_element.find_earliest_candidates aliases
      ~filter_by_scope ~min_name_mode
  with
  | Some at_earliest_mode ->
    (* Aliases_of_canonical_element.find_earliest_candidates only returns
       non-empty sets *)
    assert (not (Name.Map.is_empty at_earliest_mode));
    let earliest, coercion_from_earliest_to_canonical =
      Name.Map.fold
        (fun elt coercion ((min_elt, _min_coercion) as min_binding) ->
          if name_defined_earlier ~binding_time_resolver
               ~binding_times_and_modes elt ~than:min_elt
          then elt, coercion
          else min_binding)
        at_earliest_mode
        (Name.Map.min_binding at_earliest_mode)
    in
    let coercion_from_earliest_to_element =
      Coercion.compose_exn coercion_from_earliest_to_canonical
        ~then_:coercion_from_canonical_to_element
    in
    Simple.with_coercion (Simple.name earliest)
      coercion_from_earliest_to_element
  | None -> raise Not_found

let get_canonical_element_exn ~binding_time_resolver ~binding_times_and_modes t
    element elt_name_mode ~min_name_mode ~min_binding_time =
  let canonical = canonical t element in
  match canonical with
  | Is_canonical
    when match Name_mode.compare_partial_order elt_name_mode min_name_mode with
         | None -> false
         | Some c -> c >= 0 ->
    element
  | Is_canonical | Alias_of_canonical _ -> (
    let canonical_element, name_mode, coercion_from_canonical_to_element =
      match canonical with
      | Is_canonical ->
        Simple.without_coercion element, elt_name_mode, Simple.coercion element
      | Alias_of_canonical { canonical_element; coercion_to_canonical } ->
        let name_mode =
          compute_name_mode ~binding_times_and_modes canonical_element
            ~min_binding_time
        in
        canonical_element, name_mode, Coercion.inverse coercion_to_canonical
    in
    assert (not (Simple.has_coercion canonical_element));
    match Name_mode.compare_partial_order name_mode min_name_mode with
    | None ->
      find_earliest_alias t ~canonical_element ~binding_times_and_modes
        ~min_binding_time ~min_name_mode ~binding_time_resolver
        ~coercion_from_canonical_to_element
    | Some c ->
      if c >= 0
      then
        Simple.with_coercion canonical_element
          coercion_from_canonical_to_element
      else
        find_earliest_alias t ~canonical_element ~binding_times_and_modes
          ~min_binding_time ~min_name_mode ~binding_time_resolver
          ~coercion_from_canonical_to_element)

let get_aliases t element =
  match canonical t element with
  | Is_canonical ->
    let canonical_element = Simple.without_coercion element in
    assert (not (Simple.has_coercion canonical_element));
    let alias_names_with_coercions_to_canonical =
      Aliases_of_canonical_element.all
        (get_aliases_of_canonical_element t ~canonical_element)
    in
    let coercion_from_canonical_to_element = Simple.coercion element in
    let alias_names_with_coercions_to_element =
      compose_map_values_exn alias_names_with_coercions_to_canonical
        ~then_:coercion_from_canonical_to_element
    in
    Alias_set.create_aliases_of_element ~element ~canonical_element
      ~coercion_from_canonical_to_element ~alias_names_with_coercions_to_element
  | Alias_of_canonical
      { canonical_element;
        coercion_to_canonical = coercion_from_element_to_canonical
      } ->
    assert (not (Simple.has_coercion canonical_element));
    assert (
      not (Simple.equal (Simple.without_coercion element) canonical_element));
    let alias_names_with_coercions_to_canonical =
      Aliases_of_canonical_element.all
        (get_aliases_of_canonical_element t ~canonical_element)
    in
    let coercion_from_canonical_to_element =
      Coercion.inverse coercion_from_element_to_canonical
    in
    (* If any composition fails, then our coercions are inconsistent somehow,
       which should only happen when meeting *)
    let alias_names_with_coercions_to_element =
      compose_map_values_exn alias_names_with_coercions_to_canonical
        ~then_:coercion_from_canonical_to_element
    in
    (if Flambda_features.check_invariants ()
    then
      let element_coerced_to_canonical =
        Simple.apply_coercion_exn element coercion_from_element_to_canonical
      in
      (* These aliases are all equivalent to the canonical element, and so is
         our original [element] if we coerce it first, so the coerced form of
         [element] should be among the aliases. *)
      assert (
        Name.Map.exists
          (fun name coercion_from_name_to_canonical ->
            let name_coerced_to_canonical =
              Simple.apply_coercion_exn (Simple.name name)
                coercion_from_name_to_canonical
            in
            Simple.equal element_coerced_to_canonical name_coerced_to_canonical)
          alias_names_with_coercions_to_canonical));
    Alias_set.create_aliases_of_element ~element ~canonical_element
      ~coercion_from_canonical_to_element ~alias_names_with_coercions_to_element

let apply_renaming
    { canonical_elements; aliases_of_canonical_names; aliases_of_consts }
    renaming =
  let rename_name = Renaming.apply_name renaming in
  let rename_simple = Renaming.apply_simple renaming in
  let canonical_elements =
    Name.Map.fold
      (fun elt (canonical, coercion) acc ->
        Name.Map.add (rename_name elt)
          (rename_simple canonical, Coercion.apply_renaming coercion renaming)
          acc)
      canonical_elements Name.Map.empty
  in
  let aliases_of_canonical_names =
    Name.Map.fold
      (fun canonical aliases acc ->
        Name.Map.add (rename_name canonical)
          (Aliases_of_canonical_element.apply_renaming aliases renaming)
          acc)
      aliases_of_canonical_names Name.Map.empty
  in
  let aliases_of_consts =
    Reg_width_const.Map.fold
      (fun const aliases ->
        Reg_width_const.Map.add
          (Renaming.apply_const renaming const)
          (Aliases_of_canonical_element.apply_renaming aliases renaming))
      aliases_of_consts Reg_width_const.Map.empty
  in
  { canonical_elements; aliases_of_canonical_names; aliases_of_consts }

let get_canonical_ignoring_name_mode t name =
  let elt = Simple.name name in
  match canonical t elt with
  | Is_canonical -> elt
  | Alias_of_canonical { canonical_element; coercion_to_canonical } ->
    let coercion_from_canonical = Coercion.inverse coercion_to_canonical in
    Simple.apply_coercion_exn canonical_element coercion_from_canonical
