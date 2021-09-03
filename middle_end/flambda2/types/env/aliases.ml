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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Const = Reg_width_things.Const

type coercion_to_canonical = Coercion.t

let compose_map_values_exn map ~then_:coercion =
  if Coercion.is_id coercion then map else
    Name.Map.map (fun old_coercion ->
      Coercion.compose_exn old_coercion ~then_:coercion
    ) map

module Map_to_canonical = struct
  type t = coercion_to_canonical Name.Map.t

  let fatal_inconsistent ~func_name elt coercion1 coercion2 =
    Misc.fatal_errorf "[%s] maps with inconsistent  element/coercion couples; \
                      %a has coercions %a and %a"
      func_name
      Name.print elt
      Coercion.print coercion1
      Coercion.print coercion2

  let inter map1 map2 =
    Name.Map.merge (fun _elt coercion1 coercion2 ->
        match coercion1, coercion2 with
        | None, None | Some _, None | None, Some _ -> None
        | Some coercion1, Some coercion2 ->
          (* See documentation of [Alias_set.inter] *)
          if Coercion.equal coercion1 coercion2 then
            Some coercion1
          else
            None)
      map1
      map2

  let union map1 map2 =
    Name.Map.union (fun elt coercion1 coercion2 ->
        match coercion1, coercion2 with
        | coercion1, coercion2 ->
          if Coercion.equal coercion1 coercion2 then
            Some coercion1
          else
            fatal_inconsistent
              ~func_name:"Aliases.Map_to_canonical.union"
              elt coercion1 coercion2)
      map1
      map2

  let all_ids_for_export t =
    Name.Map.fold (fun name coercion ids ->
        Ids_for_export.union ids
          (Ids_for_export.add_name
            (Coercion.all_ids_for_export coercion)
            name))
      t
      Ids_for_export.empty

  let apply_renaming t renaming =
    Name.Map.fold (fun name coercion t ->
        Name.Map.add (Renaming.apply_name renaming name)
          (Coercion.apply_renaming coercion renaming)
          t)
      t
      Name.Map.empty
end

module Aliases_of_canonical_element : sig
  type t

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val empty : t
  val is_empty : t -> bool

  val add
     : t
    -> Name.t
    -> coercion_to_canonical:coercion_to_canonical
    -> Name_mode.t
    -> t

  val find_earliest_candidates
     : t
    -> filter_by_scope:(Name_mode.t -> Map_to_canonical.t -> Map_to_canonical.t)
    -> min_name_mode:Name_mode.t
    -> Map_to_canonical.t option

  val all : t -> Map_to_canonical.t

  val mem : t -> Name.t -> bool

  val union : t -> t -> t
  val inter : t -> t -> t

  val merge : t -> t -> t

  val compose : t -> then_:Coercion.t -> t

  include Contains_ids.S with type t := t

  val apply_renaming : t -> Renaming.t -> t
end = struct
  type t = {
    aliases : Map_to_canonical.t Name_mode.Map.t;
    all : Map_to_canonical.t;
  }

  let invariant { aliases; all; } =
    (* The elements in [aliases] have disjoint set of keys. *)
    let aliases_union : Map_to_canonical.t =
      Name_mode.Map.fold (fun _name_mode map acc ->
        Name.Map.union (fun elt _coercion1 _coercion2 ->
          Misc.fatal_errorf "[Aliases_of_canonical_element.invariant]: \
                             element %a appears in several modes"
            Name.print elt)
          map
          acc)
        aliases
        Name.Map.empty
    in
    (* [all] is the union of all elements in [aliases] *)
    if Name.Map.equal Coercion.equal all aliases_union then
      ()
    else
      Misc.fatal_errorf "[Aliases_of_canonical_element.invariant]: \
                         [aliases] and [all] are not consistent"

  let print ppf { aliases; all = _; } =
    Name_mode.Map.print (Name.Map.print Coercion.print) ppf aliases

  let empty = {
    aliases = Name_mode.Map.empty;
    all = Name.Map.empty;
  }

  let is_empty t = Name.Map.is_empty t.all

  let add t elt ~coercion_to_canonical name_mode =
    if Name.Map.mem elt t.all then begin
      Misc.fatal_errorf "%a already added to [Aliases_of_canonical_element]: \
                         %a"
        Name.print elt
        print t
    end;
    let aliases =
      Name_mode.Map.update name_mode
        (function
          | None -> Some (Name.Map.singleton elt coercion_to_canonical)
          | Some elts ->
            if Flambda_features.check_invariants () then begin
              assert (not (Name.Map.mem elt elts))
            end;
            Some (Name.Map.add elt coercion_to_canonical elts))
        t.aliases
    in
    let all = Name.Map.add elt coercion_to_canonical t.all in
    { aliases;
      all;
    }

  let find_earliest_candidates t ~filter_by_scope ~min_name_mode =
    Name_mode.Map.fold (fun order aliases res_opt ->
        match res_opt with
        | Some _ -> res_opt
        | None ->
          begin match
            Name_mode.compare_partial_order
              order min_name_mode
          with
          | None -> None
          | Some result ->
            if result >= 0 then
              let aliases = filter_by_scope order aliases in
              if Name.Map.is_empty aliases then None else Some aliases
            else None
          end)
      t.aliases
      None

  let mem t elt =
    Name.Map.mem elt t.all

  let all t = t.all

  let union t1 t2 =
    let aliases : Map_to_canonical.t Name_mode.Map.t=
      Name_mode.Map.union (fun _order elts1 elts2 ->
          Some (Map_to_canonical.union elts1 elts2))
        t1.aliases t2.aliases
    in
    let t =
      { aliases;
        all = Map_to_canonical.union t1.all t2.all;
      }
    in
    (* Should pass the invariant if the arguments did, since we assume that
       the maps are consistent (any keys in common map to the same coercion). *)
    invariant t;
    t

  let inter t1 t2 =
    let aliases =
      Name_mode.Map.merge (fun _order elts1 elts2 ->
        match elts1, elts2 with
        | None, None | Some _, None | None, Some _ -> None
        | Some elts1, Some elts2 ->
          Some (Map_to_canonical.inter elts1 elts2))
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

  let merge t1 t2 =
    let aliases =
      Name_mode.Map.union (fun _mode map1 map2 ->
        Some (Map_to_canonical.union map1 map2)
      )
        t1.aliases
        t2.aliases
    in
    let all = Map_to_canonical.union t1.all t2.all in
    let t = { aliases; all; } in
    (* CR vlaviron: Here we're merging structures that can come from different
       compilation units. In particular, if one variable has mode Normal in one
       of the units, then in all the other ones it will have mode In_types.
       The proper way to handle that might be to move all variables to mode
       In_types on export or import (there's code in Typing_env.find that
       takes care of returning the correct mode to the outside already, so it's
       mostly an internal issue).
       For now, as a quick fix the invariant checks are disabled here. *)
    (* invariant t; *)
    t

  let compose { aliases; all; } ~then_ =
    let f m =
      Name.Map.map (Coercion.compose_exn ~then_) m
    in
    let aliases = Name_mode.Map.map f aliases in
    let all = f all in
    { aliases; all; }

  let all_ids_for_export { aliases; all; } =
    let aliases =
      Name_mode.Map.fold (fun _mode map_to_canonical ids ->
          Ids_for_export.union ids
            (Map_to_canonical.all_ids_for_export map_to_canonical))
        aliases
        Ids_for_export.empty
    in
    let all = Map_to_canonical.all_ids_for_export all in
    Ids_for_export.union aliases all

  let apply_renaming { aliases; all; } renaming =
    let aliases =
      Name_mode.Map.map (fun map_to_canonical ->
          Map_to_canonical.apply_renaming map_to_canonical renaming)
        aliases
    in
    let all = Map_to_canonical.apply_renaming all renaming in
    { aliases; all; }
end

module Alias_set = struct
  type t = {
    const : Const.t option;
    names : Coercion.t Name.Map.t;
  }

  let empty = { const = None; names = Name.Map.empty; }

  let create_aliases_of_element
        ~element:_
        ~canonical_element
        ~coercion_from_canonical_to_element
        ~alias_names_with_coercions_to_element =
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
      ~const:(fun const ->
        { const = Some const; names = Name.Map.empty; })
      ~name:(fun name ~coercion ->
        { const = None; names = Name.Map.singleton name coercion })

  let get_singleton { const; names; } =
    match const with
    | Some const ->
      if Name.Map.is_empty names then Some (Simple.const const) else None
    | None ->
      Name.Map.get_singleton names
      |> Option.map (fun (name, coercion) ->
           Simple.with_coercion (Simple.name name) coercion)

  let print ppf { const; names; } =
    let none ppf () =
      Format.fprintf ppf "@<0>%s()" (Flambda_colours.elide ())
    in
    Format.fprintf ppf
      "@[<hov 1>(\
           @[<hov 1>(const@ %a)@]@ \
           @[<hov 1>(names@ %a)@]\
       @]"
       (Format.pp_print_option Const.print ~none) const
       (Name.Map.print Coercion.print) names

  let inter
        { const = const1; names = names1; }
        { const = const2; names = names2; } =
    let const =
      match const1, const2 with
      | Some const1, Some const2 when Const.equal const1 const2 -> Some const1
      | _, _ -> None
    in
    let names = Map_to_canonical.inter names1 names2 in
    { const; names; }

  let filter { const; names; } ~f =
    let const =
      match const with
      | Some const when f (Simple.const const) -> Some const
      | _ -> None
    in
    let names =
      Name.Map.filter (fun name coercion ->
          let simple = Simple.with_coercion (Simple.name name) coercion in
          f simple
        ) names
    in
    { const; names; }

  let find_best { const; names; } =
    match const with
    | Some const -> Some (Simple.const const)
    | None ->
      let key_is_symbol key _data = Name.is_symbol key in
      let (symbols, vars) = Name.Map.partition key_is_symbol names in
      match Name.Map.min_binding_opt symbols with
      | Some (symbol, coercion) ->
        Some (Simple.with_coercion (Simple.name symbol) coercion)
      | None ->
        match Name.Map.min_binding_opt vars with
        | Some (var, coercion) ->
          Some (Simple.with_coercion (Simple.name var) coercion)
        | None ->
          None
end


type t = {
  canonical_elements : (Simple.t * coercion_to_canonical) Name.Map.t;
  (* Canonical elements that have no known aliases are not included in
     [canonical_elements]. *)
  aliases_of_canonical_names : Aliases_of_canonical_element.t Name.Map.t;
  (* For [elt |-> aliases] in [aliases_of_canonical_names], then
     [aliases] never includes [elt]. *)
  (* CR mshinwell: check this always holds *)
  aliases_of_consts : Aliases_of_canonical_element.t Const.Map.t;
  binding_times_and_modes : Binding_time.With_name_mode.t Name.Map.t;
  (* Binding times and name modes define an order on the elements.
     The canonical element for a set of aliases is always the minimal
     element for this order, which is different from the order used
     for creating sets and maps. *)
}

(* Canonical elements can be seen as a collection of star graphs:

   canon_i <--[coercion_i_0]-- elem_i_0
       ^ ^--[...]-- ...
        \--[coercion_i_m]-- elem_i_m

   ...

   canon_j <--[coercion_j_0]-- elem_j_0
       ^ ^--[...]-- ...
        \--[coercion_j_n]-- elem_j_n


   stored as a map:

   canonical_elements[elem_i_0] = (canon_i, coercion_i_0)
   ...
   canonical_elements[elem_i_m] = (canon_i, coercion_i_m)

   ...

   canonical_elements[elem_j_0] = (canon_j, coercion_j_0)
   ...
   canonical_elements[elem_j_n] = (canon_j, coercion_j_n)
*)

let print ppf { canonical_elements; aliases_of_canonical_names;
                aliases_of_consts; binding_times_and_modes; } =
  let print_element_and_coercion ppf (elt, coercion) =
    Format.fprintf ppf "@[<hov 1>(\
                        %a@ \
                        @[<hov 1>@<0>%s(coercion@ %a)@<0>%s@]\
                        )@]"
      Simple.print elt
      (if Coercion.is_id coercion
      then Flambda_colours.elide ()
      else Flambda_colours.normal ())
      Coercion.print coercion
      (Flambda_colours.normal ())
  in
  Format.fprintf ppf
    "@[<hov 1>(\
      @[<hov 1>(canonical_elements@ %a)@]@ \
      @[<hov 1>(aliases_of_canonical_names@ %a)@]@ \
      @[<hov 1>(aliases_of_consts@ %a)@]@ \
      @[<hov 1>(binding_times_and_modes@ %a)@]\
      )@]"
    (Name.Map.print print_element_and_coercion) canonical_elements
    (Name.Map.print Aliases_of_canonical_element.print)
    aliases_of_canonical_names
    (Const.Map.print Aliases_of_canonical_element.print)
    aliases_of_consts
    (Name.Map.print Binding_time.With_name_mode.print)
    binding_times_and_modes

let name_defined_earlier t alias ~than =
  let info1 = Name.Map.find alias t.binding_times_and_modes in
  let info2 = Name.Map.find than t.binding_times_and_modes in
  Binding_time.strictly_earlier
    (Binding_time.With_name_mode.binding_time info1)
    ~than:(Binding_time.With_name_mode.binding_time info2)

let defined_earlier t alias ~than =
  Simple.pattern_match than
    ~const:(fun _ -> false)
    ~name:(fun than ~coercion:_ ->
      Simple.pattern_match alias
        ~const:(fun _ -> true)
        ~name:(fun alias ~coercion:_ -> name_defined_earlier t alias ~than))

let binding_time_and_name_mode t elt =
  Simple.pattern_match elt
    ~const:(fun _ ->
      Binding_time.With_name_mode.create
        Binding_time.consts_and_discriminants
        Name_mode.normal)
    ~name:(fun elt ~coercion:_ -> Name.Map.find elt t.binding_times_and_modes)

let name_mode_unscoped t elt =
  Binding_time.With_name_mode.name_mode (binding_time_and_name_mode t elt)

let name_mode t elt ~min_binding_time =
  Binding_time.With_name_mode.scoped_name_mode
    (binding_time_and_name_mode t elt)
    ~min_binding_time

let invariant t =
  if Flambda_features.check_invariants () then begin
    let all_aliases_of_names : Map_to_canonical.t =
      Name.Map.fold (fun canonical_element aliases all_aliases ->
          Aliases_of_canonical_element.invariant aliases;
          let aliases = Aliases_of_canonical_element.all aliases in
          if not (Name.Map.for_all (fun elt _coercion ->
            name_defined_earlier t canonical_element ~than:elt) aliases)
          then begin
            Misc.fatal_errorf "Canonical element %a is not earlier than \
                all of its aliases:@ %a"
              Name.print canonical_element
              print t
          end;
          if Name.Map.mem canonical_element aliases then begin
            Misc.fatal_errorf "Canonical element %a occurs in alias set:@ %a"
              Name.print canonical_element
              (Name.Map.print Coercion.print) aliases
          end;
          if Name.Map.inter_domain_is_non_empty aliases all_aliases then
          begin
            Misc.fatal_errorf "Overlapping alias sets:@ %a" print t
          end;
          Map_to_canonical.union aliases all_aliases)
        t.aliases_of_canonical_names
        Name.Map.empty
    in
    let _all_aliases : Map_to_canonical.t =
      Const.Map.fold (fun _const aliases all_aliases ->
          Aliases_of_canonical_element.invariant aliases;
          let aliases = Aliases_of_canonical_element.all aliases in
          if Name.Map.inter_domain_is_non_empty aliases all_aliases then
          begin
            Misc.fatal_errorf "Overlapping alias sets:@ %a" print t
          end;
          Name.Map.disjoint_union aliases all_aliases)
        t.aliases_of_consts
        all_aliases_of_names
    in
    ()
  end

let empty = {
  (* CR mshinwell: Rename canonical_elements, maybe to
     aliases_to_canonical_elements. *)
  canonical_elements = Name.Map.empty;
  aliases_of_canonical_names = Name.Map.empty;
  aliases_of_consts = Const.Map.empty;
  binding_times_and_modes = Name.Map.empty;
}

type canonical =
  | Is_canonical
  | Alias_of_canonical of {
      canonical_element : Simple.t;
      coercion_to_canonical : coercion_to_canonical;
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
          Coercion.compose_exn
            coercion_from_element_to_bare_element
            ~then_:coercion_from_bare_element_to_canonical
        in
        if Flambda_features.check_invariants () then begin
          assert (not (Simple.equal element canonical_element))
        end;
        Alias_of_canonical
          { canonical_element;
            coercion_to_canonical = coercion_from_element_to_canonical; })

let get_aliases_of_canonical_element t ~canonical_element =
  assert (not (Simple.has_coercion canonical_element));
  let name name ~coercion:_ =
    Name.Map.find name t.aliases_of_canonical_names
  in
  let const const =
    Const.Map.find const t.aliases_of_consts
  in
  match Simple.pattern_match canonical_element ~name ~const with
  | exception Not_found -> Aliases_of_canonical_element.empty
  | aliases -> aliases

(*
   before
   ~~~~~~
   canonical_element <--[coercion_ce_0]-- ce_0
     ^ ^--[...]-- ...
      \--[coercion_ce_m]-- ce_m
   to_be_demoted <--[coercion_tbd_0]-- tbd_0
     ^ ^--[...]-- ...
      \--[coercion_tbd_n]-- tbd_n

   i.e.

   canonical_elements[ce_0] = (canonical_element, coercion_ce_0)
   ...
   canonical_elements[ce_m] = (canonical_element, coercion_ce_m)
   canonical_elements[tbd_0] = (to_be_demoted, coercion_tbd_0)
   ...
   canonical_elements[tbd_n] = (to_be_demoted, coercion_tbd_n)


   after
   ~~~~~
   canonical_element <--[coercion_ce_0]-- ce_0
     ^ ^ ^ ^ ^ ^--[...]-- ...
     | | | |  \--[coercion_ce_m]-- ce_m
     | | | \--[coercion_to_canonical]-- to_be_demoted
     | | \--[compose(coercion_tbd_0, coercion_to_canonical)]-- tbd_0
     | \--[...]-- ...
     \--[compose(coercion_tbd_n, coercion_to_canonical)]-- tbd_n

   i.e.

   canonical_elements[ce_0] = (canonical_element, coercion_ce_0)
   ...
   canonical_elements[ce_m] = (canonical_element, coercion_ce_m)
   canonical_elements[to_be_demoted] = (canonical_element, coercion_to_canonical)
   canonical_elements[tbd_0] = (canonical_element, compose(coercion_tbd_0, coercion_to_canonical))
   ...
   canonical_elements[tbd_n] = (canonical_element, compose(coercion_tbd_n, coercion_to_canonical))

*)
let add_alias_between_canonical_elements t ~canonical_element
      ~coercion_to_canonical:coercion_to_canonical ~to_be_demoted =
  if Simple.equal canonical_element to_be_demoted then begin
    if Coercion.is_id coercion_to_canonical then begin
      t
    end else
      Misc.fatal_errorf
        "Cannot add an alias of %a@ to itself with a non-identity coercion@ %a"
        Simple.print canonical_element
        Coercion.print coercion_to_canonical
  end else
    let name_to_be_demoted =
      assert (not (Simple.has_coercion to_be_demoted));
      Simple.pattern_match to_be_demoted
        ~const:(fun c ->
          Misc.fatal_errorf
            "Can't demote const %a@ (while adding alias to@ %a)"
          Const.print c
          Simple.print canonical_element)
        ~name:(fun name ~coercion:_ -> name)
    in
    let aliases_of_to_be_demoted =
      get_aliases_of_canonical_element t ~canonical_element:to_be_demoted
    in
    if Flambda_features.check_invariants () then begin
      Simple.pattern_match canonical_element
        ~const:(fun _ -> ())
        ~name:(fun canonical_element ~coercion ->
          assert (Coercion.is_id coercion);
          assert (not (Aliases_of_canonical_element.mem
            aliases_of_to_be_demoted canonical_element)))
    end;
    let canonical_elements =
      t.canonical_elements
      |> Name.Map.fold (fun alias coercion_to_to_be_demoted canonical_elements ->
        let coercion_to_canonical =
          Coercion.compose_exn coercion_to_to_be_demoted ~then_:coercion_to_canonical
        in
        Name.Map.add alias (canonical_element, coercion_to_canonical) canonical_elements)
        (Aliases_of_canonical_element.all aliases_of_to_be_demoted)
      |> Name.Map.add name_to_be_demoted (canonical_element, coercion_to_canonical)
    in
    let aliases_of_canonical_element =
      get_aliases_of_canonical_element t ~canonical_element
    in
    if Flambda_features.check_invariants () then begin
      assert (not (Aliases_of_canonical_element.mem
        aliases_of_canonical_element name_to_be_demoted));
      assert (Aliases_of_canonical_element.is_empty (
        Aliases_of_canonical_element.inter
          aliases_of_canonical_element aliases_of_to_be_demoted))
    end;
    let aliases =
      (* CR lmaurer: Consider adding a combination [union] and [compose] to
         [Aliases_of_canonical_element] to save a map traversal here (a single
         single call to [Name.Map.merge] could implement both operations). *)
      Aliases_of_canonical_element.add
        (Aliases_of_canonical_element.union
           (Aliases_of_canonical_element.compose aliases_of_to_be_demoted ~then_:coercion_to_canonical)
           aliases_of_canonical_element)
        name_to_be_demoted
        ~coercion_to_canonical
        (name_mode_unscoped t to_be_demoted)
    in
    let aliases_of_canonical_names =
      Name.Map.remove name_to_be_demoted t.aliases_of_canonical_names
    in
    let aliases_of_canonical_names, aliases_of_consts =
      assert (not (Simple.has_coercion canonical_element));
      Simple.pattern_match canonical_element
        ~name:(fun name ~coercion:_ ->
          Name.Map.add (* replace *) name aliases aliases_of_canonical_names,
          t.aliases_of_consts)
        ~const:(fun const ->
          aliases_of_canonical_names,
          Const.Map.add (* replace *) const aliases t.aliases_of_consts)
    in
    let res =
    { canonical_elements;
      aliases_of_canonical_names;
      aliases_of_consts;
      binding_times_and_modes = t.binding_times_and_modes;
    } in
    invariant res;
    res

type to_be_demoted = Demote_canonical_element1 | Demote_canonical_element2

let choose_canonical_element_to_be_demoted t ~canonical_element1
      ~canonical_element2 =
  if defined_earlier t canonical_element1 ~than:canonical_element2
  then Demote_canonical_element2 else Demote_canonical_element1

(* CR mshinwell: add submodule *)
type add_result = {
  t : t;
  canonical_element : Simple.t;
  alias_of_demoted_element : Simple.t;
}

let invariant_add_result
      ~original_t { canonical_element; alias_of_demoted_element; t; } =
  if Flambda_features.check_invariants () then begin
    invariant t;
    if not (defined_earlier t canonical_element ~than:alias_of_demoted_element) then begin
      Misc.fatal_errorf "Canonical element %a should be defined earlier \
          than %a after alias addition.@ Original alias tracker:@ %a@ \
          Resulting alias tracker:@ %a"
        Simple.print canonical_element
        Simple.print alias_of_demoted_element
        print original_t
        print t
    end;
    match canonical t alias_of_demoted_element with
    | Is_canonical ->
        Misc.fatal_errorf "Alias %a must not be must not be canonical \
            anymore.@ \
            Original alias tracker:@ %a@ \
            Resulting alias tracker:@ %a"
          Simple.print alias_of_demoted_element
          print original_t
          print t
    | Alias_of_canonical _ -> ()
  end

let add_alias t ~element1 ~coercion_from_element2_to_element1 ~element2 =
  assert (not (Simple.has_coercion element1));
  assert (not (Simple.has_coercion element2));
  let add ~canonical_element1 ~canonical_element2
        ~coercion_from_element1_to_canonical_element1
        ~coercion_from_element2_to_canonical_element2
        ~coercion_from_canonical_element2_to_canonical_element1 =
    assert (not (Simple.has_coercion canonical_element1));
    assert (not (Simple.has_coercion canonical_element2));
    if Simple.equal canonical_element1 canonical_element2
    then
      let canonical_element = canonical_element1 in
      (* We don't have to change anything: since [element1] and [element2] have
         the same canonical element, they must already be aliases. But what to
         return? According to the contract for [add],
         [alias_of_demoted_element] must not be canonical and must equal either
         [element1] or [element2] (before the coercion is updated). Thus we must
         choose whichever of [element1] and [element2] is not canonical. (They
         cannot both be canonical: if [element1] is canonical then it's equal to
         [canonical_element], and the same goes for [element2], but they can't
         both be equal to [canonical_element] since we assume in [add] that
         they're different. *)
      (* CR lmaurer: These elaborate postconditions are there to avoid breaking
         [Typing_env.add_equations]. It would be better to decouple these
         functions. Per discussions with poechsel and vlaviron, the
         information that [Typing_env] is after (besides the updated [Aliases.t])
         is really:

         1. What aliases need to be updated?
         2. What do those aliases point to now?

         Currently #1 is always exactly one element, but it could be zero in
         this case since nothing needs to be updated.

         So the new [add_result] could be something like:
         {[
           type add_result = {
             t : t;
             updated_aliases : Name.t list;
             new_canonical_element : Simple.t;
           }
         ]}

         (It's not actually necessary that [new_canonical_element] is canonical
         as far as [Typing_env] is concerned, but I think this is easier
         to explain than "representative_of_new_alias_class" or some such.) *)
      let alias_of_demoted_element =
        (* This needs to return a proper alias of canonical_element, so apply
           the respective coercion *)
        if Simple.equal (Simple.without_coercion element1) canonical_element then
          let coercion_from_element2_to_canonical_element =
            (* Since canonical_element = canonical_element1 = canonical_element2 *)
            coercion_from_element2_to_canonical_element2
          in
          Simple.with_coercion element2
            coercion_from_element2_to_canonical_element
        else
          let coercion_from_element1_to_canonical_element =
            (* Since canonical_element = canonical_element1 *)
            coercion_from_element1_to_canonical_element1
          in
          Simple.with_coercion element1
            coercion_from_element1_to_canonical_element
      in
      { t; canonical_element; alias_of_demoted_element; }
    else
      let canonical_element, demoted_canonical, alias_of_demoted_element,
          coercion_from_demoted_canonical_to_canonical,
          coercion_from_demoted_alias_to_demoted_canonical =
        let which_element =
          choose_canonical_element_to_be_demoted t
            ~canonical_element1 ~canonical_element2
        in
        match which_element with
        | Demote_canonical_element1 ->
          let coercion_from_canonical_element1_to_canonical_element2 =
            Coercion.inverse
              coercion_from_canonical_element2_to_canonical_element1
          in
          canonical_element2, canonical_element1, element1,
          coercion_from_canonical_element1_to_canonical_element2,
          coercion_from_element1_to_canonical_element1
        | Demote_canonical_element2 ->
          canonical_element1, canonical_element2, element2,
          coercion_from_canonical_element2_to_canonical_element1,
          coercion_from_element2_to_canonical_element2
      in
      let t =
        add_alias_between_canonical_elements
          t
          ~canonical_element
          ~coercion_to_canonical:coercion_from_demoted_canonical_to_canonical
          ~to_be_demoted:demoted_canonical
      in
      let coercion_from_demoted_alias_to_canonical =
        Coercion.compose_exn
          coercion_from_demoted_alias_to_demoted_canonical
          ~then_:coercion_from_demoted_canonical_to_canonical
      in
      let alias_of_demoted_element =
        Simple.with_coercion alias_of_demoted_element
          coercion_from_demoted_alias_to_canonical
      in
      { t;
        canonical_element;
        alias_of_demoted_element;
      }
  in
  match canonical t element1, canonical t element2 with
  | Is_canonical, Is_canonical ->
    let canonical_element1 = element1 in
    let canonical_element2 = element2 in
    let coercion_from_element1_to_canonical_element1 = Coercion.id in
    let coercion_from_element2_to_canonical_element2 = Coercion.id in
    let coercion_from_canonical_element2_to_canonical_element1 =
      coercion_from_element2_to_element1
    in
    add ~canonical_element1 ~canonical_element2
      ~coercion_from_element1_to_canonical_element1
      ~coercion_from_element2_to_canonical_element2
      ~coercion_from_canonical_element2_to_canonical_element1
  | Alias_of_canonical {
      canonical_element = canonical_element1;
      coercion_to_canonical = coercion_from_element1_to_canonical_element1;
    },
    Is_canonical ->
    let canonical_element2 = element2 in
    let coercion_from_element2_to_canonical_element2 = Coercion.id in
    (* element1 <--[c]-- canonical_element2=element2
       +
       canonical_element1 <--[c1] element1
       ~>
       canonical_element1 <--[c1 << c]-- canonical_element2 *)
    let coercion_from_canonical_element2_to_canonical_element1 =
      Coercion.compose_exn coercion_from_element2_to_element1
        ~then_:coercion_from_element1_to_canonical_element1
    in
    add ~canonical_element1 ~canonical_element2
      ~coercion_from_element1_to_canonical_element1
      ~coercion_from_element2_to_canonical_element2
      ~coercion_from_canonical_element2_to_canonical_element1
  | Is_canonical,
    Alias_of_canonical {
      canonical_element = canonical_element2;
      coercion_to_canonical = coercion_from_element2_to_canonical_element2;
    } ->
    let canonical_element1 = element1 in
    let coercion_from_element1_to_canonical_element1 = Coercion.id in
    let coercion_from_canonical_element2_to_canonical_element1 =
      (* canonical_element1=element1 <--[c]-- element2
         +
         canonical_element2 <--[c2]-- element2
         ~>
         element2 <--[c2^-1]-- canonical_element2
         ~>
         canonical_element1 <--[c << c2^-1]-- canonical_element2
      *)
      Coercion.compose_exn
        (Coercion.inverse coercion_from_element2_to_canonical_element2)
        ~then_:coercion_from_element2_to_element1
    in
    add ~canonical_element1 ~canonical_element2
      ~coercion_from_element1_to_canonical_element1
      ~coercion_from_element2_to_canonical_element2
      ~coercion_from_canonical_element2_to_canonical_element1
  | Alias_of_canonical {
      canonical_element = canonical_element1;
      coercion_to_canonical = coercion_from_element1_to_canonical_element1;
    },
    Alias_of_canonical {
      canonical_element = canonical_element2;
      coercion_to_canonical = coercion_from_element2_to_canonical_element2;
    } ->
    let coercion_from_canonical_element2_to_canonical_element1 =
      (* canonical_element1 <--[c1]-- element1
         canonical_element2 <--[c2]-- element2
         +
         element1 <--[c]-- element2
         ~>
         element2 <--[c2^-1]-- canonical_element2
         ~>
         canonical_element1 <--[c1 << c << c2^-1]-- canonical_element2
         *)
      Coercion.compose_exn
        (Coercion.inverse coercion_from_element2_to_canonical_element2)
        ~then_:(Coercion.compose_exn
          coercion_from_element2_to_element1
          ~then_:coercion_from_element1_to_canonical_element1)
    in
    add ~canonical_element1 ~canonical_element2
      ~coercion_from_element1_to_canonical_element1
      ~coercion_from_element2_to_canonical_element2
      ~coercion_from_canonical_element2_to_canonical_element1

let add t ~element1:element1_with_coercion ~binding_time_and_mode1
      ~element2:element2_with_coercion ~binding_time_and_mode2 =
  let original_t = t in
  (* element1_with_coercion <--[c1]-- element1
     +
     element2_with_coercion <--[c2]-- element2
     ~
     element1 <--[c1^-1]-- element1_with_coercion
     ~
     element1 <--[c1^-1 << c2]-- element2
   *)
  let element1 = element1_with_coercion |> Simple.without_coercion in
  let element2 = element2_with_coercion |> Simple.without_coercion in
  let coercion_from_element2_to_element1 =
    Coercion.compose_exn (Simple.coercion element2_with_coercion)
      ~then_:(Coercion.inverse (Simple.coercion element1_with_coercion))
  in
  if Flambda_features.check_invariants () then begin
    if Simple.equal element1 element2 then begin
      Misc.fatal_errorf
        "Cannot alias an element to itself: %a" Simple.print element1
    end;
    Simple.pattern_match element1
      ~name:(fun _ ~coercion:_ -> ())
      ~const:(fun const1 ->
        Simple.pattern_match element2
          ~name:(fun _ ~coercion:_ -> ())
          ~const:(fun const2 ->
            Misc.fatal_errorf
              "Cannot add alias between two consts: %a, %a"
                Const.print const1
                Const.print const2
          ));
  end;
  let add_if_name simple data map =
    Simple.pattern_match simple
      ~const:(fun _ -> map)
      ~name:(fun name ~coercion:_ -> Name.Map.add name data map)
  in
  let t =
    { t with binding_times_and_modes =
               add_if_name element1 binding_time_and_mode1
                 (add_if_name element2 binding_time_and_mode2
                    t.binding_times_and_modes);
    }
  in
  let add_result = add_alias t ~element1 ~coercion_from_element2_to_element1 ~element2 in
  if Flambda_features.check_invariants () then begin
    invariant_add_result ~original_t add_result
  end;
  add_result

let mem t element =
  Simple.pattern_match element
    ~const:(fun const ->
      Const.Map.mem const t.aliases_of_consts)
    ~name:(fun name ~coercion:_ ->
      Name.Map.mem name t.binding_times_and_modes)

  (* CR mshinwell: This needs documenting.  For the moment we allow
     relations between canonical elements that are actually incomparable
     under the name mode ordering, and check in [get_canonical_element_exn]
     accordingly.  However maybe we should never allow these situations to
     arise. *)
  (*
  let canonical_mode =
    name_mode t add_result.canonical_element
  in
  let alias_of_mode = name_mode t add_result.alias_of in
  match
    Name_mode.compare_partial_order
      canonical_mode alias_of_mode
  with
  | Some _ -> add_result
  | None ->
    Misc.fatal_errorf "Canonical %a has mode incomparable with %a in:@ %a"
      Simple.print add_result.canonical_element
      Simple.print add_result.alias_of
      print t
  *)

let get_canonical_element_exn t element elt_name_mode ~min_name_mode
      ~min_binding_time =
  let canonical_element, name_mode, coercion_from_canonical_to_element =
    match canonical t element with
    | Is_canonical ->
      Simple.without_coercion element, elt_name_mode, Simple.coercion element
    | Alias_of_canonical { canonical_element; coercion_to_canonical; } ->
      let name_mode = name_mode t canonical_element ~min_binding_time in
      canonical_element, name_mode, Coercion.inverse coercion_to_canonical
  in
  assert (not (Simple.has_coercion canonical_element));
  (*
Format.eprintf "looking for canonical for %a, candidate canonical %a, min order %a\n%!"
  Simple.print element
  Simple.print canonical_element
  Name_mode.print min_name_mode;
*)
  let find_earliest () =
    (* There used to be a shortcut that avoided consulting the aliases in the
       common case that [element] is itself canonical and has no aliases, since
       then it does not appear in [canonical_elements]. However, this shortcut
       was broken: a canonical element *with* known aliases may still not appear
       in [canonical_elements]. See tests/flambda2-aliases for a test that gave
       incorrect output (saying x/39 had no aliases). It may be worth restoring
       the shortcut, perhaps by returning more information from [canonical]. *)
    let aliases = get_aliases_of_canonical_element t ~canonical_element in
    let filter_by_scope name_mode names =
      if Name_mode.equal name_mode Name_mode.in_types then names
      else
        Name.Map.filter (fun name _coercion ->
            let binding_time_and_mode =
              Name.Map.find name t.binding_times_and_modes
            in
            let scoped_name_mode =
              Binding_time.With_name_mode.scoped_name_mode
                binding_time_and_mode
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
        Name.Map.fold (fun elt coercion ((min_elt, _min_coercion) as min_binding) ->
            if name_defined_earlier t elt ~than:min_elt
            then elt, coercion
            else min_binding)
          at_earliest_mode
          (Name.Map.min_binding at_earliest_mode)
      in
      let coercion_from_earliest_to_element =
        Coercion.compose_exn coercion_from_earliest_to_canonical
          ~then_:coercion_from_canonical_to_element
      in
      Simple.with_coercion (Simple.name earliest) coercion_from_earliest_to_element
    | None -> raise Not_found
  in
  match
    Name_mode.compare_partial_order name_mode min_name_mode
  with
  | None -> find_earliest ()
  | Some c ->
    if c >= 0 then
      Simple.with_coercion canonical_element coercion_from_canonical_to_element
    else find_earliest ()

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
      compose_map_values_exn
        alias_names_with_coercions_to_canonical
        ~then_:coercion_from_canonical_to_element
    in
    Alias_set.create_aliases_of_element
      ~element
      ~canonical_element
      ~coercion_from_canonical_to_element
      ~alias_names_with_coercions_to_element
  | Alias_of_canonical { canonical_element;
      coercion_to_canonical = coercion_from_element_to_canonical; } ->
    assert (not (Simple.has_coercion canonical_element));
    assert (not (Simple.equal
                   (Simple.without_coercion element)
                   canonical_element));
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
    if Flambda_features.check_invariants () then begin
      let element_coerced_to_canonical =
        Simple.apply_coercion_exn element coercion_from_element_to_canonical
      in
      (* These aliases are all equivalent to the canonical element, and so is
         our original [element] if we coerce it first, so the coerced form of
         [element] should be among the aliases. *)
      assert (Name.Map.exists
        (fun name coercion_from_name_to_canonical ->
          let name_coerced_to_canonical =
            Simple.apply_coercion_exn
              (Simple.name name)
              coercion_from_name_to_canonical
          in
          Simple.equal element_coerced_to_canonical name_coerced_to_canonical
        ) alias_names_with_coercions_to_canonical)
    end;
    Alias_set.create_aliases_of_element
      ~element
      ~canonical_element
      ~coercion_from_canonical_to_element
      ~alias_names_with_coercions_to_element

let all_ids_for_export { canonical_elements;
                         aliases_of_canonical_names;
                         aliases_of_consts;
                         binding_times_and_modes; } =
  let ids =
    Name.Map.fold (fun elt (canonical, coercion) ids ->
        let ids =
          Ids_for_export.union ids (Coercion.all_ids_for_export coercion)
        in
        Ids_for_export.add_name
          (Ids_for_export.add_simple ids canonical)
          elt)
      canonical_elements
      Ids_for_export.empty
  in
  let ids =
    Name.Map.fold (fun elt aliases_of ids ->
        Ids_for_export.add_name
          (Ids_for_export.union ids
            (Aliases_of_canonical_element.all_ids_for_export aliases_of))
          elt)
      aliases_of_canonical_names
      ids
  in
  let ids =
    Const.Map.fold (fun const aliases_of ids ->
        Ids_for_export.add_const
          (Ids_for_export.union ids
            (Aliases_of_canonical_element.all_ids_for_export aliases_of))
          const)
      aliases_of_consts
      ids
  in
  Name.Map.fold (fun elt (_ : Binding_time.With_name_mode.t) ids ->
      Ids_for_export.add_name ids elt)
    binding_times_and_modes
    ids

let apply_renaming
      { canonical_elements;
        aliases_of_canonical_names;
        aliases_of_consts;
        binding_times_and_modes; }
      renaming =
  let rename_name = Renaming.apply_name renaming in
  let rename_simple = Renaming.apply_simple renaming in
  let canonical_elements =
    Name.Map.fold (fun elt (canonical, coercion) acc ->
        Name.Map.add (rename_name elt)
          (rename_simple canonical,
           Coercion.apply_renaming coercion renaming)
          acc)
      canonical_elements
      Name.Map.empty
  in
  let aliases_of_canonical_names =
    Name.Map.fold (fun canonical aliases acc ->
        Name.Map.add (rename_name canonical)
          (Aliases_of_canonical_element.apply_renaming aliases renaming)
          acc)
      aliases_of_canonical_names
      Name.Map.empty
  in
  let aliases_of_consts =
    Const.Map.fold (fun const aliases ->
        Const.Map.add (Renaming.apply_const renaming const)
          (Aliases_of_canonical_element.apply_renaming aliases renaming))
      aliases_of_consts
      Const.Map.empty
  in
  let binding_times_and_modes =
    Name.Map.fold (fun name binding_time_and_mode acc ->
        Name.Map.add (rename_name name) binding_time_and_mode acc)
      binding_times_and_modes
      Name.Map.empty
  in
  { canonical_elements;
    aliases_of_canonical_names;
    aliases_of_consts;
    binding_times_and_modes;
  }

let merge t1 t2 =
  let canonical_elements =
    Name.Map.disjoint_union
      t1.canonical_elements
      t2.canonical_elements
  in
  (* Warning: we assume that the aliases in the two alias trackers are disjoint,
     but nothing stops them from sharing a canonical element. For instance, if
     multiple compilation units define aliases to the same canonical symbol,
     that symbol will be a canonical element in both of the units' alias
     trackers, and thus their [aliases_of_canonical_names] will have a key in
     common. *)
  let merge_aliases _canonical aliases1 aliases2 =
    Some (Aliases_of_canonical_element.merge aliases1 aliases2)
  in
  let aliases_of_canonical_names =
    Name.Map.union merge_aliases
      t1.aliases_of_canonical_names
      t2.aliases_of_canonical_names
  in
  let aliases_of_consts =
    Const.Map.union merge_aliases
      t1.aliases_of_consts
      t2.aliases_of_consts
  in

  let symbol_data =
    Binding_time.With_name_mode.create
      Binding_time.symbols
      Name_mode.normal
  in
  let binding_times_and_modes =
    Name.Map.union (fun name data1 data2 ->
        Name.pattern_match name
          ~var:(fun var ->
            (* TODO: filter variables on export and restore fatal_error *)
            if Binding_time.(equal (With_name_mode.binding_time data1)
                               imported_variables)
            then Some data2
            else if Binding_time.(equal (With_name_mode.binding_time data2)
                               imported_variables)
            then Some data1
            else
              Misc.fatal_errorf
                "Variable %a is present in multiple environments"
                Variable.print var)
          ~symbol:(fun _sym ->
            assert (Binding_time.With_name_mode.equal data1 symbol_data);
            assert (Binding_time.With_name_mode.equal data2 symbol_data);
            Some data1))
      t1.binding_times_and_modes
      t2.binding_times_and_modes
  in
  { canonical_elements;
    aliases_of_canonical_names;
    aliases_of_consts;
    binding_times_and_modes;
  }

let get_canonical_ignoring_name_mode t name =
  let elt = Simple.name name in
  match canonical t elt with
  | Is_canonical ->
    elt
  | Alias_of_canonical { canonical_element; coercion_to_canonical } ->
    let coercion_from_canonical = Coercion.inverse coercion_to_canonical in
    Simple.apply_coercion_exn canonical_element coercion_from_canonical

let clean_for_export
      { canonical_elements;
        aliases_of_canonical_names = _;
        aliases_of_consts = _;
        binding_times_and_modes = _; } =
  (* CR vlaviron: We'd like to remove unreachable entries at some point. *)
  (* From this point, the structure will only be used for looking up
     names that are defined in this compilation unit.
     No new aliases will be added, name modes have become irrelevant
     (Typing_env takes care of setting the mode to In_types for imported
     variables), so all we really need to keep is the [canonical_elements] map,
     and we further restrict the map to names that are not imported. *)
  let canonical_elements =
    Name.Map.filter (fun name _canonical ->
        not (Name.is_imported name))
      canonical_elements
  in
  { canonical_elements;
    aliases_of_canonical_names = Name.Map.empty;
    aliases_of_consts = Const.Map.empty;
    binding_times_and_modes = Name.Map.empty;
  }
