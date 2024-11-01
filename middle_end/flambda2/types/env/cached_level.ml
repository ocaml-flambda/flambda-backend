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

type t =
  { names_to_types :
      (Type_grammar.t * Binding_time.With_name_mode.t) Name.Map.t;
    aliases : Aliases.t;
    symbol_projections : Symbol_projection.t Variable.Map.t
  }

let print_kind_and_mode ~min_binding_time ppf (ty, binding_time_and_mode) =
  let kind = Type_grammar.kind ty in
  let mode =
    Binding_time.With_name_mode.scoped_name_mode binding_time_and_mode
      ~min_binding_time
  in
  Format.fprintf ppf ":: %a %a" Flambda_kind.print kind Name_mode.print mode

let print_name_modes ~restrict_to ~min_binding_time ppf t =
  Name.Map.print
    (print_kind_and_mode ~min_binding_time)
    ppf
    (Name.Map.filter
       (fun name _ -> Name.Set.mem name restrict_to)
       t.names_to_types)

let empty =
  { names_to_types = Name.Map.empty;
    aliases = Aliases.empty;
    symbol_projections = Variable.Map.empty
  }

let names_to_types t = t.names_to_types

let aliases t = t.aliases

let symbol_projections t = t.symbol_projections

let add_or_replace_binding t (name : Name.t) ty binding_time name_mode =
  let binding_time_and_mode =
    Binding_time.With_name_mode.create binding_time name_mode
  in
  let names_to_types =
    Name.Map.add name (ty, binding_time_and_mode) t.names_to_types
  in
  { names_to_types;
    aliases = t.aliases;
    symbol_projections = t.symbol_projections
  }

let replace_variable_binding t var ty =
  let names_to_types =
    Name.Map.replace (Name.var var)
      (function _old_ty, binding_time_and_mode -> ty, binding_time_and_mode)
      t.names_to_types
  in
  { names_to_types;
    aliases = t.aliases;
    symbol_projections = t.symbol_projections
  }

let with_aliases t ~aliases = { t with aliases }

let add_symbol_projection t var proj =
  let symbol_projections = Variable.Map.add var proj t.symbol_projections in
  { t with symbol_projections }

let find_symbol_projection t var =
  match Variable.Map.find var t.symbol_projections with
  | exception Not_found -> None
  | proj -> Some proj

let clean_for_export t ~reachable_names =
  (* Names coming from other compilation units or unreachable are removed *)
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  let names_to_types =
    Name.Map.filter_map
      (fun name (ty, binding_time_and_mode) ->
        if Name_occurrences.mem_name reachable_names name
           && Compilation_unit.equal
                (Name.compilation_unit name)
                current_compilation_unit
        then (
          let binding_time_and_mode =
            if Name.is_var name
            then Binding_time.With_name_mode.imported_variables
            else binding_time_and_mode
          in
          (match Type_grammar.get_alias_opt ty with
          | None -> ()
          | Some alias ->
            Misc.fatal_errorf "Remaining alias after cleanup: %a -> %a@."
              Name.print name Simple.print alias);
          Some (ty, binding_time_and_mode))
        else None)
      t.names_to_types
  in
  let aliases = Aliases.empty in
  { t with names_to_types; aliases }

let apply_renaming { names_to_types; aliases; symbol_projections } renaming =
  let names_to_types =
    Name.Map.fold
      (fun name (ty, binding_time_and_mode) acc ->
        Name.Map.add
          (Renaming.apply_name renaming name)
          (Type_grammar.apply_renaming ty renaming, binding_time_and_mode)
          acc)
      names_to_types Name.Map.empty
  in
  let aliases = Aliases.apply_renaming aliases renaming in
  let symbol_projections =
    Variable.Map.fold
      (fun var proj acc ->
        Variable.Map.add
          (Renaming.apply_variable renaming var)
          (Symbol_projection.apply_renaming proj renaming)
          acc)
      symbol_projections Variable.Map.empty
  in
  { names_to_types; aliases; symbol_projections }

let merge t1 t2 =
  let names_to_types =
    Name.Map.disjoint_union t1.names_to_types t2.names_to_types
  in
  let aliases = Aliases.empty in
  let symbol_projections =
    Variable.Map.union
      (fun var proj1 proj2 ->
        if Symbol_projection.equal proj1 proj2
        then Some proj1
        else
          Misc.fatal_errorf
            "Cannot merge symbol projections for %a:@ %a@ and@ %a"
            Variable.print var Symbol_projection.print proj1
            Symbol_projection.print proj2)
      t1.symbol_projections t2.symbol_projections
  in
  { names_to_types; aliases; symbol_projections }

let canonicalise t simple =
  Simple.pattern_match simple
    ~const:(fun _ -> simple)
    ~name:(fun name ~coercion ->
      Simple.apply_coercion_exn
        (Aliases.get_canonical_ignoring_name_mode t.aliases name)
        coercion)

let remove_unused_value_slots_and_shortcut_aliases
    ({ names_to_types; aliases; symbol_projections } as t) ~used_value_slots =
  let canonicalise = canonicalise t in
  let names_to_types =
    Name.Map.map_sharing
      (fun ((ty, binding_time_and_mode) as info) ->
        let ty' =
          Type_grammar.remove_unused_value_slots_and_shortcut_aliases ty
            ~used_value_slots ~canonicalise
        in
        if ty == ty' then info else ty', binding_time_and_mode)
      names_to_types
  in
  { names_to_types; aliases; symbol_projections }

let free_function_slots_and_value_slots
    { names_to_types; aliases = _; symbol_projections } =
  let from_projections =
    Variable.Map.fold
      (fun _var proj free_names ->
        Name_occurrences.union free_names
          (Name_occurrences.restrict_to_value_slots_and_function_slots
             (Symbol_projection.free_names proj)))
      symbol_projections Name_occurrences.empty
  in
  Name.Map.fold
    (fun _name (ty, _binding_time) free_names ->
      let free_names_of_ty = Type_grammar.free_names ty in
      Name_occurrences.union free_names
        (Name_occurrences.restrict_to_value_slots_and_function_slots
           free_names_of_ty))
    names_to_types from_projections

let ids_for_export t =
  if not (Aliases.is_empty t.aliases)
  then
    Misc.fatal_error
      "Aliases structure must be empty for export; did you forget to call \
       [clean_for_export]?";
  Name.Map.fold
    (fun name (typ, _binding_time_and_mode) ids ->
      Ids_for_export.add_name
        (Ids_for_export.union ids (Type_grammar.ids_for_export typ))
        name)
    (names_to_types t) Ids_for_export.empty
