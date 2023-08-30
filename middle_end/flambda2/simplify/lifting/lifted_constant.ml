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

module DE = Downwards_env
module T = Flambda2_types

module Definition = struct
  type descr =
    | Code of Code_id.t
    | Set_of_closures of
        { denv : Downwards_env.t;
          closure_symbols_with_types :
            (Symbol.t * Flambda2_types.t) Function_slot.Lmap.t;
          symbol_projections : Symbol_projection.t Variable.Map.t
        }
    | Block_like of
        { symbol : Symbol.t;
          denv : Downwards_env.t;
          ty : Flambda2_types.t;
          symbol_projections : Symbol_projection.t Variable.Map.t
        }

  type t =
    { descr : descr;
      defining_expr : Rebuilt_static_const.t
    }

  let binds_symbol t sym =
    match t.descr with
    | Code _ -> false
    | Set_of_closures { closure_symbols_with_types; _ } ->
      Function_slot.Lmap.exists
        (fun _ (sym', _) -> Symbol.equal sym sym')
        closure_symbols_with_types
    | Block_like { symbol; _ } -> Symbol.equal sym symbol

  let free_names t =
    match t.descr with
    | Code _ -> Rebuilt_static_const.free_names t.defining_expr
    | Set_of_closures { symbol_projections; _ }
    | Block_like { symbol_projections; _ } ->
      (* The symbols mentioned in any symbol projections must be counted as free
         names, so that the definition doesn't get placed too high in the
         code. *)
      Variable.Map.fold
        (fun _var proj free_names ->
          Name_occurrences.add_symbol free_names
            (Symbol_projection.symbol proj)
            Name_mode.normal)
        symbol_projections
        (Rebuilt_static_const.free_names t.defining_expr)

  let print_descr ppf descr =
    match descr with
    | Code code_id -> Code_id.print ppf code_id
    | Set_of_closures { closure_symbols_with_types; _ } ->
      let symbols =
        Function_slot.Lmap.data closure_symbols_with_types |> List.map fst
      in
      Format.fprintf ppf "@[<hov 1>(%a)@]"
        (Format.pp_print_list ~pp_sep:Format.pp_print_space Symbol.print)
        symbols
    | Block_like { symbol; _ } -> Symbol.print ppf symbol

  let [@ocamlformat "disable"] print ppf { descr; defining_expr; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(descr@ %a)@]@ \
        @[<hov 1>(defining_expr@ %a)@]\
        @]"
      print_descr descr
      Rebuilt_static_const.print defining_expr

  let descr t = t.descr

  let defining_expr t = t.defining_expr

  let symbol_projections t =
    match t.descr with
    | Code _ -> Variable.Map.empty
    | Set_of_closures { symbol_projections; _ }
    | Block_like { symbol_projections; _ } ->
      symbol_projections

  let code code_id defining_expr = { descr = Code code_id; defining_expr }

  let set_of_closures denv ~closure_symbols_with_types ~symbol_projections
      defining_expr =
    { descr =
        Set_of_closures { denv; closure_symbols_with_types; symbol_projections };
      defining_expr
    }

  let block_like denv symbol ty ~symbol_projections defining_expr =
    { descr = Block_like { symbol; denv; ty; symbol_projections };
      defining_expr
    }

  let denv t =
    match t.descr with
    | Code _ -> None
    | Set_of_closures { denv; _ } | Block_like { denv; _ } -> Some denv

  let bound_static_pattern t =
    let module P = Bound_static.Pattern in
    match t.descr with
    | Code code_id -> P.code code_id
    | Set_of_closures { closure_symbols_with_types; _ } ->
      P.set_of_closures (Function_slot.Lmap.map fst closure_symbols_with_types)
    | Block_like { symbol; _ } -> P.block_like symbol

  let bound_static t = Bound_static.create [bound_static_pattern t]

  let types_of_symbols t =
    match t.descr with
    | Code _ -> Symbol.Map.empty
    | Set_of_closures { denv; closure_symbols_with_types; _ } ->
      Function_slot.Lmap.fold
        (fun _function_slot (symbol, ty) types_of_symbols ->
          Symbol.Map.add symbol (denv, ty) types_of_symbols)
        closure_symbols_with_types Symbol.Map.empty
    | Block_like { symbol; denv; ty; _ } ->
      Symbol.Map.singleton symbol (denv, ty)

  let simplify_projections t denv =
    let typing_env = DE.typing_env denv in
    let symbol_projections, vars_to_replace =
      Variable.Map.fold
        (fun proj_var projection (symbol_projections, vars_to_replace) ->
          let keep () =
            ( Variable.Map.add proj_var projection symbol_projections,
              vars_to_replace )
          in
          let replace simple =
            symbol_projections, Variable.Map.add proj_var simple vars_to_replace
          in
          if T.Typing_env.mem ~min_name_mode:Name_mode.normal typing_env
               (Name.var proj_var)
          then (* No need to try to replace it, it is already bound *)
            keep ()
          else
            let symbol = Symbol_projection.symbol projection in
            let ty =
              T.alias_type_of Flambda_kind.value (Simple.symbol symbol)
            in
            let meet_shortcut =
              match Symbol_projection.projection projection with
              | Block_load { index } ->
                let field_kind =
                  Symbol_projection.kind projection
                  |> Flambda_kind.With_subkind.kind
                in
                T.meet_block_field_simple typing_env
                  ~min_name_mode:Name_mode.normal ~field_kind ty index
              | Project_value_slot { project_from = _; value_slot } ->
                T.meet_project_value_slot_simple typing_env
                  ~min_name_mode:Name_mode.normal ty value_slot
            in
            match meet_shortcut with
            | Known_result simple -> replace simple
            | Need_meet -> keep ()
            | Invalid ->
              (* Propagating Invalid would be too cumbersome *)
              keep ())
        (symbol_projections t)
        (Variable.Map.empty, Variable.Map.empty)
    in
    if Variable.Map.is_empty vars_to_replace
    then t
    else
      match Rebuilt_static_const.to_const t.defining_expr with
      | None -> t
      | Some (Code _ | Deleted_code) -> t
      | Some (Static_const const) ->
        let defining_expr = Static_const.replace_vars const ~vars_to_replace in
        let defining_expr =
          Rebuilt_static_const.from_const
            (Flambda.Static_const_or_code.create_static_const defining_expr)
        in
        let descr =
          match t.descr with
          | Code _ -> t.descr
          | Set_of_closures
              { denv; closure_symbols_with_types; symbol_projections = _ } ->
            Set_of_closures
              { denv; closure_symbols_with_types; symbol_projections }
          | Block_like { symbol; denv; ty; symbol_projections = _ } ->
            Block_like { symbol; denv; ty; symbol_projections }
        in
        { descr; defining_expr }
end

type t =
  { definitions : Definition.t list;
    bound_static : Bound_static.t;
    defining_exprs : Rebuilt_static_const.Group.t;
    symbol_projections : Symbol_projection.t Variable.Map.t;
    is_fully_static : bool
  }

let definitions t = t.definitions

let symbol_projections t = t.symbol_projections

let free_names_of_defining_exprs t =
  Rebuilt_static_const.Group.free_names t.defining_exprs

let is_fully_static t = t.is_fully_static

let [@ocamlformat "disable"] print ppf
      { definitions; bound_static = _; defining_exprs = _;
        is_fully_static = _; symbol_projections = _; } =
  Format.fprintf ppf "@[<hov 1>(%a)@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Definition.print)
    definitions

let compute_bound_static definitions =
  ListLabels.map definitions ~f:Definition.bound_static_pattern
  |> Bound_static.create

let compute_defining_exprs definitions =
  ListLabels.map definitions ~f:Definition.defining_expr
  |> Rebuilt_static_const.Group.create

let create_block_like symbol ~symbol_projections defining_expr denv ty =
  if not (Rebuilt_static_const.is_block defining_expr)
  then
    Misc.fatal_errorf "Defining expression must be a block:@ %a"
      Rebuilt_static_const.print defining_expr;
  let definition =
    Definition.block_like denv symbol ty ~symbol_projections defining_expr
  in
  let definitions = [definition] in
  { definitions;
    bound_static = compute_bound_static definitions;
    defining_exprs = compute_defining_exprs definitions;
    is_fully_static = Rebuilt_static_const.is_fully_static defining_expr;
    symbol_projections = Definition.symbol_projections definition
  }

let create_set_of_closures denv ~closure_symbols_with_types ~symbol_projections
    defining_expr =
  if not (Rebuilt_static_const.is_set_of_closures defining_expr)
  then
    Misc.fatal_errorf "Defining expression must be a set of closures:@ %a"
      Rebuilt_static_const.print defining_expr;
  let definition =
    Definition.set_of_closures denv ~closure_symbols_with_types
      ~symbol_projections defining_expr
  in
  let definitions = [definition] in
  { definitions;
    bound_static = compute_bound_static definitions;
    defining_exprs = compute_defining_exprs definitions;
    is_fully_static = Rebuilt_static_const.is_fully_static defining_expr;
    symbol_projections = Definition.symbol_projections definition
  }

let create_code code_id defining_expr =
  if not (Rebuilt_static_const.is_code defining_expr)
  then
    Misc.fatal_errorf "Defining expression must be code:@ %a"
      Rebuilt_static_const.print defining_expr;
  let definition = Definition.code code_id defining_expr in
  let definitions = [definition] in
  { definitions;
    bound_static = compute_bound_static definitions;
    defining_exprs = compute_defining_exprs definitions;
    is_fully_static = Rebuilt_static_const.is_fully_static defining_expr;
    symbol_projections = Definition.symbol_projections definition
  }

let create_definition definition =
  let definitions = [definition] in
  { definitions;
    bound_static = compute_bound_static definitions;
    defining_exprs = compute_defining_exprs definitions;
    is_fully_static =
      Rebuilt_static_const.is_fully_static (Definition.defining_expr definition);
    symbol_projections = Definition.symbol_projections definition
  }

let concat ts =
  let definitions =
    List.fold_left (fun definitions t -> t.definitions @ definitions) [] ts
  in
  let bound_static =
    List.fold_left
      (fun bound_static t -> Bound_static.concat t.bound_static bound_static)
      Bound_static.empty ts
  in
  let defining_exprs =
    List.fold_left
      (fun defining_exprs t ->
        Rebuilt_static_const.Group.concat t.defining_exprs defining_exprs)
      Rebuilt_static_const.Group.empty ts
  in
  let is_fully_static =
    List.fold_left
      (fun is_fully_static t -> t.is_fully_static && is_fully_static)
      true ts
  in
  let symbol_projections =
    List.fold_left
      (fun symbol_projections t ->
        Variable.Map.disjoint_union ~eq:Symbol_projection.equal
          t.symbol_projections symbol_projections)
      Variable.Map.empty ts
  in
  { definitions;
    bound_static;
    defining_exprs;
    is_fully_static;
    symbol_projections
  }

let defining_exprs t =
  Rebuilt_static_const.Group.create
    (List.map Definition.defining_expr t.definitions)

let bound_static t =
  Bound_static.create (List.map Definition.bound_static_pattern t.definitions)

let types_of_symbols t =
  ListLabels.fold_left t.definitions ~init:Symbol.Map.empty
    ~f:(fun types_of_symbols definition ->
      Symbol.Map.disjoint_union
        (Definition.types_of_symbols definition)
        types_of_symbols)

let all_defined_symbols t = Symbol.Map.keys (types_of_symbols t)

let apply_projection t proj =
  let symbol = Symbol_projection.symbol proj in
  let matching_defining_exprs =
    ListLabels.filter_map t.definitions ~f:(fun definition ->
        if Definition.binds_symbol definition symbol
        then Some definition
        else None)
  in
  match matching_defining_exprs with
  | [matched_defining_expr] -> (
    let denv, ty =
      Symbol.Map.find symbol (Definition.types_of_symbols matched_defining_expr)
    in
    let typing_env = DE.typing_env denv in
    let meet_shortcut =
      match Symbol_projection.projection proj with
      | Block_load { index } ->
        let field_kind =
          Symbol_projection.kind proj |> Flambda_kind.With_subkind.kind
        in
        T.meet_block_field_simple typing_env ~min_name_mode:Name_mode.normal
          ~field_kind ty index
      | Project_value_slot { project_from = _; value_slot } ->
        T.meet_project_value_slot_simple typing_env
          ~min_name_mode:Name_mode.normal ty value_slot
    in
    match meet_shortcut with
    | Known_result simple -> Some simple
    | Need_meet ->
      (* [Simplify_named], which calls this function, requires [Some] to be
         returned if the projection is from a symbol defined in the same
         recursive group (see the comment in that module). As such, if the
         projection via the types fails, we currently stop. It seems very
         unlikely that this will happen; we can reconsider in the future if
         necessary. *)
      Misc.fatal_errorf
        "Symbol projection@ %a@ produced [Need_meet]:@ type is@ %a@ in env:@ %a"
        Symbol_projection.print proj T.print ty T.Typing_env.print typing_env
    | Invalid ->
      Misc.fatal_errorf
        "Symbol projection@ %a@ produced [Invalid],@ environment is:@ %a"
        Symbol_projection.print proj T.Typing_env.print typing_env)
  | [] -> None
  | _ :: _ :: _ ->
    Misc.fatal_errorf
      "Symbol projection@ %a@ matches more than one constant in:@ %a"
      Symbol_projection.print proj print t

let simplify_projections t denv =
  concat
    (List.map
       (fun definition ->
         create_definition (Definition.simplify_projections definition denv))
       t.definitions)
