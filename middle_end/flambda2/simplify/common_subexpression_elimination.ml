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

(* [Simplify_import] cannot be used owing to a circular dependency. *)
module EA = Continuation_extra_params_and_args.Extra_arg
module EP = Flambda_primitive.Eligible_for_cse
module EPA = Continuation_extra_params_and_args
module K = Flambda_kind
module BP = Bound_parameter
module NM = Name_mode
module P = Flambda_primitive
module RI = Apply_cont_rewrite_id
module T = Flambda2_types
module TE = Flambda2_types.Typing_env
module List = ListLabels

type t =
  { by_scope : Simple.t EP.Map.t Scope.Map.t;
    combined : Simple.t EP.Map.t
  }

let [@ocamlformat "disable"] print ppf { by_scope; combined; } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(by_scope@ %a)@]@ \
      @[<hov 1>(combined@ %a)@]\
      @]"
    (Scope.Map.print (EP.Map.print Simple.print)) by_scope
    (EP.Map.print Simple.print) combined

let empty = { by_scope = Scope.Map.empty; combined = EP.Map.empty }

let add t prim ~bound_to scope =
  match EP.Map.find prim t.combined with
  | exception Not_found ->
    let level =
      match Scope.Map.find scope t.by_scope with
      | exception Not_found -> EP.Map.singleton prim bound_to
      | level -> EP.Map.add prim bound_to level
    in
    let by_scope = Scope.Map.add (* replace *) scope level t.by_scope in
    let combined = EP.Map.add prim bound_to t.combined in
    { by_scope; combined }
  | _bound_to -> t

let find t prim = EP.Map.find_opt prim t.combined

module Rhs_kind : sig
  type t =
    | Needs_extra_binding of { bound_to : Simple.t }
    | Rhs_in_scope of { bound_to : Simple.t }

  val bound_to : t -> Simple.t

  include Container_types.S with type t := t
end = struct
  type t =
    | Needs_extra_binding of { bound_to : Simple.t }
    | Rhs_in_scope of { bound_to : Simple.t }

  let bound_to t =
    match t with
    | Needs_extra_binding { bound_to } | Rhs_in_scope { bound_to } -> bound_to

  include Container_types.Make (struct
    type nonrec t = t

    let [@ocamlformat "disable"] print ppf t =
      match t with
      | Needs_extra_binding { bound_to; } ->
        Format.fprintf ppf "@[<hov 1>(Needs_extra_binding@ %a)@]"
          Simple.print bound_to
      | Rhs_in_scope { bound_to; } ->
        Format.fprintf ppf "@[<hov 1>(Rhs_in_scope@ %a)@]"
          Simple.print bound_to

    let hash _ = Misc.fatal_error "Rhs_kind.hash not yet implemented"

    let equal _ = Misc.fatal_error "Rhs_kind.equal not yet implemented"

    let compare t1 t2 =
      match t1, t2 with
      | ( Needs_extra_binding { bound_to = bound_to1 },
          Needs_extra_binding { bound_to = bound_to2 } ) ->
        Simple.compare bound_to1 bound_to2
      | ( Rhs_in_scope { bound_to = bound_to1 },
          Rhs_in_scope { bound_to = bound_to2 } ) ->
        Simple.compare bound_to1 bound_to2
      | Needs_extra_binding _, _ -> -1
      | Rhs_in_scope _, _ -> 1
  end)
end

let cse_with_eligible_lhs ~typing_env_at_fork ~cse_at_each_use ~params prev_cse
    (extra_bindings : EPA.t) extra_equations =
  let params_set =
    List.map params ~f:Bound_parameter.name |> Name.Set.of_list
  in
  let params = List.map params ~f:Bound_parameter.simple in
  let is_param simple =
    Simple.pattern_match simple
      ~name:(fun name ~coercion:_ -> Name.Set.mem name params_set)
      ~const:(fun _ -> false)
  in
  List.fold_left cse_at_each_use ~init:EP.Map.empty
    ~f:(fun eligible (env_at_use, id, cse) ->
      let find_new_name =
        let find_param simple params =
          List.find_opt
            ~f:(fun param ->
              match
                TE.get_canonical_simple_exn env_at_use param
                  ~min_name_mode:NM.normal
                  ~name_mode_of_existing_simple:NM.normal
              with
              | exception Not_found -> false
              | arg -> Simple.equal arg simple)
            params
        in
        match (extra_bindings : EPA.t) with
        | Empty -> fun arg -> find_param arg params
        | Non_empty { extra_args; extra_params } -> (
          let extra_args = RI.Map.find id extra_args in
          let rec find_name simple params args =
            match args, params with
            | [], [] -> None
            | [], _ | _, [] ->
              Misc.fatal_error "Mismatching params and args arity"
            | arg :: args, param :: params -> (
              match (arg : EA.t) with
              | Already_in_scope arg when Simple.equal arg simple ->
                (* If [param] has an extra equation associated to it, we
                   shouldn't propagate equations on it as it will mess with the
                   application of constraints later *)
                if Name.Map.mem (BP.name param) extra_equations
                then None
                else Some (BP.simple param)
              | Already_in_scope _ | New_let_binding _
              | New_let_binding_with_named_args _ ->
                find_name simple params args)
          in
          fun arg ->
            match find_param arg params with
            | None ->
              find_name arg (Bound_parameters.to_list extra_params) extra_args
            | Some _ as r -> r)
      in
      EP.Map.fold
        (fun prim bound_to eligible ->
          let prim =
            EP.filter_map_args prim ~f:(fun arg ->
                match
                  TE.get_canonical_simple_exn env_at_use arg
                    ~min_name_mode:NM.normal
                    ~name_mode_of_existing_simple:NM.normal
                with
                | exception Not_found -> None
                | arg -> (
                  match find_new_name arg with
                  | None ->
                    if TE.mem_simple typing_env_at_fork arg
                    then Some arg
                    else None
                  | Some _ as arg_opt -> arg_opt))
          in
          match prim with
          | None -> eligible
          | Some prim when EP.Map.mem prim prev_cse ->
            (* We've already got it from a previous round *)
            eligible
          | Some prim -> (
            match
              TE.get_canonical_simple_exn env_at_use bound_to
                ~min_name_mode:NM.normal ~name_mode_of_existing_simple:NM.normal
            with
            | exception Not_found -> eligible
            | bound_to -> (
              let bound_to =
                (* CR-someday mshinwell: Think about whether this is the best
                   fix. The canonical simple might end up being one of the
                   [params] since they are defined in [env_at_fork]. However
                   these aren't bound at the use sites, so we must choose
                   another alias that is. *)
                if not (is_param bound_to)
                then Some bound_to
                else
                  let aliases =
                    TE.aliases_of_simple env_at_use ~min_name_mode:NM.normal
                      bound_to
                    |> TE.Alias_set.filter ~f:(fun simple ->
                           not (is_param simple))
                  in
                  (* CR-someday lmaurer: Do we need to make sure there's only
                     one alias? If not, we can use [Aliases.Alias_set.find_best]
                     here. *)
                  TE.Alias_set.get_singleton aliases
              in
              match bound_to with
              | None -> eligible
              | Some bound_to -> (
                let bound_to : Rhs_kind.t =
                  if TE.mem_simple typing_env_at_fork bound_to
                  then Rhs_in_scope { bound_to }
                  else Needs_extra_binding { bound_to }
                in
                (* CR-someday mshinwell: Add [Map.add_or_replace]. *)
                match EP.Map.find prim eligible with
                | exception Not_found ->
                  EP.Map.add prim (RI.Map.singleton id bound_to) eligible
                | from_prev_levels ->
                  let map = RI.Map.add id bound_to from_prev_levels in
                  EP.Map.add prim map eligible))))
        cse eligible)

let join_one_cse_equation ~cse_at_each_use prim bound_to_map
    (cse, extra_bindings, extra_equations, allowed) =
  let has_value_on_all_paths =
    List.for_all cse_at_each_use ~f:(fun (_, id, _) ->
        RI.Map.mem id bound_to_map)
  in
  if not has_value_on_all_paths
  then cse, extra_bindings, extra_equations, allowed
  else
    let bound_to_set = RI.Map.data bound_to_map |> Rhs_kind.Set.of_list in
    match Rhs_kind.Set.get_singleton bound_to_set with
    | Some (Rhs_kind.Rhs_in_scope { bound_to }) ->
      EP.Map.add prim bound_to cse, extra_bindings, extra_equations, allowed
    | None | Some (Rhs_kind.Needs_extra_binding { bound_to = _ }) ->
      let prim_result_kind = P.result_kind' (EP.to_primitive prim) in
      let var = Variable.create "cse_param" in
      let extra_param =
        BP.create var (K.With_subkind.create prim_result_kind Anything)
      in
      let bound_to = RI.Map.map Rhs_kind.bound_to bound_to_map in
      let cse = EP.Map.add prim (Simple.var var) cse in
      let extra_args =
        RI.Map.map (fun simple : EA.t -> Already_in_scope simple) bound_to
      in
      let extra_bindings = EPA.add extra_bindings ~extra_param ~extra_args in
      let extra_equations =
        (* For the primitives Is_int and Get_tag, they're strongly linked to
           their argument: additional information on the cse parameter should
           translate into additional information on the argument. This can be
           done by giving them the appropriate type. The same could be done for
           a lot of the other non-arithmetic primitives, but in the other cases
           the join of the types will usually give us the relevant equation
           anyway. *)
        match[@ocaml.warning "-fragile-match"] EP.to_primitive prim with
        | Unary (Is_int { variant_only = true }, scrutinee) ->
          Simple.pattern_match scrutinee
            ~name:(fun scrutinee ~coercion:_ ->
              Name.Map.add (Name.var var)
                (T.is_int_for_scrutinee ~scrutinee)
                extra_equations)
            ~const:(fun _ -> extra_equations)
        | Unary (Get_tag, block) ->
          Simple.pattern_match block
            ~name:(fun block ~coercion:_ ->
              Name.Map.add (Name.var var)
                (T.get_tag_for_block ~block)
                extra_equations)
            ~const:(fun _ -> extra_equations)
        | _ -> extra_equations
      in
      let allowed =
        Name_occurrences.add_name allowed (Name.var var) NM.normal
      in
      cse, extra_bindings, extra_equations, allowed

let cut_cse_environment { by_scope; _ } ~scope_at_fork =
  (* This extracts those CSE equations that arose between the fork point and
     each use of the continuation in question. *)
  let _, _, levels = Scope.Map.split scope_at_fork by_scope in
  Scope.Map.fold
    (fun _scope equations result -> EP.Map.disjoint_union equations result)
    levels EP.Map.empty

module Join_result = struct
  type nonrec t =
    { cse_at_join_point : t;
      extra_params : EPA.t;
      (* CR-someday mshinwell: Change [extra_equations] to
         [Typing_env_extension.t]. *)
      extra_equations : T.t Name.Map.t;
      extra_allowed_names : Name_occurrences.t
    }
end

let join0 ~typing_env_at_fork ~cse_at_fork ~cse_at_each_use ~params
    ~scope_at_fork =
  let params = Bound_parameters.to_list params in
  (* CSE equations have a left-hand side specifying a primitive and a right-hand
     side specifying a [Simple]. The left-hand side is matched against portions
     of terms. As such, the [Simple]s therein must have name mode [Normal],
     since we do not do CSE for phantom bindings (see [Simplify_common]). It
     follows that any CSE equation whose left-hand side involves a name not
     defined at the fork point, having canonicalised such name, cannot be
     propagated. This step also canonicalises the right-hand sides of the CSE
     equations. *)
  let compute_cse_one_round prev_cse extra_params extra_equations ~allowed =
    let new_cse =
      cse_with_eligible_lhs ~typing_env_at_fork ~cse_at_each_use ~params
        prev_cse extra_params extra_equations
    in
    (* To make use of a CSE equation at or after the join point, its right-hand
       side must have the same value, no matter which path is taken from the
       fork point to the join point. We filter out equations that do not satisfy
       this. Sometimes we can force an equation to satisfy the property by
       explicitly passing the value of the right-hand side as an extra parameter
       to the continuation at the join point. *)
    let cse', extra_params', extra_equations', allowed =
      EP.Map.fold
        (join_one_cse_equation ~cse_at_each_use)
        new_cse
        (EP.Map.empty, EPA.empty, Name.Map.empty, allowed)
    in
    let need_other_round =
      (* If we introduce new parameters, then CSE equations involving the
         corresponding arguments can be considered again, so we need another
         round. *)
      not (EPA.is_empty extra_params')
    in
    let cse = EP.Map.disjoint_union prev_cse cse' in
    (* The order of cse arguments does not matter since only simples already in
       scope are used as extra arguments. *)
    let extra_params = EPA.concat ~outer:extra_params' ~inner:extra_params in
    let extra_equations =
      Name.Map.disjoint_union extra_equations extra_equations'
    in
    cse, extra_params, extra_equations, allowed, need_other_round
  in
  let cse, extra_params, extra_equations, allowed =
    let rec do_rounds current_round cse extra_params extra_equations allowed =
      let cse, extra_params, extra_equations, allowed, need_other_round =
        compute_cse_one_round cse extra_params extra_equations ~allowed
      in
      if need_other_round && current_round < Flambda_features.cse_depth ()
      then
        do_rounds (succ current_round) cse extra_params extra_equations allowed
      else
        ( (* Either a fixpoint has been reached or we've already explored far
             enough *)
          cse,
          extra_params,
          extra_equations,
          allowed )
    in
    do_rounds 1 EP.Map.empty EPA.empty Name.Map.empty Name_occurrences.empty
  in
  let have_propagated_something = ref false in
  let cse_at_join_point =
    (* Any CSE equation whose right-hand side identifies a name in the [allowed]
       set is propagated. We don't need to check the left-hand sides because we
       know all of those names are in [typing_env_at_fork]. *)
    EP.Map.fold
      (fun prim bound_to cse ->
        let propagate =
          Simple.pattern_match bound_to
            ~const:(fun _ -> true)
            ~name:(fun name ~coercion:_ ->
              Name_occurrences.mem_name allowed name)
        in
        if not propagate
        then cse
        else (
          have_propagated_something := true;
          add cse prim ~bound_to (Scope.next scope_at_fork)))
      cse cse_at_fork
  in
  if not !have_propagated_something
  then None
  else
    Some
      { Join_result.cse_at_join_point;
        extra_params;
        extra_equations;
        extra_allowed_names = allowed
      }

let join ~typing_env_at_fork ~cse_at_fork ~use_info ~get_typing_env
    ~get_rewrite_id ~get_cse ~params =
  let scope_at_fork = TE.current_scope typing_env_at_fork in
  let no_equations = ref false in
  let cse_at_each_use =
    List.map use_info ~f:(fun use ->
        let t = get_cse use in
        let cse_between_fork_and_use = cut_cse_environment t ~scope_at_fork in
        (* If one branch doesn't have any equations, then the join is going to
           be empty *)
        if EP.Map.is_empty cse_between_fork_and_use then no_equations := true;
        get_typing_env use, get_rewrite_id use, cse_between_fork_and_use)
  in
  if !no_equations
  then None
  else
    join0 ~typing_env_at_fork ~cse_at_fork ~cse_at_each_use ~params
      ~scope_at_fork
