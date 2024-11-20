(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                        Basile Clément, OCamlPro                        *)
(*                                                                        *)
(*   Copyright 2013--2025 OCamlPro SAS                                    *)
(*   Copyright 2014--2025 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module K = Flambda_kind
module TG = Type_grammar
module TE = Typing_env
module TEE = Typing_env_extension
module TEL = Typing_env_level
module ET = Expand_head.Expanded_type

(* This file implements the join of typing envs, or more precisely of typing env
   levels.

   Most of it is actually concerned with the join of aliases, although some of
   it is also taking care of robustly computing the join of env extensions and
   nested env extensions.

   In the following, we will call the "target env" the environment that is the
   result of the join, and the "joined envs" the distinct environments that are
   being joined.

   We perform a full n-way join in four steps:

   1) Process all demotions in the joined envs, building a relation between a
   variable in the target env and its canonical name in each of the joined envs.

   2) Compute shared demotions by detecting variables in the target env that
   have the same canonicals in all of the joined envs. This relies on all
   environments having a consistent binding times for the *shared variables*
   (i.e. the variables that are defined in the target environment) in order to
   avoid accidental quadratic complexity.

   At this point, we have found all the aliases between existing variables in
   the target env. It remains to compute the type information.

   3) Process all the non-alias type information in the joined envs, building a
   relation between a canonical variable in the target env and its new types in
   each of the joined envs. If a variable has been demoted in one of the joined
   envs but not in the target env as part of the previous step, treat it as if
   it had received the current type of its canonical in the joined env instead.

   For instance, if we add the type "= x" to a variable [p] in one joined env
   (demoting [p] to [x]) and we add a non-alias type [ty] to [p] in the other
   joined env, we will compute the join of [ty] and the type of [x] in the first
   joined env to assign to [p] in the target env.

   4) For any variable that has been assigned a new type in all the joined envs
   in the previous step, compute its new type in the target env by joining its
   types in all the joined envs. If there is at least one joined env where the
   variable did not get a new type, the result of the join can never be more
   precise than that type, which is also the original type of the variable in
   the target env. *)

module Index : sig
  include Container_types.S

  val zero : t

  val succ : t -> t
end = struct
  include Numeric_types.Int

  let zero = 0

  let succ n = n + 1
end

module Join_aliases : sig
  type t

  val empty : t

  (** [find ~mem_name ~is_bound_strictly_earlier simples t] is:

      - [Bottom] if [simples] is empty;
      - [Ok simple] if there is a [simple] that is equal to each of the [simples]
        in the corresponding environment, or otherwise an existential variable
        introduced with [add_existential_var] for this set of [simples];
      - [Unknown] otherwise.

    [mem_name] should return [true] if the name is defined in the target
    environment (false if it is a local variable of a joined environment).

    [is_bound_strictly_earlier] determines whether a {b shared} name (i.e.
    defined in the target env and in all joined envs) is bound earlier than
    a {b shared} simple. Recall that we require a consistent ordering on
    shared names.

    {b Note}: the [simples] must be canonical in their environment. *)
  val find :
    mem_name:(Name.t -> bool) ->
    is_bound_strictly_earlier:(Name.t -> than:Simple.t -> bool) ->
    Simple.t Index.Map.t ->
    t ->
    Simple.t Or_unknown_or_bottom.t

  (** [add_existential_var ~mem_name simples t] returns a fresh variable [var]
      and an updated [t] where [var] is associated with the [simples]. *)
  val add_existential_var :
    mem_name:(Name.t -> bool) -> Simple.t Index.Map.t -> t -> Variable.t * t

  type 'a add_result =
    { values_in_target_env : 'a Index.Map.t Variable.Map.t;
      touched_variables : Variable.Set.t
    }

  (** [add_in_target_env ~mem_name t values values_in_target_env] adds the values
  in [values], keyed by their name in the corresponding environment, to the
  [values_in_target_env], keyed with their name in the target environment.

  More precisely, if there is an entry [index -> var -> value] in [values],
  an entry [target_var -> index -> value] is added to [values_in_target_env]
  for all variables [target_var] in the target environment that are equal to
  [var] in the joined environment at [index]. *)
  val add_in_target_env :
    mem_name:(Name.t -> bool) ->
    t ->
    'a Variable.Map.t Index.Map.t ->
    'a Index.Map.t Variable.Map.t ->
    'a add_result

  type join_result = private
    { demoted_in_target_env : Simple.t Variable.Map.t;
          (** Variables that should be demoted in the target env as a result of the
          join.

          The demoted variables are no longer present in [t]. *)
      demoted_in_some_envs : Simple.t Index.Map.t Variable.Map.t;
          (** Variables that have been demoted in some (possibly all, if
              they have been demoted to distinct canonicals) of the joined
              environments, but not in the target enviroment.

              These are still present in [t], but they need to be considered for
              the join of types. *)
      t : t
    }

  val n_way_join :
    mem_name:(Name.t -> bool) ->
    is_bound_strictly_earlier:(Name.t -> than:Simple.t -> bool) ->
    t ->
    Simple.t Variable.Map.t Index.Map.t ->
    join_result Or_bottom.t
end = struct
  module Indexed_simple = Container_types.Make (struct
    type t = Simple.t Index.Map.t

    let print = Index.Map.print Simple.print

    let hash map =
      Index.Map.fold
        (fun index simple hash ->
          Hashtbl.hash (hash, Index.hash index, Simple.hash simple))
        map (Hashtbl.hash 0)

    let equal = Index.Map.equal Simple.equal

    let compare = Index.Map.compare Simple.compare
  end)

  module ISM = Indexed_simple.Map

  type t =
    { joined_simples : Variable.t ISM.t;
          (** Maps a tuple of simples in the joined environments to the variable
              that represents it in the target environment, if any.

              If there is a mapping [simples -> var] in [joined_simples], then
              [demoted_from_target_env(var) = simples]. *)
      demoted_from_target_env : Simple.t Index.Map.t Variable.Map.t;
          (** Maps a variable defined in the target environment to its
              canonicals in each joined environment {b where it has been
              demoted}.

              Missing entries in the map means that the variable has not been
              demoted in the corresponding environment.

              We assume that the binding time order for shared variables (i.e.
              variables that are present in the target environment and in all
              joined environments) is consistent across all environments.

              When given a set of simples in all environments, this allows us
              to find the appropriate variable quickly: if this set of simple
              is the set of canonicals for a shared variable, it can only be
              the case for the shared variable with the latest binding time
              (because a shared variable with an earlier binding time can never
              be demoted to a shared variable with a later binding time). *)
      names_in_target_env : Variable.Set.t Variable.Map.t Index.Map.t
          (** Maps a variable in a joined environment to the set of
              (other) variables it is equal to in the target environment. *)
    }

  let empty =
    { joined_simples = ISM.empty;
      demoted_from_target_env = Variable.Map.empty;
      names_in_target_env = Index.Map.empty
    }

  let find ~mem_name ~is_bound_strictly_earlier (simples : Simple.t Index.Map.t)
      t : _ Or_unknown_or_bottom.t =
    let[@inline] mem_simple simple =
      Simple.pattern_match simple
        ~const:(fun _ -> true)
        ~name:(fun name ~coercion:_ -> mem_name name)
    in
    (* We need to determine if the provided set of simples (which are assumed to
       be canonicals in their own environment) has an existing name in the
       target environment.

       This existing name might be:

       1) A constant or symbol, which can only happen if all the joined simples
       are equal; or

       2) A shared variable demoted in zero or more, but not all, environments;
       or

       3) A shared variable demoted in all environments; or

       4) An existential variable previously created for this exact set of
       simples.

       To detect case 1), we need to pick one of the simples and check that is
       it a) equal to all the other simples and b) exists in the target
       environment.

       To detect case 3) and 4), we make a lookup in the [join_simples] table.

       To detect case 2), it is not enough to check that all the simples are
       identical, because the shared variable might have been demoted in one but
       not all environments, and performing a partial lookup in the
       [join_simples] table (or pre-populating it) would give the join quadratic
       complexity globally.

       Instead, we exploit the fact that {b shared} variables are defined in the
       same order in all environments: if the simples are equal to a shared
       variable that has been demoted in some, but not all, environments, there
       is at least one simple that is equal to that variable, and it is
       necessary the latest bound shared variable because we can only demote to
       variables that were bound earlier.

       The code below computes the latest bound simple (considering that
       constants and symbols are bound at an identical -oo binding time) that is
       defined in the target environment to combine tests for cases 1) and
       2). *)
    let latest_bound_simple =
      Index.Map.fold
        (fun _ simple acc : _ Or_unknown_or_bottom.t ->
          match (acc : _ Or_unknown_or_bottom.t) with
          | Bottom | Unknown -> if mem_simple simple then Ok simple else Unknown
          | Ok existing_simple -> (
            match Simple.must_be_var simple with
            | None -> acc
            | Some (var, _coercion) ->
              if mem_name (Name.var var)
              then
                (* NB: These are not actually aliases in the target env yet. *)
                if is_bound_strictly_earlier (Name.var var)
                     ~than:existing_simple
                then Ok simple
                else acc
              else acc))
        simples Or_unknown_or_bottom.Bottom
    in
    let[@local] find_local_variable () : _ Or_unknown_or_bottom.t =
      (* When looking for an existential variable, we only look for exact
         matches.

         This means that we might end up creating more local variables than
         would be strictly necessary, but they have more precise types. *)
      match ISM.find_opt simples t.joined_simples with
      | None -> Unknown
      | Some var -> Ok (Simple.var var)
    in
    match latest_bound_simple with
    | Bottom -> Bottom
    | Unknown ->
      (* Join of existential variables can only be case 3) or 4) *)
      find_local_variable ()
    | Ok latest_bound_simple -> (
      match Simple.must_be_var latest_bound_simple with
      | None ->
        (* Case 1), 3), or 4) *)
        if Index.Map.for_all
             (fun _ simple -> Simple.equal simple latest_bound_simple)
             simples
        then Ok latest_bound_simple
        else find_local_variable ()
      | Some (var, coercion) ->
        (* Case 2), 3), or 4) *)
        let coercion_to_var = Coercion.inverse coercion in
        let earlier_bound_simples =
          Index.Map.filter_map
            (fun _ simple ->
              if Simple.equal simple latest_bound_simple
              then None
              else Some (Simple.apply_coercion_exn simple coercion_to_var))
            simples
        in
        let canonicals_for_var =
          Option.value ~default:Index.Map.empty
            (Variable.Map.find_opt var t.demoted_from_target_env)
        in
        (* Consider the case where we have [a -> (1:b, 2:c)], i.e. [a] has been
           demoted to [b] in environment [1] and to [c] in environment [2], but
           was not demoted in environment [0].

           Suppose we are looking for a name for the tuple [(0:a, 1:b)] where
           environment [2] is not present, because we are in a situation (e.g.
           tag of a variant) which is impossible in environment [2].

           We want to recognize this as being equal to [a], which we can do by
           restricting the canonicals of [a] to those of the environments for
           which we are making a lookup.

           Note that an alternative would be to create a new existential
           variable for the pair [(0:a, 1:b)], which could get a more precise
           type. We can't record both informations without introducing an env
           extension, so we favor preserving equalities for variables defined in
           the target env and precision for existential variables (cf
           [find_local_variable]). *)
        let canonicals_for_var =
          Index.Map.inter
            (fun _ _ canonical -> canonical)
            simples canonicals_for_var
        in
        if Index.Map.equal Simple.equal earlier_bound_simples canonicals_for_var
        then Ok latest_bound_simple
        else find_local_variable ())

  let add_existential_var ~mem_name simples t =
    let shared_name =
      try
        Index.Map.fold
          (fun _ simple raw_name ->
            Simple.pattern_match' simple
              ~const:(fun _ -> raw_name)
              ~symbol:(fun _ ~coercion:_ -> raw_name)
              ~var:(fun var ~coercion:_ ->
                let var_name = Variable.raw_name var in
                match raw_name with
                | None -> Some var_name
                | Some raw_name when String.equal raw_name var_name ->
                  Some raw_name
                | Some _ -> raise Not_found))
          simples None
      with Not_found -> None
    in
    let raw_name =
      match shared_name with Some raw_name -> raw_name | None -> "join_var"
    in
    let var = Variable.create raw_name in
    let joined_simples = ISM.add simples var t.joined_simples in
    let demoted_from_target_env =
      Variable.Map.add var simples t.demoted_from_target_env
    in
    let names_in_target_env =
      Index.Map.fold
        (fun index simple names_in_target_env ->
          match Simple.must_be_var simple with
          | Some (joined_var, coercion)
            when Coercion.is_id coercion && mem_name (Name.var joined_var) ->
            Index.Map.update index
              (fun names_from_this_env_in_target_env ->
                let names_from_this_env_in_target_env =
                  Option.value ~default:Variable.Map.empty
                    names_from_this_env_in_target_env
                in
                Some
                  (Variable.Map.update joined_var
                     (function
                       | None -> Some (Variable.Set.singleton var)
                       | Some existing_vars ->
                         Some (Variable.Set.add var existing_vars))
                     names_from_this_env_in_target_env))
              names_in_target_env
          | _ -> names_in_target_env)
        simples t.names_in_target_env
    in
    var, { joined_simples; names_in_target_env; demoted_from_target_env }

  let find_canonicals demoted_var t =
    match Variable.Map.find_opt demoted_var t.demoted_from_target_env with
    | Some canonicals -> canonicals
    | None ->
      Misc.fatal_errorf "Variable %a was not demoted." Variable.print
        demoted_var

  let forget_demoted_var demoted_var t =
    (* [demoted_var] is demoted to [simple] in all environments.

       Remove it from all maps (except [map_to_canonical], which records the
       demotion) -- for all intents and purposes, we only need to consider the
       canonical [simple]. *)
    let canonicals = find_canonicals demoted_var t in
    let demoted_from_target_env =
      Variable.Map.remove demoted_var t.demoted_from_target_env
    in
    let names_in_target_env =
      Index.Map.fold
        (fun index simple names_in_target_env ->
          match Simple.must_be_var simple with
          | Some (var, coercion) when Coercion.is_id coercion ->
            Index.Map.update index
              (fun names_for_index ->
                let names_for_index =
                  Option.value ~default:Variable.Map.empty names_for_index
                in
                let names_for_index =
                  Variable.Map.update var
                    (fun names ->
                      let names =
                        Option.value ~default:Variable.Set.empty names
                      in
                      let names = Variable.Set.remove demoted_var names in
                      if Variable.Set.is_empty names then None else Some names)
                    names_for_index
                in
                if Variable.Map.is_empty names_for_index
                then None
                else Some names_for_index)
              names_in_target_env
          | _ -> names_in_target_env)
        canonicals t.names_in_target_env
    in
    { t with demoted_from_target_env; names_in_target_env }

  let expand_to_names_in_target_env ~mem_name ~update_names names_in_target_env
      table acc =
    Index.Map.fold
      (fun index values (acc, names_in_target_env, touched_vars) ->
        let names_from_this_env_in_target_env =
          Option.value ~default:Variable.Map.empty
            (Index.Map.find_opt index names_in_target_env)
        in
        let acc, names_from_this_env_in_target_env, touched_vars =
          Variable.Map.fold
            (fun var value
                 (acc, names_from_this_env_in_target_env, touched_vars) ->
              let vars_in_target_env =
                Option.value ~default:Variable.Set.empty
                  (Variable.Map.find_opt var names_from_this_env_in_target_env)
              in
              let vars_in_target_env =
                if mem_name (Name.var var)
                then Variable.Set.add var vars_in_target_env
                else vars_in_target_env
              in
              let names_from_this_env_in_target_env =
                update_names var vars_in_target_env value
                  names_from_this_env_in_target_env
              in
              let acc =
                Variable.Set.fold
                  (fun var_in_target_env values ->
                    Variable.Map.update var_in_target_env
                      (function
                        | None -> Some (Index.Map.singleton index value)
                        | Some values_in_other_envs ->
                          Some (Index.Map.add index value values_in_other_envs))
                      values)
                  vars_in_target_env acc
              in
              ( acc,
                names_from_this_env_in_target_env,
                Variable.Set.union vars_in_target_env touched_vars ))
            values
            (acc, names_from_this_env_in_target_env, touched_vars)
        in
        ( acc,
          Index.Map.add index names_from_this_env_in_target_env
            names_in_target_env,
          touched_vars ))
      table
      (acc, names_in_target_env, Variable.Set.empty)

  type 'a add_result =
    { values_in_target_env : 'a Index.Map.t Variable.Map.t;
      touched_variables : Variable.Set.t
    }

  let add_in_target_env ~mem_name t table values_by_index =
    let values_in_target_env, _names_in_target_env, touched_variables =
      expand_to_names_in_target_env ~mem_name
        ~update_names:(fun _ _ _ names -> names)
        t.names_in_target_env table values_by_index
    in
    { values_in_target_env; touched_variables }

  type join_result =
    { demoted_in_target_env : Simple.t Variable.Map.t;
      demoted_in_some_envs : Simple.t Index.Map.t Variable.Map.t;
      t : t
    }

  let n_way_join0 ~mem_name ~is_bound_strictly_earlier t all_demotions =
    let demoted_from_target_env, names_in_target_env, touched_vars =
      expand_to_names_in_target_env ~mem_name
        ~update_names:
          (fun demoted_var names_of_demoted_in_target_env canonical_simple
               names_from_this_env_in_target_env ->
          let names_from_this_env_in_target_env =
            Variable.Map.remove demoted_var names_from_this_env_in_target_env
          in
          match Simple.must_be_var canonical_simple with
          | Some (canonical_var, coercion)
            when Coercion.is_id coercion && mem_name (Name.var canonical_var) ->
            Variable.Map.update canonical_var
              (function
                | None -> Some names_of_demoted_in_target_env
                | Some existing_vars ->
                  Some
                    (Variable.Set.union existing_vars
                       names_of_demoted_in_target_env))
              names_from_this_env_in_target_env
          | _ -> names_from_this_env_in_target_env)
        t.names_in_target_env all_demotions t.demoted_from_target_env
    in
    let t = { t with demoted_from_target_env; names_in_target_env } in
    let all_indices = Index.Map.keys all_demotions in
    Variable.Set.fold
      (fun demoted_var { demoted_in_target_env; demoted_in_some_envs; t } ->
        let canonicals = find_canonicals demoted_var t in
        let[@local] is_demoted_in_some_envs t =
          let demoted_in_some_envs =
            Variable.Map.add demoted_var canonicals demoted_in_some_envs
          in
          { demoted_in_target_env; demoted_in_some_envs; t }
        in
        let[@local] is_demoted_in_all_envs t =
          let joined_simples =
            ISM.add canonicals demoted_var t.joined_simples
          in
          is_demoted_in_some_envs { t with joined_simples }
        in
        let[@local] is_demoted_in_target_env canonical t =
          let t = forget_demoted_var demoted_var t in
          let demoted_in_target_env =
            Variable.Map.add demoted_var canonical demoted_in_target_env
          in
          { demoted_in_target_env; demoted_in_some_envs; t }
        in
        (* We keep stale entries in the [joined_simples] table here, which is OK
           because we never iterate on it and we never look them up anymore
           since they are non-canonical in at least one of the joined
           environments.

           This can only happen in the presence of env extensions. *)
        if not (Index.Set.subset all_indices (Index.Map.keys canonicals))
        then is_demoted_in_some_envs t
        else
          match find ~mem_name ~is_bound_strictly_earlier canonicals t with
          | Bottom ->
            Misc.fatal_error
              "Unexpected bottom for non-empty set of canonicals."
          | Unknown -> is_demoted_in_all_envs t
          | Ok simple -> is_demoted_in_target_env simple t)
      touched_vars
      { demoted_in_target_env = Variable.Map.empty;
        demoted_in_some_envs = Variable.Map.empty;
        t
      }

  let n_way_join ~mem_name ~is_bound_strictly_earlier t all_demotions =
    if Index.Map.is_empty all_demotions
    then Or_bottom.Bottom
    else
      Or_bottom.Ok
        (n_way_join0 ~mem_name ~is_bound_strictly_earlier t all_demotions)
end

module Join_equations = struct
  (** Maps a variable in the target environment to its {b updated} types in the
  joined environments.

  If the variable did not receive a new type (either explicitly or through demotion)
  in a given environment, the corresponding entry is absent.

  {b Note}: A variable can have a more precise joined type if, and only if,
  it has been given a new type in {b all} the joined environments. *)
  type t = ET.t Index.Map.t Variable.Map.t

  let empty = Variable.Map.empty

  let find var t =
    Option.value ~default:Index.Map.empty (Variable.Map.find_opt var t)

  let n_way_join ~n_way_join_type vars equations st =
    Variable.Map.fold
      (fun var types (equations, st) ->
        let types =
          Index.Map.fold
            (fun index expanded acc -> (index, ET.to_type expanded) :: acc)
            types []
        in
        match (n_way_join_type st types : _ Or_unknown.t * _) with
        | Unknown, st -> equations, st
        | Known ty, st -> Variable.Map.add var ty equations, st)
      vars (equations, st)

  let add_joined_simple ~joined_envs demoted_var canonicals joined_types =
    Variable.Map.update demoted_var
      (fun types_of_demoted_var ->
        let types_of_demoted_var =
          Option.value ~default:Index.Map.empty types_of_demoted_var
        in
        let types_of_demoted_var =
          Index.Map.fold
            (fun index canonical types_of_demoted_var ->
              let env = Index.Map.find index joined_envs in
              let ty =
                Simple.pattern_match canonical
                  ~const:More_type_creators.type_for_const
                  ~name:(fun name ~coercion ->
                    TG.apply_coercion (TE.find env name None) coercion)
              in
              let expanded =
                Expand_head.expand_head0 env ty
                  ~known_canonical_simple_at_in_types_mode:(Some canonical)
              in
              Index.Map.add index expanded types_of_demoted_var)
            canonicals types_of_demoted_var
        in
        Some types_of_demoted_var)
      joined_types
end

module Symbol_projection = struct
  include Symbol_projection
  include Container_types.Make (Symbol_projection)
end

let n_way_join_symbol_projections ~mem_name ~is_bound_strictly_earlier
    join_aliases joined_envs all_symbol_projections =
  let joined_projections =
    Index.Map.fold
      (fun index symbol_projections acc ->
        let typing_env = Index.Map.find index joined_envs in
        Variable.Map.fold
          (fun var symbol_projection acc ->
            let canonical =
              TE.get_canonical_simple_exn typing_env (Simple.var var)
                ~min_name_mode:Name_mode.in_types
            in
            Symbol_projection.Map.update symbol_projection
              (function
                | None -> Some (Index.Map.singleton index canonical)
                | Some projections_in_other_envs ->
                  Some (Index.Map.add index canonical projections_in_other_envs))
              acc)
          symbol_projections acc)
      all_symbol_projections Symbol_projection.Map.empty
  in
  let all_indices = Index.Map.keys joined_envs in
  Symbol_projection.Map.fold
    (fun symbol_projection simples symbol_projections ->
      if not (Index.Set.subset all_indices (Index.Map.keys simples))
      then symbol_projections
      else
        match
          Join_aliases.find ~mem_name ~is_bound_strictly_earlier simples
            join_aliases
        with
        | Bottom | Unknown -> symbol_projections
        | Ok simple -> (
          match Simple.must_be_var simple with
          | Some (var, coercion) when Coercion.is_id coercion ->
            Variable.Map.add var symbol_projection symbol_projections
          | _ -> symbol_projections))
    joined_projections Variable.Map.empty

type t =
  { join_aliases : Join_aliases.t;
    join_types : Join_equations.t;
    existential_vars : K.t Variable.Map.t;
    pending_vars : Simple.t Index.Map.t Variable.Map.t;
    (* Existential variables that have been defined by their names in all the
       joined environment, but whose type has not yet been computed. *)
    joined_envs : TE.t Index.Map.t;
    (* Currently active joined environments.

       {b Note}: This can be a subset of all the actual joined environments when
       performing a join inside env extensions. *)
    target_env : TE.t
  }

type join_result =
  { demoted_in_target_env : Simple.t Variable.Map.t;
    extra_variables : K.t Variable.Map.t;
    equations : TG.t Variable.Map.t;
    symbol_projections : Symbol_projection.t Variable.Map.t
  }

let n_way_join_levels ~n_way_join_type t all_levels : _ Or_bottom.t =
  let all_demotions, all_expanded_equations, all_symbol_projections =
    Index.Map.fold
      (fun index level
           (all_demotions, all_expanded_equations, all_symbol_projections) ->
        let symbol_projections = TEL.symbol_projections level in
        let equations = TEL.equations level in
        let typing_env = Index.Map.find index t.joined_envs in
        let demotions, expanded_equations =
          Name.Map.fold
            (fun name ty (demotions, expanded_equations) ->
              match Name.must_be_var_opt name with
              | None -> demotions, expanded_equations
              | Some var -> (
                match
                  TE.get_alias_then_canonical_simple_exn
                    ~min_name_mode:Name_mode.in_types typing_env ty
                with
                | canonical_simple ->
                  ( Variable.Map.add var canonical_simple demotions,
                    expanded_equations )
                | exception Not_found ->
                  let expanded =
                    Expand_head.expand_head0 typing_env ty
                      ~known_canonical_simple_at_in_types_mode:
                        (Some (Simple.var var))
                  in
                  demotions, Variable.Map.add var expanded expanded_equations))
            equations
            (Variable.Map.empty, Variable.Map.empty)
        in
        ( Index.Map.add index demotions all_demotions,
          Index.Map.add index expanded_equations all_expanded_equations,
          Index.Map.add index symbol_projections all_symbol_projections ))
      all_levels
      (Index.Map.empty, Index.Map.empty, Index.Map.empty)
  in
  let mem_name = TE.mem ~min_name_mode:Name_mode.in_types t.target_env in
  let is_bound_strictly_earlier name ~than =
    TE.alias_is_bound_strictly_earlier t.target_env ~bound_name:name ~alias:than
  in
  match
    Join_aliases.n_way_join ~mem_name ~is_bound_strictly_earlier t.join_aliases
      all_demotions
  with
  | Bottom -> Bottom
  | Ok { demoted_in_target_env; demoted_in_some_envs; t = join_aliases } ->
    let join_types =
      Variable.Map.fold
        (Join_equations.add_joined_simple ~joined_envs:t.joined_envs)
        demoted_in_some_envs t.join_types
    in
    let { Join_aliases.values_in_target_env = join_types;
          touched_variables = touched_vars
        } =
      Join_aliases.add_in_target_env ~mem_name join_aliases
        all_expanded_equations join_types
    in
    let touched_vars =
      Variable.Set.union touched_vars (Variable.Map.keys demoted_in_some_envs)
    in
    let t = { t with join_aliases; join_types } in
    let all_indices = Index.Map.keys t.joined_envs in
    let equations_to_join =
      Variable.Set.fold
        (fun var new_vars ->
          let types = Join_equations.find var t.join_types in
          if not (Index.Set.subset all_indices (Index.Map.keys types))
          then new_vars
          else
            (* Restrict the indices in case we are joining env extensions that
               are not defined in all environments *)
            let types =
              Index.Map.inter (fun _ _ expanded -> expanded) t.joined_envs types
            in
            Variable.Map.add var types new_vars)
        touched_vars Variable.Map.empty
    in
    let rec loop equations_to_join joined_equations t =
      let equations, t =
        Join_equations.n_way_join ~n_way_join_type equations_to_join
          joined_equations t
      in
      if Variable.Map.is_empty t.pending_vars
      then
        let symbol_projections =
          n_way_join_symbol_projections ~mem_name ~is_bound_strictly_earlier
            t.join_aliases t.joined_envs all_symbol_projections
        in
        Or_bottom.Ok
          { demoted_in_target_env;
            extra_variables = t.existential_vars;
            equations;
            symbol_projections
          }
      else
        let join_types =
          Variable.Map.fold
            (Join_equations.add_joined_simple ~joined_envs:t.joined_envs)
            t.pending_vars t.join_types
        in
        let equations_to_join =
          Variable.Map.mapi
            (fun var _ -> Join_equations.find var join_types)
            t.pending_vars
        in
        let pending_vars = Variable.Map.empty in
        loop equations_to_join equations { t with pending_vars; join_types }
    in
    loop equations_to_join Variable.Map.empty t

let cut_and_n_way_join ~n_way_join_type ~meet_type ~cut_after target_env
    joined_envs =
  let _, joined_envs, joined_levels =
    List.fold_left
      (fun (discriminant, joined_envs, joined_levels) typing_env ->
        let level = TE.cut typing_env ~cut_after in
        ( Index.succ discriminant,
          Index.Map.add discriminant typing_env joined_envs,
          Index.Map.add discriminant level joined_levels ))
      (Index.zero, Index.Map.empty, Index.Map.empty)
      joined_envs
  in
  match
    n_way_join_levels ~n_way_join_type
      { join_aliases = Join_aliases.empty;
        join_types = Join_equations.empty;
        existential_vars = Variable.Map.empty;
        pending_vars = Variable.Map.empty;
        joined_envs;
        target_env
      }
      joined_levels
  with
  | Bottom ->
    (* Join of zero envs -- should possibly return bottom? *)
    target_env
  | Ok { demoted_in_target_env; extra_variables; equations; symbol_projections }
    ->
    let target_env =
      Variable.Map.fold
        (fun var kind target_env ->
          TE.add_definition target_env
            (Bound_name.create_var (Bound_var.create var Name_mode.in_types))
            kind)
        extra_variables target_env
    in
    let target_env =
      Variable.Map.fold
        (fun var simple target_env ->
          let kind = TG.kind (TE.find target_env (Name.var var) None) in
          let ty = TG.alias_type_of kind simple in
          TE.add_equation ~meet_type target_env (Name.var var) ty)
        demoted_in_target_env target_env
    in
    let target_env =
      Variable.Map.fold
        (fun var ty target_env ->
          TE.add_equation ~meet_type target_env (Name.var var) ty)
        equations target_env
    in
    let target_env =
      Variable.Map.fold
        (fun var symbol_projection target_env ->
          TE.add_symbol_projection target_env var symbol_projection)
        symbol_projections target_env
    in
    target_env

let n_way_join_env_extension ~n_way_join_type ~meet_type t envs_with_extensions
    =
  let joined_levels, joined_envs =
    List.fold_left
      (fun (joined_levels, joined_envs) (index, extension) ->
        let parent_env = Index.Map.find index t.joined_envs in
        (* The extension is not guaranteed to still be in canonical form, but we
           need the equations to be in canonical form to known which variables
           are actually touched by the extension, so we add it once then cut it.

           Note: we need to cut it as a level, because the meets from
           [add_env_extension_strict] could add perform nested joins which could
           add new variables. *)
        assert (not (TE.is_bottom parent_env));
        let cut_after = TE.current_scope parent_env in
        let typing_env = TE.increment_scope parent_env in
        match TE.add_env_extension_strict ~meet_type typing_env extension with
        | Bottom ->
          (* We can reach bottom here if the extension was created in a more
             generic context, but is added in a context where it is no longer
             reachable. *)
          joined_levels, joined_envs
        | Ok typing_env ->
          let level = TE.cut typing_env ~cut_after in
          ( Index.Map.add index level joined_levels,
            Index.Map.add index typing_env joined_envs ))
      (Index.Map.empty, Index.Map.empty)
      envs_with_extensions
  in
  match
    n_way_join_levels ~n_way_join_type
      { join_aliases = t.join_aliases;
        join_types = t.join_types;
        existential_vars = t.existential_vars;
        pending_vars = Variable.Map.empty;
        joined_envs;
        target_env = t.target_env
      }
      joined_levels
  with
  | Bottom -> Or_bottom.Bottom
  | Ok { demoted_in_target_env; extra_variables; equations; symbol_projections }
    ->
    if not (Variable.Map.is_empty symbol_projections)
    then Misc.fatal_error "Unexpected symbol projections in env extension.";
    let joined_equations =
      Variable.Map.fold
        (fun var simple equations ->
          let kind =
            match Variable.Map.find_opt var extra_variables with
            | Some kind -> kind
            | None -> TG.kind (TE.find t.target_env (Name.var var) None)
          in
          let ty = TG.alias_type_of kind simple in
          Name.Map.add (Name.var var) ty equations)
        demoted_in_target_env Name.Map.empty
    in
    let joined_equations =
      Variable.Map.fold
        (fun var ty equations -> Name.Map.add (Name.var var) ty equations)
        equations joined_equations
    in
    (* Preserve existential vars since we can't bind them in extensions. *)
    let existential_vars = extra_variables in
    Or_bottom.Ok (TEE.from_map joined_equations, { t with existential_vars })

let n_way_join_simples t kind simples : _ Or_bottom.t * _ =
  let simples = Index.Map.of_list simples in
  let mem_name = TE.mem ~min_name_mode:Name_mode.in_types t.target_env in
  let is_bound_strictly_earlier name ~than =
    TE.alias_is_bound_strictly_earlier t.target_env ~bound_name:name ~alias:than
  in
  match
    Join_aliases.find ~mem_name ~is_bound_strictly_earlier simples
      t.join_aliases
  with
  | Bottom -> Bottom, t
  | Ok simple -> Ok simple, t
  | Unknown ->
    let var, join_aliases =
      Join_aliases.add_existential_var ~mem_name simples t.join_aliases
    in
    let existential_vars = Variable.Map.add var kind t.existential_vars in
    let pending_vars = Variable.Map.add var simples t.pending_vars in
    Ok (Simple.var var), { t with existential_vars; join_aliases; pending_vars }

type env_id = Index.t

type 'a join_arg = env_id * 'a

let target_join_env { target_env; _ } = target_env

type n_way_join_type = t -> TG.t join_arg list -> TG.t Or_unknown.t * t

let joined_env env index =
  match Index.Map.find_opt index env.joined_envs with
  | Some typing_env -> typing_env
  | None -> Misc.fatal_error "Invalid joined environment."
