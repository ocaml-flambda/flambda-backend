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

let get_nth_joined_env index joined_envs =
  match Index.Map.find_opt index joined_envs with
  | Some typing_env -> typing_env
  | None ->
    Misc.fatal_errorf "Joined environment %a is not available." Index.print
      index

(* The following are intended to help make sure we don't confuse things (names,
   simples) that are living in one of the joined environments and those that
   live in the target environment.

   In particular, one simple in a joined environment can have multiple names in
   the target environment if they have been demoted. *)

module Thing_in_env (Thing : Container_types.S) () : sig
  include Container_types.S with type t = private Thing.t

  val create : Thing.t -> t
end = struct
  include Thing

  let create thing = thing
end

module Name_in_target_env = struct
  include Thing_in_env (Name) ()
end

module Simple_in_target_env : sig
  include module type of Thing_in_env (Simple) ()

  val name : Name_in_target_env.t -> t
end = struct
  include Thing_in_env (Simple) ()

  let name (name : Name_in_target_env.t) = create (Simple.name (name :> Name.t))
end

module Name_in_one_joined_env = struct
  include Thing_in_env (Name) ()
end

module Simple_in_one_joined_env : sig
  include module type of Thing_in_env (Simple) ()

  val pattern_match :
    t ->
    name:(Name_in_one_joined_env.t -> coercion:Coercion.t -> 'a) ->
    const:(Reg_width_const.t -> 'a) ->
    'a
end = struct
  include Thing_in_env (Simple) ()

  let pattern_match (t : t) ~name:on_name ~const =
    Simple.pattern_match
      (t :> Simple.t)
      ~name:(fun name ~coercion ->
        on_name (Name_in_one_joined_env.create name) ~coercion)
      ~const
end

module Simple_in_joined_envs : sig
  include
    Container_types.S
      with type t = private Simple_in_one_joined_env.t Index.Map.t

  val fold :
    (Index.t -> Simple_in_one_joined_env.t -> 'a -> 'a) -> t -> 'a -> 'a

  val distinct_from_simple_in_target_env : t -> Simple_in_target_env.t -> t

  val apply_coercion : t -> Coercion.t -> t

  val is_empty : t -> bool

  val empty : t

  val in_same_envs : t -> as_:t -> t

  val in_envs : 'a Index.Map.t -> t -> t

  val is_defined_in : Index.Set.t -> t -> bool

  val raw_name : t -> string

  val add : Index.t -> Simple_in_one_joined_env.t -> t -> t

  val of_list : (Index.t * Simple.t) list -> t
end = struct
  module T0 = struct
    type t = Simple_in_one_joined_env.t Index.Map.t

    let print = Index.Map.print Simple_in_one_joined_env.print

    let hash map =
      Index.Map.fold
        (fun index simple hash ->
          Hashtbl.hash
            (hash, Index.hash index, Simple_in_one_joined_env.hash simple))
        map (Hashtbl.hash 0)

    let equal = Index.Map.equal Simple_in_one_joined_env.equal

    let compare = Index.Map.compare Simple_in_one_joined_env.compare
  end

  include T0
  include Container_types.Make (T0)

  let fold = Index.Map.fold

  let is_empty = Index.Map.is_empty

  let empty = Index.Map.empty

  let add = Index.Map.add

  let apply_coercion t coercion =
    if Coercion.is_id coercion
    then t
    else
      Index.Map.map
        (fun (simple : Simple_in_one_joined_env.t) ->
          Simple_in_one_joined_env.create
            (Simple.apply_coercion_exn (simple :> Simple.t) coercion))
        t

  let distinct_from_simple_in_target_env (t : t)
      (simple_in_target_env : Simple_in_target_env.t) =
    Index.Map.filter_map
      (fun _ (simple_in_one_joined_env : Simple_in_one_joined_env.t) ->
        if Simple.equal
             (simple_in_one_joined_env :> Simple.t)
             (simple_in_target_env :> Simple.t)
        then None
        else Some simple_in_one_joined_env)
      t

  let in_envs envs t = Index.Map.inter (fun _ _ simple -> simple) envs t

  let in_same_envs t ~as_ = Index.Map.inter (fun _ _ simple -> simple) as_ t

  let is_defined_in envs t = Index.Set.subset envs (Index.Map.keys t)

  let raw_name (t : t) =
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
          (t :> Simple.t Index.Map.t)
          None
      with Not_found -> None
    in
    match shared_name with Some raw_name -> raw_name | None -> "join_var"

  let of_list list =
    List.fold_left
      (fun t (index, simple) ->
        Index.Map.add index (Simple_in_one_joined_env.create simple) t)
      empty list
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
    exists_in_target_env:
      (Name_in_one_joined_env.t -> Name_in_target_env.t option) ->
    is_bound_strictly_earlier:
      (Name_in_target_env.t -> than:Simple_in_target_env.t -> bool) ->
    Simple_in_joined_envs.t ->
    t ->
    Simple_in_target_env.t Or_unknown_or_bottom.t

  (** [add_existential_var ~mem_name simples t] returns a fresh variable [var]
      and an updated [t] where [var] is associated with the [simples]. *)
  val add_existential_var :
    exists_in_target_env:
      (Name_in_one_joined_env.t -> Name_in_target_env.t option) ->
    Simple_in_joined_envs.t ->
    t ->
    Variable.t * t

  type 'a add_result =
    { values_in_target_env : 'a Index.Map.t Name_in_target_env.Map.t;
      touched_variables : Name_in_target_env.Set.t
    }

  (** [add_in_target_env ~mem_name t values values_in_target_env] adds the values
  in [values], keyed by their name in the corresponding environment, to the
  [values_in_target_env], keyed with their name in the target environment.

  More precisely, if there is an entry [index -> var -> value] in [values],
  an entry [target_var -> index -> value] is added to [values_in_target_env]
  for all variables [target_var] in the target environment that are equal to
  [var] in the joined environment at [index]. *)
  val add_in_target_env :
    exists_in_target_env:
      (Name_in_one_joined_env.t -> Name_in_target_env.t option) ->
    t ->
    'a Name_in_one_joined_env.Map.t Index.Map.t ->
    'a Index.Map.t Name_in_target_env.Map.t ->
    'a add_result

  type join_result = private
    { demoted_in_target_env : Simple_in_target_env.t Name_in_target_env.Map.t;
          (** Variables that should be demoted in the target env as a result of the
          join.

          The demoted variables are no longer present in [t]. *)
      demoted_in_some_envs : Simple_in_joined_envs.t Name_in_target_env.Map.t;
          (** Variables that have been demoted in some (possibly all, if
              they have been demoted to distinct canonicals) of the joined
              environments, but not in the target enviroment.

              These are still present in [t], but they need to be considered for
              the join of types. *)
      t : t
    }

  val n_way_join :
    exists_in_target_env:
      (Name_in_one_joined_env.t -> Name_in_target_env.t option) ->
    is_bound_strictly_earlier:
      (Name_in_target_env.t -> than:Simple_in_target_env.t -> bool) ->
    t ->
    Simple_in_one_joined_env.t Name_in_one_joined_env.Map.t Index.Map.t ->
    join_result Or_bottom.t
end = struct
  type t =
    { joined_simples : Name_in_target_env.t Simple_in_joined_envs.Map.t;
          (** Maps a tuple of simples in the joined environments to the variable
              that represents it in the target environment, if any.

              If there is a mapping [simples -> var] in [joined_simples], then
              [demoted_from_target_env(var) = simples]. *)
      demoted_from_target_env :
        Simple_in_joined_envs.t Name_in_target_env.Map.t;
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
      names_in_target_env :
        Name_in_target_env.Set.t Name_in_one_joined_env.Map.t Index.Map.t
          (** Maps a variable in a joined environment to the set of
              (other) variables it is equal to in the target environment. *)
    }

  let empty =
    { joined_simples = Simple_in_joined_envs.Map.empty;
      demoted_from_target_env = Name_in_target_env.Map.empty;
      names_in_target_env = Index.Map.empty
    }

  let find
      ~(exists_in_target_env :
         Name_in_one_joined_env.t -> Name_in_target_env.t option)
      ~(is_bound_strictly_earlier :
         Name_in_target_env.t -> than:Simple_in_target_env.t -> bool)
      (simples : Simple_in_joined_envs.t) t : _ Or_unknown_or_bottom.t =
    let[@inline] simple_exists_in_target_env simple_in_one_joined_env =
      Simple_in_one_joined_env.pattern_match simple_in_one_joined_env
        ~const:(fun const ->
          Some (Simple_in_target_env.create (Simple.const const)))
        ~name:(fun name_in_one_joined_env ~coercion ->
          match exists_in_target_env name_in_one_joined_env with
          | None -> None
          | Some name_in_target_env ->
            Some
              (Simple_in_target_env.create
                 (Simple.with_coercion
                    (Simple.name (name_in_target_env :> Name.t))
                    coercion)))
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
      Simple_in_joined_envs.fold
        (fun _ simple acc : _ Or_unknown_or_bottom.t ->
          match (acc : _ Or_unknown_or_bottom.t) with
          | Bottom | Unknown -> (
            match simple_exists_in_target_env simple with
            | None -> Unknown
            | Some simple -> Ok simple)
          | Ok existing_simple -> (
            match Simple.must_be_var (simple :> Simple.t) with
            | None -> acc
            | Some (var, coercion) -> (
              match
                exists_in_target_env
                  (Name_in_one_joined_env.create (Name.var var))
              with
              | None -> acc
              | Some name_in_target_env ->
                (* NB: These are not actually aliases in the target env yet. *)
                if is_bound_strictly_earlier name_in_target_env
                     ~than:existing_simple
                then
                  Ok
                    (Simple_in_target_env.create
                       (Simple.with_coercion
                          (Simple.name (name_in_target_env :> Name.t))
                          coercion))
                else acc)))
        simples Or_unknown_or_bottom.Bottom
    in
    let[@local] find_local_variable () : _ Or_unknown_or_bottom.t =
      (* When looking for an existential variable, we only look for exact
         matches.

         This means that we might end up creating more local variables than
         would be strictly necessary, but they have more precise types. *)
      match Simple_in_joined_envs.Map.find_opt simples t.joined_simples with
      | None -> Unknown
      | Some name -> Ok (Simple_in_target_env.name name)
    in
    match latest_bound_simple with
    | Bottom -> Bottom
    | Unknown ->
      (* Join of existential variables can only be case 3) or 4) *)
      find_local_variable ()
    | Ok latest_bound_simple -> (
      let earlier_bound_simples =
        Simple_in_joined_envs.distinct_from_simple_in_target_env simples
          latest_bound_simple
      in
      match Simple.must_be_name (latest_bound_simple :> Simple.t) with
      | None ->
        (* Case 1), 3), or 4) *)
        if Simple_in_joined_envs.is_empty earlier_bound_simples
        then Ok latest_bound_simple
        else find_local_variable ()
      | Some (name, coercion) ->
        (* Case 2), 3), or 4) *)
        let coercion_to_name = Coercion.inverse coercion in
        let earlier_bound_simples =
          Simple_in_joined_envs.apply_coercion earlier_bound_simples
            coercion_to_name
        in
        let canonicals_for_name =
          Option.value ~default:Simple_in_joined_envs.empty
            (Name_in_target_env.Map.find_opt
               (Name_in_target_env.create name)
               t.demoted_from_target_env)
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
        let canonicals_for_name =
          Simple_in_joined_envs.in_same_envs ~as_:simples canonicals_for_name
        in
        if Simple_in_joined_envs.equal earlier_bound_simples canonicals_for_name
        then Ok latest_bound_simple
        else find_local_variable ())

  let add_existential_var ~exists_in_target_env simples t =
    (* We have encountered a [Simple_in_joined_envs.t] that does not cleanly
       correspond to the demotion of a name in the target env, e.g. we have type
       "= a" on the left and "= b" on the right but no variable that is equal to
       "a" on the left and "b" on the right yet.

       We now create a new existential variable for this pair of values, and
       record it so that it can be found by [find] if we encounter the same set
       of values later. *)
    let raw_name = Simple_in_joined_envs.raw_name simples in
    let var = Variable.create raw_name in
    let var_as_name = Name_in_target_env.create (Name.var var) in
    let joined_simples =
      Simple_in_joined_envs.Map.add simples var_as_name t.joined_simples
    in
    let demoted_from_target_env =
      Name_in_target_env.Map.add var_as_name simples t.demoted_from_target_env
    in
    let names_in_target_env =
      Simple_in_joined_envs.fold
        (fun index simple names_in_target_env ->
          match Simple.must_be_var (simple :> Simple.t) with
          | Some (joined_var, coercion) when Coercion.is_id coercion -> (
            let name_in_joined_env =
              Name_in_one_joined_env.create (Name.var joined_var)
            in
            match exists_in_target_env name_in_joined_env with
            | None -> names_in_target_env
            | Some (name_in_target_env : Name_in_target_env.t) ->
              Index.Map.update index
                (fun names_from_this_env_in_target_env ->
                  let names_from_this_env_in_target_env =
                    Option.value ~default:Name_in_one_joined_env.Map.empty
                      names_from_this_env_in_target_env
                  in
                  Some
                    (Name_in_one_joined_env.Map.update name_in_joined_env
                       (function
                         | None ->
                           Some
                             (Name_in_target_env.Set.singleton
                                name_in_target_env)
                         | Some existing_names ->
                           Some
                             (Name_in_target_env.Set.add name_in_target_env
                                existing_names))
                       names_from_this_env_in_target_env))
                names_in_target_env)
          | _ -> names_in_target_env)
        simples t.names_in_target_env
    in
    var, { joined_simples; names_in_target_env; demoted_from_target_env }

  let find_canonicals demoted_in_target_env t =
    match
      Name_in_target_env.Map.find_opt demoted_in_target_env
        t.demoted_from_target_env
    with
    | Some canonicals -> canonicals
    | None ->
      Misc.fatal_errorf "Variable %a was not demoted." Name_in_target_env.print
        demoted_in_target_env

  let forget_demoted_var demoted_in_target_env t =
    (* [demoted_in_target_env] is demoted to the same simple in all
       environments.

       Remove it from all maps (except [map_to_canonical], which records the
       demotion) -- we don't need to treat it as an name in the target env of
       joined simples, since we have its new canonical instead. *)
    let canonicals = find_canonicals demoted_in_target_env t in
    let demoted_from_target_env =
      Name_in_target_env.Map.remove demoted_in_target_env
        t.demoted_from_target_env
    in
    let names_in_target_env =
      Simple_in_joined_envs.fold
        (fun index simple names_in_target_env ->
          match Simple.must_be_var (simple :> Simple.t) with
          | Some (var, coercion) when Coercion.is_id coercion ->
            let name_in_joined_env =
              Name_in_one_joined_env.create (Name.var var)
            in
            Index.Map.update index
              (fun names_for_index ->
                let names_for_index =
                  Option.value ~default:Name_in_one_joined_env.Map.empty
                    names_for_index
                in
                let names_for_index =
                  Name_in_one_joined_env.Map.update name_in_joined_env
                    (fun names ->
                      let names =
                        Option.value ~default:Name_in_target_env.Set.empty names
                      in
                      let names =
                        Name_in_target_env.Set.remove demoted_in_target_env
                          names
                      in
                      if Name_in_target_env.Set.is_empty names
                      then None
                      else Some names)
                    names_for_index
                in
                if Name_in_one_joined_env.Map.is_empty names_for_index
                then None
                else Some names_for_index)
              names_in_target_env
          | _ -> names_in_target_env)
        canonicals t.names_in_target_env
    in
    { t with demoted_from_target_env; names_in_target_env }

  (* This function is responsible for recording demotions, represented as a map
     from names to their {b current canonical simple} in each joined env.

     This means we must:

     - Update the [names_in_target_env] map to ensure the keys are canonicals in
     the corresponding joined env and, if demoting a name that exists in the
     target env, include the new name as an alias of the new canonical.

     - Update the [demoted_from_target_env] map with the new canonicals in the
     joined envs (recall that [demoted_from_target_env] only stores canonicals
     in the joined envs that are *distinct* from the name in the target env). *)
  let apply_demotions ~exists_in_target_env t all_demotions =
    Index.Map.fold
      (fun index values
           (demoted_from_target_env, names_in_target_env, touched_vars) ->
        let names_from_this_env_in_target_env =
          Option.value ~default:Name_in_one_joined_env.Map.empty
            (Index.Map.find_opt index names_in_target_env)
        in
        let ( demoted_from_target_env,
              names_from_this_env_in_target_env,
              touched_vars ) =
          Name_in_one_joined_env.Map.fold
            (fun demoted_var
                 (canonical_in_joined_env : Simple_in_one_joined_env.t)
                 ( demoted_from_target_env,
                   names_from_this_env_in_target_env,
                   touched_vars ) ->
              (* Usually, we expect that there is no entry in the
                 [names_in_target_env] map for a variable we are demoting, since
                 we only introduce entries on canonicals.

                 However if we are processing an env extension with a demotion
                 [y -> z], we could have demoted [x -> y] at the toplevel
                 [cut_and_n_way_join], which would have introduced a mapping
                 from [y] to [{ x }] (since [x] is a name for [y] in the target
                 env). In this case, we need to remove the mapping for [y]
                 (since it is no longer canonical in the extension) and
                 introduce the mapping [z -> { x }] (if [y] does not exist in
                 the target env) or [z -> { x, y }] (if [y] exists in the target
                 env) instead. *)
              let vars_in_target_env =
                Option.value ~default:Name_in_target_env.Set.empty
                  (Name_in_one_joined_env.Map.find_opt demoted_var
                     names_from_this_env_in_target_env)
              in
              let vars_in_target_env =
                match exists_in_target_env demoted_var with
                | None -> vars_in_target_env
                | Some name_in_target_env ->
                  Name_in_target_env.Set.add name_in_target_env
                    vars_in_target_env
              in
              let names_from_this_env_in_target_env =
                let names_from_this_env_in_target_env =
                  Name_in_one_joined_env.Map.remove demoted_var
                    names_from_this_env_in_target_env
                in
                match
                  Simple.must_be_var (canonical_in_joined_env :> Simple.t)
                with
                | Some (canonical_var, coercion) when Coercion.is_id coercion ->
                  Name_in_one_joined_env.Map.update
                    (Name_in_one_joined_env.create (Name.var canonical_var))
                    (function
                      | None -> Some vars_in_target_env
                      | Some existing_vars ->
                        Some
                          (Name_in_target_env.Set.union existing_vars
                             vars_in_target_env))
                    names_from_this_env_in_target_env
                | _ -> names_from_this_env_in_target_env
              in
              let demoted_from_target_env =
                Name_in_target_env.Set.fold
                  (fun var_in_target_env values ->
                    Name_in_target_env.Map.update var_in_target_env
                      (fun canonical_in_joined_envs ->
                        let canonical_in_joined_envs =
                          Option.value ~default:Simple_in_joined_envs.empty
                            canonical_in_joined_envs
                        in
                        Some
                          (Simple_in_joined_envs.add index
                             canonical_in_joined_env canonical_in_joined_envs))
                      values)
                  vars_in_target_env demoted_from_target_env
              in
              ( demoted_from_target_env,
                names_from_this_env_in_target_env,
                Name_in_target_env.Set.union vars_in_target_env touched_vars ))
            values
            ( demoted_from_target_env,
              names_from_this_env_in_target_env,
              touched_vars )
        in
        ( demoted_from_target_env,
          Index.Map.add index names_from_this_env_in_target_env
            names_in_target_env,
          touched_vars ))
      all_demotions
      ( t.demoted_from_target_env,
        t.names_in_target_env,
        Name_in_target_env.Set.empty )

  type 'a add_result =
    { values_in_target_env : 'a Index.Map.t Name_in_target_env.Map.t;
      touched_variables : Name_in_target_env.Set.t
    }

  (* Takes a map of values in joined envs (i.e. a map from canonical names in a
     joined env to values of type ['a] for each joined env) and transforms it
     into a map of values in the target env by adding the values on name [n] to
     all the aliases of [n] that are canonical in the target env. *)
  let add_in_target_env ~exists_in_target_env t values_in_joined_envs
      values_in_target_env =
    let values_in_target_env, touched_variables =
      Index.Map.fold
        (fun index values_in_joined_env (values_in_target_env, touched_vars) ->
          let names_from_this_env_in_target_env =
            Option.value ~default:Name_in_one_joined_env.Map.empty
              (Index.Map.find_opt index t.names_in_target_env)
          in
          let values_in_target_env, touched_vars =
            Name_in_one_joined_env.Map.fold
              (fun var value (acc, touched_vars) ->
                let vars_in_target_env =
                  Option.value ~default:Name_in_target_env.Set.empty
                    (Name_in_one_joined_env.Map.find_opt var
                       names_from_this_env_in_target_env)
                in
                let vars_in_target_env =
                  match exists_in_target_env var with
                  | None -> vars_in_target_env
                  | Some name_in_target_env ->
                    Name_in_target_env.Set.add name_in_target_env
                      vars_in_target_env
                in
                let values_in_target_env =
                  Name_in_target_env.Set.fold
                    (fun var_in_target_env values ->
                      Name_in_target_env.Map.update var_in_target_env
                        (function
                          | None -> Some (Index.Map.singleton index value)
                          | Some values_in_other_envs ->
                            Some
                              (Index.Map.add index value values_in_other_envs))
                        values)
                    vars_in_target_env acc
                in
                ( values_in_target_env,
                  Name_in_target_env.Set.union vars_in_target_env touched_vars ))
              values_in_joined_env
              (values_in_target_env, touched_vars)
          in
          values_in_target_env, touched_vars)
        values_in_joined_envs
        (values_in_target_env, Name_in_target_env.Set.empty)
    in
    { values_in_target_env; touched_variables }

  type join_result =
    { demoted_in_target_env : Simple_in_target_env.t Name_in_target_env.Map.t;
      demoted_in_some_envs : Simple_in_joined_envs.t Name_in_target_env.Map.t;
      t : t
    }

  let n_way_join0 ~exists_in_target_env ~is_bound_strictly_earlier t
      all_demotions =
    let demoted_from_target_env, names_in_target_env, touched_vars =
      apply_demotions ~exists_in_target_env t all_demotions
    in
    let t = { t with demoted_from_target_env; names_in_target_env } in
    let all_indices = Index.Map.keys all_demotions in
    Name_in_target_env.Set.fold
      (fun demoted_var { demoted_in_target_env; demoted_in_some_envs; t } ->
        let canonicals = find_canonicals demoted_var t in
        let[@local] is_demoted_in_some_envs t =
          let demoted_in_some_envs =
            Name_in_target_env.Map.add demoted_var canonicals
              demoted_in_some_envs
          in
          { demoted_in_target_env; demoted_in_some_envs; t }
        in
        let[@local] is_demoted_in_all_envs t =
          let joined_simples =
            Simple_in_joined_envs.Map.add canonicals demoted_var
              t.joined_simples
          in
          is_demoted_in_some_envs { t with joined_simples }
        in
        let[@local] is_demoted_in_target_env canonical t =
          let t = forget_demoted_var demoted_var t in
          let demoted_in_target_env =
            Name_in_target_env.Map.add demoted_var canonical
              demoted_in_target_env
          in
          { demoted_in_target_env; demoted_in_some_envs; t }
        in
        (* We keep stale entries in the [joined_simples] table here, which is OK
           because we never iterate on it and we never look them up anymore
           since they are non-canonical in at least one of the joined
           environments.

           This can only happen in the presence of env extensions. *)
        if not (Simple_in_joined_envs.is_defined_in all_indices canonicals)
        then is_demoted_in_some_envs t
        else
          match
            find ~exists_in_target_env ~is_bound_strictly_earlier canonicals t
          with
          | Bottom ->
            Misc.fatal_error
              "Unexpected bottom for non-empty set of canonicals."
          | Unknown -> is_demoted_in_all_envs t
          | Ok simple -> is_demoted_in_target_env simple t)
      touched_vars
      { demoted_in_target_env = Name_in_target_env.Map.empty;
        demoted_in_some_envs = Name_in_target_env.Map.empty;
        t
      }

  let n_way_join ~exists_in_target_env ~is_bound_strictly_earlier t
      all_demotions =
    if Index.Map.is_empty all_demotions
    then Or_bottom.Bottom
    else
      Or_bottom.Ok
        (n_way_join0 ~exists_in_target_env ~is_bound_strictly_earlier t
           all_demotions)
end

module Join_equations = struct
  (** Maps a variable in the target environment to its {b updated} types in the
  joined environments.

  If the variable did not receive a new type (either explicitly or through demotion)
  in a given environment, the corresponding entry is absent.

  {b Note}: A variable can have a more precise joined type if, and only if,
  it has been given a new type in {b all} the joined environments. *)
  type t = ET.t Index.Map.t Name_in_target_env.Map.t

  let empty = Name_in_target_env.Map.empty

  let find var t =
    Option.value ~default:Index.Map.empty
      (Name_in_target_env.Map.find_opt var t)

  let n_way_join ~n_way_join_type vars equations st =
    Name_in_target_env.Map.fold
      (fun var types (equations, st) ->
        let types =
          Index.Map.fold
            (fun index expanded acc -> (index, ET.to_type expanded) :: acc)
            types []
        in
        match (n_way_join_type st types : _ Or_unknown.t * _) with
        | Unknown, st -> equations, st
        | Known ty, st -> Name_in_target_env.Map.add var ty equations, st)
      vars (equations, st)

  let add_joined_simple ~joined_envs demoted_var canonicals joined_types =
    Name_in_target_env.Map.update demoted_var
      (fun types_of_demoted_var ->
        let types_of_demoted_var =
          Option.value ~default:Index.Map.empty types_of_demoted_var
        in
        let types_of_demoted_var =
          Simple_in_joined_envs.fold
            (fun index canonical types_of_demoted_var ->
              let env = get_nth_joined_env index joined_envs in
              let canonical_simple = (canonical :> Simple.t) in
              let ty =
                Simple.pattern_match canonical_simple
                  ~const:More_type_creators.type_for_const
                  ~name:(fun name ~coercion ->
                    TG.apply_coercion (TE.find env name None) coercion)
              in
              let expanded =
                Expand_head.expand_head0 env ty
                  ~known_canonical_simple_at_in_types_mode:
                    (Some canonical_simple)
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

let n_way_join_symbol_projections ~exists_in_target_env
    ~is_bound_strictly_earlier join_aliases joined_envs all_symbol_projections =
  let joined_projections =
    Index.Map.fold
      (fun index symbol_projections acc ->
        let typing_env = get_nth_joined_env index joined_envs in
        Variable.Map.fold
          (fun var symbol_projection acc ->
            let canonical =
              TE.get_canonical_simple_exn typing_env (Simple.var var)
                ~min_name_mode:Name_mode.in_types
            in
            let canonical = Simple_in_one_joined_env.create canonical in
            Symbol_projection.Map.update symbol_projection
              (fun joined_projections ->
                let joined_projections =
                  Option.value joined_projections
                    ~default:Simple_in_joined_envs.empty
                in
                Some
                  (Simple_in_joined_envs.add index canonical joined_projections))
              acc)
          symbol_projections acc)
      all_symbol_projections Symbol_projection.Map.empty
  in
  let all_indices = Index.Map.keys joined_envs in
  Symbol_projection.Map.fold
    (fun symbol_projection simples symbol_projections ->
      if not (Simple_in_joined_envs.is_defined_in all_indices simples)
      then symbol_projections
      else
        match
          Join_aliases.find ~exists_in_target_env ~is_bound_strictly_earlier
            simples join_aliases
        with
        | Bottom | Unknown -> symbol_projections
        | Ok simple -> (
          match Simple.must_be_var (simple :> Simple.t) with
          | Some (var, coercion) when Coercion.is_id coercion ->
            Variable.Map.add var symbol_projection symbol_projections
          | _ -> symbol_projections))
    joined_projections Variable.Map.empty

type t =
  { join_aliases : Join_aliases.t;
    join_types : Join_equations.t;
    existential_vars : K.t Variable.Map.t;
    pending_vars : Simple_in_joined_envs.t Name_in_target_env.Map.t;
    (* Existential variables that have been defined by their names in all the
       joined environment, but whose type has not yet been computed. *)
    joined_envs : TE.t Index.Map.t;
    (* Currently active joined environments.

       {b Note}: This can be a subset of all the actual joined environments when
       performing a join inside env extensions. *)
    target_env : TE.t
  }

type join_result =
  { demoted_in_target_env : Simple_in_target_env.t Name_in_target_env.Map.t;
    extra_variables : K.t Variable.Map.t;
    equations : TG.t Name_in_target_env.Map.t;
    symbol_projections : Symbol_projection.t Variable.Map.t
  }

let n_way_join_levels ~n_way_join_type t all_levels : _ Or_bottom.t =
  let all_demotions, all_expanded_equations, all_symbol_projections =
    Index.Map.fold
      (fun index level
           (all_demotions, all_expanded_equations, all_symbol_projections) ->
        let symbol_projections = TEL.symbol_projections level in
        let equations = TEL.equations level in
        let typing_env = get_nth_joined_env index t.joined_envs in
        let demotions, expanded_equations =
          Name.Map.fold
            (fun name ty (demotions, expanded_equations) ->
              match Name.must_be_var_opt name with
              | None -> demotions, expanded_equations
              | Some var -> (
                let name_in_joined_env =
                  Name_in_one_joined_env.create (Name.var var)
                in
                (* Note: we must compute the current canonical here, because
                   [Join_aliases.n_way_join] expects a fully compressed map
                   demotions (i.e. the right-hand side must not themselves be
                   demoted) *)
                match
                  TE.get_alias_then_canonical_simple_exn
                    ~min_name_mode:Name_mode.in_types typing_env ty
                with
                | canonical_simple ->
                  ( Name_in_one_joined_env.Map.add name_in_joined_env
                      (Simple_in_one_joined_env.create canonical_simple)
                      demotions,
                    expanded_equations )
                | exception Not_found ->
                  let expanded =
                    Expand_head.expand_head0 typing_env ty
                      ~known_canonical_simple_at_in_types_mode:
                        (Some (Simple.var var))
                  in
                  ( demotions,
                    Name_in_one_joined_env.Map.add name_in_joined_env expanded
                      expanded_equations )))
            equations
            (Name_in_one_joined_env.Map.empty, Name_in_one_joined_env.Map.empty)
        in
        ( Index.Map.add index demotions all_demotions,
          Index.Map.add index expanded_equations all_expanded_equations,
          Index.Map.add index symbol_projections all_symbol_projections ))
      all_levels
      (Index.Map.empty, Index.Map.empty, Index.Map.empty)
  in
  let target_env = t.target_env in
  let exists_in_target_env (name : Name_in_one_joined_env.t) =
    if TE.mem ~min_name_mode:Name_mode.in_types target_env (name :> Name.t)
    then Some (Name_in_target_env.create (name :> Name.t))
    else None
  in
  let is_bound_strictly_earlier (name : Name_in_target_env.t)
      ~(than : Simple_in_target_env.t) =
    TE.alias_is_bound_strictly_earlier t.target_env
      ~bound_name:(name :> Name.t)
      ~alias:(than :> Simple.t)
  in
  match
    Join_aliases.n_way_join ~exists_in_target_env ~is_bound_strictly_earlier
      t.join_aliases all_demotions
  with
  | Bottom -> Bottom
  | Ok { demoted_in_target_env; demoted_in_some_envs; t = join_aliases } ->
    let join_types =
      Name_in_target_env.Map.fold
        (fun name_in_target_env canonicals ->
          let canonicals =
            Simple_in_joined_envs.in_envs all_levels canonicals
          in
          Join_equations.add_joined_simple ~joined_envs:t.joined_envs
            name_in_target_env canonicals)
        demoted_in_some_envs t.join_types
    in
    let { Join_aliases.values_in_target_env = join_types;
          touched_variables = touched_vars
        } =
      Join_aliases.add_in_target_env ~exists_in_target_env join_aliases
        all_expanded_equations join_types
    in
    let touched_vars =
      Name_in_target_env.Set.union touched_vars
        (Name_in_target_env.Map.keys demoted_in_some_envs)
    in
    let t = { t with join_aliases; join_types } in
    let all_indices = Index.Map.keys t.joined_envs in
    let equations_to_join =
      Name_in_target_env.Set.fold
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
            Name_in_target_env.Map.add var types new_vars)
        touched_vars Name_in_target_env.Map.empty
    in
    let rec loop equations_to_join joined_equations t =
      let equations, t =
        Join_equations.n_way_join ~n_way_join_type equations_to_join
          joined_equations t
      in
      if Name_in_target_env.Map.is_empty t.pending_vars
      then
        let symbol_projections =
          n_way_join_symbol_projections ~exists_in_target_env
            ~is_bound_strictly_earlier t.join_aliases t.joined_envs
            all_symbol_projections
        in
        Or_bottom.Ok
          { demoted_in_target_env;
            extra_variables = t.existential_vars;
            equations;
            symbol_projections
          }
      else
        let join_types =
          Name_in_target_env.Map.fold
            (fun name_in_target_env canonicals ->
              let canonicals =
                Simple_in_joined_envs.in_envs all_levels canonicals
              in
              Join_equations.add_joined_simple ~joined_envs:t.joined_envs
                name_in_target_env canonicals)
            t.pending_vars t.join_types
        in
        let equations_to_join =
          Name_in_target_env.Map.mapi
            (fun var _ -> Join_equations.find var join_types)
            t.pending_vars
        in
        let pending_vars = Name_in_target_env.Map.empty in
        loop equations_to_join equations { t with pending_vars; join_types }
    in
    loop equations_to_join Name_in_target_env.Map.empty t

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
        pending_vars = Name_in_target_env.Map.empty;
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
      Name_in_target_env.Map.fold
        (fun name (simple : Simple_in_target_env.t) target_env ->
          let name = (name :> Name.t) in
          let simple = (simple :> Simple.t) in
          let kind = TG.kind (TE.find target_env name None) in
          let ty = TG.alias_type_of kind simple in
          TE.add_equation ~meet_type target_env name ty)
        demoted_in_target_env target_env
    in
    let target_env =
      Name_in_target_env.Map.fold
        (fun name ty target_env ->
          TE.add_equation ~meet_type target_env (name :> Name.t) ty)
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
        let parent_env = get_nth_joined_env index t.joined_envs in
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
        pending_vars = Name_in_target_env.Map.empty;
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
      Name_in_target_env.Map.fold
        (fun name (simple : Simple_in_target_env.t) equations ->
          let kind =
            match Name.must_be_var_opt (name :> Name.t) with
            | None -> TG.kind (TE.find t.target_env (name :> Name.t) None)
            | Some var -> (
              match Variable.Map.find_opt var extra_variables with
              | Some kind -> kind
              | None -> TG.kind (TE.find t.target_env (name :> Name.t) None))
          in
          let ty = TG.alias_type_of kind (simple :> Simple.t) in
          Name.Map.add (name :> Name.t) ty equations)
        demoted_in_target_env Name.Map.empty
    in
    let joined_equations =
      Name_in_target_env.Map.fold
        (fun name ty equations -> Name.Map.add (name :> Name.t) ty equations)
        equations joined_equations
    in
    (* Preserve existential vars since we can't bind them in extensions. *)
    let existential_vars = extra_variables in
    Or_bottom.Ok (TEE.from_map joined_equations, { t with existential_vars })

let n_way_join_simples t kind simples : _ Or_bottom.t * _ =
  let simples = Simple_in_joined_envs.of_list simples in
  let target_env = t.target_env in
  let exists_in_target_env (name : Name_in_one_joined_env.t) =
    if TE.mem ~min_name_mode:Name_mode.in_types target_env (name :> Name.t)
    then Some (Name_in_target_env.create (name :> Name.t))
    else None
  in
  let is_bound_strictly_earlier (name : Name_in_target_env.t)
      ~(than : Simple_in_target_env.t) =
    TE.alias_is_bound_strictly_earlier t.target_env
      ~bound_name:(name :> Name.t)
      ~alias:(than :> Simple.t)
  in
  match
    Join_aliases.find ~exists_in_target_env ~is_bound_strictly_earlier simples
      t.join_aliases
  with
  | Bottom -> Bottom, t
  | Ok simple -> Ok (simple :> Simple.t), t
  | Unknown ->
    let var, join_aliases =
      Join_aliases.add_existential_var ~exists_in_target_env simples
        t.join_aliases
    in
    let var_as_name = Name_in_target_env.create (Name.var var) in
    let existential_vars = Variable.Map.add var kind t.existential_vars in
    let pending_vars =
      Name_in_target_env.Map.add var_as_name simples t.pending_vars
    in
    Ok (Simple.var var), { t with existential_vars; join_aliases; pending_vars }

type env_id = Index.t

type 'a join_arg = env_id * 'a

let target_join_env { target_env; _ } = target_env

type n_way_join_type = t -> TG.t join_arg list -> TG.t Or_unknown.t * t

let joined_env env index = get_nth_joined_env index env.joined_envs
