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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module K = Flambda_kind
module MTC = More_type_creators
module TG = Type_grammar
module TEL = Typing_env_level

(* CR mshinwell: Add signatures to these submodules. *)
module One_level = struct
  type t =
    { scope : Scope.t;
      level : TEL.t;
      just_after_level : Cached_level.t
    }

  let [@ocamlformat "disable"] print ~min_binding_time ppf
        { scope = _; level; just_after_level; } =
    let restrict_to = TEL.defined_names level in
    if Name.Set.is_empty restrict_to then
      Format.fprintf ppf "@[<hov 0>\
          %a\
          @]"
        TEL.print level
    else
      Format.fprintf ppf "@[<hov 0>\
          @[<hov 1>(defined_vars@ %a)@]@ \
          %a\
          @]"
        (Cached_level.print_name_modes ~restrict_to ~min_binding_time) just_after_level
        TEL.print level

  let create scope level ~just_after_level = { scope; level; just_after_level }

  let create_empty scope =
    { scope; level = TEL.empty; just_after_level = Cached_level.empty }

  let scope t = t.scope

  let level t = t.level

  let just_after_level t = t.just_after_level

  let with_aliases t ~aliases =
    let just_after_level =
      Cached_level.with_aliases t.just_after_level ~aliases
    in
    { t with just_after_level }

  let is_empty t = TEL.is_empty t.level

  (* let defines_name_but_no_equations t name =
     TEL.defines_name_but_no_equations t.level name *)

  let clean_for_export t ~reachable_names =
    { t with
      just_after_level =
        Cached_level.clean_for_export t.just_after_level ~reachable_names
    }
end

type t =
  { resolver : Compilation_unit.t -> t option;
    binding_time_resolver : Name.t -> Binding_time.t;
    get_imported_names : unit -> Name.Set.t;
    defined_symbols : Symbol.Set.t;
    code_age_relation : Code_age_relation.t;
    prev_levels : One_level.t Scope.Map.t;
    current_level : One_level.t;
    next_binding_time : Binding_time.t;
    min_binding_time : Binding_time.t (* Earlier variables have mode In_types *)
  }

type typing_env = t

let is_empty t =
  One_level.is_empty t.current_level
  && Scope.Map.is_empty t.prev_levels
  && Symbol.Set.is_empty t.defined_symbols

let aliases t =
  Cached_level.aliases (One_level.just_after_level t.current_level)

(* CR mshinwell: Should print name occurrence kinds *)
(* CR mshinwell: Add option to print [aliases] *)
let [@ocamlformat "disable"] print ppf
      ({ resolver = _; binding_time_resolver = _;get_imported_names = _;
         prev_levels; current_level; next_binding_time = _;
         defined_symbols; code_age_relation; min_binding_time;
       } as t) =
  if is_empty t then
    Format.pp_print_string ppf "Empty"
  else
    let levels =
      Scope.Map.add (One_level.scope current_level) current_level prev_levels
    in
    let levels =
      Scope.Map.filter (fun _ level -> not (One_level.is_empty level))
        levels
    in
    Format.fprintf ppf
      "@[<hov 1>(\
          @[<hov 1>(defined_symbols@ %a)@]@ \
          @[<hov 1>(code_age_relation@ %a)@]@ \
          @[<hov 1>(levels@ %a)@]@ \
          @[<hov 1>(aliases@ %a)@]\
          )@]"
      Symbol.Set.print defined_symbols
      Code_age_relation.print code_age_relation
      (Scope.Map.print (One_level.print ~min_binding_time))
      levels
      Aliases.print (aliases t)

module Meet_env = struct
  type t =
    { env : typing_env;
      already_meeting : Name.Pair.Set.t
    }

  let [@ocamlformat "disable"] print ppf { env; already_meeting; } =
    Format.fprintf ppf
      "@[<hov 1>(\
        @[<hov 1>(env@ %a)@]@ \
        @[<hov 1>(already_meeting@ %a)@])@]"
      print env
      Name.Pair.Set.print already_meeting

  let create env = { env; already_meeting = Name.Pair.Set.empty }

  let env t = t.env

  let already_meeting_names t name1 name2 =
    Name.Pair.Set.mem (name1, name2) t.already_meeting
    || Name.Pair.Set.mem (name2, name1) t.already_meeting

  let already_meeting t simple1 simple2 =
    let const _const = false in
    Simple.pattern_match simple1 ~const ~name:(fun name1 ~coercion:_ ->
        Simple.pattern_match simple2 ~const ~name:(fun name2 ~coercion:_ ->
            already_meeting_names t name1 name2))

  let now_meeting_names t name1 name2 =
    if already_meeting_names t name1 name2
    then
      Misc.fatal_errorf "Already meeting %a and %a:@ %a" Name.print name1
        Name.print name2 print t;
    let already_meeting = Name.Pair.Set.add (name1, name2) t.already_meeting in
    { t with already_meeting }

  let now_meeting t simple1 simple2 =
    let const _const = t in
    Simple.pattern_match simple1 ~const ~name:(fun name1 ~coercion:_ ->
        Simple.pattern_match simple2 ~const ~name:(fun name2 ~coercion:_ ->
            now_meeting_names t name1 name2))
end

type meet_type =
  Meet_env.t -> TG.t -> TG.t -> (TG.t * Typing_env_extension.t) Or_bottom.t

module Join_env = struct
  type t =
    { central_env : Meet_env.t;
      left_join_env : typing_env;
      right_join_env : typing_env
    }

  let [@ocamlformat "disable"] print ppf
      { central_env; left_join_env; right_join_env } =
    let join_env name ppf env =
      Format.fprintf ppf "@ @[<hov 1>(%s@ %a)@]@" name print env
    in
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(central_env@ %a)@]\
        %a%a)@]"
      Meet_env.print central_env
      (join_env "left_join_env") left_join_env
      (join_env "right_join_env") right_join_env

  let create central_env ~left_env ~right_env =
    { central_env = Meet_env.create central_env;
      left_join_env = left_env;
      right_join_env = right_env
    }

  let target_join_env t = Meet_env.env t.central_env

  let left_join_env t = t.left_join_env

  let right_join_env t = t.right_join_env

  (* CR mshinwell: fix naming, it's odd at the moment to be using
     [already_meeting]... *)
  let now_joining t simple1 simple2 =
    { t with central_env = Meet_env.now_meeting t.central_env simple1 simple2 }

  let already_joining { central_env; _ } simple1 simple2 =
    Meet_env.already_meeting central_env simple1 simple2
end

let names_to_types t =
  Cached_level.names_to_types (One_level.just_after_level t.current_level)

exception Binding_time_resolver_failure

let binding_time_resolver resolver name =
  match resolver (Name.compilation_unit name) with
  | exception _ ->
    Misc.fatal_errorf
      "Exception in resolver (via [binding_time_resolver])@ Backtrace is: %s"
      (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))
  | None -> raise Binding_time_resolver_failure
  | Some t -> (
    match Name.Map.find name (names_to_types t) with
    | exception Not_found ->
      Misc.fatal_errorf "Binding time resolver cannot find name %a in:@ %a"
        Name.print name print t
    | _, binding_time, _ -> binding_time)

module Serializable : sig
  type t

  val create : typing_env -> t

  val print : Format.formatter -> t -> unit

  val all_ids_for_export : t -> Ids_for_export.t

  val apply_renaming : t -> Renaming.t -> t

  val merge : t -> t -> t

  val to_typing_env :
    t ->
    resolver:(Compilation_unit.t -> typing_env option) ->
    get_imported_names:(unit -> Name.Set.t) ->
    typing_env
end = struct
  type t =
    { defined_symbols : Symbol.Set.t;
      code_age_relation : Code_age_relation.t;
      just_after_level : Cached_level.t;
      next_binding_time : Binding_time.t
    }

  let defined_symbols t = t.defined_symbols

  let code_age_relation t = t.code_age_relation

  let just_after_level t = t.just_after_level

  let next_binding_time t = t.next_binding_time

  let create ~defined_symbols ~code_age_relation ~just_after_level
      ~next_binding_time =
    { defined_symbols; code_age_relation; just_after_level; next_binding_time }

  let [@ocamlformat "disable"] print ppf
      { defined_symbols; code_age_relation; just_after_level;
        next_binding_time = _ } =
    Format.fprintf ppf
      "@[<hov 1>(\
          @[<hov 1>(defined_symbols@ %a)@]@ \
          @[<hov 1>(code_age_relation@ %a)@]@ \
          @[<hov 1>(type_equations@ %a)@]@ \
          @[<hov 1>(aliases@ %a)@]\
          )@]"
      Symbol.Set.print defined_symbols
      Code_age_relation.print code_age_relation
      (Name.Map.print (fun ppf (ty, _bt, _mode) -> TG.print ppf ty))
      (Cached_level.names_to_types just_after_level)
      Aliases.print (Cached_level.aliases just_after_level)

  (* CR mshinwell for vlaviron: Shouldn't some of this be in
     [Cached_level.all_ids_for_export]? *)
  let all_ids_for_export
      { defined_symbols;
        code_age_relation;
        just_after_level;
        next_binding_time = _
      } =
    let symbols = defined_symbols in
    let code_ids =
      Code_age_relation.all_code_ids_for_export code_age_relation
    in
    let ids = Ids_for_export.create ~symbols ~code_ids () in
    let ids =
      Name.Map.fold
        (fun name (typ, _binding_time, _name_mode) ids ->
          Ids_for_export.add_name
            (Ids_for_export.union ids (TG.all_ids_for_export typ))
            name)
        (Cached_level.names_to_types just_after_level)
        ids
    in
    let ids =
      Ids_for_export.union ids
        (Aliases.all_ids_for_export (Cached_level.aliases just_after_level))
    in
    let ids =
      Variable.Map.fold
        (fun var proj ids ->
          let ids =
            Ids_for_export.union ids (Symbol_projection.all_ids_for_export proj)
          in
          Ids_for_export.add_variable ids var)
        (Cached_level.symbol_projections just_after_level)
        ids
    in
    ids

  let apply_renaming
      { defined_symbols;
        code_age_relation;
        just_after_level;
        next_binding_time
      } renaming =
    let defined_symbols =
      Symbol.Set.fold
        (fun sym symbols ->
          Symbol.Set.add (Renaming.apply_symbol renaming sym) symbols)
        defined_symbols Symbol.Set.empty
    in
    let code_age_relation =
      Code_age_relation.apply_renaming code_age_relation renaming
    in
    let just_after_level =
      Cached_level.apply_renaming just_after_level renaming
    in
    { defined_symbols; code_age_relation; just_after_level; next_binding_time }

  let merge t1 t2 =
    let defined_symbols =
      Symbol.Set.union t1.defined_symbols t2.defined_symbols
    in
    let code_age_relation =
      Code_age_relation.union t1.code_age_relation t2.code_age_relation
    in
    let just_after_level =
      Cached_level.merge t1.just_after_level t2.just_after_level
    in
    let next_binding_time =
      (* Take the latest one *)
      if Binding_time.strictly_earlier t1.next_binding_time
           ~than:t2.next_binding_time
      then t2.next_binding_time
      else t1.next_binding_time
    in
    { defined_symbols; code_age_relation; just_after_level; next_binding_time }

  let create (t : typing_env) =
    create ~defined_symbols:t.defined_symbols
      ~code_age_relation:t.code_age_relation
      ~just_after_level:(One_level.just_after_level t.current_level)
      ~next_binding_time:t.next_binding_time

  let to_typing_env serializable ~resolver ~get_imported_names =
    let defined_symbols = defined_symbols serializable in
    let code_age_relation = code_age_relation serializable in
    let just_after_level = just_after_level serializable in
    let next_binding_time = next_binding_time serializable in
    { resolver;
      binding_time_resolver = binding_time_resolver resolver;
      get_imported_names;
      defined_symbols;
      code_age_relation;
      prev_levels = Scope.Map.empty;
      current_level = One_level.create Scope.initial TEL.empty ~just_after_level;
      (* Note: the field [next_binding_time] of the new env will not be used,
         but setting [min_binding_time] to the value of [next_binding_time] from
         the serialized env marks all variables as having mode In_types. *)
      next_binding_time;
      min_binding_time = next_binding_time
    }
end

let invariant0 ?force _t =
  if Flambda_features.check_invariants ()
     || Option.is_some (force : unit option)
  then
    ( (* CR mshinwell: Fix things so this check passes, or delete it. let
         no_empty_prev_levels = Scope.Map.for_all (fun _scope level -> not
         (One_level.is_empty level)) t.prev_levels in if not
         no_empty_prev_levels then begin Misc.fatal_errorf "Typing environment
         contains [prev_levels] that are \ empty:@ %a" print t end; let
         current_scope = One_level.scope t.current_level in let max_prev_scope =
         Scope.Map.fold (fun scope _level max_prev_scope -> Scope.max scope
         max_prev_scope) t.prev_levels Scope.initial in if (not (is_empty t)) &&
         Scope.(<=) current_scope max_prev_scope then begin Misc.fatal_errorf
         "Typing environment contains a [current_level] with a \ scope (%a) that
         is not strictly greater than all scopes in \ [prev_levels] (%a):@ %a"
         Scope.print current_scope Scope.print max_prev_scope print t end *) )

let invariant t : unit = invariant0 t

let resolver t = t.resolver

let code_age_relation_resolver t comp_unit =
  match t.resolver comp_unit with
  | None -> None
  | Some t -> Some t.code_age_relation

let current_scope t = One_level.scope t.current_level

let aliases_with_min_binding_time t = aliases t, t.min_binding_time

let create ~resolver ~get_imported_names =
  { resolver;
    binding_time_resolver = binding_time_resolver resolver;
    get_imported_names;
    prev_levels = Scope.Map.empty;
    current_level = One_level.create_empty Scope.initial;
    next_binding_time = Binding_time.earliest_var;
    defined_symbols = Symbol.Set.empty;
    code_age_relation = Code_age_relation.empty;
    min_binding_time = Binding_time.earliest_var
  }

let increment_scope t =
  let current_scope = current_scope t in
  let prev_levels = Scope.Map.add current_scope t.current_level t.prev_levels in
  let current_level =
    One_level.create (Scope.next current_scope) TEL.empty
      ~just_after_level:(One_level.just_after_level t.current_level)
  in
  { t with prev_levels; current_level }

let defined_symbols t = t.defined_symbols

let name_domain t =
  Name.Set.union
    (Name.Map.keys (names_to_types t))
    (Name.set_of_symbol_set (defined_symbols t))

let initial_symbol_type =
  lazy (MTC.unknown K.value, Binding_time.symbols, Name_mode.normal)

let variable_is_from_missing_cmx_file t name =
  if Name.is_symbol name
  then false
  else
    let comp_unit = Name.compilation_unit name in
    if Compilation_unit.equal comp_unit (Compilation_unit.get_current_exn ())
    then false
    else
      match (resolver t) comp_unit with
      | exception _ -> true
      | None -> true
      | Some _ -> false

let check_optional_kind_matches name ty kind_opt =
  match kind_opt with
  | None -> ()
  | Some kind ->
    let ty_kind = TG.kind ty in
    if not (K.equal kind ty_kind)
    then
      Misc.fatal_errorf
        "Kind %a of type@ %a@ for %a@ doesn't match expected kind %a" K.print
        ty_kind TG.print ty Name.print name K.print kind

exception Missing_cmx_and_kind

(* CR mshinwell: [kind] could also take a [subkind] *)
let find_with_binding_time_and_mode' t name kind =
  match Name.Map.find name (names_to_types t) with
  | exception Not_found -> (
    let comp_unit = Name.compilation_unit name in
    if Compilation_unit.equal comp_unit (Compilation_unit.get_current_exn ())
    then
      let[@inline always] var var =
        Misc.fatal_errorf "Variable %a not bound in typing environment:@ %a"
          Variable.print var print t
      in
      let[@inline always] symbol sym =
        if Symbol.Set.mem sym t.defined_symbols
        then (
          let result = Lazy.force initial_symbol_type in
          check_optional_kind_matches name (Misc.fst3 result) kind;
          result)
        else
          Misc.fatal_errorf "Symbol %a not bound in typing environment:@ %a"
            Symbol.print sym print t
      in
      Name.pattern_match name ~var ~symbol
    else
      match (resolver t) comp_unit with
      | exception _ ->
        Misc.fatal_errorf "Exception in resolver@ Backtrace is: %s"
          (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))
      | None ->
        Name.pattern_match name
          ~symbol:(fun _ ->
            (* .cmx file missing *)
            let result = Lazy.force initial_symbol_type in
            check_optional_kind_matches name (Misc.fst3 result) kind;
            result)
          ~var:(fun _ ->
            match kind with
            | Some kind ->
              (* See comment below about binding times. *)
              ( MTC.unknown kind,
                Binding_time.imported_variables,
                Name_mode.in_types )
            | None -> raise Missing_cmx_and_kind)
      | Some t -> (
        match Name.Map.find name (names_to_types t) with
        | exception Not_found ->
          Name.pattern_match name
            ~symbol:(fun _ ->
              (* CR vlaviron: This could be an error, but it can actually occur
                 with predefined exceptions and maybe regular symbols too *)
              let result = Lazy.force initial_symbol_type in
              check_optional_kind_matches name (Misc.fst3 result) kind;
              result)
            ~var:(fun var ->
              Misc.fatal_errorf
                "Variable %a not bound in imported typing environment (maybe \
                 the wrong .cmx file is present?):@ %a"
                Variable.print var print t)
        | ty, _binding_time, _name_mode ->
          check_optional_kind_matches name ty kind;
          Name.pattern_match name
            ~symbol:(fun _ -> ty, Binding_time.symbols, Name_mode.normal)
            ~var:(fun _ ->
              (* Binding times for imported variables are meaningless outside
                 their original environment. Variables from foreign compilation
                 units are always out of scope, so their mode must be In_types
                 (we cannot rely on the scoping by binding time). *)
              ty, Binding_time.imported_variables, Name_mode.in_types)))
  | found ->
    let ty, _, _ = found in
    check_optional_kind_matches name ty kind;
    found

(* This version doesn't check min_binding_time. This ensures that no allocation
   occurs when we're not interested in the name mode. *)
let find_with_binding_time_and_mode_unscoped t name kind =
  try find_with_binding_time_and_mode' t name kind
  with Missing_cmx_and_kind ->
    Misc.fatal_errorf
      "Don't know kind of variable %a from another unit whose .cmx file is \
       unavailable"
      Name.print name

let find t name kind =
  let ty, _binding_time, _name_mode =
    find_with_binding_time_and_mode_unscoped t name kind
  in
  ty

let find_with_binding_time_and_mode t name kind =
  let ((ty, binding_time, mode) as found) =
    find_with_binding_time_and_mode_unscoped t name kind
  in
  let scoped_mode =
    Binding_time.With_name_mode.scoped_name_mode
      (Binding_time.With_name_mode.create binding_time mode)
      ~min_binding_time:t.min_binding_time
  in
  if Name_mode.equal mode scoped_mode
  then found
  else ty, binding_time, scoped_mode

let find_or_missing t name =
  match find_with_binding_time_and_mode' t name None with
  | ty, _, _ -> Some ty
  | exception Missing_cmx_and_kind -> None

let find_params t params =
  List.map
    (fun param ->
      let name = Bound_parameter.name param in
      let kind = Flambda_kind.With_subkind.kind (Bound_parameter.kind param) in
      find t name (Some kind))
    params

let binding_time_and_mode t name =
  if variable_is_from_missing_cmx_file t name
  then
    Binding_time.With_name_mode.create Binding_time.imported_variables
      Name_mode.in_types
  else
    Name.pattern_match name
      ~var:(fun _var ->
        let _typ, binding_time, name_mode =
          find_with_binding_time_and_mode t name None
        in
        Binding_time.With_name_mode.create binding_time name_mode)
      ~symbol:(fun _sym ->
        Binding_time.With_name_mode.create Binding_time.symbols Name_mode.normal)

let binding_time_and_mode_of_simple t simple =
  Simple.pattern_match simple
    ~const:(fun _ ->
      Binding_time.With_name_mode.create Binding_time.consts_and_discriminants
        Name_mode.normal)
    ~name:(fun name ~coercion:_ -> binding_time_and_mode t name)

let mem ?min_name_mode t name =
  Name.pattern_match name
    ~var:(fun _var ->
      let name_mode =
        match Name.Map.find name (names_to_types t) with
        | exception Not_found ->
          if Name.Set.mem name (t.get_imported_names ())
          then Some Name_mode.in_types
          else None
        | _ty, binding_time, name_mode ->
          let scoped_name_mode =
            Binding_time.With_name_mode.scoped_name_mode
              (Binding_time.With_name_mode.create binding_time name_mode)
              ~min_binding_time:t.min_binding_time
          in
          Some scoped_name_mode
      in
      match name_mode, min_name_mode with
      | None, _ -> false
      | Some _, None -> true
      | Some name_mode, Some min_name_mode -> begin
        match Name_mode.compare_partial_order min_name_mode name_mode with
        | None -> false
        | Some c -> c <= 0
      end)
    ~symbol:(fun sym ->
      (* CR mshinwell: This might not take account of symbols in missing .cmx
         files *)
      Symbol.Set.mem sym t.defined_symbols
      || Name.Set.mem name (t.get_imported_names ()))

let mem_simple ?min_name_mode t simple =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ -> mem ?min_name_mode t name)
    ~const:(fun _ -> true)

let with_current_level t ~current_level =
  let t = { t with current_level } in
  invariant t;
  t

let with_current_level_and_next_binding_time t ~current_level next_binding_time
    =
  let t = { t with current_level; next_binding_time } in
  invariant t;
  t

let with_aliases t ~aliases =
  let current_level = One_level.with_aliases t.current_level ~aliases in
  with_current_level t ~current_level

let cached t = One_level.just_after_level t.current_level

let add_variable_definition t var kind name_mode =
  (* We can add equations in our own compilation unit on variables and symbols
     defined in another compilation unit. However we can't define other
     compilation units' variables or symbols (except for predefined symbols such
     as exceptions) in our own compilation unit. *)
  let comp_unit = Variable.compilation_unit var in
  let this_comp_unit = Compilation_unit.get_current_exn () in
  if not (Compilation_unit.equal comp_unit this_comp_unit)
  then
    Misc.fatal_errorf
      "Cannot define a variable that belongs to a different compilation unit: \
       %a@ in environment:@ %a"
      Variable.print var print t;
  let name = Name.var var in
  if Flambda_features.check_invariants () && mem t name
  then
    Misc.fatal_errorf "Cannot rebind %a in environment:@ %a" Name.print name
      print t;
  let level =
    TEL.add_definition
      (One_level.level t.current_level)
      var kind t.next_binding_time
  in
  let just_after_level =
    Cached_level.add_or_replace_binding (cached t) name (MTC.unknown kind)
      t.next_binding_time name_mode
  in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level_and_next_binding_time t ~current_level
    (Binding_time.succ t.next_binding_time)

let add_symbol_definition t sym =
  (* CR mshinwell: check for redefinition when invariants enabled? *)
  let comp_unit = Symbol.compilation_unit sym in
  let this_comp_unit = Compilation_unit.get_current_exn () in
  if not (Compilation_unit.equal comp_unit this_comp_unit)
  then
    Misc.fatal_errorf
      "Cannot define symbol %a that belongs to a different compilation unit@ \
       (%a, current unit: %a) %b@ in environment:@ %a"
      Symbol.print sym Compilation_unit.print comp_unit Compilation_unit.print
      this_comp_unit
      (Compilation_unit.equal comp_unit this_comp_unit)
      print t;
  { t with defined_symbols = Symbol.Set.add sym t.defined_symbols }

let add_symbol_definitions t syms =
  { t with defined_symbols = Symbol.Set.union syms t.defined_symbols }

let add_symbol_projection t var proj =
  let level =
    TEL.add_symbol_projection (One_level.level t.current_level) var proj
  in
  let current_level =
    One_level.create (current_scope t) level
      ~just_after_level:(Cached_level.add_symbol_projection (cached t) var proj)
  in
  with_current_level t ~current_level

let find_symbol_projection t var =
  Cached_level.find_symbol_projection (cached t) var

let add_definition t (name : Bound_name.t) kind =
  let name_mode = Bound_name.name_mode name in
  Name.pattern_match (Bound_name.name name)
    ~var:(fun var -> add_variable_definition t var kind name_mode)
    ~symbol:(fun sym ->
      if not (Name_mode.equal name_mode Name_mode.normal)
      then
        Misc.fatal_errorf
          "Cannot define symbol %a with name mode that is not `normal'"
          Bound_name.print name;
      add_symbol_definition t sym)

let invariant_for_alias (t : t) name ty =
  (* Check that no canonical element gets an [Equals] type *)
  if Flambda_features.check_invariants () || true
  then
    match TG.get_alias_exn ty with
    | exception Not_found -> ()
    | alias ->
      assert (not (Simple.equal alias (Simple.name name)));
      let canonical =
        Aliases.get_canonical_ignoring_name_mode (aliases t) name
      in
      if Simple.equal canonical (Simple.name name)
      then
        Misc.fatal_errorf
          "There is about to be an [Equals] equation on canonical name %a@\n\
           equation: %a@\n\
           @."
          Name.print name TG.print ty

(* This is too costly to check, but it can be useful for debugging problems with
   canonical aliases. let invariant_for_aliases (t:t) = Name.Map.iter (fun name
   (ty, _, _) -> invariant_for_alias t name ty ) (names_to_types t) *)

let invariant_for_new_equation (t : t) name ty =
  if Flambda_features.check_invariants ()
  then begin
    invariant_for_alias t name ty;
    (* CR mshinwell: This should check that precision is not decreasing. *)
    let defined_names =
      Name_occurrences.create_names
        (Name.Set.union (name_domain t) (t.get_imported_names ()))
        Name_mode.in_types
    in
    (* CR mshinwell: It's a shame we can't check code IDs here. *)
    let free_names = Name_occurrences.without_code_ids (TG.free_names ty) in
    if not (Name_occurrences.subset_domain free_names defined_names)
    then
      let unbound_names = Name_occurrences.diff free_names defined_names in
      Misc.fatal_errorf "New equation@ %a@ =@ %a@ has unbound names@ (%a):@ %a"
        Name.print name TG.print ty Name_occurrences.print unbound_names print t
  end

let rec add_equation0 (t : t) name ty =
  (if Flambda_features.Debug.concrete_types_only_on_canonicals ()
  then
    let is_concrete =
      match TG.get_alias_exn ty with exception Not_found -> true | _ -> false
    in
    if is_concrete
    then
      let canonical =
        Aliases.get_canonical_ignoring_name_mode (aliases t) name
        |> Simple.without_coercion
      in
      if not (Simple.equal canonical (Simple.name name))
      then
        Misc.fatal_errorf
          "Trying to add equation giving concrete type on %a which is not \
           canonical (its canonical is %a): %a"
          Name.print name Simple.print canonical TG.print ty);
  invariant_for_new_equation t name ty;
  let level =
    TEL.add_or_replace_equation (One_level.level t.current_level) name ty
  in
  let just_after_level =
    Name.pattern_match name
      ~var:(fun var ->
        let just_after_level =
          if Compilation_unit.equal
               (Variable.compilation_unit var)
               (Compilation_unit.get_current_exn ())
          then
            Cached_level.replace_variable_binding
              (One_level.just_after_level t.current_level)
              var ty
          else
            Cached_level.add_or_replace_binding
              (One_level.just_after_level t.current_level)
              name ty Binding_time.imported_variables Name_mode.in_types
        in
        just_after_level)
      ~symbol:(fun _ ->
        let just_after_level =
          Cached_level.add_or_replace_binding
            (One_level.just_after_level t.current_level)
            name ty Binding_time.symbols Name_mode.normal
        in
        just_after_level)
  in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  let res = with_current_level t ~current_level in
  (* invariant_for_aliases res; *)
  res

and add_equation1 t name ty ~(meet_type : meet_type) =
  (if Flambda_features.check_invariants ()
  then
    let existing_ty = find t name None in
    if not (K.equal (TG.kind existing_ty) (TG.kind ty))
    then
      Misc.fatal_errorf
        "Cannot add equation %a = %a@ given existing binding %a = %a@ whose \
         type is of a different kind:@ %a"
        Name.print name TG.print ty Name.print name TG.print existing_ty print t
    (* XXX Needs to be guarded let free_names = Type_free_names.free_names ty in
       if not (Name_occurrences.subset_domain free_names (domain t)) then begin
       let unbound_names = Name_occurrences.diff free_names (domain t) in
       Misc.fatal_errorf "Cannot add equation, involving unbound names@ (%a),@
       on \ name@ %a =@ %a@ (free names %a) in environment with domain %a:@ %a"
       Name_occurrences.print unbound_names Name.print name TG.print ty
       Name_occurrences.print free_names Name_occurrences.print (domain t) print
       t end; *));
  (if Flambda_features.check_invariants ()
  then
    match TG.get_alias_exn ty with
    | exception Not_found -> ()
    | simple ->
      Simple.pattern_match simple
        ~name:(fun name' ~coercion:_ ->
          if Name.equal name name'
          then
            Misc.fatal_errorf
              "Directly recursive equation@ %a = %a@ disallowed:@ %a" Name.print
              name TG.print ty print t)
        ~const:(fun _ -> ()));
  let simple, t, ty =
    let aliases = aliases t in
    match TG.get_alias_exn ty with
    | exception Not_found ->
      (* Equations giving concrete types may only be added to the canonical
         element as known by the alias tracker (the actual canonical, ignoring
         any name modes). *)
      let canonical = Aliases.get_canonical_ignoring_name_mode aliases name in
      canonical, t, ty
    | alias_of ->
      (* Forget where [name] and [alias_of] came from---our job is now to record
         that they're equal. In general, they have canonical expressions [c_n]
         and [c_a], respectively, so what we ultimately need to record is that
         [c_n] = [c_a]. Clearly, only one of them can remain canonical, so we
         pick whichever was bound earlier. If [c_a] was bound earlier, then we
         demote [c_n] and give [name] the type "= c_a" (which will always be
         valid since [c_a] was bound earlier). Otherwise, we demote [c_a] and
         give [alias_of] the type "= c_n". *)
      let alias = Simple.name name in
      let kind = TG.kind ty in
      let binding_time_and_mode_alias = binding_time_and_mode t name in
      let binding_time_and_mode_alias_of =
        binding_time_and_mode_of_simple t alias_of
      in
      let ({ canonical_element; alias_of_demoted_element; t = aliases }
            : Aliases.add_result) =
        (* This may raise [Binding_time_resolver_failure]. *)
        Aliases.add ~binding_time_resolver:t.binding_time_resolver aliases
          ~element1:alias ~binding_time_and_mode1:binding_time_and_mode_alias
          ~element2:alias_of
          ~binding_time_and_mode2:binding_time_and_mode_alias_of
      in
      let t = with_aliases t ~aliases in
      (* We need to change the demoted alias's type to point to the new
         canonical element. *)
      let ty = TG.alias_type_of kind canonical_element in
      alias_of_demoted_element, t, ty
  in
  (* We have [(coerce <bare_lhs> <coercion>) : <ty>]. Thus [<bare_lhs> : (coerce
     <ty> <coercion>^-1)]. *)
  let bare_lhs = Simple.without_coercion simple in
  let coercion_from_bare_lhs_to_ty = Simple.coercion simple in
  let coercion_from_ty_to_bare_lhs =
    Coercion.inverse coercion_from_bare_lhs_to_ty
  in
  let ty = TG.apply_coercion ty coercion_from_ty_to_bare_lhs in
  (* Beware: if we're about to add the equation on a name which is different
     from the one that the caller passed in, then we need to make sure that the
     type we assign to that name is the most precise available. This
     necessitates calling [meet].

     For example, suppose [p] is defined earlier than [x], with [p] of type
     [Unknown] and [x] of type [ty]. If the caller says that the best type of
     [p] is now to be "= x", then this function will add an equation on [x]
     rather than [p], due to the definition ordering. However we should not just
     say that [x] has type "= p", as that would forget any existing information
     about [x]. Instead we should say that [x] has type "(= p) meet ty".

     Note also that [p] and [x] may have different name modes! *)
  let ty, t =
    let[@inline always] name eqn_name ~coercion =
      assert (Coercion.is_id coercion);
      (* true by definition *)
      if Name.equal name eqn_name
      then ty, t
      else
        let env = Meet_env.create t in
        let existing_ty = find t eqn_name (Some (TG.kind ty)) in
        match meet_type env ty existing_ty with
        | Bottom -> MTC.bottom (TG.kind ty), t
        | Ok (meet_ty, env_extension) ->
          meet_ty, add_env_extension t env_extension ~meet_type
    in
    Simple.pattern_match bare_lhs ~name ~const:(fun _ -> ty, t)
  in
  let[@inline always] name name ~coercion =
    assert (Coercion.is_id coercion);
    (* true by definition *)
    add_equation0 t name ty
  in
  Simple.pattern_match bare_lhs ~name ~const:(fun _ -> t)

and[@inline always] add_equation t name ty ~meet_type =
  match add_equation1 t name ty ~meet_type with
  | exception Binding_time_resolver_failure ->
    (* Addition of aliases between names that are both in external compilation
       units failed, e.g. due to a missing .cmx file. Simply drop the
       equation. *)
    t
  | t -> t

and add_env_extension t (env_extension : Typing_env_extension.t) ~meet_type =
  Typing_env_extension.fold
    ~equation:(fun name ty t -> add_equation t name ty ~meet_type)
    env_extension t

and add_env_extension_with_extra_variables t
    (env_extension : Typing_env_extension.With_extra_variables.t) ~meet_type =
  Typing_env_extension.With_extra_variables.fold
    ~variable:(fun var kind t ->
      add_variable_definition t var kind Name_mode.in_types)
    ~equation:(fun name ty t -> add_equation t name ty ~meet_type)
    env_extension t

(* These version is outside the [let rec] and thus does not cause [caml_apply*]
   to be used when calling from outside this module. *)
let add_equation t name ty ~meet_type = add_equation t name ty ~meet_type

let add_env_extension t env_extension = add_env_extension t env_extension

let add_env_extension_with_extra_variables t env_extension =
  add_env_extension_with_extra_variables t env_extension

let add_env_extension_from_level t level ~meet_type : t =
  let t =
    TEL.fold_on_defined_vars
      (fun var kind t -> add_variable_definition t var kind Name_mode.in_types)
      level t
  in
  let t =
    Name.Map.fold
      (fun name ty t -> add_equation t name ty ~meet_type)
      (TEL.equations level) t
  in
  Variable.Map.fold
    (fun var proj t -> add_symbol_projection t var proj)
    (TEL.symbol_projections level)
    t

let add_definitions_of_params t ~params =
  List.fold_left
    (fun t param ->
      let name =
        Bound_name.create (Bound_parameter.name param) Name_mode.normal
      in
      add_definition t name
        (Flambda_kind.With_subkind.kind (Bound_parameter.kind param)))
    t params

let check_params_and_types ~params ~param_types =
  if Flambda_features.check_invariants ()
     && List.compare_lengths params param_types <> 0
  then
    Misc.fatal_errorf
      "Mismatch between number of [params] and [param_types]:@ (%a)@ and@ %a"
      Bound_parameter.List.print params
      (Format.pp_print_list ~pp_sep:Format.pp_print_space TG.print)
      param_types

let add_equations_on_params t ~params ~param_types ~meet_type =
  check_params_and_types ~params ~param_types;
  List.fold_left2
    (fun t param param_type ->
      add_equation t (Bound_parameter.name param) param_type ~meet_type)
    t params param_types

let add_to_code_age_relation t ~new_code_id ~old_code_id =
  let code_age_relation =
    match old_code_id with
    | None -> t.code_age_relation
    | Some old_code_id ->
      Code_age_relation.add t.code_age_relation ~newer:new_code_id
        ~older:old_code_id
  in
  { t with code_age_relation }

let code_age_relation t = t.code_age_relation

let with_code_age_relation t code_age_relation = { t with code_age_relation }

(* CR mshinwell: Change the name of the labelled argument *)
let cut t ~unknown_if_defined_at_or_later_than:min_scope =
  let current_scope = current_scope t in
  if Scope.( > ) min_scope current_scope
  then TEL.empty
  else
    let _strictly_less, at_min_scope, strictly_greater =
      Scope.Map.split min_scope t.prev_levels
    in
    let at_or_after_cut =
      match at_min_scope with
      | None -> strictly_greater
      | Some typing_env_level ->
        Scope.Map.add min_scope typing_env_level strictly_greater
    in
    let at_or_after_cut =
      Scope.Map.add current_scope t.current_level at_or_after_cut
    in
    Scope.Map.fold
      (fun _scope one_level result ->
        TEL.concat result (One_level.level one_level))
      at_or_after_cut TEL.empty

let type_simple_in_term_exn t ?min_name_mode simple =
  (* If [simple] is a variable then it should not come from a missing .cmx file,
     since this function is only used for typing variables in terms, and even
     imported code is closed with respect to variables. This also means that the
     kind of such variables should always be inferrable, so we pass [None] to
     [find] below. *)
  let ty, _binding_time, name_mode_simple =
    let[@inline always] const const =
      ( MTC.type_for_const const,
        Binding_time.consts_and_discriminants,
        Name_mode.normal )
    in
    let[@inline always] name name ~coercion:_ =
      (* Applying coercion below *)
      find_with_binding_time_and_mode t name None
    in
    Simple.pattern_match simple ~const ~name
  in
  let ty =
    if Simple.has_coercion simple
    then TG.apply_coercion ty (Simple.coercion simple)
    else ty
  in
  let kind = TG.kind ty in
  let aliases_for_simple, min_binding_time =
    if Aliases.mem (aliases t) simple
    then aliases_with_min_binding_time t
    else
      Simple.pattern_match simple
        ~const:(fun _ -> aliases_with_min_binding_time t)
        ~name:(fun name ->
          Name.pattern_match name
            ~var:(fun var ~coercion:_ ->
              let comp_unit = Variable.compilation_unit var in
              if Compilation_unit.equal comp_unit
                   (Compilation_unit.get_current_exn ())
              then aliases_with_min_binding_time t
              else
                match (resolver t) comp_unit with
                | Some env -> aliases_with_min_binding_time env
                | None ->
                  Misc.fatal_errorf
                    "Error while looking up variable %a:@ No corresponding \
                     .cmx file was found"
                    Variable.print var)
            ~symbol:(fun _sym ~coercion:_ ->
              (* Symbols can't alias, so lookup in the current aliases is
                 fine *)
              aliases_with_min_binding_time t))
  in
  let min_name_mode =
    match min_name_mode with
    | None -> name_mode_simple
    | Some name_mode -> name_mode
  in
  match
    Aliases.get_canonical_element_exn
      ~binding_time_resolver:t.binding_time_resolver aliases_for_simple simple
      name_mode_simple ~min_name_mode ~min_binding_time
  with
  | exception Misc.Fatal_error ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf "\n%sContext is:%s typing environment@ %a\n"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      print t;
    Printexc.raise_with_backtrace Misc.Fatal_error bt
  | exception Binding_time_resolver_failure -> TG.alias_type_of kind simple
  | alias -> TG.alias_type_of kind alias

let get_canonical_simple_exn t ?min_name_mode ?name_mode_of_existing_simple
    simple =
  let aliases_for_simple, min_binding_time =
    if Aliases.mem (aliases t) simple
    then aliases_with_min_binding_time t
    else
      Simple.pattern_match simple
        ~const:(fun _ -> aliases_with_min_binding_time t)
        ~name:(fun name ~coercion:_ ->
          Name.pattern_match name
            ~var:(fun var ->
              let comp_unit = Variable.compilation_unit var in
              if Compilation_unit.equal comp_unit
                   (Compilation_unit.get_current_exn ())
              then aliases_with_min_binding_time t
              else
                match (resolver t) comp_unit with
                | Some env -> aliases_with_min_binding_time env
                | None ->
                  (* Transcript of Slack conversation relating to the next line:

                     mshinwell: @vlaviron Should it say "aliases t" perhaps?
                     There could be some weird cases here, e.g. if we are
                     building c.cmx and b.cmx is unavailable, but if b.cmx were
                     available it would tell us that this var is an alias to
                     something in a.cmx, which is available I'm concerned that
                     this could lead to not propagating a constraint, e.g. if
                     the var in c.cmx is found to be bottom, it should make the
                     one in a.cmx bottom too, but it won't as the chain is
                     broken. That could be observable if something else in c.cmx
                     directly points at a.cmx. Maybe this won't matter in
                     practice because the new type should always be more
                     precise, but I'm unsure. And what happens if it's not?

                     vlaviron: That's actually fine, I think: if you hide b.cmx,
                     then c.cmx does not know that the two variables are
                     aliased, so it will be less precise, but that's all Since
                     we've fixed Get_tag, I don't think loss of precision is a
                     soundness issue anymore And the new type should always be
                     the result of a meet with the previous type, I think, so it
                     should never be less precise For the aliases issue, I think
                     using aliases t is fine. It would only be a problem if we
                     had a way to learn later that the variable is actually an
                     alias, but that would only happen if for some reason we
                     later successfully load the missing cmx. *)
                  aliases_with_min_binding_time t)
            ~symbol:(fun _sym ->
              (* Symbols can't alias, so lookup in the current aliases is
                 fine *)
              aliases_with_min_binding_time t))
  in
  let name_mode_simple =
    let in_types =
      Simple.pattern_match simple
        ~const:(fun _ -> false)
        ~name:(fun name ~coercion:_ -> variable_is_from_missing_cmx_file t name)
    in
    if in_types
    then Name_mode.in_types
    else
      match name_mode_of_existing_simple with
      | Some name_mode -> name_mode
      | None ->
        Binding_time.With_name_mode.name_mode
          (binding_time_and_mode_of_simple t simple)
  in
  let min_name_mode =
    match min_name_mode with
    | None -> name_mode_simple
    | Some name_mode -> name_mode
  in
  match
    Aliases.get_canonical_element_exn
      ~binding_time_resolver:t.binding_time_resolver aliases_for_simple simple
      name_mode_simple ~min_name_mode ~min_binding_time
  with
  | exception Misc.Fatal_error ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf "\n%sContext is:%s typing environment@ %a\n"
      (Flambda_colours.error ())
      (Flambda_colours.normal ())
      print t;
    Printexc.raise_with_backtrace Misc.Fatal_error bt
  | exception Binding_time_resolver_failure -> simple
  | alias -> alias

let get_alias_then_canonical_simple_exn t ?min_name_mode
    ?name_mode_of_existing_simple typ =
  let simple = TG.get_alias_exn typ in
  get_canonical_simple_exn t ?min_name_mode ?name_mode_of_existing_simple simple

let aliases_of_simple t ~min_name_mode simple =
  Aliases.get_aliases (aliases t) simple
  |> Aliases.Alias_set.filter ~f:(fun alias ->
         let name_mode =
           Binding_time.With_name_mode.name_mode
             (binding_time_and_mode_of_simple t alias)
         in
         match Name_mode.compare_partial_order name_mode min_name_mode with
         | None -> false
         | Some c -> c >= 0)

let aliases_of_simple_allowable_in_types t simple =
  aliases_of_simple t ~min_name_mode:Name_mode.in_types simple

let closure_env t = { t with min_binding_time = t.next_binding_time }

let rec free_names_transitive_of_type_of_name t name ~result =
  let result = Name_occurrences.add_name result name Name_mode.in_types in
  if variable_is_from_missing_cmx_file t name
  then result
  else
    let typ = find t name None in
    free_names_transitive0 t typ ~result

and free_names_transitive0 t typ ~result =
  let free_names = TG.free_names typ in
  let to_traverse = Name_occurrences.diff free_names result in
  if Name_occurrences.is_empty to_traverse
  then result
  else
    Name_occurrences.fold_names to_traverse ~init:result ~f:(fun result name ->
        free_names_transitive_of_type_of_name t name ~result)

let free_names_transitive t typ =
  free_names_transitive0 t typ ~result:Name_occurrences.empty

let clean_for_export t ~reachable_names =
  let current_level =
    One_level.clean_for_export t.current_level ~reachable_names
  in
  { t with current_level }
