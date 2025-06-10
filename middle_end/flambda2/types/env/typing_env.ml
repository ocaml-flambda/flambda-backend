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

module K = Flambda_kind
module MTC = More_type_creators
module TG = Type_grammar
module TEL = Typing_env_level

module One_level : sig
  type t

  val print : min_binding_time:Binding_time.t -> Format.formatter -> t -> unit

  val create : Scope.t -> TEL.t -> just_after_level:Cached_level.t -> t

  val create_empty : Scope.t -> t

  val scope : t -> Scope.t

  val level : t -> TEL.t

  val just_after_level : t -> Cached_level.t

  val with_aliases : t -> aliases:Aliases.t -> t

  val is_empty : t -> bool

  val clean_for_export : t -> reachable_names:Name_occurrences.t -> t

  val remove_unused_value_slots_and_shortcut_aliases :
    t -> used_value_slots:Value_slot.Set.t -> t

  val canonicalise : t -> Simple.t -> Simple.t

  val bump_scope : t -> t
end = struct
  type t =
    { scope : Scope.t;
      level : TEL.t;
      just_after_level : Cached_level.t
    }

  let print ~min_binding_time ppf { scope; level; just_after_level } =
    let restrict_to = TEL.defined_names level in
    if Name.Set.is_empty restrict_to
    then
      Format.fprintf ppf "@[<hov 0>((scope@ %a)@ %a)@]" Scope.print scope
        TEL.print level
    else
      Format.fprintf ppf
        "@[<hov 0>@[<hov 1>((scope@ %a)@ (defined_vars@ %a))@]@ %a@]"
        Scope.print scope
        (Cached_level.print_name_modes ~restrict_to ~min_binding_time)
        just_after_level TEL.print level

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

  let clean_for_export t ~reachable_names =
    { t with
      just_after_level =
        Cached_level.clean_for_export t.just_after_level ~reachable_names
    }

  let remove_unused_value_slots_and_shortcut_aliases t ~used_value_slots =
    let just_after_level =
      Cached_level.remove_unused_value_slots_and_shortcut_aliases
        t.just_after_level ~used_value_slots
    in
    { t with just_after_level }

  let canonicalise t = Cached_level.canonicalise t.just_after_level

  let bump_scope t = { t with scope = Scope.next t.scope }
end

type t =
  { resolver : Compilation_unit.t -> serializable option;
    binding_time_resolver : Name.t -> Binding_time.With_name_mode.t;
    get_imported_names : unit -> Name.Set.t;
    defined_symbols : Symbol.Set.t;
    code_age_relation : Code_age_relation.t;
    prev_levels : One_level.t list;
    (* [prev_levels] is sorted with the greatest scope at the head of the
       list *)
    current_level : One_level.t;
    next_binding_time : Binding_time.t;
    min_binding_time : Binding_time.t;
        (* Earlier variables have mode In_types *)
    is_bottom : bool;
    extensions :
      (Continuation.t * Apply_cont_rewrite_id.Set.t) Database.Extension_id.Map.t
  }

and serializable =
  { defined_symbols_without_equations : Symbol.t list;
    code_age_relation : Code_age_relation.t;
    just_after_level : Cached_level.t
  }

type typing_env = t

let is_empty t =
  One_level.is_empty t.current_level
  && (match t.prev_levels with [] -> true | _ :: _ -> false)
  && Symbol.Set.is_empty t.defined_symbols

let make_bottom t = { t with is_bottom = true }

let is_bottom t = t.is_bottom

let cached t = One_level.just_after_level t.current_level

let aliases t = Cached_level.aliases (cached t)

let database t = Cached_level.database (cached t)

let extensions t = Cached_level.extensions (cached t)

(* CR-someday mshinwell: Should print name occurrence kinds *)
let [@ocamlformat "disable"] print ppf
      ({ resolver = _; binding_time_resolver = _; get_imported_names = _;
         prev_levels; current_level; next_binding_time = _;
         defined_symbols; code_age_relation; min_binding_time;
         is_bottom; extensions = _;
       } as t) =
  if is_empty t then
    Format.pp_print_string ppf "Empty"
  else if is_bottom then
    Format.pp_print_string ppf "Bottom"
  else
    let levels =
      current_level :: prev_levels
    in
    let levels =
      List.filter (fun level -> not (One_level.is_empty level))
        levels
    in
    Format.fprintf ppf
      "@[<hov 1>(\
         @[<hov 1>(defined_symbols@ %a)@]@ \
         @[<hov 1>(code_age_relation@ %a)@]@ \
         @[<hov 1>(levels@ %a)@]@ \
         @[<hov 1>(aliases@ %a)@]@ \
         @[<hov 1>(extensions@ %a)@]@ \
         @[<hov 1>(database@ %a)@]@ \
       )@]"
      Symbol.Set.print defined_symbols
      Code_age_relation.print code_age_relation
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         (One_level.print ~min_binding_time))
      levels
      Aliases.print (aliases t)
      (Database.Extension_id.Map.print Typing_env_extension.print) (extensions t)
      (Database.print) (database t)

let [@ocamlformat "disable"] print_serializable ppf
    { defined_symbols_without_equations; code_age_relation; just_after_level } =
  Format.fprintf ppf
    "@[<hov 1>(\
        @[<hov 1>(defined_symbols_without_equations@ (%a))@]@ \
        @[<hov 1>(code_age_relation@ %a)@]@ \
        @[<hov 1>(type_equations@ %a)@]@ \
        @[<hov 1>(aliases@ %a)@]\
        )@]"
    (Format.pp_print_list ~pp_sep:Format.pp_print_space Symbol.print) defined_symbols_without_equations
    Code_age_relation.print code_age_relation
    (Name.Map.print (fun ppf (ty, _bt_and_mode) -> TG.print ppf ty))
    (Cached_level.names_to_types just_after_level)
    Aliases.print (Cached_level.aliases just_after_level)

module Meet_or_join_env_base : sig
  type t

  val print : Format.formatter -> t -> unit

  val create : typing_env -> t

  val env : t -> typing_env

  val now_meeting_or_joining : t -> Simple.t -> Simple.t -> t

  val already_meeting_or_joining : t -> Simple.t -> Simple.t -> bool
end = struct
  type t =
    { env : typing_env;
      already_meeting_or_joining : Name.Pair.Set.t
    }

  let [@ocamlformat "disable"] print ppf { env; already_meeting_or_joining; } =
    Format.fprintf ppf
      "@[<hov 1>(\
         @[<hov 1>(env@ %a)@]@ \
         @[<hov 1>(already_meeting_or_joining@ %a)@])\
       @]"
      print env
      Name.Pair.Set.print already_meeting_or_joining

  let create env = { env; already_meeting_or_joining = Name.Pair.Set.empty }

  let env t = t.env

  let already_meeting_or_joining_names t name1 name2 =
    Name.Pair.Set.mem (name1, name2) t.already_meeting_or_joining
    || Name.Pair.Set.mem (name2, name1) t.already_meeting_or_joining

  let already_meeting_or_joining t simple1 simple2 =
    let const _const = false in
    Simple.pattern_match simple1 ~const ~name:(fun name1 ~coercion:_ ->
        Simple.pattern_match simple2 ~const ~name:(fun name2 ~coercion:_ ->
            already_meeting_or_joining_names t name1 name2))

  let now_meeting_or_joining_names t name1 name2 =
    if already_meeting_or_joining_names t name1 name2
    then
      Misc.fatal_errorf "Already meeting_or_joining %a and %a:@ %a" Name.print
        name1 Name.print name2 print t;
    let already_meeting_or_joining =
      Name.Pair.Set.add (name1, name2) t.already_meeting_or_joining
    in
    { t with already_meeting_or_joining }

  let now_meeting_or_joining t simple1 simple2 =
    let const _const = t in
    Simple.pattern_match simple1 ~const ~name:(fun name1 ~coercion:_ ->
        Simple.pattern_match simple2 ~const ~name:(fun name2 ~coercion:_ ->
            now_meeting_or_joining_names t name1 name2))
end

type 'a meet_return_value =
  | Left_input
  | Right_input
  | Both_inputs
  | New_result of 'a

type meet_type = t -> TG.t -> TG.t -> (TG.t meet_return_value * t) Or_bottom.t

module Join_env : sig
  type t

  val print : Format.formatter -> t -> unit

  val create : typing_env -> left_env:typing_env -> right_env:typing_env -> t

  val target_join_env : t -> typing_env

  val left_join_env : t -> typing_env

  val right_join_env : t -> typing_env

  type now_joining_result =
    | Continue of t
    | Stop

  val now_joining : t -> Simple.t -> Simple.t -> now_joining_result

  val already_joining : t -> Simple.t -> Simple.t -> bool
end = struct
  type t =
    { central_env : Meet_or_join_env_base.t;
      left_join_env : typing_env;
      right_join_env : typing_env;
      depth : int
    }

  let print ppf { central_env; left_join_env; right_join_env; depth } =
    let join_env name ppf env =
      Format.fprintf ppf "@ @[<hov 1>(%s@ %a)@]@" name print env
    in
    Format.fprintf ppf
      "@[<hov 1>(@[<hov 1>(central_env@ %a)@]%a%a@ (depth %d))@]"
      Meet_or_join_env_base.print central_env (join_env "left_join_env")
      left_join_env
      (join_env "right_join_env")
      right_join_env depth

  let create central_env ~left_env ~right_env =
    { central_env = Meet_or_join_env_base.create central_env;
      left_join_env = left_env;
      right_join_env = right_env;
      depth = 0
    }

  let target_join_env t = Meet_or_join_env_base.env t.central_env

  let left_join_env t = t.left_join_env

  let right_join_env t = t.right_join_env

  type now_joining_result =
    | Continue of t
    | Stop

  let now_joining t simple1 simple2 =
    if t.depth >= Flambda_features.join_depth ()
    then Stop
    else
      Continue
        { t with
          central_env =
            Meet_or_join_env_base.now_meeting_or_joining t.central_env simple1
              simple2;
          depth = t.depth + 1
        }

  let already_joining { central_env; _ } simple1 simple2 =
    Meet_or_join_env_base.already_meeting_or_joining central_env simple1 simple2
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
    match
      Name.Map.find name (Cached_level.names_to_types t.just_after_level)
    with
    | exception Not_found ->
      Misc.fatal_errorf "Binding time resolver cannot find name %a in:@ %a"
        Name.print name print_serializable t
    | _, binding_time_and_mode -> binding_time_and_mode)

let resolver t = t.resolver

let code_age_relation_resolver t comp_unit =
  match t.resolver comp_unit with
  | None -> None
  | Some t -> Some t.code_age_relation

let current_scope t = One_level.scope t.current_level

let create ~resolver ~get_imported_names =
  { resolver;
    binding_time_resolver = binding_time_resolver resolver;
    get_imported_names;
    prev_levels = [];
    (* Since [Scope.prev] may be used in the simplifier on this scope, in order
       to allow an efficient implementation of [cut] (see below), we always
       increment the scope by one here. *)
    current_level = One_level.create_empty (Scope.next Scope.initial);
    next_binding_time = Binding_time.earliest_var;
    defined_symbols = Symbol.Set.empty;
    code_age_relation = Code_age_relation.empty;
    min_binding_time = Binding_time.earliest_var;
    extensions = Database.Extension_id.Map.empty;
    is_bottom = false
  }

let increment_scope t =
  let current_scope = current_scope t in
  let prev_levels = t.current_level :: t.prev_levels in
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
  MTC.unknown K.value, Binding_time.With_name_mode.symbols

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

(* CR-someday mshinwell: [kind] could also take a [subkind] *)
let find_with_binding_time_and_mode' t name kind =
  (* Note that [Pre_serializable] (below) assumes this function only looks up
     types of names in the cache for the current level. *)
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
          check_optional_kind_matches name (fst initial_symbol_type) kind;
          initial_symbol_type)
        else
          Misc.fatal_errorf "Symbol %a not bound in typing environment:@ %a"
            Symbol.print sym print t
      in
      Name.pattern_match name ~var ~symbol
    else
      match (resolver t) comp_unit with
      | exception exn ->
        Misc.fatal_errorf "Exception in resolver: %s@ Backtrace is: %s"
          (Printexc.to_string exn)
          (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()))
      | None ->
        Name.pattern_match name
          ~symbol:(fun _ ->
            (* .cmx file missing *)
            check_optional_kind_matches name (fst initial_symbol_type) kind;
            initial_symbol_type)
          ~var:(fun _ ->
            match kind with
            | Some kind ->
              (* See comment below about binding times. *)
              MTC.unknown kind, Binding_time.With_name_mode.imported_variables
            | None -> raise Missing_cmx_and_kind)
      | Some t -> (
        match
          Name.Map.find name (Cached_level.names_to_types t.just_after_level)
        with
        | exception Not_found ->
          Name.pattern_match name
            ~symbol:(fun symbol ->
              (* The symbol has no equation. Check that it is defined and return
                 the default symbol type. *)
              if List.mem symbol t.defined_symbols_without_equations
              then (
                check_optional_kind_matches name (fst initial_symbol_type) kind;
                initial_symbol_type)
              else
                Misc.fatal_errorf "Reference to undefined symbol %a"
                  Symbol.print symbol)
            ~var:(fun var ->
              Misc.fatal_errorf
                "Variable %a not bound in imported typing environment (maybe \
                 the wrong .cmx file is present?):@ %a"
                Variable.print var print_serializable t)
        | type_and_binding_time ->
          (* All variables in exported maps already have the right name mode
             (see [Cached_level.clean_for_export]) *)
          type_and_binding_time))
  | found ->
    let ty, binding_time_and_mode = found in
    check_optional_kind_matches name ty kind;
    if t.is_bottom then MTC.bottom_like ty, binding_time_and_mode else found

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
  let ty, _binding_time_and_mode =
    find_with_binding_time_and_mode_unscoped t name kind
  in
  ty

let find_with_binding_time_and_mode t name kind =
  let ((ty, binding_time_and_mode) as found) =
    find_with_binding_time_and_mode_unscoped t name kind
  in
  let scoped_mode =
    Binding_time.With_name_mode.scoped_name_mode binding_time_and_mode
      ~min_binding_time:t.min_binding_time
  in
  if Name_mode.equal
       (Binding_time.With_name_mode.name_mode binding_time_and_mode)
       scoped_mode
  then found
  else
    ( ty,
      Binding_time.With_name_mode.create
        (Binding_time.With_name_mode.binding_time binding_time_and_mode)
        scoped_mode )

let find_or_missing t name =
  match find_with_binding_time_and_mode' t name None with
  | ty, _ -> Some ty
  | exception Missing_cmx_and_kind -> None

let find_params t params =
  List.map
    (fun param ->
      let name = Bound_parameter.name param in
      let kind = Flambda_kind.With_subkind.kind (Bound_parameter.kind param) in
      find t name (Some kind))
    (Bound_parameters.to_list params)

let binding_time_and_mode t name =
  Name.pattern_match name
    ~var:(fun var ->
      let comp_unit = Variable.compilation_unit var in
      if Compilation_unit.is_current comp_unit
      then
        let _typ, binding_time_and_mode =
          find_with_binding_time_and_mode t name None
        in
        binding_time_and_mode
      else Binding_time.With_name_mode.imported_variables)
    ~symbol:(fun _sym -> Binding_time.With_name_mode.symbols)

let binding_time_and_mode_of_simple t simple =
  Simple.pattern_match simple
    ~const:(fun _ -> Binding_time.With_name_mode.consts)
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
        | _ty, binding_time_and_mode ->
          let scoped_name_mode =
            Binding_time.With_name_mode.scoped_name_mode binding_time_and_mode
              ~min_binding_time:t.min_binding_time
          in
          Some scoped_name_mode
      in
      match name_mode, min_name_mode with
      | None, _ -> false
      | Some _, None -> true
      | Some name_mode, Some min_name_mode -> (
        match Name_mode.compare_partial_order min_name_mode name_mode with
        | None -> false
        | Some c -> c <= 0))
    ~symbol:(fun sym ->
      (* CR mshinwell: This might not take account of symbols in missing .cmx
         files *)
      Symbol.Set.mem sym t.defined_symbols
      || Name.Set.mem name (t.get_imported_names ()))

let mem_simple ?min_name_mode t simple =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ -> mem ?min_name_mode t name)
    ~const:(fun _ -> true)

let alias_is_bound_strictly_earlier t ~bound_name ~alias =
  let time_of_name =
    binding_time_and_mode t bound_name
    |> Binding_time.With_name_mode.binding_time
  in
  let time_of_alias =
    binding_time_and_mode_of_simple t alias
    |> Binding_time.With_name_mode.binding_time
  in
  Binding_time.strictly_earlier time_of_alias ~than:time_of_name

let with_current_level t ~current_level = { t with current_level }

let with_current_level_and_next_binding_time t ~current_level next_binding_time
    =
  { t with current_level; next_binding_time }

let with_aliases t ~aliases =
  let current_level = One_level.with_aliases t.current_level ~aliases in
  with_current_level t ~current_level

let with_database_and_aliases t ~database ~aliases =
  let just_after_level =
    Cached_level.with_database_and_aliases (cached t) ~database ~aliases
  in
  let level = One_level.level t.current_level in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  with_current_level t ~current_level

let with_extension t extension =
  let extension_id = Database.Extension_id.create () in
  let just_after_level =
    Cached_level.add_or_replace_extension (cached t) extension_id extension
  in
  let level = One_level.level t.current_level in
  let current_level =
    One_level.create (current_scope t) level ~just_after_level
  in
  extension_id, with_current_level t ~current_level

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
  (* CR-someday mshinwell: check for redefinition when invariants enabled? *)
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
  if Flambda_features.check_light_invariants ()
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

let invariant_for_new_equation (t : t) name ty =
  if Flambda_features.check_invariants ()
  then (
    invariant_for_alias t name ty;
    let defined_names =
      Name_occurrences.create_names
        (Name.Set.union (name_domain t) (t.get_imported_names ()))
        Name_mode.in_types
    in
    let free_names = Name_occurrences.without_code_ids (TG.free_names ty) in
    if not (Name_occurrences.subset_domain free_names defined_names)
    then
      let unbound_names =
        Name_occurrences.diff free_names ~without:defined_names
      in
      Misc.fatal_errorf "New equation@ %a@ =@ %a@ has unbound names@ (%a):@ %a"
        Name.print name TG.print ty Name_occurrences.print unbound_names print t)

exception Bottom_equation

let replace_equation (t : t) name ty =
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
  with_current_level t ~current_level

let aliases_add t ~canonical_element1 ~canonical_element2 =
  (* This may raise [Binding_time_resolver_failure]. *)
  Aliases.add ~binding_time_resolver:t.binding_time_resolver (aliases t)
    ~binding_times_and_modes:(names_to_types t) ~canonical_element1
    ~canonical_element2

let replace_concrete_equation t name ty =
  match TG.must_be_singleton ty with
  | None ->
    (* [ty] must be a concrete type. *)
    (match TG.get_alias_opt ty with
    | None -> ()
    | Some alias ->
      Misc.fatal_errorf "Expected concrete type for %a but got an alias to %a"
        Name.print name Simple.print alias);
    replace_equation t name ty
  | Some const -> (
    match
      aliases_add t ~canonical_element1:(Simple.name name)
        ~canonical_element2:(Simple.const const)
    with
    | Bottom ->
      Misc.fatal_error "Unexpected bottom while adding alias to constant"
    | exception Binding_time_resolver_failure ->
      (* This should only happen when adding aliases between names defined in
         external compilation units, but we are adding an alias to a
         constant. *)
      Misc.fatal_error
        "Unexpected resolver failure while adding alias to constant"
    | Ok { canonical_element; demoted_name; t = aliases } ->
      if (not (Name.equal demoted_name name))
         || not (Simple.equal canonical_element (Simple.const const))
      then Misc.fatal_error "Unexpected demotion of constant.";
      let kind = MTC.kind_for_const const in
      let ty = TG.alias_type_of kind canonical_element in
      let t = with_aliases t ~aliases in
      replace_equation t demoted_name ty)

let add_concrete_equation_on_canonical ~raise_on_bottom t simple ty
    ~(meet_type : meet_type) =
  (* When adding a type to a canonical name, we need to call [meet] with the
     existing type for that name in order to ensure we record the most precise
     type available.

     For example, suppose [p] is defined earlier than [x], with [p] of type
     [ty1] and [x] of type [ty2]. If the caller says that the type of [p] is now
     to be "= x", then we will instead add a type "= p" on [x] and demote [x] to
     [p], due to the definition ordering. We then need to record the information
     that [p] now has type [ty1 meet ty2], otherwise the type [ty2] would be
     lost.

     If instead we say that the type of [p] is to be "= c", where [c] is a
     constant, we will add the type "= c" to [p] and demote [p] to [c]. We have
     no type to record for [c], however we still need to check that [c] is
     compatible with the previous type of [p].

     Note also that [p] and [x] may have different name modes! *)
  Simple.pattern_match simple
    ~const:(fun const ->
      match meet_type t ty (MTC.type_for_const const) with
      | Ok (_, env) -> env
      | Bottom -> if raise_on_bottom then raise Bottom_equation else t)
    ~name:(fun name ~coercion ->
      (* If [(coerce name coercion)] has type [ty], then [name] has type
         [(coerce ty coercion^-1)]. *)
      let ty = TG.apply_coercion ty (Coercion.inverse coercion) in
      (* Note: this will check that the [existing_ty] has the expected kind. *)
      let existing_ty = find t name (Some (TG.kind ty)) in
      match meet_type t ty existing_ty with
      | Bottom ->
        if raise_on_bottom
        then raise Bottom_equation
        else replace_equation t name (MTC.bottom (TG.kind ty))
      | Ok ((Right_input | Both_inputs), env) -> env
      | Ok (Left_input, env) -> replace_concrete_equation env name ty
      | Ok (New_result ty', env) -> replace_concrete_equation env name ty')

let record_demotion ~raise_on_bottom t kind demoted canonical ~meet_type =
  (* We have demoted [demoted], which used to be canonical, to [canonical] in
     the aliases structure.

     We now need to record that information in the types structure, and add the
     previous type of [demoted] to [canonical] to ensure we do not lose
     information that was only stored on the type of [demoted]. *)
  let ty_of_demoted = find t demoted kind in
  (if Flambda_features.check_light_invariants ()
  then
    match TG.get_alias_opt ty_of_demoted with
    | None -> ()
    | Some alias ->
      Misc.fatal_errorf
        "Expected %a to have a concrete type, not an alias type to %a"
        Name.print demoted Simple.print alias);
  let kind = TG.kind ty_of_demoted in
  let t = replace_equation t demoted (TG.alias_type_of kind canonical) in
  add_concrete_equation_on_canonical ~raise_on_bottom t canonical ty_of_demoted
    ~meet_type

let get_canonical_simple_ignoring_name_mode t simple =
  Simple.pattern_match simple
    ~const:(fun _ -> simple)
    ~name:(fun name ~coercion ->
      let canonical_of_name =
        Aliases.get_canonical_ignoring_name_mode (aliases t) name
      in
      Simple.apply_coercion_exn canonical_of_name coercion)

let rebuild_types ~raise_on_bottom t demotions ~meet_type =
  Name.Map.fold
    (fun demoted _ t ->
      let canonical =
        get_canonical_simple_ignoring_name_mode t (Simple.name demoted)
      in
      record_demotion ~raise_on_bottom t None demoted canonical ~meet_type)
    demotions t

let rebuild_once ~raise_on_bottom t demotions ~meet_type =
  let aliases_before_rebuild = aliases t in
  let database_before_rebuild = database t in
  match
    Database.rebuild ~binding_time_resolver:t.binding_time_resolver
      ~binding_times_and_modes:(names_to_types t) aliases_before_rebuild
      database_before_rebuild demotions
  with
  | Bottom -> if raise_on_bottom then raise Bottom_equation else t
  | Ok (database_after_rebuild, aliases_after_rebuild) ->
    let t =
      with_database_and_aliases t ~database:database_after_rebuild
        ~aliases:aliases_after_rebuild
    in
    let demotions_from_rebuild =
      Aliases.diff ~later:aliases_after_rebuild ~earlier:aliases_before_rebuild
    in
    if Name.Map.is_empty demotions_from_rebuild
    then t
    else rebuild_types ~raise_on_bottom t demotions_from_rebuild ~meet_type

let rec rebuild ~raise_on_bottom t ~cut_after ~meet_type =
  let aliases = aliases t in
  let demotions = Aliases.diff ~later:aliases ~earlier:cut_after in
  if Name.Map.is_empty demotions
  then t
  else
    let t = rebuild_once ~raise_on_bottom t demotions ~meet_type in
    rebuild ~raise_on_bottom t ~cut_after:aliases ~meet_type

let add_alias_between_canonicals ~raise_on_bottom t kind canonical_element1
    canonical_element2 ~meet_type =
  (* We are adding an equality between two canonical simples [canonical1] and
     [canonical2].

     We'll ask the aliases structure to record the equality and determine which
     of [canonical1] or [canonical2] should remain canonical, then forward to
     [record_demotion] which takes care of recording an alias type on the
     demoted element and updating the type of the element that remains
     canonical. *)
  if Simple.equal canonical_element1 canonical_element2
  then t
  else
    match aliases_add t ~canonical_element1 ~canonical_element2 with
    | Bottom -> if raise_on_bottom then raise Bottom_equation else t
    | exception Binding_time_resolver_failure ->
      (* Addition of aliases between names that are both in external compilation
         units failed, e.g. due to a missing .cmx file. Simply drop the
         equation. *)
      t
    | Ok { demoted_name; canonical_element; t = aliases } ->
      let t = with_aliases t ~aliases in
      record_demotion ~raise_on_bottom t (Some kind) demoted_name
        canonical_element ~meet_type

let add_equation_on_canonical ~raise_on_bottom t simple ty ~meet_type =
  (* We are adding a type [ty] to [simple], which must be canonical. There are
     two general cases to consider:

     - Either [ty] is a concrete (non-alias) type, to be recorded in the types
     structure on the [canonical_simple];

     - or [ty] is an alias "= alias" to another simple, to be recorded in the
     aliases structure. *)
  match TG.get_alias_opt ty with
  | None ->
    add_concrete_equation_on_canonical ~raise_on_bottom t simple ty ~meet_type
  | Some alias ->
    let alias = get_canonical_simple_ignoring_name_mode t alias in
    add_alias_between_canonicals ~raise_on_bottom t (TG.kind ty) simple alias
      ~meet_type

let add_equation_on_simple ~raise_on_bottom t simple ty ~meet_type =
  let canonical = get_canonical_simple_ignoring_name_mode t simple in
  add_equation_on_canonical ~raise_on_bottom t canonical ty ~meet_type

let add_equation ~raise_on_bottom t name ty ~meet_type =
  add_equation_on_simple ~raise_on_bottom t (Simple.name name) ty ~meet_type

let add_relation ~raise_on_bottom t property ~scrutinee value =
  let scrutinee = get_canonical_simple_ignoring_name_mode t scrutinee in
  let value = get_canonical_simple_ignoring_name_mode t value in
  match
    Database.add_property ~binding_time_resolver:t.binding_time_resolver
      ~binding_times_and_modes:(names_to_types t) (aliases t) (database t)
      property ~arg:scrutinee ~result:value
  with
  | Or_bottom.Bottom ->
    if raise_on_bottom then raise Bottom_equation else make_bottom t
  | Or_bottom.Ok (database, aliases) ->
    with_database_and_aliases t ~database ~aliases

let check_relation t property ~scrutinee value =
  let scrutinee = get_canonical_simple_ignoring_name_mode t scrutinee in
  let value = get_canonical_simple_ignoring_name_mode t value in
  match
    Database.add_property ~binding_time_resolver:t.binding_time_resolver
      ~binding_times_and_modes:(names_to_types t) (aliases t) (database t)
      property ~arg:scrutinee ~result:value
  with
  | Or_bottom.Bottom -> false
  | Or_bottom.Ok (database0, aliases0) ->
    database0 == database t && aliases0 == aliases t

let add_env_extension ~raise_on_bottom t
    (env_extension : Typing_env_extension.t) ~meet_type =
  Typing_env_extension.fold
    ~equation:(fun name ty t ->
      add_equation ~raise_on_bottom t name ty ~meet_type)
    ~relation:(fun fn ~arg ~result t ->
      add_relation ~raise_on_bottom t fn ~scrutinee:(Simple.name arg) result)
    env_extension t

let add_env_extension_with_extra_variables t
    (env_extension : Typing_env_extension.With_extra_variables.t) ~meet_type =
  Typing_env_extension.With_extra_variables.fold
    ~variable:(fun var kind t ->
      add_variable_definition t var kind Name_mode.in_types)
    ~equation:(fun name ty t ->
      try add_equation ~raise_on_bottom:true t name ty ~meet_type
      with Bottom_equation -> make_bottom t)
    env_extension t

let reduce_once ~raise_on_bottom ~meet_type database_level t =
  let t =
    Database.Extension_id.Set.fold
      (fun extension_id t ->
        (* There is no need to keep track of the definition of the
           [extension_id] once it has been activated, but there should also be
           no harm in doing so as a active extension will not be re-activated by
           the database. *)
        match Cached_level.find_extension (cached t) extension_id with
        | None -> t
        | Some extension ->
          add_env_extension ~raise_on_bottom t extension ~meet_type)
      (Database.Level.activated_extensions database_level)
      t
  in
  Database.Level.fold_constant_properties
    (fun property name value t ->
      let module I = Targetint_31_63 in
      match
        ( Database.Function.descr property,
          Reg_width_const.is_naked_immediate value )
      with
      | Is_int, Some is_int ->
        if I.equal I.zero is_int
        then add_equation ~raise_on_bottom ~meet_type t name MTC.any_block
        else if I.equal I.one is_int
        then
          add_equation ~raise_on_bottom ~meet_type t name
            MTC.any_tagged_immediate
        else make_bottom t
      | Get_tag, Some get_tag -> (
        match Tag.create_from_targetint get_tag with
        | None -> (* We could probably return bottom here. *) t
        | Some tag -> (
          let tags = Tag.Set.singleton tag in
          match
            MTC.blocks_with_these_tags tags (Alloc_mode.For_types.unknown ())
          with
          | Known ty -> add_equation ~raise_on_bottom ~meet_type t name ty
          | Unknown -> (* Should not happen, but is it worth an assertion? *) t)
        )
      | Is_null, Some is_null ->
        if I.equal I.zero is_null
        then
          add_equation ~raise_on_bottom ~meet_type t name TG.any_non_null_value
        else if I.equal I.one is_null
        then add_equation ~raise_on_bottom ~meet_type t name TG.null
        else make_bottom t
      | (Is_int | Get_tag | Is_null), None -> assert false
      | (Untag_imm | Tag_imm), _ -> t)
    database_level t

let rec reduce ~raise_on_bottom t ~cut_after_aliases ~cut_after_database
    ~meet_type =
  let current_aliases = aliases t in
  let demotions =
    Aliases.diff ~later:current_aliases ~earlier:cut_after_aliases
  in
  let t =
    if Name.Map.is_empty demotions
    then t
    else
      let t = rebuild_once ~raise_on_bottom t demotions ~meet_type in
      rebuild ~raise_on_bottom t ~cut_after:current_aliases ~meet_type
  in
  let aliases_after_rebuild = aliases t in
  let database_after_rebuild = database t in
  let level =
    Database.cut database_after_rebuild ~cut_after:cut_after_database
  in
  let before_reduction = t in
  let t = reduce_once ~raise_on_bottom ~meet_type level t in
  if t == before_reduction
  then t
  else
    reduce ~raise_on_bottom t ~cut_after_aliases:aliases_after_rebuild
      ~cut_after_database:database_after_rebuild ~meet_type

let reducer ~raise_on_bottom t_before ~meet_type =
  let aliases_before = aliases t_before in
  let database_before = database t_before in
  fun t ->
    reduce ~raise_on_bottom t ~cut_after_aliases:aliases_before
      ~cut_after_database:database_before ~meet_type

let reducer t_before ~meet_type =
  let reduce = reducer ~raise_on_bottom:true t_before ~meet_type in
  fun t -> try reduce t with Bottom_equation -> make_bottom t

let with_reduce ~raise_on_bottom f t ~meet_type =
  let aliases_before = aliases t in
  let database_before = database t in
  let t = f t in
  reduce ~raise_on_bottom t ~cut_after_aliases:aliases_before
    ~cut_after_database:database_before ~meet_type

let with_reduce f t ~meet_type =
  try with_reduce ~raise_on_bottom:true f t ~meet_type
  with Bottom_equation -> make_bottom t

let add_env_extension_from_level t level ~meet_type : t =
  let t =
    TEL.fold_on_defined_vars
      (fun var kind t -> add_variable_definition t var kind Name_mode.in_types)
      level t
  in
  let t =
    Name.Map.fold
      (fun name ty t ->
        try add_equation ~raise_on_bottom:true t name ty ~meet_type
        with Bottom_equation -> make_bottom t)
      (TEL.equations level) t
  in
  Variable.Map.fold
    (fun var proj t -> add_symbol_projection t var proj)
    (TEL.symbol_projections level)
    t

let add_equation_strict t name ty ~meet_type : _ Or_bottom.t =
  if t.is_bottom
  then Bottom
  else
    try Ok (add_equation ~raise_on_bottom:true t name ty ~meet_type)
    with Bottom_equation -> Bottom

let add_env_extension_strict t env_extension ~meet_type : _ Or_bottom.t =
  if t.is_bottom
  then Bottom
  else
    try Ok (add_env_extension ~raise_on_bottom:true t env_extension ~meet_type)
    with Bottom_equation -> Bottom

let add_env_extension_maybe_bottom t env_extension ~meet_type =
  add_env_extension ~raise_on_bottom:false t env_extension ~meet_type

let add_equation t name ty ~meet_type =
  try add_equation ~raise_on_bottom:true t name ty ~meet_type
  with Bottom_equation -> make_bottom t

let add_relation t relation ~scrutinee simple =
  try add_relation ~raise_on_bottom:true t relation ~scrutinee simple
  with Bottom_equation -> make_bottom t

let add_variant_extension t arg ~when_block ~when_immediate ~meet_type:_ =
  let when_block, t =
    if Typing_env_extension.is_empty when_block
    then Database.Extension_id.Set.empty, t
    else
      let when_block_id, t = with_extension t when_block in
      Database.Extension_id.Set.singleton when_block_id, t
  in
  let when_immediate, t =
    if Typing_env_extension.is_empty when_immediate
    then Database.Extension_id.Set.empty, t
    else
      let when_block_id, t = with_extension t when_immediate in
      Database.Extension_id.Set.singleton when_block_id, t
  in
  let database = database t and aliases = aliases t in
  match
    Database.add_switch_on_property Database.Function.is_int arg
      ~arms:
        (Reg_width_const.Map.of_list
           [ Reg_width_const.untagged_const_false, Or_bottom.Ok when_block;
             Reg_width_const.untagged_const_true, Or_bottom.Ok when_immediate ])
      database ~aliases
  with
  | Bottom -> make_bottom t
  | Ok database -> with_database_and_aliases t ~database ~aliases

let add_conditional_get_tag_relation t ~arg ~result ~meet_type =
  (* It is semantically unsound to add a [Get_tag] relation when we do not know
     for sure that the argument is a block, because [Get_tag] is not defined for
     immediates.

     When using the types database, we record the [Get_tag] relation in an
     extension that only gets activated when we know that the argument is
     actually a block, which is safe semantically.

     When not using the types database, we cannot do this, because it would
     require an extension on the scrutinee that potentially adds an equation on
     the scrutinee, triggering a potential infinite loop when computing the env
     extension [meet]. In practice, it is probably fine to add the [Get_tag]
     relation unconditionally in this case, since we will only reduce the
     [Get_tag] relation if the result is used. *)
  if Flambda_features.types_database ()
  then
    add_variant_extension t (Simple.name arg) ~meet_type
      ~when_immediate:Typing_env_extension.empty
      ~when_block:
        (Typing_env_extension.add_or_replace_relation Typing_env_extension.empty
           Database.Function.get_tag ~arg ~result:(Simple.name result))
  else
    add_equation t result ~meet_type
      (TG.get_tag_for_block ~block:(Simple.name arg))

let add_env_extension t env_extension ~meet_type =
  try add_env_extension ~raise_on_bottom:true t env_extension ~meet_type
  with Bottom_equation -> make_bottom t

let add_definitions_of_params t ~params =
  List.fold_left
    (fun t param ->
      let name =
        Bound_name.create (Bound_parameter.name param) Name_mode.normal
      in
      add_definition t name
        (Flambda_kind.With_subkind.kind (Bound_parameter.kind param)))
    t
    (Bound_parameters.to_list params)

let check_params_and_types ~params ~param_types =
  if Flambda_features.check_invariants ()
     && List.compare_lengths (Bound_parameters.to_list params) param_types <> 0
  then
    Misc.fatal_errorf
      "Mismatch between number of [params] and [param_types]:@ (%a)@ and@ %a"
      Bound_parameters.print params
      (Format.pp_print_list ~pp_sep:Format.pp_print_space TG.print)
      param_types

let add_equations_on_params t ~params ~param_types ~meet_type =
  check_params_and_types ~params ~param_types;
  List.fold_left2
    (fun t param param_type ->
      add_equation t (Bound_parameter.name param) param_type ~meet_type)
    t
    (Bound_parameters.to_list params)
    param_types

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

let bump_current_level_scope t =
  { t with current_level = One_level.bump_scope t.current_level }

let cut ~with_database t ~cut_after =
  let current_scope = current_scope t in
  if Scope.( >= ) cut_after current_scope
  then TEL.empty, Database.Level.empty
  else
    let rec loop result = function
      | [] ->
        let database_level =
          if with_database
          then Database.cut (database t) ~cut_after:Database.empty
          else Database.Level.empty
        in
        result, database_level
      | one_level :: levels ->
        if Scope.( > ) (One_level.scope one_level) cut_after
        then
          let result =
            TEL.concat ~earlier:(One_level.level one_level) ~later:result
          in
          loop result levels
        else
          let just_after_level = One_level.just_after_level one_level in
          let cut_after_database = Cached_level.database just_after_level in
          let database_level =
            if with_database
            then Database.cut (database t) ~cut_after:cut_after_database
            else Database.Level.empty
          in
          result, database_level
    in
    (* Owing to the check above it is certain that we want [t.current_level]
       included in the result. *)
    loop (One_level.level t.current_level) t.prev_levels

let cut_with_database t ~cut_after = cut ~with_database:true t ~cut_after

let cut t ~cut_after =
  let level, db_level = cut ~with_database:false t ~cut_after in
  assert (Database.Level.is_empty db_level);
  level

let cut_as_extension t ~cut_after =
  let level = cut t ~cut_after in
  Typing_env_level.as_extension_without_bindings level

let type_simple_in_term_exn t ?min_name_mode simple =
  (* If [simple] is a variable then it should not come from a missing .cmx file,
     since this function is only used for typing variables in terms, and even
     imported code is closed with respect to variables. This also means that the
     kind of such variables should always be inferrable, so we pass [None] to
     [find] below. *)
  let ty, binding_time_and_name_mode_simple =
    let[@inline always] const const =
      MTC.type_for_const const, Binding_time.With_name_mode.consts
    in
    let[@inline always] name name ~coercion:_ =
      (* Applying coercion below *)
      find_with_binding_time_and_mode t name None
    in
    Simple.pattern_match simple ~const ~name
  in
  let name_mode_simple =
    Binding_time.With_name_mode.name_mode binding_time_and_name_mode_simple
  in
  let ty =
    if Simple.has_coercion simple
    then TG.apply_coercion ty (Simple.coercion simple)
    else ty
  in
  let kind = TG.kind ty in
  let min_name_mode =
    match min_name_mode with
    | None -> name_mode_simple
    | Some name_mode -> name_mode
  in
  match
    Aliases.get_canonical_element_exn
      ~binding_time_resolver:t.binding_time_resolver (aliases t)
      ~binding_times_and_modes:(names_to_types t) simple name_mode_simple
      ~min_name_mode ~min_binding_time:t.min_binding_time
  with
  | exception Misc.Fatal_error ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf "\n%tContext is:%t typing environment@ %a\n"
      Flambda_colours.error Flambda_colours.pop print t;
    Printexc.raise_with_backtrace Misc.Fatal_error bt
  | exception Binding_time_resolver_failure ->
    TG.alias_type_of kind simple, simple
  | alias -> TG.alias_type_of kind alias, alias

let get_canonical_simple_exn t ?min_name_mode ?name_mode_of_existing_simple
    simple =
  let name_mode_simple =
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
      ~binding_time_resolver:t.binding_time_resolver (aliases t) simple
      ~binding_times_and_modes:(names_to_types t) name_mode_simple
      ~min_name_mode ~min_binding_time:t.min_binding_time
  with
  | exception Misc.Fatal_error ->
    let bt = Printexc.get_raw_backtrace () in
    Format.eprintf "\n%tContext is:%t typing environment@ %a\n"
      Flambda_colours.error Flambda_colours.pop print t;
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

let compute_joined_aliases base_env alias_candidates envs_at_uses =
  match List.map aliases envs_at_uses with
  | [] -> base_env
  | aliases_at_first_use :: aliases_at_other_uses ->
    let new_aliases =
      Name.Set.fold
        (fun name new_aliases ->
          let alias_set =
            List.fold_left
              (fun alias_set aliases ->
                Aliases.Alias_set.inter alias_set
                  (Aliases.get_aliases aliases (Simple.name name)))
              (Aliases.get_aliases aliases_at_first_use (Simple.name name))
              aliases_at_other_uses
          in
          let alias_set =
            Aliases.Alias_set.filter alias_set ~f:(fun simple ->
                mem_simple base_env simple
                && not (Simple.equal simple (Simple.name name)))
          in
          if Aliases.Alias_set.is_empty alias_set
          then new_aliases
          else
            Aliases.add_alias_set
              ~binding_time_resolver:base_env.binding_time_resolver
              ~binding_times_and_modes:(names_to_types base_env) new_aliases
              name alias_set)
        alias_candidates (aliases base_env)
    in
    with_aliases base_env ~aliases:new_aliases

let closure_env t =
  increment_scope { t with min_binding_time = t.next_binding_time }

let rec free_names_transitive_of_type_of_name t name ~result =
  let result = Name_occurrences.add_name result name Name_mode.in_types in
  if variable_is_from_missing_cmx_file t name
  then result
  else
    let typ = find t name None in
    free_names_transitive0 t typ ~result

and free_names_transitive0 t typ ~result =
  let free_names = TG.free_names typ in
  let to_traverse = Name_occurrences.diff free_names ~without:result in
  if Name_occurrences.is_empty to_traverse
  then result
  else
    Name_occurrences.fold_names to_traverse ~init:result ~f:(fun result name ->
        free_names_transitive_of_type_of_name t name ~result)

let free_names_transitive t typ =
  free_names_transitive0 t typ ~result:Name_occurrences.empty

module Pre_serializable : sig
  type t = typing_env

  val create :
    typing_env ->
    used_value_slots:Value_slot.Set.t ->
    t * (Simple.t -> Simple.t)

  val find_or_missing : t -> Name.t -> Type_grammar.t option
end = struct
  type t = typing_env

  let create (t : typing_env) ~used_value_slots =
    let current_level =
      One_level.remove_unused_value_slots_and_shortcut_aliases t.current_level
        ~used_value_slots
    in
    { t with current_level }, One_level.canonicalise current_level

  let find_or_missing = find_or_missing
end

module Serializable : sig
  type t = serializable

  val create : Pre_serializable.t -> reachable_names:Name_occurrences.t -> t

  val create_from_closure_conversion_approx :
    'a Value_approximation.t Symbol.Map.t -> t

  val predefined_exceptions : Symbol.Set.t -> t

  val free_function_slots_and_value_slots : t -> Name_occurrences.t

  val print : Format.formatter -> t -> unit

  val name_domain : t -> Name.Set.t

  val ids_for_export : t -> Ids_for_export.t

  val apply_renaming : t -> Renaming.t -> t

  val merge : t -> t -> t

  val extract_symbol_approx :
    t -> Symbol.t -> (Code_id.t -> 'code) -> 'code Value_approximation.t
end = struct
  type t = serializable

  let create (env : Pre_serializable.t) ~reachable_names : t =
    let current_level =
      One_level.clean_for_export env.current_level ~reachable_names
    in
    let code_age_relation =
      Code_age_relation.clean_for_export env.code_age_relation ~reachable_names
    in
    let just_after_level = One_level.just_after_level current_level in
    let names_to_types = Cached_level.names_to_types just_after_level in
    let defined_symbols_without_equations =
      Symbol.Set.fold
        (fun symbol defined_symbols_without_equations ->
          if Name_occurrences.mem_symbol reachable_names symbol
             && not (Name.Map.mem (Name.symbol symbol) names_to_types)
          then symbol :: defined_symbols_without_equations
          else defined_symbols_without_equations)
        env.defined_symbols []
    in
    { defined_symbols_without_equations; code_age_relation; just_after_level }

  let predefined_exceptions symbols : t =
    let defined_symbols_without_equations = Symbol.Set.elements symbols in
    { defined_symbols_without_equations;
      code_age_relation = Code_age_relation.empty;
      just_after_level = Cached_level.empty
    }

  let create_from_closure_conversion_approx
      (symbols : _ Value_approximation.t Symbol.Map.t) : t =
    (* By using Cached_level.add_or_replace_binding below, we ensure that all
       symbols have an equation (that may be Unknown). *)
    let defined_symbols_without_equations = [] in
    let code_age_relation = Code_age_relation.empty in
    let rec type_from_approx approx =
      match (approx : _ Value_approximation.t) with
      | Value_unknown -> MTC.unknown Flambda_kind.value
      | Value_const cst -> MTC.type_for_const cst
      | Value_symbol symbol ->
        TG.alias_type_of Flambda_kind.value (Simple.symbol symbol)
      | Block_approximation (tag, shape, fields, alloc_mode) ->
        let fields = List.map type_from_approx (Array.to_list fields) in
        MTC.immutable_block ~is_unique:false (Tag.Scannable.to_tag tag)
          ~shape:(Scannable shape) ~fields alloc_mode
      | Closure_approximation { code_id; function_slot; code = _; symbol } ->
        MTC.static_closure_with_this_code ~this_function_slot:function_slot
          ~closure_symbol:symbol ~code_id
    in
    let just_after_level =
      Symbol.Map.fold
        (fun sym approx cached ->
          Cached_level.add_or_replace_binding cached (Name.symbol sym)
            (type_from_approx approx) Binding_time.symbols Name_mode.normal)
        symbols Cached_level.empty
    in
    { defined_symbols_without_equations; code_age_relation; just_after_level }

  let free_function_slots_and_value_slots t =
    Cached_level.free_function_slots_and_value_slots t.just_after_level

  let print = print_serializable

  let name_domain t =
    List.fold_left
      (fun name_domain symbol -> Name.Set.add (Name.symbol symbol) name_domain)
      (Name.Map.keys (Cached_level.names_to_types t.just_after_level))
      t.defined_symbols_without_equations

  let ids_for_export
      { defined_symbols_without_equations; code_age_relation; just_after_level }
      =
    Ids_for_export.create
      ~symbols:(Symbol.Set.of_list defined_symbols_without_equations)
      ~code_ids:(Code_age_relation.all_code_ids_for_export code_age_relation)
      ()
    |> Ids_for_export.union (Cached_level.ids_for_export just_after_level)
    |> Variable.Map.fold
         (fun var proj ids ->
           Ids_for_export.add_variable ids var
           |> Ids_for_export.union (Symbol_projection.ids_for_export proj))
         (Cached_level.symbol_projections just_after_level)

  let apply_renaming
      { defined_symbols_without_equations; code_age_relation; just_after_level }
      renaming =
    let defined_symbols_without_equations =
      List.map
        (Renaming.apply_symbol renaming)
        defined_symbols_without_equations
    in
    let code_age_relation =
      Code_age_relation.apply_renaming code_age_relation renaming
    in
    let just_after_level =
      Cached_level.apply_renaming just_after_level renaming
    in
    { defined_symbols_without_equations; code_age_relation; just_after_level }

  let merge (t1 : t) (t2 : t) : t =
    let defined_symbols_without_equations =
      t1.defined_symbols_without_equations
      @ t2.defined_symbols_without_equations
    in
    let code_age_relation =
      Code_age_relation.union t1.code_age_relation t2.code_age_relation
    in
    let just_after_level =
      Cached_level.merge t1.just_after_level t2.just_after_level
    in
    { defined_symbols_without_equations; code_age_relation; just_after_level }

  let extract_symbol_approx env symbol find_code =
    let rec type_to_approx (ty : Type_grammar.t) : _ Value_approximation.t =
      let module VA = Value_approximation in
      match ty with
      | Value descr -> (
        match Type_descr.descr descr with
        | Unknown | Bottom -> Value_unknown
        | Ok (Equals simple) ->
          Simple.pattern_match' simple
            ~const:(fun const -> VA.Value_const const)
            ~var:(fun _ ~coercion:_ -> VA.Value_unknown)
            ~symbol:(fun symbol ~coercion:_ -> VA.Value_symbol symbol)
        | Ok (No_alias { is_null = Maybe_null; _ })
        | Ok (No_alias { non_null = Unknown | Bottom; _ }) ->
          VA.Value_unknown
        | Ok (No_alias { is_null = Not_null; non_null = Ok head }) -> (
          match head with
          | Mutable_block _ | Boxed_float _ | Boxed_float32 _ | Boxed_int32 _
          | Boxed_int64 _ | Boxed_vec128 _ | Boxed_nativeint _ | String _
          | Array _ ->
            Value_unknown
          | Closures { by_function_slot; alloc_mode = _ } -> (
            let approx_of_closures_entry ~exact function_slot closures_entry :
                _ Value_approximation.t =
              match
                TG.Closures_entry.find_function_type closures_entry ~exact
                  function_slot
              with
              | Bottom | Unknown -> Value_unknown
              | Ok function_type ->
                let code_id = TG.Function_type.code_id function_type in
                let code_or_meta = find_code code_id in
                Closure_approximation
                  { code_id; function_slot; code = code_or_meta; symbol = None }
            in
            match TG.Row_like_for_closures.get_single_tag by_function_slot with
            | No_singleton -> Value_unknown
            | Exact_closure (function_slot, closures_entry) ->
              approx_of_closures_entry ~exact:true function_slot closures_entry
            | Incomplete_closure (function_slot, closures_entry) ->
              approx_of_closures_entry ~exact:false function_slot closures_entry
            )
          | Variant
              { immediates = _;
                blocks = Unknown;
                extensions = _;
                is_unique = _
              }
          | Variant
              { immediates = Unknown;
                blocks = _;
                extensions = _;
                is_unique = _
              } ->
            Value_unknown
          | Variant
              { immediates = Known imms;
                blocks = Known blocks;
                extensions = _;
                is_unique = _
              } ->
            if TG.is_obviously_bottom imms
            then
              match TG.Row_like_for_blocks.get_singleton blocks with
              | None -> Value_unknown
              | Some (tag, Scannable shape, _size, fields, alloc_mode) ->
                let tag =
                  match Tag.Scannable.of_tag tag with
                  | Some tag -> tag
                  | None ->
                    Misc.fatal_errorf
                      "For symbol %a, the tag %a is non-scannable yet the \
                       block shape appears to be scannable:@ %a"
                      Symbol.print symbol Tag.print tag
                      K.Scannable_block_shape.print shape
                in
                let fields =
                  List.map type_to_approx
                    (TG.Product.Int_indexed.components fields)
                in
                Block_approximation
                  (tag, shape, Array.of_list fields, alloc_mode)
              | Some (_, Float_record, _, _, _) -> Value_unknown
            else Value_unknown))
      | Naked_immediate _ | Naked_float _ | Naked_float32 _ | Naked_int32 _
      | Naked_int64 _ | Naked_vec128 _ | Naked_nativeint _ | Rec_info _
      | Region _ ->
        assert false
    in
    let symbol_ty, _binding_time_and_mode =
      Name.Map.find (Name.symbol symbol)
        (Cached_level.names_to_types env.just_after_level)
    in
    type_to_approx symbol_ty
end
