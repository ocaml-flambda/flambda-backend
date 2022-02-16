(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda.Import
module CSE = Common_subexpression_elimination
module K = Flambda_kind
module BP = Bound_parameter
module T = Flambda2_types
module TE = Flambda2_types.Typing_env

type resolver = Compilation_unit.t -> Flambda2_types.Typing_env.t option

type get_imported_names = unit -> Name.Set.t

type get_imported_code = unit -> Exported_code.t

type t =
  { round : int;
    typing_env : TE.t;
    inlined_debuginfo : Debuginfo.t;
    can_inline : bool;
    inlining_state : Inlining_state.t;
    float_const_prop : bool;
    at_unit_toplevel : bool;
    unit_toplevel_return_continuation : Continuation.t;
    unit_toplevel_exn_continuation : Continuation.t;
    variables_defined_at_toplevel : Variable.Set.t;
    cse : CSE.t;
    do_not_rebuild_terms : bool;
    closure_info : Closure_info.t;
    get_imported_code : unit -> Exported_code.t;
    all_code : Code.t Code_id.Map.t;
    inlining_history_tracker : Inlining_history.Tracker.t
  }

let print_debuginfo ppf dbg =
  if Debuginfo.is_none dbg
  then Format.pp_print_string ppf "None"
  else Debuginfo.print_compact ppf dbg

let [@ocamlformat "disable"] print ppf { round; typing_env;
                inlined_debuginfo; can_inline;
                inlining_state; float_const_prop;
                at_unit_toplevel; unit_toplevel_exn_continuation;
                variables_defined_at_toplevel; cse;
                do_not_rebuild_terms; closure_info;
                unit_toplevel_return_continuation; all_code;
                get_imported_code = _; inlining_history_tracker = _;
              } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(round@ %d)@]@ \
      @[<hov 1>(typing_env@ %a)@]@ \
      @[<hov 1>(inlined_debuginfo@ %a)@]@ \
      @[<hov 1>(can_inline@ %b)@]@ \
      @[<hov 1>(inlining_state@ %a)@]@ \
      @[<hov 1>(float_const_prop@ %b)@]@ \
      @[<hov 1>(at_unit_toplevel@ %b)@]@ \
      @[<hov 1>(unit_toplevel_return_continuation@ %a)@]@ \
      @[<hov 1>(unit_toplevel_exn_continuation@ %a)@]@ \
      @[<hov 1>(variables_defined_at_toplevel@ %a)@]@ \
      @[<hov 1>(cse@ @[<hov 1>%a@])@]@ \
      @[<hov 1>(do_not_rebuild_terms@ %b)@]@ \
      @[<hov 1>(closure_info@ %a)@]@ \
      @[<hov 1>(all_code@ %a)@]\
      )@]"
    round
    TE.print typing_env
    print_debuginfo inlined_debuginfo
    can_inline
    Inlining_state.print inlining_state
    float_const_prop
    at_unit_toplevel
    Continuation.print unit_toplevel_return_continuation
    Continuation.print unit_toplevel_exn_continuation
    Variable.Set.print variables_defined_at_toplevel
    CSE.print cse
    do_not_rebuild_terms
    Closure_info.print closure_info
    (Code_id.Map.print Code.print) all_code

let create ~round ~(resolver : resolver)
    ~(get_imported_names : get_imported_names)
    ~(get_imported_code : get_imported_code) ~float_const_prop
    ~unit_toplevel_exn_continuation ~unit_toplevel_return_continuation =
  { round;
    typing_env = TE.create ~resolver ~get_imported_names;
    inlined_debuginfo = Debuginfo.none;
    can_inline = true;
    inlining_state = Inlining_state.default ~round;
    float_const_prop;
    at_unit_toplevel = true;
    unit_toplevel_return_continuation;
    unit_toplevel_exn_continuation;
    variables_defined_at_toplevel = Variable.Set.empty;
    cse = CSE.empty;
    do_not_rebuild_terms = false;
    closure_info = Closure_info.not_in_a_closure;
    all_code = Code_id.Map.empty;
    get_imported_code;
    inlining_history_tracker =
      Inlining_history.Tracker.empty (Compilation_unit.get_current_exn ())
  }

let all_code t = t.all_code

let resolver t = TE.resolver t.typing_env

let typing_env t = t.typing_env

let round t = t.round

let get_continuation_scope t = TE.current_scope t.typing_env

let can_inline t = t.can_inline

let float_const_prop t = t.float_const_prop

let unit_toplevel_exn_continuation t = t.unit_toplevel_exn_continuation

let unit_toplevel_return_continuation t = t.unit_toplevel_return_continuation

let at_unit_toplevel t = t.at_unit_toplevel

let set_not_at_unit_toplevel t = { t with at_unit_toplevel = false }

let set_at_unit_toplevel_state t at_unit_toplevel = { t with at_unit_toplevel }

let is_defined_at_toplevel t var =
  Variable.Set.mem var t.variables_defined_at_toplevel

let get_inlining_state t = t.inlining_state

let set_inlining_state t inlining_state = { t with inlining_state }

let inlining_history_tracker t = t.inlining_history_tracker

let relative_history t =
  Inlining_history.Tracker.relative t.inlining_history_tracker

let set_inlining_history_tracker inlining_history_tracker t =
  { t with inlining_history_tracker }

let increment_continuation_scope t =
  { t with typing_env = TE.increment_scope t.typing_env }

let increment_continuation_scope_twice t =
  increment_continuation_scope (increment_continuation_scope t)

let enter_set_of_closures
    { round;
      typing_env;
      inlined_debuginfo = _;
      can_inline;
      inlining_state;
      float_const_prop;
      at_unit_toplevel = _;
      unit_toplevel_return_continuation;
      unit_toplevel_exn_continuation;
      variables_defined_at_toplevel;
      cse = _;
      do_not_rebuild_terms;
      closure_info = _;
      get_imported_code;
      all_code;
      inlining_history_tracker
    } =
  { round;
    typing_env = TE.closure_env typing_env;
    inlined_debuginfo = Debuginfo.none;
    can_inline;
    inlining_state;
    float_const_prop;
    at_unit_toplevel = false;
    unit_toplevel_return_continuation;
    unit_toplevel_exn_continuation;
    variables_defined_at_toplevel;
    cse = CSE.empty;
    do_not_rebuild_terms;
    closure_info = Closure_info.in_a_set_of_closures;
    get_imported_code;
    all_code;
    inlining_history_tracker
  }

let define_variable t var kind =
  let typing_env =
    let var = Bound_name.var var in
    TE.add_definition t.typing_env var kind
  in
  let variables_defined_at_toplevel =
    if t.at_unit_toplevel
    then Variable.Set.add (Bound_var.var var) t.variables_defined_at_toplevel
    else t.variables_defined_at_toplevel
  in
  { t with typing_env; variables_defined_at_toplevel }

let add_name t name ty =
  let typing_env =
    TE.add_equation
      (TE.add_definition t.typing_env name (T.kind ty))
      (Bound_name.name name) ty
  in
  let variables_defined_at_toplevel =
    Name.pattern_match (Bound_name.name name)
      ~var:(fun var ->
        if t.at_unit_toplevel
        then Variable.Set.add var t.variables_defined_at_toplevel
        else t.variables_defined_at_toplevel)
      ~symbol:(fun _ -> t.variables_defined_at_toplevel)
  in
  { t with typing_env; variables_defined_at_toplevel }

let add_variable0 t var ty ~at_unit_toplevel =
  let typing_env =
    let var' = Bound_name.var var in
    TE.add_equation
      (TE.add_definition t.typing_env var' (T.kind ty))
      (Name.var (Bound_var.var var))
      ty
  in
  let variables_defined_at_toplevel =
    if at_unit_toplevel
    then Variable.Set.add (Bound_var.var var) t.variables_defined_at_toplevel
    else t.variables_defined_at_toplevel
  in
  { t with typing_env; variables_defined_at_toplevel }

let add_variable t var ty =
  add_variable0 t var ty ~at_unit_toplevel:t.at_unit_toplevel

let add_equation_on_variable t var ty =
  let typing_env = TE.add_equation t.typing_env (Name.var var) ty in
  { t with typing_env }

let mem_name t name = TE.mem t.typing_env name

let mem_variable t var = TE.mem t.typing_env (Name.var var)

let define_symbol t sym kind =
  let typing_env =
    let sym = Bound_name.create (Name.symbol sym) Name_mode.normal in
    TE.add_definition t.typing_env sym kind
  in
  { t with typing_env }

let define_symbol_if_undefined t sym kind =
  if TE.mem t.typing_env (Name.symbol sym) then t else define_symbol t sym kind

let add_symbol t sym ty =
  let typing_env =
    let sym = Name.symbol sym in
    let sym' = Bound_name.create sym Name_mode.normal in
    TE.add_equation (TE.add_definition t.typing_env sym' (T.kind ty)) sym ty
  in
  { t with typing_env }

let add_equation_on_symbol t sym ty =
  let typing_env =
    let sym = Name.symbol sym in
    TE.add_equation t.typing_env sym ty
  in
  { t with typing_env }

let mem_symbol t sym = mem_name t (Name.symbol sym)

let find_symbol t sym = TE.find (typing_env t) (Name.symbol sym) (Some K.value)

let add_symbol_projection t var proj =
  { t with typing_env = TE.add_symbol_projection t.typing_env var proj }

let find_symbol_projection t var = TE.find_symbol_projection t.typing_env var

let define_name t name kind =
  let typing_env = TE.add_definition t.typing_env name kind in
  let variables_defined_at_toplevel =
    Name.pattern_match (Bound_name.name name)
      ~var:(fun var ->
        if t.at_unit_toplevel
        then Variable.Set.add var t.variables_defined_at_toplevel
        else t.variables_defined_at_toplevel)
      ~symbol:(fun _ -> t.variables_defined_at_toplevel)
  in
  { t with typing_env; variables_defined_at_toplevel }

let define_name_if_undefined t name kind =
  if TE.mem t.typing_env (Bound_name.to_name name)
  then t
  else define_name t name kind

let add_equation_on_name t name ty =
  let typing_env = TE.add_equation t.typing_env name ty in
  { t with typing_env }

let define_parameters t ~params =
  List.fold_left
    (fun t param ->
      let var = Bound_var.create (BP.var param) Name_mode.normal in
      define_variable t var (K.With_subkind.kind (BP.kind param)))
    t params

let add_parameters ?(name_mode = Name_mode.normal) ?at_unit_toplevel t params
    ~param_types =
  if List.compare_lengths params param_types <> 0
  then
    Misc.fatal_errorf
      "Mismatch between number of [params] and [param_types]:@ (%a)@ and@ %a"
      Bound_parameter.List.print params
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print)
      param_types;
  let at_unit_toplevel =
    Option.value at_unit_toplevel ~default:t.at_unit_toplevel
  in
  List.fold_left2
    (fun t param param_type ->
      let var = Bound_var.create (BP.var param) name_mode in
      add_variable0 t var param_type ~at_unit_toplevel)
    t params param_types

let add_parameters_with_unknown_types' ?name_mode ?at_unit_toplevel t params =
  let param_types =
    ListLabels.map params ~f:(fun param ->
        T.unknown_with_subkind (BP.kind param))
  in
  add_parameters ?name_mode ?at_unit_toplevel t params ~param_types, param_types

let add_parameters_with_unknown_types ?name_mode ?at_unit_toplevel t params =
  fst (add_parameters_with_unknown_types' ?name_mode ?at_unit_toplevel t params)

let mark_parameters_as_toplevel t params =
  let variables_defined_at_toplevel =
    Variable.Set.union t.variables_defined_at_toplevel (BP.List.var_set params)
  in
  { t with variables_defined_at_toplevel }

let define_variable_and_extend_typing_environment t var kind env_extension =
  (* This is a combined operation to reduce allocation. *)
  let typing_env =
    let var' = Bound_name.var var in
    TE.add_definition t.typing_env var' kind
  in
  let variables_defined_at_toplevel =
    if t.at_unit_toplevel
    then Variable.Set.add (Bound_var.var var) t.variables_defined_at_toplevel
    else t.variables_defined_at_toplevel
  in
  let typing_env = TE.add_env_extension typing_env env_extension in
  { t with typing_env; variables_defined_at_toplevel }

let add_variable_and_extend_typing_environment t var ty env_extension =
  (* This is a combined operation to reduce allocation. *)
  let typing_env =
    let var' = Bound_name.var var in
    TE.add_equation
      (TE.add_definition t.typing_env var' (T.kind ty))
      (Name.var (Bound_var.var var))
      ty
  in
  let variables_defined_at_toplevel =
    if t.at_unit_toplevel
    then Variable.Set.add (Bound_var.var var) t.variables_defined_at_toplevel
    else t.variables_defined_at_toplevel
  in
  let typing_env = TE.add_env_extension typing_env env_extension in
  { t with typing_env; variables_defined_at_toplevel }

let extend_typing_environment t env_extension =
  (* There doesn't seem any need to augment [t.variables_defined_at_toplevel]
     here for the existential variables, since they will have [In_types]
     mode. *)
  let typing_env =
    TE.add_env_extension_with_extra_variables t.typing_env env_extension
  in
  { t with typing_env }

let with_typing_env t typing_env = { t with typing_env }

let map_typing_env t ~f = with_typing_env t (f t.typing_env)

let check_name_is_bound t name =
  if not (TE.mem t.typing_env name)
  then
    Misc.fatal_errorf "Unbound name %a in environment:@ %a" Name.print name
      print t

let check_simple_is_bound t (simple : Simple.t) =
  Simple.pattern_match simple
    ~name:(fun name ~coercion:_ -> check_name_is_bound t name)
    ~const:(fun _ -> ())

let mem_code t id =
  Code_id.Map.mem id t.all_code || Exported_code.mem id (t.get_imported_code ())

let find_code_exn t id =
  match Code_id.Map.find id t.all_code with
  | code -> Code_or_metadata.create code
  | exception Not_found -> (
    (* This [find_exn] call doesn't load any .cmx files, but in the majority of
       cases will succeed. *)
    match Exported_code.find_exn (t.get_imported_code ()) id with
    | code_or_metadata -> code_or_metadata
    | exception Not_found -> (
      (* In this case either the code ID isn't bound due to a compiler error or
         we haven't yet loaded the relevant .cmx file. Make sure the .cmx is
         loaded and try again. *)
      match TE.resolver t.typing_env (Code_id.get_compilation_unit id) with
      | None -> raise Not_found
      | Some _typing_env -> Exported_code.find_exn (t.get_imported_code ()) id))

let define_code t ~code_id ~code =
  if not
       (Code_id.in_compilation_unit code_id
          (Compilation_unit.get_current_exn ()))
  then
    Misc.fatal_errorf "Cannot define code ID %a as it is from another unit:@ %a"
      Code_id.print code_id Code.print code;
  if not (Code_id.equal code_id (Code.code_id code))
  then
    Misc.fatal_errorf "Code ID %a does not match code ID in@ %a" Code_id.print
      code_id Code.print code;
  let typing_env =
    TE.add_to_code_age_relation t.typing_env ~new_code_id:code_id
      ~old_code_id:(Code.newer_version_of code)
  in
  let all_code = Code_id.Map.add code_id code t.all_code in
  { t with typing_env; all_code }

let set_inlined_debuginfo t dbg = { t with inlined_debuginfo = dbg }

let get_inlined_debuginfo t = t.inlined_debuginfo

let add_inlined_debuginfo t dbg = Debuginfo.inline t.inlined_debuginfo dbg

let cse t = t.cse

let add_cse t prim ~bound_to =
  let scope = get_continuation_scope t in
  let cse = CSE.add t.cse prim ~bound_to scope in
  { t with cse }

let find_cse t prim = CSE.find t.cse prim

let with_cse t cse = { t with cse }

let set_do_not_rebuild_terms_and_disable_inlining t =
  { t with do_not_rebuild_terms = true; can_inline = false }

let set_rebuild_terms t = { t with do_not_rebuild_terms = false }

let are_rebuilding_terms t =
  Are_rebuilding_terms.of_bool (not t.do_not_rebuild_terms)

let enter_closure code_id ~return_continuation ~exn_continuation t =
  { t with
    closure_info =
      Closure_info.in_a_closure code_id ~return_continuation ~exn_continuation
  }

let closure_info t = t.closure_info

let inlining_arguments { inlining_state; _ } =
  Inlining_state.arguments inlining_state

let inlining_depth { inlining_state; _ } = Inlining_state.depth inlining_state

let set_inlining_arguments arguments t =
  { t with
    inlining_state = Inlining_state.with_arguments arguments t.inlining_state
  }

let enter_inlined_apply ~called_code ~apply t =
  let arguments =
    Inlining_state.arguments t.inlining_state
    |> Inlining_arguments.meet (Code.inlining_arguments called_code)
    |> Inlining_arguments.meet (Apply.inlining_arguments apply)
  in
  { t with
    inlined_debuginfo = Apply.dbg apply;
    inlining_state =
      t.inlining_state |> Inlining_state.increment_depth
      |> Inlining_state.with_arguments arguments;
    inlining_history_tracker =
      Inlining_history.Tracker.enter_inlined_apply
        ~callee:(Code.absolute_history called_code)
        ~dbg:(Apply.dbg apply)
        ~apply_relative_history:(Apply.relative_history apply)
        t.inlining_history_tracker
  }

let generate_phantom_lets t =
  Flambda_features.debug ()
  && Flambda_features.Expert.phantom_lets ()
  (* It would be a waste of time generating phantom lets when not rebuilding
     terms, since they have no effect on cost metrics. *)
  && Are_rebuilding_terms.are_rebuilding (are_rebuilding_terms t)
