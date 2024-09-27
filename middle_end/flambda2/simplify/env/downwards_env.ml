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

open! Flambda.Import
module CSE = Common_subexpression_elimination
module K = Flambda_kind
module BP = Bound_parameter
module T = Flambda2_types
module TE = Flambda2_types.Typing_env

type resolver =
  Compilation_unit.t -> Flambda2_types.Typing_env.Serializable.t option

type get_imported_names = unit -> Name.Set.t

type get_imported_code = unit -> Exported_code.t

type t =
  { round : int;
    typing_env : TE.t;
    inlined_debuginfo : Inlined_debuginfo.t;
    can_inline : bool;
    inlining_state : Inlining_state.t;
    propagating_float_consts : bool;
    at_unit_toplevel : bool;
    unit_toplevel_return_continuation : Continuation.t;
    unit_toplevel_exn_continuation : Continuation.t;
    variables_defined_at_toplevel : Variable.Set.t;
    cse : CSE.t;
    comparison_results : Comparison_result.t Variable.Map.t;
    do_not_rebuild_terms : bool;
    closure_info : Closure_info.t;
    get_imported_code : unit -> Exported_code.t;
    all_code : Code.t Code_id.Map.t;
    inlining_history_tracker : Inlining_history.Tracker.t;
    loopify_state : Loopify_state.t
  }

let [@ocamlformat "disable"] print ppf { round; typing_env;
                inlined_debuginfo; can_inline;
                inlining_state; propagating_float_consts;
                at_unit_toplevel; unit_toplevel_exn_continuation;
                variables_defined_at_toplevel; cse; comparison_results;
                do_not_rebuild_terms; closure_info;
                unit_toplevel_return_continuation; all_code;
                get_imported_code = _; inlining_history_tracker = _;
                loopify_state
              } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(round@ %d)@]@ \
      @[<hov 1>(typing_env@ %a)@]@ \
      @[<hov 1>(inlined_debuginfo@ %a)@]@ \
      @[<hov 1>(can_inline@ %b)@]@ \
      @[<hov 1>(inlining_state@ %a)@]@ \
      @[<hov 1>(propagating_float_consts@ %b)@]@ \
      @[<hov 1>(at_unit_toplevel@ %b)@]@ \
      @[<hov 1>(unit_toplevel_return_continuation@ %a)@]@ \
      @[<hov 1>(unit_toplevel_exn_continuation@ %a)@]@ \
      @[<hov 1>(variables_defined_at_toplevel@ %a)@]@ \
      @[<hov 1>(cse@ @[<hov 1>%a@])@]@ \
      @[<hov 1>(comparison_results@ @[<hov 1>%a@])@]@ \
      @[<hov 1>(do_not_rebuild_terms@ %b)@]@ \
      @[<hov 1>(closure_info@ %a)@]@ \
      @[<hov 1>(all_code@ %a)@]@ \
      @[<hov 1>(loopify_state@ %a)@]\
      )@]"
    round
    TE.print typing_env
    Inlined_debuginfo.print inlined_debuginfo
    can_inline
    Inlining_state.print inlining_state
    propagating_float_consts
    at_unit_toplevel
    Continuation.print unit_toplevel_return_continuation
    Continuation.print unit_toplevel_exn_continuation
    Variable.Set.print variables_defined_at_toplevel
    CSE.print cse
    (Variable.Map.print Comparison_result.print) comparison_results
    do_not_rebuild_terms
    Closure_info.print closure_info
    (Code_id.Map.print Code.print) all_code
    Loopify_state.print loopify_state

let define_variable t var kind =
  let typing_env =
    let var = Bound_name.create_var var in
    TE.add_definition t.typing_env var kind
  in
  let variables_defined_at_toplevel =
    if t.at_unit_toplevel
    then Variable.Set.add (Bound_var.var var) t.variables_defined_at_toplevel
    else t.variables_defined_at_toplevel
  in
  { t with typing_env; variables_defined_at_toplevel }

let create ~round ~(resolver : resolver)
    ~(get_imported_names : get_imported_names)
    ~(get_imported_code : get_imported_code) ~propagating_float_consts
    ~unit_toplevel_exn_continuation ~unit_toplevel_return_continuation
    ~toplevel_my_region ~toplevel_my_ghost_region =
  let typing_env = TE.create ~resolver ~get_imported_names in
  let t =
    { round;
      typing_env;
      inlined_debuginfo = Inlined_debuginfo.none;
      can_inline = true;
      inlining_state = Inlining_state.default ~round;
      propagating_float_consts;
      at_unit_toplevel = true;
      unit_toplevel_return_continuation;
      unit_toplevel_exn_continuation;
      variables_defined_at_toplevel = Variable.Set.empty;
      cse = CSE.empty;
      comparison_results = Variable.Map.empty;
      do_not_rebuild_terms = false;
      closure_info = Closure_info.not_in_a_closure;
      all_code = Code_id.Map.empty;
      get_imported_code;
      inlining_history_tracker =
        Inlining_history.Tracker.empty (Compilation_unit.get_current_exn ());
      loopify_state = Loopify_state.do_not_loopify
    }
  in
  define_variable
    (define_variable t
       (Bound_var.create toplevel_my_region Name_mode.normal)
       K.region)
    (Bound_var.create toplevel_my_ghost_region Name_mode.normal)
    K.region

let all_code t = t.all_code

let resolver t = TE.resolver t.typing_env

let typing_env t = t.typing_env

let round t = t.round

let get_continuation_scope t = TE.current_scope t.typing_env

let can_inline t = t.can_inline

let propagating_float_consts t = t.propagating_float_consts

let unit_toplevel_exn_continuation t = t.unit_toplevel_exn_continuation

let unit_toplevel_return_continuation t = t.unit_toplevel_return_continuation

let at_unit_toplevel t = t.at_unit_toplevel

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

let enter_set_of_closures
    { round;
      typing_env;
      inlined_debuginfo = _;
      can_inline;
      inlining_state;
      propagating_float_consts;
      at_unit_toplevel = _;
      unit_toplevel_return_continuation;
      unit_toplevel_exn_continuation;
      variables_defined_at_toplevel;
      cse = _;
      comparison_results = _;
      do_not_rebuild_terms;
      closure_info = _;
      get_imported_code;
      all_code;
      inlining_history_tracker;
      loopify_state = _
    } =
  { round;
    typing_env = TE.closure_env typing_env;
    inlined_debuginfo = Inlined_debuginfo.none;
    can_inline;
    inlining_state;
    propagating_float_consts;
    at_unit_toplevel = false;
    unit_toplevel_return_continuation;
    unit_toplevel_exn_continuation;
    variables_defined_at_toplevel;
    cse = CSE.empty;
    comparison_results = Variable.Map.empty;
    do_not_rebuild_terms;
    closure_info = Closure_info.in_a_set_of_closures;
    get_imported_code;
    all_code;
    inlining_history_tracker;
    loopify_state = Loopify_state.do_not_loopify
  }

let define_symbol t sym kind =
  let typing_env =
    let sym = Bound_name.create (Name.symbol sym) Name_mode.normal in
    TE.add_definition t.typing_env sym kind
  in
  { t with typing_env }

let define_name t name kind =
  Name.pattern_match (Bound_name.name name)
    ~var:(fun [@inline] var ->
      (define_variable [@inlined hint]) t
        (Bound_var.create var (Bound_name.name_mode name))
        kind)
    ~symbol:(fun [@inline] sym -> (define_symbol [@inlined hint]) t sym kind)

let add_variable t var ty =
  let t = (define_variable [@inlined hint]) t var (T.kind ty) in
  { t with
    typing_env = TE.add_equation t.typing_env (Name.var (Bound_var.var var)) ty
  }

let add_symbol t sym ty =
  let t = (define_symbol [@inlined hint]) t sym (T.kind ty) in
  { t with typing_env = TE.add_equation t.typing_env (Name.symbol sym) ty }

let add_name t name ty =
  Name.pattern_match (Bound_name.name name)
    ~var:(fun [@inline] var ->
      add_variable t (Bound_var.create var (Bound_name.name_mode name)) ty)
    ~symbol:(fun [@inline] sym -> add_symbol t sym ty)

let add_equation_on_variable t var ty =
  let typing_env = TE.add_equation t.typing_env (Name.var var) ty in
  { t with typing_env }

let mem_name t name = TE.mem t.typing_env name

let mem_variable t var = TE.mem t.typing_env (Name.var var)

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

let define_name_if_undefined t name kind =
  if TE.mem t.typing_env (Bound_name.name name)
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
    t
    (Bound_parameters.to_list params)

let add_parameters ?(name_mode = Name_mode.normal) t params ~param_types =
  let params' = params in
  let params = Bound_parameters.to_list params in
  if List.compare_lengths params param_types <> 0
  then
    Misc.fatal_errorf
      "Mismatch between number of [params] and [param_types]:@ (%a)@ and@ %a"
      Bound_parameters.print params'
      (Format.pp_print_list ~pp_sep:Format.pp_print_space T.print)
      param_types;
  List.fold_left2
    (fun t param param_type ->
      let var = Bound_var.create (BP.var param) name_mode in
      add_variable t var param_type)
    t params param_types

let add_parameters_with_unknown_types ?alloc_modes ?name_mode t params =
  let params' = params in
  let params = Bound_parameters.to_list params in
  let alloc_modes =
    match alloc_modes with
    | Some alloc_modes ->
      if List.compare_lengths alloc_modes params <> 0
      then
        Misc.fatal_errorf "Params and alloc modes do not match up:@ %a"
          Bound_parameters.print params';
      alloc_modes
    | None -> List.map (fun _ -> Alloc_mode.For_types.unknown ()) params
  in
  let param_types =
    ListLabels.map2 params alloc_modes ~f:(fun param alloc_mode ->
        T.unknown_with_subkind ~alloc_mode (BP.kind param))
  in
  add_parameters ?name_mode t params' ~param_types

let mark_parameters_as_toplevel t params =
  let variables_defined_at_toplevel =
    Variable.Set.union t.variables_defined_at_toplevel
      (Bound_parameters.var_set params)
  in
  { t with variables_defined_at_toplevel }

let define_variable_and_extend_typing_environment t var kind env_extension =
  let t = (define_variable [@inlined hint]) t var kind in
  let typing_env = TE.add_env_extension t.typing_env env_extension in
  { t with typing_env }

let add_variable_and_extend_typing_environment t var ty env_extension =
  let t = (add_variable [@inlined hint]) t var ty in
  let typing_env = TE.add_env_extension t.typing_env env_extension in
  { t with typing_env }

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
  | exception Not_found ->
    (* We might have already loaded the metadata, from another unit that
       references it. However we force loading of the corresponding .cmx to make
       sure that we will have access to the actual code (assuming the .cmx isn't
       missing). *)
    let (_ : TE.Serializable.t option) =
      TE.resolver t.typing_env (Code_id.get_compilation_unit id)
    in
    Exported_code.find_exn (t.get_imported_code ()) id

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

let cse t = t.cse

let comparison_results t = t.comparison_results

let add_cse t prim ~bound_to =
  let scope = get_continuation_scope t in
  let cse = CSE.add t.cse prim ~bound_to scope in
  let comparison_results =
    let prim = Flambda_primitive.Eligible_for_cse.to_primitive prim in
    match
      ( Comparison_result.create ~prim ~comparison_results:t.comparison_results,
        Simple.must_be_var bound_to )
    with
    | None, _ | _, None -> t.comparison_results
    | Some comp, Some (var, _) -> Variable.Map.add var comp t.comparison_results
  in
  { t with cse; comparison_results }

let find_cse t prim = CSE.find t.cse prim

let find_comparison_result t var =
  Variable.Map.find_opt var t.comparison_results

let with_cse t cse = { t with cse }

let set_do_not_rebuild_terms_and_disable_inlining t =
  { t with do_not_rebuild_terms = true; can_inline = false }

let disable_inlining t = { t with can_inline = false }

let set_rebuild_terms t = { t with do_not_rebuild_terms = false }

let are_rebuilding_terms t =
  Are_rebuilding_terms.of_bool (not t.do_not_rebuild_terms)

let enter_closure code_id ~return_continuation ~exn_continuation ~my_closure t =
  { t with
    closure_info =
      Closure_info.in_a_closure code_id ~return_continuation ~exn_continuation
        ~my_closure
  }

let closure_info t = t.closure_info

let inlining_arguments { inlining_state; _ } =
  Inlining_state.arguments inlining_state

let set_inlining_arguments arguments t =
  { t with
    inlining_state = Inlining_state.with_arguments arguments t.inlining_state
  }

(* CR mshinwell/gbury: we might be dropping [Enter_inlined_apply] context here
   when mixing code compiled in classic and Simplify modes *)

let set_inlined_debuginfo t ~from =
  { t with inlined_debuginfo = from.inlined_debuginfo }

let merge_inlined_debuginfo t ~from_apply_expr =
  { t with
    inlined_debuginfo =
      Inlined_debuginfo.merge t.inlined_debuginfo ~from_apply_expr
  }

let add_inlined_debuginfo t dbg =
  Inlined_debuginfo.rewrite t.inlined_debuginfo dbg

let enter_inlined_apply ~called_code ~apply ~was_inline_always t =
  let arguments =
    Inlining_state.arguments t.inlining_state
    |> Inlining_arguments.meet (Code.inlining_arguments called_code)
    |> Inlining_arguments.meet (Apply.inlining_arguments apply)
  in
  let inlining_state =
    (* The depth limit for [@inline always] and [@inlined always] is really to
       make sure the compiler terminates if user code containing implicit
       recursion turns into explicit recursion within Flambda. We want to honour
       the user's requests for these attributes basically all the time. As such
       inlining when these attributes are in effect affects the depth limit much
       less than in other scenarios. *)
    Inlining_state.with_arguments arguments
      (if Code.stub called_code
      then t.inlining_state
      else
        let by =
          if was_inline_always
          then 1
          else Flambda_features.Inlining.depth_scaling_factor
        in
        Inlining_state.increment_depth t.inlining_state ~by)
  in
  let inlined_debuginfo =
    Inlined_debuginfo.create ~called_code_id:(Code.code_id called_code)
      ~apply_dbg:(Apply.dbg apply)
  in
  { t with
    inlined_debuginfo;
    inlining_state;
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

let loopify_state t = t.loopify_state

let set_loopify_state loopify_state t = { t with loopify_state }

let with_code_age_relation code_age_relation t =
  { t with
    typing_env = TE.with_code_age_relation t.typing_env code_age_relation
  }
