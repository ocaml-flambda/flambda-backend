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

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  { code_id : Code_id.t;
    newer_version_of : Code_id.t option;
    params_arity : Flambda_arity.With_subkinds.t;
    num_trailing_local_params : int;
    result_arity : Flambda_arity.With_subkinds.t;
    result_types : Result_types.t;
    contains_no_escaping_local_allocs : bool;
    stub : bool;
    inline : Inline_attribute.t;
    is_a_functor : bool;
    recursive : Recursive.t;
    cost_metrics : Cost_metrics.t;
    inlining_arguments : Inlining_arguments.t;
    dbg : Debuginfo.t;
    is_tupled : bool;
    is_my_closure_used : bool;
    inlining_decision : Function_decl_inlining_decision_type.t;
    absolute_history : Inlining_history.Absolute.t;
    relative_history : Inlining_history.Relative.t
  }

let code_id { code_id; _ } = code_id

let newer_version_of { newer_version_of; _ } = newer_version_of

let params_arity { params_arity; _ } = params_arity

let num_leading_heap_params { params_arity; num_trailing_local_params; _ } =
  let n =
    Flambda_arity.With_subkinds.cardinal params_arity
    - num_trailing_local_params
  in
  assert (n >= 0);
  (* see [create] *)
  n

let num_trailing_local_params { num_trailing_local_params; _ } =
  num_trailing_local_params

let result_arity { result_arity; _ } = result_arity

let result_types { result_types; _ } = result_types

let stub { stub; _ } = stub

let inline { inline; _ } = inline

let is_a_functor { is_a_functor; _ } = is_a_functor

let recursive { recursive; _ } = recursive

let cost_metrics { cost_metrics; _ } = cost_metrics

let inlining_arguments { inlining_arguments; _ } = inlining_arguments

let dbg { dbg; _ } = dbg

let is_tupled { is_tupled; _ } = is_tupled

let is_my_closure_used { is_my_closure_used; _ } = is_my_closure_used

let inlining_decision { inlining_decision; _ } = inlining_decision

let contains_no_escaping_local_allocs { contains_no_escaping_local_allocs; _ } =
  contains_no_escaping_local_allocs

let absolute_history { absolute_history; _ } = absolute_history

let relative_history { relative_history; _ } = relative_history

let create code_id ~newer_version_of ~params_arity ~num_trailing_local_params
    ~result_arity ~result_types ~contains_no_escaping_local_allocs ~stub
    ~(inline : Inline_attribute.t) ~is_a_functor ~recursive ~cost_metrics
    ~inlining_arguments ~dbg ~is_tupled ~is_my_closure_used ~inlining_decision
    ~absolute_history ~relative_history =
  begin
    match stub, inline with
    | true, (Available_inline | Never_inline | Default_inline)
    | ( false,
        ( Never_inline | Default_inline | Always_inline | Available_inline
        | Unroll _ ) ) ->
      ()
    | true, (Always_inline | Unroll _) ->
      Misc.fatal_error
        "Stubs may not be annotated as [Always_inline] or [Unroll]"
  end;
  if num_trailing_local_params < 0
     || num_trailing_local_params
        > Flambda_arity.With_subkinds.cardinal params_arity
  then
    Misc.fatal_errorf
      "Illegal num_trailing_local_params=%d for params arity: %a"
      num_trailing_local_params Flambda_arity.With_subkinds.print params_arity;
  { code_id;
    newer_version_of;
    params_arity;
    num_trailing_local_params;
    result_arity;
    result_types;
    contains_no_escaping_local_allocs;
    stub;
    inline;
    is_a_functor;
    recursive;
    cost_metrics;
    inlining_arguments;
    dbg;
    is_tupled;
    is_my_closure_used;
    inlining_decision;
    absolute_history;
    relative_history
  }

let with_code_id code_id t = { t with code_id }

let with_newer_version_of newer_version_of t = { t with newer_version_of }

let with_cost_metrics cost_metrics t = { t with cost_metrics }

module Option = struct
  include Option

  let print_compact print_contents ppf t =
    match t with
    | None -> Format.pp_print_string ppf "()"
    | Some contents -> Format.fprintf ppf "%a" print_contents contents
end

let [@ocamlformat "disable"] print_inlining_paths ppf
                                (relative_history, absolute_history) =
  if !Flambda_backend_flags.dump_inlining_paths then
    Format.fprintf ppf
      "@[<hov 1>(relative_history@ %a)@]@ \
       @[<hov 1>(absolute_history@ %a)@]@ "
      Inlining_history.Relative.print relative_history
      Inlining_history.Absolute.print absolute_history

let [@ocamlformat "disable"] print ppf
      { code_id = _; newer_version_of; stub; inline; is_a_functor;
        params_arity; num_trailing_local_params; result_arity;
        result_types; contains_no_escaping_local_allocs;
        recursive; cost_metrics; inlining_arguments;
        dbg; is_tupled; is_my_closure_used; inlining_decision;
        absolute_history; relative_history} =
  let module C = Flambda_colours in
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>@<0>%s(newer_version_of@ %a)@<0>%s@]@ \
      @[<hov 1>@<0>%s(stub@ %b)@<0>%s@]@ \
      @[<hov 1>@<0>%s(inline@ %a)@<0>%s@]@ \
      @[<hov 1>@<0>%s(is_a_functor@ %b)@<0>%s@]@ \
      @[<hov 1>@<0>%s(params_arity@ @<0>%s%a@<0>%s)@<0>%s@]@ \
      @[<hov 1>(num_trailing_local_params@ %d)@]@ \
      @[<hov 1>@<0>%s(result_arity@ @<0>%s%a@<0>%s)@<0>%s@]@ \
      @[<hov 1>(result_types@ @[<hov 1>(%a)@])@]@ \
      @[<hov 1>(contains_no_escaping_local_allocs@ %b)@]@ \
      @[<hov 1>@<0>%s(recursive@ %a)@<0>%s@]@ \
      @[<hov 1>(cost_metrics@ %a)@]@ \
      @[<hov 1>(inlining_arguments@ %a)@]@ \
      @[<hov 1>@<0>%s(dbg@ %a)@<0>%s@]@ \
      @[<hov 1>@<0>%s(is_tupled@ %b)@<0>%s@]@ \
      @[<hov 1>(is_my_closure_used@ %b)@]@ \
      %a
      @[<hov 1>(inlining_decision@ %a)@]\
      )@]"
    (if Option.is_none newer_version_of then Flambda_colours.elide ()
    else Flambda_colours.normal ())
    (Option.print_compact Code_id.print) newer_version_of
    (Flambda_colours.normal ())
    (if not stub then Flambda_colours.elide () else C.normal ())
    stub
    (Flambda_colours.normal ())
    (if Inline_attribute.is_default inline
    then Flambda_colours.elide ()
    else C.normal ())
    Inline_attribute.print inline
    (Flambda_colours.normal ())
    (if not is_a_functor then Flambda_colours.elide () else C.normal ())
    is_a_functor
    (Flambda_colours.normal ())
    (if Flambda_arity.With_subkinds.is_singleton_value params_arity
    then Flambda_colours.elide ()
    else Flambda_colours.normal ())
    (Flambda_colours.normal ())
    Flambda_arity.With_subkinds.print params_arity
    (if Flambda_arity.With_subkinds.is_singleton_value params_arity
    then Flambda_colours.elide ()
    else Flambda_colours.normal ())
    (Flambda_colours.normal ())
    num_trailing_local_params
    (if Flambda_arity.With_subkinds.is_singleton_value result_arity
    then Flambda_colours.elide ()
    else Flambda_colours.normal ())
    (Flambda_colours.normal ())
    Flambda_arity.With_subkinds.print result_arity
    (if Flambda_arity.With_subkinds.is_singleton_value result_arity
    then Flambda_colours.elide ()
    else Flambda_colours.normal ())
    (Flambda_colours.normal ())
    Result_types.print result_types
    contains_no_escaping_local_allocs
    (match recursive with
    | Non_recursive -> Flambda_colours.elide ()
    | Recursive -> Flambda_colours.normal ())
    Recursive.print recursive
    (Flambda_colours.normal ())
    Cost_metrics.print cost_metrics
    Inlining_arguments.print inlining_arguments
    (Flambda_colours.debuginfo ())
    Debuginfo.print_compact dbg
    (Flambda_colours.normal ())
    (if is_tupled
    then Flambda_colours.normal ()
    else Flambda_colours.elide ())
    is_tupled
    (Flambda_colours.normal ())
    is_my_closure_used
    print_inlining_paths (relative_history, absolute_history)
    Function_decl_inlining_decision_type.print inlining_decision

let free_names
    { code_id = _;
      newer_version_of;
      params_arity = _;
      num_trailing_local_params = _;
      result_arity = _;
      result_types;
      contains_no_escaping_local_allocs = _;
      stub = _;
      inline = _;
      is_a_functor = _;
      recursive = _;
      cost_metrics = _;
      inlining_arguments = _;
      dbg = _;
      is_tupled = _;
      is_my_closure_used = _;
      inlining_decision = _;
      absolute_history = _;
      relative_history = _
    } =
  (* [code_id] is only in [t.code_metadata] for the use of [compare]; it doesn't
     count as a free name. *)
  let free_names =
    match newer_version_of with
    | None -> Name_occurrences.empty
    | Some older ->
      Name_occurrences.add_newer_version_of_code_id Name_occurrences.empty older
        Name_mode.normal
  in
  Name_occurrences.union free_names
    (Name_occurrences.downgrade_occurrences_at_strictly_greater_name_mode
       (Result_types.free_names result_types)
       Name_mode.in_types)

let apply_renaming
    ({ code_id;
       newer_version_of;
       params_arity = _;
       num_trailing_local_params = _;
       result_arity = _;
       result_types;
       contains_no_escaping_local_allocs = _;
       stub = _;
       inline = _;
       is_a_functor = _;
       recursive = _;
       cost_metrics = _;
       inlining_arguments = _;
       dbg = _;
       is_tupled = _;
       is_my_closure_used = _;
       inlining_decision = _;
       absolute_history = _;
       relative_history = _
     } as t) perm =
  (* inlined and modified version of Option.map to preserve sharing *)
  let newer_version_of' =
    match newer_version_of with
    | None -> newer_version_of
    | Some code_id ->
      let code_id' = Renaming.apply_code_id perm code_id in
      if code_id == code_id' then newer_version_of else Some code_id'
  in
  let code_id' = Renaming.apply_code_id perm code_id in
  let result_types' = Result_types.apply_renaming result_types perm in
  if code_id == code_id'
     && newer_version_of == newer_version_of'
     && result_types == result_types'
  then t
  else
    { t with
      code_id = code_id';
      newer_version_of = newer_version_of';
      result_types = result_types'
    }

let all_ids_for_export
    { code_id;
      newer_version_of;
      params_arity = _;
      num_trailing_local_params = _;
      result_arity = _;
      result_types;
      contains_no_escaping_local_allocs = _;
      stub = _;
      inline = _;
      is_a_functor = _;
      recursive = _;
      cost_metrics = _;
      inlining_arguments = _;
      dbg = _;
      is_tupled = _;
      is_my_closure_used = _;
      inlining_decision = _;
      absolute_history = _;
      relative_history = _
    } =
  let ids =
    let newer_version_of_ids =
      match newer_version_of with
      | None -> Ids_for_export.empty
      | Some older -> Ids_for_export.add_code_id Ids_for_export.empty older
    in
    Ids_for_export.add_code_id newer_version_of_ids code_id
  in
  Ids_for_export.union ids (Result_types.all_ids_for_export result_types)

let approx_equal
    { code_id = code_id1;
      newer_version_of = newer_version_of1;
      params_arity = params_arity1;
      num_trailing_local_params = num_trailing_local_params1;
      result_arity = result_arity1;
      result_types = _;
      contains_no_escaping_local_allocs = contains_no_escaping_local_allocs1;
      stub = stub1;
      inline = inline1;
      is_a_functor = is_a_functor1;
      recursive = recursive1;
      cost_metrics = cost_metrics1;
      inlining_arguments = inlining_arguments1;
      dbg = dbg1;
      is_tupled = is_tupled1;
      is_my_closure_used = is_my_closure_used1;
      inlining_decision = inlining_decision1;
      absolute_history = absolute_history1;
      relative_history = relative_history1
    }
    { code_id = code_id2;
      newer_version_of = newer_version_of2;
      params_arity = params_arity2;
      num_trailing_local_params = num_trailing_local_params2;
      result_arity = result_arity2;
      result_types = _;
      contains_no_escaping_local_allocs = contains_no_escaping_local_allocs2;
      stub = stub2;
      inline = inline2;
      is_a_functor = is_a_functor2;
      recursive = recursive2;
      cost_metrics = cost_metrics2;
      inlining_arguments = inlining_arguments2;
      dbg = dbg2;
      is_tupled = is_tupled2;
      is_my_closure_used = is_my_closure_used2;
      inlining_decision = inlining_decision2;
      absolute_history = absolute_history2;
      relative_history = relative_history2
    } =
  Code_id.equal code_id1 code_id2
  && (Option.equal Code_id.equal) newer_version_of1 newer_version_of2
  && Flambda_arity.With_subkinds.equal params_arity1 params_arity2
  && Int.equal num_trailing_local_params1 num_trailing_local_params2
  && Flambda_arity.With_subkinds.equal result_arity1 result_arity2
  && Bool.equal contains_no_escaping_local_allocs1
       contains_no_escaping_local_allocs2
  && Bool.equal stub1 stub2
  && Inline_attribute.equal inline1 inline2
  && Bool.equal is_a_functor1 is_a_functor2
  && Recursive.equal recursive1 recursive2
  && Cost_metrics.equal cost_metrics1 cost_metrics2
  && Inlining_arguments.equal inlining_arguments1 inlining_arguments2
  && Int.equal (Debuginfo.compare dbg1 dbg2) 0
  && Bool.equal is_tupled1 is_tupled2
  && Bool.equal is_my_closure_used1 is_my_closure_used2
  && Function_decl_inlining_decision_type.equal inlining_decision1
       inlining_decision2
  && Inlining_history.Absolute.compare absolute_history1 absolute_history2 = 0
  && Inlining_history.Relative.compare relative_history1 relative_history2 = 0

let map_result_types ({ result_types; _ } as t) ~f =
  { t with result_types = Result_types.map_result_types result_types ~f }
