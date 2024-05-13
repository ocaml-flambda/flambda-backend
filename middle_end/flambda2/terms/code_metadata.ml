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

type t =
  { code_id : Code_id.t;
    newer_version_of : Code_id.t option;
    params_arity : [`Complex] Flambda_arity.t;
    param_modes : Alloc_mode.For_types.t list;
    first_complex_local_param : int;
    (* Note: first_complex_local_param cannot be computed from param_modes,
       because it might be 0 if the closure itself has to be allocated locally,
       for instance as a result of a partial application. *)
    result_arity : [`Unarized] Flambda_arity.t;
    result_types : Result_types.t Or_unknown_or_bottom.t;
    result_mode : Lambda.alloc_mode;
    contains_no_escaping_local_allocs : bool;
    stub : bool;
    inline : Inline_attribute.t;
    zero_alloc_attribute : Zero_alloc_attribute.t;
    poll_attribute : Poll_attribute.t;
    is_a_functor : bool;
    is_opaque : bool;
    recursive : Recursive.t;
    cost_metrics : Cost_metrics.t;
    inlining_arguments : Inlining_arguments.t;
    dbg : Debuginfo.t;
    is_tupled : bool;
    is_my_closure_used : bool;
    inlining_decision : Function_decl_inlining_decision_type.t;
    absolute_history : Inlining_history.Absolute.t;
    relative_history : Inlining_history.Relative.t;
    loopify : Loopify_attribute.t
  }

type code_metadata = t

module type Metadata_view_type = sig
  type 'a t

  val metadata : 'a t -> code_metadata
end

module Code_metadata_accessors (X : Metadata_view_type) = struct
  open X

  let code_id t = (metadata t).code_id

  let newer_version_of t = (metadata t).newer_version_of

  let params_arity t = (metadata t).params_arity

  let param_modes t = (metadata t).param_modes

  let first_complex_local_param t = (metadata t).first_complex_local_param

  let result_arity t = (metadata t).result_arity

  let result_types t = (metadata t).result_types

  let result_mode t = (metadata t).result_mode

  let stub t = (metadata t).stub

  let inline t = (metadata t).inline

  let zero_alloc_attribute t = (metadata t).zero_alloc_attribute

  let poll_attribute t = (metadata t).poll_attribute

  let is_a_functor t = (metadata t).is_a_functor

  let is_opaque t = (metadata t).is_opaque

  let recursive t = (metadata t).recursive

  let cost_metrics t = (metadata t).cost_metrics

  let inlining_arguments t = (metadata t).inlining_arguments

  let dbg t = (metadata t).dbg

  let is_tupled t = (metadata t).is_tupled

  let is_my_closure_used t = (metadata t).is_my_closure_used

  let inlining_decision t = (metadata t).inlining_decision

  let contains_no_escaping_local_allocs t =
    (metadata t).contains_no_escaping_local_allocs

  let absolute_history t = (metadata t).absolute_history

  let relative_history t = (metadata t).relative_history

  let loopify t = (metadata t).loopify
end

module type Code_metadata_accessors_result_type = sig
  type 'a t

  include module type of Code_metadata_accessors (struct
    type nonrec 'a t = 'a t

    let metadata = assert false
  end)
end

module Metadata_view = struct
  type nonrec 'a t = t

  let metadata t = t
end

include Code_metadata_accessors [@inlined hint] (Metadata_view)

type 'a create_type =
  Code_id.t ->
  newer_version_of:Code_id.t option ->
  params_arity:[`Complex] Flambda_arity.t ->
  param_modes:Alloc_mode.For_types.t list ->
  first_complex_local_param:int ->
  result_arity:[`Unarized] Flambda_arity.t ->
  result_types:Result_types.t Or_unknown_or_bottom.t ->
  result_mode:Lambda.alloc_mode ->
  contains_no_escaping_local_allocs:bool ->
  stub:bool ->
  inline:Inline_attribute.t ->
  zero_alloc_attribute:Zero_alloc_attribute.t ->
  poll_attribute:Poll_attribute.t ->
  is_a_functor:bool ->
  is_opaque:bool ->
  recursive:Recursive.t ->
  cost_metrics:Cost_metrics.t ->
  inlining_arguments:Inlining_arguments.t ->
  dbg:Debuginfo.t ->
  is_tupled:bool ->
  is_my_closure_used:bool ->
  inlining_decision:Function_decl_inlining_decision_type.t ->
  absolute_history:Inlining_history.Absolute.t ->
  relative_history:Inlining_history.Relative.t ->
  loopify:Loopify_attribute.t ->
  'a

let createk k code_id ~newer_version_of ~params_arity ~param_modes
    ~first_complex_local_param ~result_arity ~result_types ~result_mode
    ~contains_no_escaping_local_allocs ~stub ~(inline : Inline_attribute.t)
    ~zero_alloc_attribute ~poll_attribute ~is_a_functor ~is_opaque ~recursive ~cost_metrics
    ~inlining_arguments ~dbg ~is_tupled ~is_my_closure_used ~inlining_decision
    ~absolute_history ~relative_history ~loopify =
  (match stub, inline with
  | true, (Available_inline | Never_inline | Default_inline)
  | ( false,
      ( Never_inline | Default_inline | Always_inline | Available_inline
      | Unroll _ ) ) ->
    ()
  | true, (Always_inline | Unroll _) ->
    Misc.fatal_error "Stubs may not be annotated as [Always_inline] or [Unroll]");
  if first_complex_local_param < 0
     || first_complex_local_param > Flambda_arity.num_params params_arity
  then
    Misc.fatal_errorf
      "Illegal first_complex_local_param=%d for params arity: %a"
      first_complex_local_param Flambda_arity.print params_arity;
  if List.compare_length_with param_modes
       (Flambda_arity.cardinal_unarized params_arity)
     <> 0
  then
    Misc.fatal_errorf "Parameter modes do not match arity: %a and (%a)"
      Flambda_arity.print params_arity
      (Format.pp_print_list ~pp_sep:Format.pp_print_space
         Alloc_mode.For_types.print)
      param_modes;
  k
    { code_id;
      newer_version_of;
      params_arity;
      param_modes;
      first_complex_local_param;
      result_arity;
      result_types;
      result_mode;
      contains_no_escaping_local_allocs;
      stub;
      inline;
      zero_alloc_attribute;
      poll_attribute;
      is_a_functor;
      is_opaque;
      recursive;
      cost_metrics;
      inlining_arguments;
      dbg;
      is_tupled;
      is_my_closure_used;
      inlining_decision;
      absolute_history;
      relative_history;
      loopify
    }

let create = createk (fun t -> t)

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
       { code_id = _; newer_version_of; stub; inline; zero_alloc_attribute; poll_attribute;
         is_a_functor; is_opaque; params_arity; param_modes;
         first_complex_local_param; result_arity;
         result_types; result_mode; contains_no_escaping_local_allocs;
         recursive; cost_metrics; inlining_arguments;
         dbg; is_tupled; is_my_closure_used; inlining_decision;
         absolute_history; relative_history; loopify } =
  let module C = Flambda_colours in
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>%t(newer_version_of@ %a)%t@]@ \
      @[<hov 1>%t(stub@ %b)%t@]@ \
      @[<hov 1>%t(inline@ %a)%t@]@ \
      @[<hov 1>%t(%a)%t@]@ \
      @[<hov 1>%t(poll_attribute@ %a)%t@]@ \
      @[<hov 1>%t(is_a_functor@ %b)%t@]@ \
      @[<hov 1>%t(is_opaque@ %b)%t@]@ \
      @[<hov 1>%t(params_arity@ %t%a%t)%t@]@ \
      @[<hov 1>%t(param_modes@ %t(%a)%t)%t@]@ \
      @[<hov 1>(first_complex_local_param@ %d)@]@ \
      @[<hov 1>%t(result_arity@ %t%a%t)%t@]@ \
      @[<hov 1>(result_types@ @[<hov 1>(%a)@])@]@ \
      @[<hov 1>(result_mode@ %s)@]@ \
      @[<hov 1>(contains_no_escaping_local_allocs@ %b)@]@ \
      @[<hov 1>%t(recursive@ %a)%t@]@ \
      @[<hov 1>(cost_metrics@ %a)@]@ \
      @[<hov 1>(inlining_arguments@ %a)@]@ \
      @[<hov 1>%t(dbg@ %a)%t@]@ \
      @[<hov 1>%t(is_tupled@ %b)%t@]@ \
      @[<hov 1>(is_my_closure_used@ %b)@]@ \
      %a\
      @[<hov 1>(inlining_decision@ %a)@]@ \
      @[<hov 1>(loopify@ %a)@]\
      )@]"
    (if Option.is_none newer_version_of then Flambda_colours.elide
    else Flambda_colours.none)
    (Option.print_compact Code_id.print) newer_version_of
    Flambda_colours.pop
    (if not stub then Flambda_colours.elide else C.none)
    stub
    Flambda_colours.pop
    (if Inline_attribute.is_default inline
    then Flambda_colours.elide
    else C.none)
    Inline_attribute.print inline
    Flambda_colours.pop
    (if Zero_alloc_attribute.is_default zero_alloc_attribute
     then Flambda_colours.elide else C.none)
    Zero_alloc_attribute.print zero_alloc_attribute
    Flambda_colours.pop
    (if Poll_attribute.is_default poll_attribute
     then Flambda_colours.elide else C.none)
    Poll_attribute.print poll_attribute
    Flambda_colours.pop
    (if not is_a_functor then Flambda_colours.elide else C.none)
    is_a_functor
    Flambda_colours.pop
    (if not is_opaque then Flambda_colours.elide else C.none)
    is_opaque
    Flambda_colours.pop
    (if Flambda_arity.is_one_param_of_kind_value params_arity
    then Flambda_colours.elide
    else Flambda_colours.none)
    Flambda_colours.pop
    Flambda_arity.print params_arity
    (if Flambda_arity.is_one_param_of_kind_value params_arity
    then Flambda_colours.elide
    else Flambda_colours.none)
    Flambda_colours.pop
    (if List.for_all
      (fun mode -> Alloc_mode.For_types.equal mode Alloc_mode.For_types.heap)
      param_modes
    then Flambda_colours.elide
    else Flambda_colours.none)
    Flambda_colours.pop
    (Format.pp_print_list ~pp_sep:Format.pp_print_space
      Alloc_mode.For_types.print)
    param_modes
    (if List.for_all
      (fun mode -> Alloc_mode.For_types.equal mode Alloc_mode.For_types.heap)
      param_modes
    then Flambda_colours.elide
    else Flambda_colours.none)
    Flambda_colours.pop
    first_complex_local_param
    (if Flambda_arity.is_one_param_of_kind_value result_arity
    then Flambda_colours.elide
    else Flambda_colours.none)
    Flambda_colours.pop
    Flambda_arity.print result_arity
    (if Flambda_arity.is_one_param_of_kind_value result_arity
    then Flambda_colours.elide
    else Flambda_colours.none)
    Flambda_colours.pop
    (Or_unknown_or_bottom.print Result_types.print) result_types
    (match result_mode with Alloc_heap -> "Heap" | Alloc_local -> "Local")
    contains_no_escaping_local_allocs
    (match recursive with
    | Non_recursive -> Flambda_colours.elide
    | Recursive -> Flambda_colours.none)
    Recursive.print recursive
    Flambda_colours.pop
    Cost_metrics.print cost_metrics
    Inlining_arguments.print inlining_arguments
    Flambda_colours.debuginfo
    Debuginfo.print_compact dbg
    Flambda_colours.pop
    (if is_tupled
    then Flambda_colours.none
    else Flambda_colours.elide)
    is_tupled
    Flambda_colours.pop
    is_my_closure_used
    print_inlining_paths (relative_history, absolute_history)
    Function_decl_inlining_decision_type.print inlining_decision
    Loopify_attribute.print loopify

let free_names
    { code_id = _;
      newer_version_of;
      params_arity = _;
      param_modes = _;
      first_complex_local_param = _;
      result_arity = _;
      result_types;
      result_mode = _;
      contains_no_escaping_local_allocs = _;
      stub = _;
      inline = _;
      zero_alloc_attribute = _;
      poll_attribute = _;
      is_a_functor = _;
      is_opaque = _;
      recursive = _;
      cost_metrics = _;
      inlining_arguments = _;
      dbg = _;
      is_tupled = _;
      is_my_closure_used = _;
      inlining_decision = _;
      absolute_history = _;
      relative_history = _;
      loopify = _
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
       (match result_types with
       | Unknown | Bottom -> Name_occurrences.empty
       | Ok result_types -> Result_types.free_names result_types)
       Name_mode.in_types)

let apply_renaming
    ({ code_id;
       newer_version_of;
       params_arity = _;
       param_modes = _;
       first_complex_local_param = _;
       result_arity = _;
       result_types;
       result_mode = _;
       contains_no_escaping_local_allocs = _;
       stub = _;
       inline = _;
       zero_alloc_attribute = _;
       poll_attribute = _;
       is_a_functor = _;
       is_opaque = _;
       recursive = _;
       cost_metrics = _;
       inlining_arguments = _;
       dbg = _;
       is_tupled = _;
       is_my_closure_used = _;
       inlining_decision = _;
       absolute_history = _;
       relative_history = _;
       loopify = _
     } as t) renaming =
  (* inlined and modified version of Option.map to preserve sharing *)
  let newer_version_of' =
    match newer_version_of with
    | None -> newer_version_of
    | Some code_id ->
      let code_id' = Renaming.apply_code_id renaming code_id in
      if code_id == code_id' then newer_version_of else Some code_id'
  in
  let code_id' = Renaming.apply_code_id renaming code_id in
  let result_types' =
    match result_types with
    | Unknown | Bottom -> result_types
    | Ok result_types ->
      Or_unknown_or_bottom.Ok
        (Result_types.apply_renaming result_types renaming)
  in
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

let ids_for_export
    { code_id;
      newer_version_of;
      params_arity = _;
      param_modes = _;
      first_complex_local_param = _;
      result_arity = _;
      result_types;
      result_mode = _;
      contains_no_escaping_local_allocs = _;
      stub = _;
      inline = _;
      zero_alloc_attribute = _;
      poll_attribute = _;
      is_a_functor = _;
      is_opaque = _;
      recursive = _;
      cost_metrics = _;
      inlining_arguments = _;
      dbg = _;
      is_tupled = _;
      is_my_closure_used = _;
      inlining_decision = _;
      absolute_history = _;
      relative_history = _;
      loopify = _
    } =
  let ids =
    let newer_version_of_ids =
      match newer_version_of with
      | None -> Ids_for_export.empty
      | Some older -> Ids_for_export.add_code_id Ids_for_export.empty older
    in
    Ids_for_export.add_code_id newer_version_of_ids code_id
  in
  Ids_for_export.union ids
    (match result_types with
    | Unknown | Bottom -> Ids_for_export.empty
    | Ok result_types -> Result_types.ids_for_export result_types)

let approx_equal
    { code_id = code_id1;
      newer_version_of = newer_version_of1;
      params_arity = params_arity1;
      param_modes = param_modes1;
      first_complex_local_param = first_complex_local_param1;
      result_arity = result_arity1;
      result_types = _;
      result_mode = result_mode1;
      contains_no_escaping_local_allocs = contains_no_escaping_local_allocs1;
      stub = stub1;
      inline = inline1;
      zero_alloc_attribute = zero_alloc_attribute1;
      poll_attribute = poll_attribute1;
      is_a_functor = is_a_functor1;
      is_opaque = is_opaque1;
      recursive = recursive1;
      cost_metrics = cost_metrics1;
      inlining_arguments = inlining_arguments1;
      dbg = dbg1;
      is_tupled = is_tupled1;
      is_my_closure_used = is_my_closure_used1;
      inlining_decision = inlining_decision1;
      absolute_history = absolute_history1;
      relative_history = relative_history1;
      loopify = loopify1
    }
    { code_id = code_id2;
      newer_version_of = newer_version_of2;
      params_arity = params_arity2;
      param_modes = param_modes2;
      first_complex_local_param = first_complex_local_param2;
      result_arity = result_arity2;
      result_types = _;
      result_mode = result_mode2;
      contains_no_escaping_local_allocs = contains_no_escaping_local_allocs2;
      stub = stub2;
      inline = inline2;
      zero_alloc_attribute = zero_alloc_attribute2;
      poll_attribute = poll_attribute2;
      is_a_functor = is_a_functor2;
      is_opaque = is_opaque2;
      recursive = recursive2;
      cost_metrics = cost_metrics2;
      inlining_arguments = inlining_arguments2;
      dbg = dbg2;
      is_tupled = is_tupled2;
      is_my_closure_used = is_my_closure_used2;
      inlining_decision = inlining_decision2;
      absolute_history = absolute_history2;
      relative_history = relative_history2;
      loopify = loopify2
    } =
  Code_id.equal code_id1 code_id2
  && (Option.equal Code_id.equal) newer_version_of1 newer_version_of2
  && Flambda_arity.equal_ignoring_subkinds params_arity1 params_arity2
  && List.equal Alloc_mode.For_types.equal param_modes1 param_modes2
  && Int.equal first_complex_local_param1 first_complex_local_param2
  && Flambda_arity.equal_ignoring_subkinds result_arity1 result_arity2
  && Lambda.equal_alloc_mode result_mode1 result_mode2
  && Bool.equal contains_no_escaping_local_allocs1
       contains_no_escaping_local_allocs2
  && Bool.equal stub1 stub2
  && Inline_attribute.equal inline1 inline2
  && Zero_alloc_attribute.equal zero_alloc_attribute1 zero_alloc_attribute2
  && Poll_attribute.equal poll_attribute1 poll_attribute2
  && Bool.equal is_a_functor1 is_a_functor2
  && Bool.equal is_opaque1 is_opaque2
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
  && Loopify_attribute.equal loopify1 loopify2

let map_result_types ({ result_types; _ } as t) ~f =
  { t with
    result_types =
      Or_unknown_or_bottom.map result_types
        ~f:(Result_types.map_result_types ~f)
  }
