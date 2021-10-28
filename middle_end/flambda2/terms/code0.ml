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

module Params_and_body_state = struct
  type 'function_params_and_body t =
    | Inlinable of 'function_params_and_body
    | Non_inlinable
    | Cannot_be_called

  let inlinable params_and_body = Inlinable params_and_body

  (* This function is deliberately not exposed in the .mli to make it clear that
     the transition from inlinable to non-inlinable only happens when
     [make_non_inlinable] (see below) is called. *)
  let non_inlinable = Non_inlinable

  let cannot_be_called = Cannot_be_called

  let map t ~f =
    match t with
    | Inlinable params_and_body -> Inlinable (f params_and_body)
    | Non_inlinable -> Non_inlinable
    | Cannot_be_called -> Cannot_be_called

  let print print_params_and_body ppf t =
    match t with
    | Inlinable params_and_body ->
      Format.fprintf ppf "@[<hov 1>(Inlinable@ %a)@]" print_params_and_body
        params_and_body
    | Non_inlinable -> Format.fprintf ppf "Non_inlinable"
    | Cannot_be_called -> Format.fprintf ppf "Cannot_be_called"
end

type 'function_params_and_body t =
  { code_id : Code_id.t;
    params_and_body : 'function_params_and_body Params_and_body_state.t;
    free_names_of_params_and_body : Name_occurrences.t;
    newer_version_of : Code_id.t option;
    params_arity : Flambda_arity.With_subkinds.t;
    result_arity : Flambda_arity.With_subkinds.t;
    stub : bool;
    inline : Inline_attribute.t;
    is_a_functor : bool;
    recursive : Recursive.t;
    cost_metrics : Cost_metrics.t;
    inlining_arguments : Inlining_arguments.t;
    dbg : Debuginfo.t;
    is_tupled : bool;
    is_my_closure_used : bool;
    inlining_decision : Function_decl_inlining_decision_type.t
  }

let code_id { code_id; _ } = code_id

let params_and_body { params_and_body; _ } = params_and_body

let newer_version_of { newer_version_of; _ } = newer_version_of

let params_arity { params_arity; _ } = params_arity

let result_arity { result_arity; _ } = result_arity

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

let check_params_and_body ~print_function_params_and_body code_id
    (params_and_body : _ Params_and_body_state.t) =
  let free_names_of_params_and_body =
    match params_and_body with
    | Cannot_be_called | Non_inlinable -> Name_occurrences.empty
    | Inlinable (params_and_body, free_names_of_params_and_body) ->
      if not
           (Name_occurrences.no_continuations free_names_of_params_and_body
           && Name_occurrences.no_variables free_names_of_params_and_body)
      then
        Misc.fatal_errorf
          "Incorrect free names:@ %a@ for creation of code:@ %a@ =@ %a"
          Name_occurrences.print free_names_of_params_and_body Code_id.print
          code_id print_function_params_and_body params_and_body;
      free_names_of_params_and_body
  in
  let params_and_body : _ Params_and_body_state.t =
    match params_and_body with
    | Cannot_be_called -> Cannot_be_called
    | Non_inlinable -> Non_inlinable
    | Inlinable (params_and_body, _) -> Inlinable params_and_body
  in
  params_and_body, free_names_of_params_and_body

let create ~print_function_params_and_body code_id
    ~(params_and_body : _ Params_and_body_state.t) ~newer_version_of
    ~params_arity ~result_arity ~stub ~(inline : Inline_attribute.t)
    ~is_a_functor ~recursive ~cost_metrics ~inlining_arguments ~dbg ~is_tupled
    ~is_my_closure_used ~inlining_decision =
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
  let params_and_body, free_names_of_params_and_body =
    check_params_and_body ~print_function_params_and_body code_id
      params_and_body
  in
  { code_id;
    params_and_body;
    free_names_of_params_and_body;
    newer_version_of;
    params_arity;
    result_arity;
    stub;
    inline;
    is_a_functor;
    recursive;
    cost_metrics;
    inlining_arguments;
    dbg;
    is_tupled;
    is_my_closure_used;
    inlining_decision
  }

let make_non_inlinable t =
  match t.params_and_body with
  | Inlinable _ ->
    { t with
      params_and_body = Params_and_body_state.non_inlinable;
      free_names_of_params_and_body = Name_occurrences.empty
    }
  | Non_inlinable -> t
  | Cannot_be_called ->
    Misc.fatal_errorf
      "A piece of code in [Cannot_be_called] state cannot be transitioned to \
       [Non_inlinable] state"

let with_code_id code_id t = { t with code_id }

let with_params_and_body ~print_function_params_and_body params_and_body
    ~cost_metrics t =
  let params_and_body, free_names_of_params_and_body =
    check_params_and_body ~print_function_params_and_body t.code_id
      params_and_body
  in
  { t with params_and_body; cost_metrics; free_names_of_params_and_body }

let with_newer_version_of newer_version_of t = { t with newer_version_of }

module Option = struct
  include Option

  let print_compact print_contents ppf t =
    match t with
    | None -> Format.pp_print_string ppf "()"
    | Some contents -> Format.fprintf ppf "%a" print_contents contents
end

let [@ocamlformat "disable"] print ~print_function_params_and_body ppf
      { code_id = _; params_and_body; newer_version_of; stub; inline;
        is_a_functor; params_arity; result_arity; recursive;
        free_names_of_params_and_body = _; cost_metrics; inlining_arguments;
        dbg; is_tupled; is_my_closure_used; inlining_decision; } =
  let module C = Flambda_colours in
  match params_and_body with
  | Inlinable _  ->
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>@<0>%s(newer_version_of@ %a)@<0>%s@]@ \
        @[<hov 1>@<0>%s(stub@ %b)@<0>%s@]@ \
        @[<hov 1>@<0>%s(inline@ %a)@<0>%s@]@ \
        @[<hov 1>@<0>%s(is_a_functor@ %b)@<0>%s@]@ \
        @[<hov 1>@<0>%s(params_arity@ @<0>%s%a@<0>%s)@<0>%s@]@ \
        @[<hov 1>@<0>%s(result_arity@ @<0>%s%a@<0>%s)@<0>%s@]@ \
        @[<hov 1>@<0>%s(recursive@ %a)@<0>%s@]@ \
        @[<hov 1>(cost_metrics@ %a)@]@ \
        @[<hov 1>(inlining_arguments@ %a)@]@ \
        @[<hov 1>@<0>%s(dbg@ %a)@<0>%s@]@ \
        @[<hov 1>@<0>%s(is_tupled@ %b)@<0>%s@]@ \
        @[<hov 1>(is_my_closure_used@ %b)@]@ \
        @[<hov 1>(inlining_decision@ %a)@]@ \
        %a\
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
      (if Flambda_arity.With_subkinds.is_singleton_value result_arity
      then Flambda_colours.elide ()
      else Flambda_colours.normal ())
      (Flambda_colours.normal ())
      Flambda_arity.With_subkinds.print result_arity
      (if Flambda_arity.With_subkinds.is_singleton_value result_arity
      then Flambda_colours.elide ()
      else Flambda_colours.normal ())
      (Flambda_colours.normal ())
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
      Function_decl_inlining_decision_type.print inlining_decision
      (Params_and_body_state.print print_function_params_and_body) params_and_body
  | Non_inlinable ->
    Format.fprintf ppf "Non_inlinable"
  | Cannot_be_called ->
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>@<0>%s(newer_version_of@ %a)@<0>%s@]@ \
        Cannot_be_called\
        )@]"
      (if Option.is_none newer_version_of then Flambda_colours.elide ()
      else Flambda_colours.normal ())
      (Option.print_compact Code_id.print) newer_version_of
      (Flambda_colours.normal ())

let compare { code_id = code_id1; _ } { code_id = code_id2; _ } =
  Code_id.compare code_id1 code_id2

let free_names t =
  (* [code_id] is only in [t] for the use of [compare]; it doesn't count as a
     free name. *)
  let from_newer_version_of =
    match t.newer_version_of with
    | None -> Name_occurrences.empty
    | Some older ->
      Name_occurrences.add_newer_version_of_code_id Name_occurrences.empty older
        Name_mode.normal
  in
  Name_occurrences.union from_newer_version_of t.free_names_of_params_and_body

let apply_renaming ~apply_renaming_function_params_and_body
    ({ code_id;
       params_and_body;
       newer_version_of;
       params_arity = _;
       result_arity = _;
       stub = _;
       inline = _;
       is_a_functor = _;
       recursive = _;
       cost_metrics = _;
       free_names_of_params_and_body;
       inlining_arguments = _;
       dbg = _;
       is_tupled = _;
       is_my_closure_used = _;
       inlining_decision = _
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
  let params_and_body' =
    match params_and_body with
    | Cannot_be_called -> Params_and_body_state.cannot_be_called
    | Non_inlinable -> Params_and_body_state.non_inlinable
    | Inlinable params_and_body_inner ->
      let params_and_body_inner' =
        apply_renaming_function_params_and_body params_and_body_inner perm
      in
      if params_and_body_inner == params_and_body_inner'
      then params_and_body
      else Params_and_body_state.inlinable params_and_body_inner'
  in
  if params_and_body == params_and_body'
     && code_id == code_id'
     && newer_version_of == newer_version_of'
  then t
  else
    let free_names_of_params_and_body' =
      Name_occurrences.apply_renaming free_names_of_params_and_body perm
    in
    { t with
      code_id = code_id';
      params_and_body = params_and_body';
      newer_version_of = newer_version_of';
      free_names_of_params_and_body = free_names_of_params_and_body'
    }

let all_ids_for_export ~all_ids_for_export_function_params_and_body
    { code_id;
      params_and_body;
      newer_version_of;
      params_arity = _;
      result_arity = _;
      stub = _;
      inline = _;
      is_a_functor = _;
      recursive = _;
      cost_metrics = _;
      free_names_of_params_and_body;
      inlining_arguments = _;
      dbg = _;
      is_tupled = _;
      is_my_closure_used = _;
      inlining_decision = _
    } =
  let newer_version_of_ids =
    match newer_version_of with
    | None -> Ids_for_export.empty
    | Some older -> Ids_for_export.add_code_id Ids_for_export.empty older
  in
  let params_and_body_ids =
    match params_and_body with
    | Cannot_be_called -> Ids_for_export.empty
    | Inlinable params_and_body ->
      all_ids_for_export_function_params_and_body params_and_body
    | Non_inlinable ->
      (* Usually the ids for export collected from the [params_and_body] in the
         inlinable case are used to rename [free_names_of_params_and_body] upon
         import. Since we don't know what those ids for export are in the
         non-inlinable case, we double-check that the
         [free_names_of_params_and_body] field has been correctly cleared. *)
      assert (Name_occurrences.is_empty free_names_of_params_and_body);
      Ids_for_export.empty
  in
  Ids_for_export.add_code_id
    (Ids_for_export.union newer_version_of_ids params_and_body_ids)
    code_id

let make_not_callable t =
  { t with
    params_and_body = Cannot_be_called;
    free_names_of_params_and_body = Name_occurrences.empty
  }

let is_non_callable t =
  match t.params_and_body with
  | Cannot_be_called -> true
  | Inlinable _ | Non_inlinable -> false
