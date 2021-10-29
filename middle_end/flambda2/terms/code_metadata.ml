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

let create code_id ~newer_version_of ~params_arity ~result_arity ~stub
    ~(inline : Inline_attribute.t) ~is_a_functor ~recursive ~cost_metrics
    ~inlining_arguments ~dbg ~is_tupled ~is_my_closure_used ~inlining_decision =
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
  { code_id;
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

let [@ocamlformat "disable"] print ppf
      { code_id = _; newer_version_of; stub; inline; is_a_functor;
        params_arity; result_arity; recursive; cost_metrics; inlining_arguments;
        dbg; is_tupled; is_my_closure_used; inlining_decision; } =
  let module C = Flambda_colours in
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

let free_names
    { code_id = _;
      newer_version_of;
      params_arity = _;
      result_arity = _;
      stub = _;
      inline = _;
      is_a_functor = _;
      recursive = _;
      cost_metrics = _;
      inlining_arguments = _;
      dbg = _;
      is_tupled = _;
      is_my_closure_used = _;
      inlining_decision = _
    } =
  (* [code_id] is only in [t.code_metadata] for the use of [compare]; it doesn't
     count as a free name. *)
  match newer_version_of with
  | None -> Name_occurrences.empty
  | Some older ->
    Name_occurrences.add_newer_version_of_code_id Name_occurrences.empty older
      Name_mode.normal

let apply_renaming
    ({ code_id;
       newer_version_of;
       params_arity = _;
       result_arity = _;
       stub = _;
       inline = _;
       is_a_functor = _;
       recursive = _;
       cost_metrics = _;
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
  if code_id == code_id' && newer_version_of == newer_version_of'
  then t
  else { t with code_id = code_id'; newer_version_of = newer_version_of' }

let all_ids_for_export
    { code_id;
      newer_version_of;
      params_arity = _;
      result_arity = _;
      stub = _;
      inline = _;
      is_a_functor = _;
      recursive = _;
      cost_metrics = _;
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
  Ids_for_export.add_code_id newer_version_of_ids code_id

let equal
    { code_id = code_id1;
      newer_version_of = newer_version_of1;
      params_arity = params_arity1;
      result_arity = result_arity1;
      stub = stub1;
      inline = inline1;
      is_a_functor = is_a_functor1;
      recursive = recursive1;
      cost_metrics = cost_metrics1;
      inlining_arguments = inlining_arguments1;
      dbg = dbg1;
      is_tupled = is_tupled1;
      is_my_closure_used = is_my_closure_used1;
      inlining_decision = inlining_decision1
    }
    { code_id = code_id2;
      newer_version_of = newer_version_of2;
      params_arity = params_arity2;
      result_arity = result_arity2;
      stub = stub2;
      inline = inline2;
      is_a_functor = is_a_functor2;
      recursive = recursive2;
      cost_metrics = cost_metrics2;
      inlining_arguments = inlining_arguments2;
      dbg = dbg2;
      is_tupled = is_tupled2;
      is_my_closure_used = is_my_closure_used2;
      inlining_decision = inlining_decision2
    } =
  Code_id.equal code_id1 code_id2
  && (Option.equal Code_id.equal) newer_version_of1 newer_version_of2
  && Flambda_arity.With_subkinds.equal params_arity1 params_arity2
  && Flambda_arity.With_subkinds.equal result_arity1 result_arity2
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
