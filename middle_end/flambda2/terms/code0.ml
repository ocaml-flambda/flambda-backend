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

(* CR mshinwell: We should be able to remove the parameterisation over
   [Cost_metrics] once e.g. [Cost_metrics.expr_size] has gone. *)

module Make (Function_params_and_body : sig
  type t

  include Contains_ids.S with type t := t

  val apply_renaming : t -> Renaming.t -> t

  val print : Format.formatter -> t -> unit

  val print_with_cache
     : cache:Printing_cache.t
    -> Format.formatter
    -> t
    -> unit
end) (Cost_metrics : sig
  type t

  val print : Format.formatter -> t -> unit
end) = struct
  type t = {
    code_id : Code_id.t;
    params_and_body : Function_params_and_body.t Or_deleted.t;
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
  }

  let code_id { code_id; _ } = code_id

  let params_and_body { params_and_body; _ } = params_and_body

  let params_and_body_opt { params_and_body; _ } =
    match params_and_body with
    | Deleted -> None
    | Present params_and_body -> Some params_and_body

  let params_and_body_must_be_present ~error_context { params_and_body; _ } =
    match params_and_body with
    | Deleted -> Misc.fatal_errorf "%s: params and body are deleted" error_context
    | Present params_and_body -> params_and_body

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

  let check_params_and_body code_id (params_and_body : _ Or_deleted.t) =
    let free_names_of_params_and_body =
      match params_and_body with
      | Deleted -> Name_occurrences.empty
      | Present (params_and_body, free_names) ->
        if not (Name_occurrences.no_continuations free_names
                && Name_occurrences.no_variables free_names)
        then begin
          Misc.fatal_errorf "Incorrect free names:@ %a@ for creation of code:@ \
              %a@ =@ %a"
            Name_occurrences.print free_names
            Code_id.print code_id
            Function_params_and_body.print params_and_body
        end;
        free_names
    in
    let params_and_body : _ Or_deleted.t =
      match params_and_body with
      | Deleted -> Deleted
      | Present (params_and_body, _free_names) -> Present params_and_body
    in
    params_and_body, free_names_of_params_and_body

  let create
        code_id
        ~(params_and_body : _ Or_deleted.t)
        ~newer_version_of
        ~params_arity
        ~result_arity
        ~stub
        ~(inline:Inline_attribute.t)
        ~is_a_functor
        ~recursive
        ~cost_metrics
        ~inlining_arguments
        ~dbg
        ~is_tupled =
    begin match stub, inline with
    | true, (Hint_inline | Never_inline | Default_inline)
    | false, (Never_inline | Default_inline | Always_inline | Hint_inline
              | Unroll _) -> ()
    | true, (Always_inline | Unroll _) ->
      Misc.fatal_error "Stubs may not be annotated as [Always_inline] or \
        [Unroll]"
    end;
    let params_and_body, free_names_of_params_and_body =
      check_params_and_body code_id params_and_body
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
    }

  let with_code_id code_id t = { t with code_id }

  let with_params_and_body params_and_body ~cost_metrics t =
    let params_and_body, free_names_of_params_and_body =
      check_params_and_body t.code_id params_and_body
    in
    { t with params_and_body; cost_metrics; free_names_of_params_and_body; }

  let with_newer_version_of newer_version_of t = { t with newer_version_of }

  let print_params_and_body_with_cache ~cache ppf params_and_body =
    match params_and_body with
    | Or_deleted.Deleted -> Format.fprintf ppf "Deleted"
    | Or_deleted.Present params_and_body ->
      Function_params_and_body.print_with_cache ~cache ppf
        params_and_body

  module Option = struct
    include Option

    let print_compact print_contents ppf t =
      match t with
      | None -> Format.pp_print_string ppf "()"
      | Some contents ->
        Format.fprintf ppf "%a" print_contents contents
  end

  let print_with_cache ~cache ppf
        { code_id = _; params_and_body; newer_version_of; stub; inline;
          is_a_functor; params_arity; result_arity; recursive;
          free_names_of_params_and_body = _; cost_metrics; inlining_arguments;
          dbg; is_tupled; } =
    let module C = Flambda_colours in
    match params_and_body with
    | Present _ ->
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
          @[<hov 1>@<0>%s(is_tupled @ %b)@<0>%s@]@ \
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
        (print_params_and_body_with_cache ~cache) params_and_body
    | Deleted ->
      Format.fprintf ppf "@[<hov 1>(\
          @[<hov 1>@<0>%s(newer_version_of@ %a)@<0>%s@]@ \
          Deleted\
          )@]"
        (if Option.is_none newer_version_of then Flambda_colours.elide ()
        else Flambda_colours.normal ())
        (Option.print_compact Code_id.print) newer_version_of
        (Flambda_colours.normal ())

  let print ppf code =
    print_with_cache ~cache:(Printing_cache.create ()) ppf code

  let compare { code_id = code_id1; _ } { code_id = code_id2; _ } =
    Code_id.compare code_id1 code_id2

  let free_names t =
    (* [code_id] is only in [t] for the use of [compare]; it doesn't
        count as a free name. *)
    let from_newer_version_of =
      match t.newer_version_of with
      | None -> Name_occurrences.empty
      | Some older ->
        Name_occurrences.add_newer_version_of_code_id
          Name_occurrences.empty older Name_mode.normal
    in
    Name_occurrences.union from_newer_version_of t.free_names_of_params_and_body

  let apply_renaming
        ({ code_id; params_and_body; newer_version_of; params_arity = _;
          result_arity = _; stub = _; inline = _; is_a_functor = _;
           recursive = _; cost_metrics = _; free_names_of_params_and_body;
           inlining_arguments = _; dbg = _; is_tupled = _; } as t)
        perm =
    (* inlined and modified version of Option.map to preserve sharing *)
    let newer_version_of' =
      match newer_version_of with
      | None -> newer_version_of
      | Some code_id ->
        let code_id' = Renaming.apply_code_id perm code_id in
        if code_id == code_id'
        then newer_version_of
        else Some code_id'
    in
    let code_id' = Renaming.apply_code_id perm code_id in
    let params_and_body' : Function_params_and_body.t Or_deleted.t =
      match params_and_body with
      | Deleted -> Deleted
      | Present params_and_body_inner ->
        let params_and_body_inner' =
          Function_params_and_body.apply_renaming
            params_and_body_inner perm
        in
        if params_and_body_inner == params_and_body_inner' then
          params_and_body
        else
          Present params_and_body_inner'
    in
    if params_and_body == params_and_body' &&
      code_id == code_id' &&
      newer_version_of == newer_version_of' then t
    else begin
      let free_names_of_params_and_body' =
        Name_occurrences.apply_renaming free_names_of_params_and_body perm
      in
      { t with code_id = code_id';
              params_and_body = params_and_body';
              newer_version_of = newer_version_of';
              free_names_of_params_and_body = free_names_of_params_and_body';
      }
    end

  let all_ids_for_export { code_id; params_and_body; newer_version_of;
                          params_arity = _; result_arity = _; stub = _;
                          inline = _; is_a_functor = _; recursive = _;
                           cost_metrics = _; free_names_of_params_and_body = _;
                           inlining_arguments = _; dbg = _; is_tupled = _; } =
    let newer_version_of_ids =
      match newer_version_of with
      | None -> Ids_for_export.empty
      | Some older ->
        Ids_for_export.add_code_id Ids_for_export.empty older
    in
    let params_and_body_ids =
      match params_and_body with
      | Deleted -> Ids_for_export.empty
      | Present params_and_body ->
        Function_params_and_body.all_ids_for_export params_and_body
    in
    Ids_for_export.add_code_id
      (Ids_for_export.union newer_version_of_ids params_and_body_ids)
      code_id

  let make_deleted t =
    { t with
      params_and_body = Deleted;
      free_names_of_params_and_body = Name_occurrences.empty; }

  let is_deleted t =
    match t.params_and_body with
    | Deleted -> true
    | Present _ -> false
end
