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

type continuation_shortcut =
  { params : Bound_parameters.t;
    continuation : Continuation.t;
    args : Simple.t list
  }

let apply_continuation_shortcut { params; continuation; args } shortcut_args =
  let subst =
    List.fold_left2
      (fun subst param arg -> Variable.Map.add param arg subst)
      Variable.Map.empty
      (Bound_parameters.vars params)
      shortcut_args
  in
  ( continuation,
    List.map
      (fun arg ->
        Simple.pattern_match' arg
          ~var:(fun var ~coercion ->
            match Variable.Map.find var subst with
            | exception Not_found -> arg
            | simple -> Simple.apply_coercion_exn simple coercion)
          ~symbol:(fun _ ~coercion:_ -> arg)
          ~const:(fun _ -> arg))
      args )

let print_continuation_shortcut ppf { params; continuation; args } =
  Format.fprintf ppf "\\%a -> %a(%a)" Bound_parameters.print params
    Continuation.print continuation
    (Format.pp_print_list
       ~pp_sep:(fun ppf () -> Format.fprintf ppf ",@ ")
       Simple.print)
    args

type t =
  { continuations : Continuation_in_env.t Continuation.Map.t;
    continuation_shortcuts : continuation_shortcut Continuation.Map.t;
    apply_cont_rewrites : Apply_cont_rewrite.t Continuation.Map.t;
    are_rebuilding_terms : Are_rebuilding_terms.t
  }

let create are_rebuilding_terms =
  { continuations = Continuation.Map.empty;
    continuation_shortcuts = Continuation.Map.empty;
    apply_cont_rewrites = Continuation.Map.empty;
    are_rebuilding_terms
  }

let [@ocamlformat "disable"] print ppf
    { continuations;
      apply_cont_rewrites; are_rebuilding_terms ;
      continuation_shortcuts } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuations@ %a)@]@ \
      @[<hov 1>(continuation_shortcuts@ %a)@]@ \
      @[<hov 1>(apply_cont_rewrites@ %a)@]\
      )@]"
    (Continuation.Map.print (Continuation_in_env.print are_rebuilding_terms))
    continuations
    (Continuation.Map.print print_continuation_shortcut)
    continuation_shortcuts
    (Continuation.Map.print Apply_cont_rewrite.print)
    apply_cont_rewrites

let find_continuation t cont =
  match Continuation.Map.find cont t.continuations with
  | exception Not_found ->
    Misc.fatal_errorf "Unbound continuation %a in upwards environment:@ %a"
      Continuation.print cont print t
  | cont_in_env -> cont_in_env

let mem_continuation t cont = Continuation.Map.mem cont t.continuations

let check_shortcut_transitivity t cont shortcut_to =
  if Flambda_features.check_invariants ()
     && Continuation.Map.mem shortcut_to t.continuation_shortcuts
  then
    Misc.fatal_errorf
      "@[<hov 2>The continuation alias map does not represent the transitive \
       closure of alias equations on continuations:@ %a, alias for %a, is \
       already bound in %a@]"
      Continuation.print shortcut_to Continuation.print cont print t

let find_continuation_shortcut t cont =
  match Continuation.Map.find cont t.continuation_shortcuts with
  | exception Not_found -> None
  | shortcut_to ->
    check_shortcut_transitivity t cont shortcut_to.continuation;
    Some shortcut_to

let add_continuation0 t cont cont_in_env =
  let continuations = Continuation.Map.add cont cont_in_env t.continuations in
  { t with continuations }

let add_non_inlinable_continuation t cont ~params ~handler =
  if Bound_parameters.is_empty params
  then add_continuation0 t cont (Non_inlinable_zero_arity { handler })
  else
    let arity = Bound_parameters.arity params in
    add_continuation0 t cont (Non_inlinable_non_zero_arity { arity })

let add_invalid_continuation t cont arity =
  add_continuation0 t cont (Invalid { arity })

let add_continuation_shortcut t cont ~params ~shortcut_to ~args =
  let shortcut_to, args =
    match find_continuation_shortcut t shortcut_to with
    | None -> shortcut_to, args
    | Some shortcut -> apply_continuation_shortcut shortcut args
  in
  if Continuation.Map.mem cont t.continuation_shortcuts
  then
    Misc.fatal_errorf
      "Cannot add continuation shortcut %a (as shortcut to%a); the \
       continuation is already deemed to be a shortcut"
      Continuation.print cont Continuation.print shortcut_to;
  let continuation_shortcuts =
    Continuation.Map.add cont
      { params; continuation = shortcut_to; args }
      t.continuation_shortcuts
  in
  { t with continuation_shortcuts }

let add_linearly_used_inlinable_continuation t cont ~params ~handler
    ~free_names_of_handler ~cost_metrics_of_handler =
  add_continuation0 t cont
    (Linearly_used_and_inlinable
       { handler; free_names_of_handler; params; cost_metrics_of_handler })

let add_function_return_or_exn_continuation t cont arity =
  add_continuation0 t cont
    (Toplevel_or_function_return_or_exn_continuation { arity })

let add_apply_cont_rewrite t cont rewrite =
  if Continuation.Map.mem cont t.apply_cont_rewrites
  then
    Misc.fatal_errorf "Cannot redefine [Apply_cont_rewrite] for %a"
      Continuation.print cont;
  let apply_cont_rewrites =
    Continuation.Map.add cont rewrite t.apply_cont_rewrites
  in
  { t with apply_cont_rewrites }

let replace_apply_cont_rewrite t cont rewrite =
  if not (Continuation.Map.mem cont t.apply_cont_rewrites)
  then
    Misc.fatal_errorf "Must redefine [Apply_cont_rewrite] for %a"
      Continuation.print cont;
  let apply_cont_rewrites =
    Continuation.Map.add cont rewrite t.apply_cont_rewrites
  in
  { t with apply_cont_rewrites }

let find_apply_cont_rewrite t cont =
  match Continuation.Map.find cont t.apply_cont_rewrites with
  | exception Not_found -> None
  | rewrite -> Some rewrite
