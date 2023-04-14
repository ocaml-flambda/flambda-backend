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

type t =
  { continuations : Continuation_in_env.t Continuation.Map.t;
    continuation_aliases : Continuation.t Continuation.Map.t;
    apply_cont_rewrites : Apply_cont_rewrite.t Continuation.Map.t;
    are_rebuilding_terms : Are_rebuilding_terms.t
  }

let create are_rebuilding_terms =
  { continuations = Continuation.Map.empty;
    continuation_aliases = Continuation.Map.empty;
    apply_cont_rewrites = Continuation.Map.empty;
    are_rebuilding_terms
  }

let [@ocamlformat "disable"] print ppf
    { continuations; continuation_aliases;
      apply_cont_rewrites; are_rebuilding_terms } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuations@ %a)@]@ \
      @[<hov 1>(continuation_aliases@ %a)@]@ \
      @[<hov 1>(apply_cont_rewrites@ %a)@]\
      )@]"
    (Continuation.Map.print (Continuation_in_env.print are_rebuilding_terms))
    continuations
    (Continuation.Map.print Continuation.print) continuation_aliases
    (Continuation.Map.print Apply_cont_rewrite.print)
    apply_cont_rewrites

let find_continuation t cont =
  match Continuation.Map.find cont t.continuations with
  | exception Not_found ->
    Misc.fatal_errorf "Unbound continuation %a in upwards environment:@ %a"
      Continuation.print cont print t
  | cont_in_env -> cont_in_env

let mem_continuation t cont = Continuation.Map.mem cont t.continuations

let check_alias_transitivity t cont alias_for =
  if Flambda_features.check_invariants ()
     && Continuation.Map.mem alias_for t.continuation_aliases
  then
    Misc.fatal_errorf
      "@[<hov 2>The continuation alias map does not represent thetransitive \
       closure of alias equations on continuations:@ %a, alias for %a, is \
       already bound in %a@]"
      Continuation.print alias_for Continuation.print cont print t

let resolve_continuation_aliases t cont =
  match Continuation.Map.find cont t.continuation_aliases with
  | exception Not_found -> cont
  | alias_for ->
    check_alias_transitivity t cont alias_for;
    alias_for

let resolve_exn_continuation_aliases t exn_cont =
  let cont = Exn_continuation.exn_handler exn_cont in
  match Continuation.Map.find cont t.continuation_aliases with
  | exception Not_found -> exn_cont
  | alias_for ->
    check_alias_transitivity t cont alias_for;
    Exn_continuation.with_exn_handler exn_cont alias_for

let continuation_arity t cont =
  find_continuation t cont |> Continuation_in_env.arity

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

let add_continuation_alias t cont arity ~alias_for =
  let alias_for = resolve_continuation_aliases t alias_for in
  let alias_for_arity = continuation_arity t alias_for in
  if not (Flambda_arity.equal_ignoring_subkinds arity alias_for_arity)
  then
    Misc.fatal_errorf
      "%a (arity %a) cannot be an alias for %a (arity %a) since the two \
       continuations differ in arity"
      Continuation.print cont Flambda_arity.print arity Continuation.print
      alias_for Flambda_arity.print alias_for_arity;
  if Continuation.Map.mem cont t.continuation_aliases
  then
    Misc.fatal_errorf
      "Cannot add continuation alias %a (as alias for %a); the continuation is \
       already deemed to be an alias"
      Continuation.print cont Continuation.print alias_for;
  let continuation_aliases =
    Continuation.Map.add cont alias_for t.continuation_aliases
  in
  { t with continuation_aliases }

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
