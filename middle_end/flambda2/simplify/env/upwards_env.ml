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
    continuation_shortcuts : Continuation_shortcut.t Continuation.Map.t;
    apply_cont_rewrites : Apply_cont_rewrite.t Continuation.Map.t;
    (* this [are_rebuilding_terms] is **only** used for printing *)
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
    (Continuation.Map.print Continuation_shortcut.print)
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
      "@[<hov 2>The continuation shortcut map does not represent the \
       transitive closure of shortcut equations on continuations:@ %a, \
       shortcut for %a, is already bound in %a@]"
      Continuation.print shortcut_to Continuation.print cont print t

let find_continuation_shortcut t cont =
  match Continuation.Map.find cont t.continuation_shortcuts with
  | exception Not_found -> None
  | shortcut_to ->
    check_shortcut_transitivity t cont
      (Continuation_shortcut.continuation shortcut_to);
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

let continuation_arity t cont =
  find_continuation t cont |> Continuation_in_env.arity

let add_continuation_shortcut t cont ~params ~shortcut_to ~args =
  if Continuation.Map.mem shortcut_to t.continuation_shortcuts
  then
    Misc.fatal_errorf
      "Cannot add continuation shortcut %a (as shortcut to %a); the target \
       continuation is itself a shortcut"
      Continuation.print cont Continuation.print shortcut_to;
  match Continuation.Map.find cont t.continuation_shortcuts with
  | existing_shortcut ->
    Misc.fatal_errorf
      "Cannot add continuation shortcut %a (as shortcut to%a); the source \
       continuation is already deemed to be a shortcut (to %a)"
      Continuation.print cont Continuation.print shortcut_to Continuation.print
      (Continuation_shortcut.continuation existing_shortcut)
  | exception Not_found ->
    let shortcut = Continuation_shortcut.create ~params shortcut_to args in
    (* For now we keep the pre-existing check for aliases, but ideally we should
       check that the arities match after applying the substitution. However it
       seems like we should have a more general mechanism for checking arities
       when applying continuations that's independent of shortcuts. *)
    (match Continuation_shortcut.to_alias shortcut with
    | Some alias_for ->
      let arity = Bound_parameters.arity params in
      let alias_for_arity = continuation_arity t alias_for in
      if not (Flambda_arity.equal_ignoring_subkinds arity alias_for_arity)
      then
        Misc.fatal_errorf
          "%a (arity %a) cannot be an alias for %a (arity %a) since the two \
           continuations differ in arity"
          Continuation.print cont Flambda_arity.print arity Continuation.print
          alias_for Flambda_arity.print alias_for_arity
    | None -> ());
    let continuation_shortcuts =
      Continuation.Map.add cont shortcut t.continuation_shortcuts
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
