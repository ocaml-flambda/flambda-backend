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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t = {
  continuations : (Scope.t * Continuation_in_env.t) Continuation.Map.t;
  exn_continuations : Scope.t Exn_continuation.Map.t;
  continuation_aliases : Continuation.t Continuation.Map.t;
  apply_cont_rewrites : Apply_cont_rewrite.t Continuation.Map.t;
}

let invariant _t = ()

let empty =
  { continuations = Continuation.Map.empty;
    exn_continuations = Exn_continuation.Map.empty;
    continuation_aliases = Continuation.Map.empty;
    apply_cont_rewrites = Continuation.Map.empty;
  }

let print_scope_level_and_continuation_in_env ppf (scope_level, cont_in_env) =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(scope_level@ %a)@]@ \
      @[<hov 1>(cont_in_env@ %a)@]\
      )@]"
    Scope.print scope_level
    Continuation_in_env.print cont_in_env

let print ppf { continuations; exn_continuations; continuation_aliases;
                apply_cont_rewrites;
              } =
  Format.fprintf ppf "@[<hov 1>(\
      @[<hov 1>(continuations@ %a)@]@ \
      @[<hov 1>(exn_continuations@ %a)@]@ \
      @[<hov 1>(continuation_aliases@ %a)@]@ \
      @[<hov 1>(apply_cont_rewrites@ %a)@]\
      )@]"
    (Continuation.Map.print print_scope_level_and_continuation_in_env)
    continuations
    (Exn_continuation.Map.print Scope.print) exn_continuations
    (Continuation.Map.print Continuation.print) continuation_aliases
    (Continuation.Map.print Apply_cont_rewrite.print)
    apply_cont_rewrites

let find_continuation t cont =
  match Continuation.Map.find cont t.continuations with
  | exception Not_found ->
    Misc.fatal_errorf "Unbound continuation %a in upwards environment:@ %a"
      Continuation.print cont
      print t
  | (_scope_level, cont_in_env) -> cont_in_env

let mem_continuation t cont =
  Continuation.Map.mem cont t.continuations

let resolve_continuation_aliases t cont =
  match Continuation.Map.find cont t.continuation_aliases with
  | exception Not_found -> cont
  | alias_for -> alias_for

let resolve_exn_continuation_aliases t exn_cont =
  let cont = Exn_continuation.exn_handler exn_cont in
  match Continuation.Map.find cont t.continuation_aliases with
  | exception Not_found -> exn_cont
  | alias_for -> Exn_continuation.with_exn_handler exn_cont alias_for

let continuation_arity t cont =
  find_continuation t cont
  |> Continuation_in_env.arity

let add_continuation0 t cont scope cont_in_env =
  let continuations =
    Continuation.Map.add cont (scope, cont_in_env) t.continuations
  in
  { t with
    continuations;
  }

let add_non_inlinable_continuation t cont scope ~params ~handler =
  match params with
  | [] ->
    add_continuation0 t cont scope (Non_inlinable_zero_arity { handler; })
  | _::_ ->
    let arity = Kinded_parameter.List.arity_with_subkinds params in
    add_continuation0 t cont scope (Non_inlinable_non_zero_arity { arity; })

let add_unreachable_continuation t cont scope arity =
  add_continuation0 t cont scope (Unreachable { arity; })

let add_continuation_alias t cont arity ~alias_for =
  let arity = Flambda_arity.With_subkinds.to_arity arity in
  let alias_for = resolve_continuation_aliases t alias_for in
  let alias_for_arity =
    continuation_arity t alias_for
    |> Flambda_arity.With_subkinds.to_arity
  in
  if not (Flambda_arity.equal arity alias_for_arity) then begin
    Misc.fatal_errorf "%a (arity %a) cannot be an alias for %a (arity %a) \
        since the two continuations differ in arity"
      Continuation.print cont
      Flambda_arity.print arity
      Continuation.print alias_for
      Flambda_arity.print alias_for_arity
  end;
  if Continuation.Map.mem cont t.continuation_aliases then begin
    Misc.fatal_errorf "Cannot add continuation alias %a (as alias for %a); \
        the continuation is already deemed to be an alias"
      Continuation.print cont
      Continuation.print alias_for
  end;
(* CR mshinwell: This should check that they are either both exn handlers
 or both non-exn handlers
  if Continuation.is_exn cont || Continuation.is_exn alias_for then begin
    Misc.fatal_errorf "Cannot alias exception handlers: %a (exn handler? %b) \
        as alias for %a (exn handler? %b)"
      Continuation.print cont
      (Continuation.is_exn cont)
      Continuation.print alias_for
      (Continuation.is_exn alias_for)
  end;
*)
  let alias_for = resolve_continuation_aliases t alias_for in
  let continuation_aliases =
    Continuation.Map.add cont alias_for t.continuation_aliases
  in
  { t with
    continuation_aliases;
  }

let add_linearly_used_inlinable_continuation t cont scope ~params
      ~handler ~free_names_of_handler ~cost_metrics_of_handler =
  add_continuation0 t cont scope
    (Linearly_used_and_inlinable { handler; free_names_of_handler;
      params; cost_metrics_of_handler })

let add_return_continuation t cont scope arity =
  add_continuation0 t cont scope
    (Toplevel_or_function_return_or_exn_continuation { arity; })

let add_exn_continuation t exn_cont scope =
  (* CR mshinwell: Think more about keeping these in both maps *)
  let continuations =
    let cont = Exn_continuation.exn_handler exn_cont in
    let cont_in_env : Continuation_in_env.t =
      Toplevel_or_function_return_or_exn_continuation {
        arity = Exn_continuation.arity exn_cont;
      }
    in
    Continuation.Map.add cont (scope, cont_in_env) t.continuations
  in
  let exn_continuations =
    Exn_continuation.Map.add exn_cont scope t.exn_continuations
  in
  { t with
    continuations;
    exn_continuations;
  }

let check_continuation_is_bound t cont =
  if not (Continuation.Map.mem cont t.continuations) then begin
    Misc.fatal_errorf "Unbound continuation %a in environment:@ %a"
      Continuation.print cont
      print t
  end

let check_exn_continuation_is_bound t exn_cont =
  if not (Exn_continuation.Map.mem exn_cont t.exn_continuations) then begin
    Misc.fatal_errorf "Unbound exception continuation %a in environment:@ %a"
      Exn_continuation.print exn_cont
      print t
  end

let add_apply_cont_rewrite t cont rewrite =
  if Continuation.Map.mem cont t.apply_cont_rewrites then begin
    Misc.fatal_errorf "Cannot redefine [Apply_cont_rewrite] for %a"
      Continuation.print cont
  end;
  let apply_cont_rewrites =
    Continuation.Map.add cont rewrite t.apply_cont_rewrites
  in
  { t with
    apply_cont_rewrites;
  }

let find_apply_cont_rewrite t cont =
  match Continuation.Map.find cont t.apply_cont_rewrites with
  | exception Not_found -> None
  | rewrite -> Some rewrite

let delete_apply_cont_rewrite t cont =
  { t with
    apply_cont_rewrites = Continuation.Map.remove cont t.apply_cont_rewrites;
  }

let will_inline_continuation t cont =
  match find_continuation t cont with
  | Linearly_used_and_inlinable _ -> true
  | Non_inlinable_zero_arity _ | Non_inlinable_non_zero_arity _
  | Toplevel_or_function_return_or_exn_continuation _ | Unreachable _ -> false
