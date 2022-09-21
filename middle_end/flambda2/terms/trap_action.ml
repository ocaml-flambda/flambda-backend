(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Raise_kind = struct
  type t =
    | Regular
    | Reraise
    | No_trace

  let to_int = function Regular -> 0 | Reraise -> 1 | No_trace -> 2

  let option_to_string = function
    | None -> ""
    | Some Regular -> " (raise-regular)"
    | Some Reraise -> " (reraise)"
    | Some No_trace -> " (notrace)"

  let compare rk1 rk2 = Int.compare (to_int rk1) (to_int rk2)

  let from_lambda (kind : Lambda.raise_kind) =
    match kind with
    | Raise_regular -> Regular
    | Raise_reraise -> Reraise
    | Raise_notrace -> No_trace

  let option_to_lambda t_opt : Lambda.raise_kind =
    match t_opt with
    | None -> Raise_notrace
    | Some Regular -> Raise_regular
    | Some Reraise -> Raise_reraise
    | Some No_trace -> Raise_notrace
end

type t =
  | Push of { exn_handler : Continuation.t }
  | Pop of
      { exn_handler : Continuation.t;
        raise_kind : Raise_kind.t option
      }

let compare t1 t2 =
  match t1, t2 with
  | Push { exn_handler = exn_handler1 }, Push { exn_handler = exn_handler2 } ->
    Continuation.compare exn_handler1 exn_handler2
  | ( Pop { exn_handler = exn_handler1; raise_kind = raise_kind1 },
      Pop { exn_handler = exn_handler2; raise_kind = raise_kind2 } ) ->
    let c = Continuation.compare exn_handler1 exn_handler2 in
    if c <> 0
    then c
    else Option.compare Raise_kind.compare raise_kind1 raise_kind2
  | Push _, Pop _ -> -1
  | Pop _, Push _ -> 1

let [@ocamlformat "disable"] print ppf t =
  let fprintf = Format.fprintf in
  match t with
  | Push { exn_handler; } ->
    fprintf ppf "%tpush_trap%t %a %tthen%t "
      Flambda_colours.expr_keyword
      Flambda_colours.pop
      Continuation.print exn_handler
      Flambda_colours.expr_keyword
      Flambda_colours.pop
  | Pop { exn_handler; raise_kind; } ->
    fprintf ppf "%tpop_trap%t%s %a %tthen%t "
      Flambda_colours.expr_keyword
      Flambda_colours.pop
      (Raise_kind.option_to_string raise_kind)
      Continuation.print exn_handler
      Flambda_colours.expr_keyword
      Flambda_colours.pop

(* Continuations used in trap actions are tracked separately, since sometimes,
   we don't want to count them as uses. However they must be tracked for lifting
   of continuations during [Closure_conversion]. *)
let free_names = function
  | Push { exn_handler } | Pop { exn_handler; raise_kind = _ } ->
    Name_occurrences.singleton_continuation_in_trap_action exn_handler

let apply_renaming t renaming =
  match t with
  | Push { exn_handler } ->
    let exn_handler' = Renaming.apply_continuation renaming exn_handler in
    if exn_handler == exn_handler'
    then t
    else Push { exn_handler = exn_handler' }
  | Pop { exn_handler; raise_kind } ->
    let exn_handler' = Renaming.apply_continuation renaming exn_handler in
    if exn_handler == exn_handler'
    then t
    else Pop { exn_handler = exn_handler'; raise_kind }

let exn_handler t =
  match t with Push { exn_handler } | Pop { exn_handler; _ } -> exn_handler

let ids_for_export t =
  Ids_for_export.add_continuation Ids_for_export.empty (exn_handler t)

module Option = struct
  type nonrec t = t option

  let [@ocamlformat "disable"] print ppf = function
    | None -> ()
    | Some t -> print ppf t

  let ids_for_export = function
    | None -> Ids_for_export.empty
    | Some trap_action -> ids_for_export trap_action

  let apply_renaming t renaming =
    match t with
    | None -> None
    | Some trap_action ->
      let trap_action' = apply_renaming trap_action renaming in
      if trap_action == trap_action' then t else Some trap_action'
end
