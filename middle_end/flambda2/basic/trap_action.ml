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

[@@@ocaml.warning "+a-4-30-40-41-42"]

type raise_kind =
  | Regular
  | Reraise
  | No_trace

let raise_kind_to_int = function
  | Regular -> 0
  | Reraise -> 1
  | No_trace -> 2

let compare_raise_kind rk1 rk2 =
  Int.compare (raise_kind_to_int rk1) (raise_kind_to_int rk2)

type t =
  | Push of { exn_handler : Continuation.t; }
  | Pop of {
      exn_handler : Continuation.t;
      raise_kind : raise_kind option;
    }

let compare t1 t2 =
  match t1, t2 with
  | Push { exn_handler = exn_handler1; },
      Push { exn_handler = exn_handler2; } ->
    Continuation.compare exn_handler1 exn_handler2
  | Pop { exn_handler = exn_handler1; raise_kind = raise_kind1; },
      Pop { exn_handler = exn_handler2; raise_kind = raise_kind2; } ->
    let c = Continuation.compare exn_handler1 exn_handler2 in
    if c <> 0 then c
    else Option.compare compare_raise_kind raise_kind1 raise_kind2
  | Push _, Pop _ -> -1
  | Pop _, Push _ -> 1

let raise_kind_option_to_string = function
  | None -> ""
  | Some Regular -> " (raise-regular)"
  | Some Reraise -> " (reraise)"
  | Some No_trace -> " (notrace)"

let [@ocamlformat "disable"] print ppf t =
  let fprintf = Format.fprintf in
  match t with
  | Push { exn_handler; } ->
    fprintf ppf "%spush_trap%s %a %sthen%s "
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      Continuation.print exn_handler
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
  | Pop { exn_handler; raise_kind; } ->
    fprintf ppf "%spop_trap%s%s %a %sthen%s "
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())
      (raise_kind_option_to_string raise_kind)
      Continuation.print exn_handler
      (Flambda_colours.expr_keyword ())
      (Flambda_colours.normal ())

let [@ocamlformat "disable"] print_with_cache ~cache:_ ppf t = print ppf t

let invariant _env _t = ()

(* Continuations used in trap actions are tracked separately, since sometimes,
   we don't want to count them as uses.  However they must be tracked for
   lifting of continuations during [Closure_conversion]. *)
let free_names = function
  | Push { exn_handler; }
  | Pop { exn_handler; raise_kind = _; } ->
    Name_occurrences.singleton_continuation_in_trap_action exn_handler

let apply_renaming t perm =
  match t with
  | Push { exn_handler; } ->
    let exn_handler' = Renaming.apply_continuation perm exn_handler in
    if exn_handler == exn_handler' then t
    else Push { exn_handler = exn_handler'; }
  | Pop { exn_handler; raise_kind; } ->
    let exn_handler' = Renaming.apply_continuation perm exn_handler in
    if exn_handler == exn_handler' then t
    else Pop { exn_handler = exn_handler'; raise_kind; }

let exn_handler t =
  match t with
  | Push { exn_handler; }
  | Pop { exn_handler; _ } -> exn_handler

let all_ids_for_export t =
  Ids_for_export.add_continuation Ids_for_export.empty (exn_handler t)

module Option = struct
  type nonrec t = t option

  let [@ocamlformat "disable"] print ppf = function
    | None -> ()
    | Some t -> print ppf t

  let all_ids_for_export = function
    | None -> Ids_for_export.empty
    | Some trap_action -> all_ids_for_export trap_action

  let apply_renaming t renaming =
    match t with
    | None -> None
    | Some trap_action ->
      let trap_action' = apply_renaming trap_action renaming in
      if trap_action == trap_action' then t
      else Some trap_action'
end
