(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018 OCamlPro SAS                                          *)
(*   Copyright 2018 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Use_info = struct
  type t =
    | Expected_to_be_used
    | Unused_because_of_call_site_decision
    | Unused_because_function_unknown

  let equal t1 t2 =
    match t1, t2 with
    | Expected_to_be_used, Expected_to_be_used
    | Unused_because_of_call_site_decision, Unused_because_of_call_site_decision
    | Unused_because_function_unknown, Unused_because_function_unknown ->
      true
    | ( ( Expected_to_be_used | Unused_because_of_call_site_decision
        | Unused_because_function_unknown ),
        _ ) ->
      false

  let explanation t =
    match t with
    | Expected_to_be_used -> None
    | Unused_because_of_call_site_decision ->
      Some
        "the optimizer decided not to inline the function given its \
         definition, or because its definition was not visible"
    | Unused_because_function_unknown ->
      Some
        ("the optimizer did not know what function was being applied"
        ^
        if Flambda_features.classic_mode ()
        then " (is it marked [@inline never]?)"
        else "")
end

type t =
  | Always_inlined of Use_info.t
  | Hint_inlined
  | Never_inlined
  | Unroll of int * Use_info.t
  | Default_inlined

let print ppf t =
  let fprintf = Format.fprintf in
  match t with
  | Always_inlined _ -> fprintf ppf "Always_inlined"
  | Hint_inlined -> fprintf ppf "Hint_inlined"
  | Never_inlined -> fprintf ppf "Never_inlined"
  | Unroll (n, _) -> fprintf ppf "@[(Unroll %d)@]" n
  | Default_inlined -> fprintf ppf "Default_inlined"

let equal t1 t2 =
  match t1, t2 with
  | Always_inlined use_info1, Always_inlined use_info2 ->
    Use_info.equal use_info1 use_info2
  | Hint_inlined, Hint_inlined
  | Never_inlined, Never_inlined
  | Default_inlined, Default_inlined ->
    true
  | Unroll (n1, use_info1), Unroll (n2, use_info2) ->
    n1 = n2 && Use_info.equal use_info1 use_info2
  | ( ( Always_inlined _ | Hint_inlined | Never_inlined | Unroll _
      | Default_inlined ),
      _ ) ->
    false

let is_default t =
  match t with
  | Default_inlined -> true
  | Always_inlined _ | Hint_inlined | Never_inlined | Unroll _ -> false

let from_lambda (attr : Lambda.inlined_attribute) =
  match attr with
  | Always_inlined -> Always_inlined Expected_to_be_used
  | Never_inlined -> Never_inlined
  | Hint_inlined -> Hint_inlined
  | Unroll i -> Unroll (i, Expected_to_be_used)
  | Default_inlined -> Default_inlined

let with_use_info t use_info =
  match t with
  | Always_inlined _ -> Always_inlined use_info
  | Unroll (n, _) -> Unroll (n, use_info)
  | Never_inlined | Hint_inlined | Default_inlined -> t

let use_info t =
  match t with
  | Always_inlined use_info | Unroll (_, use_info) -> Some use_info
  | Never_inlined | Hint_inlined | Default_inlined -> None
