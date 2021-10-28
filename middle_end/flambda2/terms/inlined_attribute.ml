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

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | Always_inlined
  | Hint_inlined
  | Never_inlined
  | Unroll of int
  | Default_inlined

let [@ocamlformat "disable"] print ppf t =
  let fprintf = Format.fprintf in
  match t with
  | Always_inlined -> fprintf ppf "Always_inlined"
  | Hint_inlined -> fprintf ppf "Hint_inlined"
  | Never_inlined -> fprintf ppf "Never_inlined"
  | Unroll n -> fprintf ppf "@[(Unroll %d)@]" n
  | Default_inlined -> fprintf ppf "Default_inlined"

let equal t1 t2 =
  match t1, t2 with
  | Always_inlined, Always_inlined
  | Hint_inlined, Hint_inlined
  | Never_inlined, Never_inlined
  | Default_inlined, Default_inlined ->
    true
  | Unroll n1, Unroll n2 -> n1 = n2
  | ( ( Always_inlined | Hint_inlined | Never_inlined | Unroll _
      | Default_inlined ),
      _ ) ->
    false

let is_default t =
  match t with
  | Default_inlined -> true
  | Always_inlined | Hint_inlined | Never_inlined | Unroll _ -> false
