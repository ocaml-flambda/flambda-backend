(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                     Nathanaëlle Courant, OCamlPro                      *)
(*                                                                        *)
(*   Copyright 2022 OCamlPro SAS                                          *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

type t =
  | Always_loopify
  | Never_loopify
  | Already_loopified
  | Default_loopify_and_tailrec
  | Default_loopify_and_not_tailrec

let print ppf = function
  | Always_loopify -> Format.fprintf ppf "Always_loopify"
  | Never_loopify -> Format.fprintf ppf "Never_loopify"
  | Already_loopified -> Format.fprintf ppf "Already_loopified"
  | Default_loopify_and_tailrec ->
    Format.fprintf ppf "Default_loopify_and_tailrec"
  | Default_loopify_and_not_tailrec ->
    Format.fprintf ppf "Default_loopify_and_not_tailrec"

let should_loopify = function
  | Always_loopify | Default_loopify_and_tailrec -> true
  | Never_loopify | Already_loopified | Default_loopify_and_not_tailrec -> false

let was_loopified = function
  | Always_loopify | Already_loopified | Default_loopify_and_tailrec -> true
  | Never_loopify | Default_loopify_and_not_tailrec -> false

let equal t1 t2 =
  match t1, t2 with
  | Always_loopify, Always_loopify
  | Never_loopify, Never_loopify
  | Already_loopified, Already_loopified
  | Default_loopify_and_tailrec, Default_loopify_and_tailrec
  | Default_loopify_and_not_tailrec, Default_loopify_and_not_tailrec ->
    true
  | ( ( Always_loopify | Never_loopify | Already_loopified
      | Default_loopify_and_tailrec | Default_loopify_and_not_tailrec ),
      _ ) ->
    false
