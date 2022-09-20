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
  | Do_not_rewrite_self_tail_calls
  | Rewrite_self_tail_calls of Continuation.t

let print ppf = function
  | Do_not_rewrite_self_tail_calls ->
    Format.fprintf ppf "do_not_rewrite_self_tail_calls"
  | Rewrite_self_tail_calls cont ->
    Format.fprintf ppf "@[<hov 1>(rewrite_self_tail_calls@ %a)@]"
      Continuation.print cont

let do_not_rewrite_self_tail_calls = Do_not_rewrite_self_tail_calls

let rewrite_self_tail_calls cont = Rewrite_self_tail_calls cont
