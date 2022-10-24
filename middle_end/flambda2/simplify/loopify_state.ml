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
  | Do_not_loopify
  | Loopify of Continuation.t

let print ppf = function
  | Do_not_loopify -> Format.fprintf ppf "do_not_loopify"
  | Loopify cont ->
    Format.fprintf ppf "@[<hov 1>(loopify@ %a)@]" Continuation.print cont

let do_not_loopify = Do_not_loopify

let loopify cont = Loopify cont
