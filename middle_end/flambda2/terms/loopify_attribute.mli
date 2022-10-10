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

val print : Format.formatter -> t -> unit

val should_loopify : t -> bool

val was_loopified : t -> bool

val equal : t -> t -> bool
