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

type t = private
  | Do_not_rewrite_self_tail_calls
  | Rewrite_self_tail_calls of Continuation.t

val print : Format.formatter -> t -> unit

val do_not_rewrite_self_tail_calls : t

val rewrite_self_tail_calls : Continuation.t -> t
