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

module Acc = Closure_conversion_aux.Acc
module Env = Closure_conversion_aux.Env
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc

val convert_and_bind :
  Acc.t ->
  Env.t ->
  big_endian:bool ->
  Exn_continuation.t option ->
  register_const_string:(Acc.t -> string -> Acc.t * Symbol.t) ->
  Lambda.primitive ->
  args:Simple.t list ->
  Debuginfo.t ->
  (Acc.t -> Flambda.Named.t option -> Acc.t * Expr_with_acc.t) ->
  Acc.t * Expr_with_acc.t
