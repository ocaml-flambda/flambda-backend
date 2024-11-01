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
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc

val convert_and_bind :
  Acc.t ->
  big_endian:bool ->
  Exn_continuation.t option ->
  register_const0:(Acc.t -> Static_const.t -> string -> Acc.t * Symbol.t) ->
  Lambda.primitive ->
  args:Simple.t list list ->
  Debuginfo.t ->
  current_region:Variable.t ->
  current_ghost_region:Variable.t ->
  (Acc.t -> Flambda.Named.t list -> Expr_with_acc.t) ->
  Expr_with_acc.t
