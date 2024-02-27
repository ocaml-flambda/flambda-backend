(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(** Simplification functions on [Rec_info_expr.t]. *)

val simplify_rec_info_expr :
  Downwards_acc.t -> Rec_info_expr.t -> Rec_info_expr.t

module Evaluated_rec_info_expr : sig
  type t = private
    { depth : int Or_infinity.t;
      unrolling : Rec_info_expr.Unrolling_state.t
    }

  val print : Format.formatter -> t -> unit
end

val evaluate_rec_info_expr :
  Downwards_acc.t -> Rec_info_expr.t -> Evaluated_rec_info_expr.t

val depth_may_exceed : Downwards_acc.t -> Rec_info_expr.t -> int -> bool

val known_remaining_unrolling_depth :
  Downwards_acc.t -> Rec_info_expr.t -> int option

val can_unroll : Downwards_acc.t -> Rec_info_expr.t -> bool
